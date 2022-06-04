# This function fits the nowcasting model described in Section 4.1.

# Input: 
# - doa: day of analysis
# - T.0: first registration date to be considered in the nowcast, corresponds to t = 0
# - d.max: maximum duration time that is assumed, see Section 5
# - base (two options: Meldedatum or Hospdatum)
# - create.plots: if TRUE, the plots which are depicted in Figures 3-5 
# are produced, if FALSE (default), no plots are produced
# - print.effects: if TRUE (default to FALSE) prints the numbers shown in Table 3
# - quantiles to be extracted
# - age_groups (options: split60, RKI)
# - adjust quantiles: Should quantiles be adjusted due to realization
#   uncertainty

# Output: a data frame that contains the estimated distribution function
# F_t(T-t) as well as the corresponding 2.5% and 97.5% quantiles

nowcasting <- function(doa, T_0, d_max, base = "Hospdatum", n = 100,
                       path = "Hospitalisation/01_Data",
                       quantiles = c(0.025, 0.975), adjust_quantiles = TRUE,
                       save_model = FALSE, save_results = FALSE) {
    
  T_max <- interval(T_0, doa) %/% days(1)
  dates <- seq(from = as.Date(T_0), to = as.Date(T_0) + days(T_max) - 1,
               by = "days")
  
  # Use english locale for weekday levels:
  if (Sys.info()['sysname'] != "Windows") {
    locale_weekday <- "en_US.UTF-8"
  }
  else {
    locale_weekday <- "English"
  }
  
    # Read data:
    file_doa <- paste0("hosp_prepared_", doa, ".rds")
    data <- readRDS(paste0(path, "/Data_prepared/", file_doa))
    
    # Create base variable for Meldedatum or Hospdatum:
    data <- data %>% mutate(base = !!sym(base))
    
    # Store older data not used for fitting of the model:
    data_old <- data %>% filter(base < T_0)
    
    # Define age groups:
    data <- data %>%
        mutate(Altersgruppe = case_when(AlterBerechnet < 60 ~ "0-60",
                                        AlterBerechnet >= 60 ~ "60+"),
               Altersgruppe = factor(x = Altersgruppe,
                                     levels = c("0-60", "60+")))
    
    # Group data according to Registrierungsdatum, Meldedatum and Altersgruppe:
    data <- data %>%
      filter(base >= T_0) %>%
      mutate(d = as.numeric(Meldedatum_Hosp - base)) %>%
      group_by(base, Meldedatum_Hosp, d, Altersgruppe) %>%
      dplyr::summarize(N_t_d = pmax(n(), 0)) %>%
      dplyr::select(base, Meldedatum_Hosp, d, Altersgruppe, N_t_d)
    
    # Manually correct d = 0 to d = 1:
    data <- data %>%
      mutate(d = if_else(condition = d == 0, true = 1, false = d),
             base = if_else(condition = Meldedatum_Hosp == base,
                            true = base - days(1), false = base))
    
    data <- data %>% filter(base %in% dates, d <= d_max, d > 0,
                            Meldedatum_Hosp <= doa) %>%
      complete(base, Meldedatum_Hosp, d, Altersgruppe,
               fill = list(N_t_d = 0)) %>%
      mutate(Wochentag = lubridate::wday(x = base, label = TRUE, abbr = FALSE,
                                         locale = locale_weekday))
    
    # Grid for all possible date combinations:
    data_grid <- expand.grid(dates, c(dates + d_max, dates)) %>%
      distinct()
    colnames(data_grid) <- c("base", "Meldedatum_Hosp")
    data_grid <- data_grid %>% mutate(d = as.numeric(Meldedatum_Hosp - base)) %>%
      dplyr::filter(d <= d_max, Meldedatum_Hosp > base)
    
    # Join data with grid data:
    data_nowcast <- full_join(data, data_grid) %>%
      complete(base, Meldedatum_Hosp, d, Altersgruppe,
               fill = list(N_t_d = 0,
                           Altersgruppe = data$Altersgruppe[1])) %>%
      distinct() %>%
      mutate(Wochentag = lubridate::wday(x = base, label = TRUE, abbr = FALSE,
                                         locale = locale_weekday))

  # Add cumulative numbers:
  data_nowcast <- data_nowcast %>% group_by(base, Altersgruppe) %>%
    mutate(C_t_d = cumsum(N_t_d),
           t = as.numeric(base) - as.numeric(dates[1]) + 1)

  # some further data preparations:
  data_nowcast <- data_nowcast %>%
    mutate(Wochentag_hosp = lubridate::wday(x = Meldedatum_Hosp, label = TRUE,
                                            abbr = FALSE, locale = locale_weekday),
           t1 = t,  t2 = max(t1 - 28, 0), # break point four weeks before analysis date
           pi = NA)
  class(data_nowcast$Wochentag) <- "factor"
  class(data_nowcast$Wochentag_hosp) <- "factor"
  data_nowcast_model <- data_nowcast %>% filter(t + d <= T_max + 1)
  data_nowcast_new <- data_nowcast %>% filter(t + d > T_max + 1)
  
  data_nowcast_model$Wochentag <- factor(x = data_nowcast_model$Wochentag,
                                   levels = c("Monday", "Tuesday", "Wednesday",
                                              "Thursday", "Friday", "Saturday",
                                              "Sunday"))
  data_nowcast_model$Wochentag_hosp <- factor(x = data_nowcast_model$Wochentag_hosp,
                                              levels = c("Monday", "Tuesday", "Wednesday",
                                                         "Thursday", "Friday", "Saturday",
                                                         "Sunday"))
  
  # Fit the nowcast model and add fitted values to the data frame
  model <- gam(formula = cbind(N_t_d, C_t_d - N_t_d) ~
                 s(d, k = 5, bs = "ps", by = Altersgruppe) + t1 + t2  +
                 Wochentag + Wochentag_hosp,
               family = binomial(link = "logit"),
               data = data_nowcast_model[data_nowcast_model$d > 1, ])
  
  if (save_model == TRUE) {
     saveRDS(object = model,
             file = paste0(path, "/../03_Results/Models/nowcast_model_",
                           base, "_", doa, ".rds"))
  }
  
  data_nowcast_model$pi[data_nowcast_model$d > 1] <- predict.gam(object = model,
                                                                 type = "response")
  data_nowcast <- rbind(data_nowcast_model, data_nowcast_new)
  
  # Compute nowcasting:
  data_nowcast <- predict_nowcast(model = model, data = data_nowcast, 
                                  T_max = T_max, quantiles = quantiles, n = n,
                                  adjust_quantiles = TRUE)
  
  
  #########################################
  # Report nowcasting results:
  data_report <- data_nowcast %>% group_by(base, Altersgruppe) %>%
    slice(tail(row_number(), 1)) %>%
    dplyr::select(base, Altersgruppe, C_t_d, C_estimate,
                  starts_with("C_quantile_"),
                  C7_estimate,starts_with("C7_quantile_"), F_estimate,
                  starts_with("F_quantile_")) %>%
    arrange(desc(base))
  
  data_report <- add_older_data(data_report, data_old, LGL_data, age_groups,
                                quantiles)
  
  # Add 7-day sum of  values:
  data_report$Altersgruppe <- as.factor(data_report$Altersgruppe)
  data_report$reported7 <- NA
  for(i in 1:nrow(data_report)) {
    if (is.na(data_report$reported7[i])) {
      data_report$reported7[i] <-
        sum(data_report$C_t_d[i + seq(from = 0,
                                      to = length(levels(data_report$Altersgruppe)) * 6,
                                      by = length(levels(data_report$Altersgruppe)))])
    }
  }
  
  ###################################
  # Prepare output:
  data_report <- data_report %>%
    mutate_at(4:(length(quantiles) * 2 + 5), round, 1) %>%
    dplyr::select(base, Altersgruppe, C_t_d, C_estimate, starts_with("C_quantile_"),
                  reported7, C7_estimate,starts_with("C7_quantile_"),
                  F_estimate, starts_with("F_quantile_")) %>%
    filter(base >= "2020-01-28")
  colnames(data_report) <- c("date", "age60", "reported", "nowcast_est",
                             paste0("nowcast_",rep(quantiles, each=1)),
                             "reported7", "nowcast7_est", paste0("nowcast7_",rep(quantiles, each=1)),
                             "F_est", paste0("F_",rep(quantiles, each=1)))
  
  if (save_results == TRUE) {
      readr::write_csv2(data_report, paste0(path, "/../03_Results/Models/nowcasting_results_",
                                            base, "_", doa, ".csv"))
  }
  
  # Return nowcast results:
  return(data_report)
}

# This function performs the actual nowcast (i.e. calculates the nowcasted
# hospitalizations) and calculates the required quantiles via a bootstrap.
# It is only called inside the function nowcasting(...)

# Input: 
# - model: the fitted nowcast model
# - data: a data frame that contains all the data fitted in the nowcasting model
# a column that contains the fitted probabilities pi() is also included
# - newdata: a data frame that contains the data to be predicted 
# (i.e. the NAs in the matrix stated in Section 5)
# a column that contains the predicted probabilities pi() is also included
# - T_max: the number of considered registration dates
# - n: number of bootstrap samples (default to n = 10 000)
# - alpha: alpha/2 and 1-alpha/2 quantiles of the nowcast will be calculated
# - base (two options: Meldedatum or Hospdatum)
# (default to alpha = 0.05)

# Output: a data frame that binds the rows of data and newdata including new 
# columns for the nowcast estimate as well as the 
# alpha/2 and 1-alpha/2 quantiles

predict_nowcast <- function(model, data, T_max, quantiles, n = 1000,
                            alpha = 0.05, adjust_quantiles = FALSE) {
  
  # Extract newdata:
  newdata <- data %>% filter(t + d > T_max + 1)
  newdata$pi <- predict.gam(object = model, type = "response",
                            newdata = newdata)
  
  # Prediction matrix:
  model_matrix <- predict.gam(object = model, type = "lpmatrix",
                              newdata = newdata)
  
  # Mean and covariance matrix of estimated model parameters:
  theta <- model$coefficients
  V <- vcov.gam(model)
  
  # Simulate from model parameters and determine pi:
  set.seed(1)
  X <- rmvn(n, theta, V)
  lp <- X %*% t(model_matrix)
  pi <- exp(lp) / (1 + exp(lp))
  
  # Matrix of predicted F and C
  F_sim <- C_sim <- C7_sim <- matrix(0, n, ncol(pi))
  C_sim_both <- C7_sim_both <- matrix(0, n, ncol(pi) /
                                        (length(levels(newdata$Altersgruppe))))
  
  # Add prediction bounds to the datasets
  data$C_estimate <- data$C_t_d
  newdata$C_estimate <- NA
  
  # Add predictions for 7-day numbers to the dataset:
  data$C7_estimate <- newdata$C7_estimate <- NA
  
  # Add F to the datasets:
  data$F_estimate <- newdata$F_estimate <- 1
  
  # Create datasets for sum of both age groups:
  # TODO: Spalten fehlen
  data_both <- data %>% group_by(base, d) %>%
    dplyr::summarize(N_t_d = sum(N_t_d), C_t_d = sum(C_t_d),
                     C_estimate = sum(C_estimate),
                     C7_estimate = sum(C7_estimate),
                     t = mean(t), d = mean(d)) %>%
    mutate(Altersgruppe = "alle")
  data <- bind_rows(data, data_both)
  data$Altersgruppe <- as.factor(data$Altersgruppe)
  newdata_both <- newdata %>% group_by(base, d) %>%
    dplyr::summarize(N_t_d = sum(N_t_d), C_t_d = sum(C_t_d),
                     C_estimate = sum(C_estimate),
                     C7_estimate = sum(C7_estimate),
                     t = mean(t)) %>%
    mutate(Altersgruppe = "alle")
  newdata <- bind_rows(newdata, newdata_both)
  newdata$Altersgruppe <- as.factor(newdata$Altersgruppe)
  
  # Add estimates for different dates:
  for (tt in unique(newdata$t)) {
    print(tt)
    C_estimate_sum <- 0
    for(age in levels(newdata$Altersgruppe)) {

      if(age != "alle"){
        ind_t <- which(newdata$t == tt & newdata$Altersgruppe == age)
        C_t_d <- data$C_t_d[which(data$t == tt &
                                        data$d == max(data$d[which(data$t == tt)],
                                                      na.rm = TRUE) &
                                        data$Altersgruppe == age)]
        newdata$F_estimate[ind_t] <- cumprod(1 - newdata$pi[ind_t]) 
        newdata$C_estimate[ind_t] <- C_t_d / newdata$F_estimate[ind_t]
        
        #start summing up for the all variable
        C_estimate_sum <- C_estimate_sum + newdata$C_estimate[ind_t]

        # Compute predicted C over 7 days:
        for (d in unique(newdata$d[newdata$t == tt & newdata$Altersgruppe == age])) {
          ind_d <- which(newdata$t <= tt & newdata$t >= tt - 6 &
                               newdata$d == d & newdata$Altersgruppe == age)
          newdata$C7_estimate[tail(ind_d, 1)] <- sum(newdata$C_estimate[ind_d])
          
          # Calculate sum for days without all entries in newdata:
          data_known <- data %>% filter(t + d <= T_max + 1)
          if (length(ind_d) < 7) {
            len <- 7 - length(ind_d)
            C_t_d_add <- sum(data_known$C_t_d[which(data_known$t <= tt & data_known$t >= tt - 6 &
                                                          data_known$d == d & data_known$Altersgruppe == age)])
            newdata$C7_estimate[tail(ind_d, 1)] <- newdata$C7_estimate[tail(ind_d, 1)] + C_t_d_add
          } 
        }
        
        # Compute simulated C:
        for (j in 1:n) {
          F_sim[j, ind_t] <- cumprod(1 - pi[j, ind_t])
          C_sim[j, ind_t] <- C_t_d / F_sim[j, ind_t]
          
          # Optionally draw from Poisson distribution for difference of
          # reported and estimated cases (last available row for time t):
          if (adjust_quantiles == TRUE) {
            C_sim[j, tail(ind_t, 1)] <- C_t_d +
              rpois(n = 1, lambda = C_sim[j, tail(ind_t, 1)] - C_t_d)
          }
          
          # Compute simulated C7:
          for (d in unique(newdata$d[newdata$t == tt & newdata$Altersgruppe == age])) {
            ind_d <- which(newdata$t <= tt & newdata$t >= tt - 6 &
                                 newdata$d == d & newdata$Altersgruppe == age)
            C7_sim[j, tail(ind_d, 1)] <- sum(C_sim[j, ind_d])
          }
          
          # Calculate sum for days without all entries:
          if (length(ind_d) < 7) {
            len <- 7 - length(ind_d)
            C_t_d_add <- sum(data_known$C_t_d[which(data_known$t <= tt & data_known$t >= tt - 6 &
                                                          data_known$d == d & data_known$Altersgruppe == age)])
            C7_sim[j, tail(ind_d, 1)] <- C7_sim[j, tail(ind_d, 1)] + C_t_d_add
          } 
        }
        
      }
      # alle Altersgruppen gesamt
      else {
        ind_t <- which(newdata$t == tt & newdata$Altersgruppe == age)
        newdata$C_estimate[ind_t] <- C_estimate_sum
      }
    }
  }
  newdata <- newdata %>% group_by(base,d) %>% 
    mutate(C7_estimate = ifelse(Altersgruppe=="alle", sum(C7_estimate,na.rm = TRUE),
                                C7_estimate))
  
  # Compute estimates for all age groups:
  mult_factor <- length(levels(newdata$Altersgruppe)) - 1
  for (j in 1:ncol(C_sim_both)) {
    C_sim_both[, j] <- rowSums(cbind(C_sim[, mult_factor * j - (mult_factor - 1:(mult_factor - 1))],
                                     C_sim[, mult_factor * j]))
    C7_sim_both[, j] <- rowSums(cbind(C7_sim[, mult_factor * j - (mult_factor - 1:(mult_factor - 1))],
                                      C7_sim[, mult_factor * j]))
  }

  # Add quantiles:
  quantiles_newdata <- matrix(nrow = nrow(newdata), ncol = 3 * length(quantiles))
  colnames(quantiles_newdata) <- paste0(c("F_quantile_", "C_quantile_",
                                          "C7_quantile_"),
                                        rep(quantiles, each = 3))
  
  # Compute quantiles:
  # Individual age groups:
  for(k in 1:ncol(pi)) {
    quantiles_newdata[k, seq(from = 1, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(F_sim[, k], 1 - quantiles, type = 7, na.rm = TRUE)
    quantiles_newdata[k, seq(from = 2, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(C_sim[, k], quantiles, type = 7, na.rm = TRUE)
    quantiles_newdata[k, seq(from = 3, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(C7_sim[, k], quantiles, type = 7, na.rm = TRUE)
  }
  # Sum of age groups:
  for (k in 1:ncol(C_sim_both)) {
    quantiles_newdata[k + ncol(pi), seq(from = 2, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(C_sim_both[, k], quantiles, type = 7, na.rm = TRUE)
    quantiles_newdata[k + ncol(pi), seq(from = 3, to = ncol(quantiles_newdata), by = 3)] <-
      quantile(C7_sim_both[, k], quantiles, type = 7, na.rm = TRUE)
  }
  
  # Add to data:
  quantiles_newdata <- as.data.frame(quantiles_newdata)
  newdata <- bind_cols(newdata, quantiles_newdata)
  
  # Prepare known data:
  data <- data %>% filter(t + d <= T_max) %>%
    mutate(F_estimate = 1)
  
  # Add quantiles
  quantiles_data <- matrix(NA, nrow = nrow(data), ncol = 3 *length(quantiles))
  colnames(quantiles_data) <- paste0(c("F_quantile_", "C_quantile_",
                                       "C7_quantile_"),
                                        rep(quantiles, each = 3))
  
  # Add to data:
  quantiles_data <- as.data.frame(quantiles_data)
  data <- bind_cols(data, quantiles_data)
  data <- data %>% mutate_at(vars(starts_with("F")), ~1)

  # Prepare final data:
  data_final <- rbind(data, newdata) %>% arrange(desc(base), Meldedatum_Hosp)
  return(data_final)
}


## This function adds older time points to the output data of the nowcasting
## function:
# Input: 
# - data_report: output data of nowcasting model
# - data_old: data containing data with baseline dates older than T_0
add_older_data <- function(data_report, data_old, LGL_data,age_groups, quantiles) {
  
    # Define age groups:
      data_old <- data_old %>%
        mutate(Altersgruppe = case_when(AlterBerechnet < 60 ~ "0-60",
                                        AlterBerechnet >= 60 ~ "60+"),
               Altersgruppe = factor(x = Altersgruppe,
                                     levels = c("0-60", "60+")))
  
  # Preparation of older data:
    data_old <- data_old %>%
      group_by(Altersgruppe, base) %>%
      dplyr::summarize(C_t_d = pmax(n(), 0)) %>%
      complete(base = seq.Date(min(base), max(base), by = "day"), Altersgruppe,
               fill = list(C_t_d = 0)) %>%
      distinct()
  
  data_old_both <- data_old %>% group_by(base) %>%
    dplyr::summarize(C_t_d = sum(C_t_d)) %>% mutate(Altersgruppe = "alle")
  
  # Add information about F to old data:
  data_old$F_estimate <- ifelse(test = data_old$Altersgruppe == "alle",
                                   yes = NA, no = 1)
  
  for(q in quantiles){
    data_old[,ncol(data_old) + 1] <- ifelse(test = data_old$Altersgruppe == "alle",
                                          yes = NA, no = 1)
    colnames(data_old)[ncol(data_old)] <- paste0("F_quantile_", q)
  }
  
  # Match both data sources:
  data_report <- bind_rows(data_report, data_old, data_old_both) %>%
    arrange(desc(base), Altersgruppe) %>%
    mutate(Altersgruppe = as.factor(Altersgruppe))
  
  # Insert missing values:
  data_report$C_estimate <- ifelse(test = is.na(data_report$C_estimate),
                                   yes = data_report$C_t_d,
                                   no = data_report$C_estimate)
  
  # Add seven day-calculations for older time points:
  for(i in 1:nrow(data_report)) {
    if (is.na(data_report$C7_estimate[i])) {
      data_report$C7_estimate[i] <-
        sum(data_report$C_estimate[i + seq(from = 0, to = length(levels(data_report$Altersgruppe)) * 6,
                                           by = length(levels(data_report$Altersgruppe)))])
    }
  }
  
  # Add missing values:
  for(q in quantiles){
    data_report[is.na(data_report[paste0("C_quantile_", q)]),paste0("C_quantile_", q)] <-
      data_report$C_t_d[is.na(data_report[paste0("C_quantile_", q)])]
    data_report[is.na(data_report[paste0("C7_quantile_", q)]),paste0("C7_quantile_", q)] <-
      data_report$C7_estimate[is.na(data_report[paste0("C7_quantile_", q)])]
  }

  for(q in quantiles){
    data_report[is.na(data_report[paste0("C7_quantile_", q)]), paste0("C7_quantile_", q)] <-
      data_report$C7_estimate[is.na(data_report[paste0("C7_quantile_", q)])]
  }

  # Return result data:
  return(data_report)
}










