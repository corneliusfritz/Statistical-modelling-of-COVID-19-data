# This function fits the nowcasting model described in Section 4.1.

# Input: 
# - doa: day of analysis
# - T_0: first reporting date of infection to be considered in the nowcast,
#        corresponds to t = 0
# - n: number of bootstrap iterations
# - path: file path where data are loaded from
# - quantiles: quantiles to be extracted
# - adjust quantiles: Should quantiles be adjusted due to realization
#                     uncertainty?
# - save_model: Should the fitted model be saved?
# - save_results: Should an output file of the nowcasting results be saved?

# Output: a data frame containing the nowcasting results on daily and seve

nowcasting <- function(doa, T_0, d_max, n = 100,
                       path = "Hospitalisation/01_Data",
                       quantiles = c(0.025, 0.975), adjust_quantiles = TRUE,
                       save_model = FALSE, save_results = FALSE) {
  
  # Vector of dates used for the nowcasting model:
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
  data <- readRDS(paste0(path, "/data_section4.rds"))
  
  # Define age groups:
  data <- data %>%
    mutate(age_group = case_when(age_group %in% c("A00-A04", "A05-A14",
                                                  "A15-A34", "A35-A59") ~ "0-59",
                                 age_group %in% c("A60-A79", "A80+") ~ "60+"),
           age_group = factor(x = age_group, levels = c("0-59", "60+")))
  
  # Store older data not used for fitting of the model:
  data_old <- data %>% filter(date < T_0)
  
  # Group data according to date and age_group:
  data <- data %>%
    filter(date >= T_0) %>% mutate(date_report = date + days(delay)) %>%
    group_by(date, date_report, delay, age_group) %>%
    dplyr::summarize(N_t_d = pmax(n(), 0)) %>%
    dplyr::select(date, date_report, age_group, delay, N_t_d)
  
  # Create data base for nowcast model:
  data <- data %>%
    filter(date %in% dates, delay <= d_max, delay > 0, date_report <= doa) %>%
    complete(date, date_report, delay, age_group, fill = list(N_t_d = 0)) %>%
    mutate(weekday = lubridate::wday(x = date, label = TRUE, abbr = FALSE,
                                       locale = locale_weekday))
  data_grid <- expand.grid(dates, c(dates + d_max, dates)) %>%
    distinct()
  colnames(data_grid) <- c("date", "date_report")
  data_grid <- data_grid %>% mutate(delay = as.numeric(date_report - date)) %>%
    dplyr::filter(delay <= d_max, date_report > date)
  data_nowcast <- full_join(data, data_grid) %>%
    complete(date, date_report, delay, age_group,
             fill = list(N_t_d = 0, age_group = data$age_group[1])) %>%
    distinct() %>%
    mutate(weekday = lubridate::wday(x = date, label = TRUE, abbr = FALSE,
                                     locale = locale_weekday))
  
  # Add cumulative numbers:
  data_nowcast <- data_nowcast %>% group_by(date, age_group) %>%
    mutate(C_t_d = cumsum(N_t_d),
           t = as.numeric(date) - as.numeric(dates[1]) + 1)
  
  # some further data preparations:
  data_nowcast <- data_nowcast %>%
    mutate(weekday_report = lubridate::wday(x = date_report, label = TRUE,
                                            abbr = FALSE,
                                            locale = locale_weekday),
           t1 = t,  t2 = max(t1 - 28, 0), # break point four weeks before analysis date
           pi = NA)
  class(data_nowcast$weekday) <- "factor"
  class(data_nowcast$weekday_report) <- "factor"
  data_nowcast_model <- data_nowcast %>% filter(t + delay <= T_max + 1)
  data_nowcast_new <- data_nowcast %>% filter(t + delay > T_max + 1)
  data_nowcast_model$weekday <- factor(x = data_nowcast_model$weekday,
                                       levels = c("Monday", "Tuesday", "Wednesday",
                                                  "Thursday", "Friday", "Saturday",
                                                  "Sunday"))
  data_nowcast_model$weekday_report <- factor(x = data_nowcast_model$weekday_report,
                                              levels = c("Monday", "Tuesday", "Wednesday",
                                                         "Thursday", "Friday", "Saturday",
                                                         "Sunday"))
  
  # Fit the nowcast model and add fitted values to the data frame
  model <- gam(formula = cbind(N_t_d, C_t_d - N_t_d) ~
                 s(delay, k = 5, bs = "ps", by = age_group) + t1 + t2  +
                 weekday + weekday_report,
               family = binomial(link = "logit"),
               data = data_nowcast_model[data_nowcast_model$delay > 1, ])
  
  if (save_model == TRUE) {
    saveRDS(object = model,
            file = paste0(path, "/../03_Results/Models/nowcast_model_rki",
                          "_", doa, ".rds"))
  }
  
  # Prediction for training data:
  data_nowcast_model$pi[data_nowcast_model$delay > 1] <- predict.gam(object = model,
                                                                     type = "response")
  data_nowcast <- rbind(data_nowcast_model, data_nowcast_new)
  
  # Compute nowcasting:
  data_nowcast <- predict_nowcast(model = model, data = data_nowcast, 
                                  T_max = T_max, quantiles = quantiles, n = n,
                                  adjust_quantiles = TRUE)
  
  
  #########################################
  # Report nowcasting results:
  data_report <- data_nowcast %>% group_by(date, age_group) %>%
    slice(tail(row_number(), 1)) %>%
    dplyr::select(date, age_group, C_t_d, C_estimate,
                  starts_with("C_quantile_"),
                  C7_estimate,starts_with("C7_quantile_"), F_estimate,
                  starts_with("F_quantile_")) %>%
    arrange(desc(date))
  
  # Add data not included into the model fitting:
  data_report <- add_older_data(data_report, data_old, quantiles)
  
  # Add 7-day sum of  values:
  data_report$age_group <- as.factor(data_report$age_group)
  data_report$reported7 <- NA
  for(i in 1:nrow(data_report)) {
    if (is.na(data_report$reported7[i])) {
      data_report$reported7[i] <-
        sum(data_report$C_t_d[i + seq(from = 0,
                                      to = length(levels(data_report$age_group)) * 6,
                                      by = length(levels(data_report$age_group)))])
    }
  }
  
  # Prepare output:
  data_report <- data_report %>%
    mutate_at(4:(length(quantiles) * 2 + 5), round, 1) %>%
    dplyr::select(date, age_group, C_t_d, C_estimate, starts_with("C_quantile_"),
                  reported7, C7_estimate,starts_with("C7_quantile_"),
                  F_estimate, starts_with("F_quantile_")) %>%
    filter(date > "2021-09-06")
  colnames(data_report) <- c("date", "age_group", "reported", "nowcast_est",
                             paste0("nowcast_",rep(quantiles, each = 1)),
                             "reported7", "nowcast7_est",
                             paste0("nowcast7_", rep(quantiles, each = 1)),
                             "F_est", paste0("F_", rep(quantiles, each = 1)))
  
  if (save_results == TRUE) {
    readr::write_csv2(data_report, paste0(path,
                                          "/../03_Results/Models/nowcasting_results_rki_",
                                          doa, ".csv"))
  }
  
  # Return nowcast results:
  return(data_report)
}

# This function performs the actual nowcast (i.e. calculates the nowcasted
# hospitalizations) and calculates the required quantiles via bootstrap.

# Input: 
# - model: the fitted nowcast model
# - data: a data frame containing the data involved in the nowcasting process
#   (training data and data to be predicted)
# - T_max: the number of considered registration dates
# - n: number of bootstrap samples (default to n = 1000)
# - quantiles: quantiles to be extracted
# - adjust quantiles: Should quantiles be adjusted due to realization
#                     uncertainty?

# Output: a data frame that binds the rows of data and newdata including new 
# columns for the nowcast estimate as well as the specified quantiles

predict_nowcast <- function(model, data, T_max, quantiles, n = 1000,
                            adjust_quantiles = FALSE) {
  
  # Extract data for prediction:
  newdata <- data %>% filter(t + delay > T_max + 1)
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
  
  # Matrix of predicted F and C:
  F_sim <- C_sim <- C7_sim <- matrix(0, n, ncol(pi))
  C_sim_both <- C7_sim_both <- matrix(0, n, ncol(pi) /
                                        (length(levels(newdata$age_group))))
  
  # Add prediction bounds to the datasets
  data$C_estimate <- data$C_t_d
  newdata$C_estimate <- NA
  
  # Add predictions for 7-day numbers to the dataset:
  data$C7_estimate <- newdata$C7_estimate <- NA
  
  # Add F to the datasets:
  data$F_estimate <- newdata$F_estimate <- 1
  
  # Create datasets for sum of both age groups:
  data_both <- data %>% group_by(date, delay) %>%
    dplyr::summarize(N_t_d = sum(N_t_d), C_t_d = sum(C_t_d),
                     C_estimate = sum(C_estimate),
                     C7_estimate = sum(C7_estimate),
                     t = mean(t), delay = mean(delay)) %>%
    mutate(age_group = "all")
  data <- bind_rows(data, data_both)
  data$age_group <- as.factor(data$age_group)
  newdata_both <- newdata %>% group_by(date, delay) %>%
    dplyr::summarize(N_t_d = sum(N_t_d), C_t_d = sum(C_t_d),
                     C_estimate = sum(C_estimate),
                     C7_estimate = sum(C7_estimate),
                     t = mean(t)) %>%
    mutate(age_group = "all")
  newdata <- bind_rows(newdata, newdata_both)
  newdata$age_group <- as.factor(newdata$age_group)
  
  # Add estimates for different dates:
  for (tt in unique(newdata$t)) {
    print(tt)
    C_estimate_sum <- 0
    for(age in levels(newdata$age_group)) {
      
      if(age != "all"){
        ind_t <- which(newdata$t == tt & newdata$age_group == age)
        C_t_d <- data$C_t_d[which(data$t == tt &
                                    data$delay == max(data$delay[which(data$t == tt)],
                                                  na.rm = TRUE) &
                                    data$age_group == age)]
        newdata$F_estimate[ind_t] <- cumprod(1 - newdata$pi[ind_t]) 
        newdata$C_estimate[ind_t] <- C_t_d / newdata$F_estimate[ind_t]
        
        #start summing up for the all variable
        C_estimate_sum <- C_estimate_sum + newdata$C_estimate[ind_t]
        
        # Compute predicted C over 7 days:
        for (d in unique(newdata$delay[newdata$t == tt & newdata$age_group == age])) {
          ind_d <- which(newdata$t <= tt & newdata$t >= tt - 6 &
                           newdata$delay == d & newdata$age_group == age)
          newdata$C7_estimate[tail(ind_d, 1)] <- sum(newdata$C_estimate[ind_d])
          
          # Calculate sum for days without all entries in newdata:
          data_known <- data %>% filter(t + d <= T_max + 1)
          if (length(ind_d) < 7) {
            len <- 7 - length(ind_d)
            C_t_d_add <- sum(data_known$C_t_d[which(data_known$t <= tt & data_known$t >= tt - 6 &
                                                      data_known$delay == d & data_known$age_group == age)])
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
          for (d in unique(newdata$delay[newdata$t == tt & newdata$age_group == age])) {
            ind_d <- which(newdata$t <= tt & newdata$t >= tt - 6 &
                             newdata$delay == d & newdata$age_group == age)
            C7_sim[j, tail(ind_d, 1)] <- sum(C_sim[j, ind_d])
          }
          
          # Calculate sum for days without all entries:
          if (length(ind_d) < 7) {
            len <- 7 - length(ind_d)
            C_t_d_add <- sum(data_known$C_t_d[which(data_known$t <= tt & data_known$t >= tt - 6 &
                                                      data_known$delay == d & data_known$age_group == age)])
            C7_sim[j, tail(ind_d, 1)] <- C7_sim[j, tail(ind_d, 1)] + C_t_d_add
          } 
        }
        
      }
      # alle Altersgruppen gesamt
      else {
        ind_t <- which(newdata$t == tt & newdata$age_group == age)
        newdata$C_estimate[ind_t] <- C_estimate_sum
      }
    }
  }
  newdata <- newdata %>% group_by(date, delay) %>% 
    mutate(C7_estimate = ifelse(age_group == "all", sum(C7_estimate,na.rm = TRUE),
                                C7_estimate))
  
  # Compute estimates for all age groups:
  mult_factor <- length(levels(newdata$age_group)) - 1
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
  data <- data %>% filter(t + delay <= T_max) %>%
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
  data_final <- rbind(data, newdata) %>% arrange(desc(date), date_report)
  return(data_final)
}


## This function adds older time points to the output data of the nowcasting
## function:
# Input: 
# - data_report: output data of nowcasting model
# - data_old: data containing data with baseline dates older than T_0

# Output: data.frame indluding data_report and data_old
add_older_data <- function(data_report, data_old, quantiles) {
  
  # Preparation of older data:
  data_old <- data_old %>%
    group_by(age_group, date) %>%
    dplyr::summarize(C_t_d = pmax(n(), 0)) %>%
    complete(date = seq.Date(min(date), max(date), by = "day"), age_group,
             fill = list(C_t_d = 0)) %>%
    distinct()
  
  data_old_both <- data_old %>% group_by(date) %>%
    dplyr::summarize(C_t_d = sum(C_t_d)) %>% mutate(age_group = "all")
  
  # Add information about F to old data:
  data_old$F_estimate <- ifelse(test = data_old$age_group == "all",
                                yes = NA, no = 1)
  
  for(q in quantiles){
    data_old[, ncol(data_old) + 1] <- ifelse(test = data_old$age_group == "all",
                                            yes = NA, no = 1)
    colnames(data_old)[ncol(data_old)] <- paste0("F_quantile_", q)
  }
  
  # Match both data sources:
  data_report <- bind_rows(data_report, data_old, data_old_both) %>%
    arrange(desc(date), age_group) %>%
    mutate(age_group = as.factor(age_group))
  
  # Insert missing values:
  data_report$C_estimate <- ifelse(test = is.na(data_report$C_estimate),
                                   yes = data_report$C_t_d,
                                   no = data_report$C_estimate)
  
  # Add seven day-calculations for older time points:
  for(i in 1:nrow(data_report)) {
    if (is.na(data_report$C7_estimate[i])) {
      data_report$C7_estimate[i] <-
        sum(data_report$C_estimate[i + seq(from = 0, to = length(levels(data_report$age_group)) * 6,
                                           by = length(levels(data_report$age_group)))])
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


