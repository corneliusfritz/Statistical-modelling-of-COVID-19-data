# This function fits the hospitalisation model described in Section 4.2.

# Input: 
# - doa: day of analysis
# - T_0: first reporting date of infection to be considered in the nowcast,
#        corresponds to t = 0
# - nowcast: Should nowcast estimates be incorporated into the model?
#            The default (TRUE) corresponds to the analysis in the manuscript.

# Output: an mgcv model object

fit_hosp_model <- function(doa, T_0, nowcast = TRUE, path = "Hospitalisation") {
  
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
  data <- readRDS(paste0(path, "/01_Data/data_section4.rds")) 
  
  # Preprocess district data
  districts <- preprocess_districts(paste0(path, "/01_Data/Data_demographics")) %>%
    filter(districtId > 9000 & districtId < 10000)
  
  # Age-gender combinations:
  age_groups <- c("A15-A34", "A35-A59", "A60-A79", "A80+")
  genders <- c("M", "W")
  
  # Restrict data to analysis period:
  data <- data %>%
    mutate(day = interval(T_0, date) %/% days(1)) %>%
    filter(date >= T_0, date < T_0 + T_max, age_group %in% age_groups,
           gender %in% genders) %>%
    mutate(age_group = droplevels(age_group),
           gender = droplevels(gender),
           district = droplevels(district))
  
  # Prepare data for model fitting:
  data_model <- data %>% filter(age_group %in% age_groups) %>%
    group_by(date, day, district, age_group, gender) %>%
    dplyr::summarize(hosp = pmax(n(), 0))
  colnames(data_model) <- c("date", "day", "district", "age_group", "gender",
                            "hosp")
  
  # Manual correction of names of districts:
  districts$district[grepl(pattern = "Landeshauptstadt",
                           x = districts$district)] <- "SK M\u00fcnchen"
  districts$district[districts$district == "LK Landsberg am Lech"] <- "LK Landsberg a.Lech"
  districts$district[grepl(pattern = "Kempten", x = districts$district)] <- "SK Kempten"
  districts$district[districts$district == "LK Lindau (Bodensee)"] <- "LK Lindau"
  districts <- districts %>% mutate(district = as.factor(district))
  
  # Add count 0 if not all districts have an observation within the last T_max
  # days:
  if (length(unique(data_model$district)) != nrow(districts)) {
    missing_Ids <- setdiff(x = districts$district,
                           y = unique(data_model$district))
    data_model <- bind_rows(data_model, 
                            tibble(districtId = missing_Ids, day = 0, 
                                   age = "A15-A34", gender = "männlich",
                                   hosp = 0))
  }
  
  # Addd count 0 if not all days have an observation:
  if (length(unique(data_model$day)) != T_max){
    missing_days <- setdiff(0:(T_max - 1), unique(data_model$day))
    data_model <- bind_rows(data_model,
                            tibble(district = "LK Aichach-Friedberg", 
                                   day = missing_days, age = "A15-A34", 
                                   gender = "männlich", deaths = 0, cases = 0)) 
  }
  
  # Data preparation for modelling:
  data_model <- data_model %>% ungroup() %>%
    complete(date, district, age_group, gender, fill = list(hosp = 0)) %>%
    mutate(age_60 = if_else(age_group %in% c("A15-A34", "A35-A59"),
                            "0-59", "60+"),
           gender = as.factor(gender),
           weekday = lubridate::wday(x = date, label = TRUE, abbr = FALSE,
                                     locale = locale_weekday),
           day = interval(T_0, date) %/% days(1))
  levels(data_model$age_60) <- c("0-59", "60+")
  
  # Match hospitalization and district data:
  data_model <- dplyr::full_join(data_model, districts, by = "district") %>%
    mutate(weekday = as.factor(weekday),
           gender = as.factor(gender),
           age_group = as.factor(age_group))
  class(data_model$weekday) <- "factor"
  
  # Add F = 1 to all data: 
  data_model <- data_model %>%
    mutate(F_t = 1, age_60 = as.character(age_60))
  
  # Add nowcasting estimates:
  if (nowcast == TRUE) {
    data_nowcast <- read_csv2(file = paste0(path, "/03_Results/Models/nowcasting_results_rki_",
                                            doa, ".csv")) %>%
      filter(age_group != "all")
    colnames(data_nowcast)[2] <- "age_60"
    data_model <- dplyr::left_join(data_model, data_nowcast) %>%
      mutate(F_t = F_est, age_60 = as.factor(age_60))
  }
  data_model <- data_model %>%
    mutate(district = as.factor(district), age_group = as.factor(age_group))
  data_model$weekday <- factor(x = data_model$weekday,
                               levels = c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday", "Saturday",
                                          "Sunday"))
  
  # Fitting of the model:
  model <- bam(hosp ~ s(day, bs = "ps", k = 8) + s(lon, lat, k = 20) + 
                 s(district, bs = "re") +
                 age_group * gender + weekday + offset(log(pop * F_t)), 
               data = data_model, family = nb, nthreads = 40)
  
  # Add dates and underlying data to bam object:
  model$dates <- seq(from = as.Date(T_0), to = as.Date(doa - days(1)), by = 1)
  model$data <- data_model
  
  if (nowcast == TRUE) {
    nowcast <- "_nowcast"
  }
  else {
    nowcast <- ""
  }
  
  # save model in the folder
  saveRDS(model, file = paste0(path, "/03_Results/Models/hosp_model",
                               nowcast, "_rki_", doa, ".rds"))
  return(model)
}



# This function creates a data

preprocess_districts <- function(path) { 
  
  # read population and coordinates of districts
  coordinates <- read_excel(paste0(path, "/coordinates.xlsx"))
  population <- read_excel(paste0(path, "/population.xlsx"))
  pop_density <- read_excel(paste0(path, "/population_total.xlsx"))
  
  districts <- tibble(districtId = as.numeric(population$districtId[seq(1, nrow(population), 2)]),
                      pop = round(population$gesamt[seq(1, nrow(population), 2)]), 
                      pop_m = round(population$gesamt[seq(2, nrow(population), 2)]), 
                      pop_f = pop - pop_m,
                      pop_m_0.4 = round(rowSums(population[seq(2, nrow(population), 2), 5:9])),
                      pop_w_0.4 = round(rowSums(population[seq(1, nrow(population), 2), 5:9])) -
                        round(rowSums(population[seq(2, nrow(population), 2), 5:9])),
                      pop_m_5_14 = round(rowSums(population[seq(2, nrow(population), 2), 10:19])),
                      pop_w_5_14 = round(rowSums(population[seq(1, nrow(population), 2), 10:19])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 10:19])),
                      pop_m_15_34 = round(rowSums(population[seq(2, nrow(population), 2), 20:39])),
                      pop_w_15_34 = round(rowSums(population[seq(1, nrow(population), 2), 20:39])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 20:39])),
                      pop_m_35_59 = round(rowSums(population[seq(2, nrow(population), 2), 40:64])),
                      pop_w_35_59 = round(rowSums(population[seq(1, nrow(population), 2), 40:64])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 40:64])),
                      pop_m_60_79 = round(rowSums(population[seq(2, nrow(population), 2), 65:84])),
                      pop_w_60_79 = round(rowSums(population[seq(1, nrow(population), 2), 65:84])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 65:84])),
                      pop_m_80 = round(rowSums(population[seq(2, nrow(population), 2), 85:95])),
                      pop_w_80 = round(rowSums(population[seq(1, nrow(population), 2), 85:95])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 85:95]))) 
  
  
  # adapt district names to Bavarian naming scheme:
  coordinates$name_new <- ifelse(test = coordinates$Type %in% c("Kreis",
                                                                "Landkreis"),
                                 yes = paste0("LK ", coordinates$name),
                                 no = paste0("SK ", coordinates$name))
  
  # add coordinate information
  districts <- districts[order(districts$districtId), ] %>% 
    mutate(district = coordinates$name_new, 
           lon = as.numeric(coordinates$longitude), 
           lat = as.numeric(coordinates$latitude), 
           density = pop_density$perkm2)
  
  return(districts)
}

