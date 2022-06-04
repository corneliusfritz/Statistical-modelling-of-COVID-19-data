# This function fits the hospitalisation model described in Section 4.2.

# Input:
# - doa: day of analysis
# - T.0: first registration date to be considered in the nowcast, corresponds to t = 0
# - d.max: maximum duration time that is assumed, see Section 4
# - re: which random effects should be used?
# - nowcast: Should the nowcast results
# - return.model: returns the model object if TRUE (default to FALSE)
# - print.effects: if TRUE (default to FALSE) prints the numbers
# - which are shown in Table 2

# Output: model object (if required)

fit_hosp_model <- function(doa, T_0, nowcast = TRUE, base = "Hospdatum",
                           path = "Hospitalisation") {
  
  # number of considered registration (and also reporting) dates
  T_max <- interval(T_0, doa) %/% days(1)
  
  # preprocess district data
  districts <- preprocess_districts()
  
  # restrict districts to Bavaria:
  districts <- districts %>%
    filter(districtId > 9000 & districtId < 10000)
  
  # gender-age combinations
  age_groups <- c("A15-A34", "A35-A59", "A60-A79", "A80+")
  genders <- c("M", "W")
  
  # Use english locale for weekday levels:
  if (Sys.info()['sysname'] != "Windows") {
    locale_weekday <- "en_US.UTF-8"
  }
  else {
    locale_weekday <- "English"
  }
  
  # Read data:
  file_doa <- paste0("hosp_prepared_", doa, ".rds")
  data <- readRDS(paste0(path, "/01_Data/Data_prepared/", file_doa))
  
  # Restrict data to analysis period:
  data <- data %>%
    mutate(base = !!sym(base), day = interval(T_0, !!sym(base)) %/% days(1)) %>%
    filter(base >= T_0, base < T_0 + T_max, Geschlecht != "-nicht ermittelbar-",
           Geschlecht != "-nicht erhoben-", Geschlecht != "divers")
  
  # Categorization of age into age groups:
  data <- data %>%
    mutate(age_group = case_when(AlterBerechnet <= 4 ~ "A00-A04",
                                 AlterBerechnet >= 5 & AlterBerechnet <= 15 ~
                                   "A05-A14",
                                 AlterBerechnet >= 15 & AlterBerechnet <= 34 ~
                                   "A15-A34",
                                 AlterBerechnet >= 35 & AlterBerechnet <= 59 ~
                                   "A35-A59",
                                 AlterBerechnet >= 60 & AlterBerechnet <= 79 ~
                                   "A60-A79",
                                 TRUE ~ "A80+"))
  
  # Prepare data for model fitting:
  data_model <- data %>%
    filter(age_group != "A00-A04", age_group != "A05-A14") %>%
    mutate(base = as.Date(base)) %>%
    group_by(base, day, MeldeLandkreis, age_group, Geschlecht) %>%
    dplyr::summarize(hosp = pmax(n(), 0))
  colnames(data_model) <- c("base", "day", "district", "age_group", "gender",
                            "hosp")
  
  # Manual correction of names of districts:
  districts$district[grepl(pattern = "Landeshauptstadt", x = districts$district)] <- "SK M\u00fcnchen"
  districts$district[districts$district == "LK Landsberg am Lech"] <- "LK Landsberg a.Lech"
  districts$district[grepl(pattern = "Kempten", x = districts$district)] <- "SK Kempten"
  districts$district[districts$district == "LK Lindau (Bodensee)"] <- "LK Lindau"
  
  # if not all districts have an observation within the last T.max days
  if (length(unique(data_model$district)) != nrow(districts)){
    missing_Ids <- setdiff(x = districts$district, y = unique(data_model$district))
    data_model <- bind_rows(data_model, 
                            tibble(districtId = missing_Ids, day = 0, 
                                   age = "A15-A34", gender = "männlich", hosp = 0))
  }
  
  # if not all days have an observation
  if (length(unique(data_model$day)) != T_max){
    missing_days <- setdiff(0:(T_max - 1), unique(data_model$day))
    data_model <- bind_rows(data_model,
                            tibble(district = "LK Aichach-Friedberg", 
                                   day = missing_days, age = "A15-A34", 
                                   gender = "männlich", deaths = 0, cases = 0)) 
  }
  
  # complete table for modelling:
  data_model <- data_model %>% ungroup() %>%
    complete(base, district, age_group, gender, fill = list(hosp = 0)) %>%
    mutate(age_60 = as.factor(1 * (age_group %in% c("A60-A79", "A80+"))),
           gender = as.factor(gender),
           age_group = as.factor(age_group),
           #district = as.factor(district),
           weekday = lubridate::wday(x = base, label = TRUE, abbr = FALSE,
                                    locale = locale_weekday),
           day = interval(T_0, base) %/% days(1))
  levels(data_model$age_60) <- c("0-60", "60+")
  
  # Match hospitalization and district data:
  data_model <- dplyr::full_join(data_model, districts, by = "district") %>%
    mutate(weekday = as.factor(weekday),
           gender = as.factor(gender),
           age_group = as.factor(age_group))
  class(data_model$weekday) <- "factor"
           
  # Add F = 1 to all data: 
  data_model <- data_model %>%
    mutate(F_t = 1, age_group = as.character(age_group))
  
  # nowcast
  if (nowcast == TRUE) {
    data_nowcast <- read_csv2(file = paste0(path, "/03_Results/Models/nowcasting_results_",
                                           base, "_", doa, ".csv")) %>%
      filter(age60 != "alle")
    colnames(data_nowcast)[1:2] <- c("base", "age_60")
    data_model <- dplyr::left_join(data_model, data_nowcast) %>%
      mutate(F_t = F_est, age_60 = as.factor(age_60),
             age_group = as.factor(age_group))
  }
  data_model <- data_model %>%
    mutate(district = as.factor(district))
  data_model$weekday <- factor(x = data_model$weekday,
                               levels = c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday", "Saturday",
                                          "Sunday"))
  
  # fit model
  model <- bam(hosp ~ s(day, bs = "ps", k = 8) + s(lon, lat, k = 20) + 
                   s(district, bs = "re") +
                   age_group * gender + weekday + offset(log(pop * F_t)), 
                 data = data_model, family = nb, nthreads = 40)
  
  # add registration dates and data to bam object 
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
                               nowcast, "_", base, "_", doa, ".rds"))
  return(model)
}





