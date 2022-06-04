# This function reads the raw RKI data, gives suitable column names
# and saves the table as tibble

# Input:
# - all: if TRUE, all raw data available are being read and saved,
# if FALSE (default), only datasets which have not been read yet
# will be read. Setting "all" to TRUE should be avoided.
# - base_date: optional date of dataset used as a base file. Later occurred
# hospitalization are added to that data.

# Output: none

read_lgl_data <- function(all = FALSE, base_date = NULL,
                          path = "Hospitalisation/01_Data") {
  
  # Check if folders with LGL datasets and formatted data exist:
  if (!dir.exists(paste0(path, "/Data_LGL"))) {
    dir.create(paste0(path, "/Data_LGL"))
  }
  if (!dir.exists(paste0(path, "/Data_formatted"))) {
    dir.create(paste0(path, "/Data_formatted"))
  }
  
  
  # Get all filenames and the respective dates from the original LGL datasets:
  files_lgl <- list.files(path = paste0(path, "/Data_LGL"))
  dates_lgl <- substr(x = files_lgl, start = 1, stop = 10)
  dates_lgl <- as.Date(gsub(x = dates_lgl, pattern = "_", replacement = "-"))
  
  # Get all filenames and the respective dates from the data that already have
  # been preprocessed:
  files_read <- list.files(path =  paste0(path, "/Data_formatted"))
  dates_read <- as.Date(substr(x = files_read, start = 1, stop = 10))
  
  if (!all){
    # only preprocess if data has not been preprocessed yet
    files_lgl <- files_lgl[which(!is.element(dates_lgl, dates_read))]
  }
  
  for (file in files_lgl) {
    
    # Print status of function:
    print(paste0("Preprocessing file ", file))
    
    # Read lgl data:
    data_lgl <- read_delim(file = paste0(path, "/Data_LGL/", file),
                           delim = ";", locale = locale(encoding = "ISO-8859-1"))
    
    # Filter observations with hospitalization:
    data_lgl <- as.tibble(data_lgl) %>%
      dplyr::filter(HospitalisierungStatus == "Ja")
    
    # Extract reporting date:
    reporting_date <- substr(x = file, start = 1, stop = 10)
    reporting_date <- as.Date(gsub(x = reporting_date, pattern = "_",
                                   replacement = "-"))
    
    # Read base data for newer data than 2021-10-29:
    if (reporting_date > as.Date("2021-10-29")) {
      data_base <- read_delim(file = paste0(path, "/Data_LGL/",
                                            "2021_10_29_8Uhr_IfSG_Daten.csv"),
                              delim = ";",
                              locale = locale(encoding = "ISO-8859-1")) %>%
        as.tibble() %>% dplyr::filter(HospitalisierungStatus == "Ja")
      
      # Check if the data contains hospitalizations not included in the current
      # data at day of analysis:
      data_base <- data_base %>%
        filter(!(InterneRef %in% data_lgl$InterneRef)) %>%
        filter(as.Date(Meldedatum) < as.Date(reporting_date) %m-% days(87))
      
      data_lgl <- bind_rows(data_base, data_lgl)
    }
    
    # Prepare date columns:
      data_lgl <- data_lgl %>%
          mutate(Meldedatum = as.Date(Meldedatum), AtLS = as.Date(AtLS),
                 AtGA = as.Date(AtGA))

    # save prepocessed dataset
    saveRDS(object = data_lgl, file = paste0(path, "/Data_formatted/", 
                                           as.character(reporting_date),
                                           "_hosp_Bavarian.rds"))
  }
}


# This function creates a data.frame containing all information from the
# individual datasets;
prepare_lgl_data <- function(path = "Hospitalisation/01_Data", seed = 3456,
                             doa = NULL) {
  
  # Read all LGL data sets:
  set.seed(seed)
  files <- list.files(paste0(path, "/Data_formatted/"))
  start_date <- substr(x = files[1], start = 1, stop = 10)
  start_date <- as.Date(gsub(x = start_date, pattern = "_",
                                 replacement = "-"))
  if (is.null(doa)) {
    end_date <- substr(x = files[length(files)], start = 1, stop = 10)
    end_date <- as.Date(gsub(x = end_date, pattern = "_",
                             replacement = "-"))
  }
  else {
    end_date <- doa
  }
  file_enddate <- paste0(end_date, "_hosp_Bavarian.rds")
  files <- files[1:which(files == file_enddate)]
  
  # Check if file with end_date already exists:
  if (!file.exists(paste0(path, "/Data_prepared/hosp_prepared_",
                           end_date, ".rds"))) {
    data_list <- lapply(X = seq_along(files), FUN = function(i) {
      
      # Extract reporting date:
      reporting_date <- substr(x = files[i], start = 1, stop = 10)
      reporting_date <- as.Date(gsub(x = reporting_date, pattern = "_",
                                     replacement = "-"))
      
      # Read data:
      data_day <- readRDS(paste0(path, "/Data_formatted/", files[i]))
      
      # Comparison with previous data set:
      if (i > 1) {
        # Exclude already known data:
        data_previous <- readRDS(paste0(path, "/Data_formatted/", files[i - 1]))
        data_day <- data_day %>%
          filter(!(InterneRef %in% data_previous$InterneRef))
      }
      
      # Prepare date variables:
      data_day <- data_day %>%
        mutate(Meldedatum_Hosp = reporting_date)
      
      if (i > 1) {
        
        # Number of days between current and previous data:
        date_previous <- as.Date(substr(x = files[i - 1], start = 1, stop = 10))
        date_gap <- as.numeric(reporting_date - date_previous) 
        
        # Randomly assign the three days before as registration date if 
        # difference between data is more than a single day.
        
        # Single day (e.g. a holiday during the week):
        if (date_gap == 2 & as.character(data_day$Meldedatum_Hosp[1]) != "2021-01-07") {
          data_day$Meldedatum_weekend <- data_day$Meldedatum_Hosp
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(2)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(2)]),
                   replace = TRUE, prob = c(1/3, 2/3))
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(3)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(3)]),
                   replace = TRUE, prob = c(2/5, 3/5))
          data_day$Meldedatum_weekend[data_day$Meldedatum <= data_day$Meldedatum_Hosp - days(4)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum <= data_day$Meldedatum_Hosp - days(4)]),
                   replace = TRUE, prob = c(1/2, 1/2))
          data_day$Meldedatum_Hosp <- data_day$Meldedatum_weekend
          data_day <- data_day %>% select(-Meldedatum_weekend)
        }
        
        # 2 days (usually the case for a weekend):
        if (date_gap == 3) {
          data_day$Meldedatum_weekend <- data_day$Meldedatum_Hosp
          # Meldedatum "Saturday":
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(2)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(2)]),
                   replace = TRUE, prob = c(1/3, 2/3))
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(3)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1),
                         data_day$Meldedatum_Hosp[1] - days(2)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(3)]),
                   replace = TRUE, prob = c(1/6, 1/3, 1/2))
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(4)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1),
                         data_day$Meldedatum_Hosp[1] - days(2)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(4)]),
                   replace = TRUE, prob = c(1/4, 1/4, 1/2))
          data_day$Meldedatum_weekend[data_day$Meldedatum <= data_day$Meldedatum_Hosp - days(5)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1),
                         data_day$Meldedatum_Hosp[1] - days(2)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum <= data_day$Meldedatum_Hosp - days(5)]),
                   replace = TRUE, prob = c(1/3, 1/3, 1/3))
          data_day$Meldedatum_Hosp <- data_day$Meldedatum_weekend
          data_day <- data_day %>% select(-Meldedatum_weekend)
        }
        
        # 3 days (weekend + holiday):
        if (date_gap == 4) {
          data_day$Meldedatum_weekend <- data_day$Meldedatum_Hosp
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(2)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(2)]),
                   replace = TRUE, prob = c(1/3, 2/3))
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(3)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1),
                         data_day$Meldedatum_Hosp[1] - days(2)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(3)]),
                   replace = TRUE, prob = c(1/6, 1/3, 1/2))
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(4)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1),
                         data_day$Meldedatum_Hosp[1] - days(2), data_day$Meldedatum_Hosp[1] - days(3)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(4)]),
                   replace = TRUE, prob = c(1/12, 1/6, 1/3, 5/12))
          data_day$Meldedatum_weekend[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(5)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1),
                         data_day$Meldedatum_Hosp[1] - days(2), data_day$Meldedatum_Hosp[1] - days(3)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum == data_day$Meldedatum_Hosp - days(5)]),
                   replace = TRUE, prob = c(1/8, 1/8, 1/4, 1/2))
          data_day$Meldedatum_weekend[data_day$Meldedatum <= data_day$Meldedatum_Hosp - days(6)] <-
            sample(x = c(data_day$Meldedatum_Hosp[1], data_day$Meldedatum_Hosp[1] - days(1),
                         data_day$Meldedatum_Hosp[1] - days(2), data_day$Meldedatum_Hosp[1] - days(3)),
                   size = length(data_day$Meldedatum_Hosp[data_day$Meldedatum <= data_day$Meldedatum_Hosp - days(6)]),
                   replace = TRUE, prob = c(1/4, 1/4, 1/4, 1/4))
          data_day$Meldedatum_Hosp <- data_day$Meldedatum_weekend
          data_day <- data_day %>% select(-Meldedatum_weekend)
        }
        
      }
      
      data_day <- data_day %>% dplyr::select(InterneRef, Meldedatum_Hosp) 
      
      return(data_day)
    })
    
    
    # Combine all files to a single data frame:
    data_hosp <- bind_rows(data_list)
    
    # Read latest LGL data and merge data_hosp:
    files <- list.files(paste0(path, "/Data_formatted/"))
    data <- readRDS(paste0(path, "/Data_formatted/", file_enddate)) 
    data <- left_join(data, data_hosp) 
    
    # Use first hospitalization entry for duplicate cases:
    data <- data %>% group_by(InterneRef) %>% 
      filter(Meldedatum_Hosp == min(Meldedatum_Hosp)) %>%
      distinct
    
    # Create variable for hospitalization date (replace by Meldedatum if date
    # does not exist):
    data <- data %>%
      mutate(ExHosp_StayFrom1 = as.Date(ExHosp_StayFrom1),
             Hospdatum = if_else(condition = is.na(ExHosp_StayFrom1),
                                 true = Meldedatum,
                                 false = ExHosp_StayFrom1),
             # Data for sensitivity analysis:
             Hospdatum_sens = if_else(condition = is.na(ExHosp_StayFrom1),
                                      true = if_else(condition = Meldedatum_Hosp <= Meldedatum + days(7),
                                                     true = Meldedatum_Hosp - days(1),
                                                     false = Meldedatum + days(7)),
                                      false = ExHosp_StayFrom1))
    
    # Save created data set:
    if (!dir.exists(paste0(path, "/Data_prepared"))) {
      dir.create(paste0(path, "/Data_prepared"))
    }
    saveRDS(object = data,
            file = paste0(path, "/Data_prepared/hosp_prepared_",
                          end_date, ".rds"))
  }
}

# This function prepares a dataset for evaluation containing nowcasting results
# and the reported hospitalised cases after 40 days:
prepare_evaluation_data <- function(doa, d_max, base = "Hospdatum",
                                    path = "Hospitalisation") {
  
  # Read nowcasting results:
  nowcasting <- read_csv2(paste0(path,
                                 "/03_Results/Models/nowcasting_results_",
                                 base, "_", doa, ".csv")) %>%
    mutate(age60 = as.factor(age60))
  
  # Vectors of nowcast and evaluation dates:
  dates_nowcast <- seq(from = doa - days(d_max - 1), to = doa - days(1),
                       by = "days")
  dates_evaluate <- dates_nowcast + days(40)
  
  data_list <- lapply(X = seq_along(dates_evaluate), FUN = function(i) {
    
    print(dates_evaluate[i])
  
    # Check if an LGL data set is available for the evaluation date:
    file_available <- file.exists(paste0(path, "/01_Data/Data_formatted/",
                                        dates_evaluate[i],
                                        "_hosp_Bavarian.rds"))
    
    # Choose next available LGL file for evaluation:
    if (file_available == FALSE) {
      files <- list.files(path = paste0(path, "/01_Data/Data_formatted"))
      dates <- substr(x = files, start = 1, stop = 10)
      dates <- as.Date(gsub(x = dates, pattern = "_", replacement = "-"))
      dates_evaluate[i] <- dates_evaluate[i] +
        days(min(dates[dates > dates_evaluate[i]] - dates_evaluate[i],
                 na.rm = TRUE))
    }
      
      # Preprocessing of hospitalisation data for evaluation date:
      if (!file.exists(paste0(path, "/01_Data/Data_prepared/hosp_prepared_",
                              dates_evaluate[i], ".rds"))) {
        prepare_lgl_data(doa = dates_evaluate[i])
      }
      
      # Read preprocessed data and extract the reported number of
      # hospitalisations for the evaluation date:
      data <- readRDS(paste0(path, "/01_Data/Data_prepared/hosp_prepared_",
                                  dates_evaluate[i], ".rds")) %>%
        filter(Hospdatum <= dates_nowcast[i],
               Hospdatum > dates_nowcast[i] - days(7))
      data <- data %>%
        mutate(age_group = case_when(AlterBerechnet < 60 ~ "0-60",
                                     AlterBerechnet >= 60 ~ "60+"),
               age_group = factor(x = age_group,
                                  levels = c("0-60", "60+"))) %>%
        group_by(age_group) %>% summarise(realized7 = n())
      data_hosp <- bind_rows(data,
                             data.frame("age_group" = "alle",
                                        "realized7" = sum(data$realized7))) %>%
        mutate(date = dates_nowcast[i]) %>%
        dplyr::select(date, age_group, realized7)
      return(data_hosp)
  })
  
  # Combine all data to a single data file:
  data_hosp <- bind_rows(data_list) %>% arrange(desc(date))
  colnames(data_hosp) <- c("date", "age60", "realized7")
  nowcasting <- left_join(nowcasting, data_hosp)
  
  # Save results:
  readr::write_csv2(nowcasting,
                    paste0(path, "/../Hospitalisation/03_Results/Models/nowcasting_results_eval_",
                           base, "_", doa, ".csv"))
}



# This function creates a data frame that with one row for each district. 
# The columns contain 
# - the district names and Ids, 
# - the gender/age group specific population sizes
# - the coordinates of the centroids of the districts
# - the population density of the districts (not used in the analyses)
# Input: none
# Output: the data frame described above

preprocess_districts <- function(path = "Hospitalisation/01_Data/Data_demographics") { 
  
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



