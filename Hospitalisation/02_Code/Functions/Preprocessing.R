# This function creates a data frame with one row for each district.
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



