### Code to perform the nowcasting analysis of hospitalizations 

# Loading packages and functions:
library(tidyverse)
library(magrittr)
library(lubridate)
library(mgcv)
library(readxl)
library(rgdal)
library(checkmate)
library(data.table)
library(tidyr) # due to use of the complete() function currently only until version 1.1.4
source("Hospitalisation/02_Code/Functions/Fitting_nowcast_model_rki.R")
source("Hospitalisation/02_Code/Functions/Fitting_hosp_model_rki.R")

# Setup:
doa <- as.Date("2021-11-19")
T_0 <- doa - days(56) # data of eight weeks
d_max <- 40

# Main analysis of the paper:

# Perform nowcasting:
nowcasting(T_0 = T_0, doa = doa, d_max = d_max, n = 10000,
           quantiles = c(0.025, 0.975), adjust_quantiles = TRUE,
           save_model = TRUE, save_results = TRUE)

# Run hospitalization model:
fit_hosp_model(T_0 = T_0, doa = doa)

# Plots and further results for publication:
source("Hospitalisation/02_Code/Plots_rki.R")


