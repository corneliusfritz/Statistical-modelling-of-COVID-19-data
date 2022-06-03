
# 1. Get Data----


library(tidyverse)
library(data.table)
library(gridExtra)
library(grid)
library(ggpubr)
library(data.table)
library(cowplot)
library(lubridate)

rm(list=ls(all=TRUE))

clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("01_Data/bayern_data_long.RData")
# load("01_Data/simulated_bayern_data_long.RData")

# Get year of the dates 
data_long$year = data.table::year(data_long$date)
# Restrict study to 2021
data_long = data_long[year == "2021"]
data_long$week = epiweek(data_long$date)
# Delete observations of patients below 5 years and where the age is not known
data_long = data_long[Altersgruppe != "unbekannt"]
data_long = data_long[Altersgruppe != "1"]
# Depending on the analysis that should be replicated 
# (main analysis or analysis shwon in the Supplementary Material)
# either subtract here 5 or not (-5 = SM).
data_long = data_long[week %in% (7:12)]
# data_long = data_long[week %in% (7:12-5)]
# make the state (bundesland) a factor
data_long$bundesland = factor(data_long$bundesland)
# Turn the data from NUTS3 to NUTS1 level 
state_data = data_long[,.(AnzahlFall = sum(AnzahlFall), 
                          long  = mean(long),
                          lat = mean(lat), 
                          pop = sum(pop)/length(unique(date))), 
                       by = .(Landkreis, week, Altersgruppe,label)]
# Generate the offset to controll for the different exposures 
state_data$offset = log(state_data$pop)
data_long$offset = log(data_long$pop)
# Split Data according to Age 
split_data = split(state_data,f = factor(state_data$Altersgruppe))
# Calculate the increase per age/gender group
lapply(split_data, function(x){
  x[, Anzahl_Fall_minus_one:=c(NA,AnzahlFall[-length(AnzahlFall)]), by=Landkreis]
  x[, Anzahl_Fall_minus_two:=c(NA,NA,AnzahlFall[-c(length(AnzahlFall)-1,length(AnzahlFall))]), by=Landkreis]
})
complete_data = split_data[[1]]
# Note that the name of the age groups do NOT correspond to the ages that are really in those age groups 
# (see the main manuscript for the definition of the age groups)
names(complete_data)[c(5,8,10,11)] = c("Anzahl_05_14", "Pop_05_14","Anzahl_05_14_minus","Anzahl_05_14_minus_two")
complete_data$Rate_05_14 = (complete_data$Anzahl_05_14+1)/complete_data$Pop_05_14
complete_data$Change_05_14=  (complete_data$Anzahl_05_14 +1)/ (complete_data$Anzahl_05_14_minus +1)
complete_data$Change_05_14_minus =  (complete_data$Anzahl_05_14_minus +1)/ 
  (complete_data$Anzahl_05_14_minus_two +1)
complete_data$Rate_05_14_minus = (complete_data$Anzahl_05_14_minus+1)/complete_data$Pop_05_14

# Do the same as above just for the second age group
complete_data$Anzahl_15_34 = split_data[[2]]$AnzahlFall
complete_data$Anzahl_15_34_minus = split_data[[2]]$Anzahl_Fall_minus_one
complete_data$Anzahl_15_34_minus_two = split_data[[2]]$Anzahl_Fall_minus_two
complete_data$Pop_15_34 = split_data[[2]]$pop
complete_data$Rate_15_34 =( complete_data$Anzahl_15_34+1)/complete_data$Pop_15_34
complete_data$Change_15_34 =  (complete_data$Anzahl_15_34 +1)/ (complete_data$Anzahl_15_34_minus +1)
complete_data$Change_15_34_minus =  (complete_data$Anzahl_15_34_minus +1)/ (complete_data$Anzahl_15_34_minus_two +1)
complete_data$Rate_15_34_minus = (complete_data$Anzahl_15_34_minus+1)/complete_data$Pop_15_34

complete_data$Anzahl_35_59 = split_data[[3]]$AnzahlFall
complete_data$Anzahl_35_59_minus = split_data[[3]]$Anzahl_Fall_minus_one
complete_data$Anzahl_35_59_minus_two = split_data[[3]]$Anzahl_Fall_minus_two
complete_data$Pop_35_59 = split_data[[3]]$pop
complete_data$Rate_35_59 = (complete_data$Anzahl_35_59+1)/complete_data$Pop_35_59
complete_data$Change_35_59 =  complete_data$Anzahl_35_59/ complete_data$Anzahl_35_59_minus
complete_data$Change_35_59_minus =  (complete_data$Anzahl_35_59_minus+1)/ (complete_data$Anzahl_35_59_minus_two+1)
complete_data$Rate_35_59_minus = (complete_data$Anzahl_35_59_minus+1)/complete_data$Pop_35_59

complete_data$Anzahl_60_79= split_data[[4]]$AnzahlFall
complete_data$Anzahl_60_79_minus= split_data[[4]]$Anzahl_Fall_minus_one
complete_data$Anzahl_60_79_minus_two= split_data[[4]]$Anzahl_Fall_minus_two
complete_data$Pop_60_79= split_data[[4]]$pop
complete_data$Rate_60_79 = (complete_data$Anzahl_60_79+1)/complete_data$Pop_60_79
complete_data$Change_60_79 =  (complete_data$Anzahl_60_79 +1)/( complete_data$Anzahl_60_79_minus +1)
complete_data$Change_60_79_minus = ( complete_data$Anzahl_60_79_minus+1)/ (complete_data$Anzahl_60_79_minus_two+1)
complete_data$Rate_60_79_minus = (complete_data$Anzahl_60_79_minus+1)/complete_data$Pop_60_79

complete_data$Anzahl_80= split_data[[5]]$AnzahlFall
complete_data$Anzahl_80_minus= split_data[[5]]$Anzahl_Fall_minus_one
complete_data$Anzahl_80_minus_two= split_data[[5]]$Anzahl_Fall_minus_two
complete_data$Pop_80= split_data[[5]]$pop
complete_data$Rate_80 = (complete_data$Anzahl_80+1)/complete_data$Pop_80
complete_data$Rate_80_minus = (complete_data$Anzahl_80_minus+1)/complete_data$Pop_80
complete_data$Change_80 =  (complete_data$Anzahl_80+1)/ (complete_data$Anzahl_80_minus+1)
complete_data$Change_80_minus =  (complete_data$Anzahl_80_minus+1)/ (complete_data$Anzahl_80_minus_two+1)

complete_data = complete_data[!is.na(Anzahl_80_minus_two)]
complete_data$bundesland= district_data$bundesland[match(complete_data$Landkreis, district_data$name_rki)]

# Calculate all models ----

model_rate_rate_05_14= glm.nb(formula = Anzahl_05_14+1~ log(Rate_05_14_minus*10000)+ log(Rate_15_34_minus*10000)+ log(Rate_35_59_minus*10000)  + log(Rate_60_79_minus*10000)+ 
                             log(Rate_80_minus*10000)  + factor(week)+ offset(log(Pop_05_14/10000)),data =complete_data)
model_rate_rate_15_34= glm.nb(formula = Anzahl_15_34~ log(Rate_05_14_minus*10000)+ log(Rate_15_34_minus*10000)+ log(Rate_35_59_minus*10000)  + log(Rate_60_79_minus*10000)+ 
                             log(Rate_80_minus*10000)  + factor(week)+ offset(log(Pop_15_34/10000)),data =complete_data)
model_rate_rate_35_59 = glm.nb(formula = Anzahl_35_59~  log(Rate_05_14_minus*10000)+log(Rate_15_34_minus*10000) + log(Rate_35_59_minus*10000) + log(Rate_60_79_minus*10000) +
                              log(Rate_80_minus*10000)  +factor(week)+  offset(log(Pop_35_59/10000)),data =complete_data)
model_rate_rate_60_79 = glm.nb(formula = Anzahl_60_79~  log(Rate_05_14_minus*10000)+log(Rate_15_34_minus*10000) + log(Rate_35_59_minus*10000) + log(Rate_60_79_minus*10000) +
                              log(Rate_80_minus*10000)  +factor(week)+  offset(log(Pop_60_79/10000)),data =complete_data)
model_rate_rate_80 = glm.nb(formula = Anzahl_80~  log(Rate_05_14_minus*10000)+log(Rate_15_34_minus*10000) + log(Rate_35_59_minus*10000) + log(Rate_60_79_minus*10000) +
                           log(Rate_80_minus*10000)  + factor(week)+ offset(log(Pop_80/10000)),data =complete_data)
# Compile the information on the std errors of all models
summary_model_rate_rate_05_14 = summary(model_rate_rate_05_14)
model_rate_rate_05_14$cov = summary_model_rate_rate_05_14$cov.scaled
summary_model_rate_rate_15_34 = summary(model_rate_rate_15_34)
model_rate_rate_15_34$cov = summary_model_rate_rate_15_34$cov.scaled
summary_model_rate_rate_35_59 = summary(model_rate_rate_35_59)
model_rate_rate_35_59$cov = summary_model_rate_rate_35_59$cov.scaled
summary_model_rate_rate_60_79 = summary(model_rate_rate_60_79)
model_rate_rate_60_79$cov = summary_model_rate_rate_60_79$cov.scaled
summary_model_rate_rate_80 = summary(model_rate_rate_80)
model_rate_rate_80$cov = summary_model_rate_rate_80$cov.scaled

coef_complete = data.table(coef = c(coef( model_rate_rate_05_14),
                                    coef( model_rate_rate_15_34),
                                    coef( model_rate_rate_35_59),
                                    coef( model_rate_rate_60_79),
                                    coef( model_rate_rate_80)), 
                           names = rep(names(coef( model_rate_rate_15_34)), 5), 
                           var = c(diag( model_rate_rate_05_14$cov),
                                   diag( model_rate_rate_15_34$cov),
                                   diag( model_rate_rate_35_59$cov),
                                   diag( model_rate_rate_60_79$cov),
                                   diag( model_rate_rate_80$cov)), 
                           target_age = rep(rep(c("05-14","15_34","35-59","60-79","80"), each = length(coef( model_rate_rate_05_14))),1))

coef_complete = coef_complete[names != "(Intercept)"]
coef_complete$upper = coef_complete$coef + qnorm(p = 0.975)*sqrt(coef_complete$var)
coef_complete$lower = coef_complete$coef - qnorm(p = 0.975)*sqrt(coef_complete$var)
ind_time = grep(pattern = "week",coef_complete$names)
coef_complete = coef_complete[-ind_time]
age_labels = c("5-11","12-20","21-39","40-65",">65")
age_labels_model = c("Model \n5-11","Model \n12-20","Model \n21-39","Model \n40-65","Model \n>65")
colors = c("gold", "#E18727FF","#0072B5FF", "#20854EFF","#374E55FF")
coef_complete$shape = 19
coef_complete$shape[c(1,7,13,19,25)] = 17

coef_complete$size = 1.5
coef_complete$size[c(1,7,13,19,25)] = 1.7


pdf("03_Plots/figure_2.pdf",width = 11.5,height = 7)

a = ggplot(data = coef_complete) +
  geom_hline(yintercept = 0,lty = 2)+
  geom_pointrange(aes(x = target_age, y = coef, ymin = lower, ymax = upper, color =names) ,
                  alpha = 1,position =  position_dodge(width=1),size = coef_complete$size,
                  shape=coef_complete$shape) + 
  theme_pubr(base_size = 20) +
  geom_vline(xintercept = c(1.5:15.5),lty = 1,color = "grey")+
  xlab("Target: Incidences of the current week") + 
  ggtitle("")+
  scale_color_manual(values = colors,label = age_labels, name = "Incidences of the previous week in the age group:")+
  scale_x_discrete(label = age_labels_model)+
  ylab(expression(paste(theta["a,k"], " with 95% CI"))) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 17),
        legend.position="bottom",
        axis.ticks.x  = element_blank(),
        axis.line.x = element_blank())

a
dev.off()
