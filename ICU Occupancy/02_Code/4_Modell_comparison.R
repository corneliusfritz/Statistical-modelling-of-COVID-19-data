setwd(substr(dirname(rstudioapi::getActiveDocumentContext()$path),1,nchar(dirname(rstudioapi::getActiveDocumentContext()$path))-8))

load("01_Data/data_model/longer_period_for_comparison.RData")

### Data Wrangling in Data Table rather than Tbl ###
Roll_base=0
summary_list<-list()
predict_list<-list()
fit_list<-list()
model_list<-list()


Predic_data_tot$month = months(Predic_data_tot$date)
Predic_data_tot$year = year(Predic_data_tot$date)
Predic_data_tot$week = epiweek(Predic_data_tot$date)
Predic_data_tot$week_fac = factor(Predic_data_tot$week)
Predic_data_tot$district_fac = factor(Predic_data_tot$districtId)
setDT(Predic_data_tot)



tmp_data = Predic_data_tot[,.(diff_6 =c(NA,diff(Log_Inzidenz_G_6_lag_w_1)), 
                              diff_5 =c(NA,diff(Log_Inzidenz_G_5_lag_w_1)), 
                              diff_4 =c(NA,diff(Log_Inzidenz_G_4_lag_w_1)),
                              diff_3 =c(NA,diff(Log_Inzidenz_G_3_lag_w_1))),by = districtId]
tmp_data$districtId = NULL
Predic_data_tot = cbind(Predic_data_tot, 
                        tmp_data)

Predic_data_tot = Predic_data_tot[bed_tot != 0]


### Set up for the prediction ###

for(i in 0:Roll_base){
  
  train<-Predic_data_tot[year == 2021 & month %in% c("March","April", "May","June")]
  train$district_fac = droplevels(train$district_fac)
  
  
  # test<-Predic_data_tot[Predic_data_tot$num_date==(max(Predic_data_tot$num_date)-i)&
  #                         Predic_data_tot$num_date>(max(Predic_data_tot$num_date)-i-9),]
  y_train<-round(cbind(train$betten_frei_avg,
                       train$betten_belegt_avg,
                       train$faelle_covid_aktuell_avg))
  
  
  # N_train = rowSums(y_train)
  
  train_new = rbind(train,train, train)
  y_train_new = c(y_train[,1],y_train[,2],y_train[,3])
  stat = c(rep(c(1,2,3),each = nrow(train)))
  train_new$stat = stat
  train_new$weight = y_train_new
  # stat = (stat-1)
  train_new$stat = stat
  
  # trying = gam(list(stat~week+ betten_belegt_avg_w_1+
  #                     Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
  #                     Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1 +
  #                     betten_frei_avg_w_1, 
  #                   ~ week), family=multinom(K = 2),data = train_new)
  # sandwich(trying)
  # 
  
  
  stat_1 = stat == 1
  stat_1[stat == 2] = NA
  stat_2 = stat == 2
  stat_2[stat == 1] = NA
  train_new$stat_2 = stat_2
  train_new$stat_1 = stat_1
  
  
  
  # Estimate Model for comparison free vs. covid-beds 
  train_1 = train_new[!is.na(stat_1)]
  # train_1 = train_new[!is.na(stat_2)]
  train_1_scale = train_1
  
  for(i in 5:20) {
    train_1_scale[,i] = scale(train_1[,i, with = F])
  }
  
  mod1 = bam(stat_1 ~ s(district_fac, bs = "re")+betten_frei_avg_w_1+ betten_belegt_avg_w_1+
               Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
               Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1 , family=binomial,data = train_1_scale,
             weights = weight, nthreads = 20, discrete = T)
  
  mu = predict(mod1,type = "response")
  mod1$residuals = 1/(mu*(1-mu))*(train_1$stat_1-mu)
  class(mod1) = c("multinom_gam",class(mod1))
  bread. <- bread.multinom_gam(mod1)
  meat. <- meat.multinom_gam(mod1)
  mod1$Vp = (bread. %*% meat. %*% bread.)
  
  # Estimate Model for comparison non-covid vs. covid-beds 
  train_2 = train_new[!is.na(stat_2)]
  # train_1 = train_new[!is.na(stat_2)]
  
  train_2_scale = train_2
  
  for(i in 5:20) {
    train_2_scale[,i] = scale(train_2[,i, with = F])
  }
  
  mod2 = bam(stat_2 ~  s(district_fac, bs = "re")+ betten_frei_avg_w_1+ betten_belegt_avg_w_1+
               Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
               Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1, family=binomial,data = train_2_scale,
             weights = weight, nthreads = 40, discrete = T)
  
  mu = predict(mod2,type = "response")
  mod2$residuals = 1/(mu*(1-mu))*(train_2$stat_2-mu)
  class(mod2) = c("multinom_gam",class(mod2))
  bread. <- bread.multinom_gam(mod2)
  meat. <- meat.multinom_gam(mod2)
  mod2$Vp = (bread. %*% meat. %*% bread.)
  
  train_scale = train
  for(i in 5:20) {
    train_scale[,i] = scale(train[,i, with = F])
  }
  pred_1 = predict(mod1, newdata = train_scale, type = "link")
  pred_2 = predict(mod2, newdata = train_scale, type = "link")
  p_pred = cbind(exp(pred_1)/(1+exp(pred_1) + exp(pred_2)),exp(pred_2)/(1+exp(pred_1) + exp(pred_2)),
                 1 - exp(pred_1)/(1+exp(pred_1) + exp(pred_2)) - exp(pred_2)/(1+exp(pred_1) + exp(pred_2)))
  
  
  
}


comparison_data = data.table(cbind(p_pred,y_train/rowSums(y_train)))
names(comparison_data) = c("pred_1", "pred_2", "pred_3", "real_1", "real_2", "real_3")
comparison_data= cbind(train,comparison_data)
comparison_data$pred_2_cumul = comparison_data$pred_2 + comparison_data$pred_1
comparison_data$real_2_cumul = comparison_data$real_2 + comparison_data$real_1
comparison_data$real_3_cumul = comparison_data$real_3 + comparison_data$real_2 + comparison_data$real_1

national_data = comparison_data[, .(pred_1 = mean(pred_1), 
                                    pred_2 = mean(pred_2_cumul), 
                                    real_3 = mean(real_3), 
                                    real_1 = mean(real_1), 
                                    real_2 = mean(real_2)), by = date]

national_data = melt.data.table(national_data,id.vars = "date")
national_data$is_real = grepl(pattern = "real", x = national_data$variable)
national_data$variable = factor(national_data$variable,levels =c("pred_1", "pred_2", "pred_3",  "real_2","real_3","real_1"))

### Prediction and Comparison Plot ###


max_week_2020 = max(Predic_data_tot$week[Predic_data_tot$year == 2020])
Predic_data_tot$week_new =Predic_data_tot$week
Predic_data_tot$week_new[Predic_data_tot$year == 2021] = Predic_data_tot$week_new[Predic_data_tot$year == 2021] + max_week_2020

train<-Predic_data_tot[year == 2021 & month %in% c("March","April", "May","June")]
start = min(train$week_new)
end = max(train$week_new)

index_full_model = c()
index_no_random = c()
index_no_infection = c()
index_no_ar = c()
index_intercept = c()
n = 1
times = c()

for(i in 40:-10){
  train<-Predic_data_tot[week_new %in% c((start- i):(end-i))]
  train$district_fac = droplevels(train$district_fac)
  test<-Predic_data_tot[week_new  == (end - i +1)]
  test$district_fac = droplevels(test$district_fac)
  times[n] = as.character(min(test$date))
  n = n +1
  y_train<-round(cbind(train$betten_frei_avg,
                       train$betten_belegt_avg,
                       train$faelle_covid_aktuell_avg))
  
  y_test<-round(cbind(test$betten_frei_avg,
                      test$betten_belegt_avg,
                      test$faelle_covid_aktuell_avg))
  
  # N_train = rowSums(y_train)
  
  train_new = rbind(train,train, train)
  y_train_new = c(y_train[,1],y_train[,2],y_train[,3])
  stat = c(rep(c(1,2,3),each = nrow(train)))
  train_new$stat = stat
  train_new$weight = y_train_new
  train_new$stat = stat
  
  stat_1 = stat == 1
  stat_1[stat == 2] = NA
  stat_2 = stat == 2
  stat_2[stat == 1] = NA
  train_new$stat_2 = stat_2
  train_new$stat_1 = stat_1
  
  formula_1 = stat_1 ~ s(district_fac, bs = "re")+ s(Lat, Long) + betten_frei_avg_w_1+ betten_belegt_avg_w_1+
    Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
    Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1 
  formula_2 = stat_2 ~  s(district_fac, bs = "re")+ s(Lat, Long) + betten_frei_avg_w_1+ betten_belegt_avg_w_1+
    Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
    Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1
  
  
  formula_1_no_infection = stat_1 ~ s(district_fac, bs = "re")+ s(Lat, Long) + betten_frei_avg_w_1+ betten_belegt_avg_w_1
  formula_2_no_infection = stat_2 ~  s(district_fac, bs = "re")+ s(Lat, Long) + betten_frei_avg_w_1+ betten_belegt_avg_w_1
  
  
  formula_1_no_random = stat_1 ~ betten_frei_avg_w_1+ betten_belegt_avg_w_1+
    Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
    Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1 
  formula_2_no_random = stat_2 ~  betten_frei_avg_w_1+ betten_belegt_avg_w_1+
    Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
    Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1
  
  formula_1_no_ar = stat_1 ~ s(district_fac, bs = "re")+ s(Lat, Long) + 
    Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
    Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1 
  formula_2_no_ar = stat_2 ~  s(district_fac, bs = "re")+ s(Lat, Long) + 
    Log_Inzidenz_G_6_lag_w_1+Log_Inzidenz_G_5_lag_w_1 +
    Log_Inzidenz_G_4_lag_w_1 + Log_Inzidenz_G_3_lag_w_1
  
  formula_1_intercept = stat_1 ~ 1
  formula_2_intercept = stat_2 ~ 1
  
  train_1 = train_new[!is.na(stat_1)]
  train_2 = train_new[!is.na(stat_2)]
  
  
  full_model = estimate_model(formula_1, formula_2, train_1, train_2, test, y_test)
  no_infection_model = estimate_model(formula_1_no_infection, formula_2_no_infection, train_1, train_2, test, y_test)
  no_random_model = estimate_model(formula_1_no_random, formula_2_no_random, train_1, train_2, test, y_test,bam = F)
  no_ar_full_model = estimate_model(formula_1_no_ar, formula_2_no_ar, train_1, train_2, test, y_test)
  intercept_model = estimate_model(formula_1_intercept, formula_2_intercept, train_1, train_2, test, y_test,bam = F)
  
  # dmultinom(x = cbind(test$betten_frei_avg[1:2],
  #                     test$betten_belegt_avg[1:2],
  #                     test$faelle_covid_aktuell_avg[1:2]), 
  #           prob = as.matrix(no_infection_model[1:2,1:3]), log = T)
  # 
  
  
  index_full_model = c(index_full_model,
                       -  sum(log((unlist(lapply(1:nrow(test), FUN = function(x){
                         dmultinom(x = cbind(test$betten_frei_avg[x],
                                             test$betten_belegt_avg[x],
                                             test$faelle_covid_aktuell_avg[x]), 
                                   prob = as.matrix(full_model[x,1:3]))
                       }))))))
  
  index_no_infection = c(index_no_infection,
                         -   sum(log((unlist(lapply(1:nrow(test), FUN = function(x){
                           dmultinom(x = cbind(test$betten_frei_avg[x],
                                               test$betten_belegt_avg[x],
                                               test$faelle_covid_aktuell_avg[x]), 
                                     prob = as.matrix(no_infection_model[x,1:3]))
                         }))))))
  index_no_random =  c(index_no_random,
                       -   sum(log((unlist(lapply(1:nrow(test), FUN = function(x){
                         dmultinom(x = cbind(test$betten_frei_avg[x],
                                             test$betten_belegt_avg[x],
                                             test$faelle_covid_aktuell_avg[x]), 
                                   prob = as.matrix(no_random_model[x,1:3]))
                       }))))))
  
  index_no_ar =  c(index_no_ar,
                   -  sum(log((unlist(lapply(1:nrow(test), FUN = function(x){
                     dmultinom(x = cbind(test$betten_frei_avg[x],
                                         test$betten_belegt_avg[x],
                                         test$faelle_covid_aktuell_avg[x]), 
                               prob = as.matrix(no_ar_full_model[x,1:3]))
                   }))))))
  index_intercept =   c(index_intercept,
                        -   sum(log((unlist(lapply(1:nrow(test), FUN = function(x){
                          dmultinom(x = cbind(test$betten_frei_avg[x],
                                              test$betten_belegt_avg[x],
                                              test$faelle_covid_aktuell_avg[x]), 
                                    prob = as.matrix(intercept_model[x,1:3]))
                        }))))))
  
  cat(i,"\n")
}

data_set = data.table(index_full_model, index_no_random,index_no_ar,index_intercept, index_no_infection, date = ymd(times))
library(surveillance)
test_scores = c("", round(permutationTest(data_set$index_full_model, data_set$index_no_random, nPermutation = 100000)$pVal.permut, digits = 3),
                round(  permutationTest(data_set$index_full_model, data_set$index_no_ar, nPermutation = 100000)$pVal.permut, digits = 3),
                round(permutationTest(data_set$index_full_model, data_set$index_intercept, nPermutation = 100000)$pVal.permut, digits = 3),
                round(permutationTest(data_set$index_full_model, data_set$index_no_infection, nPermutation = 100000)$pVal.permut, digits = 3))
test_scores[as.numeric(test_scores) == 0] = "(<0.0001)"
means = apply(as.matrix(data_set[,1:5]), 2, mean)

data_set = melt.data.table(data_set,id.vars = "date")
data_set$week = epiweek(data_set$date)
data_set$year = lubridate::year(data_set$date)
data_set$week_new = data_set$week
data_set$week_new[data_set$year == 2021] = data_set$week_new[data_set$year == 2021] + 53
data_set$variable = factor(data_set$variable, levels = c("index_full_model", "index_no_infection", "index_no_random", "index_no_ar", "index_intercept"))
labels_tmp = unique(data_set$week)
positions_tmp= unique(data_set$week_new)
labels_tmp[labels_tmp%% 10 != 0] = ""
labels_tmp[positions_tmp == 54] = "1\n2021"
labels_tmp[1] = paste0(labels_tmp[1],"\n2020")
labels_tmp[length(labels_tmp)] = max(data_set$week_new) - 53
levels(data_set$variable)
data_set$variable = factor(data_set$variable, levels = c("index_full_model", "index_no_ar","index_no_infection", "index_no_random", 
                                                         "index_intercept"))


vergleich<-ggplot(data_set, aes(x = week_new, y = value, color = factor(variable))) +
  geom_line(cex = 1) +
  # geom_hline(yintercept = means, cex = 0.8,
  #            color = c("gold", "#E18727FF","#0072B5FF", "#20854EFF","#374E55FF")[match(names(means),levels(data_set$variable))],
  #            lty = 2) + 
  ylab("Logarithmic Score") +
  xlab("Calendar week of test data") +
  scale_x_continuous(breaks = positions_tmp,labels = labels_tmp)+
  # scale_color_manual("Model\nAverage Score\np-value",values = c("gold", "#E18727FF","#0072B5FF", "#20854EFF","#374E55FF"),
  #                    label = paste0(c("Full model", "No smooths", "No AR(1)", "Intercept","No infection"),"\n",
  #                                  round(means, digits = 3),"\n",
  #                                  test_scores)[match(levels(data_set$variable),names(means))]) +
  scale_color_manual("Model",values = c("gold", "#E18727FF","#0072B5FF", "#20854EFF","#374E55FF"),
                     label = c("Full model", "Linear", "No AR(1)", "Intercept","No infection")[match(levels(data_set$variable),names(means))]) + 
  theme_pubr(base_size = 20) +
  theme( legend.title = element_text(size = 15), 
         legend.text  = element_text(size = 15), 
         panel.grid.major = element_line()
  )


ggsave("03_Results/plots/Camparison_Models.png", vergleich,
       height=7, width=8)
