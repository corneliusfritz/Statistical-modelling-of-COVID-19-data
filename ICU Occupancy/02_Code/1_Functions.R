################## Functions ########################

######## BreadMeetsBread ##############

bread.multinom_gam = function (x,...){
  if (!is.null(x$na.action)) 
    class(x$na.action) <- "omit"
  sx <- summary(x)
  # this n = 5200 is hard-coded (change if the size of the multinom-dataset changes)
  sx$cov.unscaled 
}

meat.multinom_gam = function (x,...){
  psi <- estfun(x, ...)
  k <- NCOL(psi)
  # n <- NROW(psi)
  rval <- crossprod(as.matrix(psi))
}

######## Model MGCV work around ##############

fit_bam_simultaneously<-function(train_data=train, formula=as.formula(
  stat ~ s(district_fac, bs = "re")+ 
    s(Lat,Long) + 
    log_G_6_lag_w_1+
    log_G_5_lag_w_1 +
    log_G_4_lag_w_1 + 
    log_G_3_lag_w_1 +
    betten_frei_avg_lag_w_1+
    betten_belegt_avg_lag_w_1)){
  
  
  train_data<-train_data[train_data$bed_tot!=1,]
  
  train_data$district_fac<-as.factor(train_data$districtId)
  
  train_extend<-cbind(rbind(cbind(ind=1, stat=1, train_data), 
                            cbind(ind=2, stat=1, train_data), 
                            cbind(ind=3, stat=0, train_data)),
                      weights=round(c(train_data$betten_frei_avg,
                                      train_data$betten_belegt_avg,
                                      train_data$faelle_covid_aktuell_avg)))
  
  
  ### scale for the forest plot ###
  
  names_scale<-c("Long", "Lat",unlist(strsplit(as.character(formula)[3], " " ))[
    grepl("_", unlist(strsplit(as.character(formula)[3], " ")))&
      !(grepl("district", unlist(strsplit(as.character(formula)[3], " "))))])
  
  train_1<-train_extend[train_extend$ind!=2,]
  train_2<-train_extend[train_extend$ind!=1,]
  
  
  for(i in 1:length(names_scale)){
    train_1[, names_scale[i]]<-as.numeric(scale(train_1[, names_scale[i]]))
    train_2[, names_scale[i]]<-as.numeric(scale(train_2[, names_scale[i]]))
  }
  
  
  mod1 = mgcv::bam(formula=formula, 
                   family=binomial,
                   data = train_1,
                   weights = weights, 
                   nthreads = 20, 
                   discrete = T)
  
  mod2 = mgcv::bam(formula=formula, 
                   family=binomial,
                   data = train_2,
                   weights = weights, 
                   nthreads = 20, 
                   discrete = T)
  
  
  mu_1 = predict(mod1,type = "response")
  mod1$residuals = 1/(mu_1*(1-mu_1))*(train_1$stat-mu_1)
  class(mod1) = c("multinom_gam",class(mod1))
  bread. <- bread.multinom_gam(mod1)
  meat. <- meat.multinom_gam(mod1)
  mod1$Vp = (bread. %*% meat. %*% bread.)
  
  mu_2 = predict(mod2,type = "response")
  mod2$residuals = 1/(mu_2*(1-mu_2))*(train_2$stat-mu_2)
  class(mod2) = c("multinom_gam",class(mod2))
  bread. <- bread.multinom_gam(mod2)
  meat. <- meat.multinom_gam(mod2)
  mod2$Vp = (bread. %*% meat. %*% bread.)
  
  xb_1<-predict(mod1,type = "link")
  xb_2<-predict(mod2,type = "link")
  
  prediction<-cbind(free=(exp(xb_1)/(1+exp(xb_1)+exp(xb_2))), 
                    non_cov=(exp(xb_2)/(1+exp(xb_1)+exp(xb_2))),
                    cov=(1/(1+exp(xb_1)+exp(xb_2))))
  
  return(list(ModelFree=mod1, ModelNonCov=mod2, Prediction=prediction, Explanatory_Vars=names_scale))
}




######## Read in  libraries ##############
library(sandwich) # For uncertainty
library(VGAM) # For the vgam models (LGL Predictions)
library(mgcv) # For the bam models
library(lubridate) # For changing the date 
library(data.table) # Cornelius preferred data type (better but not intuitive for me)
library(ggpubr) # Same theme for all plots ()
library(forestplot) # Forestplots
library(sf)
library(tidyverse) # For ggplot and dplyr
library(cowplot) # For plot grids
library(gratia) # For the variance of the random effects
library(data.table) # For the model comparison "4_Modell_comparison.R"
library(texreg) 
library(surveillance) #Permutation test 

cbPallette<-c(grey="#E69F00", orange="#999999", turquise="#56B4E9", green="#009E73", 
              yellow="#F0E442", darkblue="#0072B2", darkorange="#D55E00", oldrose="#CC79A7")

colors = c("gold", "#E18727FF","#0072B5FF", "#20854EFF","#374E55FF")

# Colour palette from http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/


################# Modell Comparison #####################################
multinom_info <- function(model, label, alt_names = NA) {
  s = summary(model)
  names = names(model$coefficients)
  to_exclude = grep(pattern = "district_fac|Lat|Intercept",x = names)
  names = names[-to_exclude]
  co <- model$coefficients[-to_exclude]
  se <- s$se[-to_exclude]
  
  if(is.na(alt_names)) {
    tr <- data.table(
      labeltext  = names,
      mean = co,
      est = as.character(round(co,digits = 2)),
      se = se,
      upper = co + qnorm(p = 1- 0.025) *se,
      lower = co - qnorm(p = 1- 0.025) *se, 
      group = label
    )
  } else {
    tr <- data.table(
      labeltext  = alt_names,
      mean = co,
      est = as.character(round(co,digits = 2)),
      se = se,
      upper = co + qnorm(p = 1- 0.025) *se,
      lower = co - qnorm(p = 1- 0.025) *se, 
      group = label
    )
  }
  
  
  return(tr)
}


# Prediction ---- 

estimate_model = function(formula_1, formula_2, train_1, train_2, test, y_test, bam = T) {
  
  if(bam){
    # Estimate Model for comparison free vs. covid-beds 
    mod1 = bam(formula = formula_1 , family=binomial,data = train_1,
               weights = weight, nthreads = 40, discrete = T)
    # Estimate Model for comparison non-covid vs. covid-beds 
    mod2 = bam(formula = formula_2, family=binomial,data = train_2,
               weights = weight, nthreads = 40, discrete = T)
  } else {
    # Estimate Model for comparison free vs. covid-beds 
    mod1 = glm(formula = formula_1 , family=binomial,data = train_1,
               weights = weight)
    # Estimate Model for comparison non-covid vs. covid-beds 
    mod2 = glm(formula = formula_2, family=binomial,data = train_2,
               weights = weight)
  }
  
  
  pred_1 = predict(mod1, newdata = test, type = "link")
  pred_2 = predict(mod2, newdata = test, type = "link")
  p_pred = cbind(exp(pred_1)/(1+exp(pred_1) + exp(pred_2)),exp(pred_2)/(1+exp(pred_1) + exp(pred_2)),
                 1 - exp(pred_1)/(1+exp(pred_1) + exp(pred_2)) - exp(pred_2)/(1+exp(pred_1) + exp(pred_2)))
  res = data.table(cbind(p_pred,y_test/rowSums(y_test)))
  names(res) = c("pred_1", "pred_2", "pred_3", "real_1", "real_2", "real_3")
  return( res)
}




