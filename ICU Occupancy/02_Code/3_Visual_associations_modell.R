########  Read in model ##############

fit_train<-readRDS("03_Results/model/fitted_model.RData")

########  Forest plots for linear associations ##############

summ1<-summary(fit_train$ModelFree)
summ2<-summary(fit_train$ModelNonCov)
for_setup1<-cbind(y1=1:length(summ1$p.coeff[-1]), coeff=c(summ1$p.coeff[-1]),
                  lower=c(summ1$p.coeff[-1])-qnorm(p = 1- 0.025)*c(summ1$p.table[-1,2]),
                  higher=c(summ1$p.coeff[-1])+qnorm(p = 1- 0.025)*c(summ1$p.table[-1,2]))
for_setup2<-cbind(y2=1:length(summ2$p.coeff[-1])-0.25, coeff=c(summ2$p.coeff[-1]),
                  lower=c(summ2$p.coeff[-1])-qnorm(p = 1- 0.025)*c(summ2$p.table[-1,2]),
                  higher=c(summ2$p.coeff[-1])+qnorm(p = 1- 0.025)*c(summ2$p.table[-1,2]))
rownames(for_setup1)=rownames(for_setup2)<-c(
  "Lagged log incidences \n of 80+ year olds", 
  "Lagged log incidences \n of 60 to 79 year olds",
  "Lagged log incidences \n of 35 to 59 year olds",
  "Lagged log incidences \n of 15 to 34 year olds", 
  "Lagged Free Beds",
  "Lagged Non-Covid Beds")
for_setup1<-as.data.frame(for_setup1)
for_setup2<-as.data.frame(for_setup2)

ggplot()+
  geom_vline(aes(xintercept=0), lty=2, alpha=0.3, size=1.2)+
  geom_linerange(aes(x=for_setup1$lower, ymin=for_setup1$y1-0.1, ymax=for_setup1$y1+0.1, y=for_setup1$y1), alpha=0.8)+
  geom_linerange(aes(x=for_setup2$lower, ymin=for_setup2$y2-0.1, ymax=for_setup2$y2+0.1, y=for_setup2$y2), alpha=0.8)+
  geom_linerange(aes(x=for_setup1$higher, ymin=for_setup1$y1-0.1, ymax=for_setup1$y1+0.1, y=for_setup1$y1), alpha=0.8)+
  geom_linerange(aes(x=for_setup2$higher, ymin=for_setup2$y2-0.1, ymax=for_setup2$y2+0.1, y=for_setup2$y2), alpha=0.8)+
  geom_segment(aes(y=for_setup1$y1,yend=for_setup1$y1, x=for_setup1$lower, xend=for_setup1$higher), alpha=0.8)+
  geom_segment(aes(y=for_setup2$y2,yend=for_setup2$y2, x=for_setup2$lower, xend=for_setup2$higher), alpha=0.8)+
  geom_point(data=for_setup1, aes(x=coeff, y=y1, col="Free vs Covid Beds", shape="Free vs Covid Beds"), size=4)+
  geom_point(data=for_setup2, aes(x=coeff, y=y2, col="Non-Covid vs Covid Beds", shape="Non-Covid vs Covid Beds"), size=4)+
  theme_pubr(base_size = 20) +
  scale_y_continuous(name = " ",
                     labels = rownames(for_setup1),
                     breaks = for_setup1$y1-0.25) +
  scale_color_manual(name = "",values = as.vector(cbPallette[c(6,7)]), 
                     labels = c("Model of free beds vs COVID beds", "Model of Non-Covid beds vs COVID beds"))+
  scale_shape_manual(name = "",values = c(19,17), 
                     labels = c("Model of free beds vs COVID beds", "Model of Non-Covid beds vs COVID beds"))+
  xlab("Estimated coefficients with 95% Confidence-Interval")




########  Non-linear spatial effects: both smooth and district specific ##############


##### set up spatial data #####

#Read in and set up of spatial data
# load("01_Data/data_model/comparison_data.RData")
mod1<-fit_train$ModelFree
mod2<-fit_train$ModelNonCov
load("01_Data/data_model/district_data.RData")
load("01_Data/data_model/bundesland_data.RData")
district_data$bundesland = factor(district_data$bundesland)
names_tmp = names(coef(fit_train$ModelFree))
train$district_fac<-as.factor(train$districtId)
# Estimated coefficients 
random_coefs = coef(fit_train$ModelFree)[grep("district",x = names_tmp)]
district_data$random_1 = random_coefs[
  match(as.character(district_data$lk_id),levels(train$district_fac))]
random_coefs = coef(mod2)[grep("district",x = names_tmp)]

district_data$random_2 = random_coefs[
  match(as.character(district_data$lk_id),levels(train$district_fac))]
bundesland_data = st_set_crs(bundesland_data,value = "EPSG:4326")
data_tmp = Predic_data_tot[Predic_data_tot$date==(max(train$date)+7),]

names_scale<-c("Long", "Lat")
for(i in 1:length(names_scale)){
  data_tmp[, names_scale[i]]<-as.numeric(scale(data_tmp[, names_scale[i]]))
}

data_tmp$district_fac<-as.factor(data_tmp$districtId)
data_info = predict.gam(mod1,newdata = data_tmp, type = "terms",se=T)

spatial_coefs = as.data.frame(data_info$fit)$`s(Lat,Long)`
spatial_ses = as.data.frame(data_info$se.fit)$`s(Lat,Long)`
district_data$spatial_1 = spatial_coefs[match(as.character(district_data$lk_id),data_tmp$districtId)]



data_info = predict.gam(mod2,newdata = data_tmp, type = "terms",se = T)
spatial_coefs = as.data.frame(data_info$fit)$`s(Lat,Long)`
spatial_ses = as.data.frame(data_info$se.fit)$`s(Lat,Long)`
district_data$spatial_2 = spatial_coefs[match(as.character(district_data$lk_id),data_tmp$districtId)]




##### The random effects district specific #####

randeff1<-ggplot() +
  geom_sf(data = district_data, aes(fill = random_1), col = "grey30") +
  theme_pubr() +
  geom_sf(data = bundesland_data, aes(), col = "black", alpha = 0.00001) +
  theme(axis.ticks = element_blank(),
        axis.text =  element_blank(),
        strip.text.y = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        axis.line =  element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient2(name = "",low = "#4263f5",
                       midpoint = 0,mid = "white", high = "#f21111",na.value="grey",
                       guide = guide_colourbar( barwidth = 8))+ 
  theme(legend.position="bottom")+
  ggtitle("(a) Free beds vs occupied COVID \n beds")
#"#4263f5",midpoint = 0,mid = "white", high = "#f21111"
district_data$name_rki[which.min(district_data$random_1)]

randeff2<-ggplot() +
  geom_sf(data = district_data, aes(fill = random_2)) +
  theme_pubr() +
  geom_sf(data = bundesland_data, aes(), col = "black", alpha = 0.00001) +
  theme(axis.ticks = element_blank(),
        axis.text =  element_blank(),
        strip.text.y = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        axis.line =  element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient2(name = "",low = "#4263f5", high = "#f21111",na.value="grey",
                       guide = guide_colourbar( barwidth = 8))+ 
  theme(legend.position="bottom")+
  ggtitle("(b) Occupied non-COVID beds vs \n occupied COVID beds")




grid1<-plot_grid(randeff1, randeff2)


ggsave(grid1, file="03_Results/plots/District_fact.png",
       height=7, width=8)


##### The smooth penalized effects Longitude and Latitude #####

district_data$spatial_1[is.na(district_data$random_1)]<-NA
district_data$spatial_2[is.na(district_data$random_2)]<-NA
long_lat1<-ggplot() +
  geom_sf(data = district_data, aes(fill = spatial_1)) +
  theme_pubr() +
  geom_sf(data = bundesland_data, aes(), col = "black", alpha = 0.00001) +
  theme(axis.ticks = element_blank(),
        axis.text =  element_blank(),
        strip.text.y = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        axis.line =  element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient2(name = "", # option="G"
                       low = "#4263f5", high = "#f21111",na.value="grey",
                       guide = guide_colourbar( barwidth = 8))+
  theme(legend.position="bottom")+
  ggtitle("(a) Free beds vs occupied COVID \n beds")




long_lat2<-ggplot() +
  geom_sf(data = district_data, aes(fill = spatial_2)) +
  theme_pubr() +
  geom_sf(data = bundesland_data, aes(), col = "black", alpha = 0.00001) +
  theme(axis.ticks = element_blank(),
        axis.text =  element_blank(),
        strip.text.y = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        axis.line =  element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient2(name = "", # option="G"
                       low = "#4263f5", high = "#f21111",na.value="grey",
                       guide = guide_colourbar( barwidth = 8))+
  theme(legend.position="bottom")+
  ggtitle("(b) Occupied non-COVID beds vs \n occupied COVID beds")





grid2<-plot_grid(long_lat1, long_lat2)

ggsave(grid2, file="03_Results/plots/Smooth_pol.png",
       height=7, width=8)

##### Variance random effect #####

variance_comp(fit_train$ModelFree,coverage=0.95)
variance_comp(fit_train$ModelNonCov,coverage=0.95)




