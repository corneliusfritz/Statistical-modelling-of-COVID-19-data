############ Read in data ##########################
setwd(substr(dirname(rstudioapi::getActiveDocumentContext()$path),1,nchar(dirname(rstudioapi::getActiveDocumentContext()$path))-8))


train<-read.csv("01_Data/data_model/ICU_train.csv")
train$date<-as.Date(train$date)
Predic_data_tot<-read.csv("01_Data/data_model/ICU_allData.csv")
Predic_data_tot$date<-as.Date(Predic_data_tot$date)

################## Functions ########################
source("02_Code/1_Functions.R")

################## Exploratory Plots  ########################

######## Distribution Plot ##############


train%>%
  dplyr::group_by(date)%>%
  dplyr::summarize(noncov_beds=sum(betten_belegt_avg)/
                     sum(betten_frei_avg+faelle_covid_aktuell_avg+betten_belegt_avg),
                   cov_beds=sum(faelle_covid_aktuell_avg+betten_belegt_avg)/
                     sum(betten_frei_avg+faelle_covid_aktuell_avg+betten_belegt_avg),
                   free_beds=sum(betten_frei_avg+faelle_covid_aktuell_avg+betten_belegt_avg)/
                     sum(betten_frei_avg+faelle_covid_aktuell_avg+betten_belegt_avg))%>%
  ggplot()+  
  theme_pubr() +
  xlab("Date") +
  geom_area(mapping = aes(x = date, y = free_beds, fill="free")) +
  geom_area(mapping = aes(x = date, y = cov_beds, fill="covid")) +
  geom_area(mapping = aes(x = date, y = noncov_beds, fill="noncovid")) +
  ylab("Distribution of Beds (in %)") +
  scale_fill_manual(name = "",values = as.vector(cbPallette[c(7,6,5)]), labels = c("Free", "COVID", "Non-COVID"))+
  geom_area(mapping = aes(x = date, y = rep(1, length(date))), fill="white", alpha=0.3)




################## Fitting the model  ########################

fit_train<-fit_bam_simultaneously()

################## Saving the model  ########################
saveRDS(fit_train, "03_Results/model/fitted_model.RData")

