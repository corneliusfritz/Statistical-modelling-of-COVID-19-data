### This file generates the plots of the main analysis of the hospitalisation
### section of the paper.

# Loading packages and functions:
library(tidyverse)
library(magrittr)
library(lubridate)
library(mgcv)
library(readxl)
library(rgdal)
library(checkmate)
library(data.table)
library(ggpubr)
library(scales)
library(sf)
library(viridisLite)
path_data <- "Hospitalisation/01_Data"
path_results <- "Hospitalisation/03_Results/Plots/Main_analysis"
if (Sys.info()['sysname'] != "Windows") {
  locale_weekday <- "en_US.UTF-8"
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
} else {
  Sys.setlocale("LC_TIME", "English")
}


################################################################################


# Read data and models:
doa <- as.Date("2021-11-19")
T_0 <- doa - days(56)
data_year <- readRDS(paste0(path_data, "/Data_prepared/hosp_prepared_",
                            doa, ".rds")) %>%
  filter(Meldedatum_Hosp <= doa, Hospdatum < doa)
data_model <- data_year %>% filter(Meldedatum_Hosp > T_0, )
nowcast_results <- read_csv2(paste0(path_results,
                                    "/../../Models/nowcasting_results_eval_Hospdatum_",
                                    doa, ".csv")) %>%
  filter(date >= T_0)
nowcast_model <- readRDS(paste0(path_results,
                                "/../../Models/nowcast_model_Hospdatum_",
                                doa, ".rds"))
hosp_model <- readRDS(paste0(path_results,
                             "/../../Models/hosp_model_nowcast_Hospdatum_",
                             doa, ".rds"))


################################################################################


# General descriptive analyses:

# Missing values:
sum(is.na(data_model$ExHosp_StayFrom1)) / nrow(data_model)
# 9.6% missing hospitalisation dates

# Time delay:
# Preparation of data:
data_year <- data_year %>%
  filter(Hospdatum >= "2021-01-01") %>%
  mutate(d = as.numeric(Meldedatum_Hosp - Hospdatum)) %>%
  mutate(d = ifelse(d == 0, yes = 1, no = d)) %>%
  mutate(Altersgruppe = case_when(AlterBerechnet < 60 ~ "0-59",
                                  AlterBerechnet >= 60 ~ "60+"),
         Altersgruppe = factor(x = Altersgruppe,
                               levels = c("0-59", "60+")))
# Empirical distribution function:
p <- ecdf(data_year$d)
p1 <- ecdf(data_year$d[data_year$Altersgruppe == "0-59"])
p2 <- ecdf(data_year$d[data_year$Altersgruppe == "60+"])
p(1)
p(7)
p(40)
# Visualization of time delay:
plot3 <- ggplot() +
  geom_step(data = data_year,
            mapping = aes(x = d, col = Altersgruppe, y = ..y..),
            stat = "ecdf", size = 0.8, pad = TRUE) +
  xlab("Time delay [days]") + ylab("Relative frequency") +
  coord_cartesian(xlim = c(1, 40), expand = TRUE) +
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = p1(40)), lty = "dashed",
               col = "black") +
  geom_segment(aes(x = 0, y = p1(40), xend = 40, yend = p1(40)),
               lty = "dashed", col = "#e33e32") +
  geom_segment(aes(x = 0, y = p2(40), xend = 40, yend = p2(40)),
               lty = "dashed", col = "#0072B5FF") +
  scale_color_manual(name = "Age group", labels = c("0-59", "60+"),
                     values = c("#e33e32","#0072B5FF")) + 
  theme_pubr(base_size = 20) +
  theme(legend.title = element_text(size = 15),
        legend.text  = element_text(size = 15), 
        panel.grid.major = element_line(), legend.key.size =  unit(0.5, "in"))
# ggsave(paste0(path_results, "/Figure3.pdf"), width = 8, height = 5)

# Comparison hospitalisation date and reporting date of infection:
# Data preparation:
data_year <- data_year %>%
  filter(!is.na(ExHosp_StayFrom1), Meldedatum >= "2021-01-01")
data_hosp <- data_year %>% group_by(ExHosp_StayFrom1) %>%
  summarise(hosp = n()) %>% mutate(measure = "hosp")
hosp <- c()
for(i in 7:nrow(data_hosp)){
  hosp[i] <- sum(data_hosp$hosp[(i - 6):i])
}
data_hosp$hosp <- hosp
data_hosp <- data_hosp %>% filter(!is.na(hosp))
colnames(data_hosp) <- c("date", "hosp", "measure")
data_meld <- data_year %>% group_by(Meldedatum) %>%
  summarise(hosp = n()) %>% mutate(measure = "meld")
hosp <- c()
for(i in 7:nrow(data_meld)){
  hosp[i] <- sum(data_meld$hosp[(i - 6):i])
}
data_meld$hosp <- hosp
data_meld <- data_meld %>% filter(!is.na(hosp))
colnames(data_meld) <- c("date", "hosp", "measure")
plot_data <- bind_rows(data_hosp, data_meld) %>%
  mutate(measure = as.factor(measure))

# Visualization:
date_vector <- c(as_date("2021-01-01"), as_date("2021-03-01"),
                 as_date("2021-05-01"), as_date("2021-07-01"),
                 as_date("2021-09-01"), as_date("2021-11-01"))
plotA7 <- ggplot(data = plot_data,
                 mapping = aes(x = date, y = hosp , col = measure)) +
  geom_line(lwd = 1.2) +
  scale_color_manual(labels = c("Date of hospitalization",
                                "Reporting date of infection"),
                     values = c("hosp" = "cornflowerblue",
                                "meld" = "lightpink3")) +
  scale_x_date(date_labels = "%d %b", breaks = date_vector) +
  theme_pubr(base_size = 20) +xlab("") + ylab("Hospitalisations") +
  theme( legend.title = element_blank(), 
         legend.text  = element_text(size = 15), 
         panel.grid.major = element_line())
# ggsave(paste0(path_results, "/FigureA7.pdf"), width = 12, height = 7.5)


################################################################################


# Plots of nowcasting results:

# Comparison of nowcasted, reported and realized hospitalisation numbers:
date_vector <- c(doa - days(7), doa - days(7) - weeks(2),
                 doa - days(7) - weeks(4), doa - days(7) - weeks(6))
nowcast_results$age60 <- as.factor(nowcast_results$age60)
ymax <- max(nowcast_results$nowcast7_0.975)
# Both age groups:
plot_all <- ggplot(nowcast_results %>% filter(age60 == "alle") %>%
                        mutate(nowcast7_est = nowcast7_est,
                               nowcast7_lwr = nowcast7_0.025,
                               nowcast7_upr = nowcast7_0.975,
                               reported7 = reported7,
                               realized7 = realized7)) +
  geom_line(aes(date, reported7, col = "Reported", lty = "Reported"),
            lwd = 0.8) +
  geom_line(aes(date, nowcast7_est, col = "Nowcasted", lty = "Nowcasted"),
            lwd = 0.8) +
  geom_line(aes(date, realized7, col = "Realised after 40 days",
                lty = "Realised after 40 days"),
            lwd = 0.8) +
  geom_ribbon(aes(date, ymin = nowcast7_lwr, ymax = nowcast7_upr),
              fill = "lightpink3", col = NA, alpha = .4) +
  theme_pubr(base_size = 20) +
  theme( legend.title = element_text(size = 15), 
         legend.text  = element_text(size = 15), 
         panel.grid.major = element_line(), 
         plot.title = element_text(hjust = 0.5))+
  xlab("Date of hospitalisation") + ylab("Hospitalisations") +
  ggtitle("(a) Overall population") +
  scale_y_continuous(limits = c(0, ymax),
                     breaks = seq(from = 0, to = ymax, by = 500)) +
  scale_x_date(date_labels = "%d %b", breaks = date_vector) +
  scale_linetype_manual(name = NULL,
                        values = c("Nowcasted" = 1, "Reported" = 1,
                                   "Realised after 40 days" = 2)) +
  scale_color_manual(name = NULL, 
                     values = c("Nowcasted" = "lightpink3",
                                "Reported" = "lightblue",
                                "Realised after 40 days" = "black")) +
  theme(legend.key.size =  unit(0.5, "in")) + ylim(0, 3000)
plot_empty <- ggplot() + theme_void()
ymax_ages <- max(nowcast_results$nowcast7_0.975[nowcast_results$age60 !=
                                                  "alle"])
# People younger than 60 years:
plot_young <- ggplot(nowcast_results %>% filter(age60 == "0-60") %>%
                     mutate(nowcast7_est = nowcast7_est,
                            nowcast7_lwr = nowcast7_0.025,
                            nowcast7_upr = nowcast7_0.975,
                            reported7 = reported7,
                            realized7 = realized7)) +
  geom_line(aes(date, reported7, col = "Reported", lty = "Reported"),
            lwd = 0.8) +
  geom_line(aes(date, nowcast7_est, col = "Nowcasted", lty = "Nowcasted"),
            lwd = 0.8) +
  geom_line(aes(date, realized7, col = "Realised after 40 days",
                lty = "Realised after 40 days"),
            lwd = 0.8) +
  geom_ribbon(aes(date, ymin = nowcast7_lwr, ymax = nowcast7_upr),
              fill = "lightpink3", col = NA, alpha = .4) +
  xlab("Date of hospitalisation") + ylab("Hospitalisations") +
  ggtitle("(b) 0 - 59") +
  theme_pubr(base_size = 20) +
  theme( legend.title = element_text(size = 15), 
         legend.text  = element_text(size = 15), 
         panel.grid.major = element_line(), 
         plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0, ymax_ages),
                     breaks = seq(from = 0, to = ymax_ages, by = 500)) +
  scale_x_date(date_labels = "%d %b", breaks = date_vector) + 
  scale_linetype_manual(name = NULL,
                        values = c("Nowcasted" = 1, "Reported" = 1,
                                   "Realised after 40 days" = 2)) +
  scale_color_manual(name = NULL, 
                     values = c("Nowcasted" = "lightpink3",
                                "Reported" = "lightblue",
                                "Realised after 40 days" = "black")) +
  theme(legend.key.size =  unit(0.5, "in")) + ylim(0, 2000)
# People of age 60 or older:
plot_old <- ggplot(nowcast_results %>% filter(age60 == "60+") %>%
                     mutate(nowcast7_est = nowcast7_est,
                            nowcast7_lwr = nowcast7_0.025,
                            nowcast7_upr = nowcast7_0.975,
                            reported7 = reported7,
                            realized7 = realized7)) +
  geom_line(aes(date, reported7, col = "Reported", lty = "Reported"),
            lwd = 0.8) +
  geom_line(aes(date, nowcast7_est, col = "Nowcasted", lty = "Nowcasted"),
            lwd = 0.8) +
  geom_line(aes(date, realized7, col = "Realised after 40 days",
                lty = "Realised after 40 days"),
            lwd = 0.8) +
  geom_ribbon(aes(date, ymin = nowcast7_lwr, ymax = nowcast7_upr),
              fill = "lightpink3", col = NA, alpha = .4) +
  xlab("Date of hospitalisation") + ylab("Hospitalisations") +
  ggtitle("(c) 60+") +
  theme_pubr(base_size = 20) +
  theme( legend.title = element_text(size = 15), 
         legend.text  = element_text(size = 15), 
         panel.grid.major = element_line(), 
         plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0, ymax_ages),
                     breaks = seq(from = 0, to = ymax_ages, by = 500)) +
  scale_x_date(date_labels = "%d %b", breaks = date_vector) + 
  scale_linetype_manual(name = NULL,
                        values = c("Nowcasted" = 1, "Reported" = 1,
                                   "Realised after 40 days" = 2)) +
  scale_color_manual(name = NULL, 
                     values = c("Nowcasted" = "lightpink3",
                                "Reported" = "lightblue",
                                "Realised after 40 days" = "black")) +
  theme(legend.key.size =  unit(0.5, "in")) + ylim(0, 2000)
plot5a <- ggarrange(plot_empty, plot_all, plot_empty, nrow = 1,
                    widths = c(1, 5.7, 0.5), legend = "right")
plot5b <- ggarrange(plot_young, plot_old, nrow = 1,
                   ncol = 2, legend = "none")
plot5 <- ggarrange(plot5a, plot5b, nrow = 2, ncol = 1, common.legend = TRUE,
                   legend = "bottom")
# ggsave(paste0(path_results, "/Figure5.pdf"), width = 11, height = 7)

# Time effect:
data_tmp <- as.data.frame(nowcast_model$model)
data_tmp <- data_tmp[!duplicated(data_tmp$t1), ]
terms <- c("t1", "t2")
prediction <- predict(object = nowcast_model, newdata = data_tmp,
                      type = "terms", terms = terms, se.fit = TRUE)
prediction$fit <- as.data.frame(prediction$fit)
prediction$fit$t <- prediction$fit$t1 + prediction$fit$t2
prediction$se.fit <- as.data.frame(prediction$se.fit)
# data_tmp$t1 <- data_tmp$t1 - mean(data_tmp$t1)
# data_tmp$t2 <- data_tmp$t2 - mean(data_tmp$t2)

prediction$se.fit$time1 <- data_tmp$t1
prediction$se.fit$time2 <- data_tmp$t2
prediction$se.fit$t <- sqrt(vcov(nowcast_model)[2, 2] * prediction$se.fit$time1^2 +
                              vcov(nowcast_model)[3, 3] * prediction$se.fit$time2^2 +
                              2 * vcov(nowcast_model)[2, 3] *
                              prediction$se.fit$time1 * prediction$se.fit$time2)
# prediction$fit$t <- prediction$fit$t - mean(prediction$fit$t)
# Plot data:
plot_dat <- data_tmp %>%
  mutate(effect = as.numeric(prediction$fit$t),
         se = as.numeric(prediction$se.fit$t),
         date = as_date(x = data_tmp$t1, origin = "2021-09-23")) %>%
  mutate(effect = effect,
         lower = effect - qnorm(0.95) * se,
         upper = effect + qnorm(0.95) * se)
# Visualization:
date_vector <- c(doa - days(7), doa - days(7) - weeks(2),
                 doa - days(7) - weeks(4), doa - days(7) - weeks(6))
plotA2 <- ggplot(data = plot_dat,aes(x = date,y = effect)) +
  geom_line(lwd = 0.8) + theme_pubr() +
  geom_ribbon(aes(ymin = lower,ymax = upper), alpha= 0.2) +
  guides(color = FALSE, shape = FALSE) + ylab("Effect") +
  xlab("Date of hospitalisation") +
  scale_x_date(date_labels = "%d %b", breaks = date_vector) + ggtitle("") #+
  # geom_hline(yintercept = 0,lty = 2)
# ggsave(paste0(path_results, "/FigureA2.pdf"), width = 8, height = 4)

# Delay effects:
# 0 to 59 years:
summary_nowcast <- plot(nowcast_model, pages = 1)
plot_data <- data.frame("coef" = summary_nowcast[[1]]$fit,
                        "delay" = summary_nowcast[[1]]$x,
                        "se_lower" = summary_nowcast[[1]]$fit -
                          qnorm(p = 0.975) * summary_nowcast[[1]]$se, 
                        "se_upper" = summary_nowcast[[1]]$fit +
                          qnorm(p = 0.975) * summary_nowcast[[1]]$se)
plotA3a <- ggplot(data = plot_data, mapping = aes(x = delay, y = coef)) +
  geom_line(lwd = 0.8) + theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon(aes(ymin = se_lower,ymax = se_upper), alpha = 0.2) +
  guides(color = FALSE, shape = FALSE) + ylab("Effect") +
  xlab("Delay in days") + ggtitle("0 to 59") +
  geom_hline(yintercept = 0,lty = 2) +
  scale_y_continuous(limits = c(-3, 4.5),
                     breaks = seq(from = -3, to = 4, by = 1)) +
  scale_x_continuous(limits = c(0, 40),
                     breaks = seq(from = 0, to = 40, by = 10))
# 60+ years:
plot_data <- data.frame("coef" = summary_nowcast[[2]]$fit,
                        "delay" = summary_nowcast[[2]]$x,
                        "se_lower" = summary_nowcast[[2]]$fit -
                          qnorm(p = 0.975) * summary_nowcast[[2]]$se, 
                        "se_upper" = summary_nowcast[[2]]$fit +
                          qnorm(p = 0.975) * summary_nowcast[[2]]$se)
plotA3b <- ggplot(data = plot_data, mapping = aes(x = delay, y = coef)) +
  geom_line(lwd = 0.8) + theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon(aes(ymin = se_lower,ymax = se_upper), alpha = 0.2) +
  guides(color = FALSE, shape = FALSE) + ylab("Effect") +
  xlab("Delay in days") + ggtitle("60+") +
  geom_hline(yintercept = 0,lty = 2) +
  scale_y_continuous(limits = c(-3, 4.5),
                     breaks = seq(from = -3, to = 4, by = 1)) +
  scale_x_continuous(limits = c(0, 40),
                     breaks = seq(from = 0, to = 40, by = 10))
plotA3 <- ggarrange(plotA3a, plotA3b, nrow = 1, ncol = 2, common.legend = TRUE,
                    legend = "bottom")
# ggsave(paste0(path_results, "/FigureA3.pdf"), width = 9, height = 4)

# Weekday effects:
# Weekday of hospitalisation:
coefs_tmp <- nowcast_model$coefficients
cov_tmp <- nowcast_model$Ve
tmp_ind <- grep(pattern = "Wochentag", names(coefs_tmp))[1:6]
coefs_tmp <- coefs_tmp[tmp_ind]
cov_tmp <- cov_tmp[tmp_ind,tmp_ind]
plot_data <- data.table(day = weekdays(seq(from = as.Date('2020-12-01'),
                                           to = as.Date('2020-12-06'),
                                           length.out = 6)),
                        coef = coefs_tmp, 
                        stderr = sqrt(diag(cov_tmp)), 
                        upper = coefs_tmp + qnorm(p = 0.975) *
                          sqrt(diag(cov_tmp)),
                        lower = coefs_tmp - qnorm(p = 0.975) *
                          sqrt(diag(cov_tmp)))
plot_data$day = factor(plot_data$day, levels = plot_data$day)
plotA4a <- ggplot(data = plot_data,
                 mapping = aes(x = factor(day), y = coef, ymin = upper,
                               ymax = lower))+
  geom_pointrange(lwd = 0.8) + theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=8),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("") + ylab("Effect") + ggtitle("Weekday hospitalisation") +
  ylim(-0.5, 1) + geom_hline(yintercept = 0,lty = 2)
# Weekday of reporting of hospitalisation:
coefs_tmp <- nowcast_model$coefficients
cov_tmp <- nowcast_model$Ve
tmp_ind <- grep(pattern = "Wochentag_hosp", names(coefs_tmp))
coefs_tmp <- coefs_tmp[tmp_ind]
cov_tmp <- cov_tmp[tmp_ind,tmp_ind]
plot_data <- data.table(day = weekdays(seq(from = as.Date('2020-12-01'),
                                           to = as.Date('2020-12-06'),
                                           length.out = 6)),
                        coef = coefs_tmp, 
                        stderr = sqrt(diag(cov_tmp)), 
                        upper = coefs_tmp + qnorm(p = 0.975) *
                          sqrt(diag(cov_tmp)),
                        lower = coefs_tmp - qnorm(p = 0.975) *
                          sqrt(diag(cov_tmp)))
plot_data$day = factor(plot_data$day, levels = plot_data$day)
plotA4b <- ggplot(data = plot_data,
                  mapping = aes(x = factor(day), y = coef, ymin = upper,
                                ymax = lower))+
  geom_pointrange(lwd = 0.8) + theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=8),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("") + ylab("Effect") + ggtitle("Weekday reporting of hospitalisation") +
  ylim(-0.5, 1) + geom_hline(yintercept = 0,lty = 2)
plotA4 <- ggarrange(plotA4a, plotA4b, nrow = 1, ncol = 2, common.legend = TRUE,
                    legend = "bottom")
# ggsave(paste0(path_results, "/FigureA4.pdf"), width = 9, height = 4)


################################################################################


# Effects of hospitalisation model:

# Variance of random effects:
gam.vcomp(hosp_model)

# Age and gender effects:
# Data preparation:
n_1 <- nlevels(hosp_model$model$age_group)
n_2 <- nlevels(hosp_model$model$gender)
levels_1 <- levels(hosp_model$model$age_group)
levels_2 <- levels(hosp_model$model$gender)
coefs_tmp <- hosp_model$coefficients
cov_tmp <- hosp_model$Ve
tmp_ind <- c(1, unique(c(grep(pattern = "age_group", names(coefs_tmp)),
                         grep(pattern = "gender", names(coefs_tmp)))))
coefs_tmp <- coefs_tmp[tmp_ind]
cov_tmp <- cov_tmp[tmp_ind,tmp_ind]
levels_1_new <- gsub(pattern = "A", replacement = "", x = levels_1)
levels_2_new <- c("Male", "Female")
new_data <- hosp_model$model[1:(n_1 * n_2),]
new_data$age_group <- rep(levels_1_new, times = n_2)
new_data$gender <- rep(levels_2_new, each = n_1)
new_data$effects <- list(c(1), c(1, 2), c(1, 3), c(1, 4), 
                        c(1, 8), c(1, 2, 5, 8), c(1, 3, 6, 8), c(1, 4, 7, 8))
new_data$no_int_effects <- list(c(), c(2), c(3), c(4), 
                                c(8), c(2, 5, 8), c(3, 6, 8), c(4, 7, 8))
new_data$estimates <- unlist(lapply(new_data$effects,
                                    function(x){sum(coefs_tmp[x])}))
new_data$ses <- sqrt(unlist(lapply(new_data$effects,
                                   function(x){sum(cov_tmp[x,x])})))
new_data$no_int_estimates <- unlist(lapply(new_data$no_int_effects,
                                           function(x){sum(coefs_tmp[x])}))
new_data$no_int_ses <- sqrt(unlist(lapply(new_data$no_int_effects,
                                          function(x){sum(cov_tmp[x,x])})))
new_data$label <- paste0(round(new_data$estimates, 3),
                         "\n(",round(new_data$ses,3), ")")
new_data$no_int_label <- paste0(round(new_data$no_int_estimates, 3),
                                "\n(",round(new_data$no_int_ses,3), ")")
new_data$no_int_label[1] <- "Reference\ncategory"
# Visualization:
plot6 <- ggplot(data = new_data,
                mapping = aes(y = gender, x = age_group,
                              fill = no_int_estimates)) + 
  geom_tile() + geom_text(aes(label = no_int_label), size =6) + ylab(" ") + 
  xlab("  ") + theme_pubr(base_size = 17) + 
  theme(axis.line = element_blank(),axis.ticks = element_blank()) + 
  scale_fill_gradient("Coefficient",low = "white", high = "#e82727", 
                      guide = guide_colourbar( barwidth = 8)) +
  theme(legend.title = element_blank(), legend.position = "bottom")
# ggsave(paste0(path_results, "/Figure6.pdf"), width = 8, height = 4)

# Smooth and random spatial effect
# Preprocess district data:
districts <- readRDS(paste0(path_data, "/Data_demographics/districts.rds")) %>%
  filter(bundesland == "Bayern")
data_tmp <- hosp_model$model
setDT(data_tmp)
data_tmp <- data_tmp[!duplicated(district)]
data_info <- data.table(predict(hosp_model, newdata = data_tmp, type = "terms"))
data_info$district <- data_tmp$district
districts$spatial_effect <- data_info$`s(lon,lat)`[match(districts$name_rki,
                                                         data_info$district)]
districts$random_effect <- data_info$`s(district)`[match(districts$name_rki,
                                                         data_info$district)]
# Spatial effect
plot7a <- ggplot() +
  geom_sf(data = districts, mapping = aes(fill = spatial_effect),
          col = "grey30") +
  theme_pubr() +
  theme(axis.ticks = element_blank(),
        axis.text =  element_blank(),
        strip.text.y = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        axis.line =  element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient2(name = "",low = "#4263f5",midpoint = 0,mid = "white",
                       high = "#f21111",na.value="grey",
                       guide = guide_colourbar( barwidth = 8))+ 
  theme(legend.position = "bottom") + ggtitle("(a) Spatial effect")
plot7b <- ggplot() +
  geom_sf(data = districts, aes(fill = random_effect), col = "grey30") +
  theme_pubr() +
  theme(axis.ticks = element_blank(),
        axis.text =  element_blank(),
        strip.text.y = element_text(size = 17),
        strip.text.x = element_text(size = 17),
        axis.line =  element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient2(name = "",low = "#4263f5",midpoint = 0,mid = "white",
                       high = "#f21111",na.value="grey",
                       guide = guide_colourbar( barwidth = 8))+ 
  theme(legend.position = "bottom") + ggtitle("(b) Random effect")
plot7 <- ggarrange(plot7a, NULL, plot7b, nrow = 1, ncol = 3,
                   common.legend = FALSE, legend = "bottom",
                   widths = c(1, -0.05, 1)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))
# ggsave(paste0(path_results, "/Figure7.pdf"), width = 10, height = 7)

# Time effect:
summary_hosp <- plot(hosp_model, pages = 1)
plot_data <- data.frame("coef" = summary_hosp[[1]]$fit,
                        "time" = summary_hosp[[1]]$x,
                        "date" = as_date(x = summary_hosp[[1]]$x,
                                         origin = "2021-09-24"),
                        "se_lower" = summary_hosp[[1]]$fit -
                          qnorm(p  = 0.975) * summary_hosp[[1]]$se, 
                        "se_upper" = summary_hosp[[1]]$fit +
                          qnorm(p  = 0.975) * summary_hosp[[1]]$se)
date_vector <- c(doa - days(7), doa - days(7) - weeks(2),
                 doa - days(7) - weeks(4), doa - days(7) - weeks(6))
plotA5 <- ggplot(data = plot_data,aes(x = date,y = coef))+
  geom_line(lwd = 0.8) + theme_pubr() +
  geom_ribbon(aes(ymin = se_lower,ymax = se_upper),alpha= 0.2) +
  guides(color = FALSE, shape = FALSE) + ylab("Effect") +
  xlab("Date of hospitalisation") +
  scale_x_date(date_labels = "%d %b", breaks = date_vector) + ggtitle("") +
  geom_hline(yintercept = 0,lty = 2) 
# ggsave(paste0(path_results, "/FigureA5.pdf"), width = 8, height = 4)

# Weekday effect:
coefs_tmp <- hosp_model$coefficients
cov_tmp <- hosp_model$Ve
tmp_ind <- grep(pattern = "weekday", names(coefs_tmp))
coefs_tmp <- coefs_tmp[tmp_ind]
cov_tmp <- cov_tmp[tmp_ind,tmp_ind]
plot_data <- data.table(day = weekdays(seq(as.Date('2020-12-01'),
                                          as.Date('2020-12-06'),
                                          length.out = 6)),
                       coef = coefs_tmp, 
                       stderr = sqrt(diag(cov_tmp)), 
                       upper = coefs_tmp+ qnorm(p = 0.975) *
                         sqrt(diag(cov_tmp)),
                       lower = coefs_tmp- qnorm(p = 0.975) *
                         sqrt(diag(cov_tmp)))
plot_data$day <- factor(plot_data$day, levels = plot_data$day)
plotA6 <- ggplot(data = plot_data,
                 mapping = aes(x = factor(day), y = coef, ymin = upper,
                               ymax = lower)) +
  geom_pointrange(lwd = 0.8) + theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) + xlab("") +
  ylab("Effect") + ggtitle("") + geom_hline(yintercept = 0,lty = 2) 
# ggsave(paste0(path_results, "/FigureA6.pdf"), width = 8, height = 4)


################################################################################






