rm(list=ls()) 
library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
setwd ("D:/InfTeh") 
getwd() 
eddy = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"),comment =c("["))
eddy = eddy[-1,]
eddy
names(eddy)
##удаляем всякое
eddy = select(eddy, -(roll)) 
eddy = eddy[,c(-1,-3,-6,-7,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
names(eddy) 
##выбираем осенние вечера
eddy<-eddy[eddy$DOY>244 & eddy$DOY<315 & eddy$daytime == FALSE, c(1:ncol(eddy))] 
##что-то страшное
eddy = eddy %>% mutate_if(is.character, factor) 
names(eddy) = str_replace_all(names(eddy), "[!]","_emph_") 
names(eddy) = names(eddy) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(eddy) 
##корреляция
sapply(eddy,is.numeric) 
eddy_numeric = eddy[,sapply(eddy,is.numeric) ] 
eddy_nonnumeric = eddy[,!sapply(eddy,is.numeric) ] 
cor_td = cor(eddy_numeric) 
cor_td 
cor_td = cor(drop_na(eddy_numeric)) %>% as.data.frame %>% select(h2o_flux) 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
formula
row_numbers = 1:length(eddy$date) 
teach = sample(row_numbers, floor(length(eddy$date)*.7)) 
test = row_numbers[-teach] 
teaching_eddy_unq = eddy[teach,] 
testing_eddy_unq = eddy[test,] 
##разбираем по частям всё мню написанное
mod = lm(formula, data=eddy) 
mod 
coef(mod) 
resid(mod) 
confint(mod) 
summary(mod)
anova(mod)
model1 = lm(h2o_flux ~ (rand_err_Tau + H + LE + rand_err_LE + h2o_flux + rand_err_h2o_flux + 
                          co2_molar_density + co2_mixing_ratio + RH + VPD + max_speed + 
                          u_star_ + TKE + T_star_ + un_H + un_LE + un_h2o_flux + u_var + 
                          v_var + w_div_ts_cov + w_div_h2o_cov + co2_signal_strength_7200)^2, data = eddy)
model1
coef(model1) 
resid(model1) 
confint(model1) 
summary(model1)
##R2 равен одному, ну ЧТО Ж
anova(model1)