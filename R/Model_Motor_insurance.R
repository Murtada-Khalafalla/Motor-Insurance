#--------------------------------Data Analysis for Motor insurance Data.----------------------------
# Performing GLM Calculating Using Poisson, Negative Binomial ,Gamma and Quasi Poison  Distributions
# Prepared by: Murtada Khalafalla Ibrahim.
# Email:murtada.khalafalla@gmail.com               
# Mobile: +249 9 12261449
# Date : July/17/2020
#-------------------------Libraries-------------------------------------------------------
library(tidyverse)
library(readxl)
library(skimr)
library(Hmisc)
library(MASS)
library(stargazer)
library(ggcharts)
library(plotly)
library(carData)
library(glue)
library(patchwork)
library(gt)

#---------------Source Code---------------------------------------------------------
insur_data <- read_excel("")

motor_insur_df <- insur_data %>%
  
dplyr::select(Gender,Age,Occupation,Residence,Driving_Experience,Manufac_country,Make,Model,Age_of_Vehicle,Cylinder_capacity,Claim_count,Claim_amount) %>% 
  
  # Gender
  mutate(Gender = factor(Gender, levels = c(1,2),
                         labels = c("Male","Female"))) %>% 
  # Age
  mutate(Age=round(Age,0)) %>% 
  mutate(Age = case_when(
    between(Age ,31, 60) ~1,
    between(Age, 25,30) ~2,
    Age >60 ~ 3,
    between(Age, 17, 24) ~4)) %>% 
  mutate(Age = factor(Age, levels = c(1,2,3,4), 
                      labels = c("31-60","25-30",">60","18-24"))) %>% 
  
  # Occupation
  mutate(Occupation= factor(Occupation, 
                            levels = c(1,2,3,4,5,6,7), 
                            labels = c("Employee","Engineer and Programmer","Medical Profession","Business man/woman","Free Lancer","Student","Others"))) %>% 
  
  # Area
  mutate(Residence = factor(Residence, 
                           levels = c(1,2,3),
                           labels = c("Khartoum","Khartoum Bahari","Oumdrman") )) %>% 
  
  # Manufac_country
  mutate(Manufac_country = factor(Manufac_country, 
                            levels = c(1,2,3,4,5,6),
                            labels = c("Korea","Japan","Sudan","Germany","Czech","Others"))) %>% 
  
  # Make 
  mutate(Make = factor(Make,
      levels = c(1,2,3,4,5,6,7,8),
      labels = c("Hyundai","Kia","Toyota","Mitsubishi","Giad","Mercedes","Skoda","Others"))) %>% 
  
  # Model
  mutate(Model = factor(Model,
                        levels = c(1,2,3,4,5,6,7,8,9,10,11) ,
                        labels = c("Accent","Click","Tucson","Santa Fe","Visto","Corolla","Hilux","Prado / Land Cruiser","Lancer","BYD","Others"))) %>% 
  
  # Age_of_Vehicle
  mutate(Age_of_Vehicle = case_when(
    between(Age_of_Vehicle, 2006 ,2010) ~ 1,
    between(Age_of_Vehicle, 2011 , 2017) ~ 2,
    between(Age_of_Vehicle, 1996 ,2005) ~ 3,
    TRUE ~ 4)) %>%  
  mutate(Age_of_Vehicle = factor(Age_of_Vehicle, 
                                 levels = c(1,2,3,4), 
                                 labels = c("6-10","0-5","11-20",">20"))) %>%
  
  # Cylinder_capacity
  mutate(Cylinder_capacity = factor(Cylinder_capacity, 
                                    levels = c(1,2,3), 
                                    labels = c("Small","Medium","Large"))) %>% 
  
  # Driving_Experience
  mutate(Driving_Experience=round(Driving_Experience,0)) %>%
  mutate(Driving_Experience = case_when(
    between(Driving_Experience,0 ,5) ~ 1,
    between(Driving_Experience,6, 10) ~ 2,
    between(Driving_Experience,11 ,20 ) ~ 3,
    TRUE ~ 4)) %>%  
  mutate(Driving_Experience = factor(Driving_Experience, 
                                     levels = c(1,2,3,4), 
                                     labels = c("0-5","6-10","11-20",">20")))

motor_insur_df

# rename all fields to be y, x1,...x10
rename_df<- motor_insur_df %>% 
  rename(x1=Gender,x2=Age,x3=Occupation,x4=Residence,x5=Driving_Experience,x6=Manufac_country,x7=Make,x8=Model,x9=Age_of_Vehicle,x10=Cylinder_capacity,y1=Claim_count,y2=Claim_amount) 

#------------Frequency Model------------------------------------------------------
#1-Poisson distribution
freq_df <- rename_df 
glm.p <- glm(formula = y1 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 ,family = poisson(link = log), data = freq_df)

(summary(glm.p))
anova(glm.p, test="Chisq")
plot(glm.p)

#2-Negative Binomial distribution
glm.negb<- glm.nb(formula = y1 ~ x1 + x2 + x3 + x4 + x5+ x6 + x7 + x8 + x9 + x10, link = log, data = freq_df)

summary(glm.negb)
anova(glm.negb, test="Chisq")
plot(glm.negb)

#--------------------Severity Model--------------------------------------------------
#1-Log Normal Distributions
## filter(Claim_amount>0)
sevrity_df<- rename_df %>% 
dplyr::filter(y2>0) 

glm.lognorm<- glm(formula = log(y2) ~ x1 + x2 + x3 + x4 + x5+ x6 + x7 + x8 + x9 + x10,family = gaussian(link = "identity"), data = sevrity_df)

summary(glm.lognorm)
anova(glm.lognorm, test="Chisq")
plot(glm.lognorm) 

#2-Gamma distribution
glm.gamma <- glm(formula = y2 ~ x1 + x2 + x3 + x4 + x5+ x6 + x7 + x8 + x9 + x10, family = Gamma(link = "log"), data = sevrity_df) 

summary(glm.gamma)
anova(glm.gamma, test="Chisq")
plot(glm.gamma)

# --------Combine results of the Frequency Results Distributions--------------------------
stargazer(glm.p, glm.negb, type="text",
          dep.var.labels=c("Frequency Distribution Results"),
          out="frequency_model.txt")

#-----------Combine results of the Severity Results Distributions-------------------------
stargazer(glm.lognorm, glm.gamma, type="text",
          dep.var.labels=c("Severity Distribution Results"),
          out="severity_model.txt")
