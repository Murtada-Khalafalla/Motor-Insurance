#---------------------------------Data Analysis for Motor insurance Data.----------------------------
# Performing GLM Calculating Using Poisson, Negative Binomial ,Gamma and Quasi Poison  Distributions
# Prepared by:  Murtada Khalafalla Ibrahim
# Email:murtada.khalafalla@gmail.com               
# Mobile: +249 9 12261449
# July 03, 2020
#--------------------------------------Libraries------------------------------------------------------
library(tidyverse)
library(readxl)
library(funModeling)
library(skimr)
library(Hmisc)
library(MASS)
library(stargazer)
library(ggcharts)
library(plotly)
library(carData)
library(glue)
#---------------------------------Source Code---------------------------------------------------------
insur_data <- read_excel("data/Last change for R2.xlsx")

motor_insur_data <- insur_data %>%
  
dplyr::select(Gender,Age,Occupation,Area,Driving_Experience,Manufac_country,Make,Model,Age_of_Vehicle,Cylinder_capacity,Claim_count,Claim_amount) %>% 
  
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
  mutate(Area = factor(Area, 
                           levels = c(1,2,3),
                           labels = c("Khartoum","Khartoum Bahari","Oumdrman") )) %>% 
  
  # Manufac_country
  mutate(Manufac_country = factor(Manufac_country, 
                            levels = c(1,2,3,4,5,6),
                            labels = c("Korea","Japan","Sudan","Germany","Czech","Others"))) %>% 
  
  # Make 
  mutate(Make = factor(Make,
      levels = c(1,2,3,4,5,6,7,8),
      labels = c("Hyundai","Kia","Toyota","Mitsubishi","Giad","Mercedes","Skoda","Other"))) %>% 
  
  # Model
  mutate(Model = factor(Model,
                        levels = c(1,2,3,4,5,6,7,8,9,10,11) ,
                        labels = c("Accent","Click","Tucson","Santa Fe","Visto","Corolla","Hilux","Prado / Land Cruiser","Lancer","BYD","Other"))) %>% 
  
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


motor_insur_data

##----------------------------------------------ggplot--------------------------------------------
#Severity
cont_gender_sevrty<- motor_insur_data %>% 
  filter(Claim_count>0) %>% 
  group_by(Gender) %>% 
  summarise(n=n()) %>% 
  mutate(Frequency=  case_when(
    Gender=="Male" ~ paste0(round(sum(42055250.53)/n,2),"%"),
    Gender=="Female" ~ paste0(round(sum(8460539.26)/n,2),"%")
  ))

ggSeverity<- ggplot(cont_gender_sevrty, aes(x=Gender, y=Frequency, fill=Gender))+ 
geom_bar (stat="identity")+
  scale_fill_manual(values = c("red","orange"))+
  labs(title = "Severity")

ggSeverity

#Frequency
cont_gender_freq<- motor_insur_data %>% 
  filter(Claim_count>0) %>% 
  group_by(Gender) %>% 
  summarise(n=n()) %>% 
  mutate(Frequency=  case_when(
    Gender=="Male" ~ paste0(round(n/12311*100,3),"%"),
    Gender=="Female" ~ paste0(round(n/2374*100,3),"%")
  ))

ggFrequency<- ggplot(cont_gender_freq, aes(x=Gender, y=Frequency, fill=Gender))+ 
  geom_bar (stat="identity")+
  scale_fill_manual(values = c("olivedrab","orange"))+
  labs(title = "Frequency")
ggFrequency


#-----------------------------------------------------------------------------------------------
table(motor_insur_data$Claim_count)
reported<- read_excel("reported.xlsx")

lm_freq<-  glm(formula = log(frequency) ~ claim ,family = gaussian(link = "identity"), data = reported)
summary(lm_freq)
plot(lm_freq)
#-----------------------------------------------------------------------------------------------


#----------------------------------Frequency Model------------------------------------------------------
#1-Poisson distribution
glm.p <- glm(formula = Claim_count ~ Gender+Age+Occupation+Area+Driving_Experience+ Manufac_country+Make+Model+Age_of_Vehicle+Cylinder_capacity,family = poisson(link = log), data = motor_insur_data)

summary(glm.p)
anova(glm.p, test="Chisq")
plot(glm.p)

#2-Negative Binomial distribution
glm.negb<- glm.nb(formula = Claim_count ~ Gender+Age+Occupation+Area+Driving_Experience+ Manufac_country+Make+Model+Age_of_Vehicle+Cylinder_capacity, link = log,data = motor_insur_data)

summary(glm.negb)
anova(glm.negb, test="Chisq")
plot(glm.negb)

#-------------------------------------Severity Model--------------------------------------------------
#1-Log Normal Distributions
glm_lognor_motor_insur_data<- motor_insur_data %>% 
dplyr::filter(Claim_amount>0) 

glm.lognorm<- glm(formula = log(Claim_amount) ~ Gender+Age+Occupation+Area+Driving_Experience+ Manufac_country+Make+Model+Age_of_Vehicle+Cylinder_capacity,family = gaussian(link = "identity"), data = glm_lognor_motor_insur_data)

summary(glm.lognorm)
anova(glm.lognorm, test="Chisq")
plot(glm.lognorm) 

#2-Gamma distribution
# filter(Claim_amount>0
glm_gama_motor_insur_data <- motor_insur_data %>% 
  dplyr::filter(Claim_amount>0) 

glm.gamma <- glm(formula = Claim_amount ~ Gender+Age+Occupation+Area+Driving_Experience+ Manufac_country+Make+Model+Age_of_Vehicle+Cylinder_capacity, family = Gamma(link = "log"), data = glm_gama_motor_insur_data) 

summary(glm.gamma)
anova(glm.gamma, test="Chisq")
plot(glm.gamma)

# ------------------------Combine results of the Frequency Results Distributions--------------------------
stargazer(glm.p, glm.negb, type="html",
          dep.var.labels=c("Frequency Distribution Results"),
          out="frequency_model.txt")

#----------------------Combine results of the Severity Results Distributions-------------------------

stargazer(glm.lognorm, glm.gamma, type="html",
          dep.var.labels=c("Severity Distribution Results"),
          out="severity_model.txt")

#------------------------------------------Charts-----------------------------------------------------
# Charts1 Age frequency
chartr_freq_age<- motor_insur_data %>% 
  count(Age) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart(Age,Number.of.policyholders)
chartr_freq_age +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Charts1 Age_of_Vehicle frequency
chartr_freq_Age_of_Vehicle<- motor_insur_data %>% 
  count(Age_of_Vehicle) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart(Age_of_Vehicle,Number.of.policyholders)
chartr_freq_Age_of_Vehicle +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Charts1  Manufacture country frequency
chartr_freq_Manufac_country<- motor_insur_data %>% 
  count( Manufac_country) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Manufac_country,Number.of.policyholders)
chartr_freq_Manufac_country +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Charts1  Occupation frequency
chartr_freq_Occupation<- motor_insur_data %>% 
  count( Occupation) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Occupation,Number.of.policyholders)
chartr_freq_Occupation +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Charts1  Location frequency
chartr_freq_Location<- motor_insur_data %>% 
  count( Location) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Location,Number.of.policyholders)
chartr_freq_Location +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Charts1 Cylinder_capacity frequency
chartr_freq_Cylinder_capacity<- motor_insur_data %>% 
  count( Cylinder_capacity) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Cylinder_capacity,Number.of.policyholders)
chartr_freq_Cylinder_capacity +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Charts1 Make frequency
chartr_freq_Make<- motor_insur_data %>% 
  count( Make) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Make,Number.of.policyholders)
chartr_freq_Make +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Charts2 Age pct %
chartr_pct<- motor_insur_data %>% 
  #filter(Claim_count>0) %>% 
  count(Age) %>% 
  arrange(-n) %>% 
  mutate(Number.of.policyholders=n) %>% 
  column_chart(Age,Number.of.policyholders)
chartr_pct +
  geom_text(aes(label=paste0(Number.of.policyholders,"%")), vjust=1.2, color="white")

#---------------------------------Descriptive Statistics-------------------------------------------
describe(motor_insur_data)
skimr::skim(motor_insur_data)
df_status(motor_insur_data)
plot_num(motor_insur_data)
profiling_num(motor_insur_data)
describe(motor_insur_data)
freq(motor_insur_data)
summary(motor_insur_data)
str(motor_insur_data)

my_data$Sum_insure<- log(motor_insur_data$Sum_insure)
my_data$claim_amount<- log(motor_insur_data$claim_amount)
my_data$claim_count<- log(motor_insur_data$claim_count)

write.csv(motor_insur_data,'data/murtada.csv')

