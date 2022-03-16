#---------------------------------Data Analysis for Motor insurance Data.----------------------------
# Performing GLM Calculating Using Poisson, Negative Binomial ,Gamma and Quasi Poison  Distributions
# Prepared by:  Murtada Khalafalla Ibrahim
# Email:murtada.khalafalla@gmail.com               
# Mobile: +249 9 12261449
# July 03, 2020
#--------------------------------------Libraries------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
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
insur_data <- read_excel("data/Last_code_for_Rs3.xlsx")

motor_insur_data <- insur_data %>%
  
 dplyr::select(Gender,Residence,Cylinder.Capacity,Age,Driving.Experience,Age.of.Vehicle,Manufac.Country,Occupation,Make,Model,Claim.Count,Claim.Amount) %>% 
  
  # Gender
  mutate(Gender= case_when(Gender=="Male" ~ 1,
                           Gender=="Female" ~2)) %>% 
  mutate(Gender = factor(Gender, levels = c(1,2),
                         labels = c("Male","Female"))) %>% 
  
  # Residence
  mutate(Residence= case_when(Residence=="Khartoum" ~1,
                              Residence=="Khartoum Bahari" ~2,
                              Residence=="Oumdrman" ~3)) %>% 
  mutate(Residence = factor(Residence, 
                            levels = c(1,2,3),
                            labels = c("Khartoum","Khartoum Bahari","Oumdrman") )) %>% 
  
  # Cylinder_capacity
  mutate(Cylinder.Capacity= case_when(Cylinder.Capacity=="Medium"~1,
                                      Cylinder.Capacity=="Large"~2,
                                      Cylinder.Capacity=="Small"~3)) %>% 
  
  mutate(Cylinder.Capacity = factor(Cylinder.Capacity, 
                                    levels = c(1,2,3), 
                                    labels = c("Medium","Large","Small"))) %>% 
  
  # Age
  mutate(Age=round(Age,0)) %>% 
  mutate(Age = case_when(
    between(Age ,31, 60) ~1,
    between(Age, 25,30) ~2,
    Age >60 ~ 3,
    between(Age, 17, 24) ~4)) %>% 
  mutate(Age = factor(Age, levels = c(1,2,3,4), 
                      labels = c("31-60","25-30",">60","18-24"))) %>% 
  
  # Driving_Experience
  mutate(Driving.Experience=round(Driving.Experience,0)) %>%
  mutate(Driving.Experience = case_when(
    between(Driving.Experience,0 ,5) ~ 1,
    between(Driving.Experience,6, 10) ~ 2,
    between(Driving.Experience,11 ,20 ) ~ 3,
    TRUE ~ 4)) %>%  
  mutate(Driving.Experience = factor(Driving.Experience, 
                                     levels = c(1,2,3,4), 
                                    labels = c("0-5","6-10","11-20",">20"))) %>% 
    # Age_of_Vehicle
  mutate(Age.of.Vehicle = case_when(
    between(Age.of.Vehicle, 2006 ,2010) ~ 1,
    between(Age.of.Vehicle, 2011 , 2017) ~ 2,
    between(Age.of.Vehicle, 1996 ,2005) ~ 3,
    TRUE ~ 4)) %>%  
  mutate(Age.of.Vehicle = factor(Age.of.Vehicle, 
                                 levels = c(1,2,3,4), 
                                 labels = c("6-10","0-5","11-20",">20"))) %>%
  
  # Manufacture country
  mutate(Manufac.Country= case_when(Manufac.Country=="Korea"~1,
                                    Manufac.Country=="Japan"~2,
                                    Manufac.Country=="Sudan"~3,
                                    Manufac.Country=="Germany"~4,
                                    Manufac.Country=="Czech"~5,
                                    Manufac.Country=="Others"~6)) %>% 
  mutate(Manufac.Country = factor(Manufac.Country, 
                                  levels = c(1,2,3,4,5,6),
                                  labels = c("Korea","Japan","Sudan","Germany","Czech","Others"))) %>% 
  
  # Occupation
  mutate(Occupation= case_when(Occupation=="Employee" ~1,
                               Occupation=="Engineer and Programmer" ~2,
                               Occupation=="Medical Profession" ~3,
                               Occupation=="Business man/woman" ~4,
                               Occupation=="Free Lancer" ~5,
                               Occupation=="Student" ~6,
                               Occupation=="Others" ~7)) %>% 
  mutate(Occupation= factor(Occupation, 
                            levels = c(1,2,3,4,5,6,7), 
                            labels = c("Employee","Engineer and Programmer","Medical Profession","Business man/woman","Free Lancer","Student","Others"))) %>% 
  
  # Make 
  mutate(Make= case_when(Make=="Hyundai"~1,
                         Make=="Kia"~2,
                         Make=="Toyota"~3,
                         Make=="Mitsubishi"~4,
                         Make=="Giad"~5,
                         Make=="Mercedes"~6,
                         Make=="Skoda"~7,
                         Make=="Others"~8)) %>% 
  mutate(Make = factor(Make,
                    levels = c(1,2,3,4,5,6,7,8),
                    labels = c("Hyundai","Kia","Toyota","Mitsubishi","Giad","Mercedes","Skoda","Others"))) %>%   # Model
  mutate(Model= case_when(Model=="Accent"~1,
                          Model=="Click"~2,
                          Model=="Tucson"~3,
                          Model=="Santa Fe"~4,
                          Model=="Visto"~5,
                          Model=="Corolla"~6,
                          Model=="Hilux"~7,
                          Model=="Prado/ Land Cruiser"~8,
                          Model=="Lancer"~9,
                          Model=="BYD"~10,
                          Model=="Others"~11)) %>% 
  mutate(Model = factor(Model,
                        levels = c(1,2,3,4,5,6,7,8,9,10,11) ,
                        labels = c("Accent","Click","Tucson","Santa Fe","Visto","Corolla","Hilux","Prado/ Land Cruiser","Lancer","BYD","Others"))) 
  
motor_insur_data

#----------------------------------Frequency Model------------------------------------------------------
# 1-Poisson distribution
freq_motor_insur_data <- motor_insur_data %>% 
#filter(!Occupation =="Others" & !Manufac.Country =="Others" & !Make =="Others" & !Model =="Others") %>%
#filter(Cylinder.Capacity !="Large") %>% 
rename(x1=Gender,x2=Residence,x3=Cylinder.Capacity,x4=Age,x5=Driving.Experience,x6=Age.of.Vehicle,x7=Manufac.Country,x8=Occupation,x9=Make,x10=Model,y=Claim.Count,z=Claim.Amount) 

glm.p <- glm(formula = y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 ,family = poisson(link = log), data = freq_motor_insur_data)

summary(glm.p)
anova(glm.p, test="Chisq")
plot(glm.p)

# 2-Negative Binomial distribution
glm.negb<- glm.nb(formula = y ~ x1 + x2 + x3 + x4 + x5+ x6 + x7 + x8 + x9 + x10, link = log,data = freq_motor_insur_data)

summary(glm.negb)
anova(glm.negb, test="Chisq")
plot(glm.negb)

#-------------------------------------Severity Model--------------------------------------------------
#1-Log Normal Distributions
glm_lognor_motor_insur_data<- motor_insur_data %>% 
dplyr::filter(Claim_amount>0) 

glm.lognorm<- glm(formula = log(z) ~ x1 + x2 + x3 + x4 + x5+ x6 + x7 + x8 + x9 + x10,family = gaussian(link = "identity"), data = glm_lognor_motor_insur_data)

summary(glm.lognorm)
anova(glm.lognorm, test="Chisq")
plot(glm.lognorm) 

#2-Gamma distribution
# filter(Claim_amount>0
glm_gama_motor_insur_data <- motor_insur_data %>% 
dplyr::filter(Claim_amount>0) 

glm.gamma <- glm(formula = z ~ x1 + x2 + x3 + x4 + x5+ x6 + x7 + x8 + x9 + x10, family = Gamma(link = "log"), data = glm_gama_motor_insur_data) 

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

