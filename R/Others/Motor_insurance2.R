#---------------------------------Data Analysis for Motor insurance Data.----------------------------
# Performing GLM Calculating Using Poisson, Negative Binomial ,Gamma and Quasi Poison  Distributions
# Prepared by:  Murtada Khalafalla Ibrahim
# Email:murtada.khalafalla@gmail.com               
# Mobile: +249 9 12261449
# Date : July/17/2020
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
insur_data <- read_excel("C:/Users/murtada/Desktop/Motor_insurance/data/Last change for R2.xlsx")

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

#----------------------------------Frequency Model------------------------------------------------------
#1-Poisson distribution
freq_df <- rename_df 
glm.p <- glm(formula = y1 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 ,family = poisson(link = log), data = freq_df)

summary(glm.p)
anova(glm.p, test="Chisq")
plot(glm.p)

#2-Negative Binomial distribution
glm.negb<- glm.nb(formula = y1 ~ x1 + x2 + x3 + x4 + x5+ x6 + x7 + x8 + x9 + x10, link = log, data = freq_df)

summary(glm.negb)
anova(glm.negb, test="Chisq")
plot(glm.negb)

#-------------------------------------Severity Model--------------------------------------------------
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

# ------------------------Combine results of the Frequency Results Distributions--------------------------
stargazer(glm.p, glm.negb, type="text",
          dep.var.labels=c("Frequency Distribution Results"),
          out="frequency_model.txt")

#----------------------Combine results of the Severity Results Distributions-------------------------
stargazer(glm.lognorm, glm.gamma, type="text",
          dep.var.labels=c("Severity Distribution Results"),
          out="severity_model.txt")

#---------------------------------Descriptive Statistics-------------------------------------------
describe(motor_insur_df)
skimr::skim(motor_insur_data)

df_status(motor_insur_data)
plot_num(motor_insur_data)
profiling_num(motor_insur_data)
freq(motor_insur_data)
summary(motor_insur_data)
str(motor_insur_data)

my_data$Sum_insure<- log(motor_insur_data$Sum_insure)
my_data$claim_amount<- log(motor_insur_data$claim_amount)
my_data$claim_count<- log(motor_insur_data$claim_count)

#write.csv(motor_insur_data,'data/murtada.csv')


##-Frequency ggplot---------------------------------------------------
#Gender Frequency
cont_gender_freq<- motor_insur_df %>% 
  filter(Claim_count>0) %>% 
  group_by(Gender) %>% 
  summarise(Claim_count=sum(Claim_count)) %>% 
  mutate(Frequency=  case_when(
    Gender=="Male" ~ paste0(round(((Claim_count)/12311)*100,1),"%"),
    Gender=="Female" ~ paste0(round(((Claim_count)/2374)* 100,1),"%")))

ggGenderFreq<- ggplot(cont_gender_freq, aes(x=Gender, y=Frequency, fill=Gender))+ 
geom_bar (stat="identity")+
labs(title = "Gender Frequency")+
xlab("Gender") + ylab("Frequency")

  ggGenderFreq

#Severity-----------------
  # Gender
  cont_Gender_sevrty<- motor_insur_df %>% 
    filter(Claim_amount>0) %>% 
    group_by(Gender) %>% 
    summarise(Claim_amount=sum(Claim_amount), Claim_count=sum(Claim_count)) %>% 
    mutate(Frequency_qender=  case_when(
      Gender=="Male" ~ paste0(round(Claim_amount/Claim_count,2),"%"),
      Gender=="Female" ~ paste0(round(Claim_amount/Claim_count,2),"%")))
  
  ggGender_Severity<- ggplot(cont_Gender_sevrty, aes(x=Gender, y=Frequency_qender, fill=Gender))+ 
    geom_bar (stat="identity")+
    labs(title = "Gender Severity")+
    xlab("Gender") + ylab("Frequency")
  
  ggGender_Severity
#----------------------------------------------------------------
# Age_of_Vehicle Frequency
cont_age_of_vchile_freq<- motor_insur_df %>% 
  filter(Claim_count>0) %>% 
  group_by(Age_of_Vehicle) %>% 
  summarise(Claim_count=sum(Claim_count)) %>%  
  mutate(Freq=  case_when(
    Age_of_Vehicle=="0-5" ~ paste0(round(((Claim_count)/4438)* 100,1),"%"),
    Age_of_Vehicle=="6-10" ~ paste0(round(((Claim_count)/5356)*100,1),"%"),
    Age_of_Vehicle=="11-20" ~ paste0(round(((Claim_count)/4345)* 100,1),"%"),
    Age_of_Vehicle==">20" ~ paste0(round(((Claim_count)/546)* 100,1),"%")))

ggage_of_vchileFreq<- ggplot(cont_age_of_vchile_freq, aes(x=Age_of_Vehicle, y=Freq, fill=Age_of_Vehicle))+ 
  geom_bar (stat="identity")+
  labs(title = "Age of Vehicle Frequency")+
  xlab("Age of Vehicle") + ylab("Frequency")
  
  ggage_of_vchileFreq
#-------------------------------------------------
  #Age_of_Vehicle 
  cont_Age_of_Vehicle_sevrty<- motor_insur_df %>% 
    filter(Claim_amount>0) %>% 
    group_by(Age_of_Vehicle) %>% 
    summarise(Claim_amount=sum(Claim_amount), Claim_count=sum(Claim_count)) %>% 
    mutate(FreAge_of_Vehicle=  case_when(
      str_detect(Age_of_Vehicle,"6-10") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Age_of_Vehicle,"0-5") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Age_of_Vehicle,"11-20") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Age_of_Vehicle,">20") ~paste0(round(Claim_amount/Claim_count,2),"%"),))
  
  ggAge_of_Vehicle_Severity<- ggplot(cont_Age_of_Vehicle_sevrty, aes(x=Age_of_Vehicle, y=FreAge_of_Vehicle, fill=Age_of_Vehicle))+ 
    geom_bar (stat="identity")+
    labs(title = "Age of Vehicle Severity")+
    xlab("Age of Vehicle ") + ylab("Frequency")
  
  ggAge_of_Vehicle_Severity
  #-------------------------------------------------

  # Model Frequency
  cont_Model_freq<- motor_insur_df %>% 
    filter(Claim_count>0) %>% 
    group_by(Model) %>% 
    summarise(Claim_count=sum(Claim_count)) %>% 
    mutate(Frequency=  case_when(
      str_detect(Model,"Accent") ~ paste0(round(((Claim_count)/2262)* 100,1),"%"),
      str_detect(Model,"Click") ~ paste0(round(((Claim_count)/948)* 100,1),"%"),
      str_detect(Model,"Tucson") ~ paste0(round(((Claim_count)/842)* 100,1),"%"),
      str_detect(Model,"Santa Fe") ~ paste0(round(((Claim_count)/329)* 100,1),"%"),
      str_detect(Model,"Visto") ~ paste0(round(((Claim_count)/445)* 100,1),"%"),
      str_detect(Model,"Corolla") ~ paste0(round(((Claim_count)/1354)* 100,1),"%"),
      str_detect(Model,"Hilux") ~ paste0(round(((Claim_count)/1072)* 100,1),"%"),
      str_detect(Model,"Prado / Land Cruiser") ~ paste0(round(((Claim_count)/625)* 100,1),"%"),
      str_detect(Model,"Lancer") ~ paste0(round(((Claim_count)/303)* 100,1),"%"),
      str_detect(Model,"BYD") ~ paste0(round(((Claim_count)/235)* 100,1),"%"),
      str_detect(Model,"Others") ~ paste0(round(((Claim_count)/6270)* 100,1),"%")))
  
  ggModelFreq<- ggplot(cont_Model_freq, aes(x=Model, y=Frequency, fill=Model))+ 
    geom_bar (stat="identity")+
    labs(title = "Model Frequency")+
    xlab("Model") + ylab("Frequency")
  
  ggModelFreq
  
  #--------------
  
  # Model
  cont_Model_sevrty<- motor_insur_df %>% 
    filter(Claim_amount>0) %>% 
    group_by(Model) %>% 
    summarise(Claim_amount=sum(Claim_amount), Claim_count=sum(Claim_count)) %>% 
    mutate(FreModel=  case_when(
      str_detect(Model,"Accent") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Click") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Tucson") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Accent") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Santa Fe") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Visto") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Corolla") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Hilux") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Prado /Land Cruiser") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Lancer") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"BYD") ~paste0(round(Claim_amount/Claim_count,2),"%"),
      str_detect(Model,"Others") ~paste0(round(Claim_amount/Claim_count,2),"%"),))
  
  ggModel_Severity<- ggplot(cont_Model_sevrty, aes(x=Model, y=FreModel, fill=Model))+ 
    geom_bar (stat="identity")+
    labs(title = "Model Severity")+
    xlab("Model") + ylab("Frequency")
  
  ggModel_Severity
  
  #---------------
  # Age Frequency
  cont_age_freq<- motor_insur_df %>% 
    filter(Claim_count>0) %>% 
    group_by(Age) %>% 
    summarise(Claim_count=sum(Claim_count)) %>% 
    mutate(Freq_age=  case_when(
      str_detect(Age,"31-60") ~ paste0(round(((Claim_count)/10417)* 100,1),"%"),
      str_detect(Age,"25-30") ~ paste0(round(((Claim_count)/1801)* 100,1),"%"),
      str_detect(Age,">60") ~ paste0(round(((Claim_count)/1584)* 100,1),"%"),
      str_detect(Age,"18-24") ~ paste0(round(((Claim_count)/883)* 100,1),"%")))
  
  ggage_Freq<- ggplot(cont_age_freq, aes(x=Age, y=Freq_age, fill=Age))+ 
    geom_bar (stat="identity")+
    labs(title = "Age Frequency")+
    xlab("Age") + ylab("Frequency")
  
  ggage_Freq
  
  #-----------------------
  # Residence Frequency
  cont_Residence_freq<- motor_insur_df %>% 
    filter(Claim_count>0) %>% 
    group_by(Residence) %>% 
    summarise(Claim_count=sum(Claim_count)) %>% 
    mutate(Freq_Residence=  case_when(
      str_detect(Residence,"Khartoum") ~ paste0(round(((Claim_count)/6504)* 100,1),"%"),
      str_detect(Residence,"Oumdrman") ~ paste0(round(((Claim_count)/4402)* 100,1),"%"),
      str_detect(Residence,"Khartoum Bahari") ~ paste0((((Claim_count)/3779)* 100,1),"%")))
  
  ggResidence_Freq<- ggplot(cont_Residence_freq, aes(x=Residence, y=Freq_Residence, fill=Residence))+ 
    geom_bar (stat="identity")+
    labs(title = "Residence Frequency")+
    xlab("Residence") + ylab("Frequency")
  ggResidence_Freq
  
  #----------------------------------------------------------
  #  Manufac_country Frequency
  cont_Manufac_country_freq<- motor_insur_df %>% 
    filter(Claim_count>0) %>% 
    group_by(Manufac_country) %>% 
    summarise(Claim_count=sum(Claim_count)) %>% 
    mutate(Freq_Manufac_country=  case_when(
      str_detect(Manufac_country,"Korea") ~ paste0(round(((Claim_count)/7576)* 100,1),"%"),
      str_detect(Manufac_country,"Japan") ~ paste0(round(((Claim_count)/4871)* 100,1),"%"),
      str_detect(Manufac_country,"Sudan") ~ paste0(round(((Claim_count)/1157)* 100,1),"%"),
      str_detect(Manufac_country,"Germany") ~ paste0(round(((Claim_count)/584)* 100,1),"%"),
      str_detect(Manufac_country,"Czech") ~ paste0(round(((Claim_count)/240)* 100,1),"%"),
      str_detect(Manufac_country,"Others") ~ paste0(round(((Claim_count)/257)* 100,1),"%")))
  
  ggManufacure_Freq<- ggplot(cont_Manufac_country_freq, aes(x=Manufac_country, y=Freq_Manufac_country, fill=Manufac_country))+ 
    geom_bar (stat="identity")+
    labs(title = "Manufacure country Frequency")+
    xlab("Manufacure country") + ylab("Frequency")
  
  ggManufacure_Freq
  #--------------------------------------------------------  
  # Make Frequency
  cont_Make_freq<- motor_insur_df %>% 
    filter(Claim_count>0) %>% 
    group_by(Make) %>% 
    summarise(Claim_count=sum(Claim_count)) %>% 
    mutate(Frequency=  case_when(
      str_detect(Make,"Hyundai") ~ paste0(round(((Claim_count)/6143)* 100,1),"%"),
      str_detect(Make,"Kia") ~ paste0(round(((Claim_count)/1100)* 100,1),"%"),
      str_detect(Make,"Toyota") ~ paste0(round(((Claim_count)/3671)* 100,1),"%"),
      str_detect(Make,"Mitsubishi") ~ paste0(round(((Claim_count)/529)* 100,1),"%"),
      str_detect(Make,"Giad") ~ paste0(round(((Claim_count)/1157)* 100,1),"%"),
      str_detect(Make,"Mercedes") ~ paste0(round(((Claim_count)/254)* 100,1),"%"),
      str_detect(Make,"Skoda") ~ paste0(round(((Claim_count)/240)* 100,1),"%"),
      str_detect(Make,"Others") ~ paste0(round(((Claim_count)/1591)* 100,1),"%")))
  
  ggMakeFreq<- ggplot(cont_Make_freq, aes(x=Make, y=Frequency, fill=Make))+ 
    geom_bar (stat="identity")+
    labs(title = "Make Frequency")+
    xlab("Make") + ylab("Frequency")
  
  ggMakeFreq
  #-----------------
  
# occupation
cont_occupation_sevrty<- motor_insur_df %>% 
  filter(Claim_amount>0) %>% 
  group_by(Occupation) %>% 
  summarise(Claim_amount=sum(Claim_amount), Claim_count=sum(Claim_count)) %>% 
  mutate(FreOccupation=  case_when(
  str_detect(Occupation,"Employee") ~paste0(round(Claim_amount/Claim_count,2),"%"),
  str_detect(Occupation,"Engineer and Programmer") ~paste0(round(Claim_amount/Claim_count,2),"%"),
  str_detect(Occupation,"Medical Profession") ~paste0(round(Claim_amount/Claim_count,2),"%"),
  str_detect(Occupation,"Business man/woman") ~paste0(round(Claim_amount/Claim_count,2),"%"),
  str_detect(Occupation,"Free Lancer") ~paste0(round(Claim_amount/Claim_count,2),"%"),
  str_detect(Occupation,"Student") ~paste0(round(Claim_amount/Claim_count,2),"%"),
  str_detect(Occupation,"Others") ~paste0(round(Claim_amount/Claim_count,2),"%"),))

ggoccupation_Severity<- ggplot(cont_occupation_sevrty, aes(x=Occupation, y=FreOccupation, fill=Occupation))+ 
  geom_bar (stat="identity")+
  labs(title = "Occupation Severity")+
  xlab("Occupation") + ylab("Frequency")

ggoccupation_Severity

#-----------------------------------------------------------------------------------------------
table(motor_insur_data$Claim_count)
reported<- read_excel("reported.xlsx")
lm_freq<-  glm(formula = log(frequency) ~ claim ,family = gaussian(link = "identity"), data = reported)
summary(lm_freq)
plot(lm_freq)
#-----------------------------------------------------------------------------------------------

#------------------------------------------Chart
#s-----------------------------------------------------
# Age frequency

chartr_freq_Age_<- motor_insur_df %>% 
  count(Age) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart(Age,Number.of.policyholders)+
  theme_minimal()+
  ggtitle(" Frequency of Age and Number of Policyholders")
chartr_freq_Age_ +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")+
  ylab("Number of policyholders")


# Age_of_Vehicle frequency
chartr_freq_Age_of_Vehicle<- motor_insur_df %>% 
  count(Age_of_Vehicle) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart(Age_of_Vehicle,Number.of.policyholders)+
  theme_minimal()+
  ggtitle("Frequency of Age_of_Vehicle and Number of Policyholders")
chartr_freq_Age_of_Vehicle +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")+
  ylab("Number of policyholders")

# Manufacture country frequency
chartr_freq_Manufac_country<- motor_insur_df %>% 
  count( Manufac_country) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Manufac_country,Number.of.policyholders, bar_color = "purple")+
  theme_minimal()+  
  ggtitle(" Frequency of Manufacure country and Number of Policyholders")
chartr_freq_Manufac_country +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Occupation frequency
chartr_freq_Occupation<- motor_insur_df %>% 
  count( Occupation) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Occupation,Number.of.policyholders)+
  theme_minimal()+
  coord_flip() +
  ggtitle(" Frequency of Occupation and Number of Policyholders")
chartr_freq_Occupation +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")

# Location frequency
chartr_freq_Location<- motor_insur_df %>% 
  count( Location) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Location,Number.of.policyholders)+
  theme_minimal()+
  ggtitle(" Frequency of Location and Number of Policyholders")
chartr_freq_Location +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")+
  ylab("Number of policyholders")

# Cylinder_capacity frequency
chartr_freq_Cylinder_capacity<- motor_insur_df %>% 
  count( Cylinder_capacity) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Cylinder_capacity,Number.of.policyholders)+
  theme_minimal()+
  ggtitle(" Frequency of Cylinder capacity and Number of Policyholders")
chartr_freq_Cylinder_capacity +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")+
  ylab("Number of policyholders")

# Make frequency
chartr_freq_Make<- motor_insur_df %>% 
  count( Make) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  column_chart( Make,Number.of.policyholders)+
  theme_minimal()+
  ggtitle(" Frequency of Make and Number of Policyholders")
chartr_freq_Make +
  geom_text(aes(label=Number.of.policyholders), vjust=1.2, color="white")+
  ylab("Number of policyholders")

# Age pct %
chartr_pct<- motor_insur_df %>% 
  #filter(Claim_count>0) %>% 
  count(Age) %>% 
  arrange(-n) %>% 
  mutate(Number.of.policyholders=n) %>% 
  column_chart(Age,Number.of.policyholders)+
  theme_minimal()+
  xlab("Age %") + ylab("Number of policyholders")+ 
  ggtitle(" Frequency of Age Perantage")
chartr_pct +
  geom_text(aes(label=paste0(Number.of.policyholders,"%")), vjust=1.2, color="white")


