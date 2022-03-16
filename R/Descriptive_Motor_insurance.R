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
library(patchwork)
library(gt)
library(reactable)
library(DT)
library(knitr)
#---------------------------------Source Code---------------------------------------------------------
#insur_data <- read_excel("C:/Users/murtada/Desktop/Motor_insurance/data/Descriptive_Data.xlsx")

insur_data <- read_excel("data/Descriptive_Data.xlsx")

View(insur_data)

motor_insur_df <- insur_data %>%
  
  dplyr::select(Gender,Age,Occupation,Residence,Driving_Experience,
                Manufacturing_Country,Type,Model,Vehicle_Age,Cylinder_Capacity,
                Claim_Count,Claim_Amount) %>% 
  
  # Gender
  mutate(Gender = factor(Gender, levels = c(1,2),
                         labels = c("Male","Female"))) %>% 
  # Age
  mutate(Age=round(Age,0)) %>% 
  mutate(Age = case_when(
    between(Age, 17, 24) ~1,
    between(Age, 25,30) ~2,
    between(Age ,31, 60) ~3,
    Age >60 ~ 4)) %>% 
  mutate(Age = factor(Age, levels = c(1,2,3,4), 
                      labels = c("18-24","25-30","31-60",">60"))) %>% 
  
  # Occupation
  mutate(Occupation= factor(Occupation, 
                            levels = c(1,2,3,4,5,6,7), 
                            labels = c("Employee","Engineer and Programmer","Medical Profession","Business man/woman","Free Lancer","Student","Others"))) %>% 
  
  # Residence
  mutate(Residence = factor(Residence, 
                            levels = c(1,2,3),
                            labels = c("Khartoum","Khartoum Bahari","Oumdrman") )) %>% 
  
  # Manufacutring_country
  mutate(Manufacturing_Country = factor(Manufacturing_Country, 
                                  levels = c(1,2,3,4,5,6),
                                  labels = c("Korea","Japan","Sudan","Germany","Czech","Others"))) %>% 
  
  # Type 
  mutate(Type = factor(Type,
                      levels = c(1,2,3,4,5,6,7,8),
                      labels = c("Hyundai","Kia","Toyota","Mitsubishi","Giad","Mercedes","Skoda","Others"))) %>% 
  
  # Model
  mutate(Model = factor(Model,
                        levels = c(1,2,3,4,5,6,7,8,9,10,11) ,
                        labels = c("Accent","Click","Tucson","Santa Fe","Visto","Corolla","Hilux","Prado / Land Cruiser","Lancer","BYD","Others"))) %>% 
  
  # Age_of_Vehicle
  mutate(Vehicle_Age = case_when(
    between(Vehicle_Age, 2011 , 2017) ~ 1,
    between(Vehicle_Age, 2006 ,2010) ~ 2,
    between(Vehicle_Age, 1996 ,2005) ~ 3,
    TRUE ~ 4)) %>%  
  mutate(Vehicle_Age = factor(Vehicle_Age, 
                                 levels = c(1,2,3,4), 
                                 labels = c("0-5","6-10","11-20",">20"))) %>%
  
  # Cylinder_capacity
  mutate(Cylinder_Capacity = factor(Cylinder_Capacity, 
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

#-----------------------------------------------
#filter by Gender and Residence
filter_Gender_Age<- motor_insur_df %>% 
  select(Claim_Count,Gender,Age) %>% 
  filter(Claim_Count>0) %>% 
  group_by(Gender,Age) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  pivot_wider(names_from = Gender, values_from = Claim_Count) %>% 
  ungroup() 

datatable(data=filter_Gender_Age, rownames = FALSE,
          caption ="Relation Between Gender - Age" , 
          colnames = c("Age", "Male","Female")) 
plot_ly(filter_Gender_Age,
        x=~Age ,
        y=~Male,
        type = "bar",
        name="Male") %>% 
  add_trace(y=~Female,
            name="Female") %>% 
  layout(title="Relation Between Gender - Age",
         xaxis=list(title="Age", zeroline=FALSE),
         yaxis=list(title="Annual Claim Frequency", zeroline=FALSE),
         barmode="group")

#-------------------------filter by Gender and Residence
filter_Gender_Residence<- motor_insur_df %>% 
  select(Claim_Count,Gender,Residence) %>% 
  filter(Claim_Count>0) %>% 
  group_by(Gender,Residence) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  pivot_wider(names_from = Gender, values_from = Claim_Count) %>% 
  ungroup()

datatable(data=filter_Gender_Residence, rownames = FALSE,
          caption ="Relation Between Residence & Gender" , 
          colnames = c("Residence", "Male","Female")) 

plot_ly(filter_Gender_Residence,
        x=~Residence ,
        y=~Male,
        type = "bar",
        name="Male") %>% 
  add_trace(y=~Female,
            name="Female") %>% 
  layout(title="Relation Between Residence & Gender",
         xaxis=list(title="Residence", zeroline=FALSE,
                    yaxis=list(title="Annual Claim Frequency"), zeroline=FALSE,
                    barmode="group")) 

#--------------------Gender and Occupation
filter_Gender_Occupation <- motor_insur_df %>% 
  select(Claim_Count,Gender,Occupation) %>% 
  filter(Claim_Count>0) %>% 
  group_by(Gender,Occupation) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  pivot_wider(names_from = Gender, values_from = Claim_Count) %>% 
  ungroup()

datatable(data=filter_Gender_Occupation, rownames = FALSE,
          caption ="Relation Between Gender - Occupation" , 
          colnames = c("Occupation", "Male","Female")) 

plot_ly(filter_Gender_Occupation,
        x=~Occupation ,
        y=~Male,
        type = "bar",
        name="Male") %>% 
  add_trace(y=~Female,
            name="Female") %>% 
  
  layout(title="Relation Between Gender - Occupation",
         xaxis=list(title="Occupation", zeroline=FALSE),
         yaxis=list(title="Annual Claim Frequency", zeroline=FALSE),
         barmode="group")

#-------------Type_Vehicle_Age
filter_Type_Vehicle_Age <- motor_insur_df %>% 
  select(Claim_Count,Type,Vehicle_Age) %>% 
  filter(Claim_Count>0) %>% 
  group_by(Type,Vehicle_Age) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  pivot_wider(names_from = Type, values_from = Claim_Count) %>% 
mutate(Vehicle_Age = case_when(
  str_detect(Vehicle_Age,'0-5') ~ "2011-2017",
  str_detect(Vehicle_Age,'6-10') ~ "2006-2010",
  str_detect(Vehicle_Age,'11-20') ~ "1996-2005",
  str_detect(Vehicle_Age,'>20') ~ "<1996"))

datatable(data=filter_Type_Vehicle_Age, rownames = FALSE,
          caption ="Relation Between Type - Age of Vehicle" , 
          colnames = c("Age of Vehicle","Hyundai", "Kia", "Toyota", "Mitsubishi","Giad","Mercedes","Skoda","Others")) 

plot_ly(filter_Type_Vehicle_Age,
        x=~Vehicle_Age ,
        y=~Hyundai,
        type = "bar",
        name="Hyundai") %>% 
  add_trace(y=~Kia,
            name="Kia") %>% 
  add_trace(y=~Toyota,
            name="Toyota") %>% 
  add_trace(y=~Mitsubishi,
            name="Mitsubishi") %>% 
  add_trace(y=~Giad,
            name="Giad") %>% 
  add_trace(y=~Mercedes,
            name="Mercedes") %>% 
  add_trace(y=~Skoda,
            name="Skoda") %>% 
  add_trace(y=~Others,
            name="Others") %>% 
  layout(title="Relation Between Type - Age of Vehicle",
         xaxis=list(title="Age of Vehicle", zeroline=FALSE),
         yaxis=list(title="Annual Claim Frequency", zeroline=FALSE),
         barmode="group")

#------------------


descrip_data<-motor_insur_df %>% 
  set_names("Gender","Age","Occupation","Residence","Driving Experience","Manufacturing Country","Type","Model","Vehicle Age","Cylinder Capacity","Claim Count","ClaimAmount") 


summary(descrip_data)

descrip_data1<- motor_insur_df %>% 
  filter(Claim_Amount>0) %>% 
  select (Claim_Count,Claim_Amount) %>% 
  mutate(Sum_Claim_Amount= (Claim_Amount/Claim_Count))

skim(descrip_data1)

# Descriptive Models------------------------------------------------
#Age
# Age Frequency
cont_age_freq<- motor_insur_df %>% 
  filter(Claim_Count>0) %>% 
  group_by(Age) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  mutate(Freq_age=  case_when(
    str_detect(Age,"18-24") ~ round(Claim_Count/883,3),
    str_detect(Age,"25-30") ~ round(Claim_Count/1801,3),
    str_detect(Age,"31-60") ~ round(Claim_Count/10417,3), 
    str_detect(Age,">60") ~ round(Claim_Count/1584,3)))

ggage_Freq<- ggplot(cont_age_freq, aes(x=Age, y=Freq_age, fill=Age))+ 
  geom_bar (stat="identity",size=1, width = 1 , position = position_dodge(width = .1))+
  geom_text(aes(label=Freq_age , hjust=0.5, vjust=1.5), color="white")+
  labs(title = "Age - Frequency")+
  theme_minimal()+
  theme(legend.position = "nonw" )+
  xlab("Age") + ylab("Annual Claim Frequency") + ylim(0,0.6)

ggage_Freq

#-----------------------------------------------------------

#Capacity_policyholder
Capacity_policyholder<- motor_insur_df %>% 
  count(Cylinder_Capacity) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  mutate(Freq_Capacity=  case_when(
    str_detect(Cylinder_Capacity,"Large") ~ (Number.of.policyholders),
    str_detect(Cylinder_Capacity,"Medium") ~ (Number.of.policyholders),
    str_detect(Cylinder_Capacity,"Small") ~ (Number.of.policyholders)))

ggCapacity_policyholder<- ggplot(Capacity_policyholder, aes(x=Cylinder_Capacity, y=Freq_Capacity, fill=Cylinder_Capacity))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity",size=2, width = .9 , position = position_dodge(width = 1))+
  geom_text(aes(label=Freq_Type , hjust=0.5, vjust=1.5), color="white")+
  labs(title = "Cylinder Capacity - Number of policyholders")+
  xlab("Cylinder Capacity") + ylab("Number of Policyholders")+ ylim(0,10000)

ggCapacity_policyholder
#-----------------------------------------------------
#Gender Frequency
cont_gender_freq<- motor_insur_df %>% 
  filter(Claim_Count>0) %>% 
  group_by(Gender) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  mutate(Frequency=  case_when(
    Gender=="Male" ~ paste0(round(((Claim_Count)/12311)*100,1),"%"),
    Gender=="Female" ~ paste0(round(((Claim_Count)/2374)*100,1),"%")))

ggGenderFreq<- ggplot(cont_gender_freq, aes(x=Gender, y=Frequency, fill=Gender))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  #scale_fill_manual(values = c("darkorange","cyan4", guide=FALSE))+
  geom_bar (stat="identity", alpha=0.8)+
  labs(title = "Gender - Frequency")+
  xlab("Gender") + ylab("Annual Claim Frequency")
ggGenderFreq

#Gender Severity ----------------
cont_Gender_sevrty<- motor_insur_df %>% 
  filter(Claim_Amount>0) %>% 
  group_by(Gender) %>% 
  summarise(Claim_Amount=sum(Claim_Amount), Claim_Count=sum(Claim_Count)) %>% 
  mutate(Frequency_qender=  case_when(
    Gender=="Male" ~ round((Claim_Amount/Claim_Count),3),
    Gender=="Female" ~ round((Claim_Amount/Claim_Count),3)))
ggGender_Severity<- ggplot(cont_Gender_sevrty, aes(x=Gender, y=Frequency_qender, fill=Gender))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  scale_fill_manual(values = c("darkorange","cyan4", guide=FALSE))+
  geom_bar (stat="identity",size=2, width = .9 , position = position_dodge(width = 0.9))+
  geom_text(aes(label=Frequency_qender, hjust=0.5, vjust=1.5), color="white")+
  labs(title = "Gender - Severity")+
  xlab("Gender") + ylab("Annual Claim Severity")
ggGender_Severity

ggGenderFreq+ ggGender_Severity
#---------------------------------------------------------------------
# Model Severity
cont_Model_sevrty<- motor_insur_df %>% 
  filter(Claim_Amount>0) %>% 
  group_by(Model) %>% 
  summarise(Claim_Amount=sum(Claim_Amount), Claim_Count=sum(Claim_Count)) %>% 
  mutate(FreModel=  case_when(
    Model=="Accent" ~ round(Claim_Amount/Claim_Count,0),
    Model== "Click" ~ round(Claim_Amount/Claim_Count,0),
    Model == "Tucson" ~ round(Claim_Amount/Claim_Count,0),
    Model == "Accent" ~ round(Claim_Amount/Claim_Count,0),
    Model == "Santa Fe" ~ round(Claim_Amount/Claim_Count,0),
    Model == "Visto" ~ round(Claim_Amount/Claim_Count,0),
    Model == "Corolla"  ~ round(Claim_Amount/Claim_Count,0),
    Model== "Hilux" ~ round(Claim_Amount/Claim_Count,0),
    Model== "Prado / Land Cruiser" ~ round(Claim_Amount/Claim_Count,0),
    Model== "Lancer" ~ round(Claim_Amount/Claim_Count,0),
    Model== "BYD" ~ round(Claim_Amount/Claim_Count,0),
    Model== "Others" ~ round(Claim_Amount/Claim_Count,0))) 
ggModel_Severity<- ggplot(cont_Model_sevrty, aes(x=Model, y=FreModel, fill=Model))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=FreModel))+
  labs(title = "Model - Severity")+
  xlab("Model") + ylab("Annual Claim Severity")
ggModel_Severity

# Model Frequency
cont_Model_freq<- motor_insur_df %>% 
  filter(Claim_Count>0) %>% 
  group_by(Model) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  mutate(Frequency=  case_when(
    str_detect(Model,"Accent") ~ (round(((Claim_Count)/2262),3)),
    str_detect(Model,"Click") ~ (round(((Claim_Count)/948),3)),
    str_detect(Model,"Tucson") ~ (round(((Claim_Count)/842),3)),
    str_detect(Model,"Santa Fe") ~ (round(((Claim_Count)/329),3)),
    str_detect(Model,"Visto") ~ (round(((Claim_Count)/445),3)),
    str_detect(Model,"Corolla") ~ (round(((Claim_Count)/1354),3)),
    str_detect(Model,"Hilux") ~ (round(((Claim_Count)/1072),3)),
    str_detect(Model,"Prado / Land Cruiser") ~ (round(((Claim_Count)/625),3)),
    str_detect(Model,"Lancer") ~ (round(((Claim_Count)/303),3)),
    str_detect(Model,"BYD") ~ (round(((Claim_Count)/235),3)),
    str_detect(Model,"Others") ~ (round(((Claim_Count)/6270),3))))
ggModelFreq<- ggplot(cont_Model_freq, aes(x=Model, y=Frequency, fill=Model))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=Frequency))+
  labs(title = "Model - Frequency")+
  xlab("Model") + ylab("Annual Claim Frequency")
ggModelFreq

ggModelFreq+ggModel_Severity
#----------------------------------------------------------------
# Age_of_Vehicle Frequency
cont_age_of_vchile_freq<- motor_insur_df %>% 
  filter(Claim_Count>0) %>% 
  group_by(Vehicle_Age) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  mutate(Freq=  case_when(
    Vehicle_Age=="0-5" ~ round((Claim_Count/4438),3),
    Vehicle_Age=="6-10" ~ round((Claim_Count/5356),3),
    Vehicle_Age=="11-20" ~ round((Claim_Count/4345),3),
    Vehicle_Age==">20" ~ round((Claim_Count/546),3)))

ggage_of_vchileFreq<- ggplot(cont_age_of_vchile_freq, aes(x=Vehicle_Age, y=Freq, fill=Vehicle_Age))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=Freq))+
  labs(title = "Vehicle Age - Frequency")+
  xlab("Vehicle Age") + ylab("Annual Claim Frequency")
ggage_of_vchileFreq

#-------------------------------------------------
#Age_of_Vehicle 
cont_Age_of_Vehicle_sevrty<- motor_insur_df %>% 
  filter(Claim_Amount>0) %>% 
  group_by(Vehicle_Age) %>% 
  summarise(Claim_Amount=sum(Claim_Amount), Claim_Count=sum(Claim_Count)) %>% 
  mutate(FreAge_of_Vehicle=  case_when(
    str_detect(Vehicle_Age,"6-10") ~(round(Claim_Amount/Claim_Count,0)),
    str_detect(Vehicle_Age,"0-5") ~(round(Claim_Amount/Claim_Count,0)),
    str_detect(Vehicle_Age,"11-20") ~(round(Claim_Amount/Claim_Count,0)),
    str_detect(Vehicle_Age,">20") ~(round(Claim_Amount/Claim_Count,0))))

ggAge_of_Vehicle_Severity<- ggplot(cont_Age_of_Vehicle_sevrty, aes(x=Vehicle_Age, y=FreAge_of_Vehicle, fill=Vehicle_Age))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=FreAge_of_Vehicle))+
  labs(title = "Vehicle Age - Severity")+
  xlab("Vehicle Age") + ylab("Annual Claim Severity")
ggAge_of_Vehicle_Severity

ggage_of_vchileFreq+ ggAge_of_Vehicle_Severity

#-------------------------------------------------
#Occupation Sevrety
cont_occupation_Sevrety<- motor_insur_df %>% 
  filter(Claim_Amount>0) %>% 
  group_by(Occupation) %>% 
  summarise(Claim_Amount=sum(Claim_Amount), Claim_Count=sum(Claim_Count)) %>% 
  mutate(FreOccupation=  case_when(
    str_detect(Occupation,"Employee") ~(round(Claim_Amount/Claim_Count,0)),
    str_detect(Occupation,"Engineer and Programmer") ~(round(Claim_Amount/Claim_Count,0)),
    str_detect(Occupation,"Medical Profession") ~(round(Claim_Amount/Claim_Count,0)),
    str_detect(Occupation,"Business man/woman") ~(round(Claim_Amount/Claim_Count,0)),
    str_detect(Occupation,"Free Lancer") ~(round(Claim_Amount/Claim_Count)),
    str_detect(Occupation,"Student") ~(round(Claim_Amount/Claim_Count,0)),
    str_detect(Occupation,"Others") ~(round(Claim_Amount/Claim_Count,0)),))

ggoccupation_Sevrty<- ggplot(cont_occupation_Sevrety, aes(x=Occupation, y=FreOccupation, fill=Occupation))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=FreOccupation))+
  labs(title = "Occupation - Severity")+
  xlab("Occupation") + ylab("Annual Claim Severity")
ggoccupation_Sevrty
#--------------------------------------------
# Occupation frequncy
cont_occupation_freq<- motor_insur_df %>% 
  filter(Claim_Amount>0) %>% 
  group_by(Occupation) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  mutate(FreOccupation=  case_when(
    str_detect(Occupation,"Employee") ~ round((Claim_Count/6424),3),
    str_detect(Occupation,"Engineer and Programmer") ~ round((Claim_Count/1957),3),
    str_detect(Occupation,"Medical Profession") ~ round((Claim_Count/986),3),
    str_detect(Occupation,"Business man/woman") ~ round((Claim_Count/652),3),
    str_detect(Occupation,"Free Lancer") ~ round((Claim_Count/1635),3),
    str_detect(Occupation,"Student") ~ round((Claim_Count/613),3),
    str_detect(Occupation,"Others") ~ round((Claim_Count/2418),3)))

ggoccupation_Freq<- ggplot(cont_occupation_freq, aes(x=Occupation, y=FreOccupation, fill=Occupation))+
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=FreOccupation))+
  labs(title = "Occupation - Frequency")+
  xlab("Occupation") + ylab("Annual Claim Frequency")
ggoccupation_Freq

ggoccupation_Freq+ggoccupation_Sevrty

# Freq_Age
Age_num_policyholder<- motor_insur_df %>% 
  count(Age) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  mutate(Freq_age_num=  case_when(
    str_detect(Age,"18-24") ~ (Number.of.policyholders),
    str_detect(Age,"25-30") ~ (Number.of.policyholders),
    str_detect(Age,"31-60") ~ (Number.of.policyholders), 
    str_detect(Age,">60") ~ (Number.of.policyholders)))
ggage_num_policyholder<- ggplot(Age_num_policyholder, aes(x=Age, y=Freq_age_num, fill=Age))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=Freq_age_num))+
  labs(title = "Age - Number of policyholders")+
  xlab("Age") + ylab("Number of policyholders")
ggage_num_policyholder

#---------------
# Age Frequency
cont_age_freq<- motor_insur_df %>% 
  filter(Claim_Count>0) %>% 
  group_by(Age) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  mutate(Freq_age=  case_when(
    str_detect(Age,"18-24") ~ round(Claim_Count/883,3),
    str_detect(Age,"25-30") ~ round(Claim_Count/1801,3),
    str_detect(Age,"31-60") ~ round(Claim_Count/10417,3), 
    str_detect(Age,">60") ~ round(Claim_Count/1584,3)))

ggage_Freq<- ggplot(cont_age_freq, aes(x=Age, y=Freq_age, fill=Age))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=Freq_age))+
  labs(title = "Age - Frequency")+
  xlab("Age") + ylab("Annual Claim Frequency")
ggage_Freq

ggage_num_policyholder+ggage_Freq


#---------------
#  Manufac_country Frequency
cont_Manufac_country_freq<- motor_insur_df %>% 
  filter(Claim_Count>0) %>% 
  group_by(Manufacturing_Country) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  mutate(Freq_Manufac_country=  case_when(
    str_detect(Manufacturing_Country,"Korea") ~ (round(((Claim_Count)/7576),3)),
    str_detect(Manufacturing_Country,"Japan") ~ (round(((Claim_Count)/4871),3)),
    str_detect(Manufacturing_Country,"Sudan") ~ (round(((Claim_Count)/1157),3)),
    str_detect(Manufacturing_Country,"Germany") ~ (round(((Claim_Count)/584),3)),
    str_detect(Manufacturing_Country,"Czech") ~ (round(((Claim_Count)/240),3)),
    str_detect(Manufacturing_Country,"Others") ~ (round(((Claim_Count)/257),3))))

ggManufacure_Freq<- ggplot(cont_Manufac_country_freq, aes(x=Manufacturing_Country, y=Freq_Manufac_country, fill=Manufacturing_Country))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=Freq_Manufac_country))+
  labs(title = "Manufacuring Country - Frequency")+
  xlab("Manufacuring Country") + ylab("Annual Claim Frequency")

ggManufacure_Freq

#------------
Manufacure_policyholder<- motor_insur_df %>% 
  count(Manufacturing_Country) %>% 
  arrange(-n) %>%
  mutate(Number.of.policyholders=n) %>% 
  mutate(Freq_Manufacturing=  case_when(
    str_detect(Manufacturing_Country,"Korea") ~ (Number.of.policyholders),
    str_detect(Manufacturing_Country,"Japan") ~ (Number.of.policyholders),
    str_detect(Manufacturing_Country,"Sudan") ~ (Number.of.policyholders), 
    str_detect(Manufacturing_Country,"Germany") ~ (Number.of.policyholders), 
    str_detect(Manufacturing_Country,"Czech")  ~ (Number.of.policyholders), 
    str_detect(Manufacturing_Country,"Others") ~ (Number.of.policyholders)))

ggManufacure_policyholder<- ggplot(Manufacure_policyholder, aes(x=Manufacturing_Country, y=Freq_Manufacturing, fill=Manufacturing_Country))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity")+
  geom_text(aes(label=Freq_Manufacturing))+
  labs(title = "Manufacuring Country -  Number of policyholders")+
  xlab("Manufacturing Country") + ylab("Number of Policyholders")

ggManufacure_policyholder

ggManufacure_policyholder+ggManufacure_Freq





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


#--------------------------------------------------------  
# Make Frequency
cont_Make_freq<- motor_insur_df %>% 
  filter(Claim_Count>0) %>% 
  group_by(Make) %>% 
  summarise(Claim_Count=sum(Claim_Count)) %>% 
  mutate(Frequency=  case_when(
    str_detect(Make,"Hyundai") ~ (round(((Claim_count)/6143),3),"%"),
    str_detect(Make,"Kia") ~ (round(((Claim_count)/1100),3),"%"),
    str_detect(Make,"Toyota") ~ (round(((Claim_count)/3671),3),"%"),
    str_detect(Make,"Mitsubishi") ~ (round(((Claim_count)/529),3),"%"),
    str_detect(Make,"Giad") ~ (round(((Claim_count)/1157),3),"%"),
    str_detect(Make,"Mercedes") ~ (round(((Claim_count)/254),3),"%"),
    str_detect(Make,"Skoda") ~ (round(((Claim_count)/240),3),"%"),
    str_detect(Make,"Others") ~ (round(((Claim_count)/1591),3)"%")))

cont_Make_freq<- ggplot(cont_Make_freq, aes(x=Make, y=Frequency, fill=Make))+ 
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity",size=2, width = 0.9 , position = position_dodge(width = 2))+      
  theme(axis.text.x=element_text(angle = 90, hjust = 1 , vjust = 0.5))+
  scale_x_discrete(labels=c("Hyundai","Kia","Toyota","Mitsubishi","Giad","Mercedes","Skoda","Others"))+
  geom_text(aes(label=Frequency , hjust=0.5, vjust=1.5), color="white")+
  labs(title = "Make - Frequency")+
  xlab("Make") + ylab("Annual Claim Frequency")+ ylim(0,0.6)
ggMakeFreq
#-----------------
#--Make Sevrety
cont_make_Sevrety<- motor_insur_df %>% 
  filter(Claim_Amount>0) %>% 
  group_by(Make) %>% 
  summarise(Claim_Amount=sum(Claim_Amount), Claim_Count=sum(Claim_Count)) %>% 
  mutate(FreMake=  case_when(
      str_detect(Make,"Hyundai") ~ (round(((Claim_count)/6143),0),"%"),
      str_detect(Make,"Kia") ~ (round(((Claim_count)/1100),0),"%"),
      str_detect(Make,"Toyota") ~ (round(((Claim_count)/3671),0),"%"),
      str_detect(Make,"Mitsubishi") ~ (round(((Claim_count)/529),0),"%"),
      str_detect(Make,"Giad") ~ (round(((Claim_count)/1157),0),"%"),
      str_detect(Make,"Mercedes") ~ (round(((Claim_count)/254),0),"%"),
      str_detect(Make,"Skoda") ~ (round(((Claim_count)/240),0),"%"),
      str_detect(Make,"Others") ~ (round(((Claim_count)/1591),0),"%")))
ggmake_Sevrty<- ggplot(cont_make_Sevrety, aes(x=Make, y=FreMake, fill=Make))+
  theme_minimal()+
  theme(legend.position = "nonw" )+
  geom_bar (stat="identity",size=2, width = .9 , position = position_dodge(width = 0.9))+  theme(axis.text.x=element_text(angle = 90, hjust = 1 , vjust = 0.5))+
  scale_x_discrete(labels=c("Hyundai","Kia","Toyota","Mitsubishi","Giad","Mercedes","Skoda","Others"))+
  geom_text(aes(label=FreMake , hjust=0.5, vjust=1.5), color="white")+
  labs(title = "Make - Severity")+
  xlab("Make") + ylab("Annual Claim Severity")
#ggmake_Sevrty







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
chartr_freq_Cylinder_capacity<- descriptive_df %>% 
  chartr_freq_Cylinder_capacity<- descriptive_df %>% 
  
  descriptive_df$Cylinder_capacity<- factor(descriptive_df$Cylinder_capacity, levels = c("Small", "Medium","Large")) 
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
chartr_pct<- descriptive_df %>% 
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


