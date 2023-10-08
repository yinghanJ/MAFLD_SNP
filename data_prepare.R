library(dplyr)
library(tidyverse)


#Read in data,30700 is Creatinine
df <- read.csv("all_data.csv")
df_egfr_all <- df[, c("eid", "X31.0.0", "X21003.0.0","X30700.0.0")]

#u/mol to mg/dl
Scr <- df_egfr_all$X30700.0.0 / 88.4
#age
age <-df_egfr_all$X21003.0.0
df_egfr_mgdl_nonclean <- cbind(df_egfr_all, Scr, age)
df_egfr_mgdl <- na.omit(df_egfr_mgdl_nonclean)

#egfr model 2021
#GFR = 141 * min(Scr/κ,1)^α * max(Scr/κ, 1)^(-1.209) * 0.993^Age * 1.018 [if female] * 1.159 [if black]
#Define κ and α based on gender
k <- ifelse(df_egfr_mgdl$X31.0.0 == "1", 0.9, 0.7)
a <- ifelse(df_egfr_mgdl$X31.0.0 == "1", -0.411, -0.329)
df_egfr_mgdl <- cbind(df_egfr_mgdl, k, a)

# Calculate GFR
eGFR <- 142 * pmin(df_egfr_mgdl$Scr/k, 1)^a * pmax(df_egfr_mgdl$Scr/k, 1)^(-1.209) * 0.993^(df_egfr_mgdl$age) * 1.018^(ifelse(df_egfr_mgdl$X31.0.0 == "0", 1, 0))
df_egfr <- cbind(df_egfr_mgdl, eGFR)

#build the CKD, greater than 60, label 0, means no disease; less than 60, lable 1
df_egfr$CKD<- ifelse(df_egfr$eGFR > 60, 0, 1)
sum(df_egfr$CKD == 1, na.rm = TRUE)
colnames(df_egfr)[2] <- "sex"

#build common dataset 7 phenotypes,sex,age,Waist circumference,bmi,Diabetes diagnosed by doctor,Age diabetes diagnosed,Started insulin within one year diagnosis of diabetes
df_common_1 <- df[, c("eid", "X48.0.0", "X21001.0.0","X2443.0.0","X2976.0.0","X2986.0.0","X4041.0.0")]
colnames(df_common_1)[2] <- "WHR"
colnames(df_common_1)[3] <- "BMI"
colnames(df_common_1)[4] <- "DM_Doctor"
colnames(df_common_1)[5] <- "DM_age"
colnames(df_common_1)[6] <- "Insulin_use_1Year"
colnames(df_common_1)[7] <- "Gestational_diabetes"
df_common_2 <- df_egfr_mgdl_nonclean[, c("eid", "X31.0.0", "age")]
colnames(df_common_2)[2] <- "sex"
df_common <- merge(df_common_2, df_common_1, by = "eid")

df_Data_T2D <- read.csv("Data_T2D.csv")
#creat diabetes dataset: df_common_non_diabetes and df_common_diabetes
### Definition 1 ###
# Include:
# Diabetes Diagnosed by doctor = 1
# Age diabetes diagnoed >= 35
# Started Insulin within one year = 0

# Exclude:
# Gestational Diabetes only
# Did not report an age of diagnosis (-1 represents "Do not know", -3 represents "Prefer not to answer")
df_common_diabetes_d1 <- df_common %>%
  mutate(T2D = case_when(
    DM_Doctor == 1 & 
      DM_age > 35 &
      Insulin_use_1Year == 0 ~ 1,
    TRUE ~ 0
  ))

#Excluding
df_common_diabetes_d1$T2D[df_common_diabetes_d1$T2D == 1 & is.na(df_common_diabetes_d1$DM_age)] <- 0
df_common_diabetes_d1$T2D[df_common_diabetes_d1$T2D == 1 & !is.na(df_common_diabetes_d1$Gestational_diabetes) & df_common_diabetes_d1$Gestational_diabetes != 0] <- 0

#Diabetes Duration
df_common_diabetes_d1$diabetes_duration <- ifelse(df_common_diabetes_d1$T2D == 1, df_common_diabetes_d1$age - df_common_diabetes_d1$DM_age, NA)

# Definition 2
# Include:
# ICD-10 Primary Data reports Non-insulin-dependent diabetes mellitus (E11) and not diagnosed by a doctor
# Date when Diagnosed is before the date of assessment
# Age when diagnosed > 35


df_common_non_diabetes <- subset(df_common, DM_Doctor == 0)
df_common_diabetes <- subset(df_common, DM_Doctor == 1)

#kidney needed dataset,three main phenotype,egfr,ckd,Microalbumin
df_Microalbumin <- df[, c("eid", "X30500.0.0")]
colnames(df_Microalbumin)[2] <- "Microalbumin"
df_main_kidney_1 <- merge( df_egfr,df_Microalbumin, by = "eid")
df_main_kidney <- df_main_kidney_1[, c("eid", "eGFR", "CKD","Microalbumin")]

df_kidney <- merge(df_common, df_main_kidney, by = "eid")

#creat dataset: df_kidney_non_diabetes and df_kidney_diabetes
write.csv(df_SNP, file = "df_SNP.csv")
