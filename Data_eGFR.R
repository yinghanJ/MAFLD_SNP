######################################
### CKD with eGFR and Microalbumin ###
######################################

#Load Packages
library(dplyr)
library(tidyverse)

#Read in data
df <- read.csv("Data/Data_T2D.csv")

#Deleting observations that are NA for eGFR
df <- df %>% drop_na(year_of_birth_f34_0_0, creatinine_assay_date_f30701_0_0, creatinine_f30700_0_0)

#Converting to Date format
df$creatinine_assay_date_f30701_0_0 <- as.Date(df$creatinine_assay_date_f30701_0_0, format = "%Y-%m-%d")

#Changing the creatinine from umol/L to mg/dl
df$creatinine_f30700_0_0 <- df$creatinine_f30700_0_0/88.4

#Making lists for eGFR calculation
sex <- df$sex_f31_0_0
age <- as.numeric(format(df$creatinine_assay_date_f30701_0_0,'%Y'))- df$year_of_birth_f34_0_0
creatinine <- df$creatinine_f30700_0_0

#eGFR from 2021 equation
df <- df %>%
  mutate(eGFR = case_when(
    sex == 1 ~ 141 * (ifelse(creatinine/0.9 < 1, creatinine/0.9, 1))^(-0.411) *
      (ifelse(creatinine/0.9 >= 1, creatinine/0.9, 1))^(-1.209) *
      0.993^(age),
    sex == 0 ~ 141 * (ifelse(creatinine/0.7 < 1, creatinine/0.7, 1))^(-0.329) *
      (ifelse(creatinine/0.7 >= 1, creatinine/0.7, 1))^(-1.209) *
      0.993^(age) * 1.018
  ))


#Rounding the eGFR
df$eGFR <- round(df$eGFR, digits = 0)

#CKD variables

df <- df %>% mutate(CKD = case_when(
  eGFR > 60 ~ 0,
  TRUE ~ 1
))

clinical_df_diabetes <- df[df$T2D == 1, ]
clinical_df_nondiabetic <- df[df$T2D == 0, ]

