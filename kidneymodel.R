#all data, the number is 488140, common data and the snp,kikney only have 469276, the difference due to the calculate egfr ,delete some na

df_kidneymodel_1 <- merge(df_kidney, df_SNP, by = "eid")
df_kidneymodel_all <- select(df_kidneymodel_1,-DM_age, -DM_Doctor, -Insulin_use_1Year, -Gestational_diabetes)
write.csv(df_kidneymodel_all, file = "df_kidneymodel_all.csv")

#Egfr afterclean 301346 at all
df_kidneymodel_eGFR <- select(df_kidneymodel_all, -CKD, -Microalbumin)
df_kidneymodel_eGFR <- na.omit(df_kidneymodel_eGFR)
df_kidneymodel_eGFR_DM_age  <- select(df_kidneymodel_1,-DM_Doctor.x, -Insulin_use_1Year.x, -Gestational_diabetes.x,-DM_Doctor.y,-Insulin_use_1Year.y, -Gestational_diabetes.y, -CKD, -Microalbumin)
df_kidneymodel_eGFR_DM_age <- na.omit(df_kidneymodel_eGFR_DM_age)

#CKD afterclean 301346 at all
df_kidneymodel_CKD <- select(df_kidneymodel_all, -eGFR, -Microalbumin)
df_kidneymodel_CKD <- na.omit(df_kidneymodel_CKD)

#Microalbumin afterclean 91480 at all
df_kidneymodel_Microalbumin <- select(df_kidneymodel_all, -eGFR, -CKD)
df_kidneymodel_Microalbumin <- na.omit(df_kidneymodel_Microalbumin)
write.csv(df_kidneymodel_Microalbumin, file = "df_kidneymodel_Microalbumin.csv")
write.csv(df_kidneymodel_CKD, file = "df_kidneymodel_CKD.csv")
write.csv(df_kidneymodel_eGFR, file = "df_kidneymodel_eGFR.csv")

#egfr model in the all data
library(dplyr)
library(broom)

SNP_cols <- grep("rs", names(df_kidneymodel_eGFR), value = TRUE)

eGFR_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  eGFR_model <- lm(eGFR ~ sex + age + WHR + BMI + ., data = df_kidneymodel_eGFR[, c(SNP_cols[i], "sex", "age", "WHR", "BMI", "eGFR")])
  
  eGFR_summary <- tidy(eGFR_model)
  
  eGFR_result_all[i, "P_value"] <- eGFR_summary[nrow(eGFR_summary), "p.value"]
  eGFR_result_all[i, "beta"] <- eGFR_summary[nrow(eGFR_summary), "estimate"]
}
write.csv(eGFR_result_all, "eGFR_result_all.csv", row.names = FALSE)
df_eGFR_result_all <- read.csv("eGFR_result_all.csv")

#egfr model in the diabete data
library(tidyr)
library(broom)

SNP_cols_T2D <- grep("rs", names(df_T2D_kidenymodel_eGFR), value = TRUE)

eGFR_result_T2D <- data.frame(SNP = SNP_cols_T2D, P_value = numeric(length(SNP_cols_T2D)), beta = numeric(length(SNP_cols_T2D)))

for (i in 1:length(SNP_cols_T2D)) {
  eGFR_model_T2D <- lm(eGFR ~ sex.x + age.x + WHR.x + BMI.x + DM_age.x + ., data = df_T2D_kidenymodel_eGFR[, c(SNP_cols_T2D[i], "sex.x", "age.x", "WHR.x", "BMI.x", "DM_age.x", "eGFR")])
  
  eGFR_summary_T2D <- tidy (eGFR_model_T2D)
  
  eGFR_result_T2D[i, "P_value"] <- eGFR_summary_T2D[nrow(eGFR_summary_T2D), "p.value"]
  eGFR_result_T2D[i, "beta"] <- eGFR_summary_T2D[nrow(eGFR_summary_T2D), "estimate"]
}
write.csv(eGFR_result_T2D, "eGFR_result_T2D.csv", row.names = FALSE)


df <- read.csv("eGFR_result_T2D.csv")
write_xlsx(df, "eGFR_result_T2D.xlsx")

library(tidyr)
df_T2D_EID_1 <- read.csv("T2D_eid.csv")
df_T2D_EID <- separate(df_T2D_EID_1, x, into = c("x", "eid"), sep = " ", remove = TRUE)

#build data set for t2d

# 按照eid列合并两个数据框
merged_df <- merge(df_T2D_EID, df_kidneymodel_eGFR_DM_age, by = "eid",all = TRUE)
df_merged <- subset(merged_df, select = -c(x, X))
# 将共同的eid数据储存为df_T2D_kidenymodel_eGFR
df_T2D_kidenymodel_eGFR <-  merge(df_T2D_EID, df_kidneymodel_eGFR_DM_age, by = "eid")

df_T2D_kidenymodel_eGFR <- subset(df_kidneymodel_eGFR_DM_age, select = -c(x,X))

# 将其他数据储存为df_nonT2D_kidenymodel_eGFR
# 将df_kidneymodel_eGFR文件中的eid列转换为字符型
df_kidneymodel_eGFR$eid <- as.character(df_kidneymodel_eGFR$eid)

# 删除df_T2D_kidenymodel_eGFR文件中包含的所有行
df_nonT2D_kidenymodel_eGFR <- anti_join(df_kidneymodel_eGFR, df_T2D_kidenymodel_eGFR, by = "eid")

df_nonT2D_kidenymodel_eGFR <- na.omit(df_nonT2D_kidenymodel_eGFR)

SNP_cols_nonT2D <- grep("rs", names(df_nonT2D_kidenymodel_eGFR), value = TRUE)

eGFR_result_nonT2D <- data.frame(SNP = SNP_cols_nonT2D, P_value = numeric(length(SNP_cols_nonT2D)), beta = numeric(length(SNP_cols_nonT2D)))

for (i in 1:length(SNP_cols_nonT2D)) {
  eGFR_model_nonT2D <- lm(eGFR ~ sex + age + WHR + BMI + ., data = df_nonT2D_kidenymodel_eGFR[, c(SNP_cols_nonT2D[i], "sex", "age", "WHR", "BMI", "eGFR")])
  
  eGFR_summary_nonT2D <- tidy(eGFR_model_nonT2D)
  
  eGFR_result_nonT2D[i, "P_value"] <- eGFR_summary_nonT2D[nrow(eGFR_summary_nonT2D), "p.value"]
  eGFR_result_nonT2D[i, "beta"] <- eGFR_summary_nonT2D[nrow(eGFR_summary_nonT2D), "estimate"]
}
write.csv(eGFR_result_nonT2D, "eGFR_result_nonT2D.csv", row.names = FALSE)
df <- read.csv("eGFR_result_nonT2D.csv")
write_xlsx(df, "eGFR_result_nonT2D.xlsx")
install.packages("writexl")
library(writexl)
library(writexl)




##################ckd
#CKD afterclean 301346 at all
df_kidneymodel_CKD <- select(df_kidneymodel_all, -eGFR, -Microalbumin)
df_kidneymodel_all <- read.csv("df_kidneymodel_all.csv")
df_kidneymodel_CKD <- read.csv("df_kidneymodel_CKD.csv")
df_kidneymodel_CKD <- na.omit(df_kidneymodel_CKD)
df_SNP <- read.csv("df_SNP.csv")
#egfr model in the all data
library(dplyr)
library(broom)

SNP_cols <- grep("rs", names(df_kidneymodel_CKD), value = TRUE)

CKD_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  CKD_model <- glm(CKD ~ sex + age + WHR + BMI + ., data = df_kidneymodel_CKD[, c(SNP_cols[i], "sex", "age", "WHR", "BMI", "CKD")], family = binomial())
  
  CKD_summary <- tidy(CKD_model)
  
  CKD_result_all[i, "P_value"] <- CKD_summary[nrow(CKD_summary), "p.value"]
  CKD_result_all[i, "beta"] <- CKD_summary[nrow(CKD_summary), "estimate"]
}
# Add OR column to CKD_result_all data frame
CKD_result_all$OR <- exp(CKD_result_all$beta)


write.csv(CKD_result_all, "CKD_result_all.csv", row.names = FALSE)
df_CKD_result_all <- read.csv("CKD_result_all.csv")
write_xlsx(df_CKD_result_all, "CKD_result_all.xlsx")

df_kidneymodel_CKD_DM_age  <- select(df_kidneymodel_1,-DM_Doctor.x, -Insulin_use_1Year.x, -Gestational_diabetes.x,-DM_Doctor.y,-Insulin_use_1Year.y, -Gestational_diabetes.y, -eGFR, -Microalbumin)
df_kidneymodel_CKD_DM_age <- na.omit(df_kidneymodel_CKD_DM_age)

#build data set for t2d

# 按照eid列合并两个数据框
merged_df <- merge(df_T2D_EID, df_kidneymodel_CKD_DM_age, by = "eid",all = TRUE)
df_merged <- subset(merged_df, select = -c(x))
# 将共同的eid数据储存为df_T2D_kidenymodel_eGFR
df_T2D_kidenymodel__CKD <-  merge(df_T2D_EID, df_kidneymodel_CKD_DM_age, by = "eid")

df_T2D_kidenymodel__CKD <- subset(df_T2D_kidenymodel__CKD, select = -c(x))

# 将其他数据储存为df_nonT2D_kidenymodel_eGFR
# 将df_kidneymodel_eGFR文件中的eid列转换为字符型
df_kidneymodel_CKD$eid <- as.character(df_kidneymodel_CKD$eid)

# 删除df_T2D_kidenymodel_eGFR文件中包含的所有行
df_nonT2D_kidenymodel_CKD <- anti_join(df_kidneymodel_CKD, df_T2D_kidenymodel__CKD, by = "eid")

df_nonT2D_kidenymodel_CKD <- na.omit(df_nonT2D_kidenymodel_CKD)


library(tidyr)
library(broom)

SNP_cols_T2D <- grep("rs", names(df_T2D_kidenymodel__CKD), value = TRUE)

CKD_result_T2D <- data.frame(SNP = SNP_cols_T2D, P_value = numeric(length(SNP_cols_T2D)), beta = numeric(length(SNP_cols_T2D)))

for (i in 1:length(SNP_cols_T2D)) {
  CKD_model_T2D <- glm(CKD ~ sex.x + age.x + WHR.x + BMI.x + DM_age.x + ., data = df_T2D_kidenymodel__CKD[, c(SNP_cols_T2D[i], "sex.x", "age.x", "WHR.x", "BMI.x", "DM_age.x", "CKD")], family = binomial())
  
  CKD_summary_T2D <- tidy (CKD_model_T2D)
  
  CKD_result_T2D[i, "P_value"] <- CKD_summary_T2D[nrow(CKD_summary_T2D), "p.value"]
  CKD_result_T2D[i, "beta"] <- CKD_summary_T2D[nrow(CKD_summary_T2D), "estimate"]
}
CKD_result_all$OR <- exp(CKD_result_all$beta)
write.csv(CKD_result_T2D, "CKD_result_T2D.csv", row.names = FALSE)


df <- read.csv("CKD_result_T2D.csv")
write_xlsx(df, "CKD_result_T2D.xlsx")
#######NON
SNP_cols_nonT2D <- grep("rs", names(df_nonT2D_kidenymodel_CKD), value = TRUE)

CKD_result_nonT2D <- data.frame(SNP = SNP_cols_nonT2D, P_value = numeric(length(SNP_cols_nonT2D)), beta = numeric(length(SNP_cols_nonT2D)))

for (i in 1:length(SNP_cols_nonT2D)) {
  CKD_model_nonT2D <- glm(CKD ~ sex + age + WHR + BMI + ., data = df_nonT2D_kidenymodel_CKD[, c(SNP_cols_nonT2D[i], "sex", "age", "WHR", "BMI", "CKD")], family = binomial())
  
  CKD_summary_nonT2D <- tidy(CKD_model_nonT2D)
  
  CKD_result_nonT2D[i, "P_value"] <- CKD_summary_nonT2D[nrow(CKD_summary_nonT2D), "p.value"]
  CKD_result_nonT2D[i, "beta"] <- CKD_summary_nonT2D[nrow(CKD_summary_nonT2D), "estimate"]
}
CKD_result_nonT2D$OR <- exp(CKD_result_nonT2D$beta)
write.csv(CKD_result_nonT2D, "CKD_result_nonT2D.csv", row.names = FALSE)
df <- read.csv("CKD_result_nonT2D.csv")
write_xlsx(df, "CKD_result_nonT2D.xlsx")





###########Microalbumin
df_kidneymodel_Microalbumin <- select(df_kidneymodel_all, -eGFR, -CKD)
df_kidneymodel_Microalbumin <- na.omit(df_kidneymodel_Microalbumin)

# model in the all data
library(dplyr)
library(broom)

SNP_cols <- grep("rs", names(df_kidneymodel_Microalbumin), value = TRUE)

Microalbumin_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Microalbumin_model <- lm(Microalbumin ~ sex + age + WHR + BMI + ., data = df_kidneymodel_Microalbumin[, c(SNP_cols[i], "sex", "age", "WHR", "BMI", "Microalbumin")])
  
  Microalbumin_summary <- tidy(Microalbumin_model)
  
  Microalbumin_result_all[i, "P_value"] <- Microalbumin_summary[nrow(Microalbumin_summary), "p.value"]
  Microalbumin_result_all[i, "beta"] <- Microalbumin_summary[nrow(Microalbumin_summary), "estimate"]
}
write.csv(Microalbumin_result_all, "Microalbumin_result_all.csv", row.names = FALSE)
df <- read.csv("Microalbumin_result_all.csv")
write_xlsx(df, "Microalbumin_result_all.xlsx")

df_kidneymodel_Microalbumin_DM_age  <- select(df_kidneymodel_1,-DM_Doctor.x, -Insulin_use_1Year.x, -Gestational_diabetes.x,-DM_Doctor.y,-Insulin_use_1Year.y, -Gestational_diabetes.y, -eGFR, -CKD)
df_kidneymodel_Microalbumin_DM_age <- na.omit(df_kidneymodel_Microalbumin_DM_age)

#build data set for t2d

# 按照eid列合并两个数据框
merged_df <- merge(df_T2D_EID, df_kidneymodel_Microalbumin_DM_age, by = "eid",all = TRUE)
df_merged <- subset(merged_df, select = -c(x))
# 将共同的eid数据储存为df_T2D_kidenymodel_eGFR
df_T2D_kidenymodel__Microalbumin <-  merge(df_T2D_EID, df_kidneymodel_Microalbumin_DM_age, by = "eid")

df_T2D_kidenymodel__Microalbumin <- subset(df_T2D_kidenymodel__Microalbumin, select = -c(x))

# 将其他数据储存为df_nonT2D_kidenymodel_eGFR
# 将df_kidneymodel_eGFR文件中的eid列转换为字符型
df_kidneymodel_Microalbumin$eid <- as.character(df_kidneymodel_Microalbumin$eid)

# 删除df_T2D_kidenymodel_eGFR文件中包含的所有行
df_nonT2D_kidenymodel_Microalbumin <- anti_join(df_kidneymodel_Microalbumin, df_T2D_kidenymodel__Microalbumin, by = "eid")

df_nonT2D_kidenymodel_Microalbumin <- na.omit(df_nonT2D_kidenymodel_Microalbumin)


library(tidyr)
library(broom)

SNP_cols_T2D <- grep("rs", names(df_T2D_kidenymodel__Microalbumin), value = TRUE)

Microalbumin_result_T2D <- data.frame(SNP = SNP_cols_T2D, P_value = numeric(length(SNP_cols_T2D)), beta = numeric(length(SNP_cols_T2D)))

for (i in 1:length(SNP_cols_T2D)) {
  Microalbumin_model_T2D <- lm(Microalbumin ~ sex.x + age.x + WHR.x + BMI.x + DM_age.x + ., data = df_T2D_kidenymodel__Microalbumin[, c(SNP_cols_T2D[i], "sex.x", "age.x", "WHR.x", "BMI.x", "DM_age.x", "Microalbumin")])
  
  Microalbumin_summary_T2D <- tidy (Microalbumin_model_T2D)
  
  Microalbumin_result_T2D[i, "P_value"] <- Microalbumin_summary_T2D[nrow(Microalbumin_summary_T2D), "p.value"]
  Microalbumin_result_T2D[i, "beta"] <- Microalbumin_summary_T2D[nrow(Microalbumin_summary_T2D), "estimate"]
}
write.csv(Microalbumin_result_T2D, "Microalbumin_result_T2D.csv", row.names = FALSE)


df <- read.csv("Microalbumin_result_T2D.csv")
write_xlsx(df, "Microalbumin_result_T2D.xlsx")

#######NON
SNP_cols_nonT2D <- grep("rs", names(df_nonT2D_kidenymodel_Microalbumin), value = TRUE)

Microalbumin_result_nonT2D <- data.frame(SNP = SNP_cols_nonT2D, P_value = numeric(length(SNP_cols_nonT2D)), beta = numeric(length(SNP_cols_nonT2D)))

for (i in 1:length(SNP_cols_nonT2D)) {
  Microalbumin_model_nonT2D <- lm(Microalbumin ~ sex + age + WHR + BMI + ., data = df_nonT2D_kidenymodel_Microalbumin[, c(SNP_cols_nonT2D[i], "sex", "age", "WHR", "BMI", "Microalbumin")])
  
  Microalbumin_summary_nonT2D <- tidy(Microalbumin_model_nonT2D)
  
  Microalbumin_result_nonT2D[i, "P_value"] <- Microalbumin_summary_nonT2D[nrow(Microalbumin_summary_nonT2D), "p.value"]
  Microalbumin_result_nonT2D[i, "beta"] <- Microalbumin_summary_nonT2D[nrow(Microalbumin_summary_nonT2D), "estimate"]
}
write.csv(Microalbumin_result_nonT2D, "Microalbumin_result_nonT2D.csv", row.names = FALSE)
df <- read.csv("Microalbumin_result_nonT2D.csv")
write_xlsx(df, "Microalbumin_result_nonT2D.xlsx")