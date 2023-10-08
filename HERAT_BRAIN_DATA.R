###HERAT DATA #####
df_SNP <- read.csv("df_SNP.csv")
df <- read.csv("all_data.csv")
df_heart_main <- df[, c("eid","X6150.0.0", "X3894.0.0", "X4056.0.0","X2966.0.0","X30710.0.0","X30750.0.0","X30760.0.0","X30780.0.0","X30870.0.0")]
colnames(df_heart_main)[2] <- "CVD6150"
colnames(df_heart_main)[3] <- "Age_Stroke4056"
colnames(df_heart_main)[4] <- "Age_Heart_attack3894"
colnames(df_heart_main)[5] <- "Age_high_blood_pressure2966"
colnames(df_heart_main)[6] <- "C-reactive_protein30710"
colnames(df_heart_main)[7] <- "HBA1C30750"
colnames(df_heart_main)[8] <- "HDL30760"
colnames(df_heart_main)[9] <- "LDL30780"
colnames(df_heart_main)[10] <- "Triglycerides30870"

#####heartMODEL
##combine it with common data set
df_heartmodel_1 <- merge(df_heart_main, df_SNP, by = "eid")
df_heartmodel_all <- select(df_heartmodel_1,-DM_age, -DM_Doctor, -Insulin_use_1Year, -Gestational_diabetes)
write.csv(df_heartmodel_all, file = "df_heartmodel_all.csv")

######brain#########
df_brain_main <- df[, c("eid", "X4620.0.0", "X20126.0.0","X20127.0.0")]
colnames(df_brain_main)[2] <- "Depression_Episodes4620"
colnames(df_brain_main)[3] <- "Depression20126"
colnames(df_brain_main)[4] <- "NeurotisismX20127"


#####BRIANMODEL
##combine it with common data set
df_brainmodel_1 <- merge(df_brain_main, df_SNP, by = "eid")
df_brainmodel_all <- select(df_brainmodel_1,-DM_age, -DM_Doctor, -Insulin_use_1Year, -Gestational_diabetes, -BMI, -WHR)
write.csv(df_brainmodel_all, file = "df_brainmodel_all.csv")

#####删除4620Depression_Episodes次数小于1和大于9999的
df_brainmodel_Depression_Episodes4620_1 <- select(df_brainmodel_all,-Depression20126,-NeurotisismX20127)
df_brainmodel_Depression_Episodes4620_2 <- na.omit(df_brainmodel_Depression_Episodes4620_1)
df_brainmodel_Depression_Episodes4620_all <- df_brainmodel_Depression_Episodes4620_2[!(df_brainmodel_Depression_Episodes4620_2$Depression_Episodes4620 > 9999 | df_brainmodel_Depression_Episodes4620_2$Depression_Episodes4620 < 1), ]

library(broom)
library(writexl)
library(dplyr)
#####MODELFOR#####4620Depression_Episodes

SNP_cols <- grep("rs", names(df_brainmodel_Depression_Episodes4620_all), value = TRUE)

Depression_Episodes4620_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Depression_Episodes4620_model <- lm(Depression_Episodes4620 ~ sex + age + ., data = df_brainmodel_Depression_Episodes4620_all[, c(SNP_cols[i], "sex", "age", "Depression_Episodes4620")])
  
  Depression_Episodes4620_summary <- tidy(Depression_Episodes4620_model)
  
  Depression_Episodes4620_result_all[i, "P_value"] <- Depression_Episodes4620_summary[nrow(Depression_Episodes4620_summary), "p.value"]
  Depression_Episodes4620_result_all[i, "beta"] <- Depression_Episodes4620_summary[nrow(Depression_Episodes4620_summary), "estimate"]
}
write.csv(Depression_Episodes4620_result_all, "Depression_Episodes4620_result_all.csv", row.names = FALSE)
df_Depression_Episodes4620_result_all <- read.csv("Depression_Episodes4620_result_all.csv")
write_xlsx(df_Depression_Episodes4620_result_all, "Depression_Episodes4620_result_all.xlsx")

#########4620Depression_Episodes t2d and non t2d data
df_brainmodel_DM_age <- select(df_brainmodel_1, -DM_Doctor, -Insulin_use_1Year, -Gestational_diabetes, -BMI, -WHR)

# 按照eid列合并两个数据框
merged_df <- merge(df_T2D_EID, df_brainmodel_DM_age, by = "eid",all = TRUE)
df_merged <- subset(merged_df, select = -c(x))

df_T2D_brainmodel <-  merge(df_T2D_EID, df_brainmodel_DM_age, by = "eid")

df_T2D_brainmodel <- subset(df_T2D_brainmodel, select = -c(x))


df_brainmodel_all$eid <- as.character(df_brainmodel_all$eid)


df_nonT2D_brainmode_all <- anti_join(df_brainmodel_all, df_T2D_brainmodel, by = "eid")

#####删除4620Depression_Episodes次数小于1和大于9999的
df_T2D_brainmodel_Depression_Episodes4620<- select(df_T2D_brainmodel,-Depression20126,-NeurotisismX20127)

df_T2D_brainmodel_Depression_Episodes4620 <- df_T2D_brainmodel_Depression_Episodes4620[!(df_T2D_brainmodel_Depression_Episodes4620$Depression_Episodes4620 > 9999 | df_T2D_brainmodel_Depression_Episodes4620$Depression_Episodes4620 < 1), ]
df_T2D_brainmodel_Depression_Episodes4620 <- na.omit(df_T2D_brainmodel_Depression_Episodes4620)


df_nonT2D_brainmodel_Depression_Episodes4620<- select(df_nonT2D_brainmode_all,-Depression20126,-NeurotisismX20127)

df_nonT2D_brainmodel_Depression_Episodes4620 <- df_nonT2D_brainmodel_Depression_Episodes4620[!(df_nonT2D_brainmodel_Depression_Episodes4620$Depression_Episodes4620 > 9999 | df_nonT2D_brainmodel_Depression_Episodes4620$Depression_Episodes4620 < 1), ]
df_nonT2D_brainmodel_Depression_Episodes4620 <- na.omit(df_nonT2D_brainmodel_Depression_Episodes4620)

library(tidyr)
library(broom)

#####for t2d
SNP_cols <- grep("rs", names(df_T2D_brainmodel_Depression_Episodes4620), value = TRUE)

Depression_Episodes4620_result_T2D <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Depression_Episodes4620_model_T2D <- lm(Depression_Episodes4620 ~ sex + age + DM_age + ., data = df_T2D_brainmodel_Depression_Episodes4620[, c(SNP_cols[i], "sex", "age", "DM_age", "Depression_Episodes4620")])
  
  Depression_Episodes4620_summary_T2D <- tidy (Depression_Episodes4620_model_T2D)
  
  Depression_Episodes4620_result_T2D[i, "P_value"] <- Depression_Episodes4620_summary_T2D[nrow(Depression_Episodes4620_summary_T2D), "p.value"]
  Depression_Episodes4620_result_T2D[i, "beta"] <- Depression_Episodes4620_summary_T2D[nrow(Depression_Episodes4620_summary_T2D), "estimate"]
}
write.csv(Depression_Episodes4620_result_T2D, "Depression_Episodes4620_result_T2D.csv", row.names = FALSE)
df_Depression_Episodes4620_result_T2D <- read.csv("Depression_Episodes4620_result_T2D.csv")
write_xlsx(df_Depression_Episodes4620_result_T2D, "Depression_Episodes4620_result_T2D.xlsx")

#######dor non t2d
SNP_cols <- grep("rs", names(df_nonT2D_brainmodel_Depression_Episodes4620), value = TRUE)

Depression_Episodes4620_result_nonT2D <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Depression_Episodes4620_model_nonT2D <- lm(Depression_Episodes4620 ~ sex + age + ., data = df_nonT2D_brainmodel_Depression_Episodes4620[, c(SNP_cols[i], "sex", "age", "Depression_Episodes4620")])
  
  Depression_Episodes4620_summary_nonT2D <- tidy (Depression_Episodes4620_model_nonT2D)
  
  Depression_Episodes4620_result_nonT2D[i, "P_value"] <- Depression_Episodes4620_summary_nonT2D[nrow(Depression_Episodes4620_summary_nonT2D), "p.value"]
  Depression_Episodes4620_result_nonT2D[i, "beta"] <- Depression_Episodes4620_summary_nonT2D[nrow(Depression_Episodes4620_summary_nonT2D), "estimate"]
}
write.csv(Depression_Episodes4620_result_nonT2D, "Depression_Episodes4620_result_nonT2D.csv", row.names = FALSE)
df_Depression_Episodes4620_result_nonT2D <- read.csv("Depression_Episodes4620_result_nonT2D.csv")
write_xlsx(df_Depression_Episodes4620_result_nonT2D, "Depression_Episodes4620_result_nonT2D.xlsx")

##########NeurotisismX20127####
df_brainmodel_NeurotisismX20127_1 <- select(df_brainmodel_all,-Depression20126,-Depression_Episodes4620)
df_brainmodel_NeurotisismX20127_all <- na.omit(df_brainmodel_NeurotisismX20127_1)

#####for all
SNP_cols <- grep("rs", names(df_brainmodel_NeurotisismX20127_all), value = TRUE)

NeurotisismX20127_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  NeurotisismX20127_model <- lm(NeurotisismX20127 ~ sex + age + ., data = df_brainmodel_NeurotisismX20127_all[, c(SNP_cols[i], "sex", "age", "NeurotisismX20127")])
  
  NeurotisismX20127_summary <- tidy(NeurotisismX20127_model)
  
  NeurotisismX20127_result_all[i, "P_value"] <- NeurotisismX20127_summary[nrow(NeurotisismX20127_summary), "p.value"]
  NeurotisismX20127_result_all[i, "beta"] <- NeurotisismX20127_summary[nrow(NeurotisismX20127_summary), "estimate"]
}
write.csv(NeurotisismX20127_result_all, "NeurotisismX20127_result_all.csv", row.names = FALSE)
df_NeurotisismX20127_result_all <- read.csv("NeurotisismX20127_result_all.csv")
write_xlsx(df_NeurotisismX20127_result_all, "NeurotisismX20127_result_all.xlsx")

##########NeurotisismX20127 t2D AND NON t2D####

df_brainmodel_DM_age <- select(df_brainmodel_1, -DM_Doctor, -Insulin_use_1Year, -Gestational_diabetes, -BMI, -WHR)

# 按照eid列合并两个数据框
merged_df <- merge(df_T2D_EID, df_brainmodel_DM_age, by = "eid",all = TRUE)
df_merged <- subset(merged_df, select = -c(x))

df_T2D_brainmodel <-  merge(df_T2D_EID, df_brainmodel_DM_age, by = "eid")

df_T2D_brainmodel <- subset(df_T2D_brainmodel, select = -c(x))


df_brainmodel_all$eid <- as.character(df_brainmodel_all$eid)


df_nonT2D_brainmode_all <- anti_join(df_brainmodel_all, df_T2D_brainmodel, by = "eid")
#################

df_T2D_brainmodel_NeurotisismX20127<- select(df_T2D_brainmodel,-Depression20126,-Depression_Episodes4620)
df_T2D_brainmodel_NeurotisismX20127 <- na.omit(df_T2D_brainmodel_NeurotisismX20127)


df_nonT2D_brainmodel_NeurotisismX20127<- select(df_nonT2D_brainmode_all,-Depression20126,-Depression_Episodes4620)
df_nonT2D_brainmodel_NeurotisismX20127 <- na.omit(df_nonT2D_brainmodel_NeurotisismX20127)

#####for t2d
SNP_cols <- grep("rs", names(df_T2D_brainmodel_NeurotisismX20127), value = TRUE)

NeurotisismX20127_result_T2D <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  NeurotisismX20127_model_T2D <- lm(NeurotisismX20127 ~ sex + age + DM_age + ., data = df_T2D_brainmodel_NeurotisismX20127[, c(SNP_cols[i], "sex", "age", "DM_age", "NeurotisismX20127")])
  
  NeurotisismX20127_summary_T2D <- tidy (NeurotisismX20127_model_T2D)
  
  NeurotisismX20127_result_T2D[i, "P_value"] <- NeurotisismX20127_summary_T2D[nrow(NeurotisismX20127_summary_T2D), "p.value"]
  NeurotisismX20127_result_T2D[i, "beta"] <- NeurotisismX20127_summary_T2D[nrow(NeurotisismX20127_summary_T2D), "estimate"]
}
write.csv(NeurotisismX20127_result_T2D, "NeurotisismX20127_result_T2D.csv", row.names = FALSE)
df_NeurotisismX20127_result_T2D <- read.csv("NeurotisismX20127_result_T2D.csv")
write_xlsx(df_NeurotisismX20127_result_T2D, "NeurotisismX20127_result_T2D.xlsx")

#######dor non t2d
SNP_cols <- grep("rs", names(df_nonT2D_brainmodel_NeurotisismX20127), value = TRUE)

NeurotisismX20127_result_nonT2D <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  NeurotisismX20127_model_nonT2D <- lm(NeurotisismX20127 ~ sex + age + ., data = df_nonT2D_brainmodel_NeurotisismX20127[, c(SNP_cols[i], "sex", "age", "NeurotisismX20127")])
  
  NeurotisismX20127_summary_nonT2D <- tidy (NeurotisismX20127_model_nonT2D)
  
  NeurotisismX20127_result_nonT2D[i, "P_value"] <- NeurotisismX20127_summary_nonT2D[nrow(NeurotisismX20127_summary_nonT2D), "p.value"]
  NeurotisismX20127_result_nonT2D[i, "beta"] <- NeurotisismX20127_summary_nonT2D[nrow(NeurotisismX20127_summary_nonT2D), "estimate"]
}
write.csv(NeurotisismX20127_result_nonT2D, "NeurotisismX20127_result_nonT2D.csv", row.names = FALSE)
df_NeurotisismX20127_result_nonT2D <- read.csv("NeurotisismX20127_result_nonT2D.csv")
write_xlsx(df_NeurotisismX20127_result_nonT2D, "NeurotisismX20127_result_nonT2D.xlsx")