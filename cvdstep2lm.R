library(broom)
library(writexl)
library(dplyr)

df_heartmodel_all <- read.csv("df_heartmodel_all.csv")
df_heartmodel_CVD_1 <- select(df_heartmodel_all,-Creactive_protein30710,-HBA1C30750,-HDL30760,-LDL30780,-Triglycerides30870,-Age_Stroke4056,-Age_Heart_attack3894,-Age_high_blood_pressure2966)

#LM
######delete -3
df_heartmodel_CVD_1 <- df_heartmodel_CVD_1 %>% filter(CVD6150 != -3)
df_heartmodel_CVD_1 <- df_heartmodel_CVD_1 %>% filter(CVD6150 != -7)
df_heartmodel_CVD_ALL <-select(df_heartmodel_CVD_1, -DM_age)
df_heartmodel_CVD_ALL <- na.omit(df_heartmodel_CVD_ALL)
write.csv(df_heartmodel_CVD_ALL, "df_heartmodel_CVD_ALL.csv", row.names = FALSE)

#alldata forCVD 
SNP_cols <- grep("rs", names(df_heartmodel_CVD_ALL), value = TRUE)

heartmodel_CVD_lM_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  heartmodel_CVD_lM_model <- lm(CVD6150 ~ sex + age + WHR + BMI +., data = df_heartmodel_CVD_ALL[, c(SNP_cols[i], "sex", "age","WHR","BMI", "CVD6150")])
  
  heartmodel_CVD_lM_summary <- tidy(heartmodel_CVD_lM_model)
  
  heartmodel_CVD_lM_result_all[i, "P_value"] <- heartmodel_CVD_lM_summary[nrow(heartmodel_CVD_lM_summary), "p.value"]
  heartmodel_CVD_lM_result_all[i, "beta"] <- heartmodel_CVD_lM_summary[nrow(heartmodel_CVD_lM_summary), "estimate"]
}
P_values <- heartmodel_CVD_lM_result_all$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
heartmodel_CVD_lM_result_all$adjusted_p_value <- adjusted_P_values
write.csv(heartmodel_CVD_lM_result_all, "heartmodel_CVD_lM_result_all.csv", row.names = FALSE)
df_heartmodel_CVD_lM_result_all <- read.csv("heartmodel_CVD_lM_result_all.csv")
write_xlsx(df_heartmodel_CVD_lM_result_all, "heartmodel_CVD_lM_result_all.xlsx")


##########t2D AND NON t2D####

df_heartmodel_CVD_DM <- df_heartmodel_CVD_1

df_heartmodel_CVD_DM <-  merge(df_T2D_EID, df_heartmodel_CVD_DM, by = "eid")
df_heartmodel_CVD_DM <- na.omit(df_heartmodel_CVD_DM)

df_heartmodel_CVD_DM$eid <- as.character(df_heartmodel_CVD_DM$eid)
df_heartmodel_CVD_1$eid <- as.character(df_heartmodel_CVD_1$eid)
df_heartmodel_CVD_NON_DM <- anti_join(df_heartmodel_CVD_1, df_heartmodel_CVD_DM, by = "eid")
df_heartmodel_CVD_NON_DM <-select(df_heartmodel_CVD_NON_DM, -DM_age)
df_heartmodel_CVD_NON_DM <- na.omit(df_heartmodel_CVD_NON_DM)



write.csv(df_heartmodel_CVD_NON_DM, "df_heartmodel_CVD_NON_DM.csv", row.names = FALSE)
write.csv(df_heartmodel_CVD_DM, "df_heartmodel_CVD_DM", row.names = FALSE)

#####for t2d

SNP_cols <- grep("rs", names(df_heartmodel_CVD_DM), value = TRUE)

heartmodel_CVD_DM_lM_result <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  heartmodel_CVD_DM_lM_model <- lm(CVD6150 ~ sex + age  + DM_age + WHR + BMI +., data = df_heartmodel_CVD_DM[, c(SNP_cols[i], "sex", "age", "DM_age","WHR","BMI", "CVD6150")])
  
  heartmodel_CVD_DM_lM_summary <- tidy(heartmodel_CVD_DM_lM_model)
  
  heartmodel_CVD_DM_lM_result[i, "P_value"] <- heartmodel_CVD_DM_lM_summary[nrow(heartmodel_CVD_DM_lM_summary), "p.value"]
  heartmodel_CVD_DM_lM_result[i, "beta"] <- heartmodel_CVD_DM_lM_summary[nrow(heartmodel_CVD_DM_lM_summary), "estimate"]
}
P_values <- heartmodel_CVD_DM_lM_result$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
heartmodel_CVD_DM_lM_result$adjusted_p_value <- adjusted_P_values
write.csv(heartmodel_CVD_DM_lM_result, "heartmodel_CVD_DM_lM_result.csv", row.names = FALSE)
df_heartmodel_CVD_DM_lM_result <- read.csv("heartmodel_CVD_DM_lM_result.csv")
write_xlsx(df_heartmodel_CVD_DM_lM_result, "heartmodel_CVD_DM_lM_result.xlsx")

#######dor non t2d
SNP_cols <- grep("rs", names(df_heartmodel_CVD_NON_DM), value = TRUE)

heartmodel_CVD_NON_DM_lM_result <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  heartmodel_CVD_NON_DM_lM_model <- lm(CVD6150 ~ sex + age + WHR + BMI +., data = df_heartmodel_CVD_NON_DM[, c(SNP_cols[i], "sex", "age","WHR","BMI", "CVD6150")])
  
  heartmodel_CVD_NON_DM_lM_summary <- tidy(heartmodel_CVD_NON_DM_lM_model)
  
  heartmodel_CVD_NON_DM_lM_result[i, "P_value"] <- heartmodel_CVD_NON_DM_lM_summary[nrow(heartmodel_CVD_NON_DM_lM_summary), "p.value"]
  heartmodel_CVD_NON_DM_lM_result[i, "beta"] <- heartmodel_CVD_NON_DM_lM_summary[nrow(heartmodel_CVD_NON_DM_lM_summary), "estimate"]
}
P_values <- heartmodel_CVD_NON_DM_lM_result$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
heartmodel_CVD_NON_DM_lM_result$adjusted_p_value <- adjusted_P_values
write.csv(heartmodel_CVD_NON_DM_lM_result, "heartmodel_CVD_NON_DM_lM_result.csv", row.names = FALSE)
df_heartmodel_CVD_NON_DM_lM_result <- read.csv("heartmodel_CVD_NON_DM_lM_result.csv")
write_xlsx(df_heartmodel_CVD_NON_DM_lM_result, "heartmodel_CVD_NON_DM_lM_result.xlsx")





