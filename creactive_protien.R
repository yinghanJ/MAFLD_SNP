df_heartmodel_all <- read.csv("df_heartmodel_all.csv")
df_Creactive_protein30710 <- select(df_heartmodel_all,-CVD6150,-HBA1C30750,-HDL30760,-LDL30780,-Triglycerides30870,-Age_Stroke4056,-Age_Heart_attack3894,-Age_high_blood_pressure2966)

#LM
######delete 
df_Creactive_protein30710_ALL <-select(df_Creactive_protein30710, -DM_age)
df_Creactive_protein30710_ALL <- na.omit(df_Creactive_protein30710_ALL)
write.csv(df_Creactive_protein30710_ALL, "df_Creactive_protein30710_ALL.csv", row.names = FALSE)

#alldata
SNP_cols <- grep("rs", names(df_Creactive_protein30710_ALL), value = TRUE)

Creactive_protein30710_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Creactive_protein30710_model <- lm(Creactive_protein30710 ~ sex + age + WHR + BMI +., data = df_Creactive_protein30710_ALL[, c(SNP_cols[i], "sex", "age","WHR","BMI", "Creactive_protein30710")])
  
  Creactive_protein30710_summary <- tidy(Creactive_protein30710_model)
  
  Creactive_protein30710_result_all[i, "P_value"] <- Creactive_protein30710_summary[nrow(Creactive_protein30710_summary), "p.value"]
  Creactive_protein30710_result_all[i, "beta"] <- Creactive_protein30710_summary[nrow(Creactive_protein30710_summary), "estimate"]
}
P_values <- Creactive_protein30710_result_all$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
Creactive_protein30710_result_all$adjusted_p_value <- adjusted_P_values
write.csv(Creactive_protein30710_result_all, "Creactive_protein30710_result_all.csv", row.names = FALSE)
df_Creactive_protein30710_result_all <- read.csv("Creactive_protein30710_result_all.csv")
write_xlsx(df_Creactive_protein30710_result_all, "Creactive_protein30710_result_all.xlsx")


##########t2D AND NON t2D####

df_Creactive_protein30710_DM <- df_Creactive_protein30710

df_Creactive_protein30710_DM <-  merge(df_T2D_EID, df_Creactive_protein30710_DM, by = "eid")
df_Creactive_protein30710_DM <- na.omit(df_Creactive_protein30710_DM)

df_Creactive_protein30710_DM$eid <- as.character(df_Creactive_protein30710_DM$eid)
df_Creactive_protein30710$eid <- as.character(df_Creactive_protein30710$eid)

df_Creactive_protein30710_NON_DM <- anti_join(df_Creactive_protein30710, df_Creactive_protein30710_DM, by = "eid")
df_Creactive_protein30710_NON_DM <-select(df_Creactive_protein30710_NON_DM, -DM_age)
df_Creactive_protein30710_NON_DM <- na.omit(df_Creactive_protein30710_NON_DM)



write.csv(df_Creactive_protein30710_NON_DM, "df_Creactive_protein30710_NON_DM.csv", row.names = FALSE)
write.csv(df_Creactive_protein30710_DM, "df_Creactive_protein30710_DM.csv", row.names = FALSE)

#####for t2d

SNP_cols <- grep("rs", names(df_Creactive_protein30710_DM), value = TRUE)

Creactive_protein30710_result_DM <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Creactive_protein30710_DM_model <- lm(Creactive_protein30710 ~ sex + age  + DM_age + WHR + BMI +., data = df_Creactive_protein30710_DM[, c(SNP_cols[i], "sex", "age", "DM_age","WHR","BMI", "Creactive_protein30710")])
  
  Creactive_protein30710_DM_summary <- tidy(Creactive_protein30710_DM_model)
  
  Creactive_protein30710_result_DM[i, "P_value"] <- Creactive_protein30710_DM_summary[nrow(Creactive_protein30710_DM_summary), "p.value"]
  Creactive_protein30710_result_DM[i, "beta"] <- Creactive_protein30710_DM_summary[nrow(Creactive_protein30710_DM_summary), "estimate"]
}
P_values <- Creactive_protein30710_result_DM$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
Creactive_protein30710_result_DM$adjusted_p_value <- adjusted_P_values
write.csv(Creactive_protein30710_result_DM, "Creactive_protein30710_result_DM.csv", row.names = FALSE)
Creactive_protein30710_result_DM <- read.csv("Creactive_protein30710_result_DM.csv")
write_xlsx(Creactive_protein30710_result_DM, "Creactive_protein30710_result_DM.xlsx")

#######dor non t2d
SNP_cols <- grep("rs", names(df_Creactive_protein30710_NON_DM), value = TRUE)

Creactive_protein30710_NON_DM_result <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Creactive_protein30710_NON_DM_model <- lm(Creactive_protein30710 ~ sex + age + WHR + BMI +., data = df_Creactive_protein30710_NON_DM[, c(SNP_cols[i], "sex", "age","WHR","BMI", "Creactive_protein30710")])
  
  Creactive_protein30710_NON_DM_summary <- tidy(Creactive_protein30710_NON_DM_model)
  
  Creactive_protein30710_NON_DM_result[i, "P_value"] <- Creactive_protein30710_NON_DM_summary[nrow(Creactive_protein30710_NON_DM_summary), "p.value"]
  Creactive_protein30710_NON_DM_result[i, "beta"] <- Creactive_protein30710_NON_DM_summary[nrow(Creactive_protein30710_NON_DM_summary), "estimate"]
}
P_values <- Creactive_protein30710_NON_DM_result$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
Creactive_protein30710_NON_DM_result$adjusted_p_value <- adjusted_P_values
write.csv(Creactive_protein30710_NON_DM_result, "Creactive_protein30710_NON_DM_result.csv", row.names = FALSE)
Creactive_protein30710_NON_DM_result <- read.csv("Creactive_protein30710_NON_DM_result.csv")
write_xlsx(Creactive_protein30710_NON_DM_result, "Creactive_protein30710_NON_DM_result.xlsx")