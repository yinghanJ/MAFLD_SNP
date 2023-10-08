
########softmax model#####
library(nnet)
# 将Depression20126转换为分类变量
df_heartmodel_CVD_ALL <- df_heartmodel_CVD_ALL %>% 
  mutate(CVD_cat = case_when(
    CVD6150 == 1 ~ "Heart_attack",
    CVD6150 == 2 ~ "Angina",
    CVD6150 == 3 ~ "Stroke",
    CVD6150 == 4 ~ "High_blood_pressure"
  ))

SNP_cols <- grep("rs", names(df_heartmodel_CVD_ALL), value = TRUE)

heartmodel_CVD_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  heartmodel_CVD_model <- multinom(CVD_cat ~ sex + age + ., data = df_heartmodel_CVD_ALL[, c(SNP_cols[i], "sex", "age", "CVD_cat")])
  
  heartmodel_CVD_summary <- tidy(heartmodel_CVD_model)
  
  heartmodel_CVD_result_all[i, "P_value"] <- heartmodel_CVD_summary[nrow(heartmodel_CVD_summary), "p.value"]
  heartmodel_CVD_result_all[i, "beta"] <- heartmodel_CVD_summary[nrow(heartmodel_CVD_summary), "estimate"]
}
P_values <- heartmodel_CVD_result_all$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
heartmodel_CVD_result_all$adjusted_p_value <- adjusted_P_values
write.csv(heartmodel_CVD_result_all, "heartmodel_CVD_result_all.csv", row.names = FALSE)
df_heartmodel_CVD_result_all <- read.csv("heartmodel_CVD_result_all.csv")
write_xlsx(df_heartmodel_CVD_result_all, "heartmodel_CVD_result_all.xlsx")

###################T2d and non##########

#####for t2d
SNP_cols <- grep("rs", names(df_heartmodel_CVD_DM), value = TRUE)

heartmodel_CVD_result_T2D <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))
df_heartmodel_CVD_DM <- df_heartmodel_CVD_DM %>% 
  mutate(CVD_cat = case_when(
    CVD6150 == 1 ~ "Heart_attack",
    CVD6150 == 2 ~ "Angina",
    CVD6150 == 3 ~ "Stroke",
    CVD6150 == 4 ~ "High_blood_pressure"
  ))

for (i in 1:length(SNP_cols)) {
  heartmodel_CVD_model_T2D <- multinom(CVD_cat ~ sex + age + DM_age + ., data = df_heartmodel_CVD_DM[, c(SNP_cols[i], "sex", "age", "DM_age", "CVD_cat")])
  heartmodel_CVD_summary_T2D <- tidy (heartmodel_CVD_model_T2D)
  
  heartmodel_CVD_result_T2D[i, "P_value"] <- heartmodel_CVD_summary_T2D[nrow(heartmodel_CVD_summary_T2D), "p.value"]
  heartmodel_CVD_result_T2D[i, "beta"] <- heartmodel_CVD_summary_T2D[nrow(heartmodel_CVD_summary_T2D), "estimate"]
}
P_values <- heartmodel_CVD_result_T2D$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
heartmodel_CVD_result_T2D$adjusted_p_value <- adjusted_P_values
write.csv(heartmodel_CVD_result_T2D, "heartmodel_CVD_result_T2D_softmax.csv", row.names = FALSE)
df_heartmodel_CVD_result_T2D <- read.csv("heartmodel_CVD_result_T2D_softmax.csv")
write_xlsx(df_heartmodel_CVD_result_T2D, "heartmodel_CVD_result_T2D_softmax.xlsx")

#######dor non t2d
SNP_cols <- grep("rs", names(df_heartmodel_CVD_NON_DM), value = TRUE)

heartmodel_CVD_result_nonT2D <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))
df_heartmodel_CVD_NON_DM <- df_heartmodel_CVD_NON_DM %>% 
  mutate(CVD_cat = case_when(
    CVD6150 == 1 ~ "Heart_attack",
    CVD6150 == 2 ~ "Angina",
    CVD6150 == 3 ~ "Stroke",
    CVD6150 == 4 ~ "High_blood_pressure"
  ))
for (i in 1:length(SNP_cols)) {
  heartmodel_CVD_model_nonT2D <- multinom(CVD_cat ~ sex + age + ., data = df_heartmodel_CVD_NON_DM[, c(SNP_cols[i], "sex", "age", "CVD_cat")])
  
  heartmodel_CVD_summary_nonT2D <- tidy (heartmodel_CVD_model_nonT2D)
  
  heartmodel_CVD_result_nonT2D[i, "P_value"] <- heartmodel_CVD_summary_nonT2D[nrow(heartmodel_CVD_summary_nonT2D), "p.value"]
  heartmodel_CVD_result_nonT2D[i, "beta"] <- heartmodel_CVD_summary_nonT2D[nrow(heartmodel_CVD_summary_nonT2D), "estimate"]
}
P_values <- heartmodel_CVD_result_nonT2D$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
heartmodel_CVD_result_nonT2D$adjusted_p_value <- adjusted_P_values
write.csv(heartmodel_CVD_result_nonT2D, "heartmodel_CVD_result_nonT2D_softmax.csv", row.names = FALSE)
df_heartmodel_CVD_result_nonT2D <- read.csv("heartmodel_CVD_result_nonT2D_softmax.csv")
write_xlsx(df_heartmodel_CVD_result_nonT2D, "heartmodel_CVD_result_nonT2D_softmax.xlsx")