library(broom)
library(writexl)
library(dplyr)

DM_age <- df_heartmodel_1$DM_age
df_heartmodel_all <- merge(df_heartmodel_all, data.frame(eid = df_heartmodel_1$eid, DM_age), by = "eid", all.x = TRUE)

df_heartmodel_all <- read.csv("df_heartmodel_all.csv")
df_heartmodel_all <- df_heartmodel_all[, !grepl("^X", colnames(df_heartmodel_all))]
colnames(df_heartmodel_all)[6] <- "Creactive_protein30710"
colnames(df_heartmodel_all)[9] <- "LDL30780"
write.csv(df_heartmodel_all, file = "df_heartmodel_all.csv")

df_small_data <- read.csv("data_small.csv")
df <- read.csv("all_data.csv")
df_brainmodel_all <- read.csv("df_brainmodel_all.csv")
##3627	Age angina diagnosed
df_3627 <- df[, c("eid","X3627.0.0")]
colnames(df)[grepl("3627", colnames(df))]

######T2dAN NON T2D
df_T2D_EID <- read.csv("df_T2D_EID.csv")
df_T2D_EID <- select(df_T2D_EID,-x)

df_T2D_heartmodel <-  merge(df_T2D_EID, df_heartmodel_all, by = "eid")


write.csv(df_T2D_heartmodel, file = "df_T2D_heartmodel.csv")
df_heartmodel_all$eid <- as.character(df_heartmodel_all$eid)

df_T2D_heartmodel$eid <- as.character(df_T2D_heartmodel$eid)
df_nonT2D_heartmodel <- anti_join(df_heartmodel_all, df_T2D_heartmodel, by = "eid")
write.csv(df_nonT2D_heartmodel, file = "df_nonT2D_heartmodel.csv")

####CVD data######
df_heartmodel_CVD_1 <- select(df_heartmodel_all,-Creactive_protein30710,-HBA1C30750,-HDL30760,-LDL30780,-Triglycerides30870)
###show data distribution
install.packages("ggplot2")
library(ggplot2)
ggplot(df_heartmodel_CVD_1, aes(x = CVD6150)) +
  geom_histogram()
table(df_heartmodel_CVD_1$CVD6150)
######delete -3
df_heartmodel_CVD_1 <- df_heartmodel_CVD_1 %>% filter(CVD6150 != -3)


#####step 1 yes and no classification
df_heartmodel_CVD_Yes <- df_heartmodel_CVD_1[df_heartmodel_CVD_1$CVD6150 != -7, ]
df_heartmodel_CVD_Yes$CVD6150 <- na.omit(df_heartmodel_CVD_Yes$CVD6150)

df_heartmodel_CVD_NO <- df_heartmodel_CVD_1[df_heartmodel_CVD_1$CVD6150 == -7, ]
df_heartmodel_CVD_NO$CVD6150 <- na.omit(df_heartmodel_CVD_NO$CVD6150)

####change the value to 0,1
df_heartmodel_CVD_Yes$CVD6150 <- 1
df_heartmodel_CVD_NO$CVD6150 <- 0

###COMBINE
df_heartmodel_CVD_Yes_NO_include_age <- rbind(df_heartmodel_CVD_NO, df_heartmodel_CVD_Yes)
write.csv(df_heartmodel_CVD_Yes_NO_include_age, file = "df_heartmodel_CVD_Yes_NO_include_age.csv")

df_heartmodel_CVD_Yes_NO <- select(df_heartmodel_CVD_Yes_NO_include_age,-Age_Stroke4056,-Age_Heart_attack3894,-Age_high_blood_pressure2966)
write.csv(df_heartmodel_CVD_Yes_NO, file = "df_heartmodel_CVD_Yes_NO.csv")

####all data for the CVD Yes and no

df_heartmodel_CVD_Yes_NO_all <- select(df_heartmodel_CVD_Yes_NO,-DM_age)
df_heartmodel_CVD_Yes_NO_all <- na.omit(df_heartmodel_CVD_Yes_NO_all)

write.csv(df_heartmodel_CVD_Yes_NO_all, file = "df_heartmodel_CVD_Yes_NO_all.csv")

####binomial for all data#####
SNP_cols <- grep("rs", names(df_heartmodel_CVD_Yes_NO_all), value = TRUE)

heartmodel_CVD_Yes_NO_all_result <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  heartmodel_CVD_Yes_NO_all_model <- glm(CVD6150 ~ sex + age + WHR + BMI +., data = df_heartmodel_CVD_Yes_NO_all[, c(SNP_cols[i], "sex", "age","WHR","BMI", "CVD6150")], family = binomial(link = "logit"))
  
  heartmodel_CVD_Yes_NO_all_summary <- tidy(heartmodel_CVD_Yes_NO_all_model)
  
  heartmodel_CVD_Yes_NO_all_result[i, "P_value"] <- heartmodel_CVD_Yes_NO_all_summary[nrow(heartmodel_CVD_Yes_NO_all_summary), "p.value"]
  heartmodel_CVD_Yes_NO_all_result[i, "beta"] <- heartmodel_CVD_Yes_NO_all_summary[nrow(heartmodel_CVD_Yes_NO_all_summary), "estimate"]
}
heartmodel_CVD_Yes_NO_all_result$OR <- exp(heartmodel_CVD_Yes_NO_all_result$beta)
P_values <- heartmodel_CVD_Yes_NO_all_result$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
heartmodel_CVD_Yes_NO_all_result$adjusted_p_value <- adjusted_P_values

write.csv(heartmodel_CVD_Yes_NO_all_result, "heartmodel_CVD_Yes_NO_all_result.csv", row.names = FALSE)
heartmodel_CVD_Yes_NO_all_result <- read.csv("heartmodel_CVD_Yes_NO_all_result.csv")
write_xlsx(heartmodel_CVD_Yes_NO_all_result, "heartmodel_CVD_Yes_NO_all_result.xlsx")

#########t2d and non data
df_T2D_heartmodel_CVD_Yes_NO <-  merge(df_T2D_EID, df_heartmodel_CVD_Yes_NO, by = "eid")
df_T2D_heartmodel_CVD_Yes_NO <- na.omit(df_T2D_heartmodel_CVD_Yes_NO)

write.csv(df_T2D_heartmodel_CVD_Yes_NO, file = "df_T2D_heartmodel_CVD_Yes_NO.csv")
df_T2D_heartmodel_CVD_Yes_NO$eid <- as.character(df_T2D_heartmodel_CVD_Yes_NO$eid)

df_heartmodel_CVD_Yes_NO_all$eid <- as.character(df_heartmodel_CVD_Yes_NO_all$eid)

df_nonT2D_heartmodel_CVD_Yes_NO <- anti_join(df_heartmodel_CVD_Yes_NO_all, df_T2D_heartmodel_CVD_Yes_NO, by = "eid")
df_nonT2D_heartmodel_CVD_Yes_NO <- na.omit(df_nonT2D_heartmodel_CVD_Yes_NO)
write.csv(df_nonT2D_heartmodel_CVD_Yes_NO, file = "df_nonT2D_heartmodel_CVD_Yes_NO.csv")


#########t2d model
SNP_cols <- grep("rs", names(df_T2D_heartmodel_CVD_Yes_NO), value = TRUE)

T2D_heartmodel_CVD_Yes_NO_result <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  T2D_heartmodel_CVD_Yes_NO_model <- glm(CVD6150 ~ sex + age + WHR + BMI + DM_age +., data = df_T2D_heartmodel_CVD_Yes_NO[, c(SNP_cols[i], "sex", "age","WHR","BMI","DM_age", "CVD6150")], family = binomial(link = "logit"))
  
  T2D_heartmodel_CVD_Yes_NO_summary <- tidy(T2D_heartmodel_CVD_Yes_NO_model)
  
  T2D_heartmodel_CVD_Yes_NO_result[i, "P_value"] <- T2D_heartmodel_CVD_Yes_NO_summary[nrow(T2D_heartmodel_CVD_Yes_NO_summary), "p.value"]
  T2D_heartmodel_CVD_Yes_NO_result[i, "beta"] <- T2D_heartmodel_CVD_Yes_NO_summary[nrow(T2D_heartmodel_CVD_Yes_NO_summary), "estimate"]
}
T2D_heartmodel_CVD_Yes_NO_result$OR <- exp(T2D_heartmodel_CVD_Yes_NO_result$beta)
P_values <- T2D_heartmodel_CVD_Yes_NO_result$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
T2D_heartmodel_CVD_Yes_NO_result$adjusted_p_value <- adjusted_P_values

write.csv(T2D_heartmodel_CVD_Yes_NO_result, "T2D_heartmodel_CVD_Yes_NO_result.csv", row.names = FALSE)
T2D_heartmodel_CVD_Yes_NO_result <- read.csv("T2D_heartmodel_CVD_Yes_NO_result.csv")
write_xlsx(T2D_heartmodel_CVD_Yes_NO_result, "T2D_heartmodel_CVD_Yes_NO_result.xlsx")



#####non t2d
SNP_cols <- grep("rs", names(df_nonT2D_heartmodel_CVD_Yes_NO), value = TRUE)

nonT2D_heartmodel_CVD_Yes_NO_result <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  nonT2D_heartmodel_CVD_Yes_NO_model <- glm(CVD6150 ~ sex + age + WHR + BMI +., data = df_nonT2D_heartmodel_CVD_Yes_NO[, c(SNP_cols[i], "sex", "age","WHR","BMI", "CVD6150")], family = binomial(link = "logit"))
  
  nonT2D_heartmodel_CVD_Yes_NO_summary <- tidy(nonT2D_heartmodel_CVD_Yes_NO_model)
  
  nonT2D_heartmodel_CVD_Yes_NO_result[i, "P_value"] <- nonT2D_heartmodel_CVD_Yes_NO_summary[nrow(nonT2D_heartmodel_CVD_Yes_NO_summary), "p.value"]
  nonT2D_heartmodel_CVD_Yes_NO_result[i, "beta"] <- nonT2D_heartmodel_CVD_Yes_NO_summary[nrow(nonT2D_heartmodel_CVD_Yes_NO_summary), "estimate"]
}
nonT2D_heartmodel_CVD_Yes_NO_result$OR <- exp(nonT2D_heartmodel_CVD_Yes_NO_result$beta)
P_values <- nonT2D_heartmodel_CVD_Yes_NO_result$P_value
adjusted_P_values <- p.adjust(P_values, method = "BH")
nonT2D_heartmodel_CVD_Yes_NO_result$adjusted_p_value <- adjusted_P_values

write.csv(nonT2D_heartmodel_CVD_Yes_NO_result, "nonT2D_heartmodel_CVD_Yes_NO_result.csv", row.names = FALSE)
nonT2D_heartmodel_CVD_Yes_NO_result <- read.csv("nonT2D_heartmodel_CVD_Yes_NO_result.csv")
write_xlsx(nonT2D_heartmodel_CVD_Yes_NO_result, "nonT2D_heartmodel_CVD_Yes_NO_result.xlsx")
