df_brainmodel_Depression20126_Recurrent_severe<- df_brainmodel_Depression20126_2[df_brainmodel_Depression20126_2$Depression20126 == 3,]
df_brainmodel_Depression20126_Recurrent_severe$Depression20126 <- 1
df_brainmodel_Depression20126_Recurrent_moderate<- df_brainmodel_Depression20126_2[df_brainmodel_Depression20126_2$Depression20126 == 4,]
df_brainmodel_Depression20126_Recurrent_moderate$Depression20126 <- 2
df_brainmodel_Depression20126_Single<- df_brainmodel_Depression20126_2[df_brainmodel_Depression20126_2$Depression20126 == 5,]
df_brainmodel_Depression20126_Single$Depression20126 <- 3

df_brainmodel_Depression20126_123<- rbind(df_brainmodel_Depression20126_Recurrent_severe, df_brainmodel_Depression20126_Recurrent_moderate, df_brainmodel_Depression20126_Single)

#########step2 ########
#########all data ########
library(broom)
library(writexl)
library(dplyr)
SNP_cols <- grep("rs", names(df_brainmodel_Depression20126_123), value = TRUE)

Depression20126_123_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Depression20126_123_model <- lm(Depression20126 ~ sex + age + ., data = df_brainmodel_Depression20126_123[, c(SNP_cols[i], "sex", "age", "Depression20126")])
  
  Depression20126_123_summary <- tidy(Depression20126_123_model)
  
  Depression20126_123_result_all[i, "P_value"] <- Depression20126_123_summary[nrow(Depression20126_123_summary), "p.value"]
  Depression20126_123_result_all[i, "beta"] <- Depression20126_123_summary[nrow(Depression20126_123_summary), "estimate"]
}
write.csv(Depression20126_123_result_all, "Depression20126_123_result_all.csv", row.names = FALSE)
df_Depression20126_123_result_all <- read.csv("Depression20126_123_result_all.csv")
write_xlsx(df_Depression20126_123_result_all, "Depression20126_123_result_all.xlsx")

########softmax model#####
library(nnet)
# 将Depression20126转换为分类变量
df_brainmodel_Depression20126_123 <- df_brainmodel_Depression20126_123 %>% 
  mutate(Depression20126_cat = case_when(
    Depression20126 == 1 ~ "severe",
    Depression20126 == 2 ~ "moderate",
    Depression20126 == 3 ~ "single"
  ))

SNP_cols <- grep("rs", names(df_brainmodel_Depression20126_123), value = TRUE)

Depression20126_123_result_all <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))

for (i in 1:length(SNP_cols)) {
  Depression20126_123_model <- multinom(Depression20126_cat ~ sex + age + ., data = df_brainmodel_Depression20126_123[, c(SNP_cols[i], "sex", "age", "Depression20126_cat")])
  
  Depression20126_123_summary <- tidy(Depression20126_123_model)
  
  Depression20126_123_result_all[i, "P_value"] <- Depression20126_123_summary[nrow(Depression20126_123_summary), "p.value"]
  Depression20126_123_result_all[i, "beta"] <- Depression20126_123_summary[nrow(Depression20126_123_summary), "estimate"]
}

###################T2d and non##########
df_nonT2D_brainmode_all <- read.csv("df_nonT2D_brainmode_all.csv")
df_T2D_brainmodel <- read.csv("df_T2D_brainmodel.csv")
df_T2D_brainmodel_Depression20126<- select(df_T2D_brainmodel,-NeurotisismX20127,-Depression_Episodes4620,-X.1, -X)
df_T2D_brainmodel_Depression20126_123<- merge(df_T2D_brainmodel_Depression20126, 
                                              df_brainmodel_Depression20126_123[c("eid", "Depression20126")], 
                                                 by = "eid")
df_T2D_brainmodel_Depression20126_123 <- select(df_T2D_brainmodel_Depression20126_123,-Depression20126.x)
df_T2D_brainmodel_Depression20126_123 <- na.omit(df_T2D_brainmodel_Depression20126_123)


df_nonT2D_brainmodel_Depression20126_123<- select(df_nonT2D_brainmode_all,-NeurotisismX20127,-Depression_Episodes4620)
df_nonT2D_brainmodel_Depression20126_123 <- na.omit(df_nonT2D_brainmodel_Depression20126_123)
df_nonT2D_brainmodel_Depression20126_123<- merge(df_nonT2D_brainmodel_Depression20126_123, 
                                                 df_brainmodel_Depression20126_123[c("eid", "Depression20126")], 
                                                    by = "eid")
df_nonT2D_brainmodel_Depression20126_123 <- select(df_nonT2D_brainmodel_Depression20126_123,-Depression20126.x,-X)
write.csv(df_T2D_brainmodel_Depression20126_123, "df_T2D_brainmodel_Depression20126_123.csv", row.names = FALSE)
write.csv(df_nonT2D_brainmodel_Depression20126_123, "df_nonT2D_brainmodel_Depression20126_123.csv", row.names = FALSE)
#####for t2d
SNP_cols <- grep("rs", names(df_T2D_brainmodel_Depression20126_123), value = TRUE)

Depression20126_123_result_T2D <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))
df_T2D_brainmodel_Depression20126_123 <- df_T2D_brainmodel_Depression20126_123 %>% 
  mutate(Depression20126_cat = case_when(
    Depression20126.y == 1 ~ "severe",
    Depression20126.y == 2 ~ "moderate",
    Depression20126.y == 3 ~ "single"
  ))

for (i in 1:length(SNP_cols)) {
  Depression20126_123_model_T2D <- multinom(Depression20126_cat ~ sex + age + DM_age + ., data = df_T2D_brainmodel_Depression20126_123[, c(SNP_cols[i], "sex", "age", "DM_age", "Depression20126_cat")])
  Depression20126_123_summary_T2D <- tidy (Depression20126_123_model_T2D)
  
  Depression20126_123_result_T2D[i, "P_value"] <- Depression20126_123_summary_T2D[nrow(Depression20126_123_summary_T2D), "p.value"]
  Depression20126_123_result_T2D[i, "beta"] <- Depression20126_123_summary_T2D[nrow(Depression20126_123_summary_T2D), "estimate"]
}

write.csv(Depression20126_123_result_T2D, "Depression20126_123_result_T2D_softmax.csv", row.names = FALSE)
df_Depression20126_123_result_T2D <- read.csv("Depression20126_123_result_T2D_softmax.csv")
write_xlsx(df_Depression20126_123_result_T2D, "Depression20126_123_result_T2D_softmax.xlsx")

#######dor non t2d
SNP_cols <- grep("rs", names(df_nonT2D_brainmodel_Depression20126_123), value = TRUE)

Depression20126_123_result_nonT2D <- data.frame(SNP = SNP_cols, P_value = numeric(length(SNP_cols)), beta = numeric(length(SNP_cols)))
df_nonT2D_brainmodel_Depression20126_123 <- df_nonT2D_brainmodel_Depression20126_123 %>% 
  mutate(Depression20126_cat = case_when(
    Depression20126.y == 1 ~ "severe",
    Depression20126.y == 2 ~ "moderate",
    Depression20126.y == 3 ~ "single"
  ))
for (i in 1:length(SNP_cols)) {
  Depression20126_123_model_nonT2D <- multinom(Depression20126_cat ~ sex + age + ., data = df_nonT2D_brainmodel_Depression20126_123[, c(SNP_cols[i], "sex", "age", "Depression20126_cat")])
  
  Depression20126_123_summary_nonT2D <- tidy (Depression20126_123_model_nonT2D)
  
  Depression20126_123_result_nonT2D[i, "P_value"] <- Depression20126_123_summary_nonT2D[nrow(Depression20126_123_summary_nonT2D), "p.value"]
  Depression20126_123_result_nonT2D[i, "beta"] <- Depression20126_123_summary_nonT2D[nrow(Depression20126_123_summary_nonT2D), "estimate"]
}

write.csv(Depression20126_123_result_nonT2D, "Depression20126_123_result_nonT2D_softmax.csv", row.names = FALSE)
df_Depression20126_123_result_nonT2D <- read.csv("Depression20126_123_result_nonT2D_softmax.csv")
write_xlsx(df_Depression20126_123_result_nonT2D, "Depression20126_123_result_nonT2D_softmax.xlsx")


