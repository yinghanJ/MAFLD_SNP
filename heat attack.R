#heart attack

#instance 0 - intance1,2, only keep 1

#minus-3
df_6150_0 <- df_small_data[, c("eid","X6150.0.0")]
df_6150_0 <- df_6150_0[df_6150_0$X6150.0.0 != -3, ]
df_6150_0 <- na.omit(df_6150_0)
df_heart_attack_0 <- df_6150_0
df_heart_attack_0$X6150.0.0[df_heart_attack_0$X6150.0.0 != 1] <- 0

df_age_heart_attack3894_0<- df_age_attend21003[, c("eid","age_attend21003_0")]
df_age_heart_attack3894_0 <- na.omit(df_age_heart_attack3894_0)
df_age_heart_attack3894_0 <- df_age_heart_attack3894_0[df_age_heart_attack3894_0$age_heart_attack3894_0 != -3, ]
df_age_heart_attack3894_0 <- df_age_heart_attack3894_0[df_age_heart_attack3894_0$age_heart_attack3894_0 != -1, ]
df_heart_attack_0_time_condition <- merge(df_heart_attack_0, df_age_heart_attack3894_0, by = "eid", all = TRUE)
colnames(df_heart_attack_0_time_condition)[2] <- "heart_attack_0"

install.packages("survminer")

library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)

time <- df_heartmodel_CVD_Yes_NO_all$age
event <- df_heartmodel_CVD_Yes_NO_all$CVD6150
surv_obj <- Surv(time, event)
cox_model <- coxph(surv_obj ~ time)
summary(cox_model)
rs11208648_G <-df_heartmodel_CVD_Yes_NO_all$rs11208648_G
surv_fit <- survfit(surv_obj ~ rs11208648_G)

# 绘制Kaplan-Meier曲线
ggsurvplot(surv_fit, data = df_heartmodel_CVD_Yes_NO_all, risk.table = TRUE, legend.title = "rs11208648_G",
           xlab = "Time", ylab = "Survival Probability", surv.median.line = "hv")


df_6150_1 <- df_small_data[, c("eid","X6150.1.0")]
df_6150_1 <- df_6150_1[df_6150_1$X6150.1.0 != -3, ]
df_6150_1 <- na.omit(df_6150_1)
df_heart_attack_1 <- df_6150_1
df_heart_attack_1$X6150.1.0[df_heart_attack_1$X6150.1.0 != 1] <- 0


df_6150_2 <- df_small_data[, c("eid","X6150.2.0")]
df_6150_2 <- df_6150_2[df_6150_2$X6150.2.0 != -3, ]
df_heart_attack_2 <- df_6150_2
df_heart_attack_2$X6150.2.0[df_heart_attack_2$X6150.2.0 != 1] <- 0
df_heart_attack_2 <- na.omit(df_heart_attack_2)

df_6150_time_to_event_1 <- merge(df_6150_0, df_6150_1, by = "eid",  all = TRUE)
df_6150_time_to_event <- merge(df_6150_time_to_event, df_6150_2, by = "eid", all = TRUE)
