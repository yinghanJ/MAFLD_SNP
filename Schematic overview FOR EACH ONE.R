#Schematic overview FOR EACH ONE
#PREPARE DATA
df_6150_0 <- df_small_data[, c("eid","X6150.0.0")]
df_6150_1 <- df_small_data[, c("eid","X6150.1.0")]
df_6150_2 <- df_small_data[, c("eid","X6150.2.0")]
df_CVD_0123 <- df_small_data[, c("eid","X6150.0.0","X6150.1.0","X6150.2.0")]
df_CVD_0123 <- df_CVD_0123[!(df_CVD_0123$X6150.0.0 %in% c(-1, -3)), ]

# 删除 X6150.0.0 列数值不等于 -7 的行
df_CVD_7 <- df_CVD_0123[df_CVD_0123$X6150.0.0 == -7, ]
# 将 X6150.0.0 列的数值改为 0
df_CVD_7$X6150.0.0 <- 0

#df_high_blood_pressure2966
# 将 X6150.1.0 列中数值为 4 的值改为 1，其他值改为 0
df_high_blood_pressure2966 <- df_CVD_7
# 删除数值为 1、2、3、-1 和 -3 的行
df_high_blood_pressure2966 <- df_high_blood_pressure2966[!(df_high_blood_pressure2966$X6150.1.0 %in% c(1, 2, 3, -1, -3)), ]
df_high_blood_pressure2966 <- df_high_blood_pressure2966[!(df_high_blood_pressure2966$X6150.2.0 %in% c(1, 2, 3, -1, -3)), ]
# 将 X6150.1.0 列中数值为 4 的值改为 1，其他值改为 0
df_high_blood_pressure2966$X6150.1.0[df_high_blood_pressure2966$X6150.1.0 == 4] <- 1
df_high_blood_pressure2966$X6150.2.0[df_high_blood_pressure2966$X6150.2.0 == 4] <- 1
df_high_blood_pressure2966$X6150.1.0[df_high_blood_pressure2966$X6150.1.0 == -7] <- 0
df_high_blood_pressure2966$X6150.2.0[df_high_blood_pressure2966$X6150.2.0 == -7] <- 0
# 删除 X6150.1.0 和 X6150.2.0 值全部为 NA 的行
df_high_blood_pressure2966 <- df_high_blood_pressure2966[!(is.na(df_high_blood_pressure2966$X6150.1.0) & is.na(df_high_blood_pressure2966$X6150.2.0)), ]
##add the age
df_high_blood_pressure2966 <- merge(df_age_attend21003, df_high_blood_pressure2966[, c("eid", "X6150.0.0","X6150.1.0","X6150.2.0")], by = "eid")
df_high_blood_pressure2966 <- df_high_blood_pressure2966[, -which(names(df_high_blood_pressure2966) == "age_attend21003_3")]
write.csv(df_high_blood_pressure2966, file = "df_high_blood_pressure2966.csv")

##test
library(survival)
library(survminer)

# 创建时间事件数据
time_event_data <- Surv(time = df_high_blood_pressure2966$X6150.0.0,
                        event = df_high_blood_pressure2966$X6150.0.0)

# 创建 Kaplan-Meier 生存分析对象
km_survival <- survfit(time_event_data ~ X6150.0.0 + X6150.1.0 + X6150.2.0,
                       data = df_high_blood_pressure2966)

# 绘制 Kaplan-Meier 曲线
ggsurvplot(km_survival, data = df_high_blood_pressure2966,
           risk.table = TRUE, pval = TRUE,
           xlab = "Time", ylab = "Survival Probability")
