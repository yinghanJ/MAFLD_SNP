#time to event
# prepare data 6150
df_6150_0 <- df_small_data[, c("eid","X6150.0.0")]
df_6150_1 <- df_small_data[, c("eid","X6150.1.0")]
df_6150_2 <- df_small_data[, c("eid","X6150.2.0")]
df_CVD_0123 <- df_small_data[, c("eid","X6150.0.0","X6150.1.0","X6150.2.0")]
df_CVD_0123 <- df_CVD_0123[!(df_CVD_0123$X6150.0.0 %in% c(-1, -3)), ]
df_CVD_0123$X6150.0.0 <- ifelse(df_CVD_0123$X6150.0.0 == -7, 0, 1)


df_CVD_0123 <- df_CVD_0123[!(df_CVD_0123$X6150.1.0 %in% c(-1, -3)), ]
df_CVD_0123$X6150.1.0 <- ifelse(df_CVD_0123$X6150.1.0 == -7, 0, 1)


df_CVD_0123 <- df_CVD_0123[!(df_CVD_0123$X6150.2.0 %in% c(-1, -3)), ]
df_CVD_0123$X6150.2.0 <- ifelse(df_CVD_0123$X6150.2.0 == -7, 0, 1)

# 删除数值为 NA 的行
df_CVD_0123 <- df_CVD_0123[!is.na(df_CVD_0123$X6150.0.0), ]


df_age_attend21003 <- df_small_data[, c("eid","X21003.0.0","X21003.1.0","X21003.2.0","X21003.3.0")]
colnames(df_age_attend21003)[2] <- "age_attend21003_0"
colnames(df_age_attend21003)[3] <- "age_attend21003_1"
colnames(df_age_attend21003)[4] <- "age_attend21003_2"
colnames(df_age_attend21003)[5] <- "age_attend21003_3"
write.csv(df_age_attend21003, file = "df_age_attend21003.csv")

df_age_death40007 <- df_small_data[, c("eid","X40007.0.0")]
colnames(df_age_death40007)[2] <- "df_age_death40007"
write.csv(df_age_death40007, file = "df_age_death40007.csv")

df_age_angina3627 <- read.csv("age_angina.csv")
colnames(df_age_angina3627)[2] <- "age_angina3627_0"
colnames(df_age_angina3627)[3] <- "age_angina3627_1"
colnames(df_age_angina3627)[4] <- "age_angina3627_2"
colnames(df_age_angina3627)[5] <- "age_angina3627_3"
write.csv(df_age_angina3627, file = "df_age_angina3627.csv")

#df_age_high_blood_pressure2966<- df_small_data[, c("eid","X2966.0.0")]
#colnames(df_age_high_blood_pressure2966)[2] <- "age_high_blood_pressure2966_0"
#colnames(df_age_high_blood_pressure2966)[3] <- "age_high_blood_pressure2966_1"
#colnames(df_age_high_blood_pressure2966)[4] <- "age_high_blood_pressure2966_2"
#colnames(df_age_high_blood_pressure2966)[5] <- "age_high_blood_pressure2966_3"
#write.csv(df_age_high_blood_pressure2966, file = "df_age_high_blood_pressure2966.csv")

df_age_heart_attack3894<- df_small_data[, c("eid","X3894.0.0","X3894.1.0","X3894.2.0","X3894.3.0")]
colnames(df_age_heart_attack3894)[2] <- "age_heart_attack3894_0"
colnames(df_age_heart_attack3894)[3] <- "age_heart_attack3894_1"
colnames(df_age_heart_attack3894)[4] <- "age_heart_attack3894_2"
colnames(df_age_heart_attack3894)[5] <- "age_heart_attack3894_3"
write.csv(df_age_heart_attack3894, file = "df_age_heart_attack3894.csv")

df_age_stroke4056<- df_small_data[, c("eid","X4056.0.0","X4056.1.0","X4056.2.0","X4056.3.0")]
colnames(df_age_stroke4056)[2] <- "age_stroke4056_0"
colnames(df_age_stroke4056)[3] <- "age_stroke4056_1"
colnames(df_age_stroke4056)[4] <- "age_stroke4056_2"
colnames(df_age_stroke4056)[5] <- "age_stroke4056_3"
write.csv(df_age_stroke4056, file = "df_age_stroke4056.csv")



df_6150_time_to_event <- merge(df_6150_0, df_6150_1, by = "eid")
df_6150_time_to_event <- merge(df_6150_time_to_event, df_6150_2, by = "eid")

#add the year 5

df_6150_time_to_event <- na.omit(df_6150_time_to_event)

#删除minus3
library(magrittr)
df_6150_time_to_event <- df_6150_time_to_event[df_6150_time_to_event$X6150.0.0 != -3, ]
df_6150_time_to_event <- df_6150_time_to_event[df_6150_time_to_event$X6150.1.0 != -3, ]
df_6150_time_to_event <- df_6150_time_to_event[df_6150_time_to_event$X6150.2.0 != -3, ]

install.packages("survival")
library(survival)

survival_data <- Surv(time, event)
cox_model <- coxph(formula, data = your_data)


summary(cox_model)
######all time to event

timeevent <- merge(df_age_attend21003, df_CVD_0123[, c("eid", "X6150.0.0")], by = "eid")
timeevent <- merge(timeevent, df_CVD_0123[, c("eid", "X6150.1.0")], by = "eid")
timeevent <- merge(timeevent, df_CVD_0123[, c("eid", "X6150.2.0")], by = "eid")
timeevent <- timeevent[, -which(names(timeevent) == "age_attend21003_3")]

timeevent_yes <- timeevent[timeevent$X6150.0.0 != 1, ]
timeevent_yes <- timeevent_yes[!is.na(timeevent_yes$eid), ]

write.csv(timeevent_yes, file = "timeevent_yes.csv")
timeevent_plot <- timeevent_yes
timeevent_plot$X6150.1.0[is.na(timeevent_plot$X6150.1.0)] <- 0
timeevent_plot$X6150.2.0[is.na(timeevent_plot$X6150.2.0)] <- 0


timeevent_plot$age_attend21003_1[is.na(timeevent_plot$age_attend21003_1)] <- timeevent_plot$age_attend21003_0[is.na(timeevent_plot$age_attend21003_1)]
timeevent_plot$age_attend21003_2[is.na(timeevent_plot$age_attend21003_2)] <- timeevent_plot$age_attend21003_1[is.na(timeevent_plot$age_attend21003_2)]

# Subset the data based on the conditions
subset_data <- timeevent_plot[timeevent_plot$X6150.0.0 == 0 & timeevent_plot$X6150.1.0 == 0 & timeevent_plot$X6150.2.0 == 0 & timeevent_plot$age_attend21003_0 != timeevent_plot$age_attend21003_1, ]



write.xlsx(timeevent_plot, file = "timeevent_plot.xlsx")
# 安装并加载readxl包
install.packages("readxl")
library(readxl)

# 读取Excel文件为data.frame
timeevent_plot20 <- read_excel("timeevent_plot20.xlsx")




install.packages("openxlsx")

library(openxlsx)

# Write the data frame to an Excel file
write.xlsx(timeevent_plot0, file = "timeevent_plot20.xlsx")


# Subset the necessary columns
subset_cols <- c("eid", "age_attend21003_0", "age_attend21003_1", "age_attend21003_2", "X6150.0.0", "X6150.1.0", "X6150.2.0")

# Subset the data
cohort_data <- timeevent_plot20[, subset_cols]

# Create an empty plot
plot(x = 0, y = 0, xlim = range(cohort_data[, c("age_attend21003_0", "age_attend21003_1", "age_attend21003_2")]), ylim = range(cohort_data$eid), xlab = "Time", ylab = "Sample", type = "n", main = "Schematic overview of cohort")

# Plot each sample's lines and markers
for (i in 1:nrow(cohort_data)) {
  age_attend0 <- cohort_data[i, "age_attend21003_0"]
  age_attend1 <- cohort_data[i, "age_attend21003_1"]
  age_attend2 <- cohort_data[i, "age_attend21003_2"]
  X6150_0_0 <- cohort_data[i, "X6150.0.0"]
  X6150_1_0 <- cohort_data[i, "X6150.1.0"]
  X6150_2_0 <- cohort_data[i, "X6150.2.0"]
  eid <- cohort_data[i, "eid"]
  
  if (X6150_0_0 == 0) {
    points(x = age_attend0, y = eid, col = "black", pch = ".")
  } else {
    points(x = age_attend0, y = eid, col = "red",  pch = "X")
  }
  
  if (X6150_1_0 == 0) {
    points(x = age_attend1, y = eid, col = "black", pch = ".")
  } else {
    points(x = age_attend1, y = eid, col = "red", pch = "X")
  }
  
  if (X6150_2_0 == 0) {
    points(x = age_attend2, y = eid, col = "blue", pch = "X")
  } else {
    points(x = age_attend2, y = eid, col = "red", pch = "X")
  }
  
  lines(x = c(age_attend0, age_attend1, age_attend2), y = c(eid, eid, eid), col = "black")
  
  text(x = age_attend0, y = eid, labels = as.character(age_attend0), pos = 2)
  text(x = age_attend1, y = eid, labels = as.character(age_attend1), pos = 2)
  text(x = age_attend2, y = eid, labels = as.character(age_attend2), pos = 2)
}

# Add a vertical line at x = 83
abline(v = 83, col = "red", lwd = 2)

# Add legend for Censoring and Event
legend(x = "topright", legend = c("Censoring", "Event"), pch = "x", col = c("blue", "red"))
png("sample_data_plot.png")
dev.off()
