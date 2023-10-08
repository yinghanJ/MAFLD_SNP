library(broom)
library(writexl)
library(dplyr)
library(RColorBrewer)
colors <- brewer.pal(5, "Blues")[2:5]


# 从数据框中删除指定的列
columns_to_remove <- c("CVD6150", "Age_Stroke4056", "Age_Heart_attack3894", "Age_high_blood_pressure2966", "HBA1C30750", "HDL30760", "LDL30780", "Triglycerides30870")
df<- df_heartmodel_all
df_Creactive_protein30710 <- df[, !(colnames(df) %in% columns_to_remove)]

# 保存修改后的数据框为新文件
write.csv(df_Creactive_protein30710, "Creactive_protein30710.csv", row.names = FALSE)

#######for the all data
###reduce age
df_Creactive_protein30710_1 <- subset(df_Creactive_protein30710, select = -DM_age)
# 清除DataFrame中的缺失值
df_Creactive_protein30710_1 <- na.omit(df_Creactive_protein30710_1)

###tu
# 统计不同类别的计数
count_less_than_1 <- sum(df_Creactive_protein30710_1$Creactive_protein30710 < 1)
count_1_to_2 <- sum(df_Creactive_protein30710_1$Creactive_protein30710 >= 1 & df_Creactive_protein30710_1$Creactive_protein30710 < 2)
count_2_to_3 <- sum(df_Creactive_protein30710_1$Creactive_protein30710 >= 2 & df_Creactive_protein30710_1$Creactive_protein30710 < 3)
count_greater_than_3 <- sum(df_Creactive_protein30710_1$Creactive_protein30710 >= 3)

# 创建饼状图数据
labels <- c( "<1", "1~2", "2~3", ">=3")
counts <- c(count_less_than_1, count_1_to_2, count_2_to_3, count_greater_than_3)
# 计算比例
proportions <- counts / sum(counts)

# 绘制饼状图
pie(counts, labels = paste(labels, "\n", counts, " (", round(proportions * 100, 1), "%)"), col = colors, main = "C reactive protein levels")

# 统计不同类别的计数
count_less_than_1 <- sum(df_Creactive_protein30710_T2D$Creactive_protein30710 < 1)
count_1_to_2 <- sum(df_Creactive_protein30710_T2D$Creactive_protein30710 >= 1 & df_Creactive_protein30710_T2D$Creactive_protein30710 < 2)
count_2_to_3 <- sum(df_Creactive_protein30710_T2D$Creactive_protein30710 >= 2 & df_Creactive_protein30710_T2D$Creactive_protein30710 < 3)
count_greater_than_3 <- sum(df_Creactive_protein30710_T2D$Creactive_protein30710 >= 3)

# 创建饼状图数据
labels <- c( "<1", "1~2", "2~3", ">=3")
counts <- c(count_less_than_1, count_1_to_2, count_2_to_3, count_greater_than_3)
# 计算比例
proportions <- counts / sum(counts)

# 绘制饼状图
pie(counts, labels = paste(labels, "\n", counts, " (", round(proportions * 100, 1), "%)"), col = colors, main = "C reactive protein levels(DM)")
####t2d no t2d####t2d no t2d
# 将列名从 "eid" 改为 "fid"
colnames(df_T2D_EID)[colnames(df_T2D_EID) == "eid"] <- "fid"

# 按照 "fid" 对齐两个 DataFrame
merged_df1 <- merge(df_Creactive_protein30710, df_T2D_EID, by.x = "eid", by.y = "fid")
# 删除缺失值
df_Creactive_protein30710_T2D <- na.omit(merged_df1)
write.csv(df_Creactive_protein30710_T2D, "df_Creactive_protein30710_T2D.csv", row.names = FALSE)
# 根据 "eid" 对齐并删除具有相同 "eid" 的行
# 根据 "eid" 删除具有相同 "eid" 的行
df_Creactive_protein30710_nonT2D <- df_Creactive_protein30710_1[!df_Creactive_protein30710_1$eid %in% df_Creactive_protein30710_T2D$eid, ]
write.csv(df_Creactive_protein30710_nonT2D, "df_Creactive_protein30710_nonT2D.csv", row.names = FALSE)



#####################################
# 创建数据框
data <- data.frame(Category = c("<1", "1-2", "2-3", ">=3"),
                   DM = c(3366, 2860, 1616, 3524),
                   NON_DM = c(116358, 73989, 36004, 63108))

# 计算百分比
data$DM_Percentage <- data$DM / sum(data$DM) * 100
data$NON_DM_Percentage <- data$NON_DM / sum(data$NON_DM) * 100

# 将数据框转换为长格式
data_long <- tidyr::pivot_longer(data, cols = c("DM_Percentage", "NON_DM_Percentage"), names_to = "Group", values_to = "Percentage")

# 定义颜色
colors <- c("#E8F2FA", "#A2CCEC", "#6AAED6", "#3182BD")  # 蓝色渐变

# 绘制条形图
bar_plot <- ggplot(data_long, aes(x = Group, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = colors) +
  labs(x = "Group", y = "Percentage", title = "C reactive protein Distribution") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10))

# 显示条形图
print(bar_plot)

df_Creactive_protein30710_1
# Merge the data frames
# 提取指定列
df <- df_heartmodel_CVD_Yes_NO_all[, c("eid", "CVD6150")]
merged_df5 <- merge(df, df_Creactive_protein30710_1, by = "eid")
merged_df5 <- na.omit(merged_df5)

merged_df51 <- merged_df5[merged_df5$CVD6150 == 1, ]
merged_df50 <- merged_df5[merged_df5$CVD6150 == 0, ]
# 统计不同类别的计数
count_less_than_1 <- sum(merged_df51$Creactive_protein30710 < 1)
count_1_to_2 <- sum(merged_df51$Creactive_protein30710 >= 1 & merged_df51$Creactive_protein30710 < 2)
count_2_to_3 <- sum(merged_df51$Creactive_protein30710 >= 2 & merged_df51$Creactive_protein30710 < 3)
count_greater_than_3 <- sum(merged_df51$Creactive_protein30710 >= 3)

# 创建饼状图数据
labels <- c( "<1", "1~2", "2~3", ">=3")
counts <- c(count_less_than_1, count_1_to_2, count_2_to_3, count_greater_than_3)
# 计算比例
proportions <- counts / sum(counts)

# 绘制饼状图
pie(counts, labels = paste(labels, "\n", counts, " (", round(proportions * 100, 1), "%)"), col = colors, main = "C reactive protein levels(CVD)")

# 创建数据框
data <- data.frame(Category = c("<1", "1-2", "2-3", ">=3"),
                   DM = c(27013, 22929, 12691, 25584),
                   NON_DM = c(92372, 53635, 24781, 25584))

# 计算百分比
data$CVD_Percentage <- data$DM / sum(data$DM) * 100
data$NON_CVD_Percentage <- data$NON_DM / sum(data$NON_DM) * 100

# 将数据框转换为长格式
data_long <- tidyr::pivot_longer(data, cols = c("CVD_Percentage", "NON_CVD_Percentage"), names_to = "Group", values_to = "Percentage")

# 定义颜色
colors <- c("#E8F2FA", "#A2CCEC", "#6AAED6", "#3182BD")  # 蓝色渐变

# 绘制条形图
bar_plot <- ggplot(data_long, aes(x = Group, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = colors) +
  labs(x = "Group", y = "Percentage", title = "C reactive protein Distribution") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10))

# 显示条形图
print(bar_plot)

