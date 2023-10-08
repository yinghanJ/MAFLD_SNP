library(stats)

Depression20126_yes_no_result_T2D <- read.csv("kidney_Microalbumin/Microalbumin_result_all.csv")

P_values <- Depression20126_yes_no_result_T2D$P_value

adjusted_P_values <- p.adjust(P_values, method = "BH")

Depression20126_yes_no_result_T2D$adjusted_p_value <- adjusted_P_values

write_xlsx(Depression20126_yes_no_result_T2D, "1.xlsx")






Depression20126_yes_no_result_T2D <- read.csv("kidney_Microalbumin/Microalbumin_result_T2D.csv")

P_values <- Depression20126_yes_no_result_T2D$P_value

adjusted_P_values <- p.adjust(P_values, method = "BH")

Depression20126_yes_no_result_T2D$adjusted_p_value <- adjusted_P_values

write_xlsx(Depression20126_yes_no_result_T2D, "2.xlsx")




Depression20126_yes_no_result_T2D <- read.csv("kidney_Microalbumin/Microalbumin_result_nonT2D.csv")

P_values <- Depression20126_yes_no_result_T2D$P_value

adjusted_P_values <- p.adjust(P_values, method = "BH")

Depression20126_yes_no_result_T2D$adjusted_p_value <- adjusted_P_values

write_xlsx(Depression20126_yes_no_result_T2D, "3.xlsx")