#############################################
###Linear Regression Model for Metabolites###
#############################################

library(ukbwranglr)
library(dplyr)
library(tidyverse)
library(openxlsx)

log_std_transform <- function(x) {
  log_transformed <- log(x + 1) # add 1 to avoid log(0)
  standardized <- (log_transformed - mean(log_transformed)) / sd(log_transformed)
  return(standardized)
}

metabolites <- read.csv("Data/metabolites.csv")

# Changing the name of the columns
colnames(metabolites) <- gsub("X", "", colnames(metabolites), fixed = TRUE)

colnames(metabolites) <- ifelse(colnames(metabolites) == "eid", "eid", 
                       sapply(strsplit(colnames(metabolites), "\\."), function(pieces) {
                         paste(pieces[1], paste(pieces[-1], collapse = "."), sep = "-")
                       }))

ukb_data_dict <- make_data_dict(metabolites, delim = "auto", ukb_data_dict = get_ukb_data_dict())

# Choose only the specific columns needed for the change of names
col_names_to_change <- ukb_data_dict$descriptive_colnames
col_nums_to_match <- ukb_data_dict$colheaders_raw

# Make sure all values are treated as strings
colnames(metabolites) <- as.character(colnames(metabolites))
col_nums_to_match <- as.character(col_nums_to_match)

# Find the indices of the columns in df that correspond to the names in table
indices <- match(col_nums_to_match, colnames(metabolites))

# Use the indices to change the column names in df
colnames(metabolites) <- col_names_to_change[indices]

# Filtering out follow up columns

colnames_baseline <- subset(ukb_data_dict, instance %in% c(0, NA))$descriptive_colnames

metabolites <- metabolites%>%
  select(all_of(colnames_baseline))

# Filtering out eids that are not T2D
metabolites <- metabolites %>%
  filter(eid %in% clinical_df_diabetes$eid)

# Removing non-relevant columns such as concentration and diameter
cols_to_remove <- grep("concentration|diameter", names(metabolites))
metabolites <- metabolites[, -cols_to_remove]

# Names of metabolites
metabolite_names <- metabolites %>%
  select(-c(eid, creatinine_f23478_0_0))  %>%
  colnames()


#Models
#Model 1: Unadjusted
#Model 2: Adjusted for: age, sex, systolic blood pressure (SBP), BMI, smoking status, diabetes duration, HbA1c + cholesterol lowering medication (Data Field 6177)
#Model 3: Model 2 + Blood pressure lowering medication + UACR

#############
###Model 1###
#############

#eGFR
subset_df <- clinical_df_diabetes %>%
  select(eid, eGFR)

metabolites <- merge(subset_df, metabolites, by = "eid")

#Removing NA values
metabolites <- na.omit(metabolites)

regression_results_model1 <- list()

# Loop through each metabolite
for (metabolite in metabolite_names) {
  
  # Extract the current metabolite and eGFR columns
  subset_data <- metabolites[, c(metabolite, "eGFR")]
  
  # Log transformation and standardization
  subset_data[, 1] <- log_std_transform(subset_data[, 1])
  print(nrow(subset_data))
  # Linear regression
  lm_result <- lm(eGFR ~ ., data = subset_data)
  
  # Store the results for each metabolite in the list
  regression_results_model1[[metabolite]] <- list(
    coefficients = coef(lm_result)[2],
    p_values = summary(lm_result)$coefficients[, 4][2],
    r_squared = summary(lm_result)$r.squared,
    estimate = coef(lm_result)[2],
    adj_p_value = p.adjust(summary(lm_result)$coefficients[, 4], method = "bonferroni")[2]
  )
}

model1_results <- as.data.frame(do.call(rbind, lapply(regression_results_model1, function(x) unlist(x, recursive = FALSE))))

# Write the results in a excel sheet
wb <- createWorkbook("Results/linear_regression_results.xlsx")
addWorksheet(wb, sheetName = "Model1", gridLines = TRUE)
writeData(wb, sheet = "Model1", x = model1_results, rowNames = TRUE)

#############
###Model 2###
#############


# Choosing variables
subset_df <- clinical_df_diabetes %>%
  select(eid,
         age_when_attended_assessment_centre_f21003_0_0,
         sex_f31_0_0,
         systolic_blood_pressure_automated_reading_f4080_0_0,
         body_mass_index_bmi_f21001_0_0,
         current_tobacco_smoking_f1239_0_0,
         glycated_haemoglobin_hba1c_f30750_0_0,
         cholesterol_lowering_medication,
         diabetes_duration)

metabolites <- merge(subset_df, metabolites, by = "eid")

#Removing NA values
metabolites <- na.omit(metabolites)

regression_results_model2 <- list()
# Perform linear regression for each metabolite
for (metabolite in metabolite_names) {
  
  # Extract the current metabolite, eGFR and variables
  subset_data <- metabolites[, c(metabolite,
                                 "eGFR",
                                 "age_when_attended_assessment_centre_f21003_0_0",
                                 "sex_f31_0_0",
                                 "systolic_blood_pressure_automated_reading_f4080_0_0",
                                 "body_mass_index_bmi_f21001_0_0",
                                 "current_tobacco_smoking_f1239_0_0",
                                 "glycated_haemoglobin_hba1c_f30750_0_0", 
                                 "cholesterol_lowering_medication",
                                 "diabetes_duration")]
  

  print(nrow(subset_data))
  # Log transformation and standardization
  subset_data[, 1] <- log_std_transform(subset_data[, 1])
  
  # Linear regression
  lm_result <- lm(eGFR ~ ., data = subset_data)
  
  
  # Store the results for each metabolite in the list
  regression_results_model2[[metabolite]] <- list(
    coefficients = coef(lm_result)[2],
    p_values = summary(lm_result)$coefficients[, 4][2],
    r_squared = summary(lm_result)$r.squared,
    estimate = coef(lm_result)[2],
    adj_p_value = p.adjust(summary(lm_result)$coefficients[, 4], method = "bonferroni")[2]
  )
}

# Unlisiting resuæts
model2_results <- as.data.frame(do.call(rbind, lapply(regression_results_model2, function(x) unlist(x, recursive = FALSE))))

# Write the results in a excel sheet
addWorksheet(wb, sheetName = "Model2", gridLines = TRUE)
writeData(wb, sheet = "Model2", x = model2_results, rowNames = TRUE)


#############
###Model 3###
#############

#CKD
subset_df <- clinical_df_diabetes %>%
  select(eid, blood_pressure_medication, UACR) 

metabolites <- merge(subset_df, metabolites, by = "eid")

regression_results_model3 <- list()
# perform linear regression for each metabolite
for (metabolite in metabolite_names) {
  # extract the current metabolite and eGFR columns
  subset_data <- metabolites[, c(metabolite,
                                 "eGFR",
                                 "age_when_attended_assessment_centre_f21003_0_0",
                                 "sex_f31_0_0",
                                 "systolic_blood_pressure_automated_reading_f4080_0_0",
                                 "body_mass_index_bmi_f21001_0_0", 
                                 "current_tobacco_smoking_f1239_0_0",
                                 "glycated_haemoglobin_hba1c_f30750_0_0",
                                 "cholesterol_lowering_medication",
                                 "diabetes_duration",
                                 "blood_pressure_medication")]
                                 
  
  # remove any rows with missing values
  subset_data <- na.omit(subset_data)
  
  # log transformation and standardization
  subset_data[, 1] <- log_std_transform(subset_data[, 1])
  
  # linear regression
  lm_result <- lm(eGFR ~ ., data = subset_data)
  
  # store the results for each metabolite in the list
  regression_results_model3[[metabolite]] <- list(
    coefficients = coef(lm_result)[2],
    p_values = summary(lm_result)$coefficients[, 4][2],
    r_squared = summary(lm_result)$r.squared,
    estimate = coef(lm_result)[2],
    adj_p_value = p.adjust(summary(lm_result)$coefficients[, 4], method = "bonferroni")[2]
  )
}

model3_results <- as.data.frame(do.call(rbind, lapply(regression_results_model3, function(x) unlist(x, recursive = FALSE))))

# Write the results in a excel sheet
addWorksheet(wb, sheetName = "Model3", gridLines = TRUE)
writeData(wb, sheet = "Model3", x = model3_results, rowNames = TRUE)

# Save the modified Excel file
saveWorkbook(wb, "Results/linear_regression_results.xlsx", overwrite = TRUE)

  