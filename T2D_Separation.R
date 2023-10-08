##############################
### T2D Separation of Data ###
##############################

#Load Packages
library(dplyr)
library(tidyverse)

#Read in data
df <- read.csv("/home/projects/reg_00022/data/test_directory/data_small.csv")
df_primary_dates <- read.csv("/home/projects/reg_00022/data/test_directory/primary_dates.csv")

#Merge dates and main data
df <- merge(df, df_primary_dates, by = "eid")


#Changing the name of the columns
colnames(df) <- gsub("X", "", colnames(df), fixed = TRUE)

colnames(df) <- ifelse(colnames(df) == "eid", "eid", 
                       sapply(strsplit(colnames(df), "\\."), function(pieces) {
                         paste(pieces[1], paste(pieces[-1], collapse = "."), sep = "-")
                       }))

#Loading the conversion of names from the UKbb
ukb_data_dict <- read.csv("/home/projects/reg_00022/data/test_directory/ukb_data_dict.csv")

# Choose only the specific columns needed for the change of names
col_names_to_change <- ukb_data_dict$descriptive_colnames
col_nums_to_match <- ukb_data_dict$colheaders_raw

# Make sure all values are treated as strings
colnames(df) <- as.character(colnames(df))
col_nums_to_match <- as.character(col_nums_to_match)

# Matching names
indices <- match(col_nums_to_match, colnames(df))

# Use the indices to change the column names in df
colnames(df) <- col_names_to_change[indices]

#Filtering out the follow ups columns
colnames_baseline <- subset(ukb_data_dict, instance %in% c(0, NA))$descriptive_colnames

df <- df %>%
  select(all_of(colnames_baseline))

#Regex pattern for Non-Insulin-Dependant-Diabetes
icd_pattern <- "^E11[0-9]$"
icd_col_prefix <- "diagnoses_main_icd10_f41202_0_"

# Function that extracts the ICD-10 dates
extract_icd_dates <- function(df, eid, date_of_attending_assessment_center) {
  icd_cols <- grep(icd_col_prefix, names(df), value = TRUE)
  icd_codes <- unlist(df[eid, icd_cols])
  icd_dates <- unlist(df[eid, str_replace(icd_cols, icd_col_prefix, "date_of_first_in_patient_diagnosis_main_icd10_f41262_0_")])
  icd_matches <- grep(icd_pattern, icd_codes)
  icd_date_str <- paste0(icd_dates[icd_matches], collapse = ", ")
  return(icd_date_str)
}

# Apply function to each subject and add to data frame  ("2017-08-08, 2015-05-07")
df$ICD_10 <- unlist(lapply(seq_along(df$eid), function(i) extract_icd_dates(df, i)))

#Splitting the dates ("2017-08-08"  " 2015-05-07" if T2D else character(0))
df$ICD_10_dates <- strsplit(df$ICD_10, ",")

#Converting to date format
df$date_of_attending_assessment_centre_f53_0_0 <- as.Date(df$date_of_attending_assessment_centre_f53_0_0, format = "%Y-%m-%d")

#Filtering out people without diabetes
df_icd_10 <- df[sapply(df$ICD_10, function(x) nchar(x) > 0), ]

#Converting each date to a date format
df_icd_10$ICD_10_dates <- lapply(strsplit(df_icd_10$ICD_10, ","), function(x) {
  
  return(as.Date(x, format = "%Y-%m-%d"))
})

df_icd_10$T2D <- 0

for (i in seq_along(df_icd_10$ICD_10_dates)) {
  icd_dates <- df_icd_10$ICD_10_dates[[i]]
  if (!is.null(icd_dates)) {
    for (j in seq_along(icd_dates)) {
      age <- as.numeric(format(icd_dates[j],'%Y'))- df_icd_10$year_of_birth_f34_0_0[i]
      check1 = !is.na(icd_dates[j]) & icd_dates[j] < as.Date(df_icd_10$date_of_attending_assessment_centre_f53_0_0[i], format = "%Y-%m-%d")
      check2 = age > 35
      if (check1 && check2) {
        df_icd_10$T2D[i] <- 1
        break
      }
    }
  }
}


#List of eid for the ICD-10 definition
eid_icd_10 <-df_icd_10[df_icd_10$T2D == 1, ]$eid

#Original definitons
df <- df %>%
  mutate(T2D = case_when(
    diabetes_diagnosed_by_doctor_f2443_0_0 == 1 & 
    age_diabetes_diagnosed_f2976_0_0 >= 35 &
    started_insulin_within_one_year_diagnosis_of_diabetes_f2986_0_0 == 0 ~ 1,
    TRUE ~ 0
))

#Excluding
df$T2D[df$T2D == 1 & is.na(df$age_diabetes_diagnosed_f2976_0_0)] <- 0
df$T2D[df$T2D == 1 & df$gestational_diabetes_only_f4041_0_0 == 1] <- 0
df$T2D[df$T2D == 1 & df$age_diabetes_diagnosed_f2976_0_0 < 35] <- 0

#Adding subject from the ICD-10 definition
df$T2D[df$eid %in% eid_icd_10] <- 1

#Writing a csv file
write_csv(df, file = "/home/projects/reg_00022/data/test_directory/Data_T2D.csv")
