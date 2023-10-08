df_SNP <- read.csv("df_snp.csv")
df_abumin <- read.csv("df_kidneymodel_Microalbumin.csv")
df_kidneymodel_all <- read.csv("df_kidneymodel_all.csv")

df_kidneymodel_eGFR.isnull().values.any()
na_count <- colSums(is.na(df_kidneymodel_eGFR))
na_cols <- which(na_count > 0)
na_col_names <- names(df_kidneymodel_eGFR)[na_cols]
na_count <- na_count[na_cols]
cat("Columns with NAs:\n")
for (i in seq_along(na_col_names)) {
  cat(na_col_names[i], ": ", na_count[i], " NAs\n")
}

merged_df


na_count <- colSums(is.na(df_T2D_kidenymodel_eGFR_1))
na_cols <- which(na_count > 0)
na_col_names <- names(df_T2D_kidenymodel_eGFR_1)[na_cols]
na_count <- na_count[na_cols]
cat("Columns with NAs:\n")
for (i in seq_along(na_col_names)) {
  cat(na_col_names[i], ": ", na_count[i], " NAs\n")
}

df_T2D_kidenymodel_eGFR_1