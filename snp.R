library(data.table)
library(dplyr)

SNP_c1 <- fread("ukb22418_c1_b0_v2.extract.raw")
fwrite(SNP_c1, "ukb22418_c1_b0_v2.extract.txt", sep = " ", na = "NA", quote = FALSE)
SNP_c2 <- fread("ukb22418_c2_b0_v2.extract.raw")
fwrite(SNP_c2, "ukb22418_c2_b0_v2.extract.txt", sep = " ", na = "NA", quote = FALSE)
SNP_c4 <- fread("ukb22418_c4_b0_v2.extract.raw")
fwrite(SNP_c4, "ukb22418_c4_b0_v2.extract.txt", sep = " ", na = "NA", quote = FALSE)
SNP_c7 <- fread("ukb22418_c7_b0_v2.extract.raw")
fwrite(SNP_c7, "ukb22418_c7_b0_v2.extract.txt", sep = " ", na = "NA", quote = FALSE)
SNP_c8 <- fread("ukb22418_c8_b0_v2.extract.raw")
fwrite(SNP_c8, "ukb22418_c8_b0_v2.extract.txt", sep = " ", na = "NA", quote = FALSE)
SNP_c16 <- fread("ukb22418_c16_b0_v2.extract.raw")
fwrite(SNP_c16, "ukb22418_c16_b0_v2.extract.txt", sep = " ", na = "NA", quote = FALSE)
SNP_c19 <- fread("ukb22418_c19_b0_v2.extract.raw")
fwrite(SNP_c19, "ukb22418_c19_b0_v2.extract.txt", sep = " ", na = "NA", quote = FALSE)
SNP_c22 <- fread("ukb22418_c22_b0_v2.extract.raw")
fwrite(SNP_c22, "ukb22418_c22_b0_v2.extract.txt", sep = " ", na = "NA", quote = FALSE)

SNP_c1 <- subset(SNP_c1, select = -c(PAT, MAT, SEX, PHENOTYPE))
SNP_c2 <- subset(SNP_c2, select = -c(PAT, MAT, SEX, PHENOTYPE))
SNP_c4 <- subset(SNP_c4, select = -c(PAT, MAT, SEX, PHENOTYPE))
SNP_c7 <- subset(SNP_c7, select = -c(PAT, MAT, SEX, PHENOTYPE))
SNP_c8 <- subset(SNP_c8, select = -c(PAT, MAT, SEX, PHENOTYPE))
SNP_c16 <- subset(SNP_c16, select = -c(PAT, MAT, SEX, PHENOTYPE))
SNP_c19 <- subset(SNP_c19, select = -c(PAT, MAT, SEX, PHENOTYPE))
SNP_c22 <- subset(SNP_c22, select = -c(PAT, MAT, SEX, PHENOTYPE))

colnames(SNP_c1)[1] <- "eid"
colnames(SNP_c2)[1] <- "eid"
colnames(SNP_c4)[1] <- "eid"
colnames(SNP_c7)[1] <- "eid"
colnames(SNP_c8)[1] <- "eid"
colnames(SNP_c16)[1] <- "eid"
colnames(SNP_c19)[1] <- "eid"
colnames(SNP_c22)[1] <- "eid"

df_SNP <- merge(df_common, SNP_c1, by = "eid")
df_SNP <- merge(df_SNP, SNP_c2, by = "eid")
df_SNP <- merge(df_SNP, SNP_c4, by = "eid")
df_SNP <- merge(df_SNP, SNP_c7, by = "eid")
df_SNP <- merge(df_SNP, SNP_c8, by = "eid")
df_SNP <- merge(df_SNP, SNP_c16, by = "eid")
df_SNP <- merge(df_SNP, SNP_c19, by = "eid")
df_SNP <- merge(df_SNP, SNP_c22, by = "eid")

df_SNP <- select(df_SNP, -IID.x, -IID.y)
write.csv(df_SNP, file = "df_SNP.csv")
