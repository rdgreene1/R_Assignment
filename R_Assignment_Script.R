library(tidyverse)
library(stringr)
# Import original data files
fang_et_al_genotypes = read.table('./R_Assignment/fang_et_al_genotypes.txt', sep = '\t', header = T)
snp_position = read.table('./R_Assignment/snp_position.txt', sep = '\t', header = T)
# Extract columns 1, 3 and 4 from snp_postion.txt
column_134_snp_position <- select(snp_position, `SNP_ID`, `Chromosome`, `Position`)
# Create maize and teosinte vectors based on group names
teosinte_groups <- c("ZMPBA", "ZMPIL", "ZMPJA")
maize_groups <- c("ZMMIL", "ZMMLR", "ZMMMR")
# Extract maize and teosinte data based on group names
teosinte_data <- fang_et_al_genotypes %>%
  filter(fang_et_al_genotypes[[3]] %in% teosinte_groups)
maize_data <- fang_et_al_genotypes %>%
  filter(fang_et_al_genotypes[[3]] %in% maize_groups)
# Transpose extracted maize and teosinte data
transposed_maize_data <- data.frame(t(maize_data))
transposed_teosinte_data <- data.frame(t(teosinte_data))
# Move row names to first column
formatted_transposed_maize_data <- rownames_to_column(transposed_maize_data, "SNP_ID")
formatted_transposed_teosinte_data <- rownames_to_column(transposed_teosinte_data, "SNP_ID")
# Join transposed data and exracted 1, 3 and 4 snp_position columns
joined_maize_data <- inner_join(column_134_snp_position, formatted_transposed_maize_data, by = "SNP_ID") %>% 
  mutate(Chromosome = as.numeric(Chromosome))
joined_teosinte_data <- inner_join(column_134_snp_position, formatted_transposed_teosinte_data, by = "SNP_ID")
# Arrange the data and replace missing data with new variables
Asc_joined_maize_data <- joined_maize_data %>% 
  mutate_all(~str_replace_all(., "\\?/\\?", "?")) %>% 
  mutate(Position = as.numeric(Position)) %>% 
  arrange(Position)
Des_joined_maize_data <- joined_maize_data %>% 
  mutate_all(~str_replace_all(., "\\?/\\?", "-")) %>% 
  mutate(Position = as.numeric(Position)) %>% 
  arrange(desc(Position))
Asc_joined_teosinte_data <- joined_teosinte_data %>% 
  mutate_all(~str_replace_all(., "\\?/\\?", "?")) %>% 
  mutate(Position = as.numeric(Position)) %>% 
  arrange(Position)
Des_joined_teosinte_data <- joined_teosinte_data %>% 
  mutate_all(~str_replace_all(., "\\?/\\?", "-")) %>% 
  mutate(Position = as.numeric(Position)) %>% 
  arrange(desc(Position))
# Create new data files, one for each chromosome for each organism and export them as .csv files
for (i in 1:10) {
  maize_data_asc_chr <- Asc_joined_maize_data %>% filter(Chromosome == i) 
  assign(paste0("maize_data_asc_chr", i), maize_data_asc_chr)
  file_path <- file.path('~/EEOB546_R_Lesson/R_Assignment', paste0("maize_asc_chr", i, ".csv"))
  write.csv(maize_data_asc_chr, file = file_path, row.names = FALSE)
}
for (i in 1:10) {
  maize_data_des_chr <- Des_joined_maize_data %>% filter(Chromosome == i) 
  assign(paste0("maize_data_des_chr", i), maize_data_des_chr)
  file_path <- file.path('~/EEOB546_R_Lesson/R_Assignment', paste0("maize_des_chr", i, ".csv"))
  write.csv(maize_data_des_chr, file = file_path, row.names = FALSE)
} 
for (i in 1:10) {
  teosinte_data_asc_chr <- Asc_joined_teosinte_data %>% filter(Chromosome == i) 
  assign(paste0("teosinte_data_asc_chr", i), teosinte_data_asc_chr)
  file_path <- file.path('~/EEOB546_R_Lesson/R_Assignment', paste0("teosinte_asc_chr", i, ".csv"))
  write.csv(teosinte_data_asc_chr, file = file_path, row.names = FALSE)
}
for (i in 1:10) {
  teosinte_data_des_chr <- Des_joined_teosinte_data %>% filter(Chromosome == i) 
  assign(paste0("teosinte_data_des_chr", i), teosinte_data_des_chr)
  file_path <- file.path('~/EEOB546_R_Lesson/R_Assignment', paste0("teosinte_des_chr", i, ".csv"))
  write.csv(teosinte_data_des_chr, file = file_path, row.names = FALSE)
}

ggplot(data = joined_maize_data) + geom_bar(mapping = aes(x = Chromosome, fill = Blue))
ggplot(data = joined_teosinte_data) + geom_bar(mapping = aes(x = Chromosome, fill = Blue))

system("git add .")
system("git commit -m 'Final data files'")
system("git push origin main")


homozygous <- c("A/A", "T/T", "G/G", "C/C")
