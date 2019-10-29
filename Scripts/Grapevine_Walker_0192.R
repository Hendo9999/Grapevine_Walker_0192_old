#install and load packages
install.packages("janitor")
install.packages("dplyr")
install.packages("rowr")

library(tidyverse) #for tidy stuff
library(ggplot2)
library(readxl) #for reading Excel files
library(plyr) #for join_all to join multiple dataframes, note stuffs up rename function in dplyr
library(dplyr)
library(janitor) #for tidying up column names
library(rowr) #for cbind.all function


###Standards data for each replicate_batch


##17.1mM Cl standards data
raw_r1b1_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 9, range = "F49:J67") 

#reformat standards data 
#extract vectors with relevant range of values from table as vectors
a_r1b1 <- raw_r1b1_17mM_st$...3[(1:5)] 
b_r1b1 <- raw_r1b1_17mM_st$`607 mg/L`[(1:5)] 
c_r1b1 <- raw_r1b1_17mM_st$...3[(8:17)]
d_r1b1 <- raw_r1b1_17mM_st$`607 mg/L`[(8:17)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b1 <- cbind.fill(a_r1b1, b_r1b1, c_r1b1, d_r1b1, fill = NA)

#rename columns in matrix
colnames(mx_r1b1) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b1 <- data.frame(mx_r1b1) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b1 <- df_1_r1b1 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r1b1_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 9, range = "M49:O57") 

#reformat bulk standards data 
#rename columns in dataframe, correct bulk standard reading & calc Cl% dry weight
bulk_r1b1 <- raw_r1b1_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b1)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b1[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r1b2_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 10, range = "F48:J66")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b2 <- raw_r1b2_17mM_st$...3[(1:5)] 
b_r1b2 <- raw_r1b2_17mM_st$`607 mg/L`[(1:5)] 
c_r1b2 <- raw_r1b2_17mM_st$...3[(8:17)]
d_r1b2 <- raw_r1b2_17mM_st$`607 mg/L`[(8:17)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b2 <- cbind.fill(a_r1b2, b_r1b2, c_r1b2, d_r1b2, fill = NA)

#rename columns in matrix
colnames(mx_r1b2) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b2 <- data.frame(mx_r1b2) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b2 <- df_1_r1b2 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))



###bulk grapevine data
raw_r1b2_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 10, range = "M48:O57") 

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b2 <- raw_r1b2_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b2)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b2[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r1b3_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 12, range = "B48:E66")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b3 <- raw_r1b3_17mM_st$...2[(1:5)] 
b_r1b3 <- raw_r1b3_17mM_st$`607 mg/L`[(1:5)] 
c_r1b3 <- raw_r1b3_17mM_st$...2[(8:16)]
d_r1b3 <- raw_r1b3_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b3 <- cbind.fill(a_r1b3, b_r1b3, c_r1b3, d_r1b3, fill = NA)

#rename columns in matrix
colnames(mx_r1b3) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b3 <- data.frame(mx_r1b3) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b3 <- df_1_r1b3 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))



###bulk grapevine data
raw_r1b3_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 12, range = "H48:J61")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b3 <- raw_r1b3_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b3)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b3[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##17.1mM Cl standards data
raw_r1b4_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 14, range = "B49:E67")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b4 <- raw_r1b4_17mM_st$...2[(1:5)] 
b_r1b4 <- raw_r1b4_17mM_st$`607 mg/L`[(1:5)] 
c_r1b4 <- raw_r1b4_17mM_st$...2[(8:16)]
d_r1b4 <- raw_r1b4_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b4 <- cbind.fill(a_r1b4, b_r1b4, c_r1b4, d_r1b4, fill = NA)

#rename columns in matrix
colnames(mx_r1b4) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b4 <- data.frame(mx_r1b4) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b4 <- df_1_r1b4 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))



###bulk grapevine data
raw_r1b4_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 14, range = "H49:J58")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b4 <- raw_r1b4_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b4)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b4[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##17.1mM Cl standards data
raw_r1b5_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 18, range = "B54:E72")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b5 <- raw_r1b5_17mM_st$...2[(1:5)] 
b_r1b5 <- raw_r1b5_17mM_st$`607 mg/L`[(1:5)] 
c_r1b5 <- raw_r1b5_17mM_st$...2[(8:17)]
d_r1b5 <- raw_r1b5_17mM_st$`607 mg/L`[(8:17)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b5 <- cbind.fill(a_r1b5, b_r1b5, c_r1b5, d_r1b5, fill = NA)

#rename columns in matrix
colnames(mx_r1b5) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b5 <- data.frame(mx_r1b5) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b5 <- df_1_r1b5 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r1b5_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 18, range = "H54:J65")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b5 <- raw_r1b5_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b5)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b5[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r1b6_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 20, range = "B38:E56")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r1b6 <- raw_r1b6_17mM_st$...2[(1:5)] 
b_r1b6 <- raw_r1b6_17mM_st$`607 mg/L`[(1:5)] 
c_r1b6 <- raw_r1b6_17mM_st$...2[(8:15)]
d_r1b6 <- raw_r1b6_17mM_st$`607 mg/L`[(8:15)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r1b6 <- cbind.fill(a_r1b6, b_r1b6, c_r1b6, d_r1b6, fill = NA)

#rename columns in matrix
colnames(mx_r1b6) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r1b6 <- data.frame(mx_r1b6) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))


#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r1b6 <- df_1_r1b6 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r1b6_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP1.xlsx", sheet = 20, range = "H38:J49")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r1b6 <- raw_r1b6_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r1b6)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r1b6[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r2b1_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 8, range = "F122:J140")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b1 <- raw_r2b1_17mM_st$...3[(1:5)] 
b_r2b1 <- raw_r2b1_17mM_st$`607 mg/L`[(1:5)] 
c_r2b1 <- raw_r2b1_17mM_st$...3[(8:15)]
d_r2b1 <- raw_r2b1_17mM_st$`607 mg/L`[(8:15)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b1 <- cbind.fill(a_r2b1, b_r2b1, c_r2b1, d_r2b1, fill = NA)

#rename columns in matrix
colnames(mx_r2b1) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b1 <- data.frame(mx_r2b1) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b1 <- df_1_r2b1 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b1_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 8, range = "M122:O131")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b1 <- raw_r2b1_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b1)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b1[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r2b2_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 9, range = "F118:J136")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b2 <- raw_r2b2_17mM_st$...3[(1:5)] 
b_r2b2 <- raw_r2b2_17mM_st$`607 mg/L`[(1:5)] 
c_r2b2 <- raw_r2b2_17mM_st$...3[(8:16)]
d_r2b2 <- raw_r2b2_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b2 <- cbind.fill(a_r2b2, b_r2b2, c_r2b2, d_r2b2, fill = NA)

#rename columns in matrix
colnames(mx_r2b2) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b2 <- data.frame(mx_r2b2) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b2 <- df_1_r2b2 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b2_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 9, range = "M118:O128")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b2 <- raw_r2b2_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b2)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b2[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r2b3_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 11, range = "C51:G69")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b3 <- raw_r2b3_17mM_st$...3[(1:5)] 
b_r2b3 <- raw_r2b3_17mM_st$`607 mg/L`[(1:5)] 
c_r2b3 <- raw_r2b3_17mM_st$...3[(8:16)]
d_r2b3 <- raw_r2b3_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b3 <- cbind.fill(a_r2b3, b_r2b3, c_r2b3, d_r2b3, fill = NA)

#rename columns in matrix
colnames(mx_r2b3) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b3 <- data.frame(mx_r2b3) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b3 <- df_1_r2b3 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b3_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 11, range = "J51:L61")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b3 <- raw_r2b3_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b3)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b3[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))




##17.1mM Cl standards data
raw_r2b4_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 13, range = "C74:G92")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b4 <- raw_r2b4_17mM_st$...3[(1:5)] 
b_r2b4 <- raw_r2b4_17mM_st$`607 mg/L`[(1:5)] 
c_r2b4 <- raw_r2b4_17mM_st$...3[(8:16)]
d_r2b4 <- raw_r2b4_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b4 <- cbind.fill(a_r2b4, b_r2b4, c_r2b4, d_r2b4, fill = NA)

#rename columns in matrix
colnames(mx_r2b4) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b4 <- data.frame(mx_r2b4) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b4 <- df_1_r2b4 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b4_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 13, range = "J74:L85")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b4 <- raw_r2b4_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b4)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b4[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##17.1mM Cl standards data
raw_r2b5_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 17, n_max = 47, range = "A52:E68")



#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b5 <- raw_r2b4_17mM_st$...3[(1:5)] 
b_r2b5 <- raw_r2b4_17mM_st$`607 mg/L`[(1:5)] 
c_r2b5 <- raw_r2b4_17mM_st$...3[(8:16)]
d_r2b5 <- raw_r2b4_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b5 <- cbind.fill(a_r2b5, b_r2b5, c_r2b5, d_r2b5, fill = NA)

#rename columns in matrix
colnames(mx_r2b5) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b5 <- data.frame(mx_r2b5) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b5 <- df_1_r2b5 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b5_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 17, n_max = 47, range = "H52:J68")#note: no measurement data available

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b5 <- raw_r2b5_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b5)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b5[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))

####No measured bulk standard data for rep_batch r2b5


##17.1mM Cl standards data
raw_r2b6_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 19, n_max = 53, range = "A57:E74")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r2b6 <- raw_r2b6_17mM_st$...3[(1:5)] 
b_r2b6 <- raw_r2b6_17mM_st$`607 mg/L`[(1:5)] 
c_r2b6 <- raw_r2b6_17mM_st$...3[(8:16)]
d_r2b6 <- raw_r2b6_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r2b6 <- cbind.fill(a_r2b6, b_r2b6, c_r2b6, d_r2b6, fill = NA)

#rename columns in matrix
colnames(mx_r2b6) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r2b6 <- data.frame(mx_r2b6) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r2b6 <- df_1_r2b6 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r2b6_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP2.xlsx", sheet = 19, n_max = 53, range = "H57:J68")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r2b6 <- raw_r2b6_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r2b6)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r2b6[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r3b1_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 8, n_max = 44, range = "F50:J65")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b1 <- raw_r3b1_17mM_st$...3[(1:5)] 
b_r3b1 <- raw_r3b1_17mM_st$`607 mg/L`[(1:5)] 
c_r3b1 <- raw_r3b1_17mM_st$...3[(8:15)]
d_r3b1 <- raw_r3b1_17mM_st$`607 mg/L`[(8:15)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b1 <- cbind.fill(a_r3b1, b_r3b1, c_r3b1, d_r3b1, fill = NA)

#rename columns in matrix
colnames(mx_r3b1) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b1 <- data.frame(mx_r3b1) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b1 <- df_1_r3b1 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b1_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 8, n_max = 44, range = "M50:O59")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b1 <- raw_r3b1_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b1)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b1[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r3b2_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 9, n_max = 43, range = "F48:J62")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b2 <- raw_r3b2_17mM_st$...3[(1:5)] 
b_r3b2 <- raw_r3b2_17mM_st$`607 mg/L`[(1:5)] 
c_r3b2 <- raw_r3b2_17mM_st$...3[(8:13)]
d_r3b2 <- raw_r3b2_17mM_st$`607 mg/L`[(8:13)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b2 <- cbind.fill(a_r3b2, b_r3b2, c_r3b2, d_r3b2, fill = NA)

#rename columns in matrix
colnames(mx_r3b2) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b2 <- data.frame(mx_r3b2) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b2 <- df_1_r3b2 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b2_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 9, n_max = 43, range = "M48:O58")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b2 <- raw_r3b2_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b2)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b2[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))



##17.1mM Cl standards data
raw_r3b3_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 11, n_max = 44, range = "C48:G64")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b3 <- raw_r3b3_17mM_st$...3[(1:5)] 
b_r3b3 <- raw_r3b3_17mM_st$`607 mg/L`[(1:5)] 
c_r3b3 <- raw_r3b3_17mM_st$...3[(8:16)]
d_r3b3 <- raw_r3b3_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b3 <- cbind.fill(a_r3b3, b_r3b3, c_r3b3, d_r3b3, fill = NA)

#rename columns in matrix
colnames(mx_r3b3) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b3 <- data.frame(mx_r3b3) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b3 <- df_1_r3b3 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b3_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 11, n_max = 44, range = "K48:M58")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b3 <- raw_r3b3_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b3)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b3[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##17.1mM Cl standards data
raw_r3b4_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 13, n_max = 48, range = "C53:G69")

#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b4 <- raw_r3b4_17mM_st$...3[(1:5)] 
b_r3b4 <- raw_r3b4_17mM_st$`607 mg/L`[(1:5)] 
c_r3b4 <- raw_r3b4_17mM_st$...3[(8:16)]
d_r3b4 <- raw_r3b4_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b4 <- cbind.fill(a_r3b4, b_r3b4, c_r3b4, d_r3b4, fill = NA)

#rename columns in matrix
colnames(mx_r3b4) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b4 <- data.frame(mx_r3b4) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b4 <- df_1_r3b4 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b4_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 13, n_max = 48, range = "J53:L63")

#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b4 <- raw_r3b4_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b4)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b4[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))




##17.1mM Cl standards data
raw_r3b5_17mM_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 17, n_max = 45, range = "B54:E69")


#reformat standards data 
#extract vectors with relevant range of values from table as vectors and convert to numeric
a_r3b5 <- raw_r3b5_17mM_st$...2[(1:5)] 
b_r3b5 <- raw_r3b5_17mM_st$`607 mg/L`[(1:5)] 
c_r3b5 <- raw_r3b5_17mM_st$...2[(8:15)]
d_r3b5 <- raw_r3b5_17mM_st$`607 mg/L`[(8:16)]

#combine vectors to create a matrix and fill in missing values with NA. Done as vectors are of different length
mx_r3b5 <- cbind.fill(a_r3b5, b_r3b5, c_r3b5, d_r3b5, fill = NA)

#rename columns in matrix
colnames(mx_r3b5) <- c("preassay_blank", "preassay_17mM_standard", "intra_assay_blank", "intra_assay_17mM_standard") 

#convert matrix to a data frame. Note: column naming problem if I try to use tibble()
#subtract blanks from standard values, tidy and filter for adjusted data
df_1_r3b5 <- data.frame(mx_r3b5) %>% 
  mutate("preassay_standard_adj" = preassay_17mM_standard - preassay_blank, 
         "intra_assay_standard_adj" = intra_assay_17mM_standard - intra_assay_blank) %>%
  gather(key = "standard", value = "reading_adj", na.rm=TRUE) %>% 
  filter(str_detect(standard, "preassay_standard_adj|intra_assay_standard_adj"))

#calculate conversion factor for Cl% dry wt calc in chloride_data and bulk_standards 
cf_r3b5 <- df_1_r3b5 %>%
  filter(standard == "intra_assay_standard_adj") %>%
  summarise(conv_factor = 17.1 / mean(reading_adj))


###bulk grapevine data
raw_r3b5_grape_st <- read_xlsx("Data/3&4. 0192 barcodes (LAM)-BIOMASS.REP3.DB_SH.xlsx", sheet = 17, n_max = 45, range = "H54:J65")


#reformat bulk standards data 
#rename columns in dataframe, adjust bulk standard reading & calc Cl% dry weight
bulk_r3b5 <- raw_r3b5_grape_st %>% 
  dplyr::rename(intra_assay_bulk_vial = `...1`, intra_assay_bulk_weight_mg = wt., intra_assay_bulk_standard_read = read) %>% 
  mutate("intra_assay_bulk_st_adj" = intra_assay_bulk_standard_read - mean(c_r3b5)) %>%
  mutate("Cl_%_dry_weight" = ((intra_assay_bulk_st_adj * cf_r3b5[1,1]) * 35.5)/(intra_assay_bulk_weight_mg * 10)) %>% 
  filter(intra_assay_bulk_st_adj !=is.na(intra_assay_bulk_st_adj))


##Note: r3b5 is last rep_batch for this experiment