# This is the 'main' script in the PET assessment analysis codebase
# All packages and helper functions will be imported/called from this script

# Import packages
library(dplyr)

# Import raw data from '.csv' file
source("rawdataimport.R")
filename <- "data/RawData.csv"
impdata <- rawdataimport(filename)

# Calculate population demographics
raw <- impdata[[3]]
total_n <- length(unique(raw$MRN))
calc <- raw %>%
  group_by(Diagnosis) %>%
  summarise(n=n(),
            bmi_m = mean(BMI, na.rm = T), bmi_sd = sd(BMI, na.rm = T),
            age_m = mean(Age, na.rm = T), age_sd = sd(Age, na.rm = T),
            crp_m = mean(CRP, na.rm = T), crp_sd = sd(CRP, na.rm = T),
            esr_m = mean(ESR, na.rm = T), esr_sd = sd(ESR, na.rm = T),
            pred_m = mean(`Daily prednisone`, na.rm = T), pred_sd = sd(`Daily prednisone`, na.rm = T))

calcall <- raw %>%
  summarise(n=n(),
            bmi_m = mean(BMI, na.rm = T), bmi_sd = sd(BMI, na.rm = T),
            age_m = mean(Age, na.rm = T), age_sd = sd(Age, na.rm = T),
            crp_m = mean(CRP, na.rm = T), crp_sd = sd(CRP, na.rm = T),
            esr_m = mean(ESR, na.rm = T), esr_sd = sd(ESR, na.rm = T),
            pred_m = mean(`Daily prednisone`, na.rm = T), pred_sd = sd(`Daily prednisone`, na.rm = T))
  
freq <- raw %>% group_by(Diagnosis) %>% count(MRN)
counts_1 <- freq %>% filter(n == 1) %>% summarise(n=n())
counts_2 <- freq %>% filter(n == 2) %>% summarise(n=n())
counts_3 <- freq %>% filter(n >= 3) %>% summarise(n=n())
counts_all <- freq %>% summarise(n=n())

# Format raw data for GLMM analysis
source("dataselection.R")
clin <- impdata[[1]]
img <- impdata[[2]]
data <- dataselection(clin,img)

# Generate summary statistics
source("descriptive.R")
stats <- descriptive(clin, img)

# Contruct 'glmer' objects
source("mixedmodels.R")
clinicaldata <- data[[1]]
imagingdata <- data[[2]]
clinmodels <- clinicalmixedmodels(clinicaldata)
petmodels <- petmixedmodels(imagingdata)

# Calculate model outcomes
source("modeloutcomes.R")
clinout <- clinicaloutcomes(clinmodels)
petout <- petoutcomes(petmodels)

# 10-fold cross validation
source("kfolds.R")
clinmean <- clinicalcrossval(clinicaldata)
imgmean <- imagingcrossval(imagingdata)

# Track multi-visit max PETVAS patients
long <- raw[!is.na(raw$"CT PETVAS 3s"),]
capped <- raw %>% filter(`CT PETVAS 3s` >= 27) %>% select(MRN)
caplist <- unlist(unique(capped))
longtrack <- raw %>% filter(MRN %in% caplist) %>% select(`Last Name`, `First Name`,`Scan Date`, MRN, `CT Artery`,`CT TBR Liver`,`CT TBR Blood Pool`,`CT PETVAS 3s`)
colnames(longtrack) <- c("Last Name","First Name","MRN","CTSUV","TBR_L","TBR_BP","PETVAS3")
repeated <- longtrack %>% add_count(MRN) %>% filter(n > 1) %>% select(-n) %>% write.csv(.,file = "piping.csv")
