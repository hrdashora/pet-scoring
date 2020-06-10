library(readr)
library(dplyr)

rawdataimport <- function(filename){
  importraw <- read_csv(filename)
  morgancarolyn.mrn <- 0915622 # Missing MRN in Raw Data
  importraw[130,3] <- morgancarolyn.mrn
  
  # Split raw data into clinical interpretation and PET interpretation
  clinical.all <- filter(importraw, !is.na(`Disease Activity`))
  imaging.all <- filter(importraw, !is.na(`CT PET Active`))
  # clinical.all.active <- filter(clinical.all, `Disease Activity` == "Active")
  # clinical.all.remission <- filter(clinical.all, `Disease Activity` == "Remission")
  # imaging.all.active <- filter(imaging.all, `CT PET Active` == 1)
  # imaging.all.inactive <- filter(imaging.all, `CT PET Active` == 0)
  # clinical.all.GCA <- filter(clinical.all, `Diagnosis` == "GCA")
  # clinical.all.TAK <- filter(clinical.all, `Diagnosis` == "TAK")
  # imaging.all.GCA <- filter(imaging.all, `Diagnosis` == "GCA")
  # imaging.all.TAK <- filter(imaging.all, `Diagnosis` == "TAK")
  
  output <- list(clinical.all, imaging.all, importraw)
  
  return(output)
}


