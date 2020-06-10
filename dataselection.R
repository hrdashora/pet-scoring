library(dplyr)

dataselection <- function(clin, img){
  # Select data tables for exposure and outcome variables
  ### Patient Level Continuous Predictors: CRP, CT Artery, CT PETVAS 3s, Daily prednisone
  ### Random Intercept: MRN
  ### Outcomes: Disease Activity, CT PET Active
  ClinDec <- select(clin, MRN, `CT Artery`,`CT TBR Liver`,`CT TBR Blood Pool`,`CT TBR Spleen`,
                    CRP, `Disease Activity`,`CT PETVAS 3s`,`Daily prednisone`)
  ClinDec$`Disease Activity` <- factor(ClinDec$`Disease Activity`, levels = c("Remission","Active"))
  colnames(ClinDec) <- c("MRN","CTSUV","TBR_L","TBR_BP","TBR_S","CRP","ClinActivity","PETVAS3","DailyPred")
  PETInterp <- select(img, MRN, `CT Artery`,`CT TBR Liver`,`CT TBR Blood Pool`,`CT TBR Spleen`,
                      CRP, `CT PET Active`,`CT PETVAS 3s`,`Daily prednisone`)
  colnames(PETInterp) <- c("MRN","CTSUV","TBR_L","TBR_BP","TBR_S","CRP","PETActivity","PETVAS3","DailyPred")
  
  # # Split data into training and testing sets
  # set.seed(101)
  # clinical.sample <- sample.int(n = nrow(ClinDec), size = floor(0.75*nrow(ClinDec)), replace = F)
  # clinical.train <- ClinDec[clinical.sample, ]
  # clinical.test <- ClinDec[-clinical.sample, ]
  # clinical.total <- ClinDec
  # set.seed(101)
  # imaging.sample <- sample.int(n = nrow(PETInterp), size = floor(0.75*nrow(PETInterp)), replace = F)
  # imaging.train <- PETInterp[imaging.sample, ]
  # imaging.test <- PETInterp[-imaging.sample, ]
  # imaging.total <- PETInterp
  
  output <- list(ClinDec, PETInterp)
  
  return(output)
}
