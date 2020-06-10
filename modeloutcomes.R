library(lme4)
library(dplyr)

clinicaloutcomes <- function(models){
  CTSUV_ClinDec_Model <- models[[1]]
  TBRL_ClinDec_Model <- models[[2]]
  TBRBP_ClinDec_Model <- models[[3]]
  PETVAS_ClinDec_Model <- models[[4]]
  # CTSUV
  CTSUV_ClinDec_SE <- sqrt(diag(vcov(CTSUV_ClinDec_Model)))
  CTSUV_ClinDec_OR <- exp(cbind(Est = fixef(CTSUV_ClinDec_Model),
                                LL = fixef(CTSUV_ClinDec_Model) - 1.96 * CTSUV_ClinDec_SE,
                                UL = fixef(CTSUV_ClinDec_Model) + 1.96 * CTSUV_ClinDec_SE))
  # TBR_L
  TBRL_ClinDec_SE <- sqrt(diag(vcov(TBRL_ClinDec_Model)))
  TBRL_ClinDec_OR <- exp(cbind(Est = fixef(TBRL_ClinDec_Model),
                               LL = fixef(TBRL_ClinDec_Model) - 1.96 * TBRL_ClinDec_SE,
                               UL = fixef(TBRL_ClinDec_Model) + 1.96 * TBRL_ClinDec_SE))
  # TBR_BP
  TBRBP_ClinDec_SE <- sqrt(diag(vcov(TBRBP_ClinDec_Model)))
  TBRBP_ClinDec_OR <- exp(cbind(Est = fixef(TBRBP_ClinDec_Model),
                                LL = fixef(TBRBP_ClinDec_Model) - 1.96 * TBRBP_ClinDec_SE,
                                UL = fixef(TBRBP_ClinDec_Model) + 1.96 * TBRBP_ClinDec_SE))
  # PETVAS3
  PETVAS_ClinDec_SE <- sqrt(diag(vcov(PETVAS_ClinDec_Model)))
  PETVAS_ClinDec_OR <- exp(cbind(Est = fixef(PETVAS_ClinDec_Model),
                                 LL = fixef(PETVAS_ClinDec_Model) - 1.96 * PETVAS_ClinDec_SE,
                                 UL = fixef(PETVAS_ClinDec_Model) + 1.96 * PETVAS_ClinDec_SE))
  
  output <- list(CTSUV_ClinDec_SE,CTSUV_ClinDec_OR,
                 TBRL_ClinDec_SE,TBRL_ClinDec_OR,
                 TBRBP_ClinDec_SE,TBRBP_ClinDec_OR,
                 PETVAS_ClinDec_SE,PETVAS_ClinDec_OR)
  
  return(output)
}

petoutcomes <- function(models){
  CTSUV_PETInterp_Model <- models[[1]]
  TBRL_PETInterp_Model <- models[[2]]
  TBRBP_PETInterp_Model <- models[[3]]
  PETVAS_PETInterp_Model <- models[[4]]
  # CTSUV
  CTSUV_PETInterp_SE <- sqrt(diag(vcov(CTSUV_PETInterp_Model)))
  CTSUV_PETInterp_OR <- exp(cbind(Est = fixef(CTSUV_PETInterp_Model),
                                  LL = fixef(CTSUV_PETInterp_Model) - 1.96 * CTSUV_PETInterp_SE,
                                  UL = fixef(CTSUV_PETInterp_Model) + 1.96 * CTSUV_PETInterp_SE))
  # TBR_L
  TBRL_PETInterp_SE <- sqrt(diag(vcov(TBRL_PETInterp_Model)))
  TBRL_PETInterp_OR <- exp(cbind(Est = fixef(TBRL_PETInterp_Model),
                                 LL = fixef(TBRL_PETInterp_Model) - 1.96 * TBRL_PETInterp_SE,
                                 UL = fixef(TBRL_PETInterp_Model) + 1.96 * TBRL_PETInterp_SE))
  # TBR_BP
  TBRBP_PETInterp_SE <- sqrt(diag(vcov(TBRBP_PETInterp_Model)))
  TBRBP_PETInterp_OR <- exp(cbind(Est = fixef(TBRBP_PETInterp_Model),
                                  LL = fixef(TBRBP_PETInterp_Model) - 1.96 * TBRBP_PETInterp_SE,
                                  UL = fixef(TBRBP_PETInterp_Model) + 1.96 * TBRBP_PETInterp_SE))
  # PETVAS3
  PETVAS_PETInterp_SE <- sqrt(diag(vcov(PETVAS_PETInterp_Model)))
  PETVAS_PETInterp_OR <- exp(cbind(Est = fixef(PETVAS_PETInterp_Model),
                                   LL = fixef(PETVAS_PETInterp_Model) - 1.96 * PETVAS_PETInterp_SE,
                                   UL = fixef(PETVAS_PETInterp_Model) + 1.96 * PETVAS_PETInterp_SE))
  
  output <- list(CTSUV_PETInterp_SE,CTSUV_PETInterp_OR,
                 TBRL_PETInterp_SE,TBRL_PETInterp_OR,
                 TBRBP_PETInterp_SE,TBRBP_PETInterp_OR,
                 PETVAS_PETInterp_SE,PETVAS_PETInterp_OR)
  
  return(output)
}
