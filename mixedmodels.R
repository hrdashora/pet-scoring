library(lme4)

clinicalmixedmodels <- function(data){
  # 5 Clinical Decision Logit Models, 1 Fixed Effect & 1 Random Effect Each
  ## CTSUV
  CTSUV_ClinDec_Model <- glmer(ClinActivity ~ CTSUV + (1|MRN),
                               data = data,
                               family = binomial,
                               control = glmerControl(optimizer = "bobyqa"),
                               nAGQ = 10)
  
  ## TBR_L
  TBRL_ClinDec_Model <- glmer(ClinActivity ~ TBR_L + (1|MRN),
                              data = data,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"),
                              nAGQ = 10)
  
  ## TBR_BP
  TBRBP_ClinDec_Model <- glmer(ClinActivity ~ TBR_BP + (1|MRN),
                               data = data,
                               family = binomial,
                               control = glmerControl(optimizer = "bobyqa"),
                               nAGQ = 10)
  
  ## PETVAS3
  PETVAS_ClinDec_Model <- glmer(ClinActivity ~ PETVAS3 + (1|MRN),
                                data = data,
                                family = binomial,
                                control = glmerControl(optimizer = "bobyqa"),
                                na.action = na.exclude,
                                nAGQ = 10)
  
  output <- list(CTSUV_ClinDec_Model,TBRL_ClinDec_Model,TBRBP_ClinDec_Model,PETVAS_ClinDec_Model)
  
  return(output)
}

petmixedmodels <- function(data){
  # 5 PET Interpretation Logit Models, 1 Fixed Effect & 1 Random Effect Each
  ## CTSUV
  CTSUV_PETInterp_Model <- glmer(PETActivity ~ CTSUV + (1|MRN),
                                 data = data,
                                 family = binomial,
                                 control = glmerControl(optimizer = "bobyqa"),
                                 nAGQ = 10)
  
  ## TBR_L
  TBRL_PETInterp_Model <- glmer(PETActivity ~ TBR_L + (1|MRN),
                                data = data,
                                family = binomial,
                                control = glmerControl(optimizer = "bobyqa"),
                                nAGQ = 10)
  
  ## TBR_BP
  TBRBP_PETInterp_Model <- glmer(PETActivity ~ TBR_BP + (1|MRN),
                                 data = data,
                                 family = binomial,
                                 control = glmerControl(optimizer = "bobyqa"),
                                 nAGQ = 10)
  
  ## PETVAS3
  PETVAS_PETInterp_Model <- glmer(PETActivity ~ PETVAS3 + (1|MRN),
                                  data = data,
                                  family = binomial,
                                  control = glmerControl(optimizer = "bobyqa"),
                                  nAGQ = 10)
  
  output <- list(CTSUV_PETInterp_Model,TBRL_PETInterp_Model,TBRBP_PETInterp_Model,PETVAS_PETInterp_Model)
  
  return(output)
}
