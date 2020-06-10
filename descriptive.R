library(dplyr)

descriptive <- function(clin, img){
  # Descriptive Statistics
  clinsum <- clin %>%
    group_by(`Disease Activity`) %>%
    summarise(n = length(`CT Artery`),
              SUVavg = mean(`CT Artery`), SUVsd = sd(`CT Artery`), SUVsem = (sd(`CT Artery`)/sqrt(dplyr::n())),
              TBRLavg = mean(`CT TBR Liver`), TBRLsd = sd(`CT TBR Liver`), TBRLsem = (sd(`CT TBR Liver`)/sqrt(dplyr::n())),
              TBRBPavg = mean(`CT TBR Blood Pool`), TBRBPsd = sd(`CT TBR Blood Pool`), TBRBPsem = (sd(`CT TBR Blood Pool`)/sqrt(dplyr::n())),
              PETVASavg = mean(`CT PETVAS 3s`, na.rm = T), PETVASsd = sd(`CT PETVAS 3s`, na.rm = T), PETVASsem = (PETVASsd/sqrt(dplyr::n()))
    )
  
  petsum <- img %>%
    group_by(`CT PET Active`) %>%
    summarise(n = length(`CT Artery`),
              SUVavg = mean(`CT Artery`), SUVsd = sd(`CT Artery`), SUVsem = (sd(`CT Artery`)/sqrt(dplyr::n())),
              TBRLavg = mean(`CT TBR Liver`), TBRLsd = sd(`CT TBR Liver`), TBRLsem = (sd(`CT TBR Liver`)/sqrt(dplyr::n())),
              TBRBPavg = mean(`CT TBR Blood Pool`), TBRBPsd = sd(`CT TBR Blood Pool`), TBRBPsem = (sd(`CT TBR Blood Pool`)/sqrt(dplyr::n())),
              PETVASavg = mean(`CT PETVAS 3s`, na.rm = T), PETVASsd = sd(`CT PETVAS 3s`, na.rm = T), PETVASsem = (PETVASsd/sqrt(dplyr::n()))
    )
  
  output <- list(clinsum, petsum)
  
  return(output)
}
