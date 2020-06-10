library(pROC)
data = clinical.total
clinical.roc <- roc(ClinActivity ~ CTSUV + TBR_L + TBR_BP + PETVAS3, data = data)
data = imaging.total
imaging.roc <- roc(PETActivity ~ CTSUV + TBR_L + TBR_BP + PETVAS3, data = data)
clinical.coords <- lapply(clinical.roc,
                          coords,
                          x = "best",
                          ret = "threshold",
                          best.method=c("youden"),
                          transpose = FALSE)
imaging.coords <- lapply(imaging.roc,
                         coords,
                         x = "best",
                         ret = "threshold",
                         best.method=c("youden"),
                         transpose = FALSE)
