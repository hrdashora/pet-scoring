# DATA DESCRIPTION --------------------------------------------------------
# # Tests for normality -- predictor data is NON-normal
(clin.ctsuv.den <- ggplot(clinical.all, aes(x = `CT Artery`)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(`CT Artery`)),
             color = "blue", linetype = "dashed", size = 0.5))
(clin.ctsuv.log.den <- ggplot(clinical.all, aes(x = log(`CT Artery`))) +
  geom_density() +
  geom_vline(aes(xintercept = mean(log(`CT Artery`))),
             color = "blue", linetype = "dashed", size = 0.5))
# 
# clinical.all.qq <- ggplot(clinical.all, aes(sample = log(`CT Artery`))) +
#   stat_qq()
# 
# (clinical.all.CTSUVshap <- shapiro.test(log(clinical.all$`CT Artery`)))
# (clinical.all.TBRLshap <- shapiro.test(log(clinical.all$`CT TBR Liver`)))
# (clinical.all.TBRBPshap <- shapiro.test(log(clinical.all$`CT TBR Blood Pool`)))
# (clinical.all.TBRSshap <- shapiro.test(log(clinical.all$`CT TBR Spleen`)))
# 
# # Tests for correlation
# clinical.all.corr <- ggpairs(clinical.all[,c("ESR","CRP",
#                         "CT Artery","CT PETVAS 3s",
#                         "Daily prednisone")]) # ESR/CRP and CT Artery/CT PETVAS 3s correlations
# tmp <- melt(clinical.all[, c("Disease Activity", "CRP", "CT Artery", "Daily prednisone")],
#             id.vars="Disease Activity")
# clinical.all.bin <- ggplot(tmp, aes(factor(`Disease Activity`), y = value, fill=factor(`Disease Activity`))) +
#   geom_boxplot() +
#   facet_wrap(~variable, scales="free_y")






# PREDICTED PROBABILITIES -------------------------------------------------
# Generate predicted probabilities of clinical outcome
tmpdat <- ClinDec
jvalues1 <- with(ClinDec, seq(from = min(CTSUV), to = max(CTSUV), length.out = 100))
jvalues2 <- with(ClinDec, seq(from = min(TBR_L), to = max(TBR_L), length.out = 100))
jvalues3 <- with(ClinDec, seq(from = min(TBR_BP), to = max(TBR_BP), length.out = 100))
jvalues4 <- with(ClinDec, seq(from = min(TBR_S, na.rm = T), to = max(TBR_S, na.rm = T), length.out = 100))
jvalues5 <- with(ClinDec, seq(from = min(PETVAS3, na.rm = T), to = max(PETVAS3, na.rm = T), length.out = 100))
pp1 <- lapply(jvalues1, function(j) {
  tmpdat$CTSUV <- j
  predict(CTSUV_ClinDec_Model, newdata = tmpdat, type = "response")
})
plotdat1 <- t(sapply(pp1, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
pp2 <- lapply(jvalues2, function(j) {
  tmpdat$TBR_L <- j
  predict(TBRL_ClinDec_Model, newdata = tmpdat, type = "response")
})
plotdat2 <- t(sapply(pp2, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
pp3 <- lapply(jvalues3, function(j) {
  tmpdat$TBR_BP <- j
  predict(TBRBP_ClinDec_Model, newdata = tmpdat, type = "response")
})
plotdat3 <- t(sapply(pp3, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
pp4 <- lapply(jvalues4, function(j) {
  tmpdat$TBR_S <- j
  predict(TBRS_ClinDec_Model, newdata = tmpdat, type = "response", allow.new.levels = T)
})
plotdat4 <- t(sapply(pp4, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))
pp5 <- lapply(jvalues5, function(j) {
  tmpdat$PETVAS3 <- j
  predict(PETVAS_ClinDec_Model, newdata = tmpdat, type = "response", allow.new.levels = T)
})
plotdat5 <- t(sapply(pp5, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

# Plot predicted probabilities
plotdat1 <- as.data.frame(cbind(plotdat1,jvalues1))
colnames(plotdat1) <- c("Predict", "Lower","Upper","CTSUV")
plotdat2 <- as.data.frame(cbind(plotdat2,jvalues2))
colnames(plotdat2) <- c("Predict", "Lower","Upper","TBR_L")
plotdat3 <- as.data.frame(cbind(plotdat3,jvalues3))
colnames(plotdat3) <- c("Predict", "Lower","Upper","TBR_BP")
plotdat4 <- as.data.frame(cbind(plotdat4,jvalues4))
colnames(plotdat4) <- c("Predict", "Lower","Upper","TBR_S")
plotdat5 <- as.data.frame(cbind(plotdat5,jvalues5))
colnames(plotdat5) <- c("Predict", "Lower","Upper","PETVAS3")


ctsuvplot <- ggplot(plotdat1, aes(x = CTSUV, y = Predict)) + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.25, fill = "blue") +
  geom_line(color = "blue", size = 2) +
  ylim(c(0, 1)) + labs(x = "Arterial SUV", y = "Probability")
tbrlplot <- ggplot(plotdat2, aes(x = TBR_L, y = Predict)) + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.25, fill = "green") +
  geom_line(color = "green", size = 2) +
  ylim(c(0, 1)) + labs(x = "TBR Liver", y = "Probability")
tbrbpplot <- ggplot(plotdat3, aes(x = TBR_BP, y = Predict)) + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.25, fill = "red") +
  geom_line(color = "red", size = 2) +
  ylim(c(0, 1)) + labs(x = "TBR Blood Pool", y = "Probability")
tbrsplot <- ggplot(plotdat4, aes(x = TBR_S, y = Predict)) + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.25, fill = "orange") +
  geom_line(color = "orange", size = 2) +
  ylim(c(0, 1)) + labs(x = "TBR Spleen", y = "Probability")
petvasplot <- ggplot(plotdat5, aes(x = PETVAS3, y = Predict)) + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.25, fill = "purple") +
  geom_line(color = "purple", size = 2) +
  ylim(c(0, 1)) + labs(x = "PETVAS", y = "Probability")

problayout <- ggarrange(ctsuvplot, tbrlplot, tbrbpplot, tbrsplot, petvasplot,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

# # Predicted probabilities of PET interpretation
# tmpdat <- PETInterp
# jvalues <- with(CTSUV_PETInterp, seq(from = min(CTSUV), to = max(CTSUV), length.out = 100))
# pp <- lapply(jvalues, function(j) {
#   tmpdat$CTSUV <- j
#   predict(CTSUV_PETInterp_Model, newdata = tmpdat, type = "response")
# })
# plotdat <- t(sapply(pp, function(x) {
#   c(M = mean(x), quantile(x, c(i0.25, 0.75)))
# }))
# plotdat <- as.data.frame(cbind(plotdat,jvalues))
# colnames(plotdat) <- c("PredictedProbability", "Lower","Upper","CTSUV")
# ggplot(plotdat, aes(x = CTSUV, y = PredictedProbability)) + 
#   geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.25) +
#   geom_line(size = 2) +
#   ylim(c(0, 1))


# ROC CURVES --------------------------------------------------------------

# mydata <- ClinDec
# prob <- predict(CTSUV_ClinDec_Model,type=c("response"))
# mydata$prob <- prob
# roc1 <- roc(ClinActivity ~ prob, data = mydata)
# prob <- predict(TBRL_ClinDec_Model,type=c("response"))
# mydata$prob <- prob
# roc2 <- roc(ClinActivity ~ prob, data = mydata)
# prob <- predict(TBRBP_ClinDec_Model,type=c("response"))
# mydata$prob <- prob
# roc3 <- roc(ClinActivity ~ prob, data = mydata)
# prob <- predict(TBRS_ClinDec_Model,type=c("response"))
# mydata$prob <- prob
# roc4 <- roc(ClinActivity ~ prob, data = mydata)
# prob <- predict(PETVAS_ClinDec_Model,type=c("response"))
# mydata$prob <- prob
# roc5 <- roc(ClinActivity ~ prob, data = mydata)
# roc <- ggroc(data = list(roc1,roc2,roc3,roc4,roc5), legacy.axes = T, aes = c("color")) +
#   labs(x = "1 - Specificity", y = "Sensitivity", name = "TEST") +
#   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
#   scale_color_discrete(name = "Score Type", labels = c("SUV Artery","TBR Liver","TBR Blood Pool","TBR Spleen","PETVAS 3"))


# CLINICAL CSV EXPORT --------------------------------------------------------------

rocdat_active <- select(clinical.all.active,`CT Artery`,`CT TBR Liver`,
                        `CT TBR Blood Pool`,`CT TBR Spleen`,`CT PETVAS 3s`,
                        `Disease Activity`,`Diagnosis`)
rocdat_remission <- select(clinical.all.remission,`CT Artery`,`CT TBR Liver`,
                           `CT TBR Blood Pool`,`CT TBR Spleen`,`CT PETVAS 3s`,
                           `Disease Activity`,`Diagnosis`)
rocdat_active_GCA <- rocdat_active %>%
  filter(`Diagnosis` == "GCA")
rocdat_active_TAK <- rocdat_active %>%
  filter(`Diagnosis` == "TAK")
rocdat_remission_GCA <- rocdat_remission %>%
  filter(`Diagnosis` == "GCA")
rocdat_remission_TAK <- rocdat_remission %>%
  filter(`Diagnosis` == "TAK")
write.csv(rocdat_active, "ROC_data_active.csv")
write.csv(rocdat_active_GCA, "ROC_data_active_GCA.csv")
write.csv(rocdat_active_TAK, "ROC_data_active_TAK.csv")
write.csv(rocdat_remission, "ROC_data_remission.csv")
write.csv(rocdat_remission_GCA, "ROC_data_remission_GCA.csv")
write.csv(rocdat_remission_TAK, "ROC_data_remission_TAK.csv")


# PET CSV EXPORT ----------------------------------------------------------

rocdat_petactive <- select(imaging.all.active,`CT Artery`,`CT TBR Liver`,
                        `CT TBR Blood Pool`,`CT TBR Spleen`,`CT PETVAS 3s`,
                        `CT PET Active`,`Diagnosis`)
rocdat_petinactive <- select(imaging.all.inactive,`CT Artery`,`CT TBR Liver`,
                           `CT TBR Blood Pool`,`CT TBR Spleen`,`CT PETVAS 3s`,
                           `CT PET Active`,`Diagnosis`)
rocdat_petactive_GCA <- rocdat_petactive %>%
  filter(`Diagnosis` == "GCA")
rocdat_petactive_TAK <- rocdat_petactive %>%
  filter(`Diagnosis` == "TAK")
rocdat_petinactive_GCA <- rocdat_petinactive %>%
  filter(`Diagnosis` == "GCA")
rocdat_petinactive_TAK <- rocdat_petinactive %>%
  filter(`Diagnosis` == "TAK")
write.csv(rocdat_petactive, "ROC_data_petactive.csv")
write.csv(rocdat_petactive_GCA, "ROC_data_petactive_GCA.csv")
write.csv(rocdat_petactive_TAK, "ROC_data_petactive_TAK.csv")
write.csv(rocdat_petinactive, "ROC_data_petinactive.csv")
write.csv(rocdat_petinactive_GCA, "ROC_data_petinactive_GCA.csv")
write.csv(rocdat_petinactive_TAK, "ROC_data_petinactive_TAK.csv")


# CORRELATION MATRIX ------------------------------------------------------

corr <- imaging.all %>% select(`CT Artery`,`CT TBR Liver`,`CT TBR Blood Pool`,`CT TBR Spleen`,
                                            `CT PETVAS 3s`, CRP, ESR)
corr.GCA <- imaging.all.GCA %>% select(`CT Artery`,`CT TBR Liver`,`CT TBR Blood Pool`,`CT TBR Spleen`,
                               `CT PETVAS 3s`, CRP, ESR)
corr.TAK <- imaging.all.TAK %>% select(`CT Artery`,`CT TBR Liver`,`CT TBR Blood Pool`,`CT TBR Spleen`,
                               `CT PETVAS 3s`, CRP, ESR)
write.csv(corr, "CorrMatrix.csv")
write.csv(corr.GCA, "CorrMatrix_GCA.csv")
write.csv(corr.TAK, "CorrMatrix_TAK.csv")


# 
# temp <- ClinDec
# kvalues1 <- with(ClinDec, seq(from = min(PETVAS3, na.rm = T), to = max(PETVAS3, na.rm = T), length.out = 100))
# kvalues2 <- with(ClinDec, seq(from = min(CRP, na.rm = T), to = max(CRP, na.rm = T), length.out = 6))
# 
# for (i in length(kvalues2)){
#   temp$CRP <- kvalues2[i]
#   for (j in length(kvalues1)){
#     temp$PETVAS3 <- kvalues1[j]
#     predict(ClinDec_MultiModel, newdata = temp, type = "response")
#   }
# }
# 
# mpp <- outer(kvalues1, function(k1) {
#   temp$PETVAS3 <- k1
#   predict(ClinDec_MultiModel, newdata = temp, type = "response")
# })
# 
# mplotdat <- t(sapply(mpp, function(x) {
#   c(M = mean(x), quantile(x, c(0.25, 0.75)))
# }))
