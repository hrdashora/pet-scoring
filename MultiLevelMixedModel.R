ClinDec_MultiModel <- glmer(ClinActivity ~ PETVAS3 + CRP + (1|MRN),
                            data = clinical.train,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"),
                            nAGQ = 10)
SE <- sqrt(diag(vcov(ClinDec_MultiModel)))
OR <- exp(cbind(Est = fixef(ClinDec_MultiModel),
                 LL = fixef(ClinDec_MultiModel) - 1.96 * SE,
                 UL = fixef(ClinDec_MultiModel) + 1.96 * SE))
clinical.test$multi_prob <- predict(ClinDec_MultiModel,
                                   newdata = clinical.test,
                                   allow.new.levels = T,
                                   type = "response")
clinical.test <- clinical.test %>% mutate(multi_pred = 1*(multi_prob > 0.65) + 0,
                                          multi_bin = 1*(ClinActivity == "Active") + 0)
clinical.test <- clinical.test %>% mutate(multi_acc = 1*(multi_pred == multi_bin))
acc <- sum(clinical.test$multi_acc, na.rm = T)/sum(!is.na(clinical.test$multi_acc))