library(dplyr)
library(readr)
source("rawdataimport.R")

filename <- "data/RawData.csv"
data <- rawdataimport(filename)
importraw <- data[[3]][!is.na(data[[3]]$"CT PETVAS 3s"),]

ceiling <- importraw %>% group_by(`CT PETVAS 3s` >= 27) %>% summarise(mean = mean(`CT Artery`),
                                dev = sd(`CT Artery`),
                                num = n(),
                                min = min(`CT Artery`),
                                q1 = quantile(`CT Artery`, probs = 0.25, na.rm = TRUE),
                                q2 = quantile(`CT Artery`, probs = 0.50, na.rm = TRUE),
                                q3 = quantile(`CT Artery`, probs = 0.75, na.rm = TRUE),
                                max = max(`CT Artery`)
                                )
nonceiling <- importraw  %>% summarise(mean = mean(`CT Artery`, na.rm = TRUE),
                                       dev = sd(`CT Artery`, na.rm = TRUE),
                                       num = n(),
                                       min = min(`CT Artery`, na.rm = TRUE),
                                       q1 = quantile(`CT Artery`, probs = 0.25, na.rm = TRUE),
                                       q2 = quantile(`CT Artery`, probs = 0.50, na.rm = TRUE),
                                       q3 = quantile(`CT Artery`, probs = 0.75, na.rm = TRUE),
                                       max = max(`CT Artery`, na.rm = TRUE)
                                       )
capped <- importraw %>% filter(`CT PETVAS 3s` >= 27) %>% select(MRN)
caplist <- unlist(unique(capped))
longtrack <- importraw %>% filter(MRN %in% caplist)


# vals <- data.frame(max=rep(NA,226),all=numeric(226))
# vals$max[1:21] <- capped$`CT Artery`
# vals$all <- importraw$`CT Artery`
# path <- getwd()
# write.table(vals,sep = "  ", row.names = F)
