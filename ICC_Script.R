# Computing ICCs in R
library(irr)

# Joel-Armin ICC
SUVScorecard <- read.csv("data/JoelJoel_ICC_FormattedData.csv", header=TRUE)
SUVRatings <- SUVScorecard[,2:3]
SUVICC <- icc(SUVRatings, model="twoway", type="consistency", unit="single")
print(SUVICC)

# Mark-Mark ICC
PETVASScorecard <- read.csv("data/MarkMark_ICC_FormattedData.csv", header=TRUE)
PETVASRatings <- PETVASScorecard[,2:3]
PETVASICC <- icc(PETVASRatings, model="twoway", type="consistency", unit="single")
print(PETVASICC)
