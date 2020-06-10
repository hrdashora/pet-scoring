source("mixedmodels.R")

clinicalcrossval <- function(clinicaldata){
  names <- c("CTSUV","TBR_L","TBR_BP","PETVAS3")
  clinacc = matrix(data=NA,nrow=10,ncol=4)
  set.seed(11111)
  clinshuffle <- clinicaldata[sample(nrow(clinicaldata)),]
  clinfolds <- cut(seq(1,nrow(clinshuffle)),breaks=10,labels=FALSE)
  for(i in 1:10){
    clintestIndexes <- which(clinfolds==i,arr.ind=TRUE)
    clintestData <- clinshuffle[clintestIndexes,]
    clintrainData <- clinshuffle[-clintestIndexes,]
    clinmodels <- clinicalmixedmodels(clintrainData)
    clinpred <- lapply(clinmodels, predict, newdata = clintestData, allow.new.levels = T, type="response", na.action=na.omit)
    acc <- numeric(4)
    for(j in 1:4){
      name <- names[j]
      clintestData <- clintestData[!is.na(clintestData[name]),]
      clinreal <- as.numeric(clintestData$ClinActivity)-1
      acc[j] <- mean(as.numeric(clinpred[[j]]>0.5) == clinreal)
    }
    clinacc[i,] <- acc
  }
  clinmean <- colMeans(clinacc, na.rm = TRUE)
  clinmed <- apply(clinacc, 2, median)

  return(clinmean)
}

imagingcrossval <- function(imagingdata){
  names <- c("CTSUV","TBR_L","TBR_BP","PETVAS3")
  imgacc = matrix(data=NA,nrow=10,ncol=4)
  set.seed(11111)
  imgshuffle <- imagingdata[sample(nrow(imagingdata)),]
  imgfolds <- cut(seq(1,nrow(imgshuffle)),breaks=10,labels=FALSE)
  for(i in 1:10){
    imgtestIndexes <- which(imgfolds==i,arr.ind=TRUE)
    imgtestData <- imgshuffle[imgtestIndexes,]
    imgtrainData <- imgshuffle[-imgtestIndexes,]
    imgmodels <- petmixedmodels(imgtrainData)
    imgpred <- lapply(imgmodels, predict, newdata = imgtestData, allow.new.levels = T, type="response", na.action=na.omit)
    acc <- numeric(4)
    for(j in 1:4){
      name <- names[j]
      imgtestData <- imgtestData[!is.na(imgtestData[name]),]
      imgreal <- as.numeric(imgtestData$PETActivity)
      acc[j] <- mean(as.numeric(imgpred[[j]]>0.5) == imgreal)
    }
    imgacc[i,] <- acc
  }
  imgmean <- colMeans(imgacc, na.rm = TRUE)
  imgmed <- apply(imgacc, 2 , median)
  
  return(imgmean)
}