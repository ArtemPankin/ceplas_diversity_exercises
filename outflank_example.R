install.packages("devtools")
library(devtools)

source("http://bioconductor.org/biocLite.R")
biocLite("qvalue")

# biocLite("BiocUpgrade")
# install.packages("BiocManager")
# library(BiocManager)
# install("qvalue")

install_github("whitlock/OutFLANK")

library(OutFLANK)

### Example ####

### load the SNP marker data ###

SNPdata <- read.table("outflank_example.data", header=F)

## show the first 5 SNPs in the first 6 individuals:
## number of rows = number of individuals
## number of columns  = number of SNP markers

head(SNPdata[,1:5])
nrow(SNPdata)
ncol(SNPdata)

### load the individuals data - which indidual from which population
ind <- read.table("outflank_example.pop", header=F)
ind <- as.character(ind$V1)
head(ind)

### the loci info ###

loci <- read.table("outflank_example.lociinfo", header=T)
head(loci)

locinames <- paste(loci[,1],loci[,2],loci[,3], sep="_")
table(loci$type)

### calculate input matrix (fst and other parameter) ###

FstDataFrame <- MakeDiploidFSTMat(SNPdata,locinames,ind)
FstDataFrame <- data.frame(FstDataFrame,loci)

head(FstDataFrame)

### check the amount of missing data ###

missing_data <- apply(SNPdata,2, function(i){sum(i==9)})
hist(missing_data, breaks=seq(0,100,1))

### plot corrected vs. uncorrected FSTs

plot(FstDataFrame$FST, FstDataFrame$FSTNoCorr, 
     xlim=c(-0.01,0.3), ylim=c(-0.01,0.3),
     abline(0,1))

### let's check what happens if a sample has a lot of missing data ###

SNPdata_missing <- SNPdata
missing <- sample(1:nrow(SNPdata), 200, replace=FALSE)
SNPdata_missing[missing,1] <- 9
FstDataFrame_missing <- MakeDiploidFSTMat(SNPdata_missing,locinames,ind)

plot(FstDataFrame_missing$FST, FstDataFrame_missing$FSTNoCorr, xlim=c(-0.01,0.3), ylim=c(-0.01,0.3), pch=20)

points(FstDataFrame_missing$FST[1], FstDataFrame_missing$FSTNoCorr[1], col="blue", pch=8, cex=1.3)
abline(0,1)

### running OutFLANK on Fst data.frame ###
## Default options: OutFLANK(FstDataFrame, LeftTrimFraction=0.05, RightTrimFraction=0.05, Hmin=0.1, NumberOfSamples, qthreshold=0.05)

OF <- OutFLANK(subset(FstDataFrame, He > 0.2), NumberOfSamples=19, RightTrimFraction = 0.05)

str(OF)

### plot the inferred neutral FST distribution ###

OutFLANKResultsPlotter(OF, withOutliers = TRUE, NoCorr = TRUE,
                       Hmin = 0.1, binwidth = 0.005, Zoom = F, 
                       RightZoomFraction = 0.05, titletext = NULL)

OutFLANKResultsPlotter(OF, withOutliers = TRUE, NoCorr = TRUE,
                       Hmin = 0.1, binwidth = 0.005, Zoom = T, 
                       RightZoomFraction = 0.05, titletext = NULL)

### extract outlier loci ###

OF$results$LocusName[OF$results$OutlierFlag]

### plot Heterozygosity vs. FST to highlight outliers ###

plot(OF$results$He, OF$results$FST, pch=20, col= rgb(0,0,0,0.5), cex=0.5,
     xlab="He", ylab="FST")
points(OF$results$He[OF$results$OutlierFlag], OF$results$FST[OF$results$OutlierFlag], pch=8, col= rgb(0,0,1,1))

### plot the true outliers ###

plot(OF$results$He, OF$results$FST, pch=20, col= rgb(0,0,0,0.5), cex=0.5, xlab="He", ylab="FST")

### Color qtl in orange
points(OF$results$He[OF$results$type=="quanti"], OF$results$FST[OF$results$type=="quanti"], pch=19, col= rgb(1,0.5,0,1))

### Outline identified outliers in blue
points(OF$results$He[OF$results$OutlierFlag], OF$results$FST[OF$results$OutlierFlag], pch=1, col= rgb(0,0,1,1))

### check FDR of the analysis ###

desiredFDR <- seq(0.01,0.5,by=0.01)
trueFDR<-rep(NA, length(desiredFDR))
### Loop through different q-value cutoffs (desired false discovery rates)
for (i in 1:length(desiredFDR)){
  WhichPos <- OF$result$qvalues<=desiredFDR[i]
  
  # total number of outliers called at that q-value threshold
  NumPos <- sum(WhichPos)
  
  # number that are false positives
  FalsePos <- sum(WhichPos[OF$result$type=="ntrl"])
  
  trueFDR[i]<-FalsePos/NumPos
}

# Plot the desired vs. acutual FDRs
plot(desiredFDR, trueFDR, xlab="Desired FDR (q-value cutoff)", ylab="Actual FDR")
abline(0,1)

### pick outliers at FDR 0.3 ###

table(OF$result$type[OF$result$qvalues<0.3])

############### END ###################

