#library(gdata)
#install.packages("readxl")
#library(readxl)
library(xlsx)
path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??ED", list.dirs(), value = TRUE)
dir2 = grep("T???/??ND", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1NDPP = NULL
vectorphase2NDPP = NULL
vectorphase3NDPP = NULL
vectorphase4NDPP = NULL
vectorphase5NDPP = NULL

vectorphase1EDPP = NULL
vectorphase2EDPP = NULL
vectorphase3EDPP = NULL
vectorphase4EDPP = NULL
vectorphase5EDPP = NULL

notopen = NULL
noPPND = NULL
noPPED = NULL
nostm = NULL
#i=1
for(i in 1:length(dir)) {
  setwd(dir[i])
  getwd()
  fil = list.files(pattern = ".stm")
  fil
  if(identical(fil, character(0))) {
    setwd("../..")
    unlink(dir[i])
    nostm = append(nostm, i)
    next
  }
  #if(i > 17 & i < 25) {
  #timeslice = read_excel(fil)
  # timeslice = read.xlsx2(fil, sheetIndex = 1, header = TRUE)
  #} else {
  #  timeslice = read.xlsx(fil, sheetIndex = 1, header = TRUE)
  #}
  
  timeslice = read.xlsx(fil, sheetIndex = 1, colIndex = 1:10, header = FALSE)
  #timeslice = read.table(fil, header = FALSE)
  #timeslice = read.xlsx(fil, sheetIndex = 1)
  timeslice
  fil2 = list.files(pattern = ".pp")
  fil2
  if(identical(fil2, character(0))) {
    setwd("../..")
    unlink(dir[i])
    noPPED = append(noPPED, i)
    next
  }
  NRPerinasalED = read.xlsx(fil2, sheetIndex = 1, colIndex = c(2,4), header = TRUE)
  NRPerinasalED
  setwd("../..")
  unlink(dir[i])
  setwd(dir2[i])
  fil3 = list.files(pattern = ".pp")
  if(identical(fil3, character(0))) {
    setwd("../..")
    unlink(dir2[i])
    noPPND = append(noPPND, i)
    next
  }
  NRPerinasalND = read.xlsx(fil3, sheetIndex = 1, colIndex = c(2,4), header = TRUE)
  NRPerinasalND
  setwd("../..")
  unlink(dir2[i])
  
  Stime1 = as.numeric(as.character(timeslice[10,1]))
  Stime1
  #as.character(timeslice[9,1])
  #if(is.na(Stime1) == TRUE) {
  #notopen = append(notopen, i)
  #setwd("../..")
  #unlink(dir[i])
  #next
  #}
  Etime1 = as.numeric(as.character(timeslice[10,2]))
  Etime1
  Stime2 = as.numeric(as.character(timeslice[11,1]))
  Stime2
  Etime2 = as.numeric(as.character(timeslice[11,2]))
  Etime2
  
  #fil2 = list.files(pattern = ".pp")
  #fil2
  #if(identical(fil2, character(0))) {
  #  setwd("../..")
  #  unlink(dir[i])
  #  noPP = append(noPP, i)
  #  next
  #}
  
  #NRPerinasalED = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(NRPerinasalED)[1] <- "Time"
  names(NRPerinasalED)[2] <- "NR.Perinasal"
  NRPerinasalED
  NRPerinasalED = NRPerinasalED[-1,]
  NRPerinasalED = NRPerinasalED[order(NRPerinasalED$Time), , drop = FALSE]
  NRPerinasalED$NR.Perinasal <- as.numeric(as.character(NRPerinasalED$NR.Perinasal))
  NRPerinasalED$Time <- as.numeric(as.character(NRPerinasalED$Time))
  NRPerinasalED = NRPerinasalED[order(NRPerinasalED$Time), , drop = FALSE]
  #NRPerinasal[with(NRPerinasal, !(as.numeric(NRPerinasal$Time) < Stime1)), ]
  #NRPerinasalED = NRPerinasalED[!(as.numeric(NRPerinasalED$NR.Perinasal) < 4 
  #                                | as.numeric(NRPerinasalED$NR.Perinasal) > 70), ]
  phase1ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) < Stime1), ]
  phase2ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) > Stime1 & as.numeric(NRPerinasalED$Time) < Etime1),] 
  phase3ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) > Etime1 & as.numeric(NRPerinasalED$Time) < Stime2), ]
  phase4ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) > Stime2 & as.numeric(NRPerinasalED$Time) < Etime2 ), ]
  phase5ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) > Etime2), ]
  phase1ED
  phase2ED
  phase3ED
  phase4ED
  phase5ED
  #filter(NRPerinasal, as.numeric(NRPerinasal$Time) < Stime1)
  NRPerinasalED
  #if(is.data.frame(phase1ED) && nrow(phase1ED)==0) {
  #  meanphase1EDPP = 0
  #} else {
    meanphase1EDPP = mean(phase1ED$NR.Perinasal)
  #}
  #if(is.data.frame(phase2ED) && nrow(phase2ED)==0) {
  #  meanphase2EDPP = 0
  #} else {
    meanphase2EDPP = mean(phase2ED$NR.Perinasal)
  #}
  #if(is.data.frame(phase3ED) && nrow(phase3ED)==0) {
  #  meanphase3EDPP = 0
  #} else {
    meanphase3EDPP = mean(phase3ED$NR.Perinasal)
  #}
  #if(is.data.frame(phase4ED) && nrow(phase4ED)==0) {
  #  meanphase4EDPP = 0
  #} else {
    meanphase4EDPP = mean(phase4ED$NR.Perinasal)
  #}
  #if(is.data.frame(phase5ED) && nrow(phase5ED)==0) {
  #  meanphase5EDPP = 0
  #} else {
    meanphase5EDPP = mean(phase5ED$NR.Perinasal)
  #}
  #meanEDPP = mean(NRPerinasalED$NR.Perinasal)
  #if(is.nan(meanPP)==TRUE){
  #  break
  #}
  #setwd("../..")
  #unlink(dir[i])
  
  
  
  
  #setwd(dir2[i])
  #fil3 = list.files(pattern = ".pp")
  #if(identical(fil3, character(0))) {
  #  setwd("../..")
  #  unlink(dir2[i])
  #noPP = append(noPP, i)
  #  next
  #}
  #NRPerinasalND = read.xlsx(fil3, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(NRPerinasalND)[1] <- "Time"
  names(NRPerinasalND)[2] <- "NR.Perinasal"
  NRPerinasalND = NRPerinasalND[-1,]
  NRPerinasalND = NRPerinasalND[order(NRPerinasalND$Time), , drop = FALSE]
  NRPerinasalND$NR.Perinasal <- as.numeric(as.character(NRPerinasalND$NR.Perinasal))
  NRPerinasalND$Time <- as.numeric(as.character(NRPerinasalND$Time))
  NRPerinasalND = NRPerinasalND[order(NRPerinasalND$Time), , drop = FALSE]
  #NRPerinasal[with(NRPerinasal, !(as.numeric(NRPerinasal$Time) < Stime1)), ]
  #NRPerinasalND = NRPerinasalND[!(as.numeric(NRPerinasalND$NR.Perinasal) < 4 
  #                                | as.numeric(NRPerinasalND$NR.Perinasal) > 70), ]
  phase1ND = NRPerinasalND[(as.numeric(NRPerinasalND$Time) < Stime1), ]
  phase2ND = NRPerinasalND[(as.numeric(NRPerinasalND$Time) > Stime1 & as.numeric(NRPerinasalND$Time) < Etime1),] 
  phase3ND = NRPerinasalND[(as.numeric(NRPerinasalND$Time) > Etime1 & as.numeric(NRPerinasalND$Time) < Stime2), ]
  phase4ND = NRPerinasalND[(as.numeric(NRPerinasalND$Time) > Stime2 & as.numeric(NRPerinasalND$Time) < Etime2 ), ]
  phase5ND = NRPerinasalND[(as.numeric(NRPerinasalND$Time) > Etime2), ]
  #filter(NRPerinasal, as.numeric(NRPerinasal$Time) < Stime1)
  #NRPerinasalND
  #if(is.data.frame(NRPerinasalND) && nrow(NRPerinasalND)==0) {
  # meanNDPP = 0
  #} else {
  #  meanNDPP = mean(NRPerinasalND$NR.Perinasal)
  #}
  #if(is.data.frame(phase1ND) && nrow(phase1ND)==0) {
  #  meanphase1NDPP = 0
  #} else {
    meanphase1NDPP = mean(phase1ND$NR.Perinasal)
  #}
  #if(is.data.frame(phase2ND) && nrow(phase2ND)==0) {
  #  meanphase2NDPP = 0
  #} else {
    meanphase2NDPP = mean(phase2ND$NR.Perinasal)
  #}
  #if(is.data.frame(phase3ND) && nrow(phase3ND)==0) {
    meanphase3NDPP = 0
  #} else {
    meanphase3NDPP = mean(phase3ND$NR.Perinasal)
  #}
  #if(is.data.frame(phase4ND) && nrow(phase4ND)==0) {
  #  meanphase4NDPP = 0
  #} else {
    meanphase4NDPP = mean(phase4ND$NR.Perinasal)
  #}
  #if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
  #  meanphase5NDPP = 0
  #} else {
    meanphase5NDPP = mean(phase5ND$NR.Perinasal)
  #}
  
  vectorphase1NDPP = append(vectorphase1NDPP, meanphase1NDPP)
  vectorphase2NDPP = append(vectorphase2NDPP, meanphase2NDPP)
  vectorphase3NDPP = append(vectorphase3NDPP, meanphase3NDPP)
  vectorphase4NDPP = append(vectorphase4NDPP, meanphase4NDPP)
  vectorphase5NDPP = append(vectorphase5NDPP, meanphase5NDPP)
  
  vectorphase1EDPP = append(vectorphase1EDPP, meanphase1EDPP)
  vectorphase2EDPP = append(vectorphase2EDPP, meanphase2EDPP)
  vectorphase3EDPP = append(vectorphase3EDPP, meanphase3EDPP)
  vectorphase4EDPP = append(vectorphase4EDPP, meanphase4EDPP)
  vectorphase5EDPP = append(vectorphase5EDPP, meanphase5EDPP)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




vectorphase1NDPP
vectorphase2NDPP
vectorphase3NDPP
vectorphase4NDPP
vectorphase5NDPP

vectorphase1EDPP
vectorphase2EDPP
vectorphase3EDPP
vectorphase4EDPP
vectorphase5EDPP

nostm


vectorEDPP
qqnorm(vectorEDPP)
qqline(vectorEDPP)
vectorNDPP
qqnorm(vectorNDPP)
qqline(vectorNDPP)
diffvector = vectorEDPP - vectorNDPP
qqnorm(diffvector)
qqline(diffvector)


b <- boxplot(vectorEDPP, vectorNDPP, xaxt='n',
             xlab="Sessions", ylab="Noise Reduced Perinasal",
             main="Boxplot of ED and ND Sessions for Noise Reduced Perinasal")
means <- c(mean(vectorEDPP), mean(vectorNDPP))
points(means, pch=8, col="red")
axis(1, at=1:2, labels=c("ED","ND"))
text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
abline(v=mean(1:length(b$n)), lty=3)
