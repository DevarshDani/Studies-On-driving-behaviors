#library(gdata)
#install.packages("readxl")
#library(readxl)
library(xlsx)
path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??MD", list.dirs(), value = TRUE)
dir2 = grep("T???/??ND", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1NDPP = NULL
vectorphase2NDPP = NULL
vectorphase3NDPP = NULL
vectorphase4NDPP = NULL
vectorphase5NDPP = NULL

vectorphase1MDPP = NULL
vectorphase2MDPP = NULL
vectorphase3MDPP = NULL
vectorphase4MDPP = NULL
vectorphase5MDPP = NULL

notopen = NULL
noPPND = NULL
noPPMD = NULL
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
    noPPMD = append(noPPMD, i)
    next
  }
  NRPerinasalMD = read.xlsx(fil2, sheetIndex = 1, colIndex = c(2,4), header = TRUE)
  NRPerinasalMD
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
  
  #NRPerinasalMD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(NRPerinasalMD)[1] <- "Time"
  names(NRPerinasalMD)[2] <- "NR.Perinasal"
  NRPerinasalMD
  NRPerinasalMD = NRPerinasalMD[-1,]
  NRPerinasalMD = NRPerinasalMD[order(NRPerinasalMD$Time), , drop = FALSE]
  NRPerinasalMD$NR.Perinasal <- as.numeric(as.character(NRPerinasalMD$NR.Perinasal))
  NRPerinasalMD$Time <- as.numeric(as.character(NRPerinasalMD$Time))
  NRPerinasalMD = NRPerinasalMD[order(NRPerinasalMD$Time), , drop = FALSE]
  #NRPerinasal[with(NRPerinasal, !(as.numeric(NRPerinasal$Time) < Stime1)), ]
  #NRPerinasalMD = NRPerinasalMD[!(as.numeric(NRPerinasalMD$NR.Perinasal) < 4 
  #                                | as.numeric(NRPerinasalMD$NR.Perinasal) > 70), ]
  phase1MD = NRPerinasalMD[(as.numeric(NRPerinasalMD$Time) < Stime1), ]
  phase2MD = NRPerinasalMD[(as.numeric(NRPerinasalMD$Time) > Stime1 & as.numeric(NRPerinasalMD$Time) < Etime1),] 
  phase3MD = NRPerinasalMD[(as.numeric(NRPerinasalMD$Time) > Etime1 & as.numeric(NRPerinasalMD$Time) < Stime2), ]
  phase4MD = NRPerinasalMD[(as.numeric(NRPerinasalMD$Time) > Stime2 & as.numeric(NRPerinasalMD$Time) < Etime2 ), ]
  phase5MD = NRPerinasalMD[(as.numeric(NRPerinasalMD$Time) > Etime2), ]
  phase1MD
  phase2MD
  phase3MD
  phase4MD
  phase5MD
  #filter(NRPerinasal, as.numeric(NRPerinasal$Time) < Stime1)
  NRPerinasalMD
  #if(is.data.frame(phase1MD) && nrow(phase1MD)==0) {
  #  meanphase1MDPP = 0
  #} else {
    meanphase1MDPP = mean(phase1MD$NR.Perinasal)
  #}
  #if(is.data.frame(phase2MD) && nrow(phase2MD)==0) {
  #  meanphase2MDPP = 0
  #} else {
    meanphase2MDPP = mean(phase2MD$NR.Perinasal)
  #}
  #if(is.data.frame(phase3MD) && nrow(phase3MD)==0) {
  #  meanphase3MDPP = 0
  #} else {
    meanphase3MDPP = mean(phase3MD$NR.Perinasal)
  #}
  #if(is.data.frame(phase4MD) && nrow(phase4MD)==0) {
  #  meanphase4MDPP = 0
  #} else {
    meanphase4MDPP = mean(phase4MD$NR.Perinasal)
  #}
  #if(is.data.frame(phase5MD) && nrow(phase5MD)==0) {
  #  meanphase5MDPP = 0
  #} else {
    meanphase5MDPP = mean(phase5MD$NR.Perinasal)
  #}
  #meanMDPP = mean(NRPerinasalMD$NR.Perinasal)
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
  #  meanphase3NDPP = 0
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
  
  vectorphase1MDPP = append(vectorphase1MDPP, meanphase1MDPP)
  vectorphase2MDPP = append(vectorphase2MDPP, meanphase2MDPP)
  vectorphase3MDPP = append(vectorphase3MDPP, meanphase3MDPP)
  vectorphase4MDPP = append(vectorphase4MDPP, meanphase4MDPP)
  vectorphase5MDPP = append(vectorphase5MDPP, meanphase5MDPP)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




vectorphase1NDPP
vectorphase2NDPP
vectorphase3NDPP
vectorphase4NDPP
vectorphase5NDPP

vectorphase1MDPP
vectorphase2MDPP
vectorphase3MDPP
vectorphase4MDPP
vectorphase5MDPP

nostm


vectorMDPP
qqnorm(vectorMDPP)
qqline(vectorMDPP)
vectorNDPP
qqnorm(vectorNDPP)
qqline(vectorNDPP)
diffvector = vectorMDPP - vectorNDPP
qqnorm(diffvector)
qqline(diffvector)


b <- boxplot(vectorMDPP, vectorNDPP, xaxt='n',
             xlab="Sessions", ylab="Noise Reduced Perinasal",
             main="Boxplot of MD and ND Sessions for Noise Reduced Perinasal")
means <- c(mean(vectorMDPP), mean(vectorNDPP))
points(means, pch=8, col="red")
axis(1, at=1:2, labels=c("MD","ND"))
text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
abline(v=mean(1:length(b$n)), lty=3)
