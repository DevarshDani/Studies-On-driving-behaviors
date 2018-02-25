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
vectorphase1MDNDPEDA = NULL
vectorphase2MDNDPEDA = NULL
vectorphase3MDNDPEDA = NULL
vectorphase4MDNDPEDA = NULL
vectorphase5MDNDPEDA = NULL

vectorphase1MDPEDA = NULL
vectorphase2MDPEDA = NULL
vectorphase3MDPEDA = NULL
vectorphase4MDPEDA = NULL
vectorphase5MDPEDA = NULL

notopen = NULL
noPEDAND = NULL
noPEDAMD = NULL
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
  fil2 = list.files(pattern = ".peda")
  fil2
  if(identical(fil2, character(0))) {
    setwd("../..")
    unlink(dir[i])
    noPEDAMD = append(noPEDAMD, i)
    next
  }
  PalmEDAMD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  PalmEDAMD
  setwd("../..")
  unlink(dir[i])
  setwd(dir2[i])
  fil3 = list.files(pattern = ".peda")
  if(identical(fil3, character(0))) {
    setwd("../..")
    unlink(dir2[i])
    noPEDAND = append(noPEDAND, i)
    next
  }
  PalmEDAND = read.xlsx(fil3, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  PalmEDAND
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
  
  #fil2 = list.files(pattern = ".PEDA")
  #fil2
  #if(identical(fil2, character(0))) {
  #  setwd("../..")
  #  unlink(dir[i])
  #  noPEDA = append(noPEDA, i)
  #  next
  #}
  
  #PalmEDAMD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(PalmEDAMD)[1] <- "Time"
  names(PalmEDAMD)[2] <- "Palm.EDA"
  PalmEDAMD
  PalmEDAMD = PalmEDAMD[-1,]
  PalmEDAMD = PalmEDAMD[order(PalmEDAMD$Time), , drop = FALSE]
  PalmEDAMD$Palm.EDA <- as.numeric(as.character(PalmEDAMD$Palm.EDA))
  PalmEDAMD$Time <- as.numeric(as.character(PalmEDAMD$Time))
  PalmEDAMD = PalmEDAMD[order(PalmEDAMD$Time), , drop = FALSE]
  #PalmEDA[with(PalmEDA, !(as.numeric(PalmEDA$Time) < Stime1)), ]
  PalmEDAMD = PalmEDAMD[!(as.numeric(PalmEDAMD$Palm.EDA) < 10 
                          | as.numeric(PalmEDAMD$Palm.EDA) > 4700), ]
  phase1MD = PalmEDAMD[(as.numeric(PalmEDAMD$Time) < Stime1), ]
  phase2MD = PalmEDAMD[(as.numeric(PalmEDAMD$Time) > Stime1 & as.numeric(PalmEDAMD$Time) < Etime1),] 
  phase3MD = PalmEDAMD[(as.numeric(PalmEDAMD$Time) > Etime1 & as.numeric(PalmEDAMD$Time) < Stime2), ]
  phase4MD = PalmEDAMD[(as.numeric(PalmEDAMD$Time) > Stime2 & as.numeric(PalmEDAMD$Time) < Etime2 ), ]
  phase5MD = PalmEDAMD[(as.numeric(PalmEDAMD$Time) > Etime2), ]
  phase1MD
  phase2MD
  phase3MD
  phase4MD
  phase5MD
  #filter(PalmEDA, as.numeric(PalmEDA$Time) < Stime1)
  PalmEDAMD
  #if(is.data.frame(phase1MD) && nrow(phase1MD)==0) {
  #  meanphase1MDPEDA = 0
  #} else {
    meanphase1MDPEDA = mean(phase1MD$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2MD) && nrow(phase2MD)==0) {
  #  meanphase2MDPEDA = 0
  #} else {
    meanphase2MDPEDA = mean(phase2MD$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3MD) && nrow(phase3MD)==0) {
  #  meanphase3MDPEDA = 0
  #} else {
    meanphase3MDPEDA = mean(phase3MD$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4MD) && nrow(phase4MD)==0) {
  #  meanphase4MDPEDA = 0
  #} else {
    meanphase4MDPEDA = mean(phase4MD$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5MD) && nrow(phase5MD)==0) {
  #  meanphase5MDPEDA = 0
  #} else {
    meanphase5MDPEDA = mean(phase5MD$Palm.EDA, na.rm = TRUE)
  #}
  #meanMDPEDA = mean(PalmEDAMD$Palm.EDA)
  #if(is.nan(meanPEDA)==TRUE){
  #  break
  #}
  #setwd("../..")
  #unlink(dir[i])
  
  
  
  
  #setwd(dir2[i])
  #fil3 = list.files(pattern = ".peda")
  #if(identical(fil3, character(0))) {
  #  setwd("../..")
  #  unlink(dir2[i])
  #noPEDA = append(noPEDA, i)
  #  next
  #}
  #PalmEDAND = read.xlsx(fil3, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(PalmEDAND)[1] <- "Time"
  names(PalmEDAND)[2] <- "Palm.EDA"
  PalmEDAND = PalmEDAND[-1,]
  PalmEDAND = PalmEDAND[order(PalmEDAND$Time), , drop = FALSE]
  PalmEDAND$Palm.EDA <- as.numeric(as.character(PalmEDAND$Palm.EDA))
  PalmEDAND$Time <- as.numeric(as.character(PalmEDAND$Time))
  PalmEDAND = PalmEDAND[order(PalmEDAND$Time), , drop = FALSE]
  #PalmEDA[with(PalmEDA, !(as.numeric(PalmEDA$Time) < Stime1)), ]
  #PalmEDAND = PalmEDAND[!(as.numeric(PalmEDAND$Palm.EDA) < 4 
  #                                | as.numeric(PalmEDAND$Palm.EDA) > 70), ]
  phase1ND = PalmEDAND[(as.numeric(PalmEDAND$Time) < Stime1), ]
  phase2ND = PalmEDAND[(as.numeric(PalmEDAND$Time) > Stime1 & as.numeric(PalmEDAND$Time) < Etime1),] 
  phase3ND = PalmEDAND[(as.numeric(PalmEDAND$Time) > Etime1 & as.numeric(PalmEDAND$Time) < Stime2), ]
  phase4ND = PalmEDAND[(as.numeric(PalmEDAND$Time) > Stime2 & as.numeric(PalmEDAND$Time) < Etime2 ), ]
  phase5ND = PalmEDAND[(as.numeric(PalmEDAND$Time) > Etime2), ]
  #filter(PalmEDA, as.numeric(PalmEDA$Time) < Stime1)
  #PalmEDAND
  #if(is.data.frame(PalmEDAND) && nrow(PalmEDAND)==0) {
  # meanNDPEDA = 0
  #} else {
  #  meanNDPEDA = mean(PalmEDAND$Palm.EDA)
  #}
  #if(is.data.frame(phase1ND) && nrow(phase1ND)==0) {
  #  meanphase1NDPEDA = 0
  #} else {
    meanphase1NDPEDA = mean(phase1ND$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2ND) && nrow(phase2ND)==0) {
  #  meanphase2NDPEDA = 0
  #} else {
    meanphase2NDPEDA = mean(phase2ND$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3ND) && nrow(phase3ND)==0) {
  #  meanphase3NDPEDA = 0
  #} else {
    meanphase3NDPEDA = mean(phase3ND$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4ND) && nrow(phase4ND)==0) {
  #  meanphase4NDPEDA = 0
  #} else {
    meanphase4NDPEDA = mean(phase4ND$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
  #  meanphase5NDPEDA = 0
  #} else {
    meanphase5NDPEDA = mean(phase5ND$Palm.EDA, na.rm = TRUE)
  #}
  
  vectorphase1MDNDPEDA = append(vectorphase1MDNDPEDA, meanphase1NDPEDA)
  vectorphase2MDNDPEDA = append(vectorphase2MDNDPEDA, meanphase2NDPEDA)
  vectorphase3MDNDPEDA = append(vectorphase3MDNDPEDA, meanphase3NDPEDA)
  vectorphase4MDNDPEDA = append(vectorphase4MDNDPEDA, meanphase4NDPEDA)
  vectorphase5MDNDPEDA = append(vectorphase5MDNDPEDA, meanphase5NDPEDA)
  
  vectorphase1MDPEDA = append(vectorphase1MDPEDA, meanphase1MDPEDA)
  vectorphase2MDPEDA = append(vectorphase2MDPEDA, meanphase2MDPEDA)
  vectorphase3MDPEDA = append(vectorphase3MDPEDA, meanphase3MDPEDA)
  vectorphase4MDPEDA = append(vectorphase4MDPEDA, meanphase4MDPEDA)
  vectorphase5MDPEDA = append(vectorphase5MDPEDA, meanphase5MDPEDA)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




vectorphase1NDPEDA
vectorphase2NDPEDA
vectorphase3NDPEDA
vectorphase4NDPEDA
vectorphase5NDPEDA

vectorphase1MDPEDA
vectorphase2MDPEDA
vectorphase3MDPEDA
vectorphase4MDPEDA
vectorphase5MDPEDA

nostm


vectorMDPEDA
qqnorm(vectorMDPEDA)
qqline(vectorMDPEDA)
vectorNDPEDA
qqnorm(vectorNDPEDA)
qqline(vectorNDPEDA)
diffvector = vectorMDPEDA - vectorNDPEDA
qqnorm(diffvector)
qqline(diffvector)


b <- boxplot(vectorMDPEDA, vectorNDPEDA, xaxt='n',
             xlab="Sessions", ylab="Noise Reduced Perinasal",
             main="Boxplot of MD and ND Sessions for Noise Reduced Perinasal")
means <- c(mean(vectorMDPEDA), mean(vectorNDPEDA))
points(means, pch=8, col="red")
axis(1, at=1:2, labels=c("MD","ND"))
text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
abline(v=mean(1:length(b$n)), lty=3)
