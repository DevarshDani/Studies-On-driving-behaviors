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
vectorphase1EDNDPEDA = NULL
vectorphase2EDNDPEDA = NULL
vectorphase3EDNDPEDA = NULL
vectorphase4EDNDPEDA = NULL
vectorphase5EDNDPEDA = NULL

vectorphase1EDPEDA = NULL
vectorphase2EDPEDA = NULL
vectorphase3EDPEDA = NULL
vectorphase4EDPEDA = NULL
vectorphase5EDPEDA = NULL

notopen = NULL
noPEDAND = NULL
noPEDAED = NULL
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
    noPEDAED = append(noPEDAED, i)
    next
  }
  PalmEDAED = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  PalmEDAED
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
  
  #PalmEDAED = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(PalmEDAED)[1] <- "Time"
  names(PalmEDAED)[2] <- "Palm.EDA"
  PalmEDAED
  PalmEDAED = PalmEDAED[-1,]
  PalmEDAED = PalmEDAED[order(PalmEDAED$Time), , drop = FALSE]
  PalmEDAED$Palm.EDA <- as.numeric(as.character(PalmEDAED$Palm.EDA))
  PalmEDAED$Time <- as.numeric(as.character(PalmEDAED$Time))
  PalmEDAED = PalmEDAED[order(PalmEDAED$Time), , drop = FALSE]
  #PalmEDA[with(PalmEDA, !(as.numeric(PalmEDA$Time) < Stime1)), ]
  PalmEDAED = PalmEDAED[!(as.numeric(PalmEDAED$Palm.EDA) < 10 
                          | as.numeric(PalmEDAED$Palm.EDA) > 4700), ]
  phase1ED = PalmEDAED[(as.numeric(PalmEDAED$Time) < Stime1), ]
  phase2ED = PalmEDAED[(as.numeric(PalmEDAED$Time) > Stime1 & as.numeric(PalmEDAED$Time) < Etime1),] 
  phase3ED = PalmEDAED[(as.numeric(PalmEDAED$Time) > Etime1 & as.numeric(PalmEDAED$Time) < Stime2), ]
  phase4ED = PalmEDAED[(as.numeric(PalmEDAED$Time) > Stime2 & as.numeric(PalmEDAED$Time) < Etime2 ), ]
  phase5ED = PalmEDAED[(as.numeric(PalmEDAED$Time) > Etime2), ]
  phase1ED
  phase2ED
  phase3ED
  phase4ED
  phase5ED
  #filter(PalmEDA, as.numeric(PalmEDA$Time) < Stime1)
  PalmEDAED
  #if(is.data.frame(phase1ED) && nrow(phase1ED)==0) {
  #  meanphase1EDPEDA = 0
  #} else {
    meanphase1EDPEDA = mean(phase1ED$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2ED) && nrow(phase2ED)==0) {
  #  meanphase2EDPEDA = 0
  #} else {
    meanphase2EDPEDA = mean(phase2ED$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3ED) && nrow(phase3ED)==0) {
  #  meanphase3EDPEDA = 0
  #} else {
    meanphase3EDPEDA = mean(phase3ED$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4ED) && nrow(phase4ED)==0) {
  #  meanphase4EDPEDA = 0
  #} else {
    meanphase4EDPEDA = mean(phase4ED$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5ED) && nrow(phase5ED)==0) {
  #  meanphase5EDPEDA = 0
  #} else {
    meanphase5EDPEDA = mean(phase5ED$Palm.EDA, na.rm = TRUE)
  #}
  #meanEDPEDA = mean(PalmEDAED$Palm.EDA)
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
  
  vectorphase1EDNDPEDA = append(vectorphase1EDNDPEDA, meanphase1NDPEDA)
  vectorphase2EDNDPEDA = append(vectorphase2EDNDPEDA, meanphase2NDPEDA)
  vectorphase3EDNDPEDA = append(vectorphase3EDNDPEDA, meanphase3NDPEDA)
  vectorphase4EDNDPEDA = append(vectorphase4EDNDPEDA, meanphase4NDPEDA)
  vectorphase5EDNDPEDA = append(vectorphase5EDNDPEDA, meanphase5NDPEDA)
  
  vectorphase1EDPEDA = append(vectorphase1EDPEDA, meanphase1EDPEDA)
  vectorphase2EDPEDA = append(vectorphase2EDPEDA, meanphase2EDPEDA)
  vectorphase3EDPEDA = append(vectorphase3EDPEDA, meanphase3EDPEDA)
  vectorphase4EDPEDA = append(vectorphase4EDPEDA, meanphase4EDPEDA)
  vectorphase5EDPEDA = append(vectorphase5EDPEDA, meanphase5EDPEDA)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




vectorphase1NDPEDA
vectorphase2NDPEDA
vectorphase3NDPEDA
vectorphase4NDPEDA
vectorphase5NDPEDA

vectorphase1EDPEDA
vectorphase2EDPEDA
vectorphase3EDPEDA
vectorphase4EDPEDA
vectorphase5EDPEDA

nostm


vectorEDPEDA
qqnorm(vectorEDPEDA)
qqline(vectorEDPEDA)
vectorNDPEDA
qqnorm(vectorNDPEDA)
qqline(vectorNDPEDA)
diffvector = vectorEDPEDA - vectorNDPEDA
qqnorm(diffvector)
qqline(diffvector)


b <- boxplot(vectorEDPEDA, vectorNDPEDA, xaxt='n',
             xlab="Sessions", ylab="Noise Reduced Perinasal",
             main="Boxplot of ED and ND Sessions for Noise Reduced Perinasal")
means <- c(mean(vectorEDPEDA), mean(vectorNDPEDA))
points(means, pch=8, col="red")
axis(1, at=1:2, labels=c("ED","ND"))
text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
abline(v=mean(1:length(b$n)), lty=3)
