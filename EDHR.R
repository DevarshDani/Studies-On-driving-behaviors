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
vectorphase1NDHR = NULL
vectorphase2NDHR = NULL
vectorphase3NDHR = NULL
vectorphase4NDHR = NULL
vectorphase5NDHR = NULL

vectorphase1EDHR = NULL
vectorphase2EDHR = NULL
vectorphase3EDHR = NULL
vectorphase4EDHR = NULL
vectorphase5EDHR = NULL

notopen = NULL
noHRND = NULL
noHRED = NULL
nostm = NULL
#i=18
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
  fil2 = list.files(pattern = ".HR")
  fil2
  if(identical(fil2, character(0))) {
    setwd("../..")
    unlink(dir[i])
    noHRED = append(noHRED, i)
    next
  }
  heartrateED = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  heartrateED
  setwd("../..")
  unlink(dir[i])
  setwd(dir2[i])
  fil3 = list.files(pattern = ".HR")
  if(identical(fil3, character(0))) {
    setwd("../..")
    unlink(dir2[i])
    noHRND = append(noHRND, i)
    next
  }
  heartrateND = read.xlsx(fil3, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  heartrateND
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
  
  #fil2 = list.files(pattern = ".HR")
  #fil2
  #if(identical(fil2, character(0))) {
  #  setwd("../..")
  #  unlink(dir[i])
  #  noHR = append(noHR, i)
  #  next
  #}
  
  #heartrateCD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(heartrateED)[1] <- "Time"
  names(heartrateED)[2] <- "Heart.Rate"
  heartrateED
  heartrateED = heartrateED[-1,]
  heartrateED = heartrateED[order(heartrateED$Time), , drop = FALSE]
  heartrateED$Heart.Rate <- as.numeric(as.character(heartrateED$Heart.Rate))
  heartrateED$Time <- as.numeric(as.character(heartrateED$Time))
  heartrateED = heartrateED[order(heartrateED$Time), , drop = FALSE]
  #heartrate[with(heartrate, !(as.numeric(heartrate$Time) < Stime1)), ]
  heartrateED = heartrateED[!(as.numeric(heartrateED$Heart.Rate) < 40 
                              | as.numeric(heartrateED$Heart.Rate) > 140), ]
  phase1ED = heartrateED[(as.numeric(heartrateED$Time) < Stime1), ]
  phase2ED = heartrateED[(as.numeric(heartrateED$Time) > Stime1 & as.numeric(heartrateED$Time) < Etime1),] 
  phase3ED = heartrateED[(as.numeric(heartrateED$Time) > Etime1 & as.numeric(heartrateED$Time) < Stime2), ]
  phase4ED = heartrateED[(as.numeric(heartrateED$Time) > Stime2 & as.numeric(heartrateED$Time) < Etime2 ), ]
  phase5ED = heartrateED[(as.numeric(heartrateED$Time) > Etime2), ]
  phase1ED
  phase2ED
  phase3ED
  phase4ED
  phase5ED
  #filter(heartrate, as.numeric(heartrate$Time) < Stime1)
  heartrateED
  if(is.data.frame(phase1ED) && nrow(phase1ED)==0) {
    meanphase1EDHR = 0
  } else {
    meanphase1EDHR = mean(phase1ED$Heart.Rate)
  }
  if(is.data.frame(phase2ED) && nrow(phase2ED)==0) {
    meanphase2EDHR = 0
  } else {
    meanphase2EDHR = mean(phase2ED$Heart.Rate)
  }
  if(is.data.frame(phase3ED) && nrow(phase3ED)==0) {
    meanphase3EDHR = 0
  } else {
    meanphase3EDHR = mean(phase3ED$Heart.Rate)
  }
  if(is.data.frame(phase4ED) && nrow(phase4ED)==0) {
    meanphase4EDHR = 0
  } else {
    meanphase4EDHR = mean(phase4ED$Heart.Rate)
  }
  if(is.data.frame(phase5ED) && nrow(phase5ED)==0) {
    meanphase5EDHR = 0
  } else {
    meanphase5EDHR = mean(phase5ED$Heart.Rate)
  }
  #meanCDHR = mean(heartrateCD$Heart.Rate)
  #if(is.nan(meanHR)==TRUE){
  #  break
  #}
  #setwd("../..")
  #unlink(dir[i])
  
  
  
  
  #setwd(dir2[i])
  #fil3 = list.files(pattern = ".HR")
  #if(identical(fil3, character(0))) {
  #  setwd("../..")
  #  unlink(dir2[i])
  #noHR = append(noHR, i)
  #  next
  #}
  #heartrateND = read.xlsx(fil3, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(heartrateND)[1] <- "Time"
  names(heartrateND)[2] <- "Heart.Rate"
  heartrateND = heartrateND[-1,]
  heartrateND = heartrateND[order(heartrateND$Time), , drop = FALSE]
  heartrateND$Heart.Rate <- as.numeric(as.character(heartrateND$Heart.Rate))
  heartrateND$Time <- as.numeric(as.character(heartrateND$Time))
  heartrateND = heartrateND[order(heartrateND$Time), , drop = FALSE]
  #heartrate[with(heartrate, !(as.numeric(heartrate$Time) < Stime1)), ]
  heartrateND = heartrateND[!(as.numeric(heartrateND$Heart.Rate) < 40 
                              | as.numeric(heartrateND$Heart.Rate) > 140), ]
  phase1ND = heartrateND[(as.numeric(heartrateND$Time) < Stime1), ]
  phase2ND = heartrateND[(as.numeric(heartrateND$Time) > Stime1 & as.numeric(heartrateND$Time) < Etime1),] 
  phase3ND = heartrateND[(as.numeric(heartrateND$Time) > Etime1 & as.numeric(heartrateND$Time) < Stime2), ]
  phase4ND = heartrateND[(as.numeric(heartrateND$Time) > Stime2 & as.numeric(heartrateND$Time) < Etime2 ), ]
  phase5ND = heartrateND[(as.numeric(heartrateND$Time) > Etime2), ]
  #filter(heartrate, as.numeric(heartrate$Time) < Stime1)
  #heartrateND
  #if(is.data.frame(heartrateND) && nrow(heartrateND)==0) {
  # meanNDHR = 0
  #} else {
  #  meanNDHR = mean(heartrateND$Heart.Rate)
  #}
  if(is.data.frame(phase1ND) && nrow(phase1ND)==0) {
    meanphase1NDHR = 0
  } else {
    meanphase1NDHR = mean(phase1ND$Heart.Rate)
  }
  if(is.data.frame(phase2ND) && nrow(phase2ND)==0) {
    meanphase2NDHR = 0
  } else {
    meanphase2NDHR = mean(phase2ND$Heart.Rate)
  }
  if(is.data.frame(phase3ND) && nrow(phase3ND)==0) {
    meanphase3NDHR = 0
  } else {
    meanphase3NDHR = mean(phase3ND$Heart.Rate)
  }
  if(is.data.frame(phase4ND) && nrow(phase4ND)==0) {
    meanphase4NDHR = 0
  } else {
    meanphase4NDHR = mean(phase4ND$Heart.Rate)
  }
  if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
    meanphase5NDHR = 0
  } else {
    meanphase5NDHR = mean(phase5ND$Heart.Rate)
  }
  
  vectorphase1NDHR = append(vectorphase1NDHR, meanphase1NDHR)
  vectorphase2NDHR = append(vectorphase2NDHR, meanphase2NDHR)
  vectorphase3NDHR = append(vectorphase3NDHR, meanphase3NDHR)
  vectorphase4NDHR = append(vectorphase4NDHR, meanphase4NDHR)
  vectorphase5NDHR = append(vectorphase5NDHR, meanphase5NDHR)
  
  vectorphase1EDHR = append(vectorphase1EDHR, meanphase1EDHR)
  vectorphase2EDHR = append(vectorphase2EDHR, meanphase2EDHR)
  vectorphase3EDHR = append(vectorphase3EDHR, meanphase3EDHR)
  vectorphase4EDHR = append(vectorphase4EDHR, meanphase4EDHR)
  vectorphase5EDHR = append(vectorphase5EDHR, meanphase5EDHR)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




vectorphase1NDHR
vectorphase2NDHR
vectorphase3NDHR
vectorphase4NDHR
vectorphase5NDHR

vectorphase1EDHR
vectorphase2EDHR
vectorphase3EDHR
vectorphase4EDHR
vectorphase5EDHR


vectorEDHR
qqnorm(vectorEDHR)
qqline(vectorEDHR)
vectorNDHR
qqnorm(vectorNDHR)
qqline(vectorNDHR)
diffvector = vectorEDHR - vectorNDHR
qqnorm(diffvector)
qqline(diffvector)


b <- boxplot(vectorEDHR, vectorNDHR, xaxt='n',
             xlab="Sessions", ylab="Heart rate",
             main="Boxplot of ED and ND Sessions for HR")
means <- c(mean(vectorEDHR), mean(vectorNDHR))
points(means, pch=8, col="red")
axis(1, at=1:2, labels=c("ED","ND"))
text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
abline(v=mean(1:length(b$n)), lty=3)