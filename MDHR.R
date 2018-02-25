#library(gdata)
#install.packages("readxl")
#library(readxl)
library(xlsx)
path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??ED", list.dirs(), value = TRUE)
dir2 = grep("T???/??MD", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1NDHR = NULL
vectorphase2NDHR = NULL
vectorphase3NDHR = NULL
vectorphase4NDHR = NULL
vectorphase5NDHR = NULL

vectorphase1MDHR = NULL
vectorphase2MDHR = NULL
vectorphase3MDHR = NULL
vectorphase4MDHR = NULL
vectorphase5MDHR = NULL

notopen = NULL
noHRND = NULL
noHRMD = NULL
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
    noHRMD = append(noHRMD, i)
    next
  }
  heartrateMD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  heartrateMD
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
  names(heartrateMD)[1] <- "Time"
  names(heartrateMD)[2] <- "Heart.Rate"
  heartrateMD
  heartrateMD = heartrateMD[-1,]
  heartrateMD = heartrateMD[order(heartrateMD$Time), , drop = FALSE]
  heartrateMD$Heart.Rate <- as.numeric(as.character(heartrateMD$Heart.Rate))
  heartrateMD$Time <- as.numeric(as.character(heartrateMD$Time))
  heartrateMD = heartrateMD[order(heartrateMD$Time), , drop = FALSE]
  #heartrate[with(heartrate, !(as.numeric(heartrate$Time) < Stime1)), ]
  heartrateMD = heartrateMD[!(as.numeric(heartrateMD$Heart.Rate) < 40 
                              | as.numeric(heartrateMD$Heart.Rate) > 140), ]
  phase1MD = heartrateMD[(as.numeric(heartrateMD$Time) < Stime1), ]
  phase2MD = heartrateMD[(as.numeric(heartrateMD$Time) > Stime1 & as.numeric(heartrateMD$Time) < Etime1),] 
  phase3MD = heartrateMD[(as.numeric(heartrateMD$Time) > Etime1 & as.numeric(heartrateMD$Time) < Stime2), ]
  phase4MD = heartrateMD[(as.numeric(heartrateMD$Time) > Stime2 & as.numeric(heartrateMD$Time) < Etime2 ), ]
  phase5MD = heartrateMD[(as.numeric(heartrateMD$Time) > Etime2), ]
  phase1MD
  phase2MD
  phase3MD
  phase4MD
  phase5MD
  #filter(heartrate, as.numeric(heartrate$Time) < Stime1)
  heartrateMD
  if(is.data.frame(phase1MD) && nrow(phase1MD)==0) {
    meanphase1MDHR = 0
  } else {
    meanphase1MDHR = mean(phase1MD$Heart.Rate)
  }
  if(is.data.frame(phase2MD) && nrow(phase2MD)==0) {
    meanphase2MDHR = 0
  } else {
    meanphase2MDHR = mean(phase2MD$Heart.Rate)
  }
  if(is.data.frame(phase3MD) && nrow(phase3MD)==0) {
    meanphase3MDHR = 0
  } else {
    meanphase3MDHR = mean(phase3MD$Heart.Rate)
  }
  if(is.data.frame(phase4MD) && nrow(phase4MD)==0) {
    meanphase4MDHR = 0
  } else {
    meanphase4MDHR = mean(phase4MD$Heart.Rate)
  }
  if(is.data.frame(phase5MD) && nrow(phase5MD)==0) {
    meanphase5MDHR = 0
  } else {
    meanphase5MDHR = mean(phase5MD$Heart.Rate)
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
  
  vectorphase1MDHR = append(vectorphase1MDHR, meanphase1MDHR)
  vectorphase2MDHR = append(vectorphase2MDHR, meanphase2MDHR)
  vectorphase3MDHR = append(vectorphase3MDHR, meanphase3MDHR)
  vectorphase4MDHR = append(vectorphase4MDHR, meanphase4MDHR)
  vectorphase5MDHR = append(vectorphase5MDHR, meanphase5MDHR)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




vectorphase1NDHR
vectorphase2NDHR
vectorphase3NDHR
vectorphase4NDHR
vectorphase5NDHR

vectorphase1MDHR
vectorphase2MDHR
vectorphase3MDHR
vectorphase4MDHR
vectorphase5MDHR


vectorMDHR
qqnorm(vectorEDHR)
qqline(vectorEDHR)
vectorNDHR
qqnorm(vectorNDHR)
qqline(vectorNDHR)
diffvector = vectorMDHR - vectorNDHR
qqnorm(diffvector)
qqline(diffvector)


b <- boxplot(vectorMDHR, vectorNDHR, xaxt='n',
             xlab="Sessions", ylab="Heart rate",
             main="Boxplot of MD and ND Sessions for HR")
means <- c(mean(vectorMDHR), mean(vectorNDHR))
points(means, pch=8, col="red")
axis(1, at=1:2, labels=c("ED","ND"))
text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
abline(v=mean(1:length(b$n)), lty=3)