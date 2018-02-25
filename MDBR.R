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
vectorphase1NDBR = NULL
vectorphase2NDBR = NULL
vectorphase3NDBR = NULL
vectorphase4NDBR = NULL
vectorphase5NDBR = NULL

vectorphase1MDBR = NULL
vectorphase2MDBR = NULL
vectorphase3MDBR = NULL
vectorphase4MDBR = NULL
vectorphase5MDBR = NULL

notopen = NULL
noBRND = NULL
noBRMD = NULL
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
  fil2 = list.files(pattern = ".BR")
  fil2
  if(identical(fil2, character(0))) {
    setwd("../..")
    unlink(dir[i])
    noBRMD = append(noBRMD, i)
    next
  }
  breatherateMD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  breatherateMD
  setwd("../..")
  unlink(dir[i])
  setwd(dir2[i])
  fil3 = list.files(pattern = ".BR")
  if(identical(fil3, character(0))) {
    setwd("../..")
    unlink(dir2[i])
    noBRND = append(noBRND, i)
    next
  }
  breatherateND = read.xlsx(fil3, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  breatherateND
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
  
  #fil2 = list.files(pattern = ".BR")
  #fil2
  #if(identical(fil2, character(0))) {
  #  setwd("../..")
  #  unlink(dir[i])
  #  noBR = append(noBR, i)
  #  next
  #}
  
  #breatherateMD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(breatherateMD)[1] <- "Time"
  names(breatherateMD)[2] <- "Breathe.rate"
  breatherateMD
  breatherateMD = breatherateMD[-1,]
  breatherateMD = breatherateMD[order(breatherateMD$Time), , drop = FALSE]
  breatherateMD$Breathe.rate <- as.numeric(as.character(breatherateMD$Breathe.rate))
  breatherateMD$Time <- as.numeric(as.character(breatherateMD$Time))
  breatherateMD = breatherateMD[order(breatherateMD$Time), , drop = FALSE]
  #breatherate[with(breatherate, !(as.numeric(breatherate$Time) < Stime1)), ]
  breatherateMD = breatherateMD[!(as.numeric(breatherateMD$Breathe.rate) < 4 
                                  | as.numeric(breatherateMD$Breathe.rate) > 70), ]
  phase1MD = breatherateMD[(as.numeric(breatherateMD$Time) < Stime1), ]
  phase2MD = breatherateMD[(as.numeric(breatherateMD$Time) > Stime1 & as.numeric(breatherateMD$Time) < Etime1),] 
  phase3MD = breatherateMD[(as.numeric(breatherateMD$Time) > Etime1 & as.numeric(breatherateMD$Time) < Stime2), ]
  phase4MD = breatherateMD[(as.numeric(breatherateMD$Time) > Stime2 & as.numeric(breatherateMD$Time) < Etime2 ), ]
  phase5MD = breatherateMD[(as.numeric(breatherateMD$Time) > Etime2), ]
  phase1MD
  phase2MD
  phase3MD
  phase4MD
  phase5MD
  #filter(breatherate, as.numeric(breatherate$Time) < Stime1)
  breatherateMD
  if(is.data.frame(phase1MD) && nrow(phase1MD)==0) {
    meanphase1MDBR = 0
  } else {
    meanphase1MDBR = mean(phase1MD$Breathe.rate)
  }
  if(is.data.frame(phase2MD) && nrow(phase2MD)==0) {
    meanphase2MDBR = 0
  } else {
    meanphase2MDBR = mean(phase2MD$Breathe.rate)
  }
  if(is.data.frame(phase3MD) && nrow(phase3MD)==0) {
    meanphase3MDBR = 0
  } else {
    meanphase3MDBR = mean(phase3MD$Breathe.rate)
  }
  if(is.data.frame(phase4MD) && nrow(phase4MD)==0) {
    meanphase4MDBR = 0
  } else {
    meanphase4MDBR = mean(phase4MD$Breathe.rate)
  }
  if(is.data.frame(phase5MD) && nrow(phase5MD)==0) {
    meanphase5MDBR = 0
  } else {
    meanphase5MDBR = mean(phase5MD$Breathe.rate)
  }
  #meanMDBR = mean(breatherateMD$Breathe.rate)
  #if(is.nan(meanBR)==TRUE){
  #  break
  #}
  #setwd("../..")
  #unlink(dir[i])
  
  
  
  
  #setwd(dir2[i])
  #fil3 = list.files(pattern = ".BR")
  #if(identical(fil3, character(0))) {
  #  setwd("../..")
  #  unlink(dir2[i])
  #noBR = append(noBR, i)
  #  next
  #}
  #breatherateND = read.xlsx(fil3, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(breatherateND)[1] <- "Time"
  names(breatherateND)[2] <- "Breathe.rate"
  breatherateND = breatherateND[-1,]
  breatherateND = breatherateND[order(breatherateND$Time), , drop = FALSE]
  breatherateND$Breathe.rate <- as.numeric(as.character(breatherateND$Breathe.rate))
  breatherateND$Time <- as.numeric(as.character(breatherateND$Time))
  breatherateND = breatherateND[order(breatherateND$Time), , drop = FALSE]
  #breatherate[with(breatherate, !(as.numeric(breatherate$Time) < Stime1)), ]
  breatherateND = breatherateND[!(as.numeric(breatherateND$Breathe.rate) < 4 
                                  | as.numeric(breatherateND$Breathe.rate) > 70), ]
  phase1ND = breatherateND[(as.numeric(breatherateND$Time) < Stime1), ]
  phase2ND = breatherateND[(as.numeric(breatherateND$Time) > Stime1 & as.numeric(breatherateND$Time) < Etime1),] 
  phase3ND = breatherateND[(as.numeric(breatherateND$Time) > Etime1 & as.numeric(breatherateND$Time) < Stime2), ]
  phase4ND = breatherateND[(as.numeric(breatherateND$Time) > Stime2 & as.numeric(breatherateND$Time) < Etime2 ), ]
  phase5ND = breatherateND[(as.numeric(breatherateND$Time) > Etime2), ]
  #filter(breatherate, as.numeric(breatherate$Time) < Stime1)
  #breatherateND
  #if(is.data.frame(breatherateND) && nrow(breatherateND)==0) {
  # meanNDBR = 0
  #} else {
  #  meanNDBR = mean(breatherateND$Breathe.rate)
  #}
  if(is.data.frame(phase1ND) && nrow(phase1ND)==0) {
    meanphase1NDBR = 0
  } else {
    meanphase1NDBR = mean(phase1ND$Breathe.rate)
  }
  if(is.data.frame(phase2ND) && nrow(phase2ND)==0) {
    meanphase2NDBR = 0
  } else {
    meanphase2NDBR = mean(phase2ND$Breathe.rate)
  }
  if(is.data.frame(phase3ND) && nrow(phase3ND)==0) {
    meanphase3NDBR = 0
  } else {
    meanphase3NDBR = mean(phase3ND$Breathe.rate)
  }
  if(is.data.frame(phase4ND) && nrow(phase4ND)==0) {
    meanphase4NDBR = 0
  } else {
    meanphase4NDBR = mean(phase4ND$Breathe.rate)
  }
  if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
    meanphase5NDBR = 0
  } else {
    meanphase5NDBR = mean(phase5ND$Breathe.rate)
  }
  
  vectorphase1NDBR = append(vectorphase1NDBR, meanphase1NDBR)
  vectorphase2NDBR = append(vectorphase2NDBR, meanphase2NDBR)
  vectorphase3NDBR = append(vectorphase3NDBR, meanphase3NDBR)
  vectorphase4NDBR = append(vectorphase4NDBR, meanphase4NDBR)
  vectorphase5NDBR = append(vectorphase5NDBR, meanphase5NDBR)
  
  vectorphase1MDBR = append(vectorphase1MDBR, meanphase1MDBR)
  vectorphase2MDBR = append(vectorphase2MDBR, meanphase2MDBR)
  vectorphase3MDBR = append(vectorphase3MDBR, meanphase3MDBR)
  vectorphase4MDBR = append(vectorphase4MDBR, meanphase4MDBR)
  vectorphase5MDBR = append(vectorphase5MDBR, meanphase5MDBR)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




vectorphase1NDBR
vectorphase2NDBR
vectorphase3NDBR
vectorphase4NDBR
vectorphase5NDBR

vectorphase1MDBR
vectorphase2MDBR
vectorphase3MDBR
vectorphase4MDBR
vectorphase5MDBR


vectorMDBR
qqnorm(vectorMDBR)
qqline(vectorMDBR)
vectorNDBR
qqnorm(vectorNDBR)
qqline(vectorNDBR)
diffvector = vectorMDBR - vectorNDBR
qqnorm(diffvector)
qqline(diffvector)


b <- boxplot(vectorMDBR, vectorNDBR, xaxt='n',
             xlab="Sessions", ylab="Breathe rate",
             main="Boxplot of MD and ND Sessions for BR")
means <- c(mean(vectorMDBR), mean(vectorNDBR))
points(means, pch=8, col="red")
axis(1, at=1:2, labels=c("MD","ND"))
text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
abline(v=mean(1:length(b$n)), lty=3)
