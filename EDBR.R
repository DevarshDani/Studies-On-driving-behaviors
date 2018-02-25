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
vectorphase1NDBR = NULL
vectorphase2NDBR = NULL
vectorphase3NDBR = NULL
vectorphase4NDBR = NULL
vectorphase5NDBR = NULL

vectorphase1EDBR = NULL
vectorphase2EDBR = NULL
vectorphase3EDBR = NULL
vectorphase4EDBR = NULL
vectorphase5EDBR = NULL

notopen = NULL
noBRND = NULL
noBRED = NULL
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
    noBRED = append(noBRED, i)
    next
  }
  breatherateED = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  breatherateED
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
  
  #breatherateED = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(breatherateED)[1] <- "Time"
  names(breatherateED)[2] <- "Breathe.rate"
  breatherateED
  breatherateED = breatherateED[-1,]
  breatherateED = breatherateED[order(breatherateED$Time), , drop = FALSE]
  breatherateED$Breathe.rate <- as.numeric(as.character(breatherateED$Breathe.rate))
  breatherateED$Time <- as.numeric(as.character(breatherateED$Time))
  breatherateED = breatherateED[order(breatherateED$Time), , drop = FALSE]
  #breatherate[with(breatherate, !(as.numeric(breatherate$Time) < Stime1)), ]
  breatherateED = breatherateED[!(as.numeric(breatherateED$Breathe.rate) < 4 
                                  | as.numeric(breatherateED$Breathe.rate) > 70), ]
  phase1ED = breatherateED[(as.numeric(breatherateED$Time) < Stime1), ]
  phase2ED = breatherateED[(as.numeric(breatherateED$Time) > Stime1 & as.numeric(breatherateED$Time) < Etime1),] 
  phase3ED = breatherateED[(as.numeric(breatherateED$Time) > Etime1 & as.numeric(breatherateED$Time) < Stime2), ]
  phase4ED = breatherateED[(as.numeric(breatherateED$Time) > Stime2 & as.numeric(breatherateED$Time) < Etime2 ), ]
  phase5ED = breatherateED[(as.numeric(breatherateED$Time) > Etime2), ]
  phase1ED
  phase2ED
  phase3ED
  phase4ED
  phase5ED
  #filter(breatherate, as.numeric(breatherate$Time) < Stime1)
  breatherateED
  if(is.data.frame(phase1ED) && nrow(phase1ED)==0) {
    meanphase1EDBR = 0
  } else {
    meanphase1EDBR = mean(phase1ED$Breathe.rate)
  }
  if(is.data.frame(phase2ED) && nrow(phase2ED)==0) {
    meanphase2EDBR = 0
  } else {
    meanphase2EDBR = mean(phase2ED$Breathe.rate)
  }
  if(is.data.frame(phase3ED) && nrow(phase3ED)==0) {
    meanphase3EDBR = 0
  } else {
    meanphase3EDBR = mean(phase3ED$Breathe.rate)
  }
  if(is.data.frame(phase4ED) && nrow(phase4ED)==0) {
    meanphase4EDBR = 0
  } else {
    meanphase4EDBR = mean(phase4ED$Breathe.rate)
  }
  if(is.data.frame(phase5ED) && nrow(phase5ED)==0) {
    meanphase5EDBR = 0
  } else {
    meanphase5EDBR = mean(phase5ED$Breathe.rate)
  }
  #meanEDBR = mean(breatherateED$Breathe.rate)
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
  
  vectorphase1EDBR = append(vectorphase1EDBR, meanphase1EDBR)
  vectorphase2EDBR = append(vectorphase2EDBR, meanphase2EDBR)
  vectorphase3EDBR = append(vectorphase3EDBR, meanphase3EDBR)
  vectorphase4EDBR = append(vectorphase4EDBR, meanphase4EDBR)
  vectorphase5EDBR = append(vectorphase5EDBR, meanphase5EDBR)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




vectorphase1NDBR
vectorphase2NDBR
vectorphase3NDBR
vectorphase4NDBR
vectorphase5NDBR

vectorphase1EDBR
vectorphase2EDBR
vectorphase3EDBR
vectorphase4EDBR
vectorphase5EDBR


vectorEDBR
qqnorm(vectorEDBR)
qqline(vectorEDBR)
vectorNDBR
qqnorm(vectorNDBR)
qqline(vectorNDBR)
diffvector = vectorEDBR - vectorNDBR
qqnorm(diffvector)
qqline(diffvector)


b <- boxplot(vectorEDBR, vectorNDBR, xaxt='n',
             xlab="Sessions", ylab="Breathe rate",
             main="Boxplot of ED and ND Sessions for BR")
means <- c(mean(vectorEDBR), mean(vectorNDBR))
points(means, pch=8, col="red")
axis(1, at=1:2, labels=c("ED","ND"))
text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
abline(v=mean(1:length(b$n)), lty=3)
