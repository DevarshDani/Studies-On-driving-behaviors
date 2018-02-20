#library(gdata)
#install.packages("readxl")
#library(readxl)
library(xlsx)
path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??CD", list.dirs(), value = TRUE)
dir2 = grep("T???/??ND", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1CDNDBR = NULL
vectorphase2CDNDBR = NULL
vectorphase3CDNDBR = NULL
vectorphase4CDNDBR = NULL
vectorphase5CDNDBR = NULL

vectorphase1CDBR = NULL
vectorphase2CDBR = NULL
vectorphase3CDBR = NULL
vectorphase4CDBR = NULL
vectorphase5CDBR = NULL

notopen = NULL
noBRND = NULL
noBRCD = NULL
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
    noBRCD = append(noBRCD, i)
    next
  }
  breatherateCD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  breatherateCD
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
  
  #breatherateCD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(breatherateCD)[1] <- "Time"
  names(breatherateCD)[2] <- "Breathe.rate"
  breatherateCD
  breatherateCD = breatherateCD[-1,]
  breatherateCD = breatherateCD[order(breatherateCD$Time), , drop = FALSE]
  breatherateCD$Breathe.rate <- as.numeric(as.character(breatherateCD$Breathe.rate))
  breatherateCD$Time <- as.numeric(as.character(breatherateCD$Time))
  breatherateCD = breatherateCD[order(breatherateCD$Time), , drop = FALSE]
  #breatherate[with(breatherate, !(as.numeric(breatherate$Time) < Stime1)), ]
  invalidValue1 = which(breatherateCD$Breathe.rate < 4)
  invalidValue2 = which(breatherateCD$Breathe.rate > 70)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #breatherateCD = breatherateCD[!(as.numeric(breatherateCD$Breathe.rate) < 4 
  #                            | as.numeric(breatherateCD$Breathe.rate) > 70), ]
  phase1CD = breatherateCD[(as.numeric(breatherateCD$Time) < Stime1), ]
  phase2CD = breatherateCD[(as.numeric(breatherateCD$Time) > Stime1 & as.numeric(breatherateCD$Time) < Etime1),] 
  phase3CD = breatherateCD[(as.numeric(breatherateCD$Time) > Etime1 & as.numeric(breatherateCD$Time) < Stime2), ]
  phase4CD = breatherateCD[(as.numeric(breatherateCD$Time) > Stime2 & as.numeric(breatherateCD$Time) < Etime2 ), ]
  phase5CD = breatherateCD[(as.numeric(breatherateCD$Time) > Etime2), ]
  phase1CD
  phase2CD
  phase3CD
  phase4CD
  phase5CD
  #filter(breatherate, as.numeric(breatherate$Time) < Stime1)
  breatherateCD
  
    meanphase1CDBR = mean(phase1CD$Breathe.rate)
    meanphase2CDBR = mean(phase2CD$Breathe.rate)
    meanphase3CDBR = mean(phase3CD$Breathe.rate)
    meanphase4CDBR = mean(phase4CD$Breathe.rate)
    meanphase5CDBR = mean(phase5CD$Breathe.rate)
    
  #meanCDBR = mean(breatherateCD$Breathe.rate)
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
  invalidValue1 = which(breatherateND$Breathe.rate < 4)
  invalidValue2 = which(breatherateND$Breathe.rate > 70)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #breatherateND = breatherateND[!(as.numeric(breatherateND$Breathe.rate) < 4 
  #                            | as.numeric(breatherateND$Breathe.rate) > 70), ]
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
  
    meanphase1NDBR = mean(phase1ND$Breathe.rate)
    meanphase2NDBR = mean(phase2ND$Breathe.rate)
    meanphase3NDBR = mean(phase3ND$Breathe.rate)
    meanphase4NDBR = mean(phase4ND$Breathe.rate)
    meanphase5NDBR = mean(phase5ND$Breathe.rate)
  
  
  
  
    vectorphase1CDBR = append(vectorphase1CDBR, meanphase1CDBR)
    vectorphase1CDNDBR = append(vectorphase1CDNDBR, meanphase1NDBR)
  
  
    vectorphase2CDNDBR = append(vectorphase2CDNDBR, meanphase2NDBR)
    vectorphase2CDBR = append(vectorphase2CDBR, meanphase2CDBR)
   
  
  
    vectorphase3CDNDBR = append(vectorphase3CDNDBR, meanphase3NDBR)
    vectorphase3CDBR = append(vectorphase3CDBR, meanphase3CDBR)
  
  
    vectorphase4CDNDBR = append(vectorphase4CDNDBR, meanphase4NDBR)
    vectorphase4CDBR = append(vectorphase4CDBR, meanphase4CDBR)
   
  
    vectorphase5CDBR = append(vectorphase5CDBR, meanphase5CDBR)
    vectorphase5CDNDBR = append(vectorphase5CDNDBR, meanphase5NDBR)
  

  #vectorphase1CDNDBR = append(vectorphase1CDNDBR, meanphase1NDBR)
  #vectorphase2CDNDBR = append(vectorphase2CDNDBR, meanphase2NDBR)
  #vectorphase3CDNDBR = append(vectorphase3CDNDBR, meanphase3NDBR)
  #vectorphase4CDNDBR = append(vectorphase4CDNDBR, meanphase4NDBR)
  #vectorphase5CDNDBR = append(vectorphase5CDNDBR, meanphase5NDBR)
  
  #vectorphase1CDBR = append(vectorphase1CDBR, meanphase1CDBR)
  #vectorphase2CDBR = append(vectorphase2CDBR, meanphase2CDBR)
  #vectorphase3CDBR = append(vectorphase3CDBR, meanphase3CDBR)
  #vectorphase4CDBR = append(vectorphase4CDBR, meanphase4CDBR)
  #vectorphase5CDBR = append(vectorphase5CDBR, meanphase5CDBR)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}
#################################################################################################################





path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??ED", list.dirs(), value = TRUE)
dir2 = grep("T???/??ND", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1EDNDBR = NULL
vectorphase2EDNDBR = NULL
vectorphase3EDNDBR = NULL
vectorphase4EDNDBR = NULL
vectorphase5EDNDBR = NULL

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
  invalidValue1 = which(breatherateED$Breathe.rate < 4)
  invalidValue2 = which(breatherateED$Breathe.rate > 70)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #breatherateED = breatherateED[!(as.numeric(breatherateED$Breathe.rate) < 4 
  #                                | as.numeric(breatherateED$Breathe.rate) > 70), ]
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
  
    meanphase1EDBR = mean(phase1ED$Breathe.rate)
    meanphase2EDBR = mean(phase2ED$Breathe.rate)
    meanphase3EDBR = mean(phase3ED$Breathe.rate)
    meanphase4EDBR = mean(phase4ED$Breathe.rate)
    meanphase5EDBR = mean(phase5ED$Breathe.rate)
    
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
  invalidValue1 = which(breatherateND$Breathe.rate < 4)
  invalidValue2 = which(breatherateND$Breathe.rate > 70)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #breatherateND = breatherateND[!(as.numeric(breatherateND$Breathe.rate) < 4 
  #                                | as.numeric(breatherateND$Breathe.rate) > 70), ]
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
  
    meanphase1NDBR = mean(phase1ND$Breathe.rate)
    meanphase2NDBR = mean(phase2ND$Breathe.rate)
    meanphase3NDBR = mean(phase3ND$Breathe.rate)
    meanphase4NDBR = mean(phase4ND$Breathe.rate)
    meanphase5NDBR = mean(phase5ND$Breathe.rate)
  
  
  
  
    vectorphase1EDNDBR = append(vectorphase1EDNDBR, meanphase1NDBR)
    vectorphase1EDBR = append(vectorphase1EDBR, meanphase1EDBR)
  
  
    vectorphase2EDNDBR = append(vectorphase2EDNDBR, meanphase2NDBR)
    vectorphase2EDBR = append(vectorphase2EDBR, meanphase2EDBR)
  
  
    vectorphase3EDNDBR = append(vectorphase3EDNDBR, meanphase3NDBR)
    vectorphase3EDBR = append(vectorphase3EDBR, meanphase3EDBR)
  
  
    vectorphase4EDNDBR = append(vectorphase4EDNDBR, meanphase4NDBR)
    vectorphase4EDBR = append(vectorphase4EDBR, meanphase4EDBR)
  
  
    vectorphase5EDNDBR = append(vectorphase5EDNDBR, meanphase5NDBR)
    vectorphase5EDBR = append(vectorphase5EDBR, meanphase5EDBR)
  
  
  
  #vectorphase1EDNDBR = append(vectorphase1EDNDBR, meanphase1NDBR)
  #vectorphase2EDNDBR = append(vectorphase2EDNDBR, meanphase2NDBR)
  #vectorphase3EDNDBR = append(vectorphase3EDNDBR, meanphase3NDBR)
  #vectorphase4EDNDBR = append(vectorphase4EDNDBR, meanphase4NDBR)
  #vectorphase5EDNDBR = append(vectorphase5EDNDBR, meanphase5NDBR)
  
  #vectorphase1EDBR = append(vectorphase1EDBR, meanphase1EDBR)
  #vectorphase2EDBR = append(vectorphase2EDBR, meanphase2EDBR)
  #vectorphase3EDBR = append(vectorphase3EDBR, meanphase3EDBR)
  #vectorphase4EDBR = append(vectorphase4EDBR, meanphase4EDBR)
  #vectorphase5EDBR = append(vectorphase5EDBR, meanphase5EDBR)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}






################################################################################################################




path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??MD", list.dirs(), value = TRUE)
dir2 = grep("T???/??ND", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1MDNDBR = NULL
vectorphase2MDNDBR = NULL
vectorphase3MDNDBR = NULL
vectorphase4MDNDBR = NULL
vectorphase5MDNDBR = NULL

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
  invalidValue1 = which(breatherateMD$Breathe.rate < 4)
  invalidValue2 = which(breatherateMD$Breathe.rate > 70)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #breatherateMD = breatherateMD[!(as.numeric(breatherateMD$Breathe.rate) < 4 
  #                                | as.numeric(breatherateMD$Breathe.rate) > 70), ]
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
    meanphase1MDBR = mean(phase1MD$Breathe.rate)
    meanphase2MDBR = mean(phase2MD$Breathe.rate)
    meanphase3MDBR = mean(phase3MD$Breathe.rate)
    meanphase4MDBR = mean(phase4MD$Breathe.rate)
    meanphase5MDBR = mean(phase5MD$Breathe.rate)
    
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
  invalidValue1 = which(breatherateND$Breathe.rate < 4)
  invalidValue2 = which(breatherateND$Breathe.rate > 70)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #breatherateND = breatherateND[!(as.numeric(breatherateND$Breathe.rate) < 4 
  #                                | as.numeric(breatherateND$Breathe.rate) > 70), ]
  phase1ND = breatherateND[(as.numeric(breatherateND$Time) < Stime1), ]
  phase2ND = breatherateND[(as.numeric(breatherateND$Time) > Stime1 & as.numeric(breatherateND$Time) < Etime1),] 
  phase3ND = breatherateND[(as.numeric(breatherateND$Time) > Etime1 & as.numeric(breatherateND$Time) < Stime2), ]
  phase4ND = breatherateND[(as.numeric(breatherateND$Time) > Stime2 & as.numeric(breatherateND$Time) < Etime2 ), ]
  phase5ND = breatherateND[(as.numeric(breatherateND$Time) > Etime2), ]
  #filter(breatherate, as.numeric(breatherate$Time) < Stime1)
  #breatherateND
  if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
    meanphase5NDBR = -1
  } else {
    meanphase5NDBR = mean(phase5ND$Breathe.rate)
  }
  
    meanphase1NDBR = mean(phase1ND$Breathe.rate)
    meanphase2NDBR = mean(phase2ND$Breathe.rate)
    meanphase3NDBR = mean(phase3ND$Breathe.rate)
    meanphase4NDBR = mean(phase4ND$Breathe.rate)
    
  
  
  
  
  
    vectorphase1MDNDBR = append(vectorphase1MDNDBR, meanphase1NDBR)
    vectorphase1MDBR = append(vectorphase1MDBR, meanphase1MDBR)
  
  
    vectorphase2MDNDBR = append(vectorphase2MDNDBR, meanphase2NDBR)
    vectorphase2MDBR = append(vectorphase2MDBR, meanphase2MDBR)
  
  
    vectorphase3MDNDBR = append(vectorphase3MDNDBR, meanphase3NDBR)
    vectorphase3MDBR = append(vectorphase3MDBR, meanphase3MDBR)
  
  
    vectorphase4MDNDBR = append(vectorphase4MDNDBR, meanphase4NDBR)
    vectorphase4MDBR = append(vectorphase4MDBR, meanphase4MDBR)
  
  if(meanphase5NDBR != -1) {
    vectorphase5MDNDBR = append(vectorphase5MDNDBR, meanphase5NDBR)
    vectorphase5MDBR = append(vectorphase5MDBR, meanphase5MDBR)
  }
    
  
  
  #vectorphase1MDNDBR = append(vectorphase1MDNDBR, meanphase1NDBR)
  #vectorphase2MDNDBR = append(vectorphase2MDNDBR, meanphase2NDBR)
  #vectorphase3MDNDBR = append(vectorphase3MDNDBR, meanphase3NDBR)
  #vectorphase4MDNDBR = append(vectorphase4MDNDBR, meanphase4NDBR)
  #vectorphase5MDNDBR = append(vectorphase5MDNDBR, meanphase5NDBR)
  
  #vectorphase1MDBR = append(vectorphase1MDBR, meanphase1MDBR)
  #vectorphase2MDBR = append(vectorphase2MDBR, meanphase2MDBR)
  #vectorphase3MDBR = append(vectorphase3MDBR, meanphase3MDBR)
  #vectorphase4MDBR = append(vectorphase4MDBR, meanphase4MDBR)
  #vectorphase5MDBR = append(vectorphase5MDBR, meanphase5MDBR)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}



###############################################################################################################


#vectorphase1NDBR
#vectorphase2NDBR
#vectorphase3NDBR
#vectorphase4NDBR
#vectorphase5NDBR

#vectorphase1CDBR
#vectorphase2CDBR
#vectorphase3CDBR
#vectorphase4CDBR
#vectorphase5CDBR


diffvectorph1CD = vectorphase1CDBR - vectorphase1CDNDBR
diffvectorph2CD = vectorphase2CDBR - vectorphase2CDNDBR
diffvectorph3CD = vectorphase3CDBR - vectorphase3CDNDBR
diffvectorph4CD = vectorphase4CDBR - vectorphase4CDNDBR
diffvectorph5CD = vectorphase5CDBR - vectorphase5CDNDBR

diffvectorph1ED = vectorphase1EDBR - vectorphase1EDNDBR
diffvectorph2ED = vectorphase2EDBR - vectorphase2EDNDBR
diffvectorph3ED = vectorphase3EDBR - vectorphase3EDNDBR
diffvectorph4ED = vectorphase4EDBR - vectorphase4EDNDBR
diffvectorph5ED = vectorphase5EDBR - vectorphase5EDNDBR

diffvectorph1MD = vectorphase1MDBR - vectorphase1MDNDBR
diffvectorph2MD = vectorphase2MDBR - vectorphase2MDNDBR
diffvectorph3MD = vectorphase3MDBR - vectorphase3MDNDBR
diffvectorph4MD = vectorphase4MDBR - vectorphase4MDNDBR
diffvectorph5MD = vectorphase5MDBR - vectorphase5MDNDBR


pdf("C:/Users/Devarsh Dani/Desktop/BRP2.pdf", height = 15, width = 15)
par(mfrow = c(3,5))
colors = c(rep("blue",1))
b <- boxplot(diffvectorph1CD, xaxt='n',
             xlab="Phase 1", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of CD and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2CD, xaxt='n',
             xlab="Phase 2", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of CD and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3CD, xaxt='n',
             xlab="Phase 3", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of CD and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4CD, xaxt='n',
             xlab="Phase 4", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of CD and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5CD, xaxt='n',
             xlab="Phase 5", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of CD and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph1ED, xaxt='n',
             xlab="Phase 1", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of ED and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2ED, xaxt='n',
             xlab="Phase 2", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of ED and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3ED, xaxt='n',
             xlab="Phase 3", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of ED and ND Sessions for BR\n **", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4ED, xaxt='n',
             xlab="Phase 4", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of ED and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5ED, xaxt='n',
             xlab="Phase 5", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of ED and ND Sessions for BR\n ", col = colors, ylim = c(-15,15))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph1MD, xaxt='n',
             xlab="Phase 1", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of MD and ND Sessions for BR\n ", col = colors, ylim = c(-30,30))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2MD, xaxt='n',
             xlab="Phase 2", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of MD and ND Sessions for BR\n ***", col = colors, ylim = c(-30,30))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3MD, xaxt='n',
             xlab="Phase 3", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of MD and ND Sessions for BR\n ", col = colors, ylim = c(-30,30))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4MD, xaxt='n',
             xlab="Phase 4", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of MD and ND Sessions for BR\n ***", col = colors, ylim = c(-30,30))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5MD, xaxt='n',
             xlab="Phase 5", ylab="Difference in Breathing rate [in bpm]",
             main="Boxplot of MD and ND Sessions for BR\n *", col = colors, ylim = c(-30,30))
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

dev.off()

#vectorCDBR
#qqnorm(vectorCDBR)
#qqline(vectorCDBR)
#vectorNDBR
#qqnorm(vectorNDBR)
#qqline(vectorNDBR)
#diffvector = vectorCDBR - vectorNDBR
#qqnorm(diffvector)
#qqline(diffvector)


#b <- boxplot(vectorCDBR, vectorNDBR, xaxt='n',
##             xlab="Sessions", ylab="Breathe rate",
#             main="Boxplot of CD and ND Sessions for BR")
#means <- c(mean(vectorCDBR), mean(vectorNDBR))
#points(means, pch=8, col="red")
#axis(1, at=1:2, labels=c("CD","ND"))
#text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
#abline(v=mean(1:length(b$n)), lty=3)


#diffvectorph5MD


#vectorphase5MDBR 
#vectorphase5MDNDBR

#H0 : mean difference is 0
#H1 : mean difference is not 0
#p-value <= 0.05 the difference between the two paired samples are significantly different. So H1. else H0.

pdf("C:/Users/Devarsh Dani/Desktop/BRQQ.pdf", height = 15, width = 15)
par(mfrow = c(3,5))

qqnorm(diffvectorph1CD, main = "QQ Plot Phase 1 CD vs ND for BR")
qqline(diffvectorph1CD)
shapiro.test(diffvectorph1CD)
# pvalue = 0.03376 not normal almost normal from qqplot assuming outliers
t.test(diffvectorph1CD, conf.level = 0.95)
# p-value = 0.8628
# no significant difference in means.

#diffvectorph1CDT = log10(diffvectorph1CD-min(diffvectorph1CD)+1)
#diffvectorph1CDT
#qqnorm(diffvectorph1CDT)
#qqline(diffvectorph1CDT)
#shapiro.test(diffvectorph1CDT)




qqnorm(diffvectorph2CD, main = "QQ Plot Phase 2 CD vs ND for BR")
qqline(diffvectorph2CD)
shapiro.test(diffvectorph2CD)
# pvalue = 0.1555
t.test(diffvectorph2CD, conf.level = 0.95)
#t.test(vectorphase2CDBR, vectorphase2CDNDBR, paired = TRUE, conf.level = 0.95)
# no significant difference in means. p-value = 0.8192

qqnorm(diffvectorph3CD, main = "QQ Plot Phase 3 CD vs ND for BR")
qqline(diffvectorph3CD)
shapiro.test(diffvectorph3CD)
# pvalue = 0.07948
t.test(diffvectorph3CD, conf.level = 0.95)
#p-value = 0.7267
# no significant difference in means.

qqnorm(diffvectorph4CD, main = "QQ Plot Phase 4 CD vs ND for BR")
qqline(diffvectorph4CD)
shapiro.test(diffvectorph4CD)
# pvalue = 0.02936 not normal almost normal from qqplot assuming outliers
t.test(diffvectorph4CD, conf.level = 0.95)
# p-value = 0.3099
# no significant difference in means.


qqnorm(diffvectorph5CD, main = "QQ Plot Phase 5 CD vs ND for BR")
qqline(diffvectorph5CD)
shapiro.test(diffvectorph5CD)
# pvalue = 0.7182
t.test(diffvectorph5CD, conf.level = 0.95)
# p-value = 0.5249
# no significant difference in means.


qqnorm(diffvectorph1ED, main = "QQ Plot Phase 1 ED vs ND for BR")
qqline(diffvectorph1ED)
shapiro.test(diffvectorph1ED)
# pvalue = 0.005141 not normal almost normal from qqplot assuming outliers
t.test(diffvectorph1ED, conf.level = 0.95)
# p-value = 0.2745
# no significant difference in means.

qqnorm(diffvectorph2ED, main = "QQ Plot Phase 2 ED vs ND for BR")
qqline(diffvectorph2ED)
shapiro.test(diffvectorph2ED)
# pvalue = 0.5523
t.test(diffvectorph2ED, conf.level = 0.95)
# p-value = 0.3565
# no significant difference in means.


qqnorm(diffvectorph3ED, main = "QQ Plot Phase 3 ED vs ND for BR")
qqline(diffvectorph3ED)
shapiro.test(diffvectorph3ED)
diffvectorph3EDT = log(diffvectorph3ED)
# pvalue = 0.01844 not normal almost normal from qqplot assuming outliers
t.test(diffvectorph3ED, conf.level = 0.95)
# p-value = 0.005472
# significant difference in means.

qqnorm(diffvectorph4ED, main = "QQ Plot Phase 4 ED vs ND for BR")
qqline(diffvectorph4ED)
shapiro.test(diffvectorph4ED)
# pvalue = 0.8961
t.test(diffvectorph4ED, conf.level = 0.95)
# p-value = 0.5801
# no significant difference in means.


qqnorm(diffvectorph5ED, main = "QQ Plot Phase 5 ED vs ND for BR")
qqline(diffvectorph5ED)
shapiro.test(diffvectorph5ED)
# pvalue = 0.4332
t.test(diffvectorph5ED, conf.level = 0.95)
# p-value = 0.08363
# no significant difference in means.


qqnorm(diffvectorph1MD, main = "QQ Plot Phase 1 MD vs ND for BR")
qqline(diffvectorph1MD)
shapiro.test(diffvectorph1MD)
# pvalue = 0.00266 not normal almost normal from qqplot assuming outliers
t.test(diffvectorph1MD, conf.level = 0.95)
# p-value = 0.8432
# no significant difference in means.

qqnorm(diffvectorph2MD, main = "QQ Plot Phase 2 MD vs ND for BR")
qqline(diffvectorph2MD)
shapiro.test(diffvectorph2MD)
# pvalue = 0.09115
t.test(diffvectorph2MD, conf.level = 0.95)
# p-value = 1.635e-08
# significant difference in means.



qqnorm(diffvectorph3MD, main = "QQ Plot Phase 3 MD vs ND for BR")
qqline(diffvectorph3MD)
shapiro.test(diffvectorph3MD)
# pvalue = 0.003224 not normal almost normal from qqplot assuming outliers
t.test(diffvectorph3MD, conf.level = 0.95)
# p-value = 0.147
# no significant difference in means.

qqnorm(diffvectorph4MD, main = "QQ Plot Phase 4 MD vs ND for BR")
qqline(diffvectorph4MD)
shapiro.test(diffvectorph4MD)
# pvalue = 0.4601
t.test(diffvectorph4MD, conf.level = 0.95)
# p-value = 1.039e-10
# significant difference in means.


qqnorm(diffvectorph5MD, main = "QQ Plot Phase 5 MD vs ND for BR")
qqline(diffvectorph5MD)
shapiro.test(diffvectorph5MD)
# pvalue = 0.3803 
t.test(diffvectorph5MD, conf.level = 0.95)
# p-value = 0.047
# significant difference in means.
dev.off()