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
vectorphase1CDNDHR = NULL
vectorphase2CDNDHR = NULL
vectorphase3CDNDHR = NULL
vectorphase4CDNDHR = NULL
vectorphase5CDNDHR = NULL

vectorphase1CDHR = NULL
vectorphase2CDHR = NULL
vectorphase3CDHR = NULL
vectorphase4CDHR = NULL
vectorphase5CDHR = NULL

notopen = NULL
noHRND = NULL
noHRCD = NULL
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
    noHRCD = append(noHRCD, i)
    next
  }
  heartrateCD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  heartrateCD
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
  names(heartrateCD)[1] <- "Time"
  names(heartrateCD)[2] <- "Heart.Rate"
  heartrateCD
  heartrateCD = heartrateCD[-1,]
  heartrateCD = heartrateCD[order(heartrateCD$Time), , drop = FALSE]
  heartrateCD$Heart.Rate <- as.numeric(as.character(heartrateCD$Heart.Rate))
  heartrateCD$Time <- as.numeric(as.character(heartrateCD$Time))
  heartrateCD = heartrateCD[order(heartrateCD$Time), , drop = FALSE]
  #heartrate[with(heartrate, !(as.numeric(heartrate$Time) < Stime1)), ]
  invalidValue1 = which(heartrateCD$Heart.Rate < 40)
  invalidValue2 = which(heartrateCD$Heart.Rate > 120)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #heartrateCD = heartrateCD[!(as.numeric(heartrateCD$Heart.Rate) <= 40 
  #                            | as.numeric(heartrateCD$Heart.Rate) >= 120), ]
  phase1CD = heartrateCD[(as.numeric(heartrateCD$Time) < Stime1), ]
  phase2CD = heartrateCD[(as.numeric(heartrateCD$Time) > Stime1 & as.numeric(heartrateCD$Time) < Etime1),] 
  phase3CD = heartrateCD[(as.numeric(heartrateCD$Time) > Etime1 & as.numeric(heartrateCD$Time) < Stime2), ]
  phase4CD = heartrateCD[(as.numeric(heartrateCD$Time) > Stime2 & as.numeric(heartrateCD$Time) < Etime2 ), ]
  phase5CD = heartrateCD[(as.numeric(heartrateCD$Time) > Etime2), ]
  phase1CD
  phase2CD
  phase3CD
  phase4CD
  phase5CD
  #filter(heartrate, as.numeric(heartrate$Time) < Stime1)
  heartrateCD
    meanphase1CDHR = mean(phase1CD$Heart.Rate)
    meanphase2CDHR = mean(phase2CD$Heart.Rate)
    meanphase3CDHR = mean(phase3CD$Heart.Rate)
    meanphase4CDHR = mean(phase4CD$Heart.Rate)
    meanphase5CDHR = mean(phase5CD$Heart.Rate)
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
  invalidValue1 = which(heartrateND$Heart.Rate < 40)
  invalidValue2 = which(heartrateND$Heart.Rate > 120)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #heartrateND = heartrateND[!(as.numeric(heartrateND$Heart.Rate) <= 40 
  #                            | as.numeric(heartrateND$Heart.Rate) >= 120), ]
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
    meanphase1NDHR = mean(phase1ND$Heart.Rate)
    meanphase2NDHR = mean(phase2ND$Heart.Rate)
    meanphase3NDHR = mean(phase3ND$Heart.Rate)
    meanphase4NDHR = mean(phase4ND$Heart.Rate)
    meanphase5NDHR = mean(phase5ND$Heart.Rate)
  
    
    
    vectorphase1CDNDHR = append(vectorphase1CDNDHR, meanphase1NDHR)
    vectorphase1CDHR = append(vectorphase1CDHR, meanphase1CDHR)
  
    vectorphase2CDNDHR = append(vectorphase2CDNDHR, meanphase2NDHR)
    vectorphase2CDHR = append(vectorphase2CDHR, meanphase2CDHR)
  
  
    vectorphase3CDHR = append(vectorphase3CDHR, meanphase3CDHR)
    vectorphase3CDNDHR = append(vectorphase3CDNDHR, meanphase3NDHR)
  
  
    vectorphase4CDNDHR = append(vectorphase4CDNDHR, meanphase4NDHR)
    vectorphase4CDHR = append(vectorphase4CDHR, meanphase4CDHR)
  
  
    vectorphase5CDNDHR = append(vectorphase5CDNDHR, meanphase5NDHR)
    vectorphase5CDHR = append(vectorphase5CDHR, meanphase5CDHR)
  
  
  
  
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}



###################################################################################################################



path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??MD", list.dirs(), value = TRUE)
dir2 = grep("T???/??ND", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1MDNDHR = NULL
vectorphase2MDNDHR = NULL
vectorphase3MDNDHR = NULL
vectorphase4MDNDHR = NULL
vectorphase5MDNDHR = NULL

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
  invalidValue1 = which(heartrateMD$Heart.Rate < 40)
  invalidValue2 = which(heartrateMD$Heart.Rate > 120)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #heartrateMD = heartrateMD[!(as.numeric(heartrateMD$Heart.Rate) <= 40 
  #                            | as.numeric(heartrateMD$Heart.Rate) >= 120), ]
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
  
    meanphase1MDHR = mean(phase1MD$Heart.Rate)
    meanphase2MDHR = mean(phase2MD$Heart.Rate)
    meanphase3MDHR = mean(phase3MD$Heart.Rate)
    meanphase4MDHR = mean(phase4MD$Heart.Rate)
    meanphase5MDHR = mean(phase5MD$Heart.Rate)
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
  invalidValue1 = which(heartrateND$Heart.Rate < 40)
  invalidValue2 = which(heartrateND$Heart.Rate > 120)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #heartrateND = heartrateND[!(as.numeric(heartrateND$Heart.Rate) <= 40 
  #                            | as.numeric(heartrateND$Heart.Rate) >= 140), ]
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

    meanphase1NDHR = mean(phase1ND$Heart.Rate)
    meanphase2NDHR = mean(phase2ND$Heart.Rate)
    meanphase3NDHR = mean(phase3ND$Heart.Rate)
    meanphase4NDHR = mean(phase4ND$Heart.Rate)
    meanphase5NDHR = mean(phase5ND$Heart.Rate)
  
  
  
    vectorphase1MDNDHR = append(vectorphase1MDNDHR, meanphase1NDHR)
    vectorphase1MDHR = append(vectorphase1MDHR, meanphase1MDHR)
  
  
    vectorphase2MDNDHR = append(vectorphase2MDNDHR, meanphase2NDHR)
    vectorphase2MDHR = append(vectorphase2MDHR, meanphase2MDHR)
  
  
    vectorphase3MDNDHR = append(vectorphase3MDNDHR, meanphase3NDHR)
    vectorphase3MDHR = append(vectorphase3MDHR, meanphase3MDHR)
  
  
    vectorphase4MDNDHR = append(vectorphase4MDNDHR, meanphase4NDHR)
    vectorphase4MDHR = append(vectorphase4MDHR, meanphase4MDHR)
  
  
    vectorphase5MDNDHR = append(vectorphase5MDNDHR, meanphase5NDHR)
    vectorphase5MDHR = append(vectorphase5MDHR, meanphase5MDHR)
  
    vectorphase1MDNDHR
    vectorphase1MDHR
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




##################################################################################################################




path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??ED", list.dirs(), value = TRUE)
dir2 = grep("T???/??ND", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1EDNDHR = NULL
vectorphase2EDNDHR = NULL
vectorphase3EDNDHR = NULL
vectorphase4EDNDHR = NULL
vectorphase5EDNDHR = NULL

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
  invalidValue1 = which(heartrateED$Heart.Rate < 40)
  invalidValue2 = which(heartrateED$Heart.Rate > 120)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #heartrateED = heartrateED[!(as.numeric(heartrateED$Heart.Rate) <= 40 
  #                            | as.numeric(heartrateED$Heart.Rate) >= 120), ]
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
  
    meanphase1EDHR = mean(phase1ED$Heart.Rate)
    meanphase2EDHR = mean(phase2ED$Heart.Rate)
    meanphase3EDHR = mean(phase3ED$Heart.Rate)
    meanphase4EDHR = mean(phase4ED$Heart.Rate)
    meanphase5EDHR = mean(phase5ED$Heart.Rate)
    
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
  invalidValue1 = which(heartrateND$Heart.Rate < 40)
  invalidValue2 = which(heartrateND$Heart.Rate > 120)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #heartrateND = heartrateND[!(as.numeric(heartrateND$Heart.Rate) <= 40 
  #                            | as.numeric(heartrateND$Heart.Rate) >= 120), ]
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

    meanphase1NDHR = mean(phase1ND$Heart.Rate)
    meanphase2NDHR = mean(phase2ND$Heart.Rate)
    meanphase3NDHR = mean(phase3ND$Heart.Rate)
    meanphase4NDHR = mean(phase4ND$Heart.Rate)
    meanphase5NDHR = mean(phase5ND$Heart.Rate)
  
  
    vectorphase1EDNDHR = append(vectorphase1EDNDHR, meanphase1NDHR)
    vectorphase1EDHR = append(vectorphase1EDHR, meanphase1EDHR)
  
  
    vectorphase2EDNDHR = append(vectorphase2EDNDHR, meanphase2NDHR)
    vectorphase2EDHR = append(vectorphase2EDHR, meanphase2EDHR)
  
  
    vectorphase3EDNDHR = append(vectorphase3EDNDHR, meanphase3NDHR)
    vectorphase3EDHR = append(vectorphase3EDHR, meanphase3EDHR)
  
  
    vectorphase4EDNDHR = append(vectorphase4EDNDHR, meanphase4NDHR)
    vectorphase4EDHR = append(vectorphase4EDHR, meanphase4EDHR)
  
  
    vectorphase5EDNDHR = append(vectorphase5EDNDHR, meanphase5NDHR)
    vectorphase5EDHR = append(vectorphase5EDHR, meanphase5EDHR)
  
  
  
  
  
  
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}




#################################################################################################################
#vectorphase1CDNDHR
#vectorphase2CDNDHR
#vectorphase3CDNDHR
#vectorphase4CDNDHR
#vectorphase5CDNDHR

diffvectorph1CD = vectorphase1CDHR - vectorphase1CDNDHR
diffvectorph2CD = vectorphase2CDHR - vectorphase2CDNDHR
diffvectorph3CD = vectorphase3CDHR - vectorphase3CDNDHR
diffvectorph4CD = vectorphase4CDHR - vectorphase4CDNDHR
diffvectorph5CD = vectorphase5CDHR - vectorphase5CDNDHR


diffvectorph1ED = vectorphase1EDHR - vectorphase1EDNDHR
diffvectorph2ED = vectorphase2EDHR - vectorphase2EDNDHR
diffvectorph3ED = vectorphase3EDHR - vectorphase3EDNDHR
diffvectorph4ED = vectorphase4EDHR - vectorphase4EDNDHR
diffvectorph5ED = vectorphase5EDHR - vectorphase5EDNDHR


diffvectorph1MD = vectorphase1MDHR - vectorphase1MDNDHR
diffvectorph2MD = vectorphase2MDHR - vectorphase2MDNDHR
diffvectorph3MD = vectorphase3MDHR - vectorphase3MDNDHR
diffvectorph4MD = vectorphase4MDHR - vectorphase4MDNDHR
diffvectorph5MD = vectorphase5MDHR - vectorphase5MDNDHR



#vectorphase2CDHR
#vectorphase3CDHR
#vectorphase4CDHR
#vectorphase5CDHR


#vectorCDHR
#qqnorm(vectorCDHR)
#qqline(vectorCDHR)
#vectorNDHR
#qqnorm(vectorNDHR)
#qqline(vectorNDHR)
#diffvector = vectorCDHR - vectorNDHR
#qqnorm(diffvector)
#qqline(diffvector)


#b <- boxplot(vectorCDHR, vectorNDHR, xaxt='n',
#             xlab="Sessions", ylab="Heart rate",
#             main="Boxplot of CD and ND Sessions for HR")
#means <- c(mean(vectorCDHR), mean(vectorNDHR))
#points(means, pch=8, col="red")
#axis(1, at=1:2, labels=c("CD","ND"))
#text(1:length(b$n), b$stats[5,]+3, paste("n=", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

pdf("C:/Users/Devarsh Dani/Desktop/HRP2.pdf", height = 15, width = 15)
par(mfrow = c(3,5))
colors = c(rep("blue",1))
b <- boxplot(diffvectorph1CD, xaxt='n',
             xlab="Phase 1", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of CD and ND Sessions for HR\n ", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2CD, xaxt='n',
             xlab="Phase 2", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of CD and ND Sessions for HR\n ***", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3CD, xaxt='n',
             xlab="Phase 3", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of CD and ND Sessions for HR\n ", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4CD, xaxt='n',
             xlab="Phase 4", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of CD and ND Sessions for HR\n ***", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5CD, xaxt='n',
             xlab="Phase 5", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of CD and ND Sessions for HR\n ", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph1ED, xaxt='n',
             xlab="Phase 1", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of ED and ND Sessions for HR\n ", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2ED, xaxt='n',
             xlab="Phase 2", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of ED and ND Sessions for HR\n ***", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3ED, xaxt='n',
             xlab="Phase 3", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of ED and ND Sessions for HR\n ", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4ED, xaxt='n',
             xlab="Phase 4", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of ED and ND Sessions for HR\n *", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5ED, xaxt='n',
             xlab="Phase 5", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of ED and ND Sessions for HR\n *", ylim = c(-15,15), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph1MD, xaxt='n',
             xlab="Phase 1", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of MD and ND Sessions for HR\n ", ylim = c(-50,50), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2MD, xaxt='n',
             xlab="Phase 2", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of MD and ND Sessions for HR\n ", ylim = c(-50,50), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3MD, xaxt='n',
             xlab="Phase 3", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of MD and ND Sessions for HR\n ", ylim = c(-50,50), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4MD, xaxt='n',
             xlab="Phase 4", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of MD and ND Sessions for HR\n ", ylim = c(-50,50), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5MD, xaxt='n',
             xlab="Phase 5", ylab="Difference in Heart rate [in bpm]",
             main="Boxplot of MD and ND Sessions for HR\n ", ylim = c(-50,50), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+3, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

dev.off()

#qqnorm(vectorphase1CDNDHR)
#qqline(vectorphase1CDNDHR)
#qqnorm(vectorphase1CDHR)
#qqline(vectorphase1CDHR)
#t.test(vectorphase1CDNDHR, vectorphase1CDHR, conf.level = 0.95, paired = TRUE)

#qqnorm(vectorphase2CDNDHR)
#qqline(vectorphase2CDNDHR)
#qqnorm(vectorphase2CDHR)
#qqline(vectorphase2CDHR)
#t.test(vectorphase2CDNDHR, vectorphase2CDHR, conf.level = 0.95, paired = TRUE)

#qqnorm(vectorphase3CDNDHR)
#qqline(vectorphase3CDNDHR)
#qqnorm(vectorphase3CDHR)
#qqline(vectorphase3CDHR)
#t.test(vectorphase3CDNDHR, vectorphase3CDHR, conf.level = 0.95, paired = TRUE)


pdf("C:/Users/Devarsh Dani/Desktop/HRQQ.pdf", height = 15, width = 15)
par(mfrow = c(3,5))

qqnorm(diffvectorph1CD, main = "QQ Plot Phase 1 CD vs ND for HR")
qqline(diffvectorph1CD)
shapiro.test(diffvectorph1CD)
# pvalue = 0.1189
t.test(diffvectorph1CD, conf.level = 0.95)
#p-value = 0.1227
# no significant difference in means.



qqnorm(diffvectorph2CD, main = "QQ Plot Phase 2 CD vs ND for HR")
qqline(diffvectorph2CD)
shapiro.test(diffvectorph2CD)
# pvalue = 0.0995
t.test(diffvectorph2CD, conf.level = 0.95)
#p-value = 1.076e-07
# significant difference in means.


qqnorm(diffvectorph3CD, main = "QQ Plot Phase 3 CD vs ND for HR")
qqline(diffvectorph3CD)
shapiro.test(diffvectorph3CD)
#diffvectorph3CDT = log(diffvectorph3CD - min(diffvectorph3CD) + 2)
#shapiro.test(diffvectorph3CDT)
# pvalue = 0.03531 not normal not normal outliers so considering normality
t.test(diffvectorph3CD, conf.level = 0.95)
# p-value = 0.6081
# no significant difference in means.


qqnorm(diffvectorph4CD, main = "QQ Plot Phase 4 CD vs ND for HR")
qqline(diffvectorph4CD)
shapiro.test(diffvectorph4CD)
# pvalue = 0.05202
t.test(diffvectorph4CD, conf.level = 0.95)
#p-value = 0.0001221
# significant difference in means.


qqnorm(diffvectorph5CD, main = "QQ Plot Phase 5 CD vs ND for HR")
qqline(diffvectorph5CD)
shapiro.test(diffvectorph5CD)
# pvalue = 0.4019
t.test(diffvectorph5CD, conf.level = 0.95)
#p-value = 0.2265
# no significant difference in means.


qqnorm(diffvectorph1ED, main = "QQ Plot Phase 1 ED vs ND for HR")
qqline(diffvectorph1ED)
shapiro.test(diffvectorph1ED)
# pvalue = 0.004177 not normal outliers so considering normality
t.test(diffvectorph1ED, conf.level = 0.95)
# p-value = 0.1265
# no significant difference in means.

qqnorm(diffvectorph2ED, main = "QQ Plot Phase 2 ED vs ND for HR")
qqline(diffvectorph2ED)
shapiro.test(diffvectorph2ED)
# pvalue = 0.07382
t.test(diffvectorph2ED, conf.level = 0.95)
#p-value = 0.0003584
# significant difference in means.


qqnorm(diffvectorph3ED, main = "QQ Plot Phase 3 ED vs ND for HR")
qqline(diffvectorph3ED)
shapiro.test(diffvectorph3ED)
# pvalue = 0.2814
t.test(diffvectorph3ED, conf.level = 0.95)
# p-value = 0.7332
# no significant difference in means.


qqnorm(diffvectorph4ED, main = "QQ Plot Phase 4 ED vs ND for HR")
qqline(diffvectorph4ED)
shapiro.test(diffvectorph4ED)
# pvalue = 0.9652
t.test(diffvectorph4ED, conf.level = 0.95)
# p-value = 0.03082
# significant difference in means.


qqnorm(diffvectorph5ED, main = "QQ Plot Phase 5 ED vs ND for HR")
qqline(diffvectorph5ED)
shapiro.test(diffvectorph5ED)
# pvalue = 0.01512 not normal
t.test(diffvectorph5ED, conf.level = 0.95)
# p-value = 0.04045
# significant difference in means.

qqnorm(diffvectorph1MD, main = "QQ Plot Phase 1 MD vs ND for HR")
qqline(diffvectorph1MD)
shapiro.test(diffvectorph1MD)
# pvalue = 0.04454 not normal outliers so considering normal
t.test(diffvectorph1MD, conf.level = 0.95)
# p-value = p-value = 0.7936
# no significant difference in means.

qqnorm(diffvectorph2MD, main = "QQ Plot Phase 2 MD vs ND for HR")
qqline(diffvectorph2MD)
shapiro.test(diffvectorph2MD)
# pvalue = 0.1805
t.test(diffvectorph2MD, conf.level = 0.95)
# p-value = 0.1242
# no significant difference in means.


qqnorm(diffvectorph3MD, main = "QQ Plot Phase 3 MD vs ND for HR")
qqline(diffvectorph3MD)
shapiro.test(diffvectorph3MD)
# pvalue = 0.5744
t.test(diffvectorph3MD, conf.level = 0.95)
# p-value = 0.8197
# no significant difference in means.

qqnorm(diffvectorph4MD, main = "QQ Plot Phase 4 MD vs ND for HR")
qqline(diffvectorph4MD)
shapiro.test(diffvectorph4MD)
# pvalue = 0.5432
t.test(diffvectorph4MD, conf.level = 0.95)
# p-value = 0.4195
# no significant difference in means.


qqnorm(diffvectorph5MD, main = "QQ Plot Phase 5 MD vs ND for HR")
qqline(diffvectorph5MD)
shapiro.test(diffvectorph5MD)
# pvalue = 0.2056
#p5MD = t.test(diffvectorph5MD, conf.level = 0.95)
#p5MD$p.value
#p.ad
#p.adjust(p5MD$p.value, method = "bonf")
t.test(diffvectorph5MD, conf.level = 0.95)
# p-value = 0.5144
# no significant difference in means.
dev.off()