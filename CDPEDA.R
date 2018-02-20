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
vectorphase1CDNDPEDA = NULL
vectorphase2CDNDPEDA = NULL
vectorphase3CDNDPEDA = NULL
vectorphase4CDNDPEDA = NULL
vectorphase5CDNDPEDA = NULL

vectorphase1CDPEDA = NULL
vectorphase2CDPEDA = NULL
vectorphase3CDPEDA = NULL
vectorphase4CDPEDA = NULL
vectorphase5CDPEDA = NULL

notopen = NULL
noPEDAND = NULL
noPEDACD = NULL
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
    noPEDACD = append(noPEDACD, i)
    next
  }
  PalmEDACD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  PalmEDACD
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
  
  #PalmEDACD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(PalmEDACD)[1] <- "Time"
  names(PalmEDACD)[2] <- "Palm.EDA"
  PalmEDACD
  PalmEDACD = PalmEDACD[-1,]
  PalmEDACD = PalmEDACD[order(PalmEDACD$Time), , drop = FALSE]
  PalmEDACD$Palm.EDA <- as.numeric(as.character(PalmEDACD$Palm.EDA))
  PalmEDACD$Time <- as.numeric(as.character(PalmEDACD$Time))
  PalmEDACD = PalmEDACD[order(PalmEDACD$Time), , drop = FALSE]
  #PalmEDA[with(PalmEDA, !(as.numeric(PalmEDA$Time) < Stime1)), ]
  invalidValue1 = which(PalmEDACD$Palm.EDA < 10)
  invalidValue2 = which(PalmEDACD$Palm.EDA > 4700)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #PalmEDACD = PalmEDACD[!(as.numeric(PalmEDACD$Palm.EDA) < 10 
  #                                | as.numeric(PalmEDACD$Palm.EDA) > 4700), ]
  phase1CD = PalmEDACD[(as.numeric(PalmEDACD$Time) < Stime1), ]
  phase2CD = PalmEDACD[(as.numeric(PalmEDACD$Time) > Stime1 & as.numeric(PalmEDACD$Time) < Etime1),] 
  phase3CD = PalmEDACD[(as.numeric(PalmEDACD$Time) > Etime1 & as.numeric(PalmEDACD$Time) < Stime2), ]
  phase4CD = PalmEDACD[(as.numeric(PalmEDACD$Time) > Stime2 & as.numeric(PalmEDACD$Time) < Etime2 ), ]
  phase5CD = PalmEDACD[(as.numeric(PalmEDACD$Time) > Etime2), ]
  phase1CD
  phase2CD
  phase3CD
  phase4CD
  phase5CD
  #filter(PalmEDA, as.numeric(PalmEDA$Time) < Stime1)
  PalmEDACD
  #if(is.data.frame(phase1CD) && nrow(phase1CD)==0) {
  #  meanphase1CDPEDA = 0
  #} else {
    meanphase1CDPEDA = mean(phase1CD$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2CD) && nrow(phase2CD)==0) {
  #  meanphase2CDPEDA = 0
  #} else {
    meanphase2CDPEDA = mean(phase2CD$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3CD) && nrow(phase3CD)==0) {
  #  meanphase3CDPEDA = 0
  #} else {
    meanphase3CDPEDA = mean(phase3CD$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4CD) && nrow(phase4CD)==0) {
  #  meanphase4CDPEDA = 0
  #} else {
    meanphase4CDPEDA = mean(phase4CD$Palm.EDA, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5CD) && nrow(phase5CD)==0) {
  #  meanphase5CDPEDA = 0
  #} else {
    meanphase5CDPEDA = mean(phase5CD$Palm.EDA, na.rm = TRUE)
  #}
  #meanCDPEDA = mean(PalmEDACD$Palm.EDA)
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
  invalidValue1 = which(PalmEDAND$Palm.EDA < 10)
  invalidValue2 = which(PalmEDAND$Palm.EDA > 4700)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
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
  
  vectorphase1CDNDPEDA = append(vectorphase1CDNDPEDA, meanphase1NDPEDA)
  vectorphase2CDNDPEDA = append(vectorphase2CDNDPEDA, meanphase2NDPEDA)
  vectorphase3CDNDPEDA = append(vectorphase3CDNDPEDA, meanphase3NDPEDA)
  vectorphase4CDNDPEDA = append(vectorphase4CDNDPEDA, meanphase4NDPEDA)
  vectorphase5CDNDPEDA = append(vectorphase5CDNDPEDA, meanphase5NDPEDA)
  
  vectorphase1CDPEDA = append(vectorphase1CDPEDA, meanphase1CDPEDA)
  vectorphase2CDPEDA = append(vectorphase2CDPEDA, meanphase2CDPEDA)
  vectorphase3CDPEDA = append(vectorphase3CDPEDA, meanphase3CDPEDA)
  vectorphase4CDPEDA = append(vectorphase4CDPEDA, meanphase4CDPEDA)
  vectorphase5CDPEDA = append(vectorphase5CDPEDA, meanphase5CDPEDA)
  
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
  invalidValue1 = which(PalmEDAED$Palm.EDA < 10)
  invalidValue2 = which(PalmEDAED$Palm.EDA > 4700)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #PalmEDAED = PalmEDAED[!(as.numeric(PalmEDAED$Palm.EDA) < 10 
  #                        | as.numeric(PalmEDAED$Palm.EDA) > 4700), ]
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
  invalidValue1 = which(PalmEDAND$Palm.EDA < 10)
  invalidValue2 = which(PalmEDAND$Palm.EDA > 4700)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
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


##################################################################################################################


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
  invalidValue1 = which(PalmEDAMD$Palm.EDA < 10)
  invalidValue2 = which(PalmEDAMD$Palm.EDA > 4700)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
  #PalmEDAMD = PalmEDAMD[!(as.numeric(PalmEDAMD$Palm.EDA) < 10 
  #                        | as.numeric(PalmEDAMD$Palm.EDA) > 4700), ]
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
  invalidValue1 = which(PalmEDAND$Palm.EDA < 10)
  invalidValue2 = which(PalmEDAND$Palm.EDA > 4700)
  if(length(invalidValue1) > 0 || length(invalidValue2) > 0) {
    next
  }
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



#################################################################################################################
vectorphase1NDPEDA
vectorphase2NDPEDA
vectorphase3NDPEDA
vectorphase4NDPEDA
vectorphase5NDPEDA

vectorphase1CDPEDA
vectorphase2CDPEDA
vectorphase3CDPEDA
vectorphase4CDPEDA
vectorphase5CDPEDA

nostm


diffvectorph1CD = vectorphase1CDPEDA - vectorphase1CDNDPEDA
diffvectorph2CD = vectorphase2CDPEDA - vectorphase2CDNDPEDA
diffvectorph3CD = vectorphase3CDPEDA - vectorphase3CDNDPEDA
diffvectorph4CD = vectorphase4CDPEDA - vectorphase4CDNDPEDA
diffvectorph5CD = vectorphase5CDPEDA - vectorphase5CDNDPEDA

diffvectorph1ED = vectorphase1EDPEDA - vectorphase1EDNDPEDA
diffvectorph2ED = vectorphase2EDPEDA - vectorphase2EDNDPEDA
diffvectorph3ED = vectorphase3EDPEDA - vectorphase3EDNDPEDA
diffvectorph4ED = vectorphase4EDPEDA - vectorphase4EDNDPEDA
diffvectorph5ED = vectorphase5EDPEDA - vectorphase5EDNDPEDA

diffvectorph1MD = vectorphase1MDPEDA - vectorphase1MDNDPEDA
diffvectorph2MD = vectorphase2MDPEDA - vectorphase2MDNDPEDA
diffvectorph3MD = vectorphase3MDPEDA - vectorphase3MDNDPEDA
diffvectorph4MD = vectorphase4MDPEDA - vectorphase4MDNDPEDA
diffvectorph5MD = vectorphase5MDPEDA - vectorphase5MDNDPEDA





vectorCDPEDA
qqnorm(vectorCDPEDA)
qqline(vectorCDPEDA)
vectorNDPEDA
qqnorm(vectorNDPEDA)
qqline(vectorNDPEDA)
diffvector = vectorCDPEDA - vectorNDPEDA
qqnorm(diffvector)
qqline(diffvector)










pdf("C:/Users/Devarsh Dani/Desktop/PEDAP2.pdf", height = 15, width = 15)
par(mfrow = c(3,5))
colors = c(rep("blue",1))
b <- boxplot(diffvectorph1CD, xaxt='n',
             xlab="Phase 1", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of CD and ND Sessions for PEDA\n ", ylim=c(-150,150), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2CD, xaxt='n',
             xlab="Phase 2", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of CD and ND Sessions for PEDA\n ", ylim=c(-150,150), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3CD, xaxt='n',
             xlab="Phase 3", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of CD and ND Sessions for PEDA\n ", ylim=c(-150,150), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4CD, xaxt='n',
             xlab="Phase 4", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of CD and ND Sessions for PEDA\n ", ylim=c(-150,150), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5CD, xaxt='n',
             xlab="Phase 5", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of CD and ND Sessions for PEDA\n ", ylim=c(-150,150), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)


colors = c(rep("blue",1))
b <- boxplot(diffvectorph1ED, xaxt='n',
             xlab="Phase 1", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of ED and ND Sessions for PEDA\n ", ylim=c(-200,200), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2ED, xaxt='n',
             xlab="Phase 2", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of ED and ND Sessions for PEDA\n ", ylim=c(-200,200), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3ED, xaxt='n',
             xlab="Phase 3", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of ED and ND Sessions for PEDA\n ", ylim=c(-200,200), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4ED, xaxt='n',
             xlab="Phase 4", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of ED and ND Sessions for PEDA\n ", ylim=c(-200,200), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)


colors = c(rep("blue",1))
b <- boxplot(diffvectorph5ED, xaxt='n',
             xlab="Phase 5", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of ED and ND Sessions for PEDA\n ", ylim=c(-200,200), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph1MD, xaxt='n',
             xlab="Phase 1", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of MD and ND Sessions for PEDA\n ***", ylim=c(-300,300), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2MD, xaxt='n',
             xlab="Phase 2", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of MD and ND Sessions for PEDA\n ***", ylim=c(-300,300), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3MD, xaxt='n',
             xlab="Phase 3", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of MD and ND Sessions for PEDA\n ", ylim=c(-300,300), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4MD, xaxt='n',
             xlab="Phase 4", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of MD and ND Sessions for PEDA\n ", ylim=c(-300,300), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5MD, xaxt='n',
             xlab="Phase 5", ylab="Difference in Palm EDA [in kOhm]",
             main="Boxplot of MD and ND Sessions for PEDA\n ", ylim=c(-300,300), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+4.5, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

dev.off()




pdf("C:/Users/Devarsh Dani/Desktop/PEDAQQ.pdf", height = 15, width = 15)
par(mfrow = c(3,5))

qqnorm(diffvectorph1CD, main = "QQ Plot Phase 1 CD vs ND for PEDA")
qqline(diffvectorph1CD)
shapiro.test(diffvectorph1CD)
# pvalue = 7.56e-12 not normal
t.test(diffvectorph1CD,conf.level = 0.95)
#p-value = 0.8598
diffvectorph1CDT = log(diffvectorph1CD-min(diffvectorph1CD)+1)
t.test(diffvectorph1CDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph2CD, main = "QQ Plot Phase 2 CD vs ND for PEDA")
qqline(diffvectorph2CD)
shapiro.test(diffvectorph2CD)
# pvalue = 3.581e-10 not normal
t.test(diffvectorph2CD, conf.level = 0.95)
#p-value = 0.5914
diffvectorph2CDT = log(diffvectorph2CD-min(diffvectorph2CD)+1)
t.test(diffvectorph2CDT,conf.level = 0.95)
# p-value < 2.2e-16


qqnorm(diffvectorph3CD, main = "QQ Plot Phase 3 CD vs ND for PEDA")
qqline(diffvectorph3CD)
shapiro.test(diffvectorph3CD)
# pvalue = 2.196e-08 not normal
t.test(diffvectorph3CD, conf.level = 0.95)
#p-value = 0.686
diffvectorph3CDT = log(diffvectorph3CD-min(diffvectorph3CD)+1)
t.test(diffvectorph3CDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph4CD, main = "QQ Plot Phase 4 CD vs ND for PEDA")
qqline(diffvectorph4CD)
shapiro.test(diffvectorph4CD)
# pvalue = 7.085e-07 not normal
t.test(diffvectorph4CD, conf.level = 0.95)
#p-value = 0.4522
diffvectorph4CDT = log(diffvectorph4CD-min(diffvectorph4CD)+1)
t.test(diffvectorph4CDT,conf.level = 0.95)
# p-value < 2.2e-16



qqnorm(diffvectorph5CD, main = "QQ Plot Phase 5 CD vs ND for PEDA")
qqline(diffvectorph5CD)
shapiro.test(diffvectorph5CD)
# pvalue = 2.205e-08 not normal
t.test(diffvectorph5CD, conf.level = 0.95)
#p-value = 0.5796
diffvectorph5CDT = log(diffvectorph5CD-min(diffvectorph5CD)+1)
t.test(diffvectorph5CDT,conf.level = 0.95)
# p-value < 2.2e-16


qqnorm(diffvectorph1ED, main = "QQ Plot Phase 1 ED vs ND for PEDA")
qqline(diffvectorph1ED)
shapiro.test(diffvectorph1ED)
# pvalue = 2.435e-11 not normal
t.test(diffvectorph1ED, conf.level = 0.95)
# p-value = 0.1278
diffvectorph1EDT = log(diffvectorph1ED-min(diffvectorph1ED)+1)
t.test(diffvectorph1EDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph2ED, main = "QQ Plot Phase 2 ED vs ND for PEDA")
qqline(diffvectorph2ED)
shapiro.test(diffvectorph2ED)
# pvalue = 3.954e-11 not normal
t.test(diffvectorph2ED, conf.level = 0.95)
# p-value = 0.05344
diffvectorph2EDT = log(diffvectorph2ED-min(diffvectorph2ED)+1)
t.test(diffvectorph2EDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph3ED, main = "QQ Plot Phase 3 ED vs ND for PEDA")
qqline(diffvectorph3ED)
shapiro.test(diffvectorph3ED)
# pvalue = 6.518e-13 not normal
t.test(diffvectorph3ED, conf.level = 0.95)
# p-value = 0.05038
diffvectorph3EDT = log(diffvectorph3ED-min(diffvectorph3ED)+1)
t.test(diffvectorph3EDT,conf.level = 0.95)
# p-value < 2.2e-16


qqnorm(diffvectorph4ED, main = "QQ Plot Phase 4 ED vs ND for PEDA")
qqline(diffvectorph4ED)
shapiro.test(diffvectorph4ED)
# pvalue = 1.256e-12 not normal
t.test(diffvectorph4ED, conf.level = 0.95)
# p-value = 0.06888
diffvectorph4EDT = log(diffvectorph4ED-min(diffvectorph4ED)+1)
t.test(diffvectorph4EDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph5ED, main = "QQ Plot Phase 5 ED vs ND for PEDA")
qqline(diffvectorph5ED)
shapiro.test(diffvectorph5ED)
# pvalue = 2.495e-12 not normal
t.test(diffvectorph5ED, conf.level = 0.95)
# p-value = 0.05886
diffvectorph5EDT = log(diffvectorph5ED-min(diffvectorph5ED)+1)
#diffvectorph5EDT = log(vectorphase5EDPEDA) - log(vectorphase5EDNDPEDA)
#qqnorm(diffvectorph5EDT, main = "QQ Plot Phase 5 ED vs ND for PEDA After Transformation")
#qqline(diffvectorph5EDT)
#shapiro.test(diffvectorph5EDT)
t.test(diffvectorph5EDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph1MD, main = "QQ Plot Phase 1 MD vs ND for PEDA")
qqline(diffvectorph1MD)
shapiro.test(diffvectorph1MD)
# pvalue = 3.954e-08 not normal
t.test(diffvectorph1MD, conf.level = 0.95)
# p-value = 0.9882
diffvectorph1MDT = log(diffvectorph1MD-min(diffvectorph1MD)+1)
t.test(diffvectorph1MDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph2MD, main = "QQ Plot Phase 2 MD vs ND for PEDA")
qqline(diffvectorph2MD)
shapiro.test(diffvectorph2MD)
# pvalue = 7.318e-08 not normal
t.test(diffvectorph2MD, conf.level = 0.95)
# p-value = 0.8022
diffvectorph2MDT = log(diffvectorph2MD-min(diffvectorph2MD)+1)
t.test(diffvectorph2MDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph3MD, main = "QQ Plot Phase 3 MD vs ND for PEDA")
qqline(diffvectorph3MD)
shapiro.test(diffvectorph3MD)
# pvalue = 4.516e-08 not normal
t.test(diffvectorph3MD, conf.level = 0.95)
# p-value = 0.595
diffvectorph3MDT = log(diffvectorph3MD-min(diffvectorph3MD)+1)
t.test(diffvectorph3MDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph4MD, main = "QQ Plot Phase 4 MD vs ND for PEDA")
qqline(diffvectorph4MD)
shapiro.test(diffvectorph4MD)
# pvalue = 3.483e-08 not normal
t.test(diffvectorph4MD, conf.level = 0.95)
# p-value = 0.5827
diffvectorph4MDT = log(diffvectorph4MD-min(diffvectorph4MD)+1)
t.test(diffvectorph4MDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph5MD, main = "QQ Plot Phase 5 MD vs ND for PEDA")
qqline(diffvectorph5MD)
shapiro.test(diffvectorph5MD)
# pvalue = 9.022e-08 not normal
t.test(diffvectorph5MD, conf.level = 0.95)
# p-value = 0.7541
diffvectorph5MDT = log(diffvectorph5MD-min(diffvectorph5MD)+1)
t.test(diffvectorph5MDT,conf.level = 0.95)
# p-value < 2.2e-16


dev.off()