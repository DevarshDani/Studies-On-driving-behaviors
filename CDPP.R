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
vectorphase1CDNDPP = NULL
vectorphase2CDNDPP = NULL
vectorphase3CDNDPP = NULL
vectorphase4CDNDPP = NULL
vectorphase5CDNDPP = NULL

vectorphase1CDPP = NULL
vectorphase2CDPP = NULL
vectorphase3CDPP = NULL
vectorphase4CDPP = NULL
vectorphase5CDPP = NULL

notopen = NULL
noPPND = NULL
noPPCD = NULL
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
    noPPCD = append(noPPCD, i)
    next
  }
  NRPerinasalCD = read.xlsx(fil2, sheetIndex = 1, colIndex = c(2,4), header = TRUE)
  NRPerinasalCD
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
  
  #NRPerinasalCD = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(NRPerinasalCD)[1] <- "Time"
  names(NRPerinasalCD)[2] <- "NR.Perinasal"
  NRPerinasalCD
  NRPerinasalCD = NRPerinasalCD[-1,]
  NRPerinasalCD = NRPerinasalCD[order(NRPerinasalCD$Time), , drop = FALSE]
  NRPerinasalCD$NR.Perinasal <- as.numeric(as.character(NRPerinasalCD$NR.Perinasal))
  NRPerinasalCD$Time <- as.numeric(as.character(NRPerinasalCD$Time))
  NRPerinasalCD = NRPerinasalCD[order(NRPerinasalCD$Time), , drop = FALSE]
  #NRPerinasal[with(NRPerinasal, !(as.numeric(NRPerinasal$Time) < Stime1)), ]
  #NRPerinasalCD = NRPerinasalCD[!(as.numeric(NRPerinasalCD$NR.Perinasal) < 4 
  #                                | as.numeric(NRPerinasalCD$NR.Perinasal) > 70), ]
  phase1CD = NRPerinasalCD[(as.numeric(NRPerinasalCD$Time) < Stime1), ]
  phase2CD = NRPerinasalCD[(as.numeric(NRPerinasalCD$Time) > Stime1 & as.numeric(NRPerinasalCD$Time) < Etime1),] 
  phase3CD = NRPerinasalCD[(as.numeric(NRPerinasalCD$Time) > Etime1 & as.numeric(NRPerinasalCD$Time) < Stime2), ]
  phase4CD = NRPerinasalCD[(as.numeric(NRPerinasalCD$Time) > Stime2 & as.numeric(NRPerinasalCD$Time) < Etime2 ), ]
  phase5CD = NRPerinasalCD[(as.numeric(NRPerinasalCD$Time) > Etime2), ]
  phase1CD
  phase2CD
  phase3CD
  phase4CD
  phase5CD
  #filter(NRPerinasal, as.numeric(NRPerinasal$Time) < Stime1)
  NRPerinasalCD
  #if(is.data.frame(phase1CD) && nrow(phase1CD)==0) {
  #  meanphase1CDPP = 0
  #} else {
    meanphase1CDPP = mean(phase1CD$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2CD) && nrow(phase2CD)==0) {
  #  meanphase2CDPP = 0
  #} else {
    meanphase2CDPP = mean(phase2CD$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3CD) && nrow(phase3CD)==0) {
  #  meanphase3CDPP = 0
  #} else {
    meanphase3CDPP = mean(phase3CD$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4CD) && nrow(phase4CD)==0) {
  #  meanphase4CDPP = 0
  #} else {
    meanphase4CDPP = mean(phase4CD$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5CD) && nrow(phase5CD)==0) {
  # meanphase5CDPP = 0
  #} else {
    meanphase5CDPP = mean(phase5CD$NR.Perinasal, na.rm = TRUE)
  #}
  #meanCDPP = mean(NRPerinasalCD$NR.Perinasal)
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
  #  meanNDPP = mean(NRPerinasalND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase1ND) && nrow(phase1ND)==0) {
  #  meanphase1NDPP = 0
  #} else {
    meanphase1NDPP = mean(phase1ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2ND) && nrow(phase2ND)==0) {
  #  meanphase2NDPP = 0
  #} else {
    meanphase2NDPP = mean(phase2ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3ND) && nrow(phase3ND)==0) {
  #  meanphase3NDPP = 0
  #} else {
    meanphase3NDPP = mean(phase3ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4ND) && nrow(phase4ND)==0) {
  # meanphase4NDPP = 0
  #} else {
    meanphase4NDPP = mean(phase4ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
  #  meanphase5NDPP = 0
  #} else {
    meanphase5NDPP = mean(phase5ND$NR.Perinasal, na.rm = TRUE)
  #}
  
  vectorphase1CDNDPP = append(vectorphase1CDNDPP, meanphase1NDPP)
  vectorphase2CDNDPP = append(vectorphase2CDNDPP, meanphase2NDPP)
  vectorphase3CDNDPP = append(vectorphase3CDNDPP, meanphase3NDPP)
  vectorphase4CDNDPP = append(vectorphase4CDNDPP, meanphase4NDPP)
  vectorphase5CDNDPP = append(vectorphase5CDNDPP, meanphase5NDPP)
  
  vectorphase1CDPP = append(vectorphase1CDPP, meanphase1CDPP)
  vectorphase2CDPP = append(vectorphase2CDPP, meanphase2CDPP)
  vectorphase3CDPP = append(vectorphase3CDPP, meanphase3CDPP)
  vectorphase4CDPP = append(vectorphase4CDPP, meanphase4CDPP)
  vectorphase5CDPP = append(vectorphase5CDPP, meanphase5CDPP)
  
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
vectorphase1EDNDPP = NULL
vectorphase2EDNDPP = NULL
vectorphase3EDNDPP = NULL
vectorphase4EDNDPP = NULL
vectorphase5EDNDPP = NULL

vectorphase1EDPP = NULL
vectorphase2EDPP = NULL
vectorphase3EDPP = NULL
vectorphase4EDPP = NULL
vectorphase5EDPP = NULL

notopen = NULL
noPPND = NULL
noPPED = NULL
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
    noPPED = append(noPPED, i)
    next
  }
  NRPerinasalED = read.xlsx(fil2, sheetIndex = 1, colIndex = c(2,4), header = TRUE)
  NRPerinasalED
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
  
  #NRPerinasalED = read.xlsx(fil2, sheetIndex = 1, colIndex = 2:3, header = TRUE)
  names(NRPerinasalED)[1] <- "Time"
  names(NRPerinasalED)[2] <- "NR.Perinasal"
  NRPerinasalED
  NRPerinasalED = NRPerinasalED[-1,]
  NRPerinasalED = NRPerinasalED[order(NRPerinasalED$Time), , drop = FALSE]
  NRPerinasalED$NR.Perinasal <- as.numeric(as.character(NRPerinasalED$NR.Perinasal))
  NRPerinasalED$Time <- as.numeric(as.character(NRPerinasalED$Time))
  NRPerinasalED = NRPerinasalED[order(NRPerinasalED$Time), , drop = FALSE]
  #NRPerinasal[with(NRPerinasal, !(as.numeric(NRPerinasal$Time) < Stime1)), ]
  #NRPerinasalED = NRPerinasalED[!(as.numeric(NRPerinasalED$NR.Perinasal) < 4 
  #                                | as.numeric(NRPerinasalED$NR.Perinasal) > 70), ]
  phase1ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) < Stime1), ]
  phase2ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) > Stime1 & as.numeric(NRPerinasalED$Time) < Etime1),] 
  phase3ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) > Etime1 & as.numeric(NRPerinasalED$Time) < Stime2), ]
  phase4ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) > Stime2 & as.numeric(NRPerinasalED$Time) < Etime2 ), ]
  phase5ED = NRPerinasalED[(as.numeric(NRPerinasalED$Time) > Etime2), ]
  phase1ED
  phase2ED
  phase3ED
  phase4ED
  phase5ED
  #filter(NRPerinasal, as.numeric(NRPerinasal$Time) < Stime1)
  NRPerinasalED
  #if(is.data.frame(phase1ED) && nrow(phase1ED)==0) {
  #  meanphase1EDPP = 0
  #} else {
  meanphase1EDPP = mean(phase1ED$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2ED) && nrow(phase2ED)==0) {
  #  meanphase2EDPP = 0
  #} else {
  meanphase2EDPP = mean(phase2ED$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3ED) && nrow(phase3ED)==0) {
  #  meanphase3EDPP = 0
  #} else {
  meanphase3EDPP = mean(phase3ED$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4ED) && nrow(phase4ED)==0) {
  #  meanphase4EDPP = 0
  #} else {
  meanphase4EDPP = mean(phase4ED$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5ED) && nrow(phase5ED)==0) {
  #  meanphase5EDPP = 0
  #} else {
  meanphase5EDPP = mean(phase5ED$NR.Perinasal, na.rm = TRUE)
  #}
  #meanEDPP = mean(NRPerinasalED$NR.Perinasal)
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
  meanphase1NDPP = mean(phase1ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2ND) && nrow(phase2ND)==0) {
  #  meanphase2NDPP = 0
  #} else {
  meanphase2NDPP = mean(phase2ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3ND) && nrow(phase3ND)==0) {
  #meanphase3NDPP = 0
  #} else {
  meanphase3NDPP = mean(phase3ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4ND) && nrow(phase4ND)==0) {
  #  meanphase4NDPP = 0
  #} else {
  meanphase4NDPP = mean(phase4ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
  #  meanphase5NDPP = 0
  #} else {
  meanphase5NDPP = mean(phase5ND$NR.Perinasal, na.rm = TRUE)
  #}
  
  vectorphase1EDNDPP = append(vectorphase1EDNDPP, meanphase1NDPP)
  vectorphase2EDNDPP = append(vectorphase2EDNDPP, meanphase2NDPP)
  vectorphase3EDNDPP = append(vectorphase3EDNDPP, meanphase3NDPP)
  vectorphase4EDNDPP = append(vectorphase4EDNDPP, meanphase4NDPP)
  vectorphase5EDNDPP = append(vectorphase5EDNDPP, meanphase5NDPP)
  
  vectorphase1EDPP = append(vectorphase1EDPP, meanphase1EDPP)
  vectorphase2EDPP = append(vectorphase2EDPP, meanphase2EDPP)
  vectorphase3EDPP = append(vectorphase3EDPP, meanphase3EDPP)
  vectorphase4EDPP = append(vectorphase4EDPP, meanphase4EDPP)
  vectorphase5EDPP = append(vectorphase5EDPP, meanphase5EDPP)
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}



#################################################################################################################

path1 = "C:\\Users\\Devarsh Dani\\Desktop\\UH\\Spring 2017\\Statistical methods in research\\Other Study Data"
setwd(path1)
dir = grep("T???/??MD", list.dirs(), value = TRUE)
dir2 = grep("T???/??ND", list.dirs(), value = TRUE)
#dir
unlink(path1)
vectorphase1MDNDPP = NULL
vectorphase2MDNDPP = NULL
vectorphase3MDNDPP = NULL
vectorphase4MDNDPP = NULL
vectorphase5MDNDPP = NULL

vectorphase1MDPP = NULL
vectorphase2MDPP = NULL
vectorphase3MDPP = NULL
vectorphase4MDPP = NULL
vectorphase5MDPP = NULL

notopen = NULL
noPPND = NULL
noPPMD = NULL
nostm = NULL
#i=66
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
  meanphase1MDPP = mean(phase1MD$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2MD) && nrow(phase2MD)==0) {
  #  meanphase2MDPP = 0
  #} else {
  meanphase2MDPP = mean(phase2MD$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3MD) && nrow(phase3MD)==0) {
  #  meanphase3MDPP = 0
  #} else {
  meanphase3MDPP = mean(phase3MD$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4MD) && nrow(phase4MD)==0) {
  #  meanphase4MDPP = 0
  #} else {
  meanphase4MDPP = mean(phase4MD$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5MD) && nrow(phase5MD)==0) {
  #  meanphase5MDPP = 0
  #} else {
  meanphase5MDPP = mean(phase5MD$NR.Perinasal, na.rm = TRUE)
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
  phase5ND
  #filter(NRPerinasal, as.numeric(NRPerinasal$Time) < Stime1)
  #NRPerinasalND
  if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
    meanphase5NDPP = -1
  } else {
    meanphase5NDPP = mean(phase5ND$NR.Perinasal, na.rm = TRUE)
  }
  
  #if(is.data.frame(phase1ND) && nrow(phase1ND)==0) {
  #  meanphase1NDPP = 0
  #} else {
  meanphase1NDPP = mean(phase1ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase2ND) && nrow(phase2ND)==0) {
  #  meanphase2NDPP = 0
  #} else {
  meanphase2NDPP = mean(phase2ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase3ND) && nrow(phase3ND)==0) {
  #  meanphase3NDPP = 0
  #} else {
  meanphase3NDPP = mean(phase3ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase4ND) && nrow(phase4ND)==0) {
  #  meanphase4NDPP = 0
  #} else {
  meanphase4NDPP = mean(phase4ND$NR.Perinasal, na.rm = TRUE)
  #}
  #if(is.data.frame(phase5ND) && nrow(phase5ND)==0) {
  #  meanphase5NDPP = 0
  #} else {
  
  
  #}
  
  vectorphase1MDNDPP = append(vectorphase1MDNDPP, meanphase1NDPP)
  vectorphase2MDNDPP = append(vectorphase2MDNDPP, meanphase2NDPP)
  vectorphase3MDNDPP = append(vectorphase3MDNDPP, meanphase3NDPP)
  vectorphase4MDNDPP = append(vectorphase4MDNDPP, meanphase4NDPP)
  
  vectorphase1MDPP = append(vectorphase1MDPP, meanphase1MDPP)
  vectorphase2MDPP = append(vectorphase2MDPP, meanphase2MDPP)
  vectorphase3MDPP = append(vectorphase3MDPP, meanphase3MDPP)
  vectorphase4MDPP = append(vectorphase4MDPP, meanphase4MDPP)
  
  if(meanphase5NDPP != -1 && !(is.nan(meanphase5NDPP))) {
    vectorphase5MDNDPP = append(vectorphase5MDNDPP, meanphase5NDPP)
    vectorphase5MDPP = append(vectorphase5MDPP, meanphase5MDPP)
  }
  
  #setwd("../..")
  #unlink(dir2[i])
  #i = i+1
}

#################################################################################################################

#vectorphase1NDPP
#vectorphase2NDPP
#vectorphase3NDPP
#vectorphase4NDPP
#vectorphase5NDPP

#vectorphase1CDPP
#vectorphase2CDPP
#vectorphase3CDPP
#vectorphase4CDPP
#vectorphase5CDPP

#nostm

diffvectorph1CD = vectorphase1CDPP - vectorphase1CDNDPP
diffvectorph2CD = vectorphase2CDPP - vectorphase2CDNDPP
diffvectorph3CD = vectorphase3CDPP - vectorphase3CDNDPP
diffvectorph4CD = vectorphase4CDPP - vectorphase4CDNDPP
diffvectorph5CD = vectorphase5CDPP - vectorphase5CDNDPP

diffvectorph1ED = vectorphase1EDPP - vectorphase1EDNDPP
diffvectorph2ED = vectorphase2EDPP - vectorphase2EDNDPP
diffvectorph3ED = vectorphase3EDPP - vectorphase3EDNDPP
diffvectorph4ED = vectorphase4EDPP - vectorphase4EDNDPP
diffvectorph5ED = vectorphase5EDPP - vectorphase5EDNDPP

diffvectorph1MD = vectorphase1MDPP - vectorphase1MDNDPP
diffvectorph2MD = vectorphase2MDPP - vectorphase2MDNDPP
diffvectorph3MD = vectorphase3MDPP - vectorphase3MDNDPP
diffvectorph4MD = vectorphase4MDPP - vectorphase4MDNDPP
diffvectorph5MD = vectorphase5MDPP - vectorphase5MDNDPP


min(diffvectorph5MD)
max(diffvectorph5MD)

vectorCDPP
qqnorm(vectorCDPP)
qqline(vectorCDPP)
vectorNDPP
qqnorm(vectorNDPP)
qqline(vectorNDPP)
diffvector = vectorCDPP - vectorNDPP
qqnorm(diffvector)
qqline(diffvector)


pdf("C:/Users/Devarsh Dani/Desktop/PPP2.pdf", height = 15, width = 15)
par(mfrow = c(3,5))
colors = c(rep("blue",1))
b <- boxplot(diffvectorph1CD, xaxt='n',
             xlab="Phase 1", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of CD and ND Sessions for PP\n ", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2CD, xaxt='n',
             xlab="Phase 2", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of CD and ND Sessions for PP\n ***", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)


colors = c(rep("blue",1))
b <- boxplot(diffvectorph3CD, xaxt='n',
             xlab="Phase 3", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of CD and ND Sessions for PP\n *", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4CD, xaxt='n',
             xlab="Phase 4", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of CD and ND Sessions for PP\n ***", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5CD, xaxt='n',
             xlab="Phase 5", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of CD and ND Sessions for PP\n ", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5CD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph1ED, xaxt='n',
             xlab="Phase 1", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of ED and ND Sessions for PP\n ", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2ED, xaxt='n',
             xlab="Phase 2", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of ED and ND Sessions for PP\n ***", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3ED, xaxt='n',
             xlab="Phase 3", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of ED and ND Sessions for PP\n ", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4ED, xaxt='n',
             xlab="Phase 4", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of ED and ND Sessions for PP\n ***", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5ED, xaxt='n',
             xlab="Phase 5", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of ED and ND Sessions for PP\n ***", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5ED))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph1MD, xaxt='n',
             xlab="Phase 1", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of MD and ND Sessions for PP\n ***", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph1MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph2MD, xaxt='n',
             xlab="Phase 2", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of MD and ND Sessions for PP\n ***", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph2MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph3MD, xaxt='n',
             xlab="Phase 3", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of MD and ND Sessions for PP\n ", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph3MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("red",1))
b <- boxplot(diffvectorph4MD, xaxt='n',
             xlab="Phase 4", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of MD and ND Sessions for PP\n ", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph4MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

colors = c(rep("blue",1))
b <- boxplot(diffvectorph5MD, xaxt='n',
             xlab="Phase 5", ylab="Difference in Noise Reduced Perinasal [in ???C2]",
             main="Boxplot of MD and ND Sessions for PP\n ", ylim = c(-0.00969665,0.009505241), col = colors)
#par(cex.axis=1.5)
means <- c(mean(diffvectorph5MD))
points(means, pch=8, col="green")
#axis(1, at=1:5, labels=c("Phase 1","Phase 2", "Phase 3", "Phase 4", "Phase 5"))
text(1:length(b$n), b$stats[5,]+0.005, paste("n =", b$n))
#abline(v=mean(1:length(b$n)), lty=3)

dev.off()



pdf("C:/Users/Devarsh Dani/Desktop/PPQQ.pdf", height = 15, width = 15)
par(mfrow = c(3,5))

qqnorm(diffvectorph1CD, main = "QQ Plot Phase 1 CD vs ND for PP")
qqline(diffvectorph1CD)
shapiro.test(diffvectorph1CD)
# pvalue = 1.631e-05 not normal
t.test(diffvectorph1CD,conf.level = 0.95)
# p-value = 0.2831
min(diffvectorph1CD)
diffvectorph1CD
diffvectorph1CDT
diffvectorph1CDT = log(diffvectorph1CD-min(diffvectorph1CD)+1)
t.test(diffvectorph1CDT,conf.level = 0.95)
# p-value < 2.2e-16

#qqnorm(diffvectorph1CDT)
#qqline(diffvectorph1CDT)
#shapiro.test(diffvectorph1CDT)

qqnorm(diffvectorph2CD, main = "QQ Plot Phase 2 CD vs ND for PP")
qqline(diffvectorph2CD)
shapiro.test(diffvectorph2CD)
# pvalue = 8.748e-07 not normal
t.test(diffvectorph2CD,conf.level = 0.95)
# p-value = 6.845e-06
diffvectorph2CD
diffvectorph2CDT
diffvectorph2CDT = log(diffvectorph2CD-min(diffvectorph2CD)+1)
t.test(diffvectorph2CDT,conf.level = 0.95)
# p-value = 1.125e-14

#qqnorm(diffvectorph1CDT)
#qqline(diffvectorph1CDT)
#shapiro.test(diffvectorph2CDT)

qqnorm(diffvectorph3CD, main = "QQ Plot Phase 3 CD vs ND for PP")
qqline(diffvectorph3CD)
shapiro.test(diffvectorph3CD)
# pvalue = 1.516e-06 not normal
t.test(diffvectorph3CD,conf.level = 0.95)
# p-value = 0.0135
diffvectorph3CD
diffvectorph3CDT
diffvectorph3CDT = log(diffvectorph3CD-min(diffvectorph3CD)+1)
t.test(diffvectorph3CDT,conf.level = 0.95)
# p-value < 2.2e-16

#qqnorm(diffvectorph1CDT)
#qqline(diffvectorph1CDT)
#shapiro.test(diffvectorph3CDT)

qqnorm(diffvectorph4CD, main = "QQ Plot Phase 4 CD vs ND for PP")
qqline(diffvectorph4CD)
shapiro.test(diffvectorph4CD)
# pvalue = 9.273e-07 not normal
t.test(diffvectorph4CD,conf.level = 0.95)
# p-value = 0.0001553
diffvectorph4CD
diffvectorph4CDT
diffvectorph4CDT = log(diffvectorph4CD-min(diffvectorph4CD)+1)
t.test(diffvectorph4CDT,conf.level = 0.95)
# p-value < 2.2e-16
#qqnorm(diffvectorph1CDT)
#qqline(diffvectorph1CDT)
#shapiro.test(diffvectorph4CDT)

qqnorm(diffvectorph5CD, main = "QQ Plot Phase 5 CD vs ND for PP")
qqline(diffvectorph5CD)
shapiro.test(diffvectorph5CD)
# pvalue = 3.822e-06 not normal
t.test(diffvectorph5CD,conf.level = 0.95)
# p-value = 0.6408
diffvectorph5CD
diffvectorph5CDT
diffvectorph5CDT = log(diffvectorph5CD-min(diffvectorph5CD)+1)
t.test(diffvectorph5CDT,conf.level = 0.95)
# p-value < 2.2e-16
#qqnorm(diffvectorph1CDT)
#qqline(diffvectorph1CDT)
#shapiro.test(diffvectorph5CDT)


qqnorm(diffvectorph1ED, main = "QQ Plot Phase 1 ED vs ND for PP")
qqline(diffvectorph1ED)
shapiro.test(diffvectorph1ED)
# pvalue = 0.00107 not normal
t.test(diffvectorph1ED,conf.level = 0.95)
# p-value = 0.1458
diffvectorph1ED
diffvectorph1EDT
diffvectorph1EDT = log(diffvectorph1ED-min(diffvectorph1ED)+1)
t.test(diffvectorph1EDT,conf.level = 0.95)
# p-value < 2.2e-16
#qqnorm(diffvectorph1CDT)
#qqline(diffvectorph1CDT)
shapiro.test(diffvectorph1EDT)


qqnorm(diffvectorph2ED, main = "QQ Plot Phase 2 ED vs ND for PP")
qqline(diffvectorph2ED)
shapiro.test(diffvectorph2ED)
# pvalue = 5.542e-07 not normal
t.test(diffvectorph2ED,conf.level = 0.95)
# p-value = 1.071e-05
diffvectorph2EDT = log(diffvectorph2ED-min(diffvectorph2ED)+1)
t.test(diffvectorph2EDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph3ED, main = "QQ Plot Phase 3 ED vs ND for PP")
qqline(diffvectorph3ED)
shapiro.test(diffvectorph3ED)
# pvalue = 1.277e-08 not normal
t.test(diffvectorph3ED,conf.level = 0.95)
# p-value = 0.6237
diffvectorph3EDT = log(diffvectorph3ED-min(diffvectorph3ED)+1)
t.test(diffvectorph3EDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph4ED, main = "QQ Plot Phase 4 ED vs ND for PP")
qqline(diffvectorph4ED)
shapiro.test(diffvectorph4ED)
# pvalue = 4.16e-06 not normal
t.test(diffvectorph4ED,conf.level = 0.95)
# p-value = 0.0005508
diffvectorph4EDT = log(diffvectorph4ED-min(diffvectorph4ED)+1)
t.test(diffvectorph4EDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph5ED, main = "QQ Plot Phase 5 ED vs ND for PP")
qqline(diffvectorph5ED)
shapiro.test(diffvectorph5ED)
# pvalue = 3.195e-05 not normal
diffvectorph5EDT = log(diffvectorph5ED-min(diffvectorph5ED)+1)
t.test(diffvectorph5EDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph1MD, main = "QQ Plot Phase 1 MD vs ND for PP")
qqline(diffvectorph1MD)
shapiro.test(diffvectorph1MD)
# pvalue = 7.698e-06 not normal
diffvectorph1MDT = log(diffvectorph1MD-min(diffvectorph1MD)+1)
t.test(diffvectorph1MDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph2MD, main = "QQ Plot Phase 2 MD vs ND for PP")
qqline(diffvectorph2MD)
shapiro.test(diffvectorph2MD)
# pvalue = 5.339e-06 not normal
diffvectorph2MDT = log(diffvectorph2MD-min(diffvectorph2MD)+1)
t.test(diffvectorph2MDT,conf.level = 0.95)
# p-value < 2.2e-16


qqnorm(diffvectorph3MD, main = "QQ Plot Phase 3 MD vs ND for PP")
qqline(diffvectorph3MD)
shapiro.test(diffvectorph3MD)
# pvalue = 1.097e-05 not normal
t.test(diffvectorph3MD,conf.level = 0.95)
# p-value = 0.6606
diffvectorph3MDT = log(diffvectorph3MD-min(diffvectorph3MD)+1)
t.test(diffvectorph3MDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph4MD, main = "QQ Plot Phase 4 MD vs ND for PP")
qqline(diffvectorph4MD)
shapiro.test(diffvectorph4MD)
# pvalue = 2.557e-05 not normal
t.test(diffvectorph4MD,conf.level = 0.95)
# p-value = 0.3107
diffvectorph4MDT = log(diffvectorph4MD-min(diffvectorph4MD)+1)
t.test(diffvectorph4MDT,conf.level = 0.95)
# p-value < 2.2e-16

qqnorm(diffvectorph5MD, main = "QQ Plot Phase 5 MD vs ND for PP")
qqline(diffvectorph5MD)
shapiro.test(diffvectorph5MD)
# pvalue = 0.0001772 not normal
t.test(diffvectorph5MD,conf.level = 0.95)
# p-value = 0.6985
diffvectorph5MDT = log(diffvectorph5MD-min(diffvectorph5MD)+1)
t.test(diffvectorph5MDT,conf.level = 0.95)
# p-value < 2.2e-16

dev.off()



diffvectorph1CDT = log(diffvectorph1CD-min(diffvectorph1CD)+2)
qqnorm(diffvectorph1CDT)
qqline(diffvectorph1CDT)
