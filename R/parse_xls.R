# Type: R file
# Version: 0.0.3
# Date: 2017-11-13
# Authors: Christine Choirat (cchoirat@gmail.com) & Yara Abu Awad (yaa291@mail.harvard.edu)
# Description: Parses measurements from xls file and formats list into dataframe
# License: GPL3

library(readxl)
library(data.table)

parse_xls <-
  function(file = x) {
    ##----- Read xls file
    d <- read_xls(file)
    ##----- Select columns of interest
    #d <- data.table(cbind(d$X__1, d$X__2, d$X__3, d$X__4)) #christine's code
    d <- data.table(d[,2:5])
    names(d) <- c("sampleid", "Test1", "Test2", "Test3")
    ##----- Create an index
    d[, Idx := 1:.N]
    ##----- Use the index to check where each sample starts
    idx <- d[sampleid == "working standard" &
               Test1 == "set to 100" &
               Test2 == "set 100"]$Idx
    ##----- Store sample result in a list
    SET <- list()
    ##----- Retrieve all sample data
    # 6 corresponds to the number of measurements
    # idx[i] is line where sample i begins
    # idx[i + 1 ] is line where sample i + 1 begins
    # We don't need Idx anymore
    for (i in 1:length(idx))
      SET[[i]] <- d[(idx[i] + 1):(idx[i] + 1 + 6)][, Idx := NULL]
    ##----- Output is a list indexed by SET number
    return(SET)
  }

#SET <- parse_xls("C:/Users/yaa291/Desktop/RefImport/MADEP_R-NEW_01aug17.xls")

# SET[[10]]
# sampleid Test1 Test2 Test3
# 1:         t0747280  85.9  85.3    NA
# 2:         t0747005  82.4  82.9    NA
# 3:         t0747019  70.4  69.9    NA
# 4:         t0747020    77  74.6  75.1
# 5:         t0747035  73.9  73.9    NA
# 6:         t0747049  81.3  80.7    NA
# 7: working standard   101  99.2 100.8

#class(SET); typeof(SET); #list


list2df = function(x){

  done = data.frame(x)
  if (sum(!is.na(done$sampleid[1:6])) >0   ){
  done$WS_T1 = done$Test1[done$sampleid == 'working standard']
  done$WS_T2 = done$Test2[done$sampleid == 'working standard']
  done$WS_T3 = done$Test3[done$sampleid == 'working standard']
  done = done[done$sampleid != 'working standard',]
  done[,2:7] = apply(done[,2:7], c(1,2), function(x) as.numeric(x))
  return(done)}

}




#sheet = do.call(rbind.data.frame, lapply(SET, list2df))


