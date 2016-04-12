mySave <- function(..., file) { 
  callingF = sys.call(-1)
  cTime = Sys.time()
  cWd = getwd()
  sInfo = Sys.info()
  metadata = list(callingF, cTime, cWd, sInfo)
  save(..., metadata, file = file)
}