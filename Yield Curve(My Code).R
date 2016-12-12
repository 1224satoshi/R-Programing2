library(xts)
#Source of JGB curve
source.jgb <- NULL
source.jgb[[length(source.jgb) + 1]] <- "http://www.mof.go.jp/jgbs/reference/interest_rate/data/jgbcm_all.csv"
source.jgb[[length(source.jgb) + 1]] <- "http://www.mof.go.jp/jgbs/reference/interest_rate/jgbcm.csv"
source.jgb
#From Japanese era To ChristianEra
ToChristianEra <- function(x)
{
  era  <- substr(x, 1, 1)
  year <- as.numeric(substr(x, 2, nchar(x)))
  if(era == "H"){
    year <- year + 1988
  }else if(era == "S"){
    year <- year + 1925
  }
  as.character(year)
}
ToChristianEra
theUrl<-"http://www.mof.go.jp/jgbs/reference/interest_rate/data/jgbcm_all.csv"
tomato <- read.table(file=theUrl,header=TRUE,sep=",")
tomato
#Down load yield curve and convert to xts object
GetJGBYield <- function(source.url)
{
  jgb <- read.csv(source.url, stringsAsFactors = FALSE)
  #Extract date only
  jgb.day  <- strsplit(jgb[, 1], "\\.")
  #stop warning
  warn.old <- getOption("warn")
  options(warn = -1)
  #From Japanese era To ChristianEra
  jgb.day <- lapply(jgb.day, function(x)c(ToChristianEra(x[1]), x[2:length(x)]))
  #From date string to date object
  jgb[,  1] <- as.Date(sapply(jgb.day, function(x)Reduce(function(y,z)paste(y,z, sep="-"),x)))
  #Convert data from string to numeric
  jgb[, -1] <- apply(jgb[, -1], 2, as.numeric)
  options(warn = warn.old)
  as.xts(read.zoo(jgb))
}
GetJGBYield
#Download JBG yield
jgb.list <- lapply(source.jgb, GetJGBYield)
