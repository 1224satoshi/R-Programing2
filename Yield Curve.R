library(xts)
#Source of JGB curve
source.jgb <- NULL
source.jgb[[length(source.jgb) + 1]] <- "http://www.mof.go.jp/jgbs/reference/interest_rate/data/jgbcm_1974-1979.csv"
source.jgb[[length(source.jgb) + 1]] <- "http://www.mof.go.jp/jgbs/reference/interest_rate/data/jgbcm_1980-1989.csv"
source.jgb[[length(source.jgb) + 1]] <- "http://www.mof.go.jp/jgbs/reference/interest_rate/data/jgbcm_1990-1999.csv"
source.jgb[[length(source.jgb) + 1]] <- "http://www.mof.go.jp/jgbs/reference/interest_rate/data/jgbcm_2000-2009.csv"
source.jgb[[length(source.jgb) + 1]] <- "http://www.mof.go.jp/jgbs/reference/interest_rate/data/jgbcm_2010.csv"
source.jgb[[length(source.jgb) + 1]] <- "http://www.mof.go.jp/jgbs/reference/interest_rate/jgbcm.csv"
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
#Download JBG yield
jgb.list <- lapply(source.jgb, GetJGBYield)
#convert one xts object
jgb.xts <- Reduce(rbind, jgb.list)
#Interpolate(nearest value)
coredata(jgb.xts) <- na.locf(t(na.locf(t(coredata(jgb.xts)))))
#to monthly
jgb.xts <- jgb.xts[endpoints(jgb.xts, on="months",k = 1)]

#Label for x-axis
label.term <- paste(gsub("X", "", colnames(jgb.xts)), "Y", sep="")
#The range of y 
y.max <- c(min(jgb.xts), max(jgb.xts))
#plot one image
Snap <- function(val){
  term.structure <- coredata(val)
  index.date     <- index(val)
  par(xaxt="n")
  plot(t(term.structure),type="l",lwd=3, col = 2, xlab = "Term", ylab = "Rate", ylim = y.max)
  par(xaxt="s")
  axis(1, 1:length(label.term), label.term)
  text(0.5, y.max[2], as.character(index.date), pos = 4)
}
#save as gif
library(animation)
saveGIF({
  for(i in 1:nrow(jgb.xts)){Snap(jgb.xts[i])}
},interval = 0.01)