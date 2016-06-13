# Packages to install first:
#install.packages(c("dplyr", "magrittr"))
#install.packages("ggplot2")

library("dplyr")
library("ggplot2")

loadData <- function(filename, filename2, filename.dstat) {
  data1 <- read.table(filename, sep="\t", header=FALSE, col.names=c("timestamp", "latency"))
  data2 <- read.table(filename2, sep="\t", header=FALSE, col.names=c("timestamp", "latency"))
  startTS <- data1$timestamp[1]

  # Uncomment if you have dstat.csv to correlate to…
  
  # startTime <- as.POSIXct(startTS / 1000, tz="UTC", origin="1970-01-01")
  # 
  # dstat <- read.csv(file = filename.dstat, header = FALSE, stringsAsFactors = FALSE)
  # csvStart <- dmy_hms(dstat[4,7])
  # mdbMem <- dstat[7:nrow(dstat), c(20,21,1,3)]
  # colnames(mdbMem) <- c("res", "virt", "usr", "idl")
  # mdbMem$secs <- as.numeric(rownames(mdbMem)) - 7
  # mdbMem$date <- csvStart + seconds(mdbMem$secs)
  # 
  # mdbMem <- mdbMem %>% filter(date >= startTime)
  
  # reroot
  data1$timestamp = data1$timestamp - startTS
  data2$timestamp = data2$timestamp - startTS
  
  # get rid of first second
  data1 <- data1[data1$timestamp > 1000,]
  data2 <- data2[data2$timestamp > 1000,]
  
  x <- data1 %>% mutate(sec=floor(timestamp/1000)) %>% group_by(sec) %>% summarise(throughput = n(), latency = mean(latency))
  
  y <- data2 %>% mutate(sec=floor(timestamp/1000)) %>%  group_by(sec) %>% summarise(throughput.read = n(), latency.read = mean(latency))
  z <- x %>% inner_join(y, by="sec")

  # Uncomment if you have dstat.csv to correlate to…

  # z$res <- as.numeric(mdbMem[1:nrow(z),1])
  # z$virt <- as.numeric(mdbMem[1:nrow(z),2])
  # z$usr <- as.numeric(mdbMem[1:nrow(z),3])
  # z$idl <- as.numeric(mdbMem[1:nrow(z),4])
  
  return(z)
}

'%&%' <- function(x,y) paste0(x,y)

# setup data dirs according to what you want to plot
# dataDirs <- c("wc1-j0-rc0-w10", "wc1-j0-rc0-w50", "wc1-j0-rc0-w100", "wc1-j0-rc0-w200", "wc1-j0-rc0-w500",
#               "wc1-j1-rc0-w10", "wc1-j1-rc0-w50", "wc1-j1-rc0-w100", "wc1-j1-rc0-w200", "wc1-j1-rc0-w500",
#               "wcm-rc0-w10", "wcm-rc0-w50", "wcm-rc0-w100", "wcm-rc0-w200", "wcm-rc0-w500",
#               "wcm-rcm-w10", "wcm-rcm-w50", "wcm-rcm-w100", "wcm-rcm-w200", "wcm-rcm-w500",
#               "wcm-rcm-secload-w10", "wcm-rcm-secload-w50", "wcm-rcm-secload-w100", "wcm-rcm-secload-w200", "wcm-rcm-secload-w500"
# )

dataDirs <- c("model1-wc1", "model1-wcm", "model1-rcm",
              "model2-wc1", "model2-wcm", "model2-rcm",
              "model3-wc1", "model3-wcm", "model3-rcm")

rootDir <- "/Users/sylvainchambon/Code/Bootcamp/data/"

list.of.lists <- lapply(dataDirs, function(dir) {
  data <- loadData(rootDir %&% dir %&% "/writeLatency.tsv", rootDir %&% dir %&% "/readLatency.tsv", rootDir %&% dir %&% "/dstat.csv")
  mtp <- mean(data$throughput)
  mlat <- mean(data$latency)
  # mvirt <- max(data$virt)
  # mres <- max(data$res)
  sdtp <- sd(data$throughput)
  sdlat <- sd(data$latency)
  # cpu <- mean(data$usr)
  mtp.r <- mean(data$throughput.read)
  mlat.r <- mean(data$latency.read)
  sdtp.r <- sd(data$throughput.read)
  sdlat.r <- sd(data$latency.read)

  name <- dir
  
  # return(data.frame(name=name, tp=mtp, lat=mlat, virt=mvirt, res=mres, sdtp=sdtp, sdlat=sdlat, tp.r=mtp.r, lat.r=mlat.r, sdtp.r=sdtp.r, sdlat.r=sdlat.r, concurrency=concurrency, cpu=cpu))
  return(data.frame(name=name, tp=mtp, lat=mlat, sdtp=sdtp, sdlat=sdlat, tp.r=mtp.r, lat.r=mlat.r, sdtp.r=sdtp.r, sdlat.r=sdlat.r))
})

performance <- do.call(rbind, list.of.lists)

# Adjust plot according to what you want!
plot <- ggplot(performance, aes(tp, lat)) +
  geom_point(aes(color=name)) +
  geom_point(aes(x=tp.r, y=lat.r, color=name), shape=1) +
  geom_errorbar(aes(ymin=lat-sdlat, ymax=lat+sdlat, color=name), alpha=0.5) + 
  geom_errorbarh(aes(xmin=tp-sdtp, xmax=tp+sdtp, color=name), alpha=0.5) +
  geom_errorbar(aes(ymin=lat.r-sdlat.r, ymax=lat.r+sdlat.r, x=tp.r, y=lat.r, color=name), alpha=0.5) + 
  geom_errorbarh(aes(xmin=tp-sdtp, xmax=tp+sdtp, x=tp.r, y=lat.r, color=name), alpha=0.5) +
  xlab("Throughput (ops/s)") + ylab("Latency (ms)") + ggtitle("Comparison of RS configuration and model used")

print(plot)
