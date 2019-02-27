library(sparklyr)
library(dplyr)

conf <- spark_config()
conf$spark.env.SPARK_LOCAL_IP.master <- "192.168.103.134"
conf$`sparklyr.shell.driver-memory` <- "42G"
conf$spark.memory.fraction <- 0.8
conf$`sparklyr.shell.executor-memory` <- "14G"

sc <- spark_connect(master = "spark://vnode1:7077", 
                    spark_home = "/home/spark/spark-2.3.1-bin-hadoop2.7", config = conf)
#-------------
setwd("/root/WORK/sbv/logs")
test <- sapply(list.files(path = "/root/WORK/sbv/logs", pattern = "*.logOLD"), 
               function(x) read.csv2(x, sep = "(", header = F, stringsAsFactors = F))

f <- read.csv2("20180724000202_SMARTBV_SS_OA_GF.logOLD", sep = "=", header = F, stringsAsFactors = F, quote = "=")
f <- f[!grepl("application/xml",f$V1),]



#tt <- NULLstrsplit(substr(f$V1, 1, 51), "\\s+")

library(reshape2)
tem <- transform(f, V1 = colsplit(V1, pattern  = " ", names = c("c1", "c2", "c3", "c4","c5", "c6", "c7", "c8")))
f <- NULL
tem$V1 <- tem$V1[,c(1,2,4,7)]

tem$date <- tem$V1$c1
tem$ts <- tem$V1$c2
tem$thread <- tem$V1$c4
tem$msisdn <- gsub("\\D", "", gsub("[()]", "", tem$V1$c7))
tem$V1 <- NULL
tem$V3 <- NULL
tem$V4 <- NULL

data <- tem[c(4,5,6,7,1,2,3)]
tem <- NULL
#------------------------------

data$V2 <- gsub(" \\s+", "", data$V2) 


ifelse(grepl("200Content-Type", data$V2) && data$V6 != "")

data %>% 
  mutata

                                                                                     
                                                                                     
                                                                                     
                                                                                     
                                                                                     


