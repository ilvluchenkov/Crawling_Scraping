#--------------------------Connect To Spark------------------------------------------------------------------------------------------------
##-------Spark---init

clc <- function() {
  while (sink.number()>0) sink(file=NULL)
  rm(list=ls()); gc() # Clean up as much memory as possible
  gc()  #grabage colector
  cat("\014") #clc
  cat("All file descriptors closed.\n")
  cat("Whole memory cleaned.\n")
}

clc()

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sparkParams <- function(){
  
  lapply(c("sparklyr", "dplyr", "plyr"), require, character.only = T)
  conf <- spark_config()
  conf$spark.env.SPARK_LOCAL_IP.master <- "192.168.103.134"
  conf$`sparklyr.shell.driver-memory` <- "16G"
  conf$spark.driver.extraJavaOptions <-	"append -XX:MaxPermSize= 6.0G"
  conf$spark.memory.fraction <- 0.8
  conf$`sparklyr.shell.executor-memory` <- "16G" 
  conf$maximizeResorceAllocation <- "T"
  conf$spark.default.parallelism <- 12
  conf$spark.driver.maxResultSize <- "5G"
  #if(nchar(Sys.getenv('SPARK_HOME')) <1){Sys.setenv(SPARK_HOME = "/home/spark/spark-2.3.1-bin-hadoop2.7")}
  #options("sparklyr.verbose" = TRUE)
  #sc <- spark_connect(master = "spark://vnode1:7077", spark_home = "/home/spark/spark-2.3.1-bin-hadoop2.7", config = conf)
  sc <- spark_connect(master = "local", spark_home = "/home/spark/spark-2.3.1-bin-hadoop2.7", config = conf)
}

sc <- sparkParams()

#-----------CONF FOR CLUSTER----------------------------------------------------------------------------------------------------------------------------------------------
#if(nchar(Sys.getenv('SPARK_HOME')) <1){Sys.setenv(SPARK_HOME = "/home/spark/spark-2.3.1-bin-hadoop2.7")}
#spark_disconnect_all()
#lapply(c("sparklyr", "dplyr", "plyr"), require, character.only = T)
#sc <- spark_connect(master = "local", spark_home = "/home/spark/spark-2.3.1-bin-hadoop2.7",app_name = "sparklyr",
 #                   config = list(
  #                    "sparklyr.shell.executor-cores"=6,
   #                   "sparklyr.shell.driver-cores"=6,
    #                  "sparklyr.shell.executor-memory"="16G",
     #                 "sparklyr.shell.driver-memory"="16G",
      #                "sparklyr.shell.spark.dynamicAllocation.minExecutors"=6,
       #               "sparklyr.shell.spark.dynamicAllocation.initialExecutors"=6))
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("/root/WORK/vas/cdrs")
#df <- spark_read_csv(sc, "flow", "file:////root/WORK/vas/cdrs/SAE-201K-1_edr_flow_format_07302018130*", header = T, delimiter = "\t", overwrite = T ) %>% collect()
#Must Be files in hdfs:// by Spark streaming. For testing processing file:///
#df <- spark_read_csv(sc, "flow", "file:////root/WORK/vas/cdrs/SAE-201K-1_edr_flow_format_07302018130754_000_000621724", header = T, delimiter = "\t", overwrite = T ) %>% collect()

#df <- spark_read_csv(sc, "flow", "file:////root/WORK/vas/cdrs/SAE-201K-1_edr_http_format_07302018132644_000_000116651", header = T, delimiter = "\t", overwrite = T ) %>% collect()

#HTTP
#df <- spark_read_csv(sc, "http", "file:////root/WORK/vas/cdrs/SAE-201K-1_edr_http_format_07302018132644_000_000116651", 
#                     header = T, delimiter = "\t", quote = "\r", overwrite = F, null_value = 'NA' ) #%>% collect()
spark_disconnect_all()


#df <- read.csv2("SAE-201K-1_edr_http_format_07302018132644_000_000116651", header = T, sep = "\t", quote = "\r" , stringsAsFactors = F)
#df <- read.csv2("SAE-201K-1_edr_http_format_07302018*", header = T, sep = "\t", quote = "\r" , stringsAsFactors = F)
#df <- spark_read_csv(sc, "flow", "file:////root/WORK/vas/cdrs/SAE-201K-1_edr_flow_format_07302018130*", header = T, delimiter = "\t", overwrite = T ) %>% collect()
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

readSAE <- function(df) {
  
  cat("\f")
  
  lapply(c("purrr","httr", "tm","plyr","Rcrawler", "reshape2", "dplyr", 
           "ggplot2", "stringr", "rvest", "curl", "RCurl", "shiny", 
           "tidyr","shinydashboard","highcharter"), 
         require, character.only = T)
  
  setwd("/root/WORK/vas/cdrs/")
  
  df <- do.call("rbind.fill", 
                lapply(list.files(
                  path = "/root/WORK/vas/cdrs/", 
                  pattern = "SAE-201K-1_edr_http_format_07302018*"), 
                  function(x) read.csv2(x,
                                        sep = "\t", 
                                        header = F, 
                                        stringsAsFactors = F, 
                                        quote = "\r", 
                                        strip.white = T,blank.lines.skip = T,skipNul = T)))
  
  df <- df %>% tidyr::drop_na()
  
  colnames(df) <- gsub("[[:punct:]]", "", unlist(df[1,]))
  df <- df[!grepl("#CHECKSUM|#", df$snstarttime),]
  df <- df[-c(4:6,10, 12,13)]
  
  df$httphost <- gsub('(www.)', '', df$httphost, perl = F)
  df$httphost <- gsub('(http://)', '', df$httphost, perl = F)
  
  df$httpurl <- gsub('(www.)', '', df$httpurl, perl = F)
  df$httpurl <- gsub('(\\")', '', df$httpurl, perl = F)
  df$httpurl <- gsub('(http://)', '', df$httpurl, perl = F)
  
  
  df$snvolumeamtipbytesuplink <- as.numeric(df$snvolumeamtipbytesuplink)
  df$snvolumeamtipbytesdownlink <- as.numeric(df$snvolumeamtipbytesdownlink)
  
  df <- as.data.frame(df)
  df$rattype <-  sapply(df$`bearer3gpp rattype`, as.numeric)
  df$`bearer3gpp rattype` <- NULL
  
  
  rownames(df) <- NULL
  
  return(df)
}

SAE <- readSAE(df = df)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

formatSAE <- function(x){
  
  x <- as.data.frame(x[grepl("^2", x$snstarttime),])
  
  x$sd <- substr(x$snstarttime, 1, 8)
  x$st <- substr(x$snstarttime, 9, 14)
  x$ed <- substr(x$snstarttime, 1, 8)
  x$et <- substr(x$snstarttime, 9, 14)
  
  x <- x[-c(1,2)]
  
  x$sd <- as.Date(x[["sd"]], "%Y%m%d")
  x$ed <- as.Date(x[["sd"]], "%Y%m%d")
  x$st <- format(as.POSIXct(strptime(x$st,"%H%M%S",tz="")) ,format = "%H:%M:%S")
  x$et <- format(as.POSIXct(strptime(x$et,"%H%M%S",tz="")) ,format = "%H:%M:%S")
  
  x<-x[c(7:10,6,1:5)]
  
  rownames(x) <- NULL
  #saveRDS(x, file = "~/WORK/vas/basic-scraping-dashboard/data/SAE.RDS")
  
  return(x)  
}

SAE <- formatSAE(x = SAE)

#


aggSAE <- SAE %>% group_by(httphost,rattype) %>%  
  summarise(up = sum(snvolumeamtipbytesuplink), down = sum(snvolumeamtipbytesdownlink), count = n())
  
  
aggSAE %>%   
  hchart("spline", hcaes(x = rattype, y = down, group = httphost)) %>% 
  hc_legend(align = "left", layout = "vertical", verticalAlign = "top") %>% 
  hc_tooltip(sort = TRUE, table = TRUE)


#-------------------------------
aggSAE %>% hchart(.,
                  type = "scatter",
                  hcaes(x = rattype,
                        y = down,
                        group = httphost)) %>% 
  hc_yAxis(opposite = TRUE, labels = list(format = "{value}%")) %>% 
  hc_tooltip(pointFormat = '{point.x:%Y-%m-%d} {point.y: .4f}%')

#


#------------------------------Event UDRlog

readEvent <- function(event){
  setwd("/root/WORK/vas")
  event <- read.csv("event_sgsn-event-reporting-action_20180511173716_000025967_MME-214M-01", header = F, sep = ",", stringsAsFactors = F)
  event <- event[c(14,17)]
  
  event <- event[!sapply(event$V14 == "", all),]
  event <- event[!is.na(event$V14),]
  
  event$sector <- sapply(str_split(event$V14, "-"), tail, 1)
  event$msisdn <- as.character(event$V17)
  
  
  event$V14 <- NULL
  event$V17 <- NULL
  rownames(event) <- NULL
  
  event <- event %>% tidyr::drop_na()
  #dplyr::filter(event,  !is.na(msisdn))
  return(event)
}

event <- readEvent(event = event)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

SAE <- inner_join(SAE, event, by = c("radiuscallingstationid" = "msisdn" ))
SAE <- SAE[!duplicated(SAE),]
saveRDS(SAE, file = "~/WORK/vas/basic-scraping-dashboard/data/SAE.RDS")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

plotSector <- function(df) {
  base.id <- df %>% group_by(SAE$rattype) %>% dplyr::summarise(n = n()) %>% arrange(desc(n))
  #sapply(str_split(dataMTS$bearer.3gpp.user.location.information, "-"), tail, 1)
  colnames(base.id) <- c('base_name', 'count')
  
  p <- ggplot(base.id, aes(x = base_name, y = count)) +
    geom_point() +
    geom_line(group = 1) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(vjust = 1.5),
          axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab('') +
    ylab('Freq.') +
    ggtitle('RAT activity per hour')
  
  return(p)
  
}

plotSector(df = SAE)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


check_url_response <- function(url) {
  
  out <- tryCatch(
    {
      message("\rhttp_status(GET(url))")
      httr::GET(url, timeout(1))$status
    },
    error=function(cond) {
      #message(paste("URL does not seem to exist:", url))
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    finally={
      message("\rClose")
    }
  ) 
  return(out)
}


#----------------------STEP-5.2--PROGRESS BAR Function-------------------------------------------------------------------------------------------------------------
lapply_pb <- function(X, FUN, ...)
{
  env <- environment()
  pb_Total <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)
  
  wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal +1 ,envir=env)
    setTxtProgressBar(get("pb", envir=env), curVal +1)
    FUN(...)
  }
  res <- sapply(X, wrapper, ...)
  close(pb)
  res
}


#----------------------------------RESPONSES CODE------------------------------------------------------------------------------
top_url_SAE <- SAE %>% 
  group_by(httphost) %>% 
  filter(!grepl("cloud|mail|apple|google|yandex|facebook|vk.com|avito|instagram|ya.ru|ssl.|viber|microsoft|youtube|skype|radio|amazon|localhost", httphost)) %>% 
  dplyr::summarize(count = n()) %>% arrange(desc(count))


if (file.exists('responses.RData')) {
  
  missing <- subset(top_url_SAE, !(httphost %in% get(load('responses.RData'))$httphost))
  ifelse(identical(missing$httphost, character(0)), top_url_SAE, 
         map(missing$httphost, .f = missing$responses <-lapply_pb(missing$httphost, check_url_response)))
  top_url_SAE <- rbind(missing, get(load('esponses.RData')))
  save(top_url_SAE,  file = 'responses.RData')
  
} else {
  response_time <- system.time(responses <- map(top_url_SAE$httphost, .f = top_url_SAE$responses <-lapply_pb(top_url_SAE$httphost, check_url_response)))
  save(top_url,  file = 'responses.RData')
  response_time
}

#----------------------------------------------------------------------------------------------------------------












