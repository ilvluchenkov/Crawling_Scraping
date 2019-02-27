#---------------------------------------------------------------------------------------------------------------------------------------
#-------STEP-0 R session cleanup ---------------------------------------------------------------------------------------------------------

clc <- function() {
  while (sink.number()>0) sink(file=NULL)
  #rm(list = ls(envir = globalenv()),envir = globalenv()) #clear Vars from global enviroment
  rm(list=ls()); gc() # Clean up as much memory as possible
  gc()  #grabage colector
  cat("\014") #clc
  cat("All file descriptors closed.\n")
  cat("Whole memory cleaned.\n")
  # Replace by the following line if you want to keep getargs array to retrieve arguments:
  # rm(list=grep("getargs", ls(), fixed=T, value=T, invert=T)); gc()
  #.rs.restartR() #clear session
}

clc()


setwd("/home/rstudio/WORK/crawling/RDATA")


#-------STEP-1---Read Files & Make DF-------------------------------------------------------------------------------------------------------------
#-------STEP-2--clean text--------------------------------------------------------------------------------------------------------------
cleanCorpus <- function(query) {
  
  cat("\f")
  lapply(c("purrr","httr", "tm","plyr","Rcrawler", "highcharter",
           "reshape2", "dplyr", "ggplot2", "stringr", "rvest", "curl", "RCurl", "shiny", "shinydashboard"), require, character.only = T)
  #setwd("/home/rstudio/WORK/crawling/cdrs/data")
  
  query <- do.call("rbind.fill", lapply(list.files(path = "/home/rstudio/WORK/crawling/RDATA", pattern = "query.log-*"), 
                                        function(x) read.csv2(x, sep = " ", header = F, stringsAsFactors = F, quote = "(") ))
  
  
  
  query <- query[grepl("client",query$V5),]
  query$Start<- paste(query$V1, query$V2)
  query$Start <- substr(query$Start, 1, 20)
  query <- query[-c(1:5,7,9:12)]
  
  query$Date <- sapply(strsplit(as.character(query$Start), " "), "[", 1)
  query$sdate <- as.Date(lubridate::parse_date_time(query$Date, orders = c("dmy", "mdy", "ymd")))
  
  query$Time <- sapply(str_split(as.character(query$Start), " "), "[", 2)
  query$Start <- paste(query$sdate, query$Time)
  query$stime <- as.POSIXct(strptime(query$Start, "%Y-%m-%d %H:%M:%S"))
  #query$stime <- format(query$stime, format = "%H:%M:%S")
  query <- query[-c(3,4,6)]
  
  #query$V2 <- sapply(str_split(query$V2, " "), tail, 1)
  query$subip <- transform(query, V6 = colsplit(V6, pattern  = "#", names = c("subip", "sesip")))$V6[[1]]
  query$V6 <- NULL
  colnames(query) <- c("urlname","sdate", "stime","subip")
  query$urlname <- gsub("^www.", '', query$urlname, perl = T)
  rownames(query) <- NULL
  
  return(query[order(query$stime),])
  
}

query <- cleanCorpus(query = query)

#------------------------------------------------------------------------------------------------------------------------------------------------
#--------STEP-3--Agg TOPlinks by host------------------------------------------------------------------------------------------------------------

top_url <- query %>% 
  group_by(urlname) %>% 
  filter(!grepl("cloud|mail|apple|google|yandex|facebook|vk.com|avito|instagram|ya.ru|ssl.|viber|microsoft|youtube|skype|radio|amazon|localhost", urlname)) %>% 
  dplyr::summarize(count = n()) %>% arrange(desc(count))

#--------STEP-4 --GGPLOT-----Counting links by host------------------------------------------------------------------------------------------------
query$ts <- strftime(as.POSIXct(query$stime,format="%H:%M"), format = "%H:%M")
links.hour <- query$ts %>% 
  table() %>% 
  as.data.frame()
colnames(links.hour) <- c('hour', 'count')
#
ggplot(links.hour, aes(x = hour, y = count)) +
  geom_point() +
  geom_line(group = 1) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = 1.0),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab('') +
  ylab('Bookmarks') +
  ggtitle('Links added per hour')

#
hchart(links.hour, "bubble", hcaes(x = hour, y = count, group = hour))


#-------STEP-5--VAILIDATE URL-------------------------------------------------------------------------------------------------------------
#-------STEP-5.1--CHECK_URL_RESPONSE & GET STATUS Function--------------------------------------------------------------------------------
check_url_response <- function(url) {
  
  out <- tryCatch(
    {
      message("\rhttp_status(GET(url))")
      #http_status(GET(top_url))
      #HEAD(url)$status_code
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

# top <- head(top_url,1056)
# map_chr(top$urlname, .f = check_url_response)

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

#top$status <- lapply_pb(top$urlname, check_url_response)



#----------------------STEP-5.3--Make responses.RData File------------------------------------------------------------------------------------------#
cat("\f")

if (file.exists('responses.RData')) {
  
  missing <- subset(top_url, !(urlname %in% get(load('responses.RData'))$urlname))
  ifelse(identical(missing$urlname, character(0)), top_url, 
         map_df(missing$urlname, .f = missing$responses <-lapply_pb(missing$urlname, check_url_response)))
  top_url <- rbind(missing, get(load('responses.RData')))
  save(top_url,  file = 'responses.RData')
  
} else {
  response_time <- system.time(responses <- map_df(top_url$urlname, .f = top_url$responses <-lapply_pb(top_url$urlname, check_url_response)))
  save(top_url,  file = 'responses.RData')
  response_time
}



#----------------------STEP-5.4--RUN validate Functions----------------------------------------------------------------------------------------------#
#---------------AGG BY Responses For Stat------------------------------------------------------------------------------------------------------------#

response.aggs <- top_url$responses %>% 
  table() %>% 
  as.data.frame(stringsAsFactors = F)
colnames(response.aggs) <- c('response', 'count')
response.aggs <- response.aggs[order(response.aggs$count, decreasing = T),]


#----
hchart(response.aggs, "bar", hcaes(x = response, y = count, group = response))


colnames(response.aggs) <- c('response', 'count')
ggplot(response.aggs, aes(x = response, y = count)) +
  geom_point() +
  geom_line(group = 1) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = 1.0),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab('') +
  ylab('Responses') +
  ggtitle('Responses per hour')

#--
top_url %>% group_by(responses) %>% dplyr::summarize(count = n()) %>% arrange(desc(count))


#----------------------STEP 7.0 RCrawling And Scraping----------------------------------------------------------------------------------------------#
#----------------------STEP-7.0 EXTRACT DATA--Status 200 Only--- Analyze URL's for popularity, using pagerank algorithm-----------------------------#

top_url_200 <- top_url %>% filter(responses == '200')%>% arrange(desc(count))
top_url_200$rnk <- top_url_200$count / max(top_url_200$count) 

#----STEP-7.1 Extract Title Node Function-----------------------------------------------------------------------------------------------------------#

extract_title <- function(url) {
  out <- tryCatch({
    message("\rExtract html_nodes 'title'")
    
    read_html(curl(url)) %>% 
      html_nodes('title') %>% 
      html_text()
      
    },
        
    error=function(cond) {
      message(cond)
      return(NA)},
    finally={message("\rClose")
    }) 
  return(out)}


#top_url_200$title <- lapply(top_url_200$urlname, extract_title)

#----STEP-7.2 Extract Click Function-----------------------------------------------------------------------------------------------------------------

extract_click <- function(url) {
  out <- tryCatch({
    message("\rExtract html_nodes 'click'")
    
    read_html(curl(url)) %>% 
      html_nodes('b') %>% 
      html_text()},
    
    error=function(cond) {
      message(cond)
      return(NA)},
    finally={message("\rClose")
    }) 
  return(out)}

#topu$click <- lapply(topu$urlname, extract_click)

#----STEP-7.3 RUN Extract Title Function-------------------------------------------------------------------------------------------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!User Version
#if (file.exists('titles.RData')) {
#  load('titles.RData')
#} else {
#  response_time <- system.time(map_chr(top_url_200$urlname, .f = top_url_200$title <-lapply_pb(top_url_200$urlname, extract_title)))
#  save(top_url_200,  file = 'titles.RData')
#  response_time
#}


if (file.exists('titles.RData')) {
  
  missing <- subset(top_url_200, !(urlname %in% get(load('titles.RData'))$urlname))
  ifelse(identical(missing$urlname, character(0)), top_url_200, 
         map_df(missing$urlname, .f = missing$title <-lapply_pb(missing$urlname, extract_title)))
  
  top_url_200 <- rbind(missing, get(load('titles.RData')))
  save(top_url_200,  file = 'titles.RData')
  

} else {
  response_time <- system.time(map_df(top_url_200$urlname, .f = top_url_200$title <-lapply_pb(top_url_200$urlname, extract_title)))
  save(top_url_200,  file = 'titles.RData')
  response_time
}


#map(top_url_200$urlname, .f = top_url_200$title <-lapply_pb(top_url_200$urlname, extract_title))
#----STEP-7.3.1 Filter Unconditional  Titles Function---------------------------------------------------------------------------------------------------------------

title_filter <- function(top_url_200) {
  top_url_title_filter <- top_url_200 %>%  filter(!grepl('NA|character', title))
  top_url_title_filter <- top_url_title_filter[top_url_title_filter$title != "",]
  top_url_title_filter$title <-  unlist(lapply(top_url_title_filter$title, '[[',1))
  return(top_url_title_filter)
}

top_url_200 <- title_filter(top_url_200 = top_url_200)

#----STEP 7.4 RUN Extract Click Function-----------------------------------------------------------------------------------------------------------------------------

if (file.exists('click.RData')) {
  
  missing <- subset(top_url_200, !(urlname %in% get(load('click.RData'))$urlname))
  
  ifelse(identical(missing$urlname, character(0)), top_url_200, 
         map_df(missing$urlname, .f = missing$click <-lapply_pb(missing$urlname, extract_click)))
  
  top_url_200 <- rbind(missing, get(load('click.RData')))
  save(top_url_200,  file = 'click.RData')
  
  
} else {
  response_time <- system.time(map_df(top_url_200$urlname, .f = top_url_200$click <-lapply_pb(top_url_200$urlname, extract_click)))
  save(top_url_200,  file = 'click.RData')
  response_time
}

#map(top_url_title_filter$urlname, .f = top_url_title_filter$click <-lapply_pb(top_url_title_filter$urlname, extract_click))


#----STEP-7.5 Filter Unconditional Click Function---------------------------------------------------------------------------------------------------------------

click_filter <- function(top_url_200) {
  top_url_200 <- top_url_200 %>%  filter(!grepl('NA|character', click))
  top_url_200 <- top_url_200[top_url_200$click != "",]
  top_url_200 <- top_url_200[top_url_200$click != " ",]
  top_url_200$click <-  unlist(lapply(top_url_200$click, '[[',1))
  #top_url_200 <- top_url_200[-c(2,3)]
  rownames(top_url_200) <- NULL
  return(top_url_200)
}

top_url_200 <- click_filter(top_url_200 = top_url_200)

#----STEP-8-MAKE JOIN BY URL for Extract IP -----------------------------------------------------------------------------------------------------------

query_final_res <- inner_join(query,top_url_200, by = c("urlname" = "urlname")) %>% arrange(desc(rnk))


rnk.hour <- query_final_res$rnk %>% 
  table() %>% 
  as.data.frame()

colnames(rnk.hour) <- c('rnk', 'count')
ggplot(rnk.hour, aes(x = rnk, y = count)) +
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
  ggtitle('Pages Ranks per hour')

hchart(query_final_res$rnk)

#readRDS("~/WORK/vas/basic-scraping-dashboard/data/query_final_res.RDS")

#-----END Query and Go to SAE------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------








                    
