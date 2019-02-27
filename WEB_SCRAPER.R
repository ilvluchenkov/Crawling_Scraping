#-----------------------------------------------------------------
years <- 1970:2016
urls <- paste0("http://www.baseball-reference.com/teams/MIL/", years, ".shtml")


get_table <- function(url) {
  url %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="dev_team_batting"]/table[1]') %>% 
    html_text()
}
sapply(urls, get_table)
#-------STEP  ----------------------------------------------------------------------------------------------------------------
#-------STEP  ----------------------------------------------------------------------------------------------------------------
#-------STEP  ----------------------------------------------------------------------------------------------------------------
#-------STEP 0----------------------------------------------------------------------------------------------------------------
getwd()
lapply(c("tm","plyr","Rcrawlerlibrar","Rcrawler", "reshape2", "dplyr", "ggplot2", "stringr", "rvest", "curl", "RCurl", "shiny", "shinydashboard"), require, character.only = T)
setwd("/root/WORK/vas/cdrs/data")

#-------STEP 1----------------------------------------------------------------------------------------------------------------
cat("\f")
#query <- read.csv("query.log", header = F, sep = " ", stringsAsFactors = F)
query <- do.call("rbind.fill", lapply(list.files(path = "/root/WORK/vas/cdrs/data", pattern = "query.log-"), 
                                      function(x) read.csv2(x, sep = " ", header = F, stringsAsFactors = F, quote = "(") ))

query <- query[grepl("client",query$V5),]
query <- query[-c(3:5,7,9:11,12)]


#Drops NAN--------------------------------------------------
#query <- subset(query, query$V12 != "")
rownames(query) <- NULL
query$V1 <- as.Date(lubridate::parse_date_time(query$V1, orders = c("dmy", "mdy", "ymd")))
#--------------------------------------------ClearingData
#query$V12 <- gsub("[()]", "", query$V12)
#query$V6 <- gsub(":", "", query$V6)
#--------------------------------------------ColSplit
tmp <- transform(query, V6 = colsplit(V6, pattern  = "#", names = c("subip", "sesip")))
query$subip <- tmp$V6[[1]]
#query$sesid <- tmp$V6[[2]]
query$V6 <- NULL
tmp <- NULL
query <- query[c(1,2,4,3)]
#colnames(query) <- c("sdate", "stime","subip","sesid", "urlname", "gsn_ip")
colnames(query) <- c("sdate", "stime","urlname","subip")

#-------STEP 2----------------------------------------------------------------------------------------------------------------
cat("\f")
query <- unique(query[1:10,])
urlValid <- data.frame(ifelse(sapply(query$urlname, function(x) url.exists(x)), query$urlname, 0))
names(urlValid) <- "url_validate"
query <- cbind(query,urlValid)
query <-query[!query$url_validate %in% "0",]
rownames(query) <- NULL
cat("\f")
#-------STEP 3----------------------------------------------------------------------------------------------------------------
#lapply(qTest, function(x) read_html(curl(x))  %>% html_nodes("*") %>% html_attr("class")) %>% unique() %>% html_text()
#query$text <- unlist(lapply(query$urlname, function(x) read_html(curl(x)) %>% html_text() %>% gsub("^\\s+|\\s+$|\n|\t", "", .)))
#------------XPATH

#lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes(xpath = "///table/tr/td/a") %>% html_text())


query$title <- lapply(query$urlname, function(x) read_html(curl(x)) %>% html_node('title')%>% html_text() %>% gsub("^\\s+|\\s+$|\n|\t", "", .))

sapply(query$urlname, function(x) read_html(curl(x)) %>%  html_nodes('*'))
a <- sapply(query$urlname, function(x) read_html(curl(x)) %>%  html_nodes(xpath = '//meta'))


#lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes(xpath = "//center//font//b") %>% html_nodes(xpath = "//b"))
#query$text <- lapply(unlist(query$urlname), function(x) read_html(curl(x))) %>% html_table %>% "[" %>% str()
#------------DIV
#query$text_div<- sapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes("title") %>% html_nodes("content") %>% html_text() %>% gsub("^\\s+|\\s+$|\n|\t", "", .))

#sapply(query$urlname, function(x) read_html(curl(x)) %>%  html_nodes("title") %>% html_nodes(xpath = '//meta') %>% html_attr('name') %>% gsub("^\\s+|\\s+$|\n|\t", "", .))
#------------TABLE
#query$text_table<- lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes("table"))
#lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes("div") %>% html_nodes("table") %>% "["(1) %>% str())  
#------------H2
query$text_h2 <- sapply(query$urlname, function(x) read_html(curl(x)) %>% 
         html_nodes("div") %>% 
         html_nodes("h2") %>% 
         html_text() %>% 
         gsub("^\\s+|\\s+$|\n|\t", "", .))
#------------H3
query$text_h3<- lapply(query$urlname, function(x) read_html(curl(x)) %>% 
                         html_nodes("div") %>% 
                         html_nodes("h3") %>% 
                         html_text() %>% 
                         gsub("^\\s+|\\s+$|\n|\t", "", .))
#------------TD
query$text_td<- lapply(query$urlname, function(x) read_html(curl(x)) %>% 
                         html_nodes("td") %>% 
                         html_text() %>% 
                         gsub("^\\s+|\\s+$|\n|\t", "", .))

#lapply(query$, function(x) read_html(curl(x)) %>% html_nodes(xpath = "///table[1]/tr/td") %>% html_node("*") %>% html_text())
#write.table(unlist(query), file = "out.csv", sep = ";")
#-------STEP 4----------------------------------------------------------------------------------------------------------------
pl <- query %>% group_by(urlname) %>% summarise(n = n())
ggplot(pl, aes(urlname, n, fill =urlname)) + geom_point()

#
query %>% 
  filter(subip == '10.169.157.179') %>% 
  #select(sdate, subip) %>% 
  group_by(urlname)
#summarize(n())

#click <- inner_join(dataMTS,query, by = c("ip.subscriber.ip.address" = "subip"))
dataConv <- dataCLEAR %>% 
  group_by(Date,ip.subscriber.ip.address, sn.direction, hour, sn.rulebase) %>% 
  summarize(count = n(),
            sumBUp = sum(sn.volume.amt.ip.bytes.uplink),
            sumBDw = sum(sn.volume.amt.ip.bytes.downlink))
dataRANK <-dataConv %>% 
  arrange(count) %>% 
  group_by(ip.subscriber.ip.address) %>% 
  mutate(rank = row_number())
#-------------------------RND
query$title <- lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes("title") %>% html_text() %>% gsub("^\\s+|\\s+$|\n|\t", "", .))
lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes("")%>% html_text() %>% gsub("^\\s+|\\s+$|\n|\t", "", .))
lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes("*") %>% html_text()%>% gsub("^\\s+|\\s+$|\n|\t", "", .))
lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes(xpath = "node"))
lapply(query$urlname, function(x) read_html(curl(x)) %>% html_nodes("*") %>% html_text())
#---------------------------------Rcrawler
lapply(query[query$urlname == "match.prod.bidr.io",], 
       function(x) Rcrawler(Website = x, 
                            no_cores = 4, 
                            no_conn = 4, 
                            ExtractXpathPat = c("//h1", "//article"), PatternsNames = c("Title", "Content") ))
ListProjects()
DATA <-LoadHTMLFiles("match.prod.bidr.io-011815", type = "vector")



que$val  <- data.frame(ifelse(sapply(que$que, function(x) url.exists(x)), que$que, 0))


