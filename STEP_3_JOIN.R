#Join 1
#query_final_res <- inner_join(query,top_url_200, by = c("urlname" = "urlname"))


#Join 2
#colnames(SAE)[3] <- "subip"
#memory.limit(size=40000)
gc()
#query_final_res <- query_final_res %>%  filter(grepl('200', responses))


query_final_res$jointime <- gsub("[:punct:]|\\s+|-", "" ,query_final_res$stime)



result <- inner_join(SAE, query_final_res, by = c("httpurl"="urlname", "X.sn.start.time" = "jointime"))



#result <- result[!sapply(result$radiuscallingstationid  == "NA", all),]


#Write to .RDS for WEb Analitics
#result$rnk <- result$count / max(result$count)
saveRDS(result, file = "~/WORK/vas/basic-scraping-dashboard/data/result.RDS")

testSAE <- head(SAE,100)


hchart(testSAE, "scatter", hcaes(x = sd, y = rattype))

#######################
Encoding( x = map_data_wgs84$Name ) <- "UTF-8"
# replace all non UTF-8 character strings with an empty space
map_data_wgs84$Name <-
  iconv( x = map_data_wgs84$Name
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "" )
####################################




