#pretend this is A Very Long List of IPs
ip.list = c("1.1.1.1", "2.3.4.99", "1.1.1.2", "2.3.4.100", "70.196.7.32", 
            "146.160.21.171", "146.160.21.172", "146.160.21.186", "2.3.4.101", 
            "216.167.176.93", "1.1.1.3", "2.3.4.5", "2.3.4.88", "2.3.4.9", 
            "98.208.205.1", "24.34.218.80", "159.127.124.209", "70.196.198.151", 
            "70.192.72.48", "173.192.34.24", "65.214.243.208", "173.45.242.179", 
            "184.106.97.102", "198.61.171.18", "71.184.118.37", "70.215.200.159", 
            "184.107.87.105", "174.121.93.90", "172.17.96.139", "108.59.250.112", 
            "24.63.14.4")

#
resolved <- unlist(lapply(ip.list, function(x) system(sprintf("dig -x %s +short",x), intern=TRUE)))
########







ip.to.host <- function(ips) {
  
  writeLines(lapply(ips, function(x) sprintf("-i%s",x)),"/tmp/ips.in")
  system.output <- system("cat /tmp/ips.in | adnshost -f",intern=TRUE)
  unlink("/tmp/ips.in")
  
  cleaned.result <- gsub("\\.in-addr\\.arpa","",system.output)
  
  split.result <- strsplit(cleaned.result," PTR ")
  
  result.df <- data.frame(do.call(rbind, lapply(split.result, rbind)))
  colnames(result.df) <- c("IP","hostname")
  
  result.df$IP <- sapply(as.character(result.df$IP), function(x) {
    y <- unlist(strsplit(x,"\\."))
    sprintf("%s.%s.%s.%s",y[4],y[3],y[2],y[1])
  })
  
  final.result <- merge(ips,result.df,by.x="x",by.y="IP",all.x=TRUE)
  colnames(final.result) = c("IP","hostname")
  return(final.result)
}

resolved.df <- ip.to.host(ip.list)




