
l <- head(result %>% select(rnk, V2, lat, long) %>% group_by(V2) %>% arrange(desc(rnk)), 100)


map <- leaflet(width = 800, height = 800) %>% 
  addTiles() %>% 
  setView()
  addMarkers(lng = l$long,
             lat = l$lat, 
             popup = l$V2)


map


#
df <-aggregate(result$rnk, by = list(result$rnk,result$lat, result$long),  FUN = length)
colnames(df) <- c("rnk", "lat", "long", "n")
df<- df[!duplicated(df),]
#df$n <- NULL

#
df_all_1 <- df %>% filter(rnk==1) %>% group_by(rnk) %>% summarise(lat = lat[which.min(lat)], long = long[which.min(long)],n = n())
df_all_2 <- df %>% group_by(rnk) %>% summarise(lat = lat[which.max(lat)], long = long[which.max(long)],n = n())

df <- rbind(df_all_1,df_all_2)
df$n <- NULL

#
df_05_1 <- df %>% group_by(rnk) %>%  filter(rnk > .5) %>% summarise(lat = lat[which.min(lat)], long = long[which.min(long)],n = n()) 
df_05_2 <- df %>% group_by(rnk) %>%  filter(rnk > .5) %>% summarise(lat = lat[which.max(lat)], long = long[which.max(long)],n = n()) 

#
df <- rbind(df, df_all_05_1,df_all_05_2)

df_02_05_1 <- df %>% group_by(rnk) %>%  filter(rnk > .1 & rnk < .5) %>% summarise(lat = lat[which.min(lat)], long = long[which.min(long)])
df_02_05_2 <- df %>% group_by(rnk) %>%  filter(rnk > .1 & rnk < .5) %>% summarise(lat = lat[which.max(lat)], long = long[which.max(long)]) 


df <- rbind(df, df_02_05_1, df_02_05_2)

#







