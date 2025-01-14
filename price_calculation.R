unique(df_stock$종목)
names_stock

price_magic <- df_stock %>% 
  select(1,7,21)
colnames(price_magic) <- c('종목','종가', 'Date')


df_final_magic <- data.frame()
for (i in 1:length(names_stock)){
  filtered_df <- as.data.frame(price_magic %>% 
    filter(종목 == names_stock[i]) %>% 
    group_by(year(Date)) %>% 
    summarise(avg_price = mean(종가, na.rm = T)))
  filtered_df$종목 <- names_stock[i]
  df_final_magic <- rbind(df_final_magic ,filtered_df)
}

df_final_magic <- df_final_magic %>% 
  select(1,3,2)
write.csv(df_final_magic, file = 'df_price.csv')
