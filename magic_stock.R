# Preprocess Data
setwd("C:/Users/user/Documents/R/FinancialData_Exam")
load('재무제표_통합_최종.rdata')
load('주식_통합_최종.rdata')
load('배당_통합_최종.rdata')
library(lubridate)
df_finance$총부채.천원._ANNUAL

finance_fs <- list(지배주주순이익=data.frame(),
                   법인세 = data.frame(),
                   이자비용 = data.frame(),
                   유동자산 = data.frame(),
                   유동부채 = data.frame(),
                   비유동자산 = data.frame(),
                   자산감가상각비 = data.frame(),
                   총부채 = data.frame(),
                   현금및현금성자산 = data.frame())


names_stock <- unique(df_finance$종목)

for (i in 1:length(names_stock)){
  F_1 <- df_finance %>% 
    filter(종목 == names_stock[i]) %>% 
    group_by(year(Date)) %>% 
    summarise(지배주주순이익 = mean(지배주주순이익.천원._ANNUAL, na.rm = T),
              법인세 = mean(법인세비용.천원._ANNUAL, na.rm = T),
              이자비용 = mean(이자비용.천원._ANNUAL, na.rm = T),
              유동자산 = mean(유동자산.천원._ANNUAL, na.rm = T),
              유동부채 = mean(유동부채.천원._ANNUAL, na.rm = T),
              비유동자산 = mean(비유동자산.천원._ANNUAL, na.rm = T),
              자산감가상각비 = mean(유형자산감가상각비.천원._ANNUAL, na.rm = T),
              총부채 = mean(총부채.천원._ANNUAL, na.rm = T),
              현금및현금성자산 = mean(현금및현금성자산.천원._ANNUAL, na.rm = T))
  
  finance_fs$지배주주순이익 <- rbind(finance_fs$지배주주순이익, F_1$지배주주순이익)
  finance_fs$법인세 <- rbind(finance_fs$법인세, F_1$법인세)
  finance_fs$이자비용 <- rbind(finance_fs$이자비용, F_1$이자비용)
  finance_fs$유동자산 <- rbind(finance_fs$유동자산, F_1$유동자산)
  finance_fs$유동부채 <- rbind(finance_fs$유동부채, F_1$유동부채)
  finance_fs$비유동자산 <- rbind(finance_fs$비유동자산, F_1$비유동자산)
  finance_fs$자산감가상각비 <- rbind(finance_fs$자산감가상각비, F_1$자산감가상각비)
  finance_fs$총부채 <- rbind(finance_fs$총부채, F_1$총부채)
  finance_fs$현금및현금성자산 <- rbind(finance_fs$현금및현금성자산, F_1$현금및현금성자산)
}
finance_fs$지배주주순이익[is.na(finance_fs$지배주주순이익)] <- 0
finance_fs$법인세[is.na(finance_fs$법인세)] <- 0
finance_fs$이자비용[is.na(finance_fs$이자비용)] <- 0
finance_fs$유동자산[is.na(finance_fs$유동자산)] <- 0
finance_fs$유동부채[is.na(finance_fs$유동부채)] <- 0
finance_fs$비유동자산[is.na(finance_fs$비유동자산)] <- 0
finance_fs$자산감가상각비[is.na(finance_fs$자산감가상각비)] <- 0
finance_fs$총부채[is.na(finance_fs$총부채)] <- 0
finance_fs$현금및현금성자산[is.na(finance_fs$현금및현금성자산)] <- 0

finance_fs$시가총액 <- data.frame()

for (i in 1:length(names_stock)){
  df_offer <- df_stock %>%
    filter(종목 == names_stock[i]) %>% 
    mutate(Year = year(Date)) %>% 
    group_by(Year) %>% 
    summarise(시가총액 = mean(수정주가.원._DAILY, na.rm=T)+mean(상장주식수.주._DAILY, na.rm=T))
  finance_fs$시가총액 <- rbind(finance_fs$시가총액, df_offer$시가총액)
}

finance_fs$시가총액[is.na(finance_fs$시가총액)] <- 0




# Assign colnames
colnames(finance_fs$지배주주순이익) <- c(1980:2022)
colnames(finance_fs$법인세) <- c(1980:2022)
colnames(finance_fs$이자비용) <- c(1980:2022)
colnames(finance_fs$유동자산) <- c(1980:2022)
colnames(finance_fs$유동부채) <- c(1980:2022)
colnames(finance_fs$비유동자산) <- c(1980:2022)
colnames(finance_fs$자산감가상각비) <- c(1980:2022)
colnames(finance_fs$시가총액) <- c(1980:2022)
colnames(finance_fs$총부채) <- c(1980:2022)
colnames(finance_fs$현금및현금성자산) <- c(1980:2022)

# value Factor
## 분자
magic_ebit = finance_fs$지배주주순이익 + finance_fs$법인세 +
  finance_fs$이자비용
# 분모
magic_cap = finance_fs$시가총액

magic_debt = finance_fs$총부채
magic_excess_cash_1 = finance_fs$유동부채 - finance_fs$유동자산 +
  finance_fs$현금및현금성자산
magic_excess_cash_1[magic_excess_cash_1 < 0] = 0
magic_excess_cash_2 =
  (finance_fs$'현금및현금성자산' - magic_excess_cash_1)
magic_ev = magic_cap + magic_debt - magic_excess_cash_2
# 이익수익률
magic_ey = magic_ebit / magic_ev

magic_ic = ((finance_fs$유동자산 - finance_fs$유동부채) +
              (finance_fs$비유동자산 - finance_fs$자산감가상각비))
magic_roc = magic_ebit / magic_ic

invest_magic = rank(rank(-magic_ey) + rank(-magic_roc)) <= 30

## Create a dataframe for machine learning
magic_all <- data.frame()
for ( i in 1981:2022){
  year_current <- i
  num_col <- str_which(colnames(finance_fs[[1]]), as.character(year_current-1))
  magic_ebit_c <- magic_ebit[num_col]
  magic_cap_c <- magic_cap[num_col]
  magic_debt_c <- magic_debt[num_col]
  magic_excess_cash_1_c <- magic_excess_cash_1[num_col]
  magic_excess_cash_1_c[magic_excess_cash_1_c < 0] = 0
  
  magic_excess_cash_2_c <- magic_excess_cash_2[num_col]
  magic_ev <- magic_cap_c+magic_debt_c-magic_excess_cash_2_c
  magic_ey <- magic_ebit_c / magic_ev
  
  magic_ic_c <- magic_ic[num_col]
  magic_roc <- magic_ebit_c / magic_ic_c
  
  
  magic_trial <- data.frame(종목 = names_stock)
  magic_new <- magic_trial %>% 
    mutate(Year = i,
           roc = magic_roc,
           ey = magic_ey,
           EBIT = magic_ebit_c,
           EV = magic_ev)
  magic_all <- rbind(magic_all, magic_new)
}

str(magic_all)
# Change structure of dataframe.
magic_all$roc <- magic_all$roc$`1980`
magic_all$ey <- magic_all$ey$`1980`
magic_all$EBIT <- magic_all$EBIT$`1980`
magic_all$EV <- magic_all$EV$`1980`
str(magic_all)

write.csv(magic_all, file = 'df_magic.csv') # save magic data.
