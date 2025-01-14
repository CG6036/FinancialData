##############################
## 22-2 Financial Analytics ##
## Individual Project       ##
## Chan Gyu Lee             ##
## 21700587                 ##
##############################


# Load Packages
library(tidyverse)
library(caret)
library(lubridate)


# 1) Data Preparation (Load)

## 1.1) Create stock data with magic formula information

### Load Dividend Data 
df_div <- read.csv('df_dividend.csv')
df_div <- df_div[-1] # erase index column
### Load Stock Data 
df_stock <- read.csv('df_stock.csv')
df_stock <- df_stock[-1]
### Load Finance Data
df_finance <- read.csv('df_finance.csv')
df_finance <- df_finance[-1] 

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

# write.csv(magic_all, file = 'df_magic.csv') #save magic data.

## 1.2) Create a dataframe that has price of each stock (yearly)
names_stock <- unique(df_finance$종목)

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
# write.csv(df_final_magic, file = 'df_price.csv')

## 1.3) Load ETF and create yearly etf price data.
df_etf <- read_excel('data_etf.xlsx')
df_etf <- df_etf %>% 
  select(1,2)
df_etf$일자 <- as.Date(df_etf$일자)
df_etf$year <- year(df_etf$일자)
str(df_etf)
yearly_etf <- as.data.frame(df_etf %>% 
                              group_by(year) %>% 
                              summarise(avg_price = mean(종가)))
# write.csv(yearly_etf, file = 'df_etf_yearly.csv')



# 2) Data Preprocessing
## Load all necessary data
df_etf <- read.csv('df_etf_yearly.csv')
df_price <- read.csv('df_price.csv')
df_price <- df_price %>% 
  select(-1)
df_etf <- df_etf %>% 
  select(-1)
## Unify the year information. (from 2002)
df_price <- df_price %>% 
  filter(year.Date. >=2002)
## add etf data to price data.
df_price$price_etf <- rep(df_etf$avg_price, 93)
names_stock <- unique(df_price$종목)

## create a new data frame to store label information.
new_df <- data.frame() 
for (i in 1:length(names_stock)){
  filtered <- df_price %>% 
    filter(종목 == names_stock[i])
  for (j in 1:18){ # Calculate the increase of price compare to the 3 year increase in etf.
    if ((filtered$avg_price[j+3]-filtered$avg_price[j])/filtered$avg_price[j] <
        (filtered$price_etf[j+3]-filtered$price_etf[j])/filtered$price_etf[j]|is.na(filtered$avg_price[j])){ # NA control
      filtered$label[j] = 0} else{
        filtered$label[j] = 1
      }
  }
  new_df <- rbind(new_df, filtered)
}
colnames(new_df)[1] <- 'Year'
str(new_df)
## Add features to data (Magic Formula)
df_magic <- read.csv('df_magic.csv')
df_magic <- df_magic[,-1]
df_merge <- merge(x=df_magic, y=new_df[,c(1,2,5)], by = c('Year', '종목'), all.x=TRUE)

## Erase all na values and equalize the proportion of 0 and 1 labels.
df_model <- df_merge[complete.cases(df_merge$label),]
df_model <- df_model[complete.cases(df_model$roc),] # NA control
df_model <- df_model[!is.infinite(df_model$roc),] # infinite number control

## Control outliers
df_model <- df_model %>% 
  filter(abs(roc) <= 1) # filter ones in the range of -1~1

df_model <- df_model %>% 
  filter(abs(ey) <=1) # filter ones in the range of -1~1

## Create derived variables to increase model performance.
df_model$roc2 <- df_model$roc^2
df_model$ey2 <- df_model$ey^2

## Down sampling (set label proportion as 50:50)
df_model$label <- as.factor(df_model$label)
df_model <- downSample(df_model[,-7],df_model[,7])
colnames(df_model)[9] <- 'label'
str(df_model)


# 3) Model Preparation

## 3.1) Data Split
table(df_model$label)
smp_size <- floor(0.90 * nrow(df_model))## 90% of the sample size
set.seed(123)
train_ind <- sample(seq_len(nrow(df_model)), size = smp_size)

training_set <- df_model[train_ind, ]
test_set <- df_model[-train_ind, ]
#write.csv(training_set, file = 'training_set.csv')
#write.csv(test_set, file = 'test_set.csv')

## 3.2) Model Evaluation Function (error metrics)
score_evaluation = function(cm){
  score_precision <- cm[2,2]/sum(cm[,2])
  score_recall <- cm[2,2]/sum(cm[2,])
  cm_df <- data.frame(precision = score_precision,
                      recall = score_recall,
                      F1_Score = 2*(score_precision*score_recall)/(score_precision+score_recall))
  return (cm_df)
}


# 4) Model Construction

## 4.1) SVM Model
library(e1071)
classifier = svm(formula = label ~ ., # Fitting SVM to the Training set
                 data = training_set[,c(3,4,7,8,9)],
                 type = 'C-classification',
                 kernel = 'sigmoid')

y_pred = predict(classifier, newdata = test_set[,c(3,4,7,8)]) # Predicting the Test set results
table(y_pred)

cm = table(actual = test_set[, 9], pred = y_pred) # Making the Confusion Matrix
accuracy_svm <- (sum(diag(cm)))/sum(cm)
accuracy_svm # Accuracy =  0.5420561

score_evaluation(cm) # Evaluation Result
#precision    recall  F1_Score
# 0.5230769 0.6538462 0.5811966


## 4.2) KNN Clustering Model
library(class)
### k = 1 experiment
set.seed(1234)
knn_1 <- knn(train = training_set[c(3,4,7,8)],
             test = test_set[c(3,4,7,8)],
             cl = training_set$label,
             k = 1)
accuracy_1 <- mean(knn_1 == test_set$label) # 0.5140187
### k = 21 experiment
knn_21 <- knn(train = training_set[c(3,4,7,8)],
              test = test_set[c(3,4,7,8)],
              cl = training_set$label,
              k = 21)
accuracy_21 <- mean(knn_21 == test_set$label) # 0.5327103
cm_21 = table(actual = test_set[,9], pred = knn_21)
score_evaluation(cm_21)
#precision    recall F1_Score
# 0.5208333 0.4807692      0.5

### grid_search for finding optimal k
accuracy_k <- NULL
for(kk in c(1:100)){
  set.seed(1234)
  knn_k <- knn(train = training_set[c(3,4,7,8)],
               test = test_set[c(3,4,7,8)],
               cl = training_set$label,
               k = kk)
  accuracy_k <- c(accuracy_k, sum(knn_k == test_set$label) / length(test_set$label))
}
valid_k <- data.frame(k = c(1:100), accuracy = accuracy_k)
### Grid search visualization
plot(formula = accuracy ~ k,
     data = valid_k,
     type = "o",
     pch = 20,
     main = "validation - optimal k") # optimal k = 33

### Modeling process with optimal k implemented
knn_33 <- knn(train = training_set[c(3,4,7,8)],
              test = test_set[c(3,4,7,8)],
              cl = training_set$label,
              k = 33)

### KNN Evaluatoin
accuracy_33 <- mean(knn_33 == test_set$label) #  0.635514
cm_33 = table(actual = test_set[,9], pred = knn_33)
score_evaluation(cm_33)
#precision    recall  F1_Score
# 0.6226415 0.6346154 0.6285714


## 4.3) XG Boost
library(xgboost)
### Convert the train and test data into xgboost matrix type
X_train <- data.matrix(training_set[,c(3,4,7,8)])
y_train <- data.matrix(training_set[,9])
X_test <- data.matrix(test_set[,c(3,4,7,8)])
y_test <- data.matrix(test_set[,9])

xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)

### Model Construction
num_class = 2
params = list(booster = 'gbtree',
              eta = 0.3,
              max_depth=3,
              gamma=3,
              subsample=0.75,
              colsample_bytree=1,
              objective="multi:softprob",
              eval_metric="mlogloss",
              num_class=num_class)

### Train the XGBoost classifer
set.seed(1234)
xgb.fit=xgb.train(
  params=params,
  data=xgboost_train,
  nrounds=100,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgboost_train,val2=xgboost_test),
  verbose=0,
  nfold = 5
)

### Predict outcomes with the test data
xgb.pred = predict(xgb.fit,X_test,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = c(0,1)


### Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$prediction = as.numeric(xgb.pred$prediction)
cm_xgb <- table(actual = test_set[,9], pred = xgb.pred$prediction)

### XGB Evaluation
xgb_accuracy <- (sum(diag(cm_xgb)))/sum(cm_xgb)
xgb_accuracy # 0.588785
score_evaluation(cm_xgb)
#precision    recall    F1_Score
# 0.5833333  0.5384615     0.56

## 4.4) Gaussian Process Classifier
library(kernlab)
gauss_model <- gausspr(training_set[,c(3,4,7,8)],training_set[,9], kernel = 'polydot') # polydot kernel
#gauss_model <- gausspr(label~., data = training_set[,c(3,4,7,9),])
gauss_pred <- predict(gauss_model, test_set[,c(3,4,7,8)])
gauss_prob <- predict(gauss_model, test_set[,c(3,4,7,8)], type = 'probabilities')
cm_gpc <- table(test_set$label, gauss_pred)
### GPC Evaluation
gpc_accuracy <- (sum(diag(cm_gpc)))/sum(cm_gpc)
gpc_accuracy # 0.6448598
score_evaluation(cm_gpc)
#precision     recall   F1_Score
# 0.6129032  0.7307692  0.6666667

## 4.5) Random Forest
library(randomForest)
rf_model <- randomForest(label~., data = training_set[,c(3,4,7,8,9)],
                         ntree = 20, proximity = T)
rf_pred <- predict(rf_model, newdata = test_set[,c(3,4,7,8)])
cm_rf <- table(test_set$label,rf_pred)
# RF Evaluation
rf_accuracy <- (sum(diag(cm_rf)))/sum(cm_rf)
rf_accuracy # 0.5140187
score_evaluation(cm_rf)
#precision    recall  F1_Score
#  0.5     0.4807692   0.4901961

## 4.6) Artificial Neural Network
library(neuralnet)
nn <- neuralnet(as.factor(label)~., data = training_set[,c(3,4,7,8,9)],
                hidden = 3, act.fct = 'logistic', linear.output = F)

plot(nn)
nn_predict <- compute(nn, test_set[,c(3,4,7,8)])
nn_predict$net.result

nn_prob <- nn_predict$net.result
nn_pred <- ifelse(nn_prob > 0.35, 1, 0)
cm_nn <- table(test_set$label, nn_pred[,2])

# ANN Evaluation
nn_accuracy <- (sum(diag(cm_nn)))/sum(cm_nn)
nn_accuracy # 0.5140187
score_evaluation(cm_nn)
#precision    recall   F1_Score
#   0.5    0.9423077  0.6533333



