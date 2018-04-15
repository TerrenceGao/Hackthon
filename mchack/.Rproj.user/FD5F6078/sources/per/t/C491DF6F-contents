library(data.table)
library(Matrix)
needs(dplyr)
needs(MLmetrics)
library(lightgbm)
set.seed(257)
needs(tidyr)


train = fread("train_ajEneEa.csv") %>% as.data.frame()
test  = fread("test_v2akXPA.csv")  %>% as.data.frame()
for( i in 1:ncol(train)){
  cat("The missing number of ",i,"variable.")
  print(sum(is.na(train[,i])))
}

#missing impute/ empty value
library(Hmisc)
train$bmi<-impute(train$bmi, median)
train$smoking_status[which(train$smoking_status=='')]<-"unknown"
train$smoking_status_value<-1
train1<-spread(train,smoking_status,smoking_status_value)
unique(train1$work_type)
train1[is.na(train1)]<-0
train1<-train1[,c(1:10,12:15,11)]
train1$bmi<-as.numeric(train1$bmi)

train1$gender<-ifelse(train1$gender=='Male',1,0)
train1$ever_married<-ifelse(train1$ever_married=='Yes',1,0)
train1$Residence_type<-ifelse(train1$Residence_type=='Urban',1,0)
train1$work_type_value<-1
train2<-spread(train1,work_type,work_type_value)
train2[is.na(train2)]<-0
train2<-train2[,c(1:13,15:19,14)]


train_label<-train2[,19]
train_data<-train2[,-19]

train3<-data.matrix(train_data)
train3<-Matrix(train3, sparse=TRUE)


#Model
lgb.train = lgb.Dataset(data=train3,label=train_label)
categoricals.vec = colnames(train1)[c(grep("cat",colnames(train)))]
  
#parameters 
lgb.grid = list(objective = "binary",
                  metric = "auc",
                  min_sum_hessian_in_leaf = 1,
                  feature_fraction = 0.7,
                  bagging_fraction = 0.7,
                  bagging_freq = 5,
                  min_data = 100,
                  max_bin = 50,
                  lambda_l1 = 8,
                  lambda_l2 = 1.3,
                  min_data_in_bin=100,
                  min_gain_to_split = 10,
                  min_data_in_leaf = 30,
                  is_unbalance = TRUE)

# Gini for Lgb
lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}


lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.02, num_leaves = 25,
                   num_threads = 2 , nrounds = 7000, early_stopping_rounds = 50,
                   eval_freq = 20, eval = lgb.normalizedgini,
                  categorical_feature = categoricals.vec, nfold = 5, stratified = TRUE)

best.iter = lgb.model.cv$best_iter


# Train final model
lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.02,
                      num_leaves = 25, num_threads = 2 , nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini,
                      categorical_feature = categoricals.vec)


preds =data.frame(test[,1],predict(lgb.model,test))


colnames(preds)<-c("id","stroke")
fwrite(preds, "submission.csv")

#test

test$bmi<-impute(test$bmi, median)
test$smoking_status[which(test$smoking_status=='')]<-"unknown"
test$smoking_status_value<-1
test<-spread(test,smoking_status,smoking_status_value)
unique(train1$work_type)
test[is.na(test)]<-0
test$bmi<-as.numeric(test$bmi)

test$gender<-ifelse(test$gender=='Male',1,0)
test$ever_married<-ifelse(test$ever_married=='Yes',1,0)
test$Residence_type<-ifelse(test$Residence_type=='Urban',1,0)
test$work_type_value<-1
test<-spread(test,work_type,work_type_value)
test[is.na(test)]<-0

test<-data.matrix(test)
test<-Matrix(test, sparse=TRUE)

