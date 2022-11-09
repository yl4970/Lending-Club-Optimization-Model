# 0 Required pkg ----
library(readr)
library(corrplot)
library(psych)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(magrittr)
library(car)
library(glmnet)
library(glmnetUtils)
library(randomForest)
library(FNN)
library(lpSolve)
library(lpSolveAPI)

# 1 Initial data pre-processing ----

loans.full <- read.csv("LoanStats3b.csv", skip=1L)
new_id <- (is.na(as.numeric(loans.full$id)))
loans.final <- loans.full[!new_id,]

## 1.1 Clean columns ----
## select and categorize columns
colnames(loans.final)
columns_to_pick <- c('id','loan_amnt','funded_amnt','term','int_rate',
                     'installment','grade','emp_length', 'home_ownership',
                     'annual_inc','issue_d','loan_status','purpose','dti', 
                     'delinq_2yrs','earliest_cr_line','pub_rec', 'revol_bal',
                     'revol_util', 'total_pymnt','last_pymnt_d', 'recoveries')

loans.final <- loans.final[columns_to_pick]  

## Processing employment length and term
loans.final['emp_length'] <- parse_number(loans.final[,'emp_length'])
loans.final['term_num'] <- parse_number(loans.final[,'term'])
loans.final <- na.omit(loans.final)

## all numeric columns are taken into float_columns
float_columns <- colnames(loans.final %>% select(where(is.numeric)))
## all character columns are taken into character_columns
character_columns <- colnames(loans.final %>% select(where(is.character)))

## find date columns and clean date
date_columns <- c('issue_d', 'earliest_cr_line', 'last_pymnt_d')
clean_date <- function(x) {
  if(is.na(x) | is.null(x) | is.nan(x)) {
    return(NA)
  } else {
    return(as.Date(paste("1",x,sep="-"),format="%d-%b-%Y"))
  }
}
for (i in date_columns) {
  loans.final[,i] <- sapply(loans.final[,i], clean_date)
}

## find percentage columns and clean percentage
percentage_columns <- c('int_rate', 'revol_util')
clean_percentage <- function(x){
  if(is.na(x) | is.null(x) | is.nan(x)){
    return(NA)
  } else {
    return(as.numeric(substr(x,1,nchar(x)-1)))
  }
}
for (i in percentage_columns) {
  loans.final[,i] <- sapply(loans.final[,i], clean_percentage)
}

## find categorical columns
categorical_columns <- setdiff(character_columns, c("id",percentage_columns,date_columns))
clean_cat <- function(x) {
  if(is.na(x) | is.null(x) | is.nan(x)) {
    return(NA)
  } else {
    return(x)
  }
}
for (i in categorical_columns) {
  loans.final[,i] <- sapply(loans.final[,i], clean_cat)
}

## confine loan status in 3 modes
loans.final <- loans.final[which(loans.final$loan_status %in% c("Fully Paid","Charged Off", "Default")) ,]
print(paste("Removed",n-nrow(loans.final),"rows"))
unique(loans.final$loan_status) # confirming

## delete columns not selected
rm("loans.full") # Save space by removing large files

## 1.2 Handle outliers ----
n <- nrow(loans.final)

## Create "loan length" & filter loans of no length
loans.final['loan_length'] <- (loans.final['last_pymnt_d'] - loans.final['issue_d']) /30
loans.final <- loans.final[which(loans.final$loan_length != 0), ]
print(paste("Removed",n-nrow(loans.final),"rows"))

## Outliers in annual income and revolving balance
loans.final <- loans.final[ which(loans.final$annual_inc <1000000, loans.final$revol_bal < 200000) ,]
print(paste("Removed",n-nrow(loans.final),"rows"))

## Final N/A removal
loans.final <- na.omit(loans.final)
print(paste("Removed",n-nrow(loans.final),"rows"))

## 1.3 Save the data ----
write.csv(loans.final,'loans_data.csv')
loans.final <- read.csv('loans_data.csv')[-1]

# 2 Define return metrics ----
return_columns <- c("ret_PESS", "ret_OPT", "ret_INTa", "ret_INTb", "ret_INTc")

## 2.1 Pessimistic return ----
loans.final['ret_PESS'] <- ((loans.final['total_pymnt'] - loans.final['funded_amnt']) / loans.final['funded_amnt']) * (12 / loans.final['term_num'])

## 2.2 Optimistic return ----
loans.final['ret_OPT'] <- ((loans.final['total_pymnt'] - loans.final['funded_amnt']) / loans.final['funded_amnt']) * (12 / loans.final['loan_length'])
loans.final[which(loans.final$ret_OPT < 0),'ret_OPT'] <- loans.final[which(loans.final$ret_OPT < 0),'ret_PESS']

## 2.3 Return by interest rate
return_method_3 <- function(t,r) {
  actual_installment <- (loans.final['total_pymnt'] - loans.final['recoveries']) / loans.final['loan_length']
  cash_by_end_of_loan <- actual_installment * (1 - (1+r) ^ loans.final['loan_length']) / (1 - (1+ r))
  cash_by_end_of_loan <- cash_by_end_of_loan + loans.final['recoveries']
  remaining_months <- t - loans.final['loan_length']
  final_return <- cash_by_end_of_loan * (1+r)^remaining_months
  return( (12/t) * (( final_return - loans.final['funded_amnt']) / loans.final['funded_amnt']) )
}

## Apply the above function with a time horizon of 5 years and three interest rates of 1%, 2.5% and 5%
loans.final['ret_INTa'] <- return_method_3(5*12, 0.001)
loans.final['ret_INTb'] <- return_method_3(5*12, 0.0025)
loans.final['ret_INTc'] <- return_method_3(5*12, 0.005)

loans.final <- na.omit(loans.final)

## 2.4 Update numeric columns ----
numerical_columns <-c('ret_INTa', 'ret_INTb', 'ret_INTc', 'ret_PESS', 'ret_OPT', 
                      'term_num', float_columns)

# 3 Data analytics ----

## 3.1 Study data by grade ----
## Compute number of loans by grade
(number_by_grade <- table(loans.final[,'grade']))

## Find percentage of defaults by grade
(defaults_by_grade <- table(subset(loans.final,loan_status != "Fully Paid")[,'grade']))
(pct_defaults_by_grade <- 100*defaults_by_grade/number_by_grade)

## Get the average interest rate by grade
(int_rate_by_grade <- aggregate(loans.final[,'int_rate'],list(loans.final$grade), mean))
### ! from the table above, notice that the decline in interest rate by grade is quite linear
### hence populate the graph below
ggplot(loans.final, aes(x=grade, y=int_rate)) + geom_point()
### the linear relationship states direct correlation between grade and interest rate
### this is conforming to intuition as interest rate should be determined after grade is assigned
### for the clustering analyses below, the direct relationship is spilling leakage
### and won't be helpful for model training
### hence will take out from data set for model training

## 3.2 Covariance heat map ----
num_col_corr <- loans.final[,numerical_columns]
corrplot(cor(num_col_corr), order="hclust")
pairs.panels(loans.final[sample(nrow(loans.final),size=1000),numerical_columns])

# 4 K means ----
analysis_columns <- c('ret_OPT', 'dti', 'loan_amnt', 'funded_amnt', 'installment', 
                      'annual_inc','revol_bal', 'delinq_2yrs', 'pub_rec', 'emp_length',
                      'total_pymnt', 'recoveries','int_rate', 'revol_util', 'grade')

## 4.1 Data finalization ----
loans.analysis <- loans.final[, analysis_columns]
loans.analysis <- na.omit(loans.analysis)
str(loans.analysis)

## Drop grade and interest rate
## Scale/Standardize data set with mean 0 and variance 1
sc.loans.analysis <- scale(loans.analysis[,-c(13,15)])

## 4.2 K-means model ----
K = 3
k_means_features <- c('annual_inc','revol_bal','emp_length','loan_amnt') # selection after trials
set.seed(1248765792)
clusters <- kmeans(sc.loans.analysis[,k_means_features],centers=K) # cluster by 3 group

## 4.3 K-means results analysis ----
confusion_df <- table(loans.analysis$grade, clusters$cluster)
addmargins(confusion_df)
## Row percentage
round(100*prop.table(confusion_df,1),digits=2)
## Column percentages
round(100*prop.table(confusion_df,2),digits=2)
### determine great clustering as results show clear distinction

## 4.4 Parallel plot ----
knames=as.character(1:K)  
round(clusters$centers,2)  
parallelplot(clusters$centers,auto.key=list(text=knames,space="top",columns=1,lines=T),scales=list(cex=.5))

## 4.5 Save the data ----
write.csv(loans.analysis,'loans_analysis.csv')
write.csv(loans.final,'ret_loans_data.csv')

# 5 Decision Trees ----
## 5.1 Add "credit history" ----
loans.final[,'cr_hist'] <- (loans.final$issue_d - loans.final$earliest_cr_line) /30
float_columns <- c(float_columns, 'cr_hist')
loans.final <- na.omit(loans.final,cols=drop_na_columns)

## 5.2 Add "defult"
loans.final[,'default'] <- as.factor(as.integer(loans.final$loan_status %in% c('Charged Off','Default')))

## 5.3 Decision trees ----
DT_features <- c('annual_inc','loan_amnt') # selection after trials
## Test/train sets
loans.pred <- downSample(loans.final[,DT_features],loans.final$default,yname='default')

set.seed(2314513)
N <- nrow(loans.pred)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction
loans.train <- loans.pred[train_idxs,]
loans.test <- loans.pred[test_idxs,]

## Decision trees model
ftree <- rpart(default ~ .,data=loans.train)
par(mfrow=c(1,1))         # reset one graphic per panel
plot(ftree); text(ftree)  # simple graph
prp(ftree)                # tree graph
prp(ftree,extra=101)      # add the size and proportion of data in the node

## 5.3 DT results analysis ----
fit.pr.tree <- predict(ftree,loans.test,type="prob")[,2]
fit.pred.tree <- prediction(fit.pr.tree,loans.test$default)

fit.perf.tree <- performance(fit.pred.tree,"tpr","fpr")
plot(fit.perf.tree,lwd=2,col="blue",
     main="ROC:  Classification Trees on Loan Default Dataset")
abline(a=0,b=1)

## AUROC by ROCR
auc.tmp <- performance(fit.pred.tree,"auc")
(auc.tree <- as.numeric(auc.tmp@y.values))

# 6 Logistic regression ----
LR_features <- c('term', 'dti', 'revol_util', 'purpose', 'annual_inc', 'loan_amnt') # selection after trial

## Test/train sets
loans.pred <- downSample(loans.final[,features],loans.final$default, yname='default')

set.seed(2314513)
N <- nrow(loans.pred)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction
loans.train <- loans.pred[train_idxs,]
loans.test <- loans.pred[test_idxs,]

## 6.1 LR model ----
## first estimate the null model (this just has an intercept)
null <- glm(default~1,data=loans.train,family="binomial")
## second estimate a complete model (with all variables that you are interested in)
full <- glm(default~.,data=loans.train,family="binomial") 

## finally estimate the step wise regression starting with the null model
swlrmdl <- step(null, scope=formula(full),steps=15,dir="forward")
summary(swlrmdl)

## 6.2 PD forecast----
lr.default.prob = predict(swlrmdl, newdata=loans.test, type="response")
pred.lr = prediction(lr.default.prob, loans.test$default)
perf.lr = performance(pred.lr, measure = "tpr", x.measure = "fpr") 

## 6.3 ROC comparison ----
plot(perf.lr,col="red"); abline(a=0,b=1)
plot(fit.perf.tree,add=TRUE,col="blue")
legend("bottomright",c("LogRegr","Tree"),pch=15,col=c("red","blue"),bty="n")

# 7 Regularization ----
## Train/test sets
set.seed(2314513)
N <- nrow(loans.final)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction

reg_features <- c( "purpose", "emp_length", "home_ownership",
                     "annual_inc", "dti", "int_rate")

ret_col <- "ret_OPT"
regression.train <- loans.final[train_idxs,c(ret_col,reg_features)]
regression.test <- loans.final[test_idxs,c(ret_col,reg_features)]

## 7.1 Lasso regularization ====
lasso.mod <- glmnet(ret_OPT ~ . - funded_amnt - grade, data=regression.train,alpha = 1)
plot(lasso.mod)

lasso.cv <- cv.glmnet(ret_OPT ~ . - funded_amnt - grade, data=regression.train,alpha = 1)
plot(lasso.cv)
coef(lasso.cv, s = "lambda.1se") # use 1se to drop out more features

final.lasso.mod <- lm(ret_OPT ~  purpose + home_ownership +
                        annual_inc + dti + int_rate,data=regression.train)

summary(final.lasso.mod)
vif(final.lasso.mod)
confint(final.lasso.mod)

(finallassoreg_mspe <- mean((regression.test$ret_OPT - predict(final.lasso.mod,regression.test))^2))
(lassoreg_mspe <- mean((regression.test$ret_OPT - predict(lasso.cv,regression.test,s="lambda.1se")) ^ 2))


## 7.2 Ridge regularization ----
ridge.mod <- glmnet(ret_OPT ~ . - funded_amnt - grade,data=regression.train,alpha=0)
plot(ridge.mod)

ridge.cv <- cv.glmnet(ret_OPT ~ . - funded_amnt - grade,data=regression.train,alpha=0)
plot(ridge.cv)
coef(ridge.cv, s = "lambda.1se")

(ridgereg_mspe <- mean((regression.test$ret_OPT - predict(ridge.cv,regression.test,s="lambda.1se")) ^ 2))

## 7.3 Strategy comparison ====
num_assort <- c(seq(20,1000, by = 5))
data <- loans.final[,c(ret_col,c(LR_features, reg_features))]

## Logistic Regression
binary_train <- loans.final[train_idxs,c('default',LR_features)]
binary_model <- glm(default ~ loan_amnt + annual_inc + dti + revol_util + purpose+ term, data=binary_train,family="binomial")

binary_datalist = vector("list", length = length(num_assort))
for (i in seq_along(num_assort)){
  default_prob <- predict(binary_model,newdata=data,type='response')
  ranking <- order(default_prob)
  selected_rows <- data[ranking[1:num_assort[[i]]],]
  binary_datalist[i] = round(mean(selected_rows[,ret_col]), digits = 4) * 100
}

## Regularized Regression: Lasso
reg_train <- loans.final[train_idxs,c(ret_col,reg_features)]
reg_model <- cv.glmnet(ret_OPT ~ home_ownership + purpose + 
                         emp_length + annual_inc + dti + int_rate,data=reg_train,alpha=0)

reg_datalist = vector("list", length = length(num_assort))
for (i in seq_along(num_assort)){
  pred_returns <- predict(reg_model,data)
  ranking <- order(-pred_returns)
  selected_rows <- data[ranking[1:num_assort[[i]]],]
  reg_datalist[i] = round(mean(selected_rows[,ret_col]), digits = 4) * 100
}

## Random select: baseline
rand_datalist = vector("list", length = length(num_assort))
selected_rows <- data[sample(nrow(data), 1000),]
for (i in seq_along(num_assort)) {
  selected_rows1 <- selected_rows[1:num_assort[i],ret_col]
  rand_datalist[i] = round(mean(selected_rows1), digits = 4) * 100
}

## Results
results <- data.frame(matrix(nrow = 0, ncol = 4))
results <- cbind(num_assort, binary_datalist, reg_datalist, rand_datalist)
colnames(results) = c("num_loans","Risk Strategy", "Return Strategy","Baseline")
results

## Plot
matplot(num_assort, cbind(binary_datalist, reg_datalist, rand_datalist), 
        type = "l", lty = "solid", lwd = 2, 
        main = "Strategy Comparison",
        xlab = "Number of Loans",
        ylab = "Return",
        cex.main = 1, cex.lab = 0.8, cex.axis = 0.6,
        col = c("blue","red","forestgreen"))
legend("right", legend = colnames(results)[2:4], cex = 0.7, 
       lty = "solid", text.font = 1, 
       col = c("blue","red","forestgreen"))

# 8 Optimization ----

## Numerize categorical variables
loans.final$new_term  <- unclass(factor(loans.final$term))
loans.final$new_home_ownership <- unclass(factor(loans.final$home_ownership))
loans.final$new_purpose  <- unclass(factor(loans.final$purpose))
## Remove NA rows
loans.final <- na.omit(loans.final)

## 8.1 K-means to assign sd to each loan ----
cluster_columns <- c('int_rate', 'annual_inc','loan_amnt','emp_length',
                     'funded_amnt','dti','pub_rec','cr_hist')
loans.mean=colMeans(loans.final[,cluster_columns])
loans.sd=apply(loans.final[,cluster_columns],2,sd)

for (i in 1:K) {
    st_dev_values[i,] = sd(loans.train[clusters$cluster == i,j])
  }

loans.final[,cluster_columns] <- scale(loans.final[,cluster_columns])

## Train/test sets
set.seed(2314513)
N <- nrow(loans.final)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction

loans.train <- loans.final[train_idxs,]
loans.test <- loans.final[test_idxs,]

## K-means model
K_max <- 100
wcss <- numeric(K_max)
for(K in 1:K_max) {
  clustersK <- kmeans(loans.train[,cluster_columns],centers=K)
  wcss[K] <- clustersK$tot.withinss
}
plot(1:K_max,wcss,main="Within Cluster Sum of Squares vs K",xlab = 'K',ylab = 'WCSS')
lines(1:K_max,wcss)
## According to WCSS graph, ideal K is selected at 9
K <- 9
clusters <- kmeans(loans.train[,cluster_columns],centers=K,nstart=50)
## Assign cluster centers
loans.clusters <- get.knnx(clusters$centers,loans.final[,cluster_columns],1)$nn.index[,1]

## Use the clustering to assign a standard deviation to each loan
st_dev_values <- as.data.frame(matrix(nrow=K,ncol=1))
for (i in 1:K) {
  st_dev_values[i,1] <- sd(loans.train[clusters$cluster == i,"ret_OPT"])
}
for (i in 1:nrow(loans.final)) {
    loans.final[i,"std_OPT"] <- st_dev_values[loans.clusters[i],1]
}

## Re-scale the continuous features
loans.final[,cluster_columns]=sweep(loans.final[,cluster_columns],MARGIN=2,loans.sd[cluster_columns],'*')       
    ## step 1) scale: multiply the corresponding sd
loans.final[,cluster_columns]=sweep(loans.final[,cluster_columns],MARGIN=2,loans.mean[cluster_columns],'+')  
    ##step 2) shift: add the original mean

## pick out the test set
loans.test <- loans.final[test_idxs,]

## 8.2 Lasso model to predict return----
## Train/test sets
regression.train <- loans.final[train_idxs,c("ret_OPT",LR_features)]
regression.test <- loans.final[test_idxs,c("ret_OPT",LR_features)]

## Lasso
lasso.mod <- glmnet(ret_OPT ~ .-funded_amnt - grade,data=regression.train,alpha=1)
summary(lasso.mod)
plot(lasso.mod)

lassoreg <- cv.glmnet(ret_OPT ~ .   - funded_amnt - grade,data=regression.train,alpha=1)
summary(lassoreg)
plot(lassoreg)
coef(lassoreg,s="lambda.1se")

test.predn <- data.frame(rownames(regression.test))
test.predn$lassoret_OPT <- predict(lassoreg,regression.test,s="lambda.1se")

loans.test.aug <- cbind(loans.test,test.predn)

## Save output
output_cols <- c('ret_OPT', 'lassoret_OPT','std_OPT','loan_amnt','int_rate','default','total_pymnt','recoveries',
                 'home_ownership','term','purpose','emp_length','annual_inc','dti','delinq_2yrs','pub_rec',
                 'revol_util','cr_hist','grade')
sample_size <- 2000 # maybe no more than 10,000, or the size of the test set
set.seed(98237)
output_rows <- sample(nrow(loans.test),sample_size)
write.csv(loans.test.aug[output_rows,output_cols],'loans_pred_opti_data.csv')

## 8.3 Optimization ====
loans.pred.opti.data <- loans.test.aug[output_rows,output_cols]

## Simple Knapsack model
f.obj <- loans.pred.opti.data$loan_amnt*loans.pred.opti.data$lassoret_OPT[1:nrow(loans.pred.opti.data)]
f.con <- matrix(c(loans.pred.opti.data$loan_amnt,
                  rep(1, times=nrow(loans.pred.opti.data)),
                  rep(1, times=nrow(loans.pred.opti.data))), 
                nrow = 3, byrow = TRUE)
f.dir <- c("<=", "<=", ">=")

## Picking portfolio size
max_assort <- c(seq(50,200,5))
skm <- vector("list", length = length(max_assort))
num_loan <- vector("list", length = length(max_assort))
for (i in seq_along(max_assort)) {
  f.dir <- c("<=",
              "<=",
              ">=")
  skm[i] <- lp("max", f.obj, f.con, f.dir, c(400000,max_assort[i],max_assort[i]-50), 
               int.vec = 1:nrow(loans.test.aug), all.bin = TRUE)$objval
  num_loan[i] <- sum(lp("max", f.obj, f.con, f.dir, c(400000,max_assort[i],max_assort[i]-50), 
                        int.vec = 1:nrow(loans.test.aug), all.bin = TRUE)$solution)
}

## Plot
matplot(cbind(num_loan)[1:31], cbind(skm)[1:31], 
        lty = "solid", type = "l", lwd = 2,
        main = "Optimal Revenue Based on Number of Selected Loans",
        xlab = "Number of Loans",
        ylab = "Revenue",
        col = c("blue", ))
legend("right", legend = cbind("Mediume Risk"), cex = 0.7, 
       lty = "solid", text.font = 1, 
       col = c("blue"))

## 8.4 Strategy variations ====
### 8.4.1 Aggressive model ====
aggres <- subset(loans.test.aug, (loans.test.aug$grade %in% c("D","E","F", "G")))
output_rows <- sample(nrow(aggres),sample_size)
aggres <- aggres[output_rows,output_cols]

f.obj3 <- aggres$loan_amnt*aggres$lassoret_OPT[1:nrow(aggres)]
f.con3 <- matrix(c(aggres$loan_amnt,
                   rep(1, times=nrow(aggres)),
                   rep(1, times=nrow(aggres))), 
                 nrow = 3, byrow = TRUE)
skm3 <- lp("max", f.obj3, f.con3, f.dir, f.rhs, int.vec = 1:nrow(aggres), all.bin = TRUE)$objval
opt_strat3  <- lp("max", f.obj3, f.con3, f.dir3, f.rhs3, int.vec = 1:nrow(aggres), all.bin = TRUE)$solution
aggres$select_YN <- opt_strat3
table(aggres$grade, aggres$select_YN)

Regular_selected3 <- aggres[which(aggres$select_YN != 0), ]
return_grade3 <- aggregate((Regular_selected3$lassoret_OPT*Regular_selected3$loan_amnt), list(Regular_selected3$grade), FUN=sum) 
colnames(return_grade3) <- c("Grade", "Return")
return_grade3

## 8.4.2 Conservative model ====
conserv <- subset(loans.test.aug, (loans.test.aug$grade %in% c("A","B","C")))
output_rows <- sample(nrow(conserv),sample_size)
conserv <- conserv[output_rows,output_cols]

f.obj2 <- conserv$loan_amnt*conserv$lassoret_OPT[1:nrow(conserv)]
f.con2 <- matrix(c(conserv$loan_amnt,
                   rep(1, times=nrow(conserv)),
                   rep(1, times=nrow(conserv))), 
                 nrow = 3, byrow = TRUE)
f.rhs2 <- c(400000, 200, 20)
skm2 <- lp("max", f.obj2, f.con2, f.dir, f.rhs2, int.vec = 1:nrow(conserv), all.bin = TRUE)$objval
opt_strat2  <- lp("max", f.obj2, f.con2, f.dir2, f.rhs2, int.vec = 1:nrow(conserv), all.bin = TRUE)$solution
conserv$select_YN <- opt_strat2
table(conserv$grade, conserv$select_YN)

Regular_selected2 <- conserv[which(conserv$select_YN != 0), ]
return_grade2 <- aggregate((Regular_selected2$lassoret_OPT*Regular_selected2$loan_amnt), list(Regular_selected2$grade), FUN=sum) 
colnames(return_grade2) <- c("Grade", "Return")
return_grade2

## 8.4 Budget sensitivity analysis ====
budget_assort <- c(seq(10000,40000,100))
skm_budget <- vector("list", length = length(budget_assort))
num_loan_budget <- vector("list", length = length(budget_assort))
for (i in seq_along(budget_assort)) {
  f.dir <- c("<=","<=",">=")
  skm_budget[i] <- lp("max", f.obj, f.con, f.dir, c(budget_assort[i],25,10), int.vec = 1:nrow(loans.pred.opti.data), all.bin = TRUE)$objval
}

matplot(budget_assort[24:length(budget_assort)], skm_budget[24:length(budget_assort)], 
        type = "l", lty = "solid", lwd = 2,
        main = "Revenue Based on Investment Budget",
        xlab = "Budget",
        ylab = "Revenue",
        col = c("blue"))










































































