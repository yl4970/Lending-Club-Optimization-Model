#Packages====
if(!require(readr)) { install.packages("readr") }
library(readr)
library(magrittr)
library(dplyr)
library(caret)
library(car)

if (!require(glmnet)){ install.packages('glmnet') }; library("glmnet")
if (!require(glmnetUtils)){ install.packages("glmnetUtils")}; library("glmnetUtils")
library(randomForest)
library(rpart)
library(rpart.plot)

#Load in data====
loans.final <- read.csv('ret_loans_data.csv')[-1]

#Preprocessing====
## Add in Credit History and Default
loans.final[,'cr_hist'] <- (loans.final$issue_d - loans.final$earliest_cr_line) /30
loans.final[,'default'] <- as.factor(as.integer(loans.final$loan_status %in% c('Charged Off','Default')))

## Remove N/A
loans.final <- na.omit(loans.final)

## Features
features <- c('home_ownership','term','purpose','emp_length',
              'loan_amnt','funded_amnt','annual_inc','dti','revol_bal','delinq_2yrs',
              'pub_rec','revol_util', 'cr_hist','grade','int_rate')

#Prepare train/test sets====
set.seed(2314513)
N <- nrow(loans.final)
fraction <- 0.7                     # fraction of examples to put in training set
rand_values <- runif(N)
train_idxs <- rand_values <= fraction
test_idxs <- rand_values > fraction

#Return metrics====
ret_col <- "ret_OPT"
# Options: "ret_PESS", "ret_OPT", "ret_INTa", "ret_INTb", "ret_INTc"
regression.train <- loans.final[train_idxs,c(ret_col,features)]
regression.test <- loans.final[test_idxs,c(ret_col,features)]

#L1 Regularized Linear Regression: Lasso====
lasso.mod <- glmnet(ret_OPT ~ . - funded_amnt - grade, data=regression.train,alpha = 1)
plot(lasso.mod)

lasso.cv <- cv.glmnet(ret_OPT ~ . - funded_amnt - grade, data=regression.train,alpha = 1)
plot(lasso.cv)
coef(lasso.cv, s = "lambda.min")

final.lasso.mod <- lm(ret_OPT ~ home_ownership + purpose + 
                        emp_length + annual_inc + dti + int_rate,data=regression.train)
summary(final.lasso.mod)
vif(final.lasso.mod)
confint(final.lasso.mod)

(finallassoreg_mspe <- mean((regression.test$ret_OPT - predict(final.lasso.mod,regression.test))^2))
(lassoreg_mspe <- mean((regression.test$ret_OPT - predict(lassoreg,regression.test,s="lambda.min")) ^ 2))


#L2 Regularized Linear Regression: Ridge====
ridge.mod <- glmnet(ret_OPT ~ . - funded_amnt - grade,data=regression.train,alpha=0)
plot(ridge.mod)

ridge.cv <- cv.glmnet(ret_OPT ~ . - funded_amnt - grade,data=regression.train,alpha=0)
plot(ridge.cv)
coef(ridge.cv, s = "lambda.1se")

# Ridge was not selective on variables
(ridgereg_mspe <- mean((regression.test$ret_OPT - predict(ridge.cv,regression.test,s="lambda.min")) ^ 2))
(ridgereg_mspe <- mean((regression.test$ret_OPT - predict(ridge.cv,regression.test,s="lambda.1se")) ^ 2))

#Heuristics for selecting loans====
num_assort <- c(seq(20,1000, by = 5))
data <- loans.final[,c(ret_col,features)]

# Logistic Regression from previous weeks
binary_train <- loans.final[train_idxs,c('default',features)]
binary_model <- glm(default ~ loan_amnt + annual_inc + dti + revol_util + purpose+ term, data=binary_train,family="binomial")

binary_datalist = vector("list", length = length(num_assort))
for (i in seq_along(num_assort)){
  default_prob <- predict(binary_model,newdata=data,type='response')
  ranking <- order(default_prob)
  selected_rows <- data[ranking[1:num_assort[[i]]],]
  binary_datalist[i] = round(mean(selected_rows[,ret_col]), digits = 4) * 100
}

# Regularized Regression: Lasso
reg_train <- loans.final[train_idxs,c(ret_col,features)]
reg_model <- cv.glmnet(ret_OPT ~ home_ownership + purpose + 
                         emp_length + annual_inc + dti + int_rate,data=reg_train,alpha=0)

reg_datalist = vector("list", length = length(num_assort))
for (i in seq_along(num_assort)){
  pred_returns <- predict(reg_model,data)
  ranking <- order(-pred_returns)
  selected_rows <- data[ranking[1:num_assort[[i]]],]
  reg_datalist[i] = round(mean(selected_rows[,ret_col]), digits = 4) * 100
}

# Random select: baseline

rand_datalist = vector("list", length = length(num_assort))
for (i in seq_along(num_assort)) {
  selected_rows1 <- data[sample(nrow(data), num_assort[[i]]),]
  selected_rows2 <- data[sample(nrow(data), num_assort[[i]]),]
  selected_rows3 <- data[sample(nrow(data), num_assort[[i]]),]
  rand_datalist[i] = round(mean(c(selected_rows1[,ret_col], 
                                selected_rows2[,ret_col],
                           selected_rows3[,ret_col])), digits = 4) * 100
}

#Results====
results <- data.frame(matrix(nrow = 0, ncol = 4))
results <- cbind(num_assort, binary_datalist, reg_datalist, rand_datalist)
colnames(results) = c("num_loans","Risk Strategy", "Return Strategy","Baseline")
results

#Plot====
matplot(num_assort, cbind(binary_datalist, reg_datalist, rand_datalist), 
        type = "l", lty = "solid", lwd = 2,
        main = "Strategy Comparison",
        xlab = "Number of Loans",
        ylab = "Return",
        col = c("blue","red","forestgreen"))
legend("bottomright", legend = colnames(results)[2:4], cex = 0.4, 
       lty = "solid", lwd = 2,
       col = c("blue","red","forestgreen"))
