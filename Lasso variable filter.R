setwd("C:/Users/Administrator/Desktop/Project in HKBU/新建文件夹")
library(readxl)
library(car)

data<-read_excel("data.xlsx")
data2<-read_excel("2020年城市人均工资标准.xlsx")

data$学历要求[which(data$学历要求=="博士")]<-"4"
data$学历要求[which(data$学历要求=="硕士")]<-"3"
data$学历要求[which(data$学历要求=="本科")]<-"2"
data$学历要求[which(data$学历要求=="大专")]<-"1"
data$学历要求[which(data$学历要求=="中技")]<-"0"
data$学历要求[which(data$学历要求=="中专")]<-"0"
data$学历要求[which(data$学历要求=="高中")]<-"0"

for (i in data$所在地) {
  data$所在地[which(data$所在地==i)]<-data2$平均工资[which(data2$城市==i)]
  
}
data$学历要求<-as.numeric(data$学历要求)
data=na.omit(data)
data$所在地<- as.numeric(data$所在地)

###########################################
set.seed(22)
train.index <- sample(x=1:nrow(data), size=ceiling(0.8*nrow(data) ))

train = data[train.index, ]
test = data[-train.index, ]
########################################## Forward Stepwise Regression
null = lm(薪资 ~ 1, data = train)  
full = lm(薪资 ~ ., data = train)

forward.lm = step(null, 
                 # 從空模型開始，一個一個丟變數，
                 # 最大不會超過完整的線性迴歸
                 # (一定要加上界 upper=full，不可以不加) 
                 scope=list(lower=null, upper=full), 
                 direction="forward")
summary(forward.lm)
########################################## Backward Stepwise Regression
full = lm(薪资 ~ ., data = train)  
backward.lm = step(full, 
                   # 這裡可以加下界(lower=null)，也可以不加
                   scope = list(upper=full), 
                   direction="backward")  
summary(backward.lm)

########################################## Both Stepwise Regression
# 此案例中，剛好跟 forward 結果一樣
step(null, scope = list(upper=full), direction="both")
# 此案例中，剛好跟 backward 結果一樣
step(full, scope = list(upper=full), direction="both") 

##########################################比较三个模型的预测效果
# self-defined 的 R-squared 函式
R_squared <- function(actual, predict){
  mean_of_obs <- rep(mean(actual), length(actual))
  
  SS_tot <- sum((actual - mean_of_obs)^2)
  SS_reg <- sum((predict - mean_of_obs)^2)
  #SS_res <- sum((actual - predict)^2)
  R_squared <- SS_reg/SS_tot   #1 - (SS_res/SS_tot)
  R_squared
}


# 直接用 predict()來預測
lm.test = predict(full, test)
forward.test = predict(forward.lm, test)
backward.test = predict(backward.lm, test)

# 衡量 lm, forward, backward 的預測結果
c(R_squared(test$薪资, lm.test),
  R_squared(test$薪资, forward.test),
  R_squared(test$薪资, backward.test)
)

##########################################lasso
require(glmnet)
library(lasso2)
str(data)
set.seed(22)
train.index = sample(x=1:nrow(data),
                     size=ceiling(0.8*nrow(data)))

train = data[train.index, ]
test = data[-train.index, ]
data.matrix(data)
x=model.matrix(薪资~.,data)[,-1]
y=data$薪资
ridge = glmnet(x,y,alpha = 0,family = "gaussian")
lasso = glmnet(x,y,alpha = 1,family = "gaussian")
par(mfcol = c(1, 2))
plot(lasso, xvar='lambda', main="Lasso")
plot(ridge, xvar='lambda', main="Ridge")


##########################################如何找出最佳lambda
# 經由 cv 的手法，評估每個模型在不同 lambda 下 
# 的 cvm(mean cross-validated error)
cv.lasso = cv.glmnet(x , 
                     y , 
                     alpha = 1,  # lasso
                     family = "gaussian")

# 評估每個模型的 cvm(mean cross-validated error)後
# 取最小 cvm 模型所對應的 lambda
best.lambda = cv.lasso$lambda.min
best.lambda
plot(lasso, xvar='lambda', main="Lasso")
abline(v=log(best.lambda), col="blue", lty=5.5 )
##########################################Lasso 的變數挑選
coef(cv.lasso, s = "lambda.min")
# 如果要取出這些重要變數的名稱，可以這樣寫：
select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 # remove `Intercept` and 平移剩下的ind
select.ind # 第幾個變數是重要的 (不看 `Intercept`)
select.varialbes = colnames(train)[select.ind]
select.varialbes


lm(薪资 ~ ., train[, c(select.varialbes, "薪资")])

##########################################挑选变量
data1=data[select.ind]

set.seed(22)
train1.index = sample(x=1:nrow(data1),
                     size=ceiling(0.8*nrow(data1)))

train1 = data[train1.index, ]
test1 = data[-train1.index, ]
data.matrix(data1)
x1=model.matrix(薪资~.,data1)[,-1]
y1=data1$薪资
ridge1 = glmnet(x1,y1,alpha = 0,family = "gaussian")
lasso1 = glmnet(x1,y1,alpha = 1,family = "gaussian")
ridge = glmnet(x1 , 
               y1 , 
               alpha = 0, # ridge
               family = "gaussian")
cv.ridge = cv.glmnet(x1 , 
                     y1 , 
                     alpha = 0,  # ridge
                     family = "gaussian")
best.ridge.lambda = cv.ridge$lambda.min
ridge.test = predict(ridge, 
                     s = best.ridge.lambda, 
                     newx = x1)
R_squared(test1$薪资, ridge.test)
##########################################预测
# 1. 先用 glmnet() 建立基本的 Ridge / Lasso 模型
ridge = glmnet(x , 
               y , 
               alpha = 0, # ridge
               family = "gaussian")

# 2. 用 cv.glmnet() 找出最佳的懲罰值 best.lambda
cv.ridge = cv.glmnet(x , 
                     y , 
                     alpha = 0,  # ridge
                     family = "gaussian")
best.ridge.lambda = cv.ridge$lambda.min

# 3. 使用 predict()進行預測
ridge.test = predict(ridge, 
                     s = best.ridge.lambda, 
                     newx = x)

# 評估模型
R_squared(test$薪资, ridge.test)
