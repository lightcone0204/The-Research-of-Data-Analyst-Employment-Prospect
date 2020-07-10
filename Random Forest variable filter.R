setwd("C:/Users/Administrator/Desktop/Project in HKBU/新建文件夹")
library(readxl)
library(car)
library(e1071)
library(caret)


data1<-read_excel("data3.xlsx")
data2<-read_excel("2020年城市人均工资标准.xlsx")
unique(data1$学历要求)


data1$学历要求[which(data1$学历要求=="博士")]<-"4"
data1$学历要求[which(data1$学历要求=="硕士")]<-"3"
data1$学历要求[which(data1$学历要求=="本科")]<-"2"
data1$学历要求[which(data1$学历要求=="大专")]<-"1"
data1$学历要求[which(data1$学历要求=="中技")]<-"0"
data1$学历要求[which(data1$学历要求=="中专")]<-"0"
data1$学历要求[which(data1$学历要求=="高中")]<-"0"


for (i in data1$所在地) {
  data1$所在地[which(data1$所在地==i)]<-data2$平均工资[which(data2$城市==i)]
  
}

data1$学历要求<-as.numeric(data1$学历要求)
data1$所在地<-as.numeric(data1$所在地)

data3<-subset(data1,select=-c(职位名称,职位简介,职位详情,招聘人数,公司名称,类型,领域,公司详情,
                                keywords_职位简介,keywords_职位详情,keywords_领域,keywords_公司详情))
data3<- na.exclude(data3)

write.table (data3, file ="resultCsv.csv", sep =",", row.names =FALSE)

##############################################################
#五折交叉验证测试随机森林表现

a=data3[,1]

CVgroup <- function(k, datasize, seed) {
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k, ceiling(datasize/k))[1:datasize] #将数据分成K份，并生成的完整数据集n
  temp <- sample(n, datasize)  #把n打乱
  x <- 1:k
  dataseq <- 1:datasize 
  cvlist <- lapply(x, function(x) dataseq[temp==x])  #dataseq中随机生成10个随机有序数据列
  return(cvlist)
}

k <- 5
datasize <- nrow(data3)
cvlist <- CVgroup(k = k, datasize = datasize, seed = 1206)


data <- data3
pred <- data.frame() #存储预测结果
library(plyr)
m <- seq(60, 100, by = 5)                        ##如果数据量大尽量间隔大点，间隔过小没有实际意义
for (j in m) {                                    #j指的是随机森林树的数量
  progress.bar <- create_progress_bar("text")     #的`create_progress_bar`函数创建一个进度条，plyr包中
  progress.bar$init(k)                            #设置上面的任务数，几折就是几个任务
  
  for (i in 1:k) {
    train <- data[-cvlist[[i]],]                     #刚才通过cvgroup生成的函数
    test <- data[cvlist[[i]],]
    
    model <- randomForest(薪资 ~ ., data = train, ntree = j)   #建模，ntree=J指的树数
    prediction <- predict(model, subset(test, select = - 薪资))#预测
    
    randomtree <- rep(j, length(prediction))          #随机森林树的数量
    
    kcross <- rep(i, length(prediction))              #i第几次循环交叉，共K次
    
    temp <- data.frame(cbind(subset(test, select = 薪资), prediction, randomtree, kcross))
    #真实值、预测值、随机森林树数、测试组编号捆绑在一起组成新的数据框temp
    pred <- rbind(pred, temp) #temp按行和pred合并
    print(paste("随机森林：", j))
    #循环至树数j的随机森林模型。这样我们就可以根据pred记录的结果进行方差分析等等，进一步研究树数对随机森林准确性及稳定行的影响。
    progress.bar$step()
    #19行输出进度条，告知完成了这个任务的百分之几
  }
}
##############################################################

library(dplyr)
maefun <- function(pred, obs) mean(abs(pred - obs))
msefun <- function(pred, obs) mean((pred - obs)^2)
nmsefun <- function(pred, obs) mean((pred - obs)^2)/mean((mean(obs) - obs)^2)
eval <- pred %>% group_by(randomtree, kcross) %>%   #randomtree=j，kcross=i
  summarise(mae = maefun(prediction, 薪资),
            mse = msefun(prediction, 薪资),
            nmse = nmsefun(prediction, 薪资))

eval1<-eval
##############################################################
#查看65层随机森林表现，筛选变量
library(randomForest)
library(varSelRF)
library(pROC)

#按index拆分训练集与测试集
index <- sample(2,nrow(data3),replace = TRUE,prob=c(0.8,0.2))
traindata <- data3[index==1,]
testdata <- data3[index==2,]

#试验性训练模型
set.seed(1234)#设置随机种子数，确保以后再执行代码时可以得到一样的结果

salary_rf <- randomForest(薪资 ~ ., data=traindata,
                            ntree=65,important=TRUE)
salary_rf

salary_pred<-predict(salary_rf, newdata=testdata)
table(salary_pred,testdata$薪资)
roc<-multiclass.roc (as.ordered(testdata$薪资) ,as.ordered(salary_pred))
roc

varImpPlot(salary_rf)
im=importance(salary_rf)
im[order(im[,1],decreasing=T),]
##############################################################
#变量筛选后再次进行五折交叉验证
data4<-subset(data3,select=c(薪资,经验要求,学历要求,所在地,规模,领域_19,公司详情_46,领域_34,职位详情_41,
                                 职位详情_24,领域_22,职位简介_45,职位简介_15,职位详情_33,领域_35,职位详情_25))

a=data4[,1]

CVgroup <- function(k, datasize, seed) {
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k, ceiling(datasize/k))[1:datasize] #将数据分成K份，并生成的完整数据集n
  temp <- sample(n, datasize)  #把n打乱
  x <- 1:k
  dataseq <- 1:datasize 
  cvlist <- lapply(x, function(x) dataseq[temp==x])  #dataseq中随机生成10个随机有序数据列
  return(cvlist)
}

k <- 5
datasize <- nrow(data4)
cvlist <- CVgroup(k = k, datasize = datasize, seed = 1206)


data <- data4
pred <- data.frame() #存储预测结果
library(plyr)
m <- seq(5, 100, by = 5)                        ##如果数据量大尽量间隔大点，间隔过小没有实际意义
for (j in m) {                                    #j指的是随机森林树的数量
  progress.bar <- create_progress_bar("text")     #的`create_progress_bar`函数创建一个进度条，plyr包中
  progress.bar$init(k)                            #设置上面的任务数，几折就是几个任务
  
  for (i in 1:k) {
    train <- data[-cvlist[[i]],]                     #刚才通过cvgroup生成的函数
    test <- data[cvlist[[i]],]
    
    model <- randomForest(薪资 ~ ., data = train, ntree = j)   #建模，ntree=J指的树数
    prediction <- predict(model, subset(test, select = - 薪资))#预测
    
    randomtree <- rep(j, length(prediction))          #随机森林树的数量
    
    kcross <- rep(i, length(prediction))              #i第几次循环交叉，共K次
    
    temp <- data.frame(cbind(subset(test, select = 薪资), prediction, randomtree, kcross))
    #真实值、预测值、随机森林树数、测试组编号捆绑在一起组成新的数据框temp
    pred <- rbind(pred, temp) #temp按行和pred合并
    print(paste("随机森林：", j))
    #循环至树数j的随机森林模型。这样我们就可以根据pred记录的结果进行方差分析等等，进一步研究树数对随机森林准确性及稳定行的影响。
    progress.bar$step()
    #19行输出进度条，告知完成了这个任务的百分之几
  }
}
maefun <- function(pred, obs) mean(abs(pred - obs))
msefun <- function(pred, obs) mean((pred - obs)^2)
nmsefun <- function(pred, obs) mean((pred - obs)^2)/mean((mean(obs) - obs)^2)
eval <- pred %>% group_by(randomtree, kcross) %>%   #randomtree=j，kcross=i
  summarise(mae = maefun(prediction, 薪资),
            mse = msefun(prediction, 薪资),
            nmse = nmsefun(prediction, 薪资))

eval2<-eval
##############################################################
#25层随机森林模型
index <- sample(2,nrow(data4),replace = TRUE,prob=c(0.8,0.2))
traindata <- data4[index==1,]
testdata <- data4[index==2,]

#试验性训练模型
set.seed(1234)#设置随机种子数，确保以后再执行代码时可以得到一样的结果

salary_rf <- randomForest(薪资 ~ ., data=traindata,
                            ntree=40,important=TRUE)
salary_rf

salary_pred<-predict(salary_rf, newdata=testdata)
table(salary_pred,testdata$薪资)
roc<-multiclass.roc (as.ordered(testdata$薪资) ,as.ordered(salary_pred))
roc

varImpPlot(salary_rf)

plot(salary_rf)



