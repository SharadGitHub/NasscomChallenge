

correlation = cor(health1[, 3:62])

highlycorrelated = findCorrelation(correlation, cutoff = 0.80)
print(highlycorrelated)

colnames(health1)[highlycorrelated[1:17]]

### removing highly correlated values
library(corrplot)
corrplot(correlation, method= "number", col= "black" )
corrplot(correlation, order = "AOE", addCoef.col = "grey")
health2= health2[, -highlycorrelated]
str(health2)
dim(health2)

write.csv(health2, file = "MyData.csv")



#### removing zero variance and near zero variance values
health2= health2[,-1]
nearZeroVar(health2, saveMetrics = T)
##### there were no such values found

library(randomForest)
install.packages("randomForestSRC")
install.packages("varSelRF")
library(varSelRF)
library(randomForestSRC)
library(caTools)


set.seed(144)
split = sample.split(Health_train$Lung_Cancer, SplitRatio = 0.7 )
healthTrain = subset(Health_train, split==T)
healthTest = subset(Health_train, split==F)

str(health_test_data)
health2 = health1
drop(health2$Patient_ID)
str(health2)
dim(health1)


library(party)


str(health1)
healthTest= healthTest[,-1]
healthTrain$Lung_Cancer = as.factor(healthTrain$Lung_Cancer)
random1 = randomForest(Lung_Cancer ~ ., data = healthTrain,
                       importance= TRUE ,ntree=500)

varUsed(random1)
imp = varUsed(random1) >800
imp= subset(imp, imp==T)

   

varImpPlot(random1)
random1$ntree

random1

pre = predict(random1, newdata = healthTest)
table(healthTest$Lung_Cancer, pre)
364/nrow(healthTest)

(844+11)/ nrow(healthTrain)




library(party)
ct.health = ctree(Lung_Cancer ~ ., data= Health_train)

pre3 = predict(ct.health, newdata= health_test, type="prob")
pre3[[1]][2]
table(health1$Lung_Cancer, pre3)
df = as.data.frame(pre3[1:596][2])

View(df)
write.csv(newhealth, file = "MyData2.csv")

CARThealth = rpart(Lung_Cancer ~. , data = health1, method = "class",
                   control = rpart.control(cp=0.5))
prp(CARThealth)
pre2 = predict(CARThealth, newdata= health_test ,type="prob")

pre[,2]

df = as.data.frame(pre[,2])

View(df)



#####applying PCA on health data
library(caTools)
set.seed(144)
split = sample.split(Health_train$Lung_Cancer, SplitRatio = 0.7 )
healthTrain = subset(Health_train, split==T)
healthTest = subset(Health_train, split==F)


my_data = subset(healthTrain, select = -c(Patient_ID , Lung_Cancer))
str(my_data)

prin_comp = prcomp(my_data, scale. = T)
names(prin_comp)
prin_comp$rotation[1:5, ]
dim(prin_comp$x)
biplot(prin_comp, scale = 0)

std_dev = prin_comp$sdev

pr_var= std_dev^2
pr_var[1:10]

prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
str(my_data)

train.data <- data.frame(Lung_Cancer = healthTrain$Lung_Cancer, prin_comp$x)
str(train.data)
train.data= train.data[,1:46]
library(rpart)
library(rpart.plot)
rpart.model = rpart(Lung_Cancer~. , data = train.data, method = "class")
summary(rpart.model)
prp(rpart.model)

test.data = predict(prin_comp, newdata = healthTest)
test.data = as.data.frame(test.data)
str(test.data)
test.data = test.data[, 1:45]

rpart.prediction <- predict(rpart.model, test.data, type= "prob")
table(healthTest$Lung_Cancer)
rpart.prediction
53/nrow(healthTest)

write.csv(df, file = "MyData2.csv")
(1191+72)/ nrow(health1)

health1$Lung_Cancer= as.factor(health1$Lung_Cancer)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.1,0.9,0.001))
train(Lung_Cancer ~., data = health1, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )




#####gradient boosting algorithm 
install.packages("gbm")
library(gbm)
library(caret)
search()

Health_train$Lung_Cancer = as.factor(Health_train$Lung_Cancer)
Health_test$Lung_Cancer= as.factor(Health_test$Lung_Cancer)
model = gbm(as.factor(Health_train$Lung_Cancer) ~ ., data= Health_train, distribution = "adaboost", n.trees = 500)
gbm.perf(model)

model
summary(model)
probability<-predict(model, Health_test,n.trees=500)


##cv gbm
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
set.seed(33)

Health_test$outcome2 <- ifelse(Health_test$Lung_Cancer == 1, "Yes","No")
gbmFit1 <- train(as.factor(outcome2) ~ ., data = Health_train, 
                 method = "gbm", trControl = fitControl,verbose = FALSE)
gbmFit1
summary(gbmFit1)


pp= predict(gbmFit1, Health_test,type= "raw")
pp
table(Health_train$Lung_Cancer)
library(pROC)
auc(healthTest$Lung_Cancer, eva)
View(healthTrain)



#####neural nets
library(nnet)
set.seed(342)
Health_train= Health_train[,-1]
fit<-nnet(Health_train$Lung_Cancer ~ ., Health_train, size = 4, rang = 0.1, decay = 0.0001, maxit = 100)

fit$fitted.values
predicted= predict(fit,healthTest,type="class") 
predicted
table(predicted)
df = as.data.frame(predicted[,1])
write.csv(df, file = "MyData2.csv")
View(df)

table(healthTest$Lung_Cancer)



install.packages("C50")
library(C50)
model17<-C5.0(healthTrain$Lung_Cancer~. ,data=healthTrain,trials=5, weights = NULL,
              control = C5.0Control( CF=0.25, earlyStopping = T, label = "outcome")) 
summary(model17)

eva <-predict(model17, healthTest, type="class")
table(healthTest$Lung_Cancer, eva)
df = as.data.frame(eva[,2])

write.csv(df, file = "MyData2.csv")
table(healthTest$Lung_Cancer, eva)
(349+11)/ nrow(healthTest)


### adaboost
install.packages("fastAdaboost")
library(fastAdaboost)

Health_train$Lung_Cancer= as.factor(Health_train$Lung_Cancer)
adafit = adaboost(Lung_Cancer~., data = MyData2, 10)
adafit
summary(adafit)
adapre = predict(adafit, newdata = MyData3)
table(MyData2$Lung_Cancer)
table(adapre$class)
adapre$error
df = as.data.frame(adapre$prob[,2])
write.csv(df, file = "MyData2.csv")
MyData3= MyData3[,-1]
