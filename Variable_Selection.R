#Ridge Regression
x = model.matrix(Activity_ID ~ .-Activity, train_data)[,-1]
y = train_data$Activity_ID
install.packages("glmnet")
library("glmnet")
grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)

set.seed(1)
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

out=glmnet(x,y,alpha=0)
coeffs <- predict(out,type="coefficients",s=bestlam)
coeffs <- as.data.frame(as.matrix(coeffs))
coeffs <- cbind(coeffs, rownames(coeffs))
names(coeffs) <- c("RidgeCoeff", "Variable")
rownames(coeffs) <- NULL
# In descending order of importance:
sorted <- coeffs[order(abs(coeffs$RidgeCoeff), decreasing = TRUE),]
sorted


#Lasso
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)

# use CV to get best lambda
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam.lasso=cv.out$lambda.min


out=glmnet(x,y,alpha=1)
coeffs <- predict(out,type="coefficients",s=bestlam)
coeffs <- as.data.frame(as.matrix(coeffs))
coeffs <- cbind(coeffs, rownames(coeffs))
names(coeffs) <- c("LASSO.Coeff", "Variable")
rownames(coeffs) <- NULL

# Important Variables:
important <- coeffs[coeffs$LASSO.Coeff != 0,]
important



#Boosting method using all the predictors

library(gbm)
install.packages("gbm")
set.seed(7)
boost.train1=gbm(Activity~.-Activity_ID, data = train_data, distribution = "gaussian", n.trees = 5000, interaction.depth=4, verbose = F)
summary(boost.train1)
ypred.boost1=predict(boost.train1, test_data, n.trees = 5000, type="response")
table(ypred.boost1,test_data$Activity)


#Random Forest
#Random forest 1st iteration
install.packages("randomForest")
library(randomForest)
set.seed(1)
RF.train1=randomForest(Activity~.-Activity_ID, data = train_data, importance=TRUE )
summary(RF.train1)
varImpPlot(RF.train1)
ypred1=predict(RF.train1, test_data, type="class")
table(ypred1,test_data$Activity)
mean(ypred1!=test_data$Activity)
RF.imp1=importance(RF.train1, type=1)
RF.imp.order1=order(RF.imp1, decreasing = TRUE)

#Random forest 2nd iteration

set.seed(4)
RF.train4=randomForest(Activity~.-Activity_ID, data = train_data, importance=TRUE )
summary(RF.train4)
varImpPlot(RF.train4)
ypred4=predict(RF.train4, test_data, type="class")
table(ypred4,test_data$Activity)
mean(ypred4!=test_data$Activity)
RF.imp4=importance(RF.train4, type=1)
RF.imp.order4=order(RF.imp4, decreasing = TRUE)
```
#Random forest 3rd iteration
set.seed(7)
RF.train7=randomForest(Activity~.-Activity_ID, data = train_data, importance=TRUE )
summary(RF.train7)
varImpPlot(RF.train7)
ypred7=predict(RF.train7, test_data, type="class")
table(ypred7,test_data$Activity)
mean(ypred7!=test_data$Activity)
RF.imp7=importance(RF.train7, type=1)
RF.imp.order7=order(RF.imp7, decreasing = TRUE)

#Random forest 4th iteration
set.seed(11)
RF.train11=randomForest(Activity~.-Activity_ID, data = train_data, importance=TRUE )
summary(RF.train11)
varImpPlot(RF.train11)
ypred11=predict(RF.train11, test_data, type="class")
table(ypred11,test_data$Activity)
mean(ypred11!=test_data$Activity)
RF.imp11=importance(RF.train11, type=1)
RF.imp.order11=order(RF.imp11, decreasing = TRUE)

#Cross validation of random forest

CVRF.train11=rf.crossValidation(RF.train11, train_data, p=0.10,n=20)
CVRF.train11

#Random forest using 15 predictors selected from various methods
set.seed(1)
RF.train.reduced1=randomForest(V564~V53+V51+V42+V54+V560+V58+V303+V75+V504+V66+V52+V38+V43+V55+V561, data = train_data2, importance=TRUE )
summary(RF.train.reduced1)
varImpPlot(RF.train.reduced1)
ypred.reduced1=predict(RF.train.reduced1, test_data2, type="class")
table(ypred.reduced1,test_data$Activity)
mean(ypred.reduced1!=test_data$Activity)

#Random forest using 8 predictors selected from few iterations
set.seed(1)
RF.train.reduced2=randomForest(V564~V559+V54+V38+V215+V76+V504+V462+V303, data = train_data2, importance=TRUE )
summary(RF.train.reduced2)
varImpPlot(RF.train.reduced2)
ypred.reduced2=predict(RF.train.reduced2, test_data2, type="class")
table(ypred.reduced2,test_data2$V564)
mean(ypred.reduced2!=test_data2$V564)












