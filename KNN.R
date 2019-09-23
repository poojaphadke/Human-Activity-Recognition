#KNN
library(class)
library(caret)

set.seed(1)

trControl <- trainControl(method = "cv",
                          number = 5,
                          verboseIter = TRUE)

out <- train(y~.,
             method = "knn",
             tuneGrid = expand.grid(k = c(1,50,80,100,120)),
             trControl = trControl,
             metric = "Accuracy",
             data = train)

set.seed(1)

trControl <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 3,
                          verboseIter = TRUE)

out2 <- train(y~.,
              method = "knn",
              tuneGrid = expand.grid(k = 1:10),
              trControl = trControl,
              metric = "Accuracy",
              data = train)
plot(out2) # knn_2.png

set.seed(1)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 5,
                          verboseIter = TRUE)

out3 <- train(y~.,
              method = "knn",
              tuneGrid = expand.grid(k = c(1,3)),
              trControl = trControl,
              metric = "Accuracy",
              data = train)

plot(out3) # knn_3.png


knn.y <- knn(train[,-ncol(train)], test[,-ncol(test)], cl = as.factor(train.y), k = 1)
cfm.knn.1 <- confusionMatrix(knn.y, as.factor(test.y))
print(cfm.knn.1)

set.seed(1)

trControl <- trainControl(method = "cv",
                          number = 5,
                          verboseIter = TRUE)

out4 <- train(y~.,
              method = "knn",
              tuneGrid = expand.grid(k = c(1,50,80,100,120)),
              trControl = trControl,
              metric = "Accuracy",
              data = train[,c(important.vars,ncol(train))])

plot(out4) # knn_4.png

set.seed(1)

trControl <- trainControl(method = "cv",
                          number = 5,
                          verboseIter = TRUE)

out5 <- train(y~.,
              method = "knn",
              tuneGrid = expand.grid(k = 1:10),
              trControl = trControl,
              metric = "Accuracy",
              data = train[,c(important.vars,ncol(train))])

plot(out5) # knn_5.png
set.seed(1)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 5,
                          verboseIter = TRUE)

out6 <- train(y~.,
              method = "knn",
              tuneGrid = expand.grid(k = c(1,3,5)),
              trControl = trControl,
              metric = "Accuracy",
              data = train[,c(important.vars,ncol(train))])

plot(out6) # knn_6.png

knn.y <- knn(train[,important.vars][,-ncol(train)], 
             test[,important.vars][,-ncol(test)], 
             cl = as.factor(train.y), k = 1)
