#Code for bagging
#Bagging 1st iteration

set.seed(7)
bag.train1=randomForest(Activity~.-Activity_ID, data = train_data, mtry=5, importance=TRUE )
summary(bag.train1)
varImpPlot(bag.train1)
ypred.bag1=predict(bag.train1, test_data, type="class")
table(ypred.bag1,test_data$Activity)
mean(ypred.bag1!=test_data$Activity)
bag.imp1=importance(bag.train1, type=1) #important variables
bag.imp.order1=order(bag.imp1, decreasing = TRUE) #important variables order (rownumbers)



















