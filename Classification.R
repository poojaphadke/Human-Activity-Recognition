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


#Bagging 2nd iteration
set.seed(7)
bag.train2=randomForest(Activity~.-Activity_ID, data = train_data, mtry=20, importance=TRUE )
summary(bag.train2)
varImpPlot(bag.train2)
ypred.bag2=predict(bag.train2, test_data, type="class")
table(ypred.bag2,test_data$Activity)
mean(ypred.bag2!=test_data$Activity)
mean(ypred.bag2!=test_data$Activity)
bag.imp2=importance(bag.train2, type=1) #important variables
bag.imp.order2=order(bag.imp2, decreasing = TRUE) #important variables order (rownumbers)
















