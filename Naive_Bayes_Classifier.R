#naive bayes
install.packages("naivebayes")
library(naivebayes)
attach(train.data)
model = naive_bayes(Activity ~ .-Activity_ID, train_data)
prediction = predict(model, test_data)

table(prediction, test_data$Activity)
mean(prediction==test_data$Activity) 

plot(model, legend = TRUE)
