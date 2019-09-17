#LDA
install.packages("ISLR")
library(ISLR)
install.packages("MASS")
library(MASS)
install.packages("PredPsych")
library(PredPsych)
LinearDA(train_data, classCol = 562, cvType = "folds", selectedCols = 1:562)
lda.model = lda(Activity_ID ~ .-Activity, data = train_data, cv = TRUE)
lda.model
summary(lda.model)
pred = predict(lda.model, train_data)
#to check training error
mean(pred$class==train_data$Activity_ID)
#model performance on test data
lda_pred =  predict(lda.model, test_data)
names(lda_pred)
#confusion matrix
table(lda_pred$class, test_data$Activity_ID)
#test error on test data
mean(lda_pred$class==test_data$Activity_ID)

