#reading train and test data
train_data = read.csv(file = "C:/Users/Pooja Phadke/Desktop/HAR Github/x_train.csv")


test_data = read.csv(file = "C:/Users/Pooja Phadke/Desktop/HAR Github/X_test.csv")



install.packages("ISLR")
library(ISLR)
install.packages("MASS")
library(MASS)
#Data exploration using PCA
#find the mean and var of various columns of the table
apply(train_data, 2, mean)
apply(train_data, 2, var)
names(train_data)
new_train_data = train_data[, 1:562]
pr.out = prcomp(new_train_data, scale. = TRUE) 
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
biplot(pr.out, scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of
     Variance Explained", ylim=c(0,1),type='b')





