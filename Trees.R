#Trees
library("tree")
install.packages("tree")

tree = tree(Activity_ID ~ .-Activity , train_data)
plot(tree)
text(tree, pretty = 0)
summary(tree)
tree.pred=predict(tree, test_data)
table(tree.pred, test_data$Activity_ID)
summary(tree.pred)
