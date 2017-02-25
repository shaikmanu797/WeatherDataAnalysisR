
source("phase1_source.R")

trimmed = trimmed[, 5:11]

#Removing -1(used as placeholder for NA) values for better predictions
trimmed = trimmed[-which(trimmed$CloudCover == -1 ),]

trimmed$CloudCover = as.numeric(trimmed$CloudCover)

n.trimmed <- data.frame(sapply(trimmed[,1:6], function(x) {
  (x - min(x))/(max(x) - min(x))
}))

require(plyr)
n.trimmed = mutate(n.trimmed, Events = trimmed$Events)
head(n.trimmed)


# Split
set.seed(12345) # makes it repeatable
ind <- sample(2, nrow(trimmed), replace = TRUE, prob = c(0.67, 0.33))

## Data Split ! We don't have to split labels and variable
## saparetly here ###
n.trimmed.training <- n.trimmed[ind == 1, ]
n.trimmed.test <- n.trimmed[ind == 2, ]


# install.packages('tree')
#install.packages('tree', dependencies = TRUE)
require("tree")
my.model <- tree(Events ~ ., data = n.trimmed.training)

#
plot(my.model)
text(my.model, pretty = 0)

my.model

my.prediction = predict(my.model, n.trimmed.test, type = "class")
table(my.prediction, n.trimmed.test$Events)

#install.packages('caret', dependencies = TRUE)
require("caret")
#install.packages('e1071', dependencies=TRUE)
require("e1071")
confusionMatrix(table(my.prediction, n.trimmed.test$Events))


##
require("tree")
cv_tree = cv.tree(my.model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = "b")


pruned.model = prune.misclass(my.model, best = 4)
plot(pruned.model)
text(pruned.model, pretty = 0)

require("caret")
require("e1071")
pruned.prediction = predict(pruned.model, n.trimmed.test, type = "class")
confusionMatrix(table(pruned.prediction, n.trimmed.test$Events))
