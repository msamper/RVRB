#this is meant to be a very rough draft of the recursive partitioning algorithm that will be used to predict millenial retention

#given a dataset that has only a set of variables that are useful for prediciton

#first we need to normalize vectors 
#one example is with age, we will have to do with each one of the vectors that is not already normalized
min_age <- min(data$Age)
max_age <- max(data$Age)
data$Age <- (data$Age - min_age) / (max_age - min_age)

#second split it up into separate sections, one to train the model and the second to compare the trained model to

n <- nrow(data)
shuffled <- data[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Fill in the model that has been learned.We will have to change the method argument to fit the data
tree <- rpart(Fired ~ ., train, method = "class")

# Predict the outcome on the test set with tree: pred, same with the type argument
pred <- predict(tree, test, type = "class")

# Calculate the confusion matrix: conf
conf <- table(test$Fired, pred)

# given the confusion matrix, we can find the accuracy
accs <- sum((diag(conf)))/sum(conf)

#If accs is lower than 95% we're in some trouble, but this is a good skeleton for now
