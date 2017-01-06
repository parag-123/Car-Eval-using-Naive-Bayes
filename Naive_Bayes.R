
library(e1071)

setwd("")
car_data = read.csv("car.csv", header=FALSE)
summary(car_data)
str(car_data)

# Add header info
names(car_data) = c("param1", "param2","param3", "param4", "param5", "param6", "condition")

# distribute to test & train
library(caTools)
set.seed(88) # will generate random number
split = sample.split(car_data$condition, SplitRatio = 0.75) # TRUE= Training Set, False = Test set
car_train = subset(car_data, split == TRUE)
car_test = subset(car_data, split == FALSE)
str(car_train)
str(car_test)

# Remove target variable from test data
car_test = car_test[-7]

# Train model
m <- naiveBayes(condition ~ ., data = car_train)

pred <- predict(m, car_test)
# Confusion Matrix
table(pred=pred,true=car_train$condition)
#Accuracy of prediction
mean(pred==car_train$condition)


#function to create, run and record model results
nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for (i in 1:n){
    split = sample.split(car_train$condition, SplitRatio = train_fraction)
    cat_train_train = subset(car_train, split == TRUE)
    cat_train_test = subset(car_train, split == FALSE)
    #cat_train_test = cat_train_test[-7]
    
    nb_model <- naiveBayes(condition~.,data = cat_train_train)
    nb_test_predict <- predict(nb_model,cat_train_test[-7])
    fraction_correct[i] <- mean(nb_test_predict==cat_train_test$condition)
  }
  return(fraction_correct)
}

#20 runs, 80% of data randomly selected for training set in each run
fraction_correct_predictions <- nb_multiple_runs(0.8,20)
fraction_correct_predictions
summary(fraction_correct_predictions)
sd(fraction_correct_predictions)

# Here we see that the accuracy of the model is between 0.8154 & 0.8885 with standard deviation of 0.017
# Hence concluding that our model has accouracy of around 85%

# Generate XLS file
# Create new column in test & add the predicted output there

car_test$output = pred
write.csv(car_test,'Car_Evaluation_OP.csv')

