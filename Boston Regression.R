# CARET Example
library(tidyverse)
library(caret)
library(e1071)
library(mlbench)

# load data
data("BostonHousing")

df <- BostonHousing

# split data
### createDataPartition
set.seed(42)
id <- createDataPartition(y = df$medv,
                          p = 0.8,
                          list = FALSE)
train_data <- df[id,]
test_data <- df[-id,]

# train_model 
set.seed(42)
knn <- train(medv ~ .,
            data = train_data,
            method = "knn",
            metric = "Rsquared",
            tuneLength = 4
            )
# score model
p_medv <- predict(knn, newdata = test_data)

# evaluate model
# rmse = root(mean((actual - train)**2))
sq_error <- (test_data$medv - p_medv)**2
rmse <- sqrt(mean(sq_error))
#### Result K=5 RMSE 6.922480 But Result model in test data = 7.156692

# train_final model k=7
set.seed(42)
knn_final <- train(medv ~ .,
              data = train_data,
              method = "knn",
              metric = "Rsquared",
              tuneGrid = data.frame(k=7),
              trControl = trainControl(method ="none"))
# score
p_medv2 <- predict(knn_final, newdata = test_data)

# evaluate
squared_error2 <- (test_data$medv - p_medv2)**2
rmse2 <- sqrt(mean(squared_error2))