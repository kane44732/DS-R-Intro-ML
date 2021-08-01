# regularized regression
# penalized regression
library(tidyverse)
library(caret)
library(e1071)
library(mlbench)

churn <- read_csv('churn.csv')

# Tx Character to factor
churn <- churn %>%
  mutate_if(is.character, as.factor)

# split data
set.seed(42)
id <- createDataPartition(churn$churn,
                          p = 0.8,
                          list = FALSE)
train_data = churn[id,]
test_data = churn[-id,]

# train model
set.seed(42)

# alpha = 0 => Ridge Regression
# alpha = 1 => Lasso Regression
# 0 < alpha < 1 => Elastic Net

## tuning parameter => lambda/ alpha
mygrid <- expand.grid(
  # alpha = 0:1 => alpha range only 0 and 1
  alpha = seq(0, 1 , 0.05),
  lambda = seq(0.01,0.05, 0.005)
)
## resampling
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE
)
reg_model <- train(churn ~ .,
                   data = train_data,
                   method ="glmnet",
                   trControl = ctrl,
                   tuneGrid = mygrid)

# score model
p <- predict(reg_model, newdata = test_data)

# evaluate model
confusionMatrix(p, test_data$churn,
                positive = "Yes",
                mode ="prec_recall")
