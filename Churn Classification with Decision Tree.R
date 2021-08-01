# Chuen prediction with Decision tree
# model ="rpart"
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
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

decision <- train(churn ~ .,
                  data = train_data,
                  method = "rpart",
                  trControl = ctrl,
                  tuneGrid = data.frame(cp = seq(0.01, 0.5, 0.01))
                  )

# score model
p <- predict(decision, newdata = test_data)

# Evaluate Model
confusionMatrix(p,
                test_data$churn,
                positive = "Yes",
                mode = "prec_recall")

# Variable Importance
varImp(decision)









