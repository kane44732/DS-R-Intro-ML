library(tidyverse)
library(caret)
library(e1071)
library(mlbench)

churn <- read_csv('churn.csv')

## Note Classifier must change type char to factor!!!!!!
churn <- churn %>%
  mutate_if(is.character,as.factor)

# split data
set.seed(42)
id <- createDataPartition(y = churn$churn,
                    p = 0.8,
                    list = FALSE)
train_data = churn[id,]
test_data = churn[-id,]

# train data with knn model + Resampling with Kflod 5 with repeated 5 times
set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)
knn <- train(churn ~ .,
             data = train_data,
             method = "knn",
             trControl = ctrl
             )

# score model
p <- predict(knn, newdata = test_data)

# evaluate model
mean(p == test_data$churn)

# evaluate with confusion matrix
confusionMatrix(p, test_data$churn,
                positive = "Yes",
                mode = "prec_recall"
                )