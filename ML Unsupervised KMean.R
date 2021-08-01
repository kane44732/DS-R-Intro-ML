# Unsupervised Learning
library(tidyverse)
data(iris)
## Check Data
iris %>%
  count(Species)

## shuffle dataset
set.seed(42)
iris <- iris %>%
  sample_frac(size = 1)

# select feature for train
data <- iris %>%
  select(Petal.Length,Petal.Width)

# Training clustering => K Means
set.seed(42)
km_result <- kmeans(data, centers = 3)

# Plot graph
plot(x = data$Petal.Length,
     y = data$Petal.Width,
     pch = km_result$cluster,
     col = km_result$cluster)
# Plot centriods
points(km_result$centers,
       pch = 8,
       cex = 4)

# Evaluate Model 
# Note: In real life, it's hard to evalaute because we don't have real target data.
table(km_result$cluster, iris$Species)

