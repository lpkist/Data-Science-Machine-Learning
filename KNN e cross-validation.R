library(tidyverse)
library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)

data("mnist_27")

mnist_27$test %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

knn_fit <- knn3(y ~ ., data = mnist_27$train)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

fit_lm <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")
}
p1 <- plot_cond_prob() +
  ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
grid.arrange(p2, p1, nrow=1)

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$test$y)$overall["Accuracy"]

p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y), pch=21) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")
p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)


### atividade 1
# Previously, we used logistic regression to predict sex based on height. Now we are going to use KNN to do the same. Set the seed to 1, then use the caret package to partition the dslabs heights data into a training and test set of equal size (p = 0.5). Use the sapply() function to perform KNN with k values of seq(1, 101, 3) on the training set and calculate F1 scores on the test set with the F_meas() function using the default value of the relevant argument.
set.seed(1)
library(dslabs)
test_idx <- createDataPartition(heights$sex, times = 1, p = 0.5, list = F)
train <- heights[-test_idx,]
test <- heights[test_idx,]
ks <- seq(1, 101, 3)
f1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ ., data = train, k = k)
  pred <- predict(fit, test, type = "class")
  confusionMatrix(test$sex, pred)$byClass["F1"]
})
max(f1)
ks[which.max(f1)]


library(dslabs)
library(caret)
data("tissue_gene_expression")
# First, set the seed to 1 and split the data into training and test sets with p = 0.5. Then, report the overall accuracy you obtain from predicting tissue type using KNN with k = seq(1, 11, 2) using sapply() or map_df(). Note: use the createDataPartition() function outside of sapply() or map_df().     
set.seed(1)
test_idx <- createDataPartition(tissue_gene_expression$y, times = 1, 
                                p=.5, list = F)
xtrain <- tissue_gene_expression$x[-test_idx, ]
ytrain <- tissue_gene_expression$y[-test_idx]
xtest <- tissue_gene_expression$x[test_idx, ]
ytest <- tissue_gene_expression$y[test_idx]
ks <- seq(1, 11, 2)
accs <- sapply(ks, function(k){
  fit <- knn3(xtrain, ytrain, k = k)
  y_hat <- predict(fit, xtest, type = "class")
  mean(y_hat == ytest)
})
accs



ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) + 
  geom_line() +
  geom_point()

ks[which.max(accuracy$test)]
max(accuracy$test)


# define the population distribution of income
set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

# calculate the population median
m <- median(income)
m

# estimate the population median
N <- 100
X <- sample(income, N)
M<- median(X)
M

# use a Monte Carlo simulation to learn the distribution of M
library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") + geom_abline()
grid.arrange(p1, p2, ncol = 2)

# compare the 95% CI based on the CLT to the actual one
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
quantile(M, c(0.025, 0.975))

# bootstrap and approximate the distribution
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

# look at the confidence interval from the bootstrap
quantile(M_star, c(0.025, 0.975))


### atividade
library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)
sum(map_dbl(indexes, function(idx){sum(idx==3)}))


# Now, set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions, generating the random dataset and estimating the 75th quantile each time. What is the expected value and standard error of the 75th quantile?
set.seed(1)
q75 <- replicate(n = 10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q75)
sd(q75)


set.seed(1)
y <- rnorm(100, 0, 1)
# Set the seed to 1 again after generating y and use 10,000 bootstrap samples to estimate the expected value and standard error of the 75th quantile.
set.seed(1)
q75 <- replicate(10000,
          {x <-sample(y, 100, T)
          quantile(x,0.75)})
mean(q75)
sd(q75)
