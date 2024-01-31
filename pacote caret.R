library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

getModelInfo("knn")
modelLookup("knn")

ggplot(train_knn, highlight = TRUE)

data.frame(k = seq(9, 67, 2))

set.seed(2008)
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

ggplot(train_knn, highlight = TRUE)

train_knn$bestTune
train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

names(train_knn_cv$results)

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])


modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1


### atividade
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
#Confirm this by setting the seed to 1 and running cross-validation using logistic regression to fit the model. Because we have so many predictors, we selected a random sample x_subset. Use the subset when training the model.
set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

# 
# Now, instead of using a random selection of predictors, we are going to search for those that are most predictive of the outcome. We can do this by comparing the values for the  group to those in the  group, for each predictor, using a t-test. We can perform this step like this:

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}


# Create an index ind with the column numbers of the predictors that were "statistically significantly" associated with y. Use a p-value cutoff of 0.01 to define "statistically significantly."
ind <- pvals<0.01
sum(ind)

set.seed(1)
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

#Set the seed to 1 and re-run the cross-validation again, but this time using kNN. Try out the following grid k = seq(101, 301, 25) of tuning parameters. Make a plot of the resulting accuracies.
set.seed(1)
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "knn",
             tuneGrid = data.frame(k=seq(101,301,25)))
ggplot(fit, highlight = T)



# Load tidyverse
library(tidyverse)

# load package for decision tree
library(rpart)

# load the dslabs package
library(dslabs)

# fit a classification tree using the polls_2008 dataset, 
# which contains only one predictor (day)
# and the outcome (margin)
fit <- rpart(margin ~ ., data = polls_2008)

# display the decision tree
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# examine the fit from the classification tree model
polls_2008 %>%  
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# fit a classification tree on the mnist data using cross validation
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1,
                                                    len = 25)),
                     data = mnist_27$train)
# and plot it
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# view the final decision tree
plot(train_rpart$finalModel, margin = 0.1) # plot tree structure
text(train_rpart$finalModel) # add text labels

# load library for random forest
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2,
                                          minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


### atividade
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y~., dat)
plot(fit)
text(fit)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)


library(randomForest)
fit <- randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)

library(randomForest)
fit <- randomForest(y~x, data = dat,
                    nodesize = 50,
                    maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")


### atividade
# Load the rpart package and then use the caret::train() function with method = "rpart" to fit a classification tree to the tissue_gene_expression dataset. Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to report the results of the best model. Set the seed to 1991.
# 
# Which value of cp gives the highest accuracy?
library(rpart)
library(caret)
library(dslabs)
set.seed(1991)
data("tissue_gene_expression")
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y,
      method = "rpart",
      tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
plot(fit)
fit$bestTune
fit$results

set.seed(1991)
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
             control = rpart.control(minsplit = 0))
fit$bestTune
fit$results
plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

# We can see that with just seven genes, we are able to predict the tissue type. Now let's see if we can predict the tissue type with even fewer genes using a Random Forest. Use the train() function and the rf method to train a Random Forest model and save it to an object called fit. Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own). What mtry value maximizes accuracy? To permit small nodesize to grow as we did with the classification trees, use the following argument: nodesize = 1.
# 
# Note: This exercise will take some time to run. If you want to test out your code first, try using smaller values with ntree. Set the seed to 1991 again.
# 
# What value of mtry maximizes accuracy?
set.seed(1991)
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y,
             method = "rf",
             tuneGrid = data.frame(mtry = seq(50, 200, 25)),
             nodesize = 1)
plot(fit)
fit$bestTune

######## titanic
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch,
         FamilySize, Embarked)

set.seed(42)
test_idx <- createDataPartition(titanic_clean$Survived,
                                times = 1,
                                p = 0.2,
                                list = F)
test_set <- titanic_clean[test_idx, ]
train_set <- titanic_clean[-test_idx, ]
nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

set.seed(3)
guess <- sample(c(0,1), size = 179, replace = T) %>% factor()
mean(guess == test_set$Survived)

train_set %>% group_by(Sex) %>% 
  summarise(mean(Survived==1))

sex_based <- ifelse(test_set$Sex == "male", 0, 1)
mean(sex_based == test_set$Survived)

train_set %>% group_by(Pclass) %>% 
  summarise(mean(Survived==1))

class_based <- ifelse(test_set$Pclass == "1", 1, 0)
mean(class_based == test_set$Survived)

train_set %>% group_by(Sex, Pclass) %>% 
  summarise(mean(Survived==1))

sex_class_based <- ifelse(test_set$Sex == "female" & 
                            test_set$Pclass %in% c("1", "2"), 1, 0)
mean(sex_class_based == test_set$Survived)

confusionMatrix(factor(sex_based), test_set$Survived)$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")]
confusionMatrix(factor(class_based), test_set$Survived)$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")]
confusionMatrix(factor(sex_class_based), test_set$Survived)$byClass[c("Sensitivity", "Specificity", "Balanced Accuracy")]

F_meas(factor(sex_based), test_set$Survived)
F_meas(factor(class_based), test_set$Survived)
F_meas(factor(sex_class_based), test_set$Survived)

set.seed(1)
loess_fare <- train(Survived ~ Fare, data = train_set,
                    method = "gamLoess")
LF <- predict(loess_fare, test_set)
mean(LF == test_set$Survived)

set.seed(1)
logistic_age <- train(Survived ~ Age, data = train_set,
                      method = "glm")
LA <- predict(logistic_age, test_set)
mean(LA == test_set$Survived)

set.seed(1)
logistic4 <- train(Survived ~ Sex + Pclass + Fare + Age,
                   data = train_set,
                   method = "glm")
L4 <- predict(logistic4, test_set)
mean(L4 == test_set$Survived)

logistic_all <- train(Survived ~ ., train_set, method = "glm")
LAll <- predict(logistic_all, test_set)
mean(LAll == test_set$Survived)

set.seed(6)
knn <- train(Survived ~ ., train_set, method = "knn",
             tuneGrid = data.frame(k = seq(3, 51, 2)))
knn$bestTune
plot(knn)
knn_p <- predict(knn, test_set)
mean(knn_p == test_set$Survived)

set.seed(8)
knn10 <- train(Survived ~ ., train_set, method = "knn",
               tuneGrid = data.frame(k = seq(3, 51, 2)),
               trControl = trainControl(method = "cv",
                                        number = 10, p = .9))
knn10$bestTune
knn_p10 <- predict(knn10, test_set)
mean(knn_p10 == test_set$Survived)

set.seed(10)
AC <- train(Survived ~., train_set, method = "rpart",
            tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
AC$bestTune
AC_p <- predict(AC, test_set)
mean(AC_p == test_set$Survived)
plot(AC$finalModel, margin = 0.1)
text(AC$finalModel, cex = 0.75)


set.seed(14)
RF <- train(Survived ~ ., train_set, method = "rf",
            tuneGrid = data.frame(mtry = seq(1:7)),
            ntree = 100)
RF$bestTune
RF_p <- predict(RF, test_set)
mean(RF_p == test_set$Survived)
