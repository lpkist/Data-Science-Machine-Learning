library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(knitr)
library(GGally)
library(MLmetrics)
data(brca)
brca <- data.frame(brca$x, y = factor(brca$y, 
                                      levels = c("M", "B"))) %>%
  tibble()

set.seed(3)
test_idx <- createDataPartition(brca$y, times = 1, p = 0.2, list = F)
train <- brca[-test_idx, ]
test <- brca[test_idx, ]
p <- ncol(brca)
train[,1:(p-1)] <- apply(train[,1:(p-1)], 2,
                         function(x) scale(x))
test[,1:(p-1)] <- apply(test[,1:(p-1)], 2,
                        function(x) scale(x))
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(cor(train[, 1:(p-1)]), zlim = c(-1,1))

stat_t <- function(col){
  t.test(col[train$y == "B"],
         col[train$y == "M"])$statistic
}
stats_t <- apply(train[,1:(p-1)], 2, stat_t)
my_image(cor(train[,c("radius_mean", 'perimeter_mean', 'area_mean',
                      'radius_worst', 'perimeter_worst', 'area_worst')]),
         zlim = c(-1, 1))
which.max(abs(stats_t[c("radius_mean", 'perimeter_mean', 'area_mean',
                        'radius_worst', 'perimeter_worst', 'area_worst')]))
train <- train[, !colnames(train) %in% c("radius_mean", 'perimeter_mean',
                                         'area_mean','radius_worst',
                                         'area_worst')]
my_image(cor(train[, c("compactness_mean", "concavity_mean",
                       "concave_pts_mean", "compactness_worst",
                       "concavity_worst", "concave_pts_worst")]),
         zlim = c(-1,1))
which.max(abs(stats_t[c("compactness_mean", "concavity_mean",
                       "concave_pts_mean", "compactness_worst",
                       "concavity_worst", "concave_pts_worst")]))
train <- train[, !colnames(train) %in% 
                 c("compactness_mean", "concavity_mean",
                   "concave_pts_mean", "compactness_worst",
                   "concavity_worst")]


my_image(cor(train[, c("radius_se", "perimeter_se",
                       "area_se")]),
         zlim = c(-1,1))
which.max(abs(stats_t[c("radius_se", "perimeter_se",
                       "area_se")]))
train <- train[, !colnames(train) %in% 
                 c("perimeter_se",
                       "area_se")]

my_image(cor(train[,c("texture_mean", "texture_worst")]), 
         z = c(-1,1))
which.max(abs(stats_t[c("texture_mean", "texture_worst")]))
train <- train[, !colnames(train) %in% 
                 c("texture_mean")]

(cor(train[,1:17]) %>% data.frame(c1 = colnames(train)[1:17]) %>% 
  pivot_longer(cols = 1:17, names_to = "c2", values_to = "cor") %>% 
  filter(cor!=1) %>% 
  arrange(-abs(cor)) %>% head())[c(1,3,5),] %>% kable()

p <- ncol(train)
test <- test[, colnames(test) %in% colnames(train)]

train %>% pivot_longer(cols = 1:(p-1), names_to = "col",
                       values_to = "value") %>% 
  mutate(col = factor(col),
         idx = factor((as.numeric(col)-1)%/%3)) %>% 
  ggplot(aes(x = factor(col), y = value, fill = y))+
  geom_boxplot()+
  facet_wrap(~idx, scales = "free")+
  theme_bw()




# Adjusting models
f1 <- function(data, lev = NULL, model = NULL) {
        f1_val <- MLmetrics::F1_Score(y_pred = data$pred,
                                      y_true = data$obs,
                                      positive = lev[1])
        c(F1 = f1_val)
}
train.control <- trainControl(method = "cv",
                              number = 10,
                              p = 0.9,
                              summaryFunction = f1)

## GLMs
logit <- glm(y ~ ., data = train, family = binomial(link = "logit"))
logit2 <- glm(y ~ radius_se + smoothness_se + fractal_dim_se + texture_worst + perimeter_worst + concave_pts_worst + fractal_dim_worst,
              data = train,
              family = binomial(link = "logit"))
coef(summary(logit2)) %>% kable()
anova(logit2, logit)

probit <- glm(y ~ ., data = train, family = binomial(link = "probit"))
probit2 <- glm(y ~ radius_se + smoothness_se + fractal_dim_se + texture_worst + perimeter_worst + concave_pts_worst + fractal_dim_worst,
               data = train, family = binomial(link = "probit"))
coef(summary(probit2))
anova(probit2, probit)

cloglog <- glm(y ~ ., data = train, family = binomial(link = "cloglog"))
cloglog2 <- glm(y ~ radius_se + smoothness_se + fractal_dim_se + texture_worst + perimeter_worst + concave_pts_worst + fractal_dim_worst,
                data = train, family = binomial(link = "cloglog"))
coef(summary(cloglog2))
anova(cloglog2, cloglog)


## LDA and QDA
lda <- train(y~., data = train,
             method = "lda",
             metric = "F1",
             trControl = train.control)
qda <- train(y~., data = train,
             method = "qda",
             metric = "F1",
             trControl = train.control)
kable(rbind(cbind(model = "LDA", lda$results[2:3]),
            cbind(model = "QDA", qda$results[2:3])))

## Naive bayes
naive_bayes <- train(y~., data = train,
             method = "naive_bayes",
             metric = "F1",
             trControl = train.control)
kable(cbind(model = "Naive_bayes", naive_bayes$results))

## KNN
knn <- train(y~., data = train,
             method = "knn",
             metric = "F1",
             tuneGrid = data.frame(k = seq(1, 51, by=2)),
             trControl = train.control)
ggplot(knn$results, aes(x = k, y = F1))+
  geom_point()+theme_bw()

knn <- train(y~., data = train,
             method = "knn",
             metric = "F1",
             tuneGrid = data.frame(k = 2:20),
             trControl = train.control)
ggplot(knn$results, aes(x = k, y = F1))+
  geom_point()+theme_bw()



## GamLoess
gamLoess <- train(y~., data = train,
                  method = "gamLoess",
                  metric = "F1",
                  trControl = train.control)
kable(gamLoess$results)


## Classification tree
rpart <- train(y~., data = train,
               method = "rpart",
               metric = "F1",
               tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
               trControl = train.control)
ggplot(rpart$results, aes(x=cp, y=F1))+
  geom_point()+
  theme_bw()


## Random forest
rf <- train(y~., data = train,
            method = "rf",
            metric = "F1",
            tuneGrid = data.frame(mtry = 1:9),
            trControl = train.control)
ggplot(rf$results, aes(x=mtry, y=F1))+
  geom_point()+
  theme_bw()


## Gradient Boosting
output <- capture.output(gbm <- train(y~., data = train,
             method = "gbm",
             metric = "F1",
             tuneGrid = data.frame(expand_grid(
               n.trees = c(100, 500, 1000),
               interaction.depth = 1:3,
               shrinkage = c(0.01,0.05,0.1,0.15),
               n.minobsinnode = c(10,20,30)
             )),
             trControl = train.control))
kable(gbm$results %>% arrange(-F1) %>% head())

output <- capture.output(gbm <- train(y~., data = train,
             method = "gbm",
             metric = "F1",
             tuneGrid = data.frame(expand_grid(
               n.trees = seq(1000,2500, by = 500),
               interaction.depth = 2:4,
               shrinkage = seq(0.05, 0.15, by = 0.05),
               n.minobsinnode = c(10, 20)
             )),
             trControl = train.control))
kable(gbm$results %>% arrange(-F1) %>% head())



## SVM
svmLinear <- train(y~., data = train,
                   method = "svmLinear", 
                   metric = "F1",
                   tuneGrid = expand.grid(C = seq(0.1, 2, by = 0.1)),
                   trControl = train.control)
ggplot(svmLinear$results, aes(x=C, y = F1))+
  geom_point()+
  theme_bw()

svmRadial <- train(y~., data = train,
                   method = "svmRadial", 
                   metric = "F1",
                   tuneGrid = expand.grid(
                     sigma = c(0.1, 0.2),
                     C = seq(0.1, 2, by = 0.1)),
                   trControl = train.control)
ggplot(svmRadial$results, aes(x=C, y = F1, color = factor(sigma)))+
  geom_point()+
  theme_bw()

svmPoly <- train(y~., data = train,
                   method = "svmPoly", 
                   metric = "F1",
                   tuneGrid = expand.grid(
                     degree = 1:2,
                     scale = c(0.1,0.01, 0.001),
                     C = seq(0.1, 2, by = 0.1)),
                   trControl = train.control)
kable(svmPoly$results %>% arrange(-F1) %>% head(5))


## Ensembles
models <- c("cloglog", "logit", "probit",
            "cloglog2", "logit2", "probit2",
            "gamLoess", "gbm", "knn","lda", 
            "naive_bayes", "qda", "rf", "rpart", 
            "svmLinear", "svmPoly", "svmRadial")
models_caret <- models[7:length(models)]
F1s <- sapply(models_caret, function(model) (eval(parse(text = model))$results %>% arrange(-F1))[1,"F1"])
which.max(F1s)
all <- models
all_caret <- models_caret
all_glm <- c("logit", "probit", "cloglog",
             "cloglog2", "logit2", "probit2")
top_3 <- names(F1s[order(F1s, decreasing = TRUE)[1:3]])
top_5 <- names(F1s[order(F1s, decreasing = TRUE)[1:5]])
top_9 <- names(F1s[order(F1s, decreasing = TRUE)[1:9]])

# Evaluating in test set
preds <- sapply(models, function(mod){
  if(mod %in% models_caret){
  predict(eval(parse(text = mod)), newdata = test)}
  else ifelse(predict(eval(parse(text = mod)), newdata = test, 
               type = "response")>0.5, 2,1)
})

cms <- apply(preds, 2, function(pred){
  confusionMatrix(factor(pred,
                         levels = c(1,2),
                         labels = c("M", "B")), test$y)
  })
names(cms) <- models
metrics <- sapply(cms, function(cm){
  c(cm$overall["Accuracy"], 
    cm$byClass["Sensitivity"],
    cm$byClass["Specificity"],
    cm$byClass["Balanced Accuracy"],
    cm$byClass["F1"])
})
t(metrics)
apply(t(metrics), 2, function(x)which.max(x))
t(metrics)[which.max(t(metrics)[,5]),]
cms[[which.max(t(metrics)[,5])]]


# Ensembles
ensembles <- c('all', 'all_caret', 'all_glm',
               'top_3', 'top_5', 'top_9')
preds_e <- sapply(ensembles, function(x){
  mods <- eval(parse(text = x))
  ifelse(rowMeans(preds[, mods]==1)>0.5, 1,2)
})
cms_e <- apply(preds_e, 2, function(pred){
  confusionMatrix(factor(pred,
                         levels = c(1,2),
                         labels = c("M", "B")), test$y)
  })
names(cms_e) <- ensembles
metrics_e <- sapply(cms_e, function(cm){
  c(cm$overall["Accuracy"], 
    cm$byClass["Sensitivity"],
    cm$byClass["Specificity"],
    cm$byClass["Balanced Accuracy"],
    cm$byClass["F1"])
})
t(metrics_e)
apply(t(metrics_e), 2, function(x)which.max(x))
cms_e[[4]]



