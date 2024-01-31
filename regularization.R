library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(2006)
indexes <- split(1:nrow(movielens), movielens$userId)
test_ind <- sapply(indexes, function(ind) sample(ind,
                                                 ceiling(length(ind)*.2))) %>% 
  unlist(use.names = TRUE) %>% sort()
test_set <- movielens[test_ind,]
train_set <- movielens[-test_ind,]
test_set <- test_set  %>%  
  semi_join(train_set, by = "movieId")
train_set <- train_set %>% 
  semi_join(test_set, by = "movieId")
y <- select(train_set, movieId, userId, rating) %>%
  pivot_wider(names_from = movieId, values_from = rating) 
rnames <- y$userId
y <- as.matrix(y[,-1])
rownames(y) <- rnames
movie_map <- train_set %>% select(movieId, title) %>% distinct(movieId, .keep_all = TRUE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu <- mean(y, na.rm = TRUE)
naive_rmse <- RMSE(test_set$rating, mu)
predictions <- rep(3, nrow(test_set))
RMSE(test_set$rating, predictions)
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
b_i <- colMeans(y - mu, na.rm = TRUE)
fit_movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu, b_i = b_i)
left_join(test_set, fit_movies, by = "movieId") %>% 
  mutate(pred = mu + b_i) %>% 
  summarize(rmse = RMSE(rating, pred))

n <-  colSums(!is.na(y))
fit_movies$n <- n
best <- fit_movies %>% left_join(movie_map, by = "movieId") %>% 
  mutate(average_rating = mu + b_i) %>%
  filter(average_rating == 5 & n>1) 
test_set %>% 
  group_by(movieId) %>%
  summarize(test_set_averge_rating = mean(rating)) %>%
  right_join(best, by = "movieId") %>%
  select(title, average_rating, n, test_set_averge_rating) 

lambdas <- seq(0, 10, 0.1)

sums <- colSums(y - mu, na.rm = TRUE)
rmses <- sapply(lambdas, function(lambda){
  b_i <-  sums / (n + lambda)
  fit_movies$b_i <- b_i
  left_join(test_set, fit_movies, by = "movieId") |> mutate(pred = mu + b_i) |> 
    summarize(rmse = RMSE(rating, pred)) |>
    pull(rmse)
})
qplot(lambdas, rmses, geom = "line")
lambda <- lambdas[which.min(rmses)]
print(lambda)
fit_movies$b_i_reg <- colSums(y - mu, na.rm = TRUE) / (n + lambda)


#### atividade
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
schools %>% top_n(10, quality) %>% arrange(desc(quality))
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# What are the top schools based on the average score? Show just the ID, size, and the average score.
# 
# Report the ID of the top school and average score of the 10th school.
top10 <- schools %>% select(id, size, score) %>%
  arrange(-score) %>% 
  head(10)

# Compare the median school size to the median school size of the top 10 schools based on the score
median(schools$size)
median(top10$size)

# According to this analysis, it appears that small schools produce better test scores than large schools. Four out of the top 10 schools have 100 or fewer students. But how can this be? We constructed the simulation so that quality and size were independent. Repeat the exercise for the worst 10 schools.
bad10 <- schools %>% select(id, size, score) %>%
  arrange(score) %>% 
  head(10)
median(bad10$size)

# From this analysis, we see that the worst schools are also small. Plot the average score versus school size to see what's going on. Highlight the top 10 schools based on the true quality.
top10_true <- schools %>% 
  arrange(-quality) %>% 
  head(10)

schools %>% 
  ggplot(aes(x = size, y = score, 
             color = id %in% top10_true$id))+
  geom_point()+
  scale_color_manual(values = c("black", "red"))

overall <- mean(sapply(scores, mean))
alpha <- 25
effects <- sapply(scores, function(x){
  sum(x-overall)/(length(x)+alpha)
})
schools %>% mutate(effect_reg = effects+overall) %>% 
  arrange(-effect_reg) %>% 
  head(10)
schools %>% mutate(ef = (score - overall)*size/(size+alpha)+overall) %>% 
  arrange(-ef) %>% 
  head(10)

# Notice that this improves things a bit. The number of small schools that are not highly ranked is now lower. Is there a better alpha? Using values of  from 10 to 250, find the  that minimizes the RMSE.
# 
# What value of alpha gives the minimum RMSE?
alphas <- 10:250
RMSEs <- sapply(alphas, function(alpha){
  pred <- schools %>% mutate(pred = (score - overall)*size/(size+alpha)+overall)
    mean((pred$quality-pred$pred)^2)
})
arrange(data.frame(alpha = alphas, RMSE = RMSEs), RMSE) %>% head(2)

# Rank the schools based on the average obtained with the best  from Q6. Note that no small school is incorrectly included.
alpha <- 135
schools %>% mutate(pred = (score - overall)*size/(size+alpha)+overall) %>% 
  arrange(-pred) %>% 
  head(10)

# A common mistake made when using regularization is shrinking values towards 0 that are not centered around 0. For example, if we don't subtract the overall average before shrinking, we actually obtain a very similar result. Confirm this by re-running the code from the exercise in Q6 but without removing the overall mean.
# 
# What value of alpha gives the minimum RMSE here?
RMSEs <- sapply(alphas, function(alpha){
  reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  mean((schools$quality-reg)^2)
})
alphas[which.min(RMSEs)]

### Matrix Factorization
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")

train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE)) #summary stats which needs to swept away, rowMeans removed
y <- sweep(y, 2, colMeans(y, na.rm=TRUE)) #summary stats which needs to swept away, colMeans removed

#Now y is a matrix of residuals
library(gridExtra)

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
p1 <- qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
p2 <- qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
p3 <- qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

grid.arrange(p1, p2 ,p3, ncol = 3)

cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") 
#factor analysis

set.seed(1988)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X <- round(X, 1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

set.seed(1988)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

### atividade
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

s <- svd(y)
names(s)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# Compute the sum of squares of the columns of  and store them in ss_y. Then compute the sum of squares of columns of the transformed  and store them in ss_yv. Confirm that sum(ss_y) is equal to sum(ss_yv).
# 
# What is the value of sum(ss_y) (and also the value of sum(ss_yv))?
ss_y <- colSums(y^2)
ss_yv <- colSums((y%*%s$v)^2)
sum(ss_y)
sum(ss_yv)

plot(1:length(ss_y), ss_y)
plot(1:length(ss_yv), ss_yv)

data.frame(x = sqrt(ss_yv), y = s$d) %>% 
  ggplot(aes(x,y))+geom_point()

sum(ss_yv[1:3])/sum(ss_yv)
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

plot(rowMeans(y), (s$u %*% diag(s$d))[,1])
my_image(s$v)

plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[1]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[1]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))
