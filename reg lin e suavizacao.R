# load the dataset
library(tidyverse)
library(dslabs)
library(gridExtra)
data("mnist_27")

# explore the data by plotting the two predictors
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

# smallest and largest values of x1 and x2
if(!exists("mnist")) mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
p1 <- tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_1")

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
p2 <- tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_2")
gridExtra::grid.arrange(p1, p2, ncol = 2)

# fit the model
fit <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)

# build a decision rule
library(caret)

p_hat <- predict(fit, newdata = mnist_27$test, type = "response")
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

# plot the true values
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

# visual representation of p_hat
p_hat <- predict(fit, newdata = mnist_27$true_p)
p_hat <- scales::squish(p_hat, c(0, 1))
p1 <- mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p2 <- mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 
gridExtra::grid.arrange(p1, p2, ncol = 2)


# atividade 1

library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# We will build 100 linear models using the data above and calculate the mean and standard deviation of the combined models. First, set the seed to 1 again. Then, within a replicate() loop, (1) partition the dataset into test and training sets with p = 0.5 and using dat$y to generate your indices, (2) train a linear model predicting y from x, (3) generate predictions on the test set, and (4) calculate the RMSE of that model. Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.
set.seed(1)
rmse <- replicate(n = 100,
          {
            test_idx <- createDataPartition(dat$y, times = 1,
                                            p = 0.5, list = F)
            train <- dat[-test_idx, ]
            test <- dat[test_idx, ]
            model <- lm(y ~ x, data = train)
            y_hat <- predict(model, newdata = test, type = "response")
            sqrt(mean((test$y-y_hat)^2))
          })
mean(rmse)
sd(rmse)


# Now we will repeat the exercise above but using larger datasets. Write a function that takes a size n, then (1) builds a dataset using the code provided at the top of Q1 but with n observations instead of 100 and without the set.seed(1), (2) runs the replicate() loop that you wrote to answer Q1, which builds 100 linear models and returns a vector of RMSEs, and (3) calculates the mean and standard deviation of the 100 RMSEs.

q2 <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(n = 100,
                    {
                      test_idx <- createDataPartition(dat$y, times = 1,
                                                      p = 0.5, list = F)
                      train <- dat[-test_idx, ]
                      test <- dat[test_idx, ]
                      model <- lm(y ~ x, data = train)
                      y_hat <- predict(model, newdata = test, type = "response")
                      sqrt(mean((test$y-y_hat)^2))
                    })
  return(c("mean" = mean(rmse), "sd" = sd(rmse)))
}

# Set the seed to 1 and then use sapply() or map() to apply your new function to n <- c(100, 500, 1000, 5000, 10000).
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
res <- map(n, q2)
# Note: You only need to set the seed once before running your function; do not set a seed within your function. Also be sure to use sapply() or map() as you will get different answers running the simulations individually due to setting the seed.
res


set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)
rmse <- replicate(n = 100,
                  {
                    test_idx <- createDataPartition(dat$y, times = 1,
                                                    p = 0.5, list = F)
                    train <- dat[-test_idx, ]
                    test <- dat[test_idx, ]
                    model <- lm(y ~ x, data = train)
                    y_hat <- predict(model, newdata = test, type = "response")
                    sqrt(mean((test$y-y_hat)^2))
                  })
mean(rmse)
sd(rmse)


set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)
# 
# Set the seed to 1, then use the caret package to partition into test and training sets with p = 0.5. Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a single linear model for each (not 100 like in the previous questions).
# 
# Which of the three models performs the best (has the lowest RMSE)?
set.seed(1)
test_idx <- createDataPartition(dat$y, times = 1, p = 0.5, list = F)
train <- dat[-test_idx,]
test <- dat[test_idx, ]
mx1 <- lm(y~x_1, train)
mx2 <- lm(y~x_2, train)
mx12 <- lm(y~x_1+x_2, train)
sqrt(mean((test$y - predict(mx1, newdata = test, type = "response"))^2))
sqrt(mean((test$y - predict(mx2, newdata = test, type = "response"))^2))
sqrt(mean((test$y - predict(mx12, newdata = test, type = "response"))^2))


set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

# Set the seed to 1, then use the caret package to partition into a test and training set of equal size. Compare the RMSE when using just x_1, just x_2, and both x_1 and x_2.
set.seed(1)
test_idx <- createDataPartition(dat$y, times = 1, p = .5, list = F) 
train <- dat[-test_idx, ]
test <- dat[test_idx, ]
mx1 <- lm(y~x_1, train)
mx2 <- lm(y~x_2, train)
mx12 <- lm(y~x_1+x_2, train)
sqrt(mean((test$y - predict(mx1, newdata = test, type = "response"))^2))
sqrt(mean((test$y - predict(mx2, newdata = test, type = "response"))^2))
sqrt(mean((test$y - predict(mx12, newdata = test, type = "response"))^2))

# Compare the results from Q6 and Q8. What can you conclude?



#see that the trend is wobbly
library(tidyverse)
set.seed(1)
n <- 100
x <- seq(-pi*4, pi*4, len = n)
tmp <- data.frame(x = x , f = sin(x) + x/8, e = rnorm(n, 0, 0.5)) 
p1 <- qplot(x, f, main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p2 <- qplot(x, e, main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3 <- qplot(x, f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
gridExtra::grid.arrange(p1, p2, p3)

# estimate the time trend in the 2008 US popular vote poll margin
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

# use regression to estimate
resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
  mutate(resid = resid) %>% 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)


## Smoothing

### Introduction to Smoothing

library(tidyverse)
library(gridExtra)
set.seed(1)
n <- 100
x <- seq(-pi*4, pi*4, len = n)
tmp <- data.frame(x = x , f = sin(x) + x/8, e = rnorm(n, 0, 0.5)) 
p1 <- qplot(x, f, main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p2 <- qplot(x, e, main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3 <- qplot(x, f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
grid.arrange(p1, p2, p3)

library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
  mutate(resid = resid) %>% 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)

### Bin Smoothing and Kernels

span <- 3.5
tmp <- polls_2008 %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(dist <= span) 
tmp %>% filter(center %in% c(-125, -55)) %>%
  ggplot(aes(day, margin)) +   
  geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "grey") +
  geom_point(size = 2) +    
  geom_smooth(aes(group = center), 
              method = "lm", formula=y~1, se = FALSE) +
  facet_wrap(~center)

library(gganimate)
library(transformr)
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, kernel="box", x.points = day, bandwidth = span))
bin_fit <- data.frame(x = fit$x, .fitted=fit$y)
p <- tmp %>% 
  ggplot() +
  geom_smooth(aes(day, margin, group = center), method = "lm", formula=y~1, se = FALSE) +
  transition_reveal(center) +
  geom_point(aes(day, margin), data = polls_2008, size = 3, alpha = .5, color = "grey") +
  geom_point(aes(day, margin)) +
  transition_states(center, transition_length=1, state_length=30)+
  geom_line(aes(x=x, y = .fitted, frame = x, cumulative = TRUE), data = bin_fit, color = "red") + 
  labs(title = 'x0 = {closest_state}')
anim_p <- animate(p, nframes=300)
anim_p

span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel="box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

x_0 <- -125
data.frame(x = polls_2008$day) %>% mutate(w_0 = 1*I(abs(x - x_0)<=span/2)) %>%
  mutate(w_0 = w_0/sum(w_0)) %>%
  ggplot(aes(x, w_0)) +
  geom_step()

x_0 <- -125
tmp <- with(data.frame(day = seq(min(polls_2008$day), max(polls_2008$day), .25)), 
            ksmooth(day, 1*I(day == x_0), kernel = "normal", x.points = day, bandwidth = span))
data.frame(x = tmp$x, w_0 = tmp$y) %>%
  mutate(w_0 = w_0/sum(w_0)) %>%
  ggplot(aes(x, w_0)) +
  geom_line()

tmp <- polls_2008 %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(dist <= span) %>% 
  mutate(weight =  dnorm(dist, 0, span/2.54))%>%
  mutate(weight = weight/max(weight))

span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin,  kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

### Local Weighted Regression (loess)

span <- 21/diff(range(polls_2008$day))

tmp <- polls_2008 %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(rank(dist) / n() <= span) %>%
  mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

tmp %>% 
  filter(center %in% c(-125, -55)) %>%
  ggplot(aes(day, margin)) +   
  scale_size(range = c(0, 3)) +
  geom_smooth(aes(group = center, weight = weight), 
              method = "lm", se = FALSE) +
  geom_point(data = polls_2008, size = 3, alpha = .5, color = "grey") +
  geom_point(aes(size = weight)) +
  facet_wrap(~center)

library(broom)
fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
loess_fit <- augment(fit)

p <- ggplot(tmp, aes(day, margin)) +
  scale_size(range = c(0, 3)) +
  geom_smooth(aes(group = center, weight = weight), method = "lm", se = FALSE) +
  transition_reveal(center) +
  geom_point(data = polls_2008, size = 3, alpha = .5, color = "grey") +
  geom_point(aes(size = weight)) +
  transition_states(center, transition_length=1, state_length=30) +
  geom_line(aes(x=day, y = .fitted, frame = day, cumulative = TRUE),
            data = loess_fit, color = "red") +
  labs(title = 'x0 = {closest_state}')
anim_p <- animate(p, nframes=300)
anim_p

total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

spans <- c(.66, 0.25, 0.15, 0.10)
fits <- data_frame(span = spans) %>% 
  group_by(span) %>% 
  do(broom::augment(loess(margin ~ day, degree=1, span = .$span, data=polls_2008)))
tmp <- fits %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(rank(dist) / n() <= span) %>%
  mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

tmp %>% ggplot(aes(day, margin)) +
  geom_point(size = 2, alpha = .5, color = "grey") +
  geom_line(aes(day, .fitted), data = fits, color = "red") +
  facet_wrap(~span)

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1) 

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth()

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red",  span = 0.15,
              method = "loess", method.args = list(degree=1))



### atividade 2
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")


fit <- loess(deaths ~ as.numeric(date), degree=1, span = 60/nrow(dat), data=dat)
dat %>% ggplot(aes(x = date, y = deaths))+
  geom_point()+
  geom_smooth(color="red",  span = 60/nrow(dat),
              method = "loess", method.args = list(degree=1))


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


library(tidyverse)
library(dslabs)
library(gridExtra)
data("mnist_27")
str(mnist_27$train)

# Fit a loess line with degree=1 to the data above and predict the 2s and 7s in the mnist_27$test dataset with just the second covariate. What is the accuracy of the prediction if we use only the second covariate as predictor?
fit <- loess(as.numeric(y==7)~x_2, data = mnist_27$train, degree = 1)
y_hat <- predict(fit, newdata = mnist_27$test, type = "response")
mean(ifelse(y_hat <0.5,2,7)==mnist_27$test$y)
