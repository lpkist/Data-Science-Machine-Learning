library(dslabs)
library(tidyverse)

data("movielens")

movielens %>% as_tibble()

movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)

tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  dplyr::select(userId, title, rating) %>% 
  pivot_wider(names_from="title", values_from="rating")
tab %>% knitr::kable()

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  dplyr::select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  pivot_wider(names_from = movieId, values_from = rating) %>% 
  (\(mat) mat[, sample(ncol(mat), 100)])()%>%
  as.matrix() %>% 
  t() %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

library(gridExtra)
p1 <- movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

p2 <- movielens %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")
grid.arrange(p1, p2, ncol = 2)

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#To make sure we don't include users and movies in the test set that do not appear in the training set, we removed these using the semi_join function, using this simple code.
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Building the Recommendation System
# A first model
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# Modeling movie effects

# fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# Modeling user effects

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


#### atividade
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
# Compute the number of ratings for each movie and then plot it against the year the movie came out using a boxplot for each year. Use the square root transformation on the y-axis (number of ratings) when creating your plot.
# 
# What year has the highest median number of ratings?
movielens %>% group_by(movieId, year) %>% 
  summarise(n = n(),
            sqrt_n = sqrt(n)) %>% 
  drop_na() %>% 
  ggplot(aes(x = factor(year), y = sqrt_n))+geom_boxplot()
movielens %>% group_by(movieId, year) %>% 
  summarise(n = n(),
            sqrt_n = sqrt(n)) %>% 
  group_by(year) %>% 
  summarise(mediana = median(sqrt_n)) %>% 
  arrange(-mediana)
 
# We see that, on average, movies that came out after 1993 get more ratings. We also see that with newer movies, starting in 1993, the number of ratings decreases with year: the more recent a movie is, the less time users have had to rate it.
# 
# Among movies that came out in 1993 or later, select the top 25 movies with the highest average number of ratings per year (n/year), and caculate the average rating of each of them. To calculate number of ratings per year, use 2018 as the end year.
top_movies <- movielens %>% filter(year >= 1993) %>% 
  mutate(n_years = 2018-year) %>% 
  group_by(title, n_years) %>% 
  summarise(n = n()) %>% 
  mutate(rpyear = n/n_years) %>% 
  arrange(-rpyear) %>% 
  head(25)

avg_ratings <- movielens %>% filter(title %in% top_movies$title) %>% 
  group_by(title) %>% 
  summarise(avg_rating = mean(rating))
# What is the average rating for the movie The Shawshank Redemption?  
avg_ratings %>% filter(title == "Shawshank Redemption, The")
top_movies %>% filter(title == "Forrest Gump")

# From the table constructed in Q2, we can see that the most frequently rated movies tend to have above average ratings. This is not surprising: more people watch popular movies. To confirm this, stratify the post-1993 movies by ratings per year and compute their average ratings. To calculate number of ratings per year, use 2018 as the end year. Make a plot of average rating versus ratings per year and show an estimate of the trend.
# 
# What type of trend do you observe?
movielens %>% filter(year >= 1993) %>% 
  mutate(n_years = 2018-year) %>% 
  group_by(title, n_years) %>% 
  summarise(n = n(),
            avg_rating = mean(rating)) %>% 
  mutate(rpyear = n/n_years) %>% 
  arrange(-rpyear) %>% 
  ggplot(aes(x = rpyear, y = avg_rating))+
  geom_point()+
  geom_smooth()+
  theme_bw()

# The movielens dataset also includes a time stamp. This variable represents the time and data in which the rating was provided. The units are seconds since January 1, 1970. Create a new column date with the date.
movielens <- movielens %>% mutate(date = as_datetime(timestamp))

# Compute the average rating for each week and plot this average against date. Hint: use the round_date() function before you group_by().
# 
# What type of trend do you observe?
movielens %>% mutate(week = round_date(date, unit = "week")) %>% 
  group_by(week) %>% 
  summarise(avg_rating = mean(rating)) %>% 
  ggplot(aes(x=week, y=avg_rating))+
  geom_point()+
  geom_smooth()

# The movielens data also has a genres column. This column includes every genre that applies to the movie. Some movies fall under several genres. Define a category as whatever combination appears in this column. Keep only categories with more than 1,000 ratings. Then compute the average and standard error for each category. Plot these as error bar plots.
# 
# Which genre has the lowest average rating?
#   Enter the name of the genre exactly as reported in the plot, including capitalization and punctuation.
movielens %>% group_by(genres) %>% 
  summarise(n = n(), avg = mean(rating), se = sd(rating)) %>% 
  filter(n>=1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se))+ 
  geom_point()+
  geom_errorbar()+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
