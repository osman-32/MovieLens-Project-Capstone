# Clear environment
rm(list=ls())


#****************************************************************************
# MovieLens Capstone Project - Osman Yardimci
# HarvardX Data Science Professional Certificate PH125.9x
#****************************************************************************


# Installing required packages
packages <- c("dslabs", "ggplot2", "lubridate", "tidyverse", "caret", "data.table", "stringr")
if (!all(packages %in% installed.packages())) {
  install.packages(packages[!packages %in% installed.packages()], repos = "http://cran.us.r-project.org")
}

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Create a temporary file
dl <- tempfile()

# Download the dataset zip file and save it to the temporary file
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", destfile = dl)

#fread() is used to read the modified text into a data table called ratings
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Similarly, fread() is used to read the modified text into a data table called movies
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- data.frame(movies)
movies$movieId <- as.numeric(movies$movieId)
movies$title <- as.character(movies$title)
movies$genres <- as.character(movies$genres)

movielens <- ratings %>% 
  left_join(movies, by = "movieId")

# Validation set will be 15% of MovieLens data
set.seed(123, sample.kind = "Rounding") 
n <- nrow(movielens)
test_index <- sample(n, size = round(0.15 * n))
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]


# Check that the userId and movieId in the validation set are also in the edx set.
validation <- temp %>%
  filter(movieId %in% edx$movieId, userId %in% edx$userId)

# Return the rows removed from the validation set to the edx set.
removed <- temp[!(temp$userId %in% validation$userId & temp$movieId %in% validation$movieId), ]
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



#**********************************************
# Data Exploration
#**********************************************

# In the final hold-out test data set "validation," look for any missing values.
anyNA(validation)

# Investigate the training data set "edx" for any missing values.
anyNA(edx)

# List amount of observations and variables in final hold-out test data set "validation"
dim(validation)

# List the number of observations and variables in the "edx" training data set.
dim(edx)

# Review the "edx" data set
summary(edx)

#**********************************************
# Data Pre-Processing
#**********************************************

# Convert the timestamp to the year rated format and add it to edx.
edx <- edx %>%
  mutate(year_rated = lubridate::year(as_datetime(timestamp))) %>%
  mutate(year_released = as.numeric(str_sub(title, -5, -2))) %>%
  mutate(ages = year_rated - year_released)

#Check for any invalid year rated and year released values.
unique(edx$year_rated)
unique(edx$year_released)

# Examine the odd values part
sum(edx$ages == -1) / nrow(edx)
sum(edx$ages == -2) / nrow(edx)

# Repeat the data pre-processing for the validation set.
validation <- validation %>%
  mutate(year_rated = lubridate::year(as_datetime(timestamp))) %>%
  mutate(year_released = as.numeric(str_sub(title, -5, -2))) %>%
  mutate(ages = year_rated - year_released)


#*****************************************
# Methods/Analysis
#*****************************************

#Head - Display some of the data.
edx %>% 
  head() %>% 
  print()

# Summary of the data
summary(edx)

# The number of distinct movies and users in the edx dataset.
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Plot the Distribution of Ratings
edx %>% 
  ggplot(aes(x = rating)) +
  geom_histogram(binwidth = 0.25, color = "orange") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0.5, 5, 0.5)) +
  scale_y_continuous(breaks = seq(0, 3000000, 500000)) +
  ggtitle("Rating distribution")

# Plot the number of ratings for each movie.
edx %>% 
  count(movieId, name = "n") %>% 
  ggplot(aes(x = n)) +
  geom_histogram(bins = 30, color = "orange") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")

# Table of 20 movies that have only been rated once
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = if(is.null(count)) 0 else count) %>%
  slice(1:20) %>%
  knitr::kable()

# Plot the number of user ratings.
edx %>% 
  count(userId, name = "n") %>% 
  ggplot(aes(x = n)) +
  geom_histogram(bins = 30, color = "orange") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Number of ratings given by users")

# Plot the average user movie rating
edx %>% 
  group_by(userId) %>% 
  filter(n() >= 100) %>% 
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(x = b_u)) +
  geom_histogram(bins = 30, color = "orange") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0.5, 5, 0.5)) +
  theme_light()

#*****************************************
# Modeling Approach
#*****************************************

### Naive Model
mu <- mean(edx$rating)
naive_rmse <- RMSE(edx$rating, mu)
rmse_results <- data.frame(method = "Average movie rating model", RMSE = naive_rmse)
knitr::kable(rmse_results)

### Movie Effect Model
movie_effects <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu))

qplot(data = movie_effects, x = b_m, geom = "histogram", bins = 10, color = I("orange"), 
      ylab = "Number of movies", main = "Number of movies with the computed b_m")

predicted_ratings <- mu + validation %>% 
  left_join(movie_effects, by = "movieId") %>% 
  pull(b_m)

model_1_rmse <- RMSE(validation$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results, data.frame(method = "Movie effect model", RMSE = model_1_rmse))
knitr::kable(rmse_results)


### Movie + User Effect Model
user_effects <- edx %>% 
  left_join(movie_effects, by = "movieId") %>% 
  group_by(userId) %>% 
  filter(n() >= 100) %>% 
  summarize(b_u = mean(rating - mu - b_m))

qplot(data = user_effects, x = b_u, geom = "histogram", bins = 30, color = I("orange"))

user_effects <- edx %>% 
  left_join(movie_effects, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_m))

predicted_ratings <- validation %>% 
  left_join(movie_effects, by = "movieId") %>% 
  left_join(user_effects, by = "userId") %>% 
  mutate(prediction = mu + b_m + b_u) %>% 
  pull(prediction)

model_2_rmse <- RMSE(validation$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Movie and user effect model", RMSE = model_2_rmse))

knitr::kable(rmse_results)


### Regularized Movie + User Effect Model

# Lambda is a tuning variable.
# Choose lambda using cross-validation.
lambdas <- seq(0, 10, 0.25)

# Define a function that computes the RMSE for a given lambda.
compute_rmse <- function(lambda) {
  mu_reg <- mean(edx$rating)
  
  # The movie effect has been regularized.
  b_m_reg <- edx %>%
    group_by(movieId) %>%
    summarize(b_m_reg = sum(rating - mu_reg)/(n() + lambda))
  
  # The user effect has been regularized.
  b_u_reg <- edx %>%
    left_join(b_m_reg, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_m_reg - mu_reg)/(n() + lambda))
  
  # Determine predicted ratings
  predicted_ratings_b_m_u <-
    validation %>%
    left_join(b_m_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(prediction = mu_reg + b_m_reg + b_u_reg) %>%
    pull(prediction)
  
  return(RMSE(validation$rating, predicted_ratings_b_m_u))
}

# Calculate the RMSE for each lambda.
rmses <- sapply(lambdas, compute_rmse)

qplot(lambdas, rmses)

#The optimal lambda for the entire model is given as
lambda <- lambdas[which.min(rmses)]
lambda

# Modeling the RMSE of a regularization model: model m_u_reg_rmse
model_m_u_reg_rmse <- min(rmses)
model_m_u_reg_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User Regularization Model",
                                     RMSE = model_m_u_reg_rmse))
rmse_results %>% knitr::kable()



#*****************************************
# Results
#*****************************************

# Overview of RMSE results
knitr::kable(rmse_results, row.names = FALSE)



