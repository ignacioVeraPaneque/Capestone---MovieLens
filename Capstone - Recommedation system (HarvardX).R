## ----setup, include = TRUE, echo = FALSE-----------------------------------------
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)


## ----data_sets, eval = TRUE------------------------------------------------------

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
#options(timeout = 900)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

ratings <- ratings |> 
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")

movies <- movies |> 
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx  <- movielens[-test_index,]
temp <- movielens[ test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp |> 
  semi_join(edx, by = 'movieId') |> 
  semi_join(edx, by = 'userId')

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)


## ----theme_setting---------------------------------------------------------------
# Setting the theme for ggplot figures
theme_set(theme_minimal() + 
            theme(legend.position = 'bottom',
                  panel.grid = element_blank(),
                  text = element_text(family = 'serif'),
                  panel.border = element_rect(color = 'grey66', 
                                               fill = NA, 
                                               size = .5))) 


## ----additional_libraries--------------------------------------------------------
if(!require(data.table)) install.packages('data.table', repos = 'http://cran.us.r-project.org')
if(!require(ggrepel)) install.packages('ggrepel', repos = 'http://cran.us.r-project.org')
if(!require(kableExtra)) install.packages('kableExtra', repos = 'http://cran.us.r-project.org')
if(!require(tictoc)) install.packages('tictoc', repos = 'http://cran.us.r-project.org')
if(!require(doParallel)) install.packages('doParallel', repos = 'http://cran.us.r-project.org')
if(!require(rsample)) install.packages('kableExtra', repos = 'http://cran.us.r-project.org')
if(!require(ggridges)) install.packages('ggridges', repos = 'http://cran.us.r-project.org')


## ----Feature_transformation------------------------------------------------------
# Transform some features in both the edx and holdout sets
# Training set
edx <- edx |> 
  mutate(
    year_released = as.integer(str_extract(title, '(?<=[(])\\d{4}(?=[)])')),
    Date = as_datetime(timestamp), 
    year_reviewed = year(Date), 
       diff_years = year_reviewed - year_released,
           detect = str_detect(genres, '\\|'),
            single_genre = ifelse(detect, str_extract(genres, '^[^|]+'), genres),
      half_rating = str_detect(rating, '\\.'),
               AM = am(Date),
         date_month = floor_date(Date, 'month'),
    y_day = yday(Date))

# Final_holdout_test set
final_holdout_test <- final_holdout_test |> 
  mutate(
    year_released = as.integer(str_extract(title, '(?<=[(])\\d{4}(?=[)])')),
             Date = as_datetime(timestamp),
    year_reviewed = year(Date), 
       diff_years = year_reviewed - year_released,
           detect = str_detect(genres, '\\|'),
            single_genre = ifelse(detect, str_extract(genres, '^[^|]+'), genres),
      half_rating = str_detect(rating, '\\.'),
               AM = am(Date),
         date_month = floor_date(Date, 'month'),
    y_day = yday(Date))


## ----Table_1---------------------------------------------------------------------
# Create a table of the variables in the dataset
classes <- map_chr(movielens, ~ class(.))
names <- names(movielens)

`Brief explanation` <- c(
  'Unique identifier assigned to each user',
  'Unique identifier assigned to each movie',
  'Valuation given by users to movies',
  'Seconds since Jan. 1, 1970 to the moment of review',
  'Title of movie, including its year of release in parentheses',
  'Categories that group films by some measure of similarity')

 tibble( Name = names, 
        Class = classes, 
        `Brief explanation`,
    `Used as` = c(rep('Feature', 2), 'Target', rep('Feature', 3)),
           id = c(3, 1, 2, 6, 4, 5)) |> 
   arrange(id) |> 
   select(1:4) |> 
   kbl(booktabs = TRUE, 
    linesep = '',
    caption = 'Variables in the movielens dataset') |> 
  kable_styling(latex_options = c("HOLD_position"))


## ----Figure_1--------------------------------------------------------------------
# Figure 1 - 
fig_1 <- edx |> 
  select(genres, single_genre) |>
  filter(single_genre != '(no genres listed)' |
         single_genre != 'IMAX' |
         single_genre != 'War') |> 
  ggplot(aes(genres, single_genre)) +
  geom_count(alpha = 1/3) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
  panel.grid.minor = element_line(color = 'grey89',
                   linewidth = .25)) 
fig_1 +
  labs(title = 'Figure 1: Mapping of 797 movie genres into 20 broad categories',
       x = '797 original categories',
       y = '20 categories') +
  theme(axis.text.x = element_blank())


## ----Figure_2, warning = FALSE---------------------------------------------------
# Figure 2 - average rating per movie as a function of number of ratings
fig_2 <- edx |> 
  select(movieId, rating, single_genre) |>
  group_by(movieId, single_genre) |> 
  reframe(total = n(), 
     avg_rating = mean(rating)) |> 
  distinct() |> 
  ggplot(aes(total/1e3, avg_rating)) +
  geom_point(size = 1/5, alpha = 1/3) + 
  geom_smooth(se = F,  linewidth = 1/3, method = 'lm') +
  facet_wrap(~single_genre)
fig_2 +
  labs(title = 'Figure 2: Average rating vs. number of ratings',
           x = 'Sample size (in thousands)', 
           y = 'Average rating')


## ----Ratings_by_users, eval = TRUE-----------------------------------------------
# Count of ratings by user
ratings_userId <- edx |> 
  count(userId)
# Summary
val <- summary(ratings_userId$n) |> 
  tibble() |> 
  pull()
  Summary <- tibble(
    Statistic = c(
    'Minimum', 
    'Q1', 
    'Median', 
    'Mean',
    'Q3',
    'Maximum'), 
    Value = round(val))

kbl(Summary, booktabs = TRUE, 
    digits = 4, 
    linesep = '', 
    caption = 'Review count per user') |> 
  kable_styling(latex_options = c("HOLD_position"))


## ----Table_daily_count-----------------------------------------------------------
# Table: Users with atypical (excessively high) daily rating counts' 
super_raters <- edx |> 
  group_by(userId, year_reviewed) |> 
  reframe(Yearly = n(),
          Daily = Yearly / 360) |> 
  arrange(desc(Daily)) |> 
  filter(Daily > 6) 

super_raters |> 
  kbl(booktabs = TRUE, 
    digits = 0, 
    linesep = '',
    caption = 'Users with atypical (excessively high) daily rating counts') |> 
  kable_styling(latex_options = c('HOLD_position'))


## ----Figure_3--------------------------------------------------------------------
# Figure 3: Daily rating count
by_userId <- edx |> 
  select(userId, year_reviewed, rating) |> 
group_by(userId, year_reviewed) |> 
reframe(Mean  = mean(rating),
            N = n() / 360) # Ignoring leap years ()
fig_3 <- by_userId |> 
  ggplot(aes(N, Mean, label = userId)) +
  geom_count(alpha = .25) +
  geom_text_repel(data = subset(by_userId, N > 6), 
                  size = 2.5, 
                  max.overlaps = Inf, 
                  min.segment.length = 0,
                  segment.size = .12) +
  scale_x_continuous(breaks = c(1, 4, 7, 10, 13), labels = c('1', '4', '7', '10', '13')) 
fig_3 +
  labs(title = 'Figure 3: Daily movie rating count',
          x = 'Number or ratings',
          y = 'Average rating') + 
  theme(legend.position = 'none')


## ----invariant_ratings, eval = T-------------------------------------------------
# for Table 4 -Invariant ratings
invariant_ratings <- edx |> 
  group_by(userId) |> 
  reframe(min = min(rating),
          max = max(rating),
          N = n(),
          test = max / min) |> 
  filter(test == 1)


## ----Table, eval = T-------------------------------------------------------------
# Table 4
  invariant_ratings |> 
  select(1, 2, 4) |> 
  arrange(desc(N)) |> 
  kbl(booktabs = TRUE,
     col.names = c('userId', 'Rating', 'Movies reviewed'),
        digits = c(0, 2, 0),
       linesep = '',
       caption = 'Users giving identical ratings regardless of movie') |> 
  kable_styling(latex_options = c('HOLD_position'))


## ----Distinct_movies, eval = T---------------------------------------------------
# In the edx data set, there are 10677 distinct movies.
distinct_movieId <- distinct(edx, movieId) |> 
  nrow()

# However, there are only 10676 distinct titles
distinct_title <-  distinct(edx, title) |> 
nrow()


## ----war_of_the_worlds, eval = T-------------------------------------------------
# War_of_the_worlds
edx |> 
  filter(title == 'War of the Worlds (2005)') |>
  group_by(movieId) |> 
  reframe(title = title,
          n = n()) |> 
  distinct() |> 
  kbl(booktabs = TRUE, 
    linesep = '',
    caption = 'War of the Worlds (2005)') |> 
  kable_styling(latex_options = c("HOLD_position"))


## ----Figure_4, eval = T----------------------------------------------------------
by_year_released <- edx |> 
group_by(year_released) |> 
reframe(Mean = mean(rating),
           N = n(), 
          SD = sd(rating))
           
fig_4 <- by_year_released |> 
     ggplot(aes(year_released, Mean, 
                size = N, 
                color = SD)) +
     geom_point(alpha = .5) +
  geom_smooth(span = 1.5, se = FALSE, linewidth = .25)
     
  fig_4 +
     labs(title = 'Figure 4: Decline in average movie ratings despite increased ratings volume \nsince the 1970s', 
          x = 'Year movie was released',
          y = 'Average rating')


## ----eval = T--------------------------------------------------------------------
# Exclude some genres with just a few movies 
# (this has no effect on the final_holdout_test, 
# since the associated movies are only in the edx)
edx <- edx |> 
  filter(single_genre != '(no genres listed)' |
         single_genre != 'IMAX'               |
         single_genre != 'War')


## --------------------------------------------------------------------------------
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings) ^ 2, na.rm = TRUE)) 
}


## ----training_set, echo = T------------------------------------------------------
# Creating a training set 
set.seed(1, sample.kind = 'Rounding')
test_index <-  createDataPartition(y = edx$rating, times = 1, p = 0.1, list = F)
training_set <-  edx[-test_index,]
temp <-  edx[test_index,]
# Make sure userId and movieId in edx_test set are also in training_set
edx_test <-  temp |> 
semi_join(training_set, by = 'movieId') |> 
semi_join(training_set, by = 'userId') |>  
as.data.table()
# Add rows removed from edx_test set back into training_set
removed_new <-  anti_join(temp, edx_test)
training_set <-  rbind(training_set, removed_new) |>  
  as.data.table() # for faster computation
rm(removed_new, temp, test_index)


## ----biases, eval = T, echo = F--------------------------------------------------
# Global average rating
mu <- mean(training_set$rating, na.rm = T)
#RMSE 
mu_rmse <- RMSE(mu, training_set$rating)

# Movie bias
b_movie <- training_set |> 
  group_by(movieId) |> 
  summarize(b_movie = mean(rating - mu))

# User bias
b_user <- training_set |> 
  left_join(b_movie, by = 'movieId') |> 
  group_by(userId) |> 
  summarize(b_user = mean(rating - mu - b_movie))

# Movie genre bias
b_genres <- training_set |> 
  left_join(b_movie, by = 'movieId') |> 
  left_join(b_user,  by = 'userId') |> 
  group_by(genres) |>
  summarize(b_genres = mean(rating - mu - b_movie - b_user))

# Year bias
b_year <- training_set |> 
  left_join(b_movie,   by = 'movieId') |>
  left_join(b_user,    by = 'userId') |>
  left_join(b_genres,  by = 'genres') |>
  group_by(year_reviewed) |> 
  summarize(b_year = mean(rating - mu - b_movie - b_user - b_genres))
# Adding year_reviewed to final_holdout_test
final_holdout_test <- final_holdout_test |> 
  mutate(year_reviewed = year(as_datetime(timestamp)))

# Predicted ratings movie
predicted_ratings_movie <- final_holdout_test |> 
  left_join(b_movie,  by = 'movieId') |> 
mutate(pred = mu + b_movie) 

# Predicted ratings user
predicted_ratings_user <- final_holdout_test |> 
  left_join(b_movie,  by = 'movieId') |> 
  left_join(b_user,   by = 'userId') |> 
mutate(pred = mu + b_movie + b_user) 

# Predicted ratings genres
predicted_ratings_genres <- final_holdout_test |> 
  left_join(b_movie,  by = 'movieId') |> 
  left_join(b_user,   by = 'userId') |> 
  left_join(b_genres, by = 'genres') |> 
mutate(pred = mu + b_movie + b_user + b_genres) 

# Predicted ratings year
predicted_ratings_year <- final_holdout_test |> 
  left_join(b_movie,  by = 'movieId') |> 
  left_join(b_user,   by = 'userId') |> 
  left_join(b_genres, by = 'genres') |> 
  left_join(b_year,   by = 'year_reviewed') |> 
mutate(pred = mu + b_movie + b_user + b_genres + b_year) 

RMSE_movie <- RMSE(final_holdout_test$rating, predicted_ratings_movie$pred)
RMSE_user <- RMSE(final_holdout_test$rating, predicted_ratings_user$pred)
RMSE_genres <- RMSE(final_holdout_test$rating, predicted_ratings_genres$pred)
RMSE_year <- RMSE(final_holdout_test$rating, predicted_ratings_year$pred)


## ----rmse------------------------------------------------------------------------

interm_table <- tribble(~Model, ~RMSE,
        'Simple, global average', mu_rmse,
        'Movie effects', RMSE_movie,
        'Movie and user effects', RMSE_user,
        'Movie, user and genre effects', RMSE_genres,
        'Movie, user, genre and year effects', RMSE_year)

interm_table |> 
  kbl(format = 'latex', booktabs = TRUE, escape = FALSE, 
    linesep = '', 
    caption = 'RMSE') |>
  kable_styling(latex_options = c('hold_position'))


## ----tunning_lambda, echo = T----------------------------------------------------
# Function for tuning lambda
set.seed(1)

RMSEs <- function(train_data, test_data, lambda) {
  mu <- mean(train_data$rating)
  
# Convert train_data to data.table for faster computations
  train_dt <- as.data.table(train_data)
  
# Movie bias 
  b_i <- train_dt[, .(b_i = sum(rating - mu) / (.N + lambda)), 
                  by = movieId]
# User bias  
  b_u <- merge(train_dt, b_i[, .(movieId, b_i)], 
               by = "movieId", all.x = TRUE)[,
               .(b_u = sum(rating - b_i - mu) / (.N + lambda)), by = userId]
# Gender bias  
  b_g <- merge(merge(train_dt, b_i, by = "movieId", all.x = TRUE),
               b_u, by = "userId", all.x = TRUE)[,
               .(b_g = sum(rating - mu - b_i - b_u) / (.N + lambda)), 
               by = genres]
  
  # Year bias (year_released)
  b_y <- merge(merge(merge(train_dt, b_i, by = "movieId", all.x = TRUE),
              b_u, by = "userId", all.x = TRUE),
              b_g, by = "genres", all.x = TRUE)[,
              .(b_y = sum(rating - mu - b_i - b_u - b_g) / (.N + lambda)), 
              by = year_released]
  
# Convert test_data to data.table for faster join
  test_dt <- as.data.table(test_data)
  
  predicted_ratings <- merge(
    merge(merge(merge(test_dt, b_i, by = 'movieId', all.x = TRUE),
             b_u, by = 'userId', all.x = TRUE),
             b_g, by = 'genres', all.x = TRUE),
             b_y, by = 'year_released', all.x = TRUE)[,
               pred := mu + b_i + b_u + b_g + b_y][!is.na(pred)]
  
return(RMSE(predicted_ratings$pred, predicted_ratings$rating))
}

n_folds <- 5 # 2 folds 72.89 secs; 5 folds: 204.327 secs; 10 folds: 435.717; 20 folds: a lot

# Create folds
  folds <- vfold_cv(training_set, v = n_folds)
# Lambdas
lambdas <-  c(4.25, seq(4.5, 4.7, by = .1), 5)
# Matrix to store RMSEs
rmse_matrix <- matrix(NA, nrow = length(lambdas), ncol = n_folds)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

for (i in seq_along(lambdas)) {
  for (j in seq_along(folds$splits)) {
    train_data <- analysis(folds$splits[[j]])
    test_data  <- assessment(folds$splits[[j]])
    rmse_matrix[i, j] <- RMSEs(train_data, test_data, lambdas[i])
  }
}
stopCluster(cl)
# Mean RMSEs for each lambda
mean_rmse <- rowMeans(rmse_matrix)

# Lambda that minimizes RMSE
optimal_lambda <- lambdas[which.min(mean_rmse)]


## ----eval = T--------------------------------------------------------------------
# Plot for lambdas
lambdas_df <- tibble(lambdas, mean_rmse)
lambdas_df |> 
ggplot(aes(lambdas, mean_rmse)) +
  geom_line() +
  geom_point(aes(lambdas, mean_rmse), 
             data = subset(lambdas_df, 
                           lambdas == optimal_lambda)) +
  labs(title = 'Figure 5: RMSE vs. lambdas',
       y = 'RMSEs')


## ----regularization--------------------------------------------------------------
# Set the regularization parameter
lambda = optimal_lambda
# Get the global average rating
mu <- mean(training_set$rating, na.rm = T)
# Movie bias
b_movie <- training_set |> 
  group_by(movieId) |> 
  summarize(b_movie = sum(rating - mu) / (n() + lambda))
# User bias
b_user <- training_set |> 
  left_join(b_movie, by = 'movieId') |> 
  group_by(userId) |> 
  summarize(b_user = sum(rating - mu - b_movie) / (n() + lambda))
# Movie genre bias
b_genres <- training_set |> 
  left_join(b_movie, by = 'movieId') |> 
  left_join(b_user,  by = 'userId') |> 
  group_by(genres) |>
  summarize(b_genres = sum(rating - mu - b_movie - b_user) / (n() + lambda))
# Year bias
b_year <- training_set |> 
  left_join(b_movie,   by = 'movieId') |>
  left_join(b_user,    by = 'userId') |>
  left_join(b_genres,  by = 'genres') |>
  group_by(year_reviewed) |> 
  summarize(b_year = 
              sum(rating - mu - b_movie - b_user - b_genres) / (n() + lambda))

# Adding year_reviewed to final_holdout_test
final_holdout_test <- final_holdout_test |> 
  mutate(year_reviewed = year(as_datetime(timestamp)))
# Predicted ratings as sums of mu and all biases
predicted_ratings <- final_holdout_test |> 
  left_join(b_movie,  by = 'movieId') |> 
  left_join(b_user,   by = 'userId') |> 
  left_join(b_genres, by = 'genres') |> 
  left_join(b_year,   by = 'year_reviewed')
predicted_ratings <- predicted_ratings |> 
mutate(pred = mu + b_movie + b_user + b_genres + b_year)

RMSE_reg <- RMSE(final_holdout_test$rating, predicted_ratings$pred)


## ----out_of_range_ratings, eval = T----------------------------------------------
# Assign min and max to less than .5 and greater than 5 predictions
constrained_predicted_ratings <- predicted_ratings |> 
  mutate(pred = case_when(pred < .5 ~ .5,
                          pred >  5 ~  5,
                      .default = pred))

RMSE_constrained <- RMSE(constrained_predicted_ratings$pred, final_holdout_test$rating)

points_df <- tibble(x = seq(.5, 5, .5),
                    y = seq(.5, 5, .5))

results <- tibble(actual = final_holdout_test$rating, predicted = 
                    constrained_predicted_ratings$pred, half = final_holdout_test$half_rating)

plot_dist <- results |> 
  ggplot(aes(x = predicted, 
             y = actual, 
         group = actual, 
          fill = half)) + 
  geom_density_ridges(stat = "binline", binwidth = 0.07, 
                      scale = 0.9, color = 'white', alpha = 1/2) +
  geom_abline(linewidth = .2) +
  labs(title = 'Figure 6: Comparing actual and predicted ratings',
       y = 'Actual ratings',
       x = 'Predicted ratings') + 
  theme(legend.position = 'none')


## --------------------------------------------------------------------------------
# Final table - all RMSEs
tribble(~Model, ~RMSE, 
        'Regularized', RMSE_reg,
        "Constrained ($0.5 \\leq \\text{rating} \\leq 5.0$)", RMSE_constrained) |> 
  rbind(interm_table) |>
  arrange(desc(RMSE)) |>
  mutate(`Growth rate` = (RMSE - lag(RMSE)) / lag(RMSE) * 100) |> 
  mutate(`Growth rate`  = round(`Growth rate`, 3)) |> 
  arrange(desc(RMSE)) |>
  kbl(format = 'latex', booktabs = TRUE, escape = FALSE, 
    linesep = '', 
    caption = 'RMSE') |>
  kable_styling(latex_options = c('hold_position'))


## --------------------------------------------------------------------------------
plot_dist


## --------------------------------------------------------------------------------
pander::pander(sessionInfo()) 

