library(gridExtra)
library(tidyverse)
library(randomForest)
library(class)
library(caret)

filter_and_format_genres <- function(book_data) {
  # Read the CSV file with approved genres
  approved_genres <- read.csv(file = 'approved_genres.csv', stringsAsFactors = FALSE, header = FALSE)[, 1]
  
  # Apply the filtering and formatting directly to the 'Genres' column
  formatted_genres <- character(length(book_data$Genres))
  
  for (i in seq_along(book_data$Genres)) {
    Genres <- book_data$Genres[i]
    Genres <- trimws(unlist(strsplit(Genres, ',')))
    filtered_genres <- Genres[Genres %in% approved_genres]
    formatted_genres[i] <- paste(filtered_genres, collapse = ',')
  }
  
  book_data$Genres <- formatted_genres
  book_data <- book_data %>% filter(!is.na(Genres) & Genres != '')
  
  return(book_data)
}

filter_and_format_data <- function(book_data) {
  # Set My_Rating as factor and Average_Rating as numeric
  book_data$My_Rating <- as.numeric(book_data$My_Rating)
  book_data$Average_Rating <- as.numeric(book_data$Average_Rating)
  
  # Filter out incomplete data
  book_data <- book_data %>% filter(!is.na(Exclusive_Shelf) & Exclusive_Shelf != '')
  book_data <- book_data %>% filter(!is.na(Number_of_Pages) & Number_of_Pages != '')
  book_data <- book_data %>% filter(!is.na(Year_Published) & Year_Published != '')
  book_data <- book_data %>% filter(!(Exclusive_Shelf == "read" & (is.na(My_Rating) | My_Rating == 0)))
                                    
  return(book_data)
}


create_train_test_list <- function(data_train_test) {
  # Set a seed for random
  set.seed(123)
  
  # Count the number of rows in the training and test
  total_rows <- nrow(data_train_test)
  train_size <- 0.7  # Set to 80%
  test_size <- 1 - train_size
  
  # Count the number of rows for each set
  train_rows <- round(train_size * total_rows)
  test_rows <- total_rows - train_rows
  
  # Create training and test sets
  train_data <- data_train_test[1:train_rows, ]
  test_data <- data_train_test[(train_rows + 1):(train_rows + test_rows), ]

  return(list(train_data, test_data))
}

# Create function for linear model
model_lm <- function(train_data, test_data){
  # Create the model and train
  model <- lm(My_Rating ~ Year_Published + Number_of_Pages + Average_Rating, data = train_data)
  # Create the prediction and round the numbers
  predictions <- round(predict(model, newdata = test_data))
  # Get the Book_id, Book_Title and predicted rating
  to_read_prediction_results <- data.frame(
    Book_id = test_data$Book_Id,
    Book_Title = test_data$Book_Title,
    Predicted_Value = predictions
  )
  
  mae_lm <- mean(abs(predictions))
  mse_lm <- mean(predictions^2)
  rmse_lm <- sqrt(mse_lm)
  cat('Linear Regression Model - MAE:', mae_lm, 'MSE:', mse_lm, 'RMSE:', rmse_lm, '\n')
  
  return(to_read_prediction_results)
}

# Create the function for knn neighbour
model_knn <- function(train_data, test_data) {
  # Define the metric, Rsquared gives 1.72 RMSE
  metric <- "Rsquared"
  # Create the resampling control object
  trControl <- trainControl(
    method = "cv",  # Use cross-validation for evaluation
    number = 10,    # Number of folds for cross-validation
    verboseIter = FALSE
  )
  
  # Create the model and train
  fit.knn <- train(
    My_Rating ~ Year_Published + Number_of_Pages + Average_Rating,
    data = train_data,
    method = "knn",
    metric = metric,
    trControl = trControl
  )
  
  # Make predictions
  predictions <- round(predict(fit.knn, newdata = test_data))
  
  # Get the Book_id, Book_Title, and predicted rating
  to_read_prediction_results <- data.frame(
    Book_id = test_data$Book_Id,
    Book_Title = test_data$Book_Title,
    Predicted_Value = predictions
  )
  
  mae_lm <- mean(abs(predictions))
  mse_lm <- mean(predictions^2)
  rmse_lm <- sqrt(mse_lm)
  cat('KNN Model - MAE:', mae_lm, 'MSE:', mse_lm, 'RMSE:', rmse_lm, '\n')
  
  return(to_read_prediction_results)
}


# Create the random forest function
model_randomforest <- function(train_data, test_data) {
  # Create the model and train
  rf_model <- randomForest(My_Rating ~ Genres + Year_Published + Number_of_Pages + Average_Rating, data = train_data, type = "regression")
  # Create the prediction
  predictions <- predict(rf_model, newdata = test_data)
  # Round the predicted rating
  predictions <- round(predictions)
  
  # Get the Book_id, Book_Title and predicted rating
  to_read_prediction_results <- data.frame(
    Book_id = test_data$Book_Id,
    Book_Title = test_data$Book_Title,
    Predicted_My_Rating = predictions
  )
  
  mae_lm <- mean(abs(predictions))
  mse_lm <- mean(predictions^2)
  rmse_lm <- sqrt(mse_lm)
  cat('Random forrest Model - MAE:', mae_lm, 'MSE:', mse_lm, 'RMSE:', rmse_lm, '\n')
  
  return(to_read_prediction_results)
}

verify_model_data <- function(test_data_actual) {
  rmse_lm <- mean((test_data_actual$lm_predicted - test_data_actual$My_Rating)^2)
  rmse_knn <- mean((test_data_actual$knn_predicted - test_data_actual$My_Rating)^2)
  rmse_rf <- mean((test_data_actual$rf_predicted - test_data_actual$My_Rating)^2)
  return(list(rmse_lm, rmse_knn, rmse_rf))
}

# Import the goodreads_data
goodreads_data <- read.csv(file = 'goodreads_export_with_genres.csv', header = TRUE, sep = ',')

# Fix the genres so they are fewer
#Goodreads are bad at genres since its user-input
goodreads_data <- filter_and_format_genres(goodreads_data)
# Fix the rest of the data
goodreads_data <- filter_and_format_data(goodreads_data)
# Set a primary genre for easy analysis
goodreads_data$Primary_Genre <- as.factor(sapply(strsplit(goodreads_data$Genres, ','), function(x) ifelse(length(x) > 0, trimws(x[1]), NA)))

# Split the data so one is for have read and one is for going to read
goodreads_have_read <- goodreads_data %>% filter(str_trim(Exclusive_Shelf) == 'read')
goodreads_to_read <- goodreads_data %>% filter(str_trim(Exclusive_Shelf) == 'to-read')

#################################
### Analyze the data visually ###
#################################

# Create boxplot to show which primary_genres are rated highly by user
primary_genre_rating <- ggplot(goodreads_have_read, aes(x = My_Rating, y = Primary_Genre)) +
  geom_boxplot() +
  labs(x = 'My Rating', y = 'Primary Genre')
print(primary_genre_rating)

# Create scatter plot to show user-rating, avg_rating and primary_genre relations
primary_genre_avg_rating <- ggplot(goodreads_have_read, aes(x = Average_Rating, y = My_Rating , color = Primary_Genre)) +
  geom_point(size = 2) +
  scale_color_brewer(palette = 'Set1')
print(primary_genre_avg_rating)

distribution_ratings <- ggplot(goodreads_have_read, aes(x = My_Rating, fill=factor(My_Rating))) + 
  geom_histogram(binwidth = 1, color = 'black', position = 'identity', alpha = 0.7) +
  labs(title = 'Distribution of Book Ratings', x = 'Ratings', y = 'Frequency')
print(distribution_ratings)

distribution_primary_genre <- ggplot(goodreads_have_read, aes(y = Primary_Genre)) +
  geom_bar(fill = 'skyblue', color = 'black', alpha = 0.7) +
  labs(title = 'Distribution of Books by Primary Genre', x = 'Frequency', y = 'Primary Genre')
print(distribution_primary_genre)

# Create a bar chart for books that have been read
primary_genre_read <- ggplot(goodreads_have_read, aes(x = ..count.., y = Primary_Genre)) +
  geom_bar(stat = 'count') +
  labs(title = 'Distribution of Books Read by Primary Genre', x = 'Frequency', y = 'Primary Genre')

# Create a bar chart for books to read
primary_genre_to_read <- ggplot(goodreads_to_read, aes(x = ..count.., y = Primary_Genre)) +
  geom_bar(stat = 'count') +
  labs(title = 'Distribution of Books to Read by Primary Genre', x = 'Frequency', y = 'Primary Genre')

# Arrange the plots side by side
grid.arrange(primary_genre_read, primary_genre_to_read, ncol = 2)

####################################################################
###########################TRAIN AND TEST###########################
####################################################################

# Split goodreads_have_read to 2 sets, one for train and one for test
train_test_data <- create_train_test_list(goodreads_have_read)

# Get the ready train and test data
train_data <- train_test_data[[1]]
test_data <- train_test_data[[2]]

#Train and test the modelse with data that is read
predicted_lm_data <- model_lm(train_data, test_data)
predicted_knn_data <- model_knn(train_data, test_data)
predicted_rf_data <- model_randomforest(train_data, test_data)

# Join the prediction results with the goodreads_have_read dataset
predicted_lm_data_with_actual <- merge(predicted_lm_data, goodreads_have_read[c('Book_Id', 'My_Rating')], by.x = 'Book_id', by.y = 'Book_Id', all.x = TRUE)
predicted_knn_data_with_actual <- merge(predicted_knn_data, goodreads_have_read[c('Book_Id', 'My_Rating')], by.x = 'Book_id', by.y = 'Book_Id', all.x = TRUE)
predicted_rf_data_with_actual <- merge(predicted_rf_data, goodreads_have_read[c('Book_Id', 'My_Rating')], by.x = 'Book_id', by.y = 'Book_Id', all.x = TRUE)

# Here we save the tested data together with the actual user-rating so we can evalute
all_predictions <- merge(predicted_lm_data, predicted_knn_data, by.x = c('Book_id', 'Book_Title'), by.y = c('Book_id', 'Book_Title'))
all_predictions <- merge(all_predictions, predicted_rf_data, by.x = c('Book_id', 'Book_Title'), by.y = c('Book_id', 'Book_Title'))

# Get the columns for the final dataframe
final_predictions <- all_predictions[c('Book_id', 'Book_Title', 'Predicted_Value.x', 'Predicted_Value.y', 'Predicted_My_Rating')]

# Merge final_predictions with the actual My_Rating from goodreads_have_read
final_predictions_with_actual <- merge(final_predictions, goodreads_have_read[c('Book_Id', 'My_Rating')], by.x = 'Book_id', by.y = 'Book_Id')

# Rename the columns
colnames(final_predictions_with_actual)[colnames(final_predictions_with_actual) == 'Predicted_Value.x'] <- 'lm_predicted'
colnames(final_predictions_with_actual)[colnames(final_predictions_with_actual) == 'Predicted_Value.y'] <- 'knn_predicted'
colnames(final_predictions_with_actual)[colnames(final_predictions_with_actual) == "Predicted_My_Rating"] <- 'rf_predicted'

# Save final_predictions_with_actual to a CSV file
write.table(final_predictions_with_actual, file = 'final_predictions_with_actual.csv', sep = ';', row.names = FALSE, col.names = TRUE, quote = FALSE)

# Evaluate which model was the best 
rmse_all <- verify_model_data(final_predictions_with_actual)
rmse_min_index <- which.min(rmse_all)

######################################################################
###########################ACTUAL DATA TEST###########################
######################################################################

#select which model to use
if (rmse_min_index == 1) {
  print('LM was the best')
  print(rmse_all[1])
  predicted_lm_data <- model_lm(goodreads_have_read, goodreads_to_read)
  write.table(predicted_lm_data, file = 'lm_predictions_total.csv', sep = ';', row.names = FALSE, col.names = TRUE, quote = FALSE)
} else if (rmse_min_index == 2) {
   print('knn was the best')
   print(rmse_all[2])
   predicted_knn_data <- model_knn(goodreads_have_read, goodreads_to_read)
   write.table(predicted_knn_data, file = 'knn_predictions_total.csv', sep = ';', row.names = FALSE, col.names = TRUE, quote = FALSE)

} else{
  print('Randow forest was the best')
  print(rmse_all[3])
  predicted_rf_data <- model_randomforest(goodreads_have_read, goodreads_to_read)
  write.table(predicted_rf_data, file = 'rf_predictions_total.csv', sep = ';', row.names = FALSE, col.names = TRUE, quote = FALSE)
}

#print(rmse_all)
