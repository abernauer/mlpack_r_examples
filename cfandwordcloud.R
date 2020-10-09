# load required libraries for example
library(mlpack)
library(data.table)
library(wordcloud)
library(tm)
library(dplyr)
library(ggplot2)


# read movie and ratings data into memory
ratings <- fread("http://www.mlpack.org/datasets/ml-20m/ratings-only.csv.gz")
movies <- fread("http://www.mlpack.org/datasets/ml-20m/movies.csv.gz")

# check if the data sets have any missing values
any(is.na(ratings))
any(is.na(movies))

# display first 10 rows of the ratings data.
head(ratings, n = 10)


# display first 10 rows of the movies data.
head(movies, n = 10)

# clean this code up with the pipe operator, %>%, read as genres_vec %>% table() is equivalent to table(genres_vec)
# create corpus of the movie titles
# clean the corpus movie object, removing stopwords, remove numbers

movie_corpus <- Corpus(VectorSource(movies$title))

movie_corpus <- tm_map(movie_corpus, content_transformer(tolower))

movie_corpus <- tm_map(movie_corpus, removeWords, stopwords(kind = 'english'))

movie_corpus <- tm_map(movie_corpus, removePunctuation)

movie_corpus <- tm_map(movie_corpus, removeNumbers)

inspect(movie_corpus)


tdm <- TermDocumentMatrix(movie_corpus, control = list(wordlengths=c(0, Inf)))

inspect(tdm)





set.seed(385)

# set up color palette for word clouds
pal <- brewer.pal(9, "BuGn")

pal <- pal[-1:-3]

movies_word_cloud <- wordcloud(movie_corpus, scale=c(4, .5), max.words = 75, random.color = FALSE, random.order = FALSE, colors = pal)

# split our movies genres by delimiter | and construct into a character vector
genres_vec <- unlist(strsplit(movies$genres, "|", fixed = TRUE))


set.seed(386)

pal <- brewer.pal(6, "BuGn")

genre_word_cloud <- wordcloud(genres_vec, scale = c(3, .5), max.words = 25, random.order = FALSE, random.color = FALSE, colors = pal)

# create a table of the genres then call the function prop.table on the object
table_of_genres <- genres_vec %>%
table() %>% 
prop.table()

# return the frequency or percentage of each film genre.
table_of_genres

# return descriptive statistics about the movie ratings
ratings$rating   %>%
as.matrix()     %>%
preprocess_describe(., verbose = TRUE)

# make a histogram of the ratings 
ratings_hist <- ggplot(ratings, aes(rating)) +
    geom_histogram(binwidth = 0.5, color="Black", fill="dodgerblue") +
    ggtitle(label = "Histogram of movie ratings")

ratings_hist

# the mean rating is 3.5 whith 3 and 4 being the other most common moving ratings.
# hold out %10 percent of the dataset into a test set
# Set seed for reproducibility 
set.seed(387)
output <- ratings   %>%
       as.matrix(.) %>%
       preprocess_split(., test_ratio = 0.1, verbose = TRUE)

ratings_train <- output$training
ratings_test <- output$test

# Train the model 
model_output <- cf(training = ratings_train,
                  test = ratings_test,
                  rank = 10,
                  verbose = TRUE,
                  max_iterations = 2,
                  algorithm = "RegSVD")

# model 
cf_model <- model_output$output_model

ouput 