
# Question 5: "How many movie ratings are in each of the following genres in the edx dataset?"

## Option 1:
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## Option 2:
genres <- c("Drama", "Comedy", "Thriller", "Romance")
n_movie_ratings_bygenres <- map_dbl(genres, function(a){
  find_a <- str_detect(edx$genres, a)
  sum(find_a)
})
n_movie_ratings_bygenres

# Question 6: "Which movie has the greatest number of ratings?"
## Solution 1
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## Solution 2
edx %>% group_by(movieId, title) %>%
  summarize(sum_rating =  sum(rating)) %>%
  arrange(desc(sum_rating))


# Question 7: "What are the five most given ratings in order from most to least?"
## Solution 1
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

## Solution 2
edx %>% group_by(rating) %>%
  summarize(count_rating = sum(rating)) %>%
  arrange(desc(count_rating))

# Question 8: "In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).

# Solution 1
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

