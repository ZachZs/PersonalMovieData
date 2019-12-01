library(rvest)
library(tidyverse)
library(openxlsx)
library(stringr)

# load in dataset
# df <- read.xlsx("Media Stats Tracking.xlsx", sheet=3, startRow=1)

# loop through every row in the df
for(i in 1:nrow(df)) 
{
  # read the url
  movie_url <- df[i, 12]
  
  if (movie_url == "Not Available") {
    next
  }
  
  imdb_page <- read_html(movie_url)
  
  #--------Director----------------------------------------------------------------------#
  
  # get the director from the html code, then put it in the director column
  director <- imdb_page %>%
    html_node("div.credit_summary_item") %>%
    html_node("a") %>% 
    html_text()
 
  df[i, 6] <- director
  
  #--------Genre-------------------------------------------------------------------------#
  
  # get the genres (and release date) from the movie
  genre <- imdb_page %>%
    html_node("div.subtext") %>%
    html_nodes("a") %>% 
    html_text()
  
  # trim the release date
  genre <- genre[-length(genre)]
  str_squish(genre)
  
  # if there is more than 1 genre, combine the first 2
  if (length(genre) >= 2) {
    sort(genre)
    genre <- paste(genre[1], genre[2], sep='-')
  }
  
  # add the genre to the column
  df[i, 7] <- genre
  
  #--------Runtime-----------------------------------------------------------------------#
  
  # get the runtime from the movie, and trim down to the hours/mins
  runtime <- imdb_page %>%
    html_node("div.subtext") %>%
    html_node("time") %>%
    html_text() %>%
    str_remove_all(" ") %>% 
    str_remove_all("[min]") %>%
    str_remove_all("[\n]")

  # if the movie is over an hour (has a h in the time), then convert to mins
  if (grepl('h', runtime)) {
    # take the first character (the hours) and make it a number, then multiply it by 60 to get mins
    hour_count <- as.numeric(substring(runtime, 1, 1))
    hour_to_min <- hour_count * 60
    
    # get the character after the h (the mins) and turn them to a number, then add it to the mins from
    # hours above 
    runtime <- as.numeric(substring(runtime, 3))
    runtime <- runtime + hour_to_min
  }

  # add the runtime to the column
  df[i, 8] <- runtime
  
  #--------Rating-------------------------------------------------------------------------#

  # get the the subbar, and trim it down to a string with "|" separators
  rating <- imdb_page %>%
    html_node("div.subtext") %>%
    html_text() %>% 
    str_remove_all(" ") %>% 
    str_remove_all("[\n]")
  
    # get only the rating by splitting on the "|" and taking only the first instance (which is the rating)
    rating <- strsplit(rating, "[|]")[[1]][1]
    
    # check to see if the value is actually a rating, and if not replace with a NA (this can be confirmed if
    # the value is a runtime value with an 'h' in it)
    if (grepl('h', rating)) {
      rating <- 'No Rating'
    }
    
  # add the rating to the column  
  df[i, 9] <- rating
  
  #--------IMDB Score---------------------------------------------------------------------#
  
  # get the imdb score out of 10 (/10 included)
  imdb_score <- imdb_page %>%
    html_node("div.ratingValue") %>%
    html_text() %>% 
    str_remove_all(" ") %>% 
    str_remove_all("[\n]")
  
  # split the string on / and only take the first part (the score)
  imdb_score <- as.numeric(strsplit(imdb_score, "[/]")[[1]][1])
  
  # add the score to the column
  df[i, 10] <- imdb_score

}

write.csv(df,"MoviesUpdated2.csv", row.names = TRUE)
