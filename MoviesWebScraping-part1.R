library(RSelenium)
library(tidyverse)
library(openxlsx)
library(stringr)

# load in dataset
df <- read.xlsx("Media Stats Tracking.xlsx", sheet=2, startRow=1)

# Access the FireFox browser, and initialize it
driver <- rsDriver(browser = c("firefox"))
ff <- driver[["client"]]
ff$open()

# connect to imdb
ff$navigate("https://www.imdb.com")

# Loop through every movie
# MAKE SURE TO CHANGE THE INDEX TO WHERE THE ADDED MOVIES START

for(i in 553:nrow(df)) 
{
  # get the title and year for the row, then paste them together
  movie <- toString(df[i, 4])
  year <- toString(df[i, 5])
  movie_and_year <- paste(movie, " (", year, ")", sep="")
  
  # check to see if its a MST3K of RiffTrax movie, and remove that part
  if (startsWith(movie_and_year, 'MST3K')) {
    movie_and_year <- str_sub(movie_and_year, 8, nchar(movie_and_year))
  }
  if (startsWith(movie_and_year, 'RiffTrax')) {
    movie_and_year <- str_sub(movie_and_year, 11, nchar(movie_and_year))
  }
  
  # pause the webpage for 1 second
  Sys.sleep(1)
  

  # find the search bar
  search_bar <- ff$findElement(using = 'name', value = 'q')
    
  # clear space, then enter the movie, and hit enter
  search_bar$clearElement()
  search_bar$sendKeysToElement(list(movie_and_year))
  Sys.sleep(1)
  search_bar$sendKeysToElement(list(key = "down_arrow"))
  Sys.sleep(1)
  search_bar$sendKeysToElement(list(key = "enter"))
    
  Sys.sleep(2)
    
  # get the current URL and save it into the df
  current_URL <- ff$getCurrentUrl()
  df[i,12] <- current_URL
    
}

# end connection
driver$server$stop()

write.csv(df,"MoviesUpdated.csv", row.names = TRUE)
