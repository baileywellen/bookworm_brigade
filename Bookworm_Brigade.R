library(rvest)
library(dplyr)
library(stringr)
library(rsconnect)

#function to scrape the HTML and return a string with the 
scrape_html <- function(url)
{
  
  html <- read_html(url)
  
  #extract the text indicating progress
  progress_html <- html %>%
    html_nodes("[class='progressText']") %>%
    html_text(trim = TRUE)
  
  progress_str <- progress_html[1][1]
  
  #return the individual's progress -  the progressText follows the structure "NAME has \n read XX of XX books \n in YYYY" 
  return(progress_str)
}

#a function to parse out the information we are interested in from the progress text - names, books completed, goal books, and year 
#returns a named list with the above information
extract_info_perperson <- function(progress_str)
{
  relevant_info <- list('name' = 'XXX', 'completed' = 0, 'goal' = 0, 'year' = 2024)
  
  #the progressText follows the structure "NAME has \n read XX of XX books \n in YYYY" - split it appropriately
  #first extract the name and save it 
  name <- str_split_1(progress_str, "has")[1] %>% trimws()
  progress_str <- str_split_1(progress_str, "has")[2] %>% trimws()
  relevant_info['name'] <- name
  
  #then extract the completed number of books
  #strip off the word "read" 
  progress_str <- str_split_1(progress_str, "read")[2] 
  completed <-  str_split_1(progress_str, "of")[1] %>% 
    trimws() %>% strtoi()
  
  progress_str <- str_split_1(progress_str, "of")[2] %>% trimws()
  relevant_info['completed'] <- completed
  
  #then extract the goal number of books
  goal <- str_split_1(progress_str, "books")[1] %>% trimws()  %>% strtoi()
  progress_str <- str_split_1(progress_str, "books")[2]
  relevant_info['goal'] <- goal
  
  #finally, extract the year 
  year_str <- str_split_1(progress_str, "in")[2] %>% trimws() 
  year <-gsub("[[:punct:] ]+", "", year_str) %>% strtoi()
  
  relevant_info['year'] <- year
  
  return(relevant_info)
  
}

#function to pull reading details for Emy, Cal, Bailey, Dustin, and Katherine
pull_emy_details <- function()
{
  emy_url <- "https://www.goodreads.com/user_challenges/48496449"
  emy_progress <- scrape_html(emy_url)
  emy_info <- extract_info_perperson(emy_progress)
  
  return(emy_info)
}

pull_bookworm_details <- function()
{
  #the URL should direct specifically to the persons's reading challenge
  bailey_url <- "https://www.goodreads.com/user_challenges/50469490"
  bailey_progress <- scrape_html(bailey_url)
  bailey_info <- extract_info_perperson(bailey_progress)
  
  dustin_url <- "https://www.goodreads.com/user_challenges/53825673"
  dustin_progress <- scrape_html(dustin_url)
  dustin_info <- extract_info_perperson(dustin_progress)
  
  cal_url <- "https://www.goodreads.com/user_challenges/51826043"
  cal_progress <- scrape_html(cal_url)
  cal_info <- extract_info_perperson(cal_progress)
  
  katherine_url <- "https://www.goodreads.com/user_challenges/53890337"
  katherine_progress <- scrape_html(katherine_url)
  katherine_info <- extract_info_perperson(katherine_progress)
  
  output <- list("bailey" = bailey_info, 
                 "dustin" = dustin_info,
                 "katherine" = katherine_info,
                 "cal" = cal_info,
                 sum_completed = bailey_info$completed + dustin_info$completed + katherine_info$completed + cal_info$completed,
                 sum_goal = bailey_info$goal + dustin_info$goal + katherine_info$goal + cal_info$goal)
  
  return(output)
}

create_standings_df <- function()
{
  emy_info <- pull_emy_details()
  bookworm_info <- pull_bookworm_details()
  
  
  standings_df <- data.frame(name = c("Emy", "Cal", "Katherine", "Bailey", "Dustin", "Bookworms"), 
                             books_read = c(emy_info$completed, bookworm_info$cal$completed, bookworm_info$katherine$completed,
                                            bookworm_info$bailey$completed, bookworm_info$dustin$completed, bookworm_info$sum_completed),
                             goal = c(emy_info$goal,bookworm_info$cal$goal, bookworm_info$katherine$goal,
                                      bookworm_info$bailey$goal, bookworm_info$dustin$goal, bookworm_info$sum_goal))
  
  #add percentages columns to the dataframe
  #standings_df$percent_complete <- standings_df$completed / standings_df$goal #* 100
  standings_df <- standings_df %>% 
    mutate(percent_complete = books_read / goal * 100)
  
  return(standings_df)
}

#now we need to create the R Shiny App


standings_df <- create_standings_df()

#pie chart 
standings_df %>%
  filter(name != "Bookworms") %>%
ggplot(aes(x="", y=books_read, fill=name)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle("Individuals' book totals")

