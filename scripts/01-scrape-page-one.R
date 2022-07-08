# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(robotstxt)

# can we scrape?


paths_allowed("https://www.jamiesonsofshetland.co.uk/jamiesons-of-shetland-spindrift-1-c.asp")

# set url ----------------------------------------------------------------------

first_url <- "https://www.jamiesonsofshetland.co.uk/jamiesons-of-shetland-spindrift-1-c.asp"

# read first page --------------------------------------------------------------

page <- read_html(first_url)

# scrape titles ----------------------------------------------------------------

yarn_name <- page %>%
  html_elements(".product__item__title") %>%
 # html_element("h3 a") %>%
  html_text() %>%
  str_squish()

# scrape links -----------------------------------------------------------------

image <- page %>%
  html_elements("img")

# links <- page %>%
#   html_elements("img") %>%
#   html_attr('src')

yarn_link <- page %>% 
  html_elements('a') %>%
  html_attr('href') %>%
  str_subset(pattern = "spindrift") %>%
  str_subset(pattern = "-p.asp") %>%
  unique()
  
link <- str_c("https://www.jamiesonsofshetland.co.uk/", yarn_link, sep = "")

# scrape artists ---------------------------------------------------------------

artists <- page %>%
  html_elements(".iteminfo") %>%
  html_element(".artist") %>% 
  html_text() 

# put together in a data frame -------------------------------------------------

first_ten <- tibble(
  title = titles,
  artist = artists,
  link = links
)

# scrape second ten paintings --------------------------------------------------

second_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=10"

# read second page --------------------------------------------

page <- read_html(second_url)

# scrape titles ----------------------------------------------------------------

titles <- page %>%
  html_elements(".iteminfo") %>%
  html_element("h3 a") %>%
  html_text() %>%
  str_squish()

# scrape links -----------------------------------------------------------------

links <- page %>%
  html_elements(".iteminfo") %>%
  html_element("h3 a") %>%
  html_attr("href") %>%
  str_replace("\\.", "https://collections.ed.ac.uk/art/")

# scrape artists ---------------------------------------------------------------

artists <- page %>%
  html_elements(".iteminfo") %>%
  html_element(".artist") %>% 
  html_text() 

# put together in a data frame -------------------------------------------------

second_ten <- tibble(
  title = titles,
  artist = artists,
  link = links
)