# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)

# function: scrape_page --------------------------------------------------------

scrape_page <- function(url){
  page <- read_html(url)
  titles <- page %>%
    html_elements(".iteminfo") %>%
    html_element("h3 a") %>%
    html_text() %>%
    str_squish()
  links <- page %>%
    html_elements(".iteminfo") %>%
    html_element("h3 a") %>%
    html_attr("href") %>%
    str_replace("\\.", "https://collections.ed.ac.uk/art/")
  artists <- page %>%
    html_elements(".iteminfo") %>%
    html_element(".artist") %>% 
    html_text()
  Dataframe1 <- tibble(
    title = titles,
    artist = artists,
    link = links
  )
  
  Dataframe1
}
