### Get Users, Rosters and Records of many leagues in Sleeper ###
library(tidyverse)
library(tidyjson)
library(httr)

# Leagues
seasons <- c(2018:2020)

url1 <- "https://api.sleeper.app/v1/user/"
url2 <- "/leagues/nfl/"

## User ID
user <- "340283540933521408"

get_leagues <- function(user,seasons){
  leagues <- httr::GET(paste0(url1,user,url2,seasons)) 
  jsontext <- content(leagues, as = "text") # we need JSON text to convert into df
  leagues_df <- jsontext %>%
    gather_array %>%              # collapses a JSON array into index-value pairs
    spread_values(league_id = jstring("league_id"),
                  name = jstring("name"))
  
  # clean up:
  leagues_df <- as.data.frame(leagues_df) %>%
    select(-document.id, - array.index, -..JSON) %>% 
    mutate(season = seasons)
  
  return(leagues_df)
}

all_leagues <- purrr::pmap_dfr(purrr::transpose(
  purrr::cross2(user, seasons)), get_leagues)
