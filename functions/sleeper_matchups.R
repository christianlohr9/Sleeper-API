library(tidyverse)
library(tidyjson)
library(httr)

# Leagues

# id_bud <- 597034773214453760
# id_burger <- 
# id_cheetos <- 597043384380612608
# id_chicken <- 597040711145152512 
# id_chili <- 597041969868361728
# id_cupcakes <- 597041707346874368
# id_hotdog <- 597040014550945792
# id_nacho <- 597041380413464576
# id_pepper <- 597038205220470784
# id_pizza <- 597038861834567680
# id_twinkie <- 
# id_burrito <- 564943267409571840

weeks <- 1:16
leagues <- c("597038861834567680", "597041380413464576",
             "597040014550945792", "597034773214453760",
             "597043384380612608", "597041707346874368",
             "597040711145152512", "597041969868361728",
             "597038205220470784", "564943267409571840")

url <- "https://api.sleeper.app/v1/league/"

# after the clean up step we join some data. I put the needed data frames out of
# the loop, because it caused some performance issues.
# we need to get rosters and users to connect names to the matchups
# for the functions see 'sleeper_user-roster-records.R'
all_rosters <- purrr::map_dfr(leagues, get_rosters)
all_users <- purrr::map_dfr(leagues, get_users)

# here starts the matchup function

get_matchups <- function(leagues,week){
  matchups <- httr::GET(paste0(url,leagues,"/matchups/",week))
  jsontext <- content(matchups, as = "text") # we need JSON text to convert into df
  matchups_df <- jsontext %>%
    gather_array %>%            
    spread_values(roster_id = jstring("roster_id"),
                  points = jnumber("points"),
                  matchup_id = jstring("matchup_id")
    ) %>% 
    enter_object("starters") %>%   # we need starters
    gather_array
  
  # Clean-Up:
  
  matchups_df <- as.data.frame(matchups_df) %>%
    select(roster_id, points, matchup_id, ..JSON) %>% 
    rename(starters = ..JSON)
  matchups_df$starters <- unlist(matchups_df$starters)
  matchups_df$wk <- week
  matchups_df$league_id <- as.character(leagues)
  
  # we use rosters for matching the roster_id with the owner_id
  # afterwards we need users to match the names to the owner_id
  ## to do: master data table with owner_, roster_, leage_id and names
  
  matchups_df <-all_rosters %>% 
    group_by(roster_id, owner_id, league_id) %>% 
    summarise(player_id = mean(player_id)) %>%  # I summarise to delete duplicates, there has to be an easier way
    select(-player_id) %>% 
    right_join(matchups_df, by = c("league_id" = "league_id",
                                   "roster_id" = "roster_id"))
  # no we need (as mentioned above) the user df to match names and avas
  matchups_df <- all_users %>% 
    select(-teamname) %>% 
    right_join(matchups_df, by = c("league_id" = "league_id",
                                   "owner_id" = "owner_id")) %>% 
    group_by(league_id,user,avatar,matchup_id,wk) %>% 
    summarise(fpts = sum(points))
  
  return(matchups_df)
}

all_matchups <- purrr::pmap_dfr(purrr::transpose(
  purrr::cross2(leagues, weeks)), get_matchups)
