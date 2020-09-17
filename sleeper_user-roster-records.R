### Get Users, Rosters and Records of many leagues in Sleeper ###
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

leagues <- c("597038861834567680", "597041380413464576",
             "597040014550945792", "597034773214453760",
             "597043384380612608", "597041707346874368",
             "597040711145152512", "597041969868361728",
             "597038205220470784", "564943267409571840")

url <- "https://api.sleeper.app/v1/league/"

############
# Users
############

get_users <- function(leagues){
  users <- httr::GET(paste0(url,leagues,"/users")) 
  jsontext <- content(users, as = "text") # we need JSON text to convert into df
  users_df <- jsontext %>%
  gather_array %>%              # collapses a JSON array into index-value pairs
  spread_values(owner_id = jstring("user_id"),
                league_id = jstring("league_id"),
                user = jstring("display_name"),
                avatar = jstring("avatar")) %>%     
  enter_object("metadata") %>% 
  spread_values(teamname = jstring("team_name"))

# clean up:
users_df <- as.data.frame(users_df) %>%
  select(-document.id, - array.index, -..JSON)

return(users_df)
}

all_users <- purrr::map_dfr(leagues, get_users)



############
# Rosters
############

get_rosters <- function(leagues){
  rosters <- httr::GET(paste0(url,leagues,"/rosters"))
  jsontext <- content(rosters, as = "text")
  
  rosters_df <- jsontext %>%
    gather_array %>%              # collapses a JSON array into index-value pairs
    spread_values(owner_id = jstring("owner_id"),
                  roster_id = jstring("roster_id"),
                  league_id = jstring("league_id")) %>%     # extract OwnerIDs
    enter_object("players") %>%       # uns interessieren jetzt die Spieler
    gather_array
  
  rosters_df <- as.data.frame(rosters_df) %>%
    select(owner_id, roster_id, league_id, ..JSON) %>%
    rename(player_id = ..JSON)
  
  rosters_df$player_id <- unlist(rosters_df$player_id)

  return(rosters_df)
}

all_rosters <- purrr::map_dfr(leagues, get_rosters)

###################################
# Records and User Settings
###################################

get_records <- function(leagues){
  records <- httr::GET(paste0(url,leagues,"/rosters"))
  jsontext <- content(records, as = "text")
  
  records_df <- jsontext %>%
    gather_array %>% 
    spread_values(owner_id = jstring("owner_id")) %>% # collapses a JSON array into index-value pairs
    enter_object("settings") %>% 
    spread_values(wins = jstring("wins"),
                  ties = jstring("ties"),
                  losses = jstring("losses"),
                  fpts = jstring(fpts),
                  waiver_position = jstring("waiver_position"),
                  waiver_budget_used = jstring("waiver_budget_used"),
                  total_moves = jstring("total_moves"),
                  division = jstring("division"))
  
  records_df <- as.data.frame(records_df) %>%
    select(-document.id, -array.index, -..JSON) %>%
    mutate(record = paste0(wins,"-",losses,"-",ties))

# arrange the best players on top
  records_df <- arrange(records_df,desc(record))

  return(records_df)
}

all_records <- purrr::map_dfr(leagues, get_records)