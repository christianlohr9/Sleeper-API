library(tidyverse)
library(tidyjson)

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

get_matchups <- function(week){
  matchups <- httr::GET(paste0(api_matchups,week))
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
  matchups_df$week <- week
  return(matchups_df)
}

all_matchups <- purrr::map_dfr(1:16, get_matchups)
