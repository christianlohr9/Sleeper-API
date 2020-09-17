# Get Draft of many leagues in Sleeper

# libraries:

library(tidyverse)
library(jsonlite)
library(httr)
library(tidyjson)   # quasi tidyverse f?r JSON
#library(lubridate)


# League IDs des Upsidebowl

id_pizza <- "597038861834567680"
id_nacho <- "597041380413464576"
id_hotdog <- "597040014550945792"
id_bud <- "597034773214453760"
id_cheetos <- "597043384380612608"
id_cupcakes <- "597041707346874368"
id_chili <- "597041969868361728"
#upsidebowl_ids <- list(c(id_pizza,id_nacho,id_hotdog))

url <- "https://api.sleeper.app/v1/league/"

###################################
# Draft
###################################

get_draft <- function(leagues){
  draft <- httr::GET(paste0(url,leagues,"/picks"))
  jsontext <- content(draft, as = "text")
  
  draft_df <- jsontext %>%
    gather_array %>%            
    spread_values(playerID = jstring("player_id"),
                  ownerID = jstring("picked_by"),
                  round = jnumber("round"),
                  pick = jnumber("pick_no")) %>%
    enter_object("metadata") %>%
    spread_values(firstname = jstring("first_name"),
                  lastname = jstring("last_name"),
                  number = jstring("number"),
                  team = jstring("team"),
                  position = jstring("position"))
  
  draft_df <- as.data.frame(draft_df) %>%
  select(playerID, ownerID, round, pick, firstname, lastname, number,
         team, position)
  draft_df$..JSON <- NULL

  return(draft_df)
}

all_drafts <- purrr::map_dfr(leagues, get_drafts)
