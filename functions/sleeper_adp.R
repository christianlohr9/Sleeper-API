# libraries:

library(tidyverse)
library(jsonlite)
library(httr)
library(tidyjson)
#library(DT)

# Hier der Path zum App-Folder:
path <- "C:/Users/matth/OneDrive/Shiny Project Upside/Upsidebowl/"

################################
# Get Draft-IDs for all leagues:
################################


# League IDs des Upsidebowl

# Pizza Pals <- 597038861834567680
# Nacho Nascher <- 597041380413464576
# Hot Dog Dudes <- 597040014550945792
# Bud Light Buddies <- 597034773214453760
# Codeword Cheetos <- 597043384380612608
# Cupcake Comrades <- 597041707346874368
# Chicken Wings Channel <- 597040711145152512 
# Chili Chicks <- 597041969868361728
# Dr. Pepper Division <- 597038205220470784 
# Burrito Barbaren <- 597037608735920128
# Burger Boys <- 597040374485143552
# Twinkie Town <- 597038475849555968

leagues <- c("597038861834567680", "597041380413464576",
             "597040014550945792", "597034773214453760",
             "597043384380612608", "597041707346874368",
             "597040711145152512", "597041969868361728",
             "597038205220470784", "597037608735920128",
             "597040374485143552", "597038475849555968")


datalist = list()

# url:
url <- "https://api.sleeper.app/v1/league/"

for (i in leagues) {
  
  # Sleeper API Befehle
  draft <- httr::GET(paste0(url,i,"/drafts")) 
  
  # Wir benoetigen den "JSON"-Text:
  jsontext <- content(draft, as = "text")
  
  draft_df <- jsontext %>%
    gather_array %>%
    spread_values(draftID = jstring("draft_id"))
  
  
  datalist[[i]] <- draft_df
  
}

draft_df <- dplyr::bind_rows(datalist)

drafts <- c(draft_df$draftID)

rm(draft, draft_df, datalist)




###################################
# Draft Picks
###################################

datalist = list()

# url:
url <- "https://api.sleeper.app/v1/draft/"

for (i in drafts) {
  
  # Sleeper API Befehle
  picks <- httr::GET(paste0(url,i,"/picks")) 
  
  # Wir benoetigen den "JSON"-Text:
  jsontext <- content(picks, as = "text")
  
  picks_df <- jsontext %>%
    gather_array %>%            
    spread_values(playerID = jstring("player_id"),
                  ownerID = jstring("picked_by"),
                  round = jnumber("round"),
                  pick = jnumber("pick_no")) %>%
    enter_object("metadata") %>%
    spread_values(firstname = jstring("first_name"),
                  lastname = jstring("last_name"),
                  team = jstring("team"),
                  position = jstring("position"))
  
  
  datalist[[i]] <- picks_df
  
}

picks_df <- dplyr::bind_rows(datalist)


# clean up:
picks_df <- as.data.frame(picks_df) %>%
  select(playerID, round, pick, firstname, lastname, team, position)

rm(datalist, picks)

################
# Calculate ADP:
################

adp_df <- as.data.frame(picks_df) %>%
  group_by(playerID) %>%
  summarise(firstname = first(firstname),
            lastname = first(lastname),
            team = first(team),
            position = first(position),
            adp = round(mean(pick), digits = 1),
            low = min(pick),
            high = max(pick),
            drafted = n()) %>%
  mutate(name = paste(firstname, lastname, sep = " "), collapse = NULL) %>%
  arrange(adp) %>%
  group_by(position) %>%
  mutate(posrank = order(order(adp))) %>%
  mutate(posrank = paste0(position, posrank, sep = "")) %>%
  select(adp, name, position, team, posrank, low, high, drafted)
adp_df <- as.data.frame(adp_df)

# Das Encoding ist hier wichtig damit man suchen kann!
write.csv(adp_df, file = paste0(path, "adp.csv"), row.names=FALSE, fileEncoding="UTF-8")