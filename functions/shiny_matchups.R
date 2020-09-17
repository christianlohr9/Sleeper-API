# libraries:
library(tidyverse)
library(tidyjson)
library(httr)


# Hier der Path zum App-Folder:
path <- "C:/Users/matth/OneDrive/Shiny Project Upside/Upsidebowl/"

# Leagues

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

weeks <- 1:16
leagues <- c("597038861834567680", "597041380413464576",
             "597040014550945792", "597034773214453760",
             "597043384380612608", "597041707346874368",
             "597040711145152512", "597041969868361728",
             "597038205220470784", "597037608735920128",
             "597040374485143552", "597038475849555968")

url <- "https://api.sleeper.app/v1/league/"

# after the clean up step we join some data. I put the needed data frames out of
# the loop, because it caused some performance issues.
# we need to get rosters and users to connect names to the matchups
# for the functions see 'sleeper_user-roster-records.R'

################################################################################
################################################################################

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

################################################################################
################################################################################



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
    ) #%>% 
  # enter_object("starters") %>%   #1 this code is needed if we want starters
  # gather_array
  
  # Clean-Up:
  
  matchups_df <- as.data.frame(matchups_df) %>%
    select(roster_id, points, matchup_id, ..JSON) %>% 
    #rename(starters = ..JSON) %>% #1 this code is needed if we want starters
    mutate(wk = week,
           league_id = as.character(leagues))
  #matchups_df$starters <- unlist(matchups_df$starters) #1 this code is needed if we want starters
  
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
  
  matchups_temp <- matchups_df %>% 
    group_by(league_id,matchup_id) %>% 
    summarise()
  
  return(matchups_df)
}

all_matchups <- purrr::pmap_dfr(purrr::transpose(
  purrr::cross2(leagues, weeks)), get_matchups)


# Format:

# Avatare:
all_matchups$avatar <- paste0("https://sleepercdn.com/avatars/thumbs/",all_matchups$avatar)
all_matchups$avatar <- paste0("<img src=", all_matchups$avatar, "></img>")
all_matchups$avatar[str_detect(all_matchups$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'


# Table format with User1 Avatar1 FPTS User2 Avatar2 FPTS:

matchups_df <- as.data.frame(left_join(
  all_matchups %>% group_by(league_id, matchup_id) %>% slice(seq(1, n(), by = 2)), 
  all_matchups %>% group_by(league_id, matchup_id) %>% slice(seq(2, n(), by = 2)), 
  by=c("league_id", "matchup_id", "wk"))) %>%
  rename(league = league_id,
         user1 = user.x,
         avatar1 = avatar.x,
         fpts1 = fpts.x,
         user2 = user.y,
         avatar2 = avatar.y,
         fpts2 = fpts.y) %>%
  select(league, matchup_id, wk, avatar1, user1, fpts1, fpts2, user2, avatar2)

# use league names instead of IDs for Output:
matchups_df$league[matchups_df$league=="597038861834567680"] <- "Pizza Pals"
matchups_df$league[matchups_df$league=="597041380413464576"] <- "Nacho Nascher"
matchups_df$league[matchups_df$league=="597040014550945792"] <- "Hot Dog Dudes"
matchups_df$league[matchups_df$league=="597034773214453760"] <- "Bud Light Buddies"
matchups_df$league[matchups_df$league=="597043384380612608"] <- "Codeword Cheetos"
matchups_df$league[matchups_df$league=="597041707346874368"] <- "Cupcake Comrades"
matchups_df$league[matchups_df$league=="597040711145152512"] <- "Chicken Wings Channel"
matchups_df$league[matchups_df$league=="597041969868361728"] <- "Chili Chicks"
matchups_df$league[matchups_df$league=="597038205220470784"] <- "Dr Pepper Division"
matchups_df$league[matchups_df$league=="597037608735920128"] <- "Burrito Barbaren"
matchups_df$league[matchups_df$league=="597040374485143552"] <- "Burger Boys"
matchups_df$league[matchups_df$league=="597038475849555968"] <- "Twinkie Town"


matchups_df$league <- as.factor(matchups_df$league)
matchups_df$wk <- as.factor(matchups_df$wk)


write.csv2(matchups_df, file = paste0(path, "matchups.csv"), row.names=FALSE)