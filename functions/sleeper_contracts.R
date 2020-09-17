### Get Users, Rosters and Records of many leagues in Sleeper ###
library(tidyverse)
library(tidyjson)
library(httr)
library(googledrive) 
library(googlesheets4)

# Leagues
url <- "https://api.sleeper.app/v1/league/"
leagues <- 584862858903330816

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
  
  #####
  # Sleeper API to get rosters
  #####
  
  rosters <- httr::GET(paste0(url,leagues,"/rosters"))
  jsontext <- content(rosters, as = "text")
  
  rosters_df_temp1 <- jsontext %>%
    gather_array %>%              # collapses a JSON array into index-value pairs
    spread_values(owner_id = jstring("owner_id")) %>%     # extract OwnerIDs
    enter_object("players") %>%       # uns interessieren jetzt die Spieler
    gather_array
  rosters_df_temp1 <- as.data.frame(rosters_df_temp1) %>%
    select(owner_id, ..JSON) %>%
    rename(player_id = ..JSON)
  
  rosters_df_temp2 <- jsontext %>%
    gather_array %>%              # collapses a JSON array into index-value pairs
    enter_object("taxi") %>%       # uns interessieren jetzt die Spieler
    gather_array
  rosters_df_temp2 <- as.data.frame(rosters_df_temp2) %>% 
    mutate(roster = "taxi")
  rosters_df_temp2 <- as.data.frame(rosters_df_temp2) %>%
    select(..JSON, roster) %>%
    rename(player_id = ..JSON)
  
  rosters_df_temp3 <- jsontext %>%
    gather_array %>%              # collapses a JSON array into index-value pairs
    enter_object("starters") %>%       # uns interessieren jetzt die Spieler
    gather_array
  rosters_df_temp3 <- as.data.frame(rosters_df_temp3) %>% 
    mutate(roster = "starter")
  rosters_df_temp3 <- as.data.frame(rosters_df_temp3) %>%
    select(..JSON, roster) %>%
    rename(player_id = ..JSON)
  
  rosters_df_temp4 <- jsontext %>%
    gather_array %>%              # collapses a JSON array into index-value pairs
    enter_object("reserve") %>%       # uns interessieren jetzt die Spieler
    gather_array
  rosters_df_temp4 <- as.data.frame(rosters_df_temp4) %>% 
    mutate(roster = "IR")
  rosters_df_temp4 <- as.data.frame(rosters_df_temp4) %>%
    select(..JSON, roster) %>%
    rename(player_id = ..JSON)
  
  rosters_df_temp5 <- list(rosters_df_temp1,rosters_df_temp2,
                           rosters_df_temp3,rosters_df_temp4) %>% 
    reduce(left_join, by = "player_id")
  
  rosters_df <- rosters_df_temp5 %>% 
    mutate(status= case_when(
      roster.x == "taxi" ~ "taxi",
      roster.y == "starter" ~ "starter",
      roster == "IR" ~ "IR",
      T ~ "bench"))
  rosters_df <- rosters_df %>% 
    select(-roster.x , -roster.y,-roster)
  
  rosters_df$player_id <- unlist(rosters_df$player_id)

  #####
  # Sleeper API to get users (to join usernames and avas with rosters)
  #####
  
  users <- httr::GET(paste0(url,leagues,"/users")) 
  jsontext <- content(users, as = "text") # we need JSON text to convert into df
  users_df <- jsontext %>%
    gather_array %>%              # collapses a JSON array into index-value pairs
    spread_values(owner_id = jstring("user_id"),
                  user = jstring("display_name"),
                  avatar = jstring("avatar")) %>%     
    enter_object("metadata") %>% 
    spread_values(teamname = jstring("team_name"))
  
  # clean up:
  users_df <- as.data.frame(users_df) %>%
    select(-document.id, - array.index, -..JSON)
  
  #####
  # Join users and avas to roster for shiny
  #####
  
  basic_df <- users_df %>% 
    select(-teamname) %>% 
    right_join(rosters_df, by = c("owner_id" = "owner_id"))
  
  #####
  # Get lineup and salary data from drive
  #####
  url_contracts <- "https://drive.google.com/file/d/1VKUJGCegNuHCupJn0BT8c_T41n_gsWMa/view"
  
  drive_folder_auth <-"clohr89@googlemail.com"
  drive_auth(email =drive_folder_auth)
  sheets_auth(email =drive_folder_auth)

  d1<-drive_find(pattern = "my_file.xlsx",type = drive_mime_type("xlsx")) # This is me finding the file created by the ERP, and I do shorten the search using the type
  1
  meta<-drive_get(id=d1$id)[["drive_resource"]] # Get the id from the file in googledrive
  n_id<-glue("https://drive.google.com/open?id=",d1$id[[1]]) # here I am creating a path for reading
  meta_name<- paste(getwd(),"/Files/",meta[[1]]$originalFilename,sep = "") # and a path to temporary save it.
  
  drive_download(file=as_id(n_id),overwrite = TRUE, path = meta_name) # Now read and save locally.
  V_CMV<-data.frame(read_xlsx(meta_name)) # store to data frame
  file.remove(meta_name) # delete from R Server
  rm(d1,n_id) # Delete temporary variables
  
  return(rosters_df)
}

all_rosters <- purrr::map_dfr(leagues, get_rosters)

