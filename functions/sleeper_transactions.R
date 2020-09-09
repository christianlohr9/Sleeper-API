# Get all Transactions from many leagues in Sleeper

#######################
# Transactions
#######################

weeks <- 1:16

leagues <- c("597038861834567680", "597041380413464576",
             "597040014550945792", "597034773214453760",
             "597043384380612608", "597041707346874368",
             "597040711145152512", "597041969868361728",
             "597038205220470784")

url <- "https://api.sleeper.app/v1/league/"

get_transactions <- function(leagues,week){
  transactions <- httr::GET(paste0(url,leagues,"/transactions/",week))
  
  # Wir ben?tigen den "JSON"-Text:
  jsontext <- content(transactions, as = "text")
  
  # Im ersten Schritt geht es an die grundlegenden Daten der Transaktionen
  transactions_df <- jsontext %>% 
    gather_array %>%            
    spread_values(type = jstring("type"),
                  transaction_id = jstring("transaction_id"),
                  status = jstring("status"),
                  creator = jstring("creator")) %>%
    enter_object("roster_ids") %>% 
      gather_array("child.num") %>% 
      append_values_string("roster_id") %>% 
    select(-document.id, -array.index)
  transactions_df$..JSON <- NULL
  
  # da scheinbar nicht in zwei Objekte reingegangen werden kann muss hier das
  # Objekt "adds" nochmal gesondert ausgewertet werden. Später joinen wir auf
  # Basis der roster- und transaction-ID

  transactions_df_temp <- jsontext %>%
    gather_array %>%
    spread_values(transaction_id = jstring("transaction_id")) %>%
  enter_object("adds") %>%
      gather_object("player_id") %>%
      append_values_string("roster_id") %>%
  select(-document.id, -array.index)
  transactions_df_temp$..JSON <- NULL

  transactions_df <- transactions_df %>%
    left_join(transactions_df_temp, by = c("transaction_id" = "transaction_id",
                                       "roster_id" = "roster_id")) %>%
    rename(adds = player_id)

  # dasselbe wie mit den "adds" nun auch für die "drops" inklusive Join

  transactions_df_temp <- jsontext %>%
    gather_array %>%
    spread_values(transaction_id = jstring("transaction_id")) %>%
    enter_object("drops") %>%
    gather_object("player_id") %>%
    append_values_string("roster_id") %>%
    select(-document.id, -array.index)
  transactions_df_temp$..JSON <- NULL

  transactions_df <- transactions_df %>%
    left_join(transactions_df_temp, by = c("transaction_id" = "transaction_id",
                                           "roster_id" = "roster_id")) %>%
    rename(drops = player_id)

  # dasselbe wie mit den "adds" und "drops" nun auch für die "settings"
  # um waiver bids zu bekommen. Inklusive Join

  transactions_df_temp <- jsontext %>%
    gather_array %>%
    spread_values(transaction_id = jstring("transaction_id")) %>%
    enter_object("settings") %>%
    gather_object("waiver_bid") %>%
    append_values_string("waiver_bid") %>%
    select(-document.id, -array.index)
  transactions_df_temp$..JSON <- NULL

  transactions_df <- transactions_df %>%
    left_join(transactions_df_temp, by = c("transaction_id" = "transaction_id"))


# Clean-Up:
  
  transactions_df <- as.data.frame(transactions_df) %>% 
    mutate(wk = week,
           league_id = as.character(leagues))
  
# Get rosters to join roster_id with owner_id (for 'get_rosters' take a look
# at 'sleeper_users-rosters-record.r')
  all_rosters <- purrr::map_dfr(leagues, get_rosters)
  
  transactions_df <- all_rosters %>%
    group_by(league_id,roster_id,owner_id) %>% 
    summarise(temp = mean(player_id)) %>% # built a master data df (is there an easier way?)
    select(league_id, roster_id, owner_id) %>% 
    right_join(transactions_df, by = c("league_id" = "league_id",
                                      "roster_id" = "roster_id"))
  
  # Get users to join user_id with names and ava (for 'get_users' take a look
  # at 'sleeper_users-rosters-record.r')
  all_users <- purrr::map_dfr(leagues, get_users)
  
  transactions_df <- all_users %>% 
    select(-teamname) %>% 
    right_join(transactions_df, by = c("owner_id" = "owner_id",
                                       "league_id" = "league_id")) %>% 
    select(league_id,user,avatar,wk,transaction_id,status,child.num,adds,
           drops,waiver_bid)
  
  # join adds with player master data
  transactions_df <- players_df %>% 
    mutate(player = paste0(firstname," ",lastname," (",team,") - ",pos)) %>% 
    select(player_id,player) %>% 
      right_join(transactions_df, by = c("player_id" = "adds")) %>% 
    rename(adds=player)
  
  # join drops with player master data
  transactions_df <- players_df %>% 
    mutate(player = paste0(firstname," ",lastname," (",team,") - ",pos)) %>% 
    select(player_id,player) %>% 
    right_join(transactions_df, by = c("player_id" = "drops")) %>% 
    rename(drops=player)
   
  return(transactions_df)
}

all_transactions <- purrr::pmap_dfr(purrr::transpose(
  purrr::cross2(leagues, weeks)), get_transactions)

