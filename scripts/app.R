#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#############################################
#############################################

# https://matmu.shinyapps.io/Upsidebowl/

#############################################
#############################################


# libraries:

library(tidyverse)
library(jsonlite)
library(httr)
library(tidyjson)
library(shiny)
library(shinyWidgets)
library(DT)


# League IDs des Upsidebowl

# id_pizza <- 597038861834567680
# id_nacho <- 597041380413464576
# id_hotdog <- 597040014550945792
# id_bud <- 597034773214453760
# id_cheetos <- 597043384380612608
# id_cupcakes <- 597041707346874368
# id_chicken <- 597040711145152512 
# id_chili <- 597041969868361728
# id_pepper <- 597038205220470784 



###################################
# Users
###################################  
leagues <- c("597038861834567680", "597041380413464576",
             "597040014550945792", "597034773214453760",
             "597043384380612608", "597041707346874368",
             "597040711145152512", "597041969868361728",
             "597038205220470784")

datalist = list()

# url:
url <- "https://api.sleeper.app/v1/league/"

for (i in leagues) {
    
    # Sleeper API Befehle
    users <- httr::GET(paste0(url,i,"/users")) 
    
    # Wir benoetigen den "JSON"-Text:
    jsontext <- content(users, as = "text")
    
    # Bauen wir eine Pipeline um die OwnerIDs und PlayerIDs zu ziehen:
    users_df <- jsontext %>%
        gather_array %>%              # collapses a JSON array into index-value pairs
        spread_values(ownerID = jstring("user_id"),
                      leagueID = jstring("league_id"),
                      user = jstring("display_name"),
                      avatar = jstring("avatar")) %>%     
        enter_object("metadata") %>% 
        spread_values(teamname = jstring("team_name"))
    
    # clean up:
    users_df <- as.data.frame(users_df) %>%
        select(-document.id, - array.index, -..JSON)
    
    datalist[[i]] <- users_df
    
}

users_df <- dplyr::bind_rows(datalist)
users_df$avatar <- paste0("https://sleepercdn.com/avatars/thumbs/",users_df$avatar)
users_df$avatar <- paste0("<img src=", users_df$avatar, "></img>")


# id_pizza <- 597038861834567680
pizza <- users_df %>%
    filter(leagueID == "597038861834567680") %>%
    select(avatar, user, teamname)

# id_nacho <- 597041380413464576
nacho <- users_df %>%
    filter(leagueID == "597041380413464576") %>%
    select(avatar, user, teamname)

# id_hotdog <- 597040014550945792
hotdog <- users_df %>%
    filter(leagueID == "597040014550945792") %>%
    select(avatar, user, teamname) 
# id_bud <- 597034773214453760
bud <- users_df %>%
    filter(leagueID == "597034773214453760") %>%
    select(avatar, user, teamname)

# id_cheetos <- 597043384380612608
cheeto <- users_df %>%
    filter(leagueID == "597043384380612608") %>%
    select(avatar, user, teamname)

# id_cupcakes <- 597041707346874368
cupcake <- users_df %>%
    filter(leagueID == "597041707346874368") %>%
    select(avatar, user, teamname)

# id_chicken <- 597040711145152512 
chickenwing <- users_df %>%
    filter(leagueID == "597040711145152512") %>%
    select(avatar, user, teamname) 

# id_chili <- 597041969868361728
chili <- users_df %>%
    filter(leagueID == "597041969868361728") %>%
    select(avatar, user, teamname) 

# id_pepper <- 597038205220470784 
drpepper <- users_df %>%
    filter(leagueID == "597038205220470784") %>%
    select(avatar, user, teamname) 











# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Upsidebowl"),

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
        #sidebarPanel(
        #),

        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("Pizza"),
            DT::dataTableOutput("Nacho"),
            DT::dataTableOutput("HotDog"),
            DT::dataTableOutput("Bud"),
            DT::dataTableOutput("Cheetos"),
            DT::dataTableOutput("Cupcakes"),
            DT::dataTableOutput("Chickenwings"),
            DT::dataTableOutput("Chili"),
            DT::dataTableOutput("DrPepper")
        )
    #)
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Pizza <- DT::renderDataTable({
        DT::datatable(pizza,
                      caption = "Pizza",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
    
    output$Nacho <- DT::renderDataTable({
        DT::datatable(nacho,
                      caption = "Nachos",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
    
    output$HotDog <- DT::renderDataTable({
        DT::datatable(hotdog,
                      caption = "Hot Dogs",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
    
    output$Bud <- DT::renderDataTable({
        DT::datatable(bud,
                      caption = "Buds",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
    
    output$Cheetos <- DT::renderDataTable({
        DT::datatable(cheeto,
                      caption = "Cheetos",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
    
    output$Cupcakes <- DT::renderDataTable({
        DT::datatable(cupcake,
                      caption = "Cupcakes",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
    
    output$Chickenwings <- DT::renderDataTable({
        DT::datatable(chickenwing,
                      caption = "Chickenwings",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
    
    output$Chili <- DT::renderDataTable({
        DT::datatable(chili,
                      caption = "Chilis",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
    
    output$DrPepper <- DT::renderDataTable({
        DT::datatable(drpepper,
                      caption = "Dr. Pepper",
                      colnames = c("", "GM", "Team"),
                      escape = F,
                      options = list(pageLength = 12))
    })
}




# Run the application 
shinyApp(ui = ui, server = server)
