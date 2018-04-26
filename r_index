#library('twitteR')
library(plotly)
library(ggplot2) 
library(leaflet)
library(maps)
library(shiny)
library(readr)
library(dplyr)
#library(ggmap)
library(stringr)
library(htmltools)

#TODO: Clarify red and blue; narrative; popup box after the texts are displayed

##### Data Collection ######

#api_key <- "yZ0Ja6zFZ7Wc9joQhgnTgQxW1" 
#api_secret <- "oVxSaa3EyFVlxHrcBGoB5CH1B2Rn81WrPEJ5YQov5uMDOSAM1z"
#token <- "730589584225144832-FIfeKMMtCpnvHBn1gKYVroD80NnzdSD" 
#token_secret <- "0TijNuANsccPryXEz3iFYdQKo8zQedSbcPu7ZogITteXV" 
#options(httr_oauth_cache=T)
#setup_twitter_oauth(api_key, api_secret, token, token_secret)

#tweets <- searchTwitter("Golden State Warriors", 
#                        n = 1000, 
#                        lang = "en", 
#                        since = "2018-04-18", 
#                        until = "2018-04-20")

#tweets_df <-twListToDF(tweets)

#users <- lookupUsers(tweets_df$screenName)
#users_df <- twListToDF(users)
#users_df$location <- str_replace_all(users_df$location,"[^[:graph:]]", " ") #

#users_df <- mutate(users_df, 
#                   lng = geocode(users_df$location)[[1]], 
#                   lat = geocode(users_df$location)[[2]])


#subset_users <- c("screenName","location","lng","lat")
#subset_tweets <- c("text","created","screenName","isRetweet", "retweetCount")
#map_data <- left_join(users_df[subset_users], tweets_df[subset_tweets], by = c("screenName" = "screenName"))
#map_data$created <- as.POSIXct(map_data$created,"%m/%d/%Y %H:%M:%S") #change date format

#write.csv(tweets_df, "~/stat041/projects/Stat041_p3_grp18/tweets_df.csv")
#write.csv(users_df, "~/stat041/projects/Stat041_p3_grp18/users_df.csv")
#write.csv(map_data, "~/stat041/projects/Stat041_p3_grp18/map_data_nba.csv")

#map_data <- read.csv("~/Stat041_p3_grp18/map_data.csv")

#map_data1 <- read.csv("~/Stat041_p3_grp18/map_data_avicii.csv")
#map_data2 <- read.csv("~/Stat041_p3_grp18/map_data_yahoo.csv")
#map_data3 <- read.csv("~/Stat041_p3_grp18/map_data_nba.csv")

map_data1 <- read.csv("map_data_avicii.csv")
map_data2 <- read.csv("map_data_yahoo.csv")
map_data3 <- read.csv("map_data_nba.csv")

#write.csv(map_data1,"~/stat041/projects/Stat041_p3_grp18/map_data_avicii.csv")
#write.csv(map_data2,"~/stat041/projects/Stat041_p3_grp18/map_data_yahoo.csv")
#write.csv(map_data3,"~/stat041/projects/Stat041_p3_grp18/map_data_nba.csv")

##### Create Master Dataset ######
map_data <- map_data1
map_data <- map_data %>% full_join(map_data2)
map_data <- map_data %>% full_join(map_data3)

##### Simple leaflet ######
#m <- leaflet(map_data) %>% addTiles()
#m %>% addCircles(lng = ~ lng, 
#                 lat = ~ lat, 
#                 weight = 8, radius = 40, 
#                 color = "black", 
#                 stroke = TRUE, 
#                 fillOpacity = 0.8
#                 )

#Reference: https://rstudio.github.io/leaflet/shiny.html
##### UI Side ######

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("output_map", width = "100%", height = "100%"),
  #App title
  absolutePanel(top = 5, 
                left = 45,
                titlePanel("How does the world react in 999 seconds?")),
  
  #Additional widgets
  absolutePanel(class = "panel panel-default",
                style = "opacity: 0.75; padding: 20px 20px 20px 20px; cursor: move; transition: opacity 500ms 1s;", 
                fixed = TRUE,
                draggable = TRUE, 
                top = 20, 
                left = "auto", 
                right = 20,
                bottom = "auto",
                width = 330, 
                height = "auto",
                cursor = c("auto", "move", "default", "inherit"),
                
                selectInput(inputId = "topic", 
                            label = "Topics",
                            choices = c("2018-04-19: NBA: Warriors Game", "2018-04-21: Avicii's Death", "2018-04-24: Yahoo Data Breach")),
                
                checkboxInput(inputId = "rts",
                              label = "Include retweets?",
                              value = TRUE),
                
                sliderInput(inputId = "animation", label = "Click play for animation:",
                            min = 1, max = 999,
                            value = 1, step = 3,
                            animate =
                              animationOptions(interval = 1, loop = FALSE)), 
                plotOutput("histogram"),
                textOutput("text1"),
                textOutput("text2")
                
                
  )
)

##### Server Side #####
server <- function(input, output){
  
  map_data_r <- reactive({
    filter(map_data[map_data$topic == input$topic,], isRetweet == input$rts | isRetweet == FALSE)
  }) 
  
  map_data_chart <- reactive({
    map_data[map_data$topic == input$topic,] %>%
      unique(by = text) %>%
      select(location, retweetCount) %>%
      group_by(location) %>%
      summarize(total = sum(retweetCount)) %>%
      filter(location != "") %>%
      top_n(n = 8, wt = total)
    
  })
  
  output$text1 <- renderText("Click on the PLAY button under the timeline to activate the animation. Tweets will appear as they were created in seconds after the first mention.")
  
  output$text2 <- renderText("Click on a circle to view the tweet. Circles are sized by number of retweets.")
  
  #these parts of the map is unchanged
  output$output_map <- renderLeaflet({
    leaflet(map_data_r()) %>% 
      addTiles() %>%
      setView(0, 20, zoom = 3)
  })
  
  output$histogram <- renderPlot({
    ggplot(data = map_data_chart(), aes(x = reorder(location, total), y = total )) + 
      geom_col(fill = "dodgerblue", alpha = .8) +
      labs(x = "Tweet Location", y = "Total Retweets", title = "Top Retweeted Locations") +
      theme_classic() +
      coord_flip() + 
      geom_text(aes(label = total))
  })
  
  content1 <- paste(sep = "<br/>",
                    "<b>Instructions</b>",
                    "Click on the PLAY button under the timeline to activate the animation. Tweets will appear as they were created in seconds after the first mention.",
                    "Click on a circle to view the tweet. Circles are sized by number of retweets.")
  
  content2 <- paste(sep = "<br/>",
                    "<b>Click on each dot to display tweet message</b>")
  
  LEAF <- reactive({
    
    leafletProxy("output_map", 
                 data = map_data_r()[(map_data_r()$seconds<input$animation) & (map_data_r()$topic==input$topic),]) %>%
      clearShapes() %>%
      addCircleMarkers(lng = ~ lng, 
                       lat = ~ lat, 
                       radius = ~(retweetCount/max(retweetCount)*20) + 8, 
                       color = ~ifelse(isRetweet==TRUE , "dodgerblue", "orangered"),
                       fillColor = ~ifelse(isRetweet==TRUE , "dodgerblue", "orangered"),
                       fill = TRUE,
                       stroke = FALSE, 
                       fillOpacity = 0.1,
                       popup = ~text
      )
  })
  
  observe({
    ifelse(input$animation == 1,
           LEAF()  %>%
             addPopups(30, 40, content1, 
                       options = popupOptions(closeButton = TRUE,
                                              zoomAnimation = TRUE, 
                                              textsize = "20px")
             ),ifelse(input$animation == 999,
                      LEAF()  %>%
                        addPopups(-95.71289, 37.09024, content2, 
                                  options = popupOptions(closeButton = TRUE,
                                                         zoomAnimation = TRUE, 
                                                         textsize = "20px")
                        ),
                      LEAF()
             )
    )
  })
  
}


shinyApp(ui=ui, server=server)
