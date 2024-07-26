# DASHBOARD LINK: https://ejoez.shinyapps.io/BasicPokemonInfo/

# Sources that I used:
# https://shiny.rstudio.com/gallery/widget-gallery.html for the radio button and text input
# https://shiny.rstudio.com/reference/shiny/latest/
# https://stackoverflow.com/questions/35651669/r-shiny-display-output-externally-hosted-image
# https://stackoverflow.com/questions/33519816/shiny-what-is-the-difference-between-observeevent-and-eventreactive
# https://community.rstudio.com/t/how-to-have-shiny-app-refresh-on-action-button/37458
# https://stackoverflow.com/questions/21996887/embedding-image-in-shiny-app/21998722#21998722
# https://stackoverflow.com/questions/13967063/remove-duplicated-rows
# https://shiny.rstudio.com/reference/shiny/0.12.2/tableOutput.html

library(shiny)
library(httr)
library(jsonlite)
suppressWarnings(library(tidyverse))

r <- GET("https://pokeapi.co/api/v2/pokemon/pikachu")
rawdata <- fromJSON(rawToChar(r$content))


ui <- fluidPage(
    titlePanel("Pokemon Information"),
    radioButtons("radio", label = h3("Display"),
                 choices = list("Stats" = 0, "Moves" = 1), 
                 selected = 0),
    textInput("text", label = h3("What pokemon do you want to learn about?"), value = "pikachu"),
    verbatimTextOutput("textin"),
    br(),
    br(),
    br(),
    htmlOutput("picture"),
    dataTableOutput('table')
    
    # sidebarLayout(
    #   sidebarPanel(
    #     
    #   ),
    #   mainPanel(
    #     
    #   )
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$text, {
    pokemon <- toString(input$text)
    output$textin <- renderText({paste(pokemon, "\nCurrent pokemon: ", rawdata$name, sep="")})
    r <- GET(paste("https://pokeapi.co/api/v2/pokemon/", pokemon, sep=""))
    # print(r)
    if (r$status_code == 200 & pokemon != "") {
      rawdata <- fromJSON(rawToChar(r$content))
      src = rawdata$sprites$front_default
      output$picture<-renderText({c('<img src="',src,'", style="width: 250px; height: 250px;">')})
      if (input$radio == 0) {  # If show stats
        output$table <- renderDataTable(rawdata$stats %>% unnest(stat) %>% select(name, base_stat, effort))
      } else {
        output$table <- renderDataTable(rawdata$moves %>% unnest(move) %>% unnest(version_group_details) %>% distinct(url, .keep_all=TRUE) %>% select(name, level_learned_at))
      }
    } else {
      output$textin <- renderText({paste(pokemon, "Not a pokemon, please check your spelling!", sep="\n")})
    }
  })
  
  observeEvent(input$radio, {
    if (input$radio == 0) {  # If show stats
      output$table <- renderDataTable(rawdata$stats %>% unnest(stat) %>% select(name, base_stat, effort))
    } else {
      output$table <- renderDataTable(rawdata$moves %>% unnest(move) %>% unnest(version_group_details) %>% distinct(url, .keep_all=TRUE) %>% select(name, level_learned_at))
    }
  })
  
  # acttext <- reactive({toString(input$text)})
  # output$value <- renderPrint({ input$radio })
  # src = rawdata$sprites$front_default
  # output$picture<-renderText({c('<img src="',src,'">')})
  # output$textin <- renderText({acttext()})
  # reactive(print(acttext))
  # r <- reactive(GET(paste("https://pokeapi.co/api/v2/pokemon/", acttext, sep="")))
  # reactive(
  #   if (r()$status_code == 200) {
  #     print("New pokeman")
  #     rawdata <- fromJSON(rawToChar(r$content))
  #     src = rawdata$sprites$front_default
  #     output$picture<-renderText({c('<img src="',src,'">')})
  #   } else {
  #     print("Oof")
  #   }
  # )
}

# Run the application 
shinyApp(ui = ui, server = server)
