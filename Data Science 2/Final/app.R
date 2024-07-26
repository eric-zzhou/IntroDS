# https://docs.posit.co/shinyapps.io/applications.html
library(shiny)
library(r2r)

# Processing text into hashmap
text_process <- function(filename, n_g) {
  # Pre-processing
  txt <- paste(readLines(filename), collapse = "\n")
  txt <- iconv(txt, "ASCII", "UTF-8", sub = "")
  txt <- gsub('\"', "", txt, fixed = TRUE)
  lower_str <- tolower(txt)
  split_str <- strsplit(gsub("\n", " ", lower_str), "\\s+")[[1]]
  split_str <- append(split_str, split_str[1])
  split_str <- append(split_str, split_str[2])
  
  # Creating a hashmap
  dict <- hashmap(default = c())
  for (j in seq(length(split_str) - 2)) {
    ngram <- ""
    for (k in seq(n_g)) {
      ngram <- paste(ngram, split_str[j + k - 1])
    }
    ngram <- trimws(ngram)
    dict[[ngram]] <- append(dict[[ngram]], split_str[j + n_g])
  }
  return(dict)
}

# Create frequency plots based on probabilities
prob_plot <- function(start_wrds, dict, n_g) {
  og_wrds <- trimws(start_wrds)
  start_wrds <- strsplit(tolower(start_wrds), "\\s+")[[1]]
  check_wrds <- c()
  for (k in seq(length(start_wrds) - n_g + 1, length(start_wrds), 1)) {
    check_wrds <- append(check_wrds, start_wrds[k])
  }
  start_wrds <- ""
  for (thing in check_wrds) {
    start_wrds <- paste(start_wrds, thing)
  }
  start_wrds <- trimws(start_wrds)
  if (is.null(dict[[start_wrds]])) {
    return("These words do not exist in the corpus, please try something else. T_T")
  }
  lst <- dict[[start_wrds]]
  return(as.data.frame(table(lst)))
}

# Generating text
gen_text <- function(len, start_wrds, dict, n_g) {
  # Pre-process query
  og_wrds <- trimws(start_wrds)
  start_wrds <- strsplit(tolower(start_wrds), "\\s+")[[1]]
  check_wrds <- c()
  for (k in seq(length(start_wrds) - n_g + 1, length(start_wrds), 1)) {
    check_wrds <- append(check_wrds, start_wrds[k])
  }
  start_wrds <- ""
  for (thing in check_wrds) {
    start_wrds <- paste(start_wrds, thing)
  }
  start_wrds <- trimws(start_wrds)
  
  # Check if entry exists
  if (is.null(dict[[start_wrds]])) {
    return("These words do not exist in the corpus, please try something else. T_T")
  }
  
  # Building markov chain string
  s <- ""
  for (j in seq(len)) {
    temp <- dict[[start_wrds]]
    new_wrd <- temp[sample(1:length(temp), 1, replace = TRUE)]
    s <- paste(s, new_wrd)
    check_wrds <- check_wrds[-1]
    check_wrds <- append(check_wrds, new_wrd)
    start_wrds <- ""
    for (thing in check_wrds) {
      start_wrds <- paste(start_wrds, thing)
    }
    start_wrds <- trimws(start_wrds)
  }
  return(paste(og_wrds, trimws(s)))
}

# All the hashmaps
shakedict <- text_process("Shakespeare.txt", 2)
hpdict <- text_process("Harry Potter 1.txt", 2)
shakedict1 <- text_process("Shakespeare.txt", 1)
hpdict1 <- text_process("Harry Potter 1.txt", 1)
shakedict3 <- text_process("Shakespeare.txt", 3)
hpdict3 <- text_process("Harry Potter 1.txt", 3)

# UI
ui <- fluidPage(titlePanel("Markov Text Generation"),
                
                sidebarLayout(
                  sidebarPanel(
                    radioButtons(
                      inputId = "text",
                      label = "Which text would you like to use?",
                      choices = c("Shakespeare" = "shake", "Harry Potter" = "hp"),
                      selected = character(0)
                    ),
                    sliderInput(
                      inputId = "ng",
                      label = "N-gram to Use",
                      min = 1,
                      max = 3,
                      value = 2
                    ),
                    sliderInput(
                      inputId = "words",
                      label = "Number of Words to Generate",
                      min = 2,
                      max = 200,
                      value = 20
                    ),
                    textInput(inputId = "start_text",
                              label = "Words to start with: "),
                    actionButton("go", "Submit")
                  ),
                  mainPanel(
                    # https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html
                    plotOutput("plot", height="500px"),
                    
                    # https://community.rstudio.com/t/wrap-text-output-that-is-rendered-with-textoutput/3812/2
                    tags$head(tags$style(
                      HTML("pre { white-space: pre-wrap; word-break: keep-all; }")
                    )),
                    
                    br(),
                    textOutput("textOut"))
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$go, {
    start <- toString(input$start_text)
    if (input$text == "shake") {
      if (input$ng == 1) {
        textfile <- shakedict1
      } else if (input$ng == 2) {
        textfile <- shakedict
      } else {
        textfile <- shakedict3
      }
    } else if (input$text == "hp") {
      if (input$ng == 1) {
        textfile <- hpdict1
      } else if (input$ng == 2) {
        textfile <- hpdict
      } else {
        textfile <- hpdict3
      }
    }
    
    # https://stackoverflow.com/questions/35134405/r-returning-the-5-rows-with-the-highest-values
    # https://stackoverflow.com/questions/30849417/display-blank-plot-in-shiny-instead-of-error
    # https://stackoverflow.com/questions/38529998/r-string-column-is-considered-as-integer
    tryCatch({
      output$plot <- renderPlot({
        df <- prob_plot(start, textfile, input$ng)
        df2 <- head(df[order(-df$Freq), ], 5)
        df2$lst <- levels(df2$lst)[df2$lst]
        barplot(
          df2$Freq ~ df2$lst,
          col = "turquoise",
          xlab = "Words",
          ylab = "Frequencies",
          main = "Top 5 Possible Next Words"
        )
      })
      
    },
    error = function(e) {
      print("Caught error")
      output$plot <- ""
    })
    
    output$textOut <- renderText({
      gen_text(input$words, start, textfile, input$ng)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
