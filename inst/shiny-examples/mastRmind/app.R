#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(beepr)

# SELECTABLE_COLOURS_NAMES <- list((image(x=0,y=0, z=matrix(1), col = "red")),
#                                  (image(x=0,y=0, z=matrix(1), col = "green")),
#                                  (image(x=0,y=0, z=matrix(1), col = "blue")),
#                                  (image(x=0,y=0, z=matrix(1), col = "yellow")),
#                                  (image(x=0,y=0, z=matrix(1), col = "purple")),
#                                  (image(x=0,y=0, z=matrix(1), col = "orange")))


SELECTABLE_COLOURS_IMAGE <- list(
img(src = "red.png", width = 20, height = 20),
img(src = "green.png", width = 20, height = 20),
img(src = "blue.png", width = 20, height = 20),
img(src = "yellow.png", width = 20, height = 20),
img(src = "purple.png", width = 20, height = 20),
img(src = "orange.png", width = 20, height = 20)
)

SELECTABLE_COLOURS_CHAR <- c("red", "forestgreen", "blue", "yellow", "purple", "orange")
READABLE_COLOUR_NAMES <- c("Red", "Green", "Blue", "Yellow", "Purple", "Orange")

CONGRATUALTORY_PHRASES <- c("good job!", "excellent work!", "well done!", "congratulations!", "you must be Sherlock Holmes!")

TRUTH <<- sample(x = SELECTABLE_COLOURS_CHAR, size = 4, replace = FALSE)
guessCounter <<- 1
victory <<- FALSE

data_colours <- data.frame(guess = numeric(40), slot = numeric(40), colour = character(40))

data_guesses <- data.frame(guess = numeric(20), count = numeric(20), slot = numeric(20))

# SELECTABLE_COLOURS_CHAR <- c("red", "green", "blue", "yellow", "purple", "orange")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("mastRmind"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            actionButton(inputId = "restart", label = "Restart game"),
            radioButtons(inputId = "choice1", label = "Colour slot 1",
                         choiceValues = SELECTABLE_COLOURS_CHAR, choiceNames = SELECTABLE_COLOURS_IMAGE),
            radioButtons(inputId = "choice2", label = "Colour slot 2",
                         choiceValues = SELECTABLE_COLOURS_CHAR, choiceNames = SELECTABLE_COLOURS_IMAGE),
            radioButtons(inputId = "choice3", label = "Colour slot 3",
                         choiceValues = SELECTABLE_COLOURS_CHAR, choiceNames = SELECTABLE_COLOURS_IMAGE),
            radioButtons(inputId = "choice4", label = "Colour slot 4",
                         choiceValues = SELECTABLE_COLOURS_CHAR, choiceNames = SELECTABLE_COLOURS_IMAGE),
            actionButton(inputId = "makeGuess", label = "Make your guess")
        , width = 2),

        # Show a plot of the generated distribution
        mainPanel(
          h2("Game Board"),
          textOutput("counter"),
          plotOutput("board"),
          h3("How to play"),
          p("For each slot, select a colour via the radio buttons on the left. Once you are comfortable with your choice, press the 'Make your guess' button in the bottom left corner.
            After you have pressed the button, your guess will be displayed on the game board and evaluated.
            Next to your chosen colours, you will find the number of colours you have guessed correctly and the number of slots for which you have guessed the correct colour.
            The number above 'Correct Colours' represents white pegs, the number above 'Correct Slots' represents black pegs.
            Please note that each colour can only be selected once, otherwise your guess will not be counted.
            To begin a new game, press the button on the top left or restart the app.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$counter <- renderText(
      paste0("Guess ", guessCounter, " of 10")
    )
    output$board <- renderPlot({
      ggplot(data = NULL) + scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
        scale_x_continuous(labels = c("1", "2", "3", "4", "Correct \nColours","Correct \nSlots"), breaks = 1:6, limits = c(1, 6)) +
        labs(x = "Slot", y = "Guess number") + theme_bw() +
        theme(panel.grid.minor = element_blank(), text=element_text(size=20))
    })
    # Guess button
    observeEvent(input$makeGuess, {

      if(guessCounter < 11) {

      # guess slot colour
      data_colours$guess[((guessCounter-1)*4+1):(guessCounter*4)] <<- guessCounter
      data_colours$slot[((guessCounter-1)*4+1):(guessCounter*4)] <<- 1:4
      data_colours$colour[((guessCounter-1)*4+1):(guessCounter*4)] <<-
        c(input$choice1, input$choice2, input$choice3, input$choice4)


      checkedGuess <- mastRmind::checkGuess(guess = c(input$choice1, input$choice2, input$choice3, input$choice4))

      # guess count slot
      data_guesses$guess[((guessCounter-1)*2+1):(guessCounter*2)] <<- guessCounter
      data_guesses$slot[((guessCounter-1)*2+1):(guessCounter*2)] <<- c(5,6)
      data_guesses$count[((guessCounter-1)*2+1):(guessCounter*2)] <<- c(checkedGuess$correctColours, checkedGuess$correctSlots)

      if(length(intersect(c(input$choice1, input$choice2, input$choice3, input$choice4), c(input$choice1, input$choice2, input$choice3, input$choice4)))
         < length(c(input$choice1, input$choice2, input$choice3, input$choice4))) {
      showNotification("You reused some colours. Guess again", type = "error")
      }
      else if(victory) {
        showNotification("You won! Restart the game to play again.")
      }
      else {
      guessCounter <<- guessCounter + 1
      showNotification(paste0("Guess ", guessCounter-1, " counted"), type = "message")

      output$board <- renderPlot({
        ggplot(data = data_colours) + geom_point(mapping=aes(x = slot, y = guess), colour = data_colours$colour, size = 12) +
          geom_text(data = data_guesses, mapping = aes(label = count, x = slot, y = guess), size = 8) + scale_colour_discrete(guide = "none") +
          scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
          scale_x_continuous(labels = c("1", "2", "3", "4", "Correct \nColours","Correct \nSlots"), breaks = 1:6, limits = c(1, 6)) +
          labs(x = "Slot", y = "Guess number") + theme_bw() + theme(panel.grid.minor = element_blank(), text=element_text(size=20))
      })
          output$counter <- renderText(
            paste0("Guess ", guessCounter, " of 10")
    )
          if(checkedGuess$correctSlots == 4) {
            beep(sound = 3)
            Sys.sleep(4)

            phrase <- CONGRATUALTORY_PHRASES[ceiling(runif(1, min = 0, max = length(CONGRATUALTORY_PHRASES)))]

            showModal(modalDialog(
              title = "Victory!",
              paste0("You guessed the code, ", phrase),
              easyClose = TRUE,
              footer = NULL
            ))
            victory <<- TRUE
          }


          if(guessCounter > 10 && !victory) {

            readableTruth <- TRUTH
            readableTruth[readableTruth=="red"] <- "Red"
            readableTruth[readableTruth=="forestgreen"] <- "Green"
            readableTruth[readableTruth=="blue"] <- "Blue"
            readableTruth[readableTruth=="yellow"] <- "Yellow"
            readableTruth[readableTruth=="purple"] <- "Purple"
            readableTruth[readableTruth=="orange"] <- "Orange"

            showModal(modalDialog(
              title = "So close...",
              paste0("The correct code was: ", paste(readableTruth, " ", collapse = "")),
              easyClose = TRUE,
              footer = NULL
            ))
          }
      }
      }
      else {
        showNotification("Out of guesses - restart the game", type = "warning")
      }
    })
  observeEvent(input$restart, {
    # Create new truth, reset guess counter
    TRUTH <<- sample(x = SELECTABLE_COLOURS_CHAR, size = 4, replace = FALSE)
    guessCounter <<- 1

    victory <<- FALSE

    # Reset data
    data_colours <<- data.frame(guess = numeric(40), slot = numeric(40), colour = character(40))
    data_guesses <<- data.frame(guess = numeric(20), count = numeric(20), slot = numeric(20))

    # Reset plot, guess counter
    output$counter <- renderText(
      paste0("Guess ", guessCounter, " of 10")
    )
    output$board <- renderPlot({
      ggplot(data = NULL) + scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
        scale_x_continuous(labels = c("1", "2", "3", "4", "Correct \nColours","Correct \nSlots"), breaks = 1:6, limits = c(1, 6)) +
        labs(x = "Slot", y = "Guess number") + theme_bw() +
        theme(panel.grid.minor = element_blank(), text=element_text(size=20))
    })
    showNotification("Game reset", type = "message")

  })


}

# Run the application
shinyApp(ui = ui, server = server)
