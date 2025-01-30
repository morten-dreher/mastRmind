#' Play console game
#'
#' @description Launches a game that you can play on the console. Mainly used for testing.
#'
#' @param seed Seed used when generating random numbers. Primarily used for testing purposes.
#' @param replaceColours Logical value describing if the same colour is allowed more than once. So far, only \code{FALSE} is implemented.
#' @param printTruth Logical value describing whether or not the true colours should be printed. Should only be set to \code{TRUE} for testing.
#' @return Nothing, launches a console game
playGame <- function(seed, replaceColours=FALSE, printTruth = FALSE) {
  if(!missing(seed)) {
      set.seed(seed)
    }
  TRUTH <<- sample(x = SELECTABLE_COLOURS_CHAR, size = 4, replace = replaceColours)

  print("I have thought of a 4-digit code containing the following colours:")
  print(paste(SELECTABLE_COLOURS_CHAR, collapse = ", "))

  if(replaceColours) {
    print("Repetitions of the same colour are allowed")
  }
  else {
    print("Repetitions of the same colour are not allowed")
  }

  print("Can you guess the code?")


  if(printTruth) {
    print(paste0(c("Truth:", TRUTH), collapse = " "))
  }

  roundCounter <- 0

  while(roundCounter < 10) {
    roundCounter <- roundCounter + 1

    guess <- readline(prompt = paste0("Guess the colours! (Guess ", roundCounter, " of 10) \n"))

    guess <- tolower(unlist(strsplit(gsub(pattern = " ", replacement = "", x = guess), split = ",")))

    if(!all(guess %in% SELECTABLE_COLOURS_CHAR)) {
      print("Your guess contained unknown colours or they were not separated by commas. Try again!")
      roundCounter <- roundCounter-1
    }
    else if(length(guess)!=4) {
      print("Your guess did not contain 4 colours or they were not separated by commas. Try again!")
      roundCounter <- roundCounter-1
    }
    else {
    result <- checkGuess(guess)
    print(paste0("Correct slots: ", as.numeric(unlist(result$correctSlots)), ", correct colours: ", as.numeric(unlist(result$correctColours))))

    if(as.numeric(unlist(result$correctSlot))==4) {
      print("Victory! You cracked the code!")
      beepr::beep(sound = 8)
      break
      }
    }
  }
  if(roundCounter >= 10) {
    print("You could not crack the code... Better luck next time!")
  }
}
