#' Check guess
#' @description Checks if a guess is correct. Is an internal function and not supposed to be called by the user.
#'
#' @param guess A vector of character values containing the guessed colours.
#'
#' @return A list with the correctly guessed colours and slots.
#' @export
checkGuess <- function(guess) {
  correctColours <- 0
  correctSlots <- 0

  for(i in 1:length(guess)) {
    if(guess[i] == TRUTH[i]) {
      correctSlots <- correctSlots + 1
    }
    if(guess[i] %in% TRUTH[-i]) {
      correctColours <- correctColours + 1
    }
  }
  return(list("correctSlots" = correctSlots, "correctColours" = correctColours))
}
