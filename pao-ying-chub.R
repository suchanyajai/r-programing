## load libraries
library(tidyverse)
library(glue)

## starter template
game <- function() {
  ## variable
  hands_score <- 0
  comp_score <- 0
  round_count <- 1
  hands <- c("hammer", "scissor", "paper")
  comp_hand <- sample(hands, 1)
  user_restart <- "YES"

  print("Welcome to Pao-Ying-Chu Game! You have 3 chances to be The Winner!")

  ## loop 3 round
  while (round_count <= 3) {
    ## display to user Choose
    print("Let Started!")
    print(paste0("Round: ", round_count))

    user_hand <- readline("Choose your hand (hammer/scissor/paper): ")

    if (
      user_hand != "hammer"
      && user_hand != "scissor"
      && user_hand != "paper"
    ) {
      print("Wrong input. Please type again.")
      next
    }

    ## logic games
    if (user_hand == comp_hand) {
      result <- "Draw"
    } else if (
      (user_hand == "hammer" && comp_hand == "scissor")
      || (user_hand == "scissor" && comp_hand == "paper")
      || (user_hand == "paper" && comp_hand == "hammer")
    ) {
      result <- "Win"
      hands_score <- hands_score + 1
    } else {
      result <- "Lose"
      comp_score <- comp_score + 1
    }

    ## result in round
    print(paste0("Bot Choose: ", comp_hand))
    print(paste0("You choose: ", user_hand))
    print(paste0("Result: ", result))
    print(paste0("You Score: ", hands_score, "  Bot Score:", comp_score))
    print("---------------------------------")

    round_count <- round_count + 1
  }

  ## Final result
  print("Finished!")
  print(paste0("Final scores - You: ", hands_score, ", Bot: ", comp_score))
  if (hands_score > comp_score){
    print("YOU WIN!!")
  } else if (hands_score < comp_score) {
    print("YOU LOSE :(")
  } else {
    print("DRAW..., No winner.")
  }

  ## restart games
  restart <- readline("Do you want to restatr? (YES): ")

  if (user_restart == restart) {
    game()
  } else {
    print("Thank You for Comming ka :)")
  }
}