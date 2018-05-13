# function to simulate the enviroment 

step <- function(state, action){
  
  players_sum <- state$players_sum
  dealers_first_card <- state$dealers_first_card
  
  # When the player chooses to hit
  
  if(action == "hit"){
      
      # player draws a card
      
      #cat('player hits')
    
      is_black <- rbinom(c(1),1,0.67)
      card_drawn <- sample(1:10, 1)
      if(is_black){
        players_sum = players_sum + card_drawn
        #cat("player got black card with value ", card_drawn, "\n")
        #cat("player sum is now ", players_sum, "\n")
      }
      else{
        players_sum = players_sum - card_drawn
        #cat("player got red card with value ", card_drawn, "\n")
        #cat("player sum is now ", players_sum, "\n")
      }
      
      # Check if player has gone burst
      
      if(players_sum > 21 || players_sum < 1){
        #cat("player chose hit and was burst -> dealer won\n")
        return(list(new_state = list(dealers_first_card = dealers_first_card, players_sum = players_sum),
             reward = -1, game_over = TRUE))
      }
      else{
        #cat("player chose hit and is safe\n")
        return(list(new_state = list(dealers_first_card = dealers_first_card, players_sum = players_sum),
             reward = 0, game_over = FALSE))
      }
  }
  
  # When player chooses to stick
  
  else{
    
    #cat('player sticks')
    
    # Check if player has gone burst
    
    if(players_sum > 21 || players_sum < 1){
      #cat("player chose to stick and was burst -> dealer won\n")
      return(list(new_state = list(dealers_first_card = dealers_first_card, players_sum = players_sum),
           reward = -1, game_over = TRUE))
    }
    
    # Dealers turn
    
    dealers_sum = dealers_first_card
    dealers_action = "hit"
    
    while(dealers_action == "hit"){
      
      # dealer draws a card
      
      is_black <- rbinom(c(1),1,0.67)
      card_drawn <- sample(1:10, 1)
      if(is_black){
        dealers_sum = dealers_sum + card_drawn
        #cat("dealer got black card with value ", card_drawn, "\n")
        #cat("dealer sum is now ", dealers_sum, "\n")
        
      }
      else{
        dealers_sum = dealers_sum - card_drawn
        #cat("dealer got red card with value ", card_drawn,"\n")
        #cat("dealer sum is now ", dealers_sum, "\n")
      }
      
      # Check if dealer has gone burst
      
      if(dealers_sum > 21 || dealers_sum < 1){
        #cat("dealer chose hit and was burst\n")
        return(list(new_state = list(dealers_first_card = dealers_first_card, players_sum = players_sum),
             reward = +1, game_over = TRUE))
      }
      else{
        dealers_action = ifelse(dealers_sum >= 17, "stick", "hit")
        #cat("dealers next action is to ", dealers_action, "\n")
      }
      
    }
    
    # When dealer sticks
    
    # player sum is less that dealers sum -> player loses -> reward = -1
    
    if(players_sum < dealers_sum){
      #cat("dealer chose stick. player sum < dealer sum, dealer won\n")
      return(list(new_state = list(dealers_first_card = dealers_first_card, players_sum = players_sum),
           reward = -1, game_over = TRUE))
    }
    
    # player sum is greater than dealers sum -> player wins -> reward = +1
    
    else if(players_sum > dealers_sum){
      
      #cat("dealer chose stick. player sum > dealer sum, player won\n")
      return(list(new_state = list(dealers_first_card = dealers_first_card, players_sum = players_sum),
           reward = +1, game_over = TRUE))
    }
    
    # draw between player and dealer -> reward = 0
    
    else{
      
      #cat("dealer chose stick. player sum = dealer sum, draw\n")
      return(list(new_state = list(dealers_first_card = dealers_first_card, players_sum = players_sum),
           reward = 0, game_over = TRUE))
    }
  }
  
}

