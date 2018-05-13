library(plot3D)

source("../Assignment 1/env.R")

options(digits = 15)


MCC <- function(n_episodes){
  
  # create a action value 3d array
  
  # dim 1: size of 10 is for the dealers first card (1-10)
  # dim 2: size of 21 is for the players sum (1-21)
  # dim 3: size of 2 is for action (1-stick, 2-hit)
  action_value_mat <- array(rep(0, 10*21*2), dim = c(10,21,2))
  
  
  # create a alpha matrix to store number of times action a is selected from state s
  state_action_count_mat <- array(rep(0, 10*21*2), dim = c(10,21,2))
  
  # create a matrix to keep track of number of times you have visited a particular state
  # this will be helful when you calculate epsilon
  state_visit_count_mat <- array(rep(0, 10*21), dim = c(10,21))
  
  # constant 
  N_O = 100
  
  for(i in 1:n_episodes){
  
    # player draws a card
    players_first_card <- sample(1:10, 1)
    
    # dealer draws a card
    dealers_first_card <- sample(1:10, 1)
    
    #print(dealers_first_card)
    
    # initializing the player's state
    player_state <- list(dealers_first_card = dealers_first_card,
                         players_sum = players_first_card)
    
    # maintaining a list of all the state actions you take
    # this will be helpful when we do the final update
    episode_action_list <- list()
    list_index <- 1
    
    while(TRUE){
        
      # dealers first card
      dfc <- player_state$dealers_first_card
      
      # player sum
      ps <- player_state$players_sum
      
      # get the number of times you have visited this state
      N_St <- state_visit_count_mat[dfc, ps]
      
      # calculate epsilon 
      epsilon <- N_O/(N_O + N_St)
      
      # increment number of times you have visited this state by 1
      state_visit_count_mat[dfc, ps] <- state_visit_count_mat[dfc, ps] + 1 
      
      # generate a random number
      rand_number_1 <- runif(1)
      
      # intialize greedy as false
      greedy = FALSE
      
      # see if you want to go greedily or want to explore
      if(rand_number_1 > epsilon){
        greedy = TRUE
      }
      
      if(!greedy){
        # if you decide to go exploratorily then choose a random action (stick/hit)
        rand_number_2 <- runif(1)
        action <- ifelse(rand_number_2 > 0.5, 2, 1)
      }else{
        # if you go greedily then see which action from this state has maximum q value
        # and use that action
        q_action_1 <- action_value_mat[dfc, ps, 1]
        q_action_2 <- action_value_mat[dfc, ps, 2]
        action <- ifelse(q_action_1 > q_action_2, 1, 2)
      }
      
      # increment the state action count by 1
      state_action_count_mat[dfc, ps, action] <- state_action_count_mat[dfc, ps, action] + 1 
      
      # add the state action to the episode action list
      episode_action_list[[list_index]] <- c(dfc, ps, action)
      list_index <- list_index + 1
      
      # get label for the action
      action_label <- ifelse(action == 1, "stick", "hit")
      
      # do the action and observe the next state you get from the environment
      player_next_state <- step(player_state, action_label)
      
      # if game over then
      if(player_next_state$game_over == TRUE){
        #cat('episode over')
        
        # get the reward
        reward <- player_next_state$reward
        
        # for each state you visited in that episode 
        # update it q value
        for(i in 1:length(episode_action_list)){
          state_action_seq <- episode_action_list[[i]]
          dfc = state_action_seq[1]
          ps = state_action_seq[2]
          a = state_action_seq[3]
          action_value_mat[dfc,ps,a] <- action_value_mat[dfc,ps,a] + ((1/(state_action_count_mat[dfc, ps,a]))*(reward - action_value_mat[dfc,ps,a]))
          #print(c(dfc,ps,a))
          #print(action_value_mat[dfc,ps,a])
        }
        
        # break the loop
        break
      }
      
      # else make your next state as current state
      player_state <- player_next_state$new_state
      
    }
  }
  return(action_value_mat)
  
}

# getting the action value matrix
avm <- MCC(500000)

# getting the optimal value function
state_value_mat <- matrix(rep(0, 10*21), nrow = 10)

for(i in 1:10){
  for(j in 1:21){
    state_value_mat[i,j] <- max(avm[i,j,])
  }
}

# plotting the optimal value function
persp3D(z = state_value_mat)









