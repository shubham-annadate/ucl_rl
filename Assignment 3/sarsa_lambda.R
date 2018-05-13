library(tidyverse)

source("../Assignment 1/env.R")
load("../Assignment 2/action_value_mat.Rdata")

options(digits = 15)

SARSA_lambda <- function(n_episodes, lambda, gamma, calc_episode_err = FALSE){
  
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
  
  # episode error
  episode_error <- c()
  
  for(i in 1:n_episodes){
    
    # this will take into account the eligibility for each state action pair 
    # while assigning rewards and penalties.
    eligibility_mat <- array(rep(0, 10*21*2), dim = c(10,21,2))
    
    # player draws a card
    players_first_card <- sample(1:10, 1)
    
    # dealer draws a card
    dealers_first_card <- sample(1:10, 1)
    
    # initializing the player's state
    player_state <- list(dealers_first_card = dealers_first_card,
                         players_sum = players_first_card)
    

    # randomly initialize action 
    action <- ifelse(runif(1) > 0.5,2,1)
    
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

      # increment the state action count by 1
      state_action_count_mat[dfc, ps, action] <- state_action_count_mat[dfc, ps, action] + 1 
      
      # get label for the action
      action_label <- ifelse(action == 1, "stick", "hit")
      
      # do the action and observe the next state you get from the environment
      player_next_state <- step(player_state, action_label)
  
      # next we select an action according to our epsilon greedy policy
      
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
        next_action <- ifelse(rand_number_2 > 0.5, 2, 1)
      }else{
        # if you go greedily then see which action from this state has maximum q value
        # and use that action
        q_action_1 <- action_value_mat[dfc, ps, 1]
        q_action_2 <- action_value_mat[dfc, ps, 2]
        next_action <- ifelse(q_action_1 > q_action_2, 1, 2)
      }
      
      reward <- player_next_state$reward
      next_dfc <- player_next_state$new_state$dealers_first_card
      next_ps <- player_next_state$new_state$players_sum
      
      # calculating delta
      if(player_next_state$game_over == TRUE){
        delta <- reward - action_value_mat[dfc, ps, action]
      }else{
        delta <- reward + gamma*(action_value_mat[next_dfc, next_ps, next_action]) - action_value_mat[dfc, ps, action]  
      }
      eligibility_mat[dfc, ps, action] <- eligibility_mat[dfc, ps, action] + 1
      
      # update all the state action value for the reward 
      for(i in 1:10){
        for(j in 1:21){
          for(k in 1:2){
            alpha <- ifelse(state_action_count_mat[i,j,k] == 0,0,(1/(state_action_count_mat[i, j, k])))
            action_value_mat[i,j,k] <- action_value_mat[i,j,k] + (alpha*delta*eligibility_mat[i,j,k])
            eligibility_mat[i,j,k] <- gamma*lambda*eligibility_mat[i,j,k]
          }
        }
      }
      
      # if terminal state of the episode then break
      if(player_next_state$game_over == TRUE){
        break
      }
      
      # make playrs next state as players current state
      # and players next action as players current action
      player_state <- player_next_state$new_state
      action <- next_action
    
      
    }
    
    # calculate_episode_error
    if(calc_episode_err){
      error <- 0
      for(i in 1:10){
        for(j in 1:21){
          for(k in 1:2){
            error <- error + (avm[i,j,k] - action_value_mat[i,j,k])^2
          }
        }
      }
      error <- error/(10*21*2)
      episode_error <- c(episode_error, error)
    }
  }

  if(calc_episode_err){
    return(list(avm = action_value_mat, errors = episode_error))
  }

  return(action_value_mat)
  
}

# main

error_vector <- c()

n_eps <- 1000

# for lambda varying from 0 to 1 by step size of 0.1

for(lambda in seq(0,1,0.1)){
  avm_sarsa <- SARSA_lambda(n_eps, lambda, 0.9)
  error <- 0
  for(i in 1:10){
    for(j in 1:21){
      for(k in 1:2){
        error <- error + (avm[i,j,k] - avm_sarsa[i,j,k])^2
      }
    }
  }
  error_vector <- c(error_vector, error)
}

#plotting

ggplot(data = NULL, aes(x =seq(0,1,0.1), y = error_vector))+
  geom_line()+
  geom_point()+
  labs(x = "lambda",
       y = "RMSE")+
  scale_x_continuous(breaks = seq(0,1,0.1))

# for lambda = 0 and lambda = 1

avm_sarsa_0 <- SARSA_lambda(n_eps, 0, 1, TRUE)
avm_sarsa_1 <- SARSA_lambda(n_eps, 1, 1, TRUE)

# plotting
tibble(tstep = 1:n_eps,
       lambda_0_err = avm_sarsa_0$errors,
       lambda_1_err = avm_sarsa_1$errors) %>% 
  gather(key = lambda, value = err, -tstep) %>%
  ggplot(aes(tstep, err)) +
  geom_line(aes(color = lambda))








