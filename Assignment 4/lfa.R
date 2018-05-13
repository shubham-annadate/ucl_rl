library(tidyverse)

source("../Assignment 1/env.R")
load("../Assignment 2/action_value_mat.Rdata")

# this data frame will help us with the feature extraction
all_codes_df <- expand.grid(
  dcode = 1:3,
  pcode = 1:6,
  acode = 1:2
) %>% mutate(feat = row_number())

# function that given the state and action returns the feature vector
feature_extraction <- function(dealers_first_card, players_sum, action){
  
  d_codes <- c(ifelse(between(dealers_first_card, 1, 4), 1, 0),
               ifelse(between(dealers_first_card, 4, 7), 2, 0),
               ifelse(between(dealers_first_card, 7, 10), 3, 0))
  
  p_codes <- c(ifelse(between(players_sum, 1, 6), 1, 0),
               ifelse(between(players_sum, 4, 9), 2, 0),
               ifelse(between(players_sum, 7, 12), 3, 0),
               ifelse(between(players_sum, 10, 15), 4, 0),
               ifelse(between(players_sum, 13, 18), 5, 0),
               ifelse(between(players_sum, 16, 21), 6, 0))
  
  a_codes <- c(ifelse(action == 1,1,0), ifelse(action == 2,1,0))

  feat_vals <- filter(all_codes_df, dcode %in% d_codes, pcode %in% p_codes, acode %in% a_codes)$feat
  
  x_s <- rep(0, 36)
  x_s[feat_vals] <- 1
  x_s
  
}

options(digits = 15)

SARSA_lambda_LFA <- function(n_episodes, lambda, gamma, calc_episode_err = FALSE){
  
  # episode error
  episode_error <- c()
  
  # initialize our weights to 0
  weights <- as.matrix(rnorm(36), nrow = 36)
  
  # fixed epsilon
  epsilon <- 0.05
  
  # fixed step size
  alpha <- 0.01
  
  for(i in 1:n_episodes){
    
    # eligibility for each feature     
    eligibility_vect <- c(rep(0,36))
    
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
        q_action_1 <- t(feature_extraction(dfc, ps, 1)) %*% weights
        q_action_2 <- t(feature_extraction(dfc, ps, 2)) %*% weights
        next_action <- ifelse(q_action_1 > q_action_2, 1, 2)
      }
      
      reward <- player_next_state$reward
      next_dfc <- player_next_state$new_state$dealers_first_card
      next_ps <- player_next_state$new_state$players_sum
      
      current_q_value <- t(feature_extraction(dfc, ps, action)) %*% weights
      next_q_value <- t(feature_extraction(next_dfc, next_ps, next_action)) %*% weights
      
      # calculating delta
      if(player_next_state$game_over == TRUE){
        delta <- as.vector(reward - current_q_value)
      }else{
        delta <- as.vector(reward + gamma*(next_q_value) - current_q_value)  
      }

      # updating eligibility for weight updates
      eligibility_vect <- (lambda * eligibility_vect) + feature_extraction(dfc, ps, action)
      
      # updating the weights
      gradient <- alpha * delta * eligibility_vect
      weights <- weights + gradient

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
            state_action_value <- t(feature_extraction(i,j,k)) %*% weights
            error <- error + (avm[i,j,k] - state_action_value)^2
          }
        }
      }
      error <- error/(10*21*2)
      episode_error <- c(episode_error, error)
    }
  }
  
  if(calc_episode_err){
    return(list(weights = weights, errors = episode_error))
  }
  
  return(weights)
  
}

# main 

# for lambda = 0 and lambda = 1

n_eps <- 1000

avm_sarsa_lfa_0 <- SARSA_lambda_LFA(n_eps, 0, 1, TRUE)
avm_sarsa_lfa_1 <- SARSA_lambda_LFA(n_eps, 1, 1, TRUE)

# plotting

tibble(tstep = 1:n_eps,
       lambda_0_err = avm_sarsa_lfa_0$errors,
       lambda_1_err = avm_sarsa_lfa_1$errors) %>% 
  gather(key = lambda, value = err, -tstep) %>%
  ggplot(aes(tstep, err)) +
  geom_line(aes(color = lambda))



