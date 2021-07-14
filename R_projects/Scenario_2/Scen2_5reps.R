#### SCENARIO 2 ####

#### Manager and user budget increase, with the same slope


### define manager budget

# define slope 
xx <- 5204.1/1275

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 400

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))





### RUN 1

UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
  res_move_type = 0, 
  res_death_type = 0, 
  observe_type = 2, 
  times_observe = 1, 
  obs_move_type = 1, 
  res_min_age = 0, 
  res_move_obs = FALSE, 
  plotting = FALSE, 
  res_consume = 0.08, 
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_1_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)

write.csv(Scen2_1_summary, file = "outputs/investment/scenarios/Scenario_2/Scen2_1_summary.csv")




### RUN 2
UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
  res_move_type = 0, 
  res_death_type = 0, 
  observe_type = 2, 
  times_observe = 1, 
  obs_move_type = 1, 
  res_min_age = 0, 
  res_move_obs = FALSE, 
  plotting = FALSE, 
  res_consume = 0.08, 
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_2_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)



### RUN 3
UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
  res_move_type = 0, 
  res_death_type = 0, 
  observe_type = 2, 
  times_observe = 1, 
  obs_move_type = 1, 
  res_min_age = 0, 
  res_move_obs = FALSE, 
  plotting = FALSE, 
  res_consume = 0.08, 
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_3_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)




### RUN 4
UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
  res_move_type = 0, 
  res_death_type = 0, 
  observe_type = 2, 
  times_observe = 1, 
  obs_move_type = 1, 
  res_min_age = 0, 
  res_move_obs = FALSE, 
  plotting = FALSE, 
  res_consume = 0.08, 
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_4_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)




### RUN 5
UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
  res_move_type = 0, 
  res_death_type = 0, 
  observe_type = 2, 
  times_observe = 1, 
  obs_move_type = 1, 
  res_min_age = 0, 
  res_move_obs = FALSE, 
  plotting = FALSE, 
  res_consume = 0.08, 
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_5_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)


# combine results
Scen2_all_summary <- rbind(Scen2_1_summary,Scen2_2_summary,Scen2_3_summary,Scen2_4_summary,Scen2_5_summary)

write.csv(Scen2_all_summary, file = "outputs/investment/scenarios/Scen2_all_summary.csv")