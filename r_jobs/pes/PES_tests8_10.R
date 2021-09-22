require('GMSE')


# test 8 - perceive_feed -0.45
Test_8_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 30,
  land_dim_1 = 100,
  land_dim_2 = 100, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
  res_move_type = 0, 
  res_death_type = 0,
  lambda = 0,
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
  user_budget = 1000, 
  manager_budget = 1000, 
  usr_budget_rng = 100,  
  manage_target = 100000, 
  RESOURCE_ini = 100000, 
  culling = TRUE,
  feeding = TRUE,
  perceive_feed = -0.45,
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Test_8 <- matrix(data=NA, nrow=30, ncol=7)

# loop the simulation. 
for(time_step in 1:30){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Test_8_sim_old)
  
  Test_8[time_step, 1] <- time_step
  Test_8[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Test_8[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Test_8[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Test_8[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Test_8[time_step, 6] <- sim_new$basic_output$manager_results[5]
  Test_8[time_step, 7] <- sum(sim_new$basic_output$user_results[,5])
  
  
  Test_8_sim_old <- sim_new
  print(time_step)
}

colnames(Test_8) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "Feed_cost", "Feed_count")
Test_8_summary <- data.frame(Test_8)
write.csv(Test_8_summary, file="outputs/pes/test_runs/Test_8_summary.csv")



# test 9 - perceive_feed -0.35
Test_9_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 30,
  land_dim_1 = 100,
  land_dim_2 = 100, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
  res_move_type = 0, 
  res_death_type = 0,
  lambda = 0,
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
  user_budget = 1000, 
  manager_budget = 1000, 
  usr_budget_rng = 100,  
  manage_target = 100000, 
  RESOURCE_ini = 100000, 
  culling = TRUE,
  feeding = TRUE,
  perceive_feed = -0.35,
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Test_9 <- matrix(data=NA, nrow=30, ncol=7)

# loop the simulation. 
for(time_step in 1:30){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Test_9_sim_old)
  
  Test_9[time_step, 1] <- time_step
  Test_9[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Test_9[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Test_9[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Test_9[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Test_9[time_step, 6] <- sim_new$basic_output$manager_results[5]
  Test_9[time_step, 7] <- sum(sim_new$basic_output$user_results[,5])
  
  
  Test_9_sim_old <- sim_new
  print(time_step)
}

colnames(Test_9) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "Feed_cost", "Feed_count")
Test_9_summary <- data.frame(Test_9)
write.csv(Test_9_summary, file="outputs/pes/test_runs/Test_9_summary.csv")



# test 10 - perceive_feed -0.25
Test_10_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 30,
  land_dim_1 = 100,
  land_dim_2 = 100, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
  res_move_type = 0, 
  res_death_type = 0,
  lambda = 0,
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
  user_budget = 1000, 
  manager_budget = 1000, 
  usr_budget_rng = 100,  
  manage_target = 100000, 
  RESOURCE_ini = 100000, 
  culling = TRUE,
  feeding = TRUE,
  perceive_feed = -0.25,
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Test_10 <- matrix(data=NA, nrow=30, ncol=7)

# loop the simulation. 
for(time_step in 1:30){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Test_10_sim_old)
  
  Test_10[time_step, 1] <- time_step
  Test_10[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Test_10[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Test_10[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Test_10[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Test_10[time_step, 6] <- sim_new$basic_output$manager_results[5]
  Test_10[time_step, 7] <- sum(sim_new$basic_output$user_results[,5])
  
  
  Test_10_sim_old <- sim_new
  print(time_step)
}

colnames(Test_10) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "Feed_cost", "Feed_count")
Test_10_summary <- data.frame(Test_10)
write.csv(Test_10_summary, file="outputs/pes/test_runs/Test_10_summary.csv")