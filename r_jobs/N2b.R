require('GMSE')


MB  <- 200

N2b_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, # landscape is 22,500ha or 22.5km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, 
  agent_move = 50, 
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 0, # no natural death 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, 
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.08, # Trees have 8% impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = 200, 
  manager_budget = MB, 
  usr_budget_rng = 20, # introduce variation around the mean user budget (removes step pattern) 
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.01, # tending crops increases yield by 1% - less than that of culling trees
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
N2b <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = N2b_sim_old, manager_budget=MB)
  
  N2b[time_step, 1] <- time_step
  N2b[time_step, 2] <- sim_new$basic_output$resource_results[1]
  N2b[time_step, 3] <- sim_new$basic_output$observation_results[1]
  N2b[time_step, 4] <- sim_new$basic_output$manager_results[3]
  N2b[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  N2b[time_step, 6] <- MB
  
  N2b_sim_old <- sim_new
  MB <- MB - 3
  
}

colnames(N2b) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count",
                   "Manager_budget")
N2b_summary <- data.frame(N2b)

write.csv(N2b_summary, file = "outputs/investment/null_scenarios/N2/N2b_summary.csv")

rm(N2b)