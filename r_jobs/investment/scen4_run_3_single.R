require('GMSE')

### As Scenario 3, but the wavelength is shorter (higher frequency) and the amplitude is smaller

## Define manager budget
s4 <- seq(0,50,1)
MB4 <- 30*sin(2.5*s4+0)+500
MB4 <- MB4[1:50]

# Standardise so that cumulative total budget = 25,000
MB4 <- 25000*(MB4/sum(MB4))



UB  <- 2000
UBR <- 200

MB <- MB4[1]


Scen4_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 100000, 
  RESOURCE_ini = 100000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Scen4 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen4_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen4[time_step, 1] <- time_step
  Scen4[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen4[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen4[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen4[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen4[time_step, 6] <- UB
  Scen4[time_step, 7] <- MB
  
  Scen4_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB4[time_step]
}

colnames(Scen4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen4_summary <- data.frame(Scen4)

write.csv(Scen4_summary, file="outputs/investment/scenarios/Run_3/scen4_summary.csv")