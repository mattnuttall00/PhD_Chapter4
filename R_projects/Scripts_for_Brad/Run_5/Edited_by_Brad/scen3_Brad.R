require('GMSE')

for(ii in 1:100){

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 350*sin(0.5*s3+0)+400
MB3 <- MB3[1:50]

# standardise to a total cumulative budget of 25,000
MB3 <- 25000*(MB3/sum(MB3))


UB  <- 2000
UBR <- 200

MB <- MB3[1]


Scen3_sim_old <- gmse_apply(
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
Scen3 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  skip_to_next <- FALSE

  tryCatch(
    expr = {
        sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
    },
    error = function(e){ 
        skip_to_next <<- TRUE
    })
  
  if(skip_to_next){
      break;
  }  

  Scen3[time_step, 1] <- time_step
  Scen3[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen3[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen3[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen3[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen3[time_step, 6] <- UB
  Scen3[time_step, 7] <- MB
  
  Scen3_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB3[time_step]

}

colnames(Scen3) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen3_summary <- data.frame(Scen3)

rm(Scen3_sim_old)
rm(Scen3)

filename <- paste("Scen3_summary_rep_", ii, ".csv", sep = "");
write.csv(Scen3_summary, file = filename, row.names = FALSE)


}

