require('GMSE')

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

write.csv(Scen4_summary, file="outputs/investment/scenarios/Run_4/Scen4_summary.csv")






## SCENARIO 5

# f function
f <- function(t, w, cs, cf, cd) { 
  ft <- dc.component + sum( cs * sin(cf*w*t + cd));
  return(ft);
}

# plot.fourier function (but set not to actually plot)
plot.fourier <- function(f_function, f.0, ts, cs, cf, cd) {
  w <- 2*pi*f.0
  traj_list    <- lapply(ts, f_function, w = w, cs = cs, cf = cf, cd = cd);
  trajectory   <- unlist(x = traj_list);
  minval       <- min(trajectory);
  maxval       <- max(trajectory);
  trajectory_c <- NULL; # For the components
  for(i in 1:length(cf)){
    traj_list         <- lapply(ts, f, w = w, cs = cs[i], cf = cf[i], 
                                cd = cd[i]);
    trajectory_c[[i]] <- unlist(x = traj_list);
    # Don't worry about these maxval and minval lines line -- just to help plot
    if(minval > min(trajectory_c[[i]])){
      minval <- min(trajectory_c[[i]])
    }
    if(maxval < max(trajectory_c[[i]])){
      maxval <- max(trajectory_c[[i]])
    }
  }
  # plot(x = ts, y = trajectory, type="l", xlab = "time", ylab = "f(t)", lwd = 2,
  #     ylim = c(minval, maxval));
  #for(i in 1:length(cf)){
  # points(x = ts, y = trajectory_c[[i]], type = "l", lwd = 0.35, col = i + 1);  
  #}
  #points(x = ts, y = trajectory, type="l", lwd = 2); # put to foreground
  #abline(h = 500,lty = 3);
  
  return(trajectory)
}

# function to produce random waves made from 3 component waves
random_wave <- function(f.0, dc.component, freq, delay, strength){
  
  acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
  time     <- 50                      # measuring time interval (time steps)
  ts       <- seq(1,time,1)         # vector of sampling time-points (one sample per time step - manager budget) 
  f.0 <- f.0                      # f.0 is the fundamental frequency of the complex wave
  
  dc.component <- dc.component                   # additive constant signal
  component.freqs <- freq          # frequency of signal components (Hz)
  component.delay <- delay         # delay of signal components (radians)
  component.strength <- strength   # strength of signal components
  
  f <- function(t, w, cs, cf, cd) { 
    ft <- dc.component + sum( cs * sin(cf*w*t + cd));
    return(ft);
  }
  
  plot.fourier(f,f.0,ts=ts,cs=component.strength, cf=component.freqs, cd=component.delay)
}

# for plotting
#par(mfrow=c(5,2))

# number of waves
reps <- 1:10

# empty object for the trajectories of the random waves
r_waves_traj <- NULL

# set seed
set.seed(123)

# loop through reps and produce a random wave for each rep
for(i in 1:length(reps)){
  
  f.0.rng <- seq(0.01,0.08,0.01)
  f.0 <- sample(f.0.rng, 1, replace = FALSE)
  
  dc.component <- 500
  freq  <- sample(1:5,3, replace = FALSE)
  freq1 <- freq[1]
  freq2 <- freq[2]
  freq3 <- freq[3]
  
  delay  <- sample(0:180,3, replace = FALSE)
  delay1 <- delay[1]
  delay2 <- delay[2]
  delay3 <- delay[3]
  
  str <- seq(1, 150, 0.2)
  strength1 <- sample(str,1)
  strength2 <- sample(str,1)
  strength3 <- sample(str,1)
  
  r_waves_traj[[i]] <- random_wave(f.0, dc.component, c(freq1,freq2,freq3), c(delay1,delay2,delay3), 
                                   c(strength1,strength2,strength3))
}  

# name the list elements
names <- c("MB5.1","MB5.2","MB5.3","MB5.4","MB5.5","MB5.6","MB5.7",
           "MB5.8","MB5.9","MB5.10")

names(r_waves_traj) <- names

# standardise to a total cumulative budget of 25,000
r_waves_traj <- lapply(r_waves_traj, function(x){25000*(x/sum(x))})

# extract to global environment
list2env(r_waves_traj, globalenv())




### RUN 1 - wave 1
UB  <- 2000
UBR <- 200

MB <- MB5.1[1]


Scen5_sim_old <- gmse_apply(
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
Scen5 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen5[time_step, 1] <- time_step
  Scen5[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen5[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen5[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen5[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen5[time_step, 6] <- UB
  Scen5[time_step, 7] <- MB
  
  Scen5_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB5.1[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_1_summary <- data.frame(Scen5)


### Wave 3

UB  <- 2000
UBR <- 200

MB <- MB5.3[1]


Scen5_sim_old <- gmse_apply(
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
Scen5 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen5[time_step, 1] <- time_step
  Scen5[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen5[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen5[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen5[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen5[time_step, 6] <- UB
  Scen5[time_step, 7] <- MB
  
  Scen5_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB5.3[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_3_summary <- data.frame(Scen5)



### Wave 5

UB  <- 2000
UBR <- 200

MB <- MB5.5[1]


Scen5_sim_old <- gmse_apply(
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
Scen5 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen5[time_step, 1] <- time_step
  Scen5[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen5[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen5[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen5[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen5[time_step, 6] <- UB
  Scen5[time_step, 7] <- MB
  
  Scen5_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB5.5[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_5_summary <- data.frame(Scen5)



### wave 7

UB  <- 2000
UBR <- 200

MB <- MB5.7[1]


Scen5_sim_old <- gmse_apply(
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
Scen5 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen5[time_step, 1] <- time_step
  Scen5[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen5[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen5[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen5[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen5[time_step, 6] <- UB
  Scen5[time_step, 7] <- MB
  
  Scen5_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB5.7[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_7_summary <- data.frame(Scen5)


### wave 9

UB  <- 2000
UBR <- 200

MB <- MB5.9[1]


Scen5_sim_old <- gmse_apply(
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
Scen5 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen5[time_step, 1] <- time_step
  Scen5[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen5[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen5[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen5[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen5[time_step, 6] <- UB
  Scen5[time_step, 7] <- MB
  
  Scen5_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB5.9[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_9_summary <- data.frame(Scen5)


scen5_summary_all <- rbind(Scen5_1_summary,Scen5_3_summary,Scen5_5_summary,Scen5_7_summary,Scen5_9_summary)


write.csv(scen5_summary_all, file="outputs/investment/scenarios/Run_4/scen5_summary.csv")