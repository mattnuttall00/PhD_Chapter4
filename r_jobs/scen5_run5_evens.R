require(GMSE)

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
r_waves_traj2 <- NULL

# set seed
set.seed(123)

# loop through reps and produce a random wave for each rep
for(i in 1:length(reps)){
  
  f.0.rng <- seq(0.01,0.2,0.01)
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
  
  str <- seq(1, 300, 1)
  strength1 <- sample(str,1)
  strength2 <- sample(str,1)
  strength3 <- sample(str,1)
  
  r_waves_traj2[[i]] <- random_wave(f.0, dc.component, c(freq1,freq2,freq3), c(delay1,delay2,delay3), 
                                    c(strength1,strength2,strength3))
}  

# name the list elements
names <- c("MB5.1","MB5.2","MB5.3","MB5.4","MB5.5","MB5.6","MB5.7",
           "MB5.8","MB5.9","MB5.10")

names(r_waves_traj2) <- names

# standardise to a total cumulative budget of 25,000
r_waves_traj2 <- lapply(r_waves_traj2, function(x){25000*(x/sum(x))})

# change all negative MB value to 0
r_waves_traj2 <- lapply(r_waves_traj2, function(x){ifelse(x<1,1,x)})

# extract to global environment
list2env(r_waves_traj2, globalenv())




### wave 2
UB  <- 2000
UBR <- 200

MB <- MB5.2[1]


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
  
  
  sim_new <- tryCatch(gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB),
                      error=function(err) NA)
  
  # if the trees go extinct
  if(is.na(sim_new) == TRUE){
    
    Scen5[time_step, 1] <- time_step
    Scen5[time_step, 2] <- 0
    Scen5[time_step, 3] <- 0
    Scen5[time_step, 4] <- NA
    Scen5[time_step, 5] <- NA
    Scen5[time_step, 6] <- UB
    Scen5[time_step, 7] <- MB
    
    print("Extinction")
    
    break
  }
    
  # if tree do not go extinct
  if(class(sim_new)=="list"){
    
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
  MB <- MB5.2[time_step]
  }

}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_2_summary <- data.frame(Scen5)



### wave 4
UB  <- 2000
UBR <- 200

MB <- MB5.4[1]


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
  
  
  sim_new <- tryCatch(gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                                 usr_budget_rng = UBR, manager_budget = MB),
                      error=function(err) NA)
  
  # if the trees go extinct
  if(is.na(sim_new) == TRUE){
    
    Scen5[time_step, 1] <- time_step
    Scen5[time_step, 2] <- 0
    Scen5[time_step, 3] <- 0
    Scen5[time_step, 4] <- NA
    Scen5[time_step, 5] <- NA
    Scen5[time_step, 6] <- UB
    Scen5[time_step, 7] <- MB
    
    print("Extinction")
    
    break
  }
  
  # if tree do not go extinct
  if(class(sim_new)=="list"){
    
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
    MB <- MB5.4[time_step]
  }
  
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_4_summary <- data.frame(Scen5)



### wave 6
UB  <- 2000
UBR <- 200

MB <- MB5.6[1]


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
  
  
  sim_new <- tryCatch(gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                                 usr_budget_rng = UBR, manager_budget = MB),
                      error=function(err) NA)
  
  # if the trees go extinct
  if(is.na(sim_new) == TRUE){
    
    Scen5[time_step, 1] <- time_step
    Scen5[time_step, 2] <- 0
    Scen5[time_step, 3] <- 0
    Scen5[time_step, 4] <- NA
    Scen5[time_step, 5] <- NA
    Scen5[time_step, 6] <- UB
    Scen5[time_step, 7] <- MB
    
    print("Extinction")
    
    break
  }
  
  # if tree do not go extinct
  if(class(sim_new)=="list"){
    
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
    MB <- MB5.6[time_step]
  }
  
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_6_summary <- data.frame(Scen5)




### wave 8
UB  <- 2000
UBR <- 200

MB <- MB5.8[1]


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
  
  
  sim_new <- tryCatch(gmse_apply(get_res = "Full", old_list = Scen5_sim_old, user_budget=UB, 
                                 usr_budget_rng = UBR, manager_budget = MB),
                      error=function(err) NA)
  
  # if the trees go extinct
  if(is.na(sim_new) == TRUE){
    
    Scen5[time_step, 1] <- time_step
    Scen5[time_step, 2] <- 0
    Scen5[time_step, 3] <- 0
    Scen5[time_step, 4] <- NA
    Scen5[time_step, 5] <- NA
    Scen5[time_step, 6] <- UB
    Scen5[time_step, 7] <- MB
    
    print("Extinction")
    
    break
  }
  
  # if tree do not go extinct
  if(class(sim_new)=="list"){
    
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
    MB <- MB5.8[time_step]
  }
  
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_8_summary <- data.frame(Scen5)



scen5_summary_even <- rbind(Scen5_2_summary,Scen5_4_summary,Scen5_6_summary,Scen5_8_summary)

write.csv(scen5_summary_even, file="outputs/investment/scenarios/Run_5/scen5_summary_even.csv")