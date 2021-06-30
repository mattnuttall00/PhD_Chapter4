## This script is to establish the upper and lower bounds for the manager and user budgets. Here I will use simulations to establish where the thresholds are below which the user/manager loses all power (i.e. user/manger budget is too low relative to the other actor's budget budget)


### Load libraries ####

library('tidyverse')
library('GMSE')
library('patchwork')
library('MESS')


### thresh1 - user budget static, manager budget varying ####

## Here I will fix the user budget at 1000, and then vary the manager budget from very low (50) to very high (4000) over a long time period to see if we can see the thresholds.

MB  <- 50

thresh1_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 100,
  land_dim_1 = 150,
  land_dim_2 = 150, # landscape is 22,500ha or 22.5km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 150, 
  agent_move = 50, 
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 0, # no natural death 
  observe_type = 2, # transect 
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
  user_budget = 1000, 
  manager_budget = 50, 
  usr_budget_rng = 100, # introduce variation around the mean user budget (removes step pattern) 
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
thresh1 <- matrix(data=NA, nrow=100, ncol=6)

# loop the simulation. 
for(time_step in 1:100){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = thresh1_sim_old, manager_budget=MB)
  
  thresh1[time_step, 1] <- time_step
  thresh1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  thresh1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  thresh1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  thresh1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  thresh1[time_step, 6] <- MB
  
  thresh1_sim_old <- sim_new
  MB <- MB + 50
  
}

colnames(thresh1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count","Manager_budget")

thresh1_summary <- data.frame(thresh1)

write.csv(thresh1_summary, file = "outputs/investment/null_scenarios/budget_thresholds/thresh1_summary.csv")


# Loads saved results from Thresh1
thresh1_summary <- read.csv("outputs/investment/null_scenarios/budget_thresholds/thresh1_summary.csv", header = T)


### thresh2 - manager budget static, user budget varying ####

### here I will fix the manager budget at 1000 and vary the user budget

UB  <- 50
UBR <- UB/5

thresh2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 100,
  land_dim_1 = 150,
  land_dim_2 = 150, # landscape is 22,500ha or 22.5km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 150, 
  agent_move = 50, 
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 0, # no natural death 
  observe_type = 2, # transect 
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
  user_budget = UB, 
  manager_budget = 1000, 
  usr_budget_rng = UBR, # introduce variation around the mean user budget (removes step pattern) 
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
thresh2 <- matrix(data=NA, nrow=100, ncol=6)

# loop the simulation. 
for(time_step in 1:100){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = thresh2_sim_old, user_budget=UB, usr_budget_rng = UBR)
  
  thresh2[time_step, 1] <- time_step
  thresh2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  thresh2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  thresh2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  thresh2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  thresh2[time_step, 6] <- UB
  
  thresh2_sim_old <- sim_new
  UB <- UB + 50
  UBR <- UB/5
  
}

colnames(thresh2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count","User_budget")

thresh2_summary <- data.frame(thresh2)

#write.csv(thresh2_summary, file = "outputs/investment/null_scenarios/budget_thresholds/thresh2_summary.csv")

# load saved results summary for thresh2
thresh2_summary <- read.csv("outputs/investment/null_scenarios/budget_thresholds/thresh2_summary.csv", header = T)

# I think the point at which the manager loses power over the user is when the cost of culling is maxes out, and is no longer varied. This means that the manager is throwing all resources into stopping culling, at every time step, and is no longer attempting any kind of adaptive management.  



### thresh3 - as thresh1 but larger absolute budget value ####

## Here I will fix the user budget at 5000, and then vary the manager budget from very low (100) to very high (10,000) over a long time period to see if we can see the thresholds.

MB  <- 100

thresh3_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 100,
  land_dim_1 = 150,
  land_dim_2 = 150, # landscape is 22,500ha or 22.5km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 150, 
  agent_move = 50, 
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 0, # no natural death 
  observe_type = 2, # transect 
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
  user_budget = 5000, 
  manager_budget = MB, 
  usr_budget_rng = 100, # introduce variation around the mean user budget (removes step pattern) 
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
thresh3 <- matrix(data=NA, nrow=100, ncol=6)

# loop the simulation. 
for(time_step in 1:100){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = thresh3_sim_old, manager_budget=MB)
  
  thresh3[time_step, 1] <- time_step
  thresh3[time_step, 2] <- sim_new$basic_output$resource_results[1]
  thresh3[time_step, 3] <- sim_new$basic_output$observation_results[1]
  thresh3[time_step, 4] <- sim_new$basic_output$manager_results[3]
  thresh3[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  thresh3[time_step, 6] <- MB
  
  thresh3_sim_old <- sim_new
  MB <- MB + 100
  
}

colnames(thresh3) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count","Manager_budget")

thresh3_summary <- data.frame(thresh3)

write.csv(thresh3_summary, file = "outputs/investment/null_scenarios/budget_thresholds/thresh3_summary.csv")



### thresh4 - as thresh2 but larger absolute budget value ####

# this is the same as thresh2 but with a manager budget of 5000 and a user budget ranging from 100 to 10,1000

UB  <- 100
UBR <- UB/5

thresh4_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 100,
  land_dim_1 = 150,
  land_dim_2 = 150, # landscape is 22,500ha or 22.5km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 150, 
  agent_move = 50, 
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 0, # no natural death 
  observe_type = 2, # transect 
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
  user_budget = UB, 
  manager_budget = 5000, 
  usr_budget_rng = UBR, # introduce variation around the mean user budget (removes step pattern) 
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
thresh4 <- matrix(data=NA, nrow=100, ncol=6)

# loop the simulation. 
for(time_step in 1:100){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = thresh4_sim_old, user_budget=UB, usr_budget_rng = UBR)
  
  thresh4[time_step, 1] <- time_step
  thresh4[time_step, 2] <- sim_new$basic_output$resource_results[1]
  thresh4[time_step, 3] <- sim_new$basic_output$observation_results[1]
  thresh4[time_step, 4] <- sim_new$basic_output$manager_results[3]
  thresh4[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  thresh4[time_step, 6] <- UB
  
  thresh4_sim_old <- sim_new
  UB <- UB + 100
  UBR <- UB/5
  
}

colnames(thresh4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count","User_budget")

thresh4_summary <- data.frame(thresh4)

#write.csv(thresh2_summary, file = "outputs/investment/null_scenarios/budget_thresholds/thresh2_summary.csv")

### Plots - thresh1, 2, 3, 4 ####

thresh1_summary <- read.csv("outputs/investment/null_scenarios/budget_thresholds/thresh1_summary.csv", header = T)
thresh2_summary <- read.csv("outputs/investment/null_scenarios/budget_thresholds/thresh2_summary.csv", header = T)
thresh3_summary <- read.csv("outputs/investment/null_scenarios/budget_thresholds/thresh3_summary.csv", header = T)
thresh4_summary <- read.csv("outputs/investment/null_scenarios/budget_thresholds/thresh4_summary.csv", header = T)

# merge dataframes
thresh1_summary$Sim <- "Thresh1_manager"
thresh1_summary <- thresh1_summary %>% rename(Budget = Manager_budget)
thresh2_summary$Sim <- "Thresh2_user"
thresh2_summary <- thresh2_summary %>% rename(Budget = User_budget) 
thresh_12_summary <- rbind(thresh1_summary, thresh2_summary)

thresh3_summary$Sim <- "Thresh3_manager"
thresh3_summary <- thresh3_summary %>% rename(Budget = Manager_budget)
thresh4_summary$Sim <- "Thresh4_user"
thresh4_summary <- thresh4_summary %>% rename(Budget = User_budget) 
thresh_34_summary <- rbind(thresh3_summary, thresh4_summary)


p.cull_count_12 <- ggplot(thresh_12_summary, aes(x=Time, y=Cull_count, group=Sim, color=Sim))+
                geom_line(size=3)+
                theme_classic()+
                theme(axis.text = element_text(size=15),
                      axis.title = element_text(size=15),
                      legend.text = element_text(size=12),
                      legend.title = element_text(size=12))

p.cull_count_34 <- ggplot(thresh_34_summary, aes(x=Time, y=Cull_count, group=Sim, color=Sim))+
                    geom_line(size=3)+
                    theme_classic()+
                    theme(axis.text = element_text(size=15),
                          axis.title = element_text(size=15),
                          legend.text = element_text(size=12),
                          legend.title = element_text(size=12))

p.cull_count_12 + p.cull_count_34

# dataframe for the static budget
static_budget <- data.frame(Time = rep(1:100, times=2),
                            Budget = rep(1000, times=200),
                            Sim = rep(c("Thresh1_manager","Thresh2_manager"), each=100))

p.budget <- ggplot(thresh_all_summary, aes(x=Time, y=Budget, group=Sim, color=Sim))+
            geom_line(size=3)+
            geom_line(data=static_budget, aes(x=Time, y=Budget), linetype="dashed", size=3)+
            theme_classic()+
            theme(axis.text = element_text(size=15),
                  axis.title = element_text(size=15),
                  legend.position = "none")

p.cost_12 <- ggplot(thresh_12_summary, aes(x=Cull_cost, y=Cull_count, group=Sim, color=Sim))+
              geom_line(size=3)+        
              theme_classic()+
              theme(axis.text = element_text(size=15),
                    axis.title = element_text(size=15),
                    legend.text = element_text(size=12),
                    legend.title = element_text(size=12))

p.cost_34 <- ggplot(thresh_34_summary, aes(x=Cull_cost, y=Cull_count, group=Sim, color=Sim))+
              geom_line(size=3)+        
              theme_classic()+
              theme(axis.text = element_text(size=15),
                    axis.title = element_text(size=15),
                    legend.text = element_text(size=12),
                    legend.title = element_text(size=12))

(p.cull_count_12 + p.cull_count_34) / (p.cost_12 + p.cost_34)

# Ok, so when the manager budget is varied (thresh1 and thresh3), we see that around time step 25 the user loses most of their ability to cull trees. This is less certain in thresh3, as the drop off in cull count is slower than in thresh1. There is some very minor culling right to the end of the simulation in thresh3, but it is very minor towards the end. For thresh1 the point at which the majority of culling stops (cull count into singel figures), is when the manager budget is 1300. So when the manager budget is about 130% of the user budget (or the user budget is ~ 75-77% of the manager budget). The cull count in thresh4 never drops into single figures, but the point at whcih it drops below 100 is time step 63 when the manager budget is 6300, or 126% of the user budget (or the user budget is 80% of the manager budget). The interesting thing to note is that the total number of trees lost is dramatically larger in thresh3. This will be because of the relative differences in manager and user budget - it thresh1 the manager budget starts off at 5% of the user budget, whereas in thresh3 the manager budget starts off at 2%, and so the manager has less power.   

# When the user budget is varied there is little culling at all until around time step 14 (thresh2) and time step 19 (thresh4), when the user budget is 700 (so 70% of the manager budget), and 1900 (38% of the manager budget). I am not sure exactly where the manager loses all power, but I am assuming it is the point at which they stop varying the cost of culling (i.e. they have fixed the cost of culling at the maximum). For thresh2 this is time step 25, where the user budget is 1250. This is where the manager budget is 80% of the user budget. For thresh4 this actually never quite happens, but most of the cost variation stops around time step 50, when the user and manager budgets are equal. 

# Brad mentioned that he viewed the point of the manager losing all power as the resource extinction, but seeing as in this landscape set up (millions of trees, few users), this is never going to happen. Myself and Nils think that in terms of mapping to reality, the point at which the manager effectively loses all power is when they are maxing out their budget on setting as high a cost as possible, but it is not really having any effect and they stop trying to adaptively manage.  


### sine wave - T1 ####

a <- seq(0,25,length.out=100)
b <- sin(a)
plot(a,b,type="l")

c <- seq(0,50,length.out=100)
d <- sin(c)
plot(c,d,type="l")
# so changing the values of the sequence changes the frequency

e <- seq(50,100,length.out=50)
f <- sin(e)
plot(e,f,type = "l")
# changing the number of values just changes the smoothness of the line (number of points)

g <- seq(0,20,length.out=100)
h <- sin(g)
plot(g,h,type = "l")

# change the amplitude
i <- seq(0,20,length.out=100)
j <- 2*sin(i)
plot(i,j,type="l")


# try to make the y values positive
# need to use the equation y = a sin (bx +c) + d, where a=1, b=1, and c=0, d=2
k <- seq(0,20,length.out=100)
l <- 1*sin(1*k+0)+1
plot(k,l,type="l")

# Now increase amplitude
m <- seq(0,20,length.out=100)
n <- 1000*sin(1*m+0)+1000
plot(m,n,type="l")

# change x axis to 0:50 (time steps), and change y axis so the minimum is 500
o <- seq(0,50,length.out=100)
p <- 500*sin(1*o+0)+1000
plot(o,p,type="l", ylim = c(0,1600))

# calculate the area under the curve
auc(o,p)


# reduce to 50 data points
q <- seq(0,50,length.out=50)
r <- 500*sin(1*q+0)+1000
plot(q,r,type="l", ylim = c(0,1600))


## experiment with allocating manager budget values from the above sin wave

# create very small, simple landscape to speed up run time

MB  <- 1000

sin1_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 20,
  land_dim_2 = 20, 
  res_movement = 0, 
  remove_pr = 0,
  lambda = 0, 
  agent_view = 20, 
  agent_move = 20, 
  res_birth_K = 1, 
  res_death_K = 5000000, 
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
  user_budget = 1000, 
  manager_budget = MB, 
  usr_budget_rng = 100, 
  manage_target = 4000, 
  RESOURCE_ini = 4000, 
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
sin1 <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = sin1_sim_old, manager_budget=MB)
  
  sin1[time_step, 1] <- time_step
  sin1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  sin1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  sin1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  sin1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  sin1[time_step, 6] <- MB
  
  sin1_sim_old <- sim_new
  MB <- r[time_step]
  
}

colnames(sin1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count",
                   "Manager_budget")
sin1 <- data.frame(sin1)

plot(sin1$Time, sin1$Manager_budget, type = "l")

## Fourier transform ####

# tutorial found here: http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html

# Function for plotting trajectories given a fourier series
plot.fourier <- function(fourier.series, f.0, ts) {
  w <- 2*pi*f.0
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}


## function to create Inverse Fourier Transform
# returns the x.n time series for a given time sequence (ts) and
# a vector with the amount of frequencies k in the signal (X.k)
get.trajectory <- function(X.k,ts,acq.freq) {
  
  N   <- length(ts)
  i   <- complex(real = 0, imaginary = 1)
  x.n <- rep(0,N)           # create vector to keep the trajectory
  ks  <- 0:(length(X.k)-1)
  
  for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
    x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
  }
  
  x.n * acq.freq 
}

# function to plot a frequenc spectrum of a given Xk
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Function to plot the i-th harmonic on the current plot
# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}


# example from the tutorial:
acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time                      # f.0 is the fundamental frequency of the complex wave

dc.component <- 1
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,0.5,0.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)


## increase the time interval to 50
acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 50                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time                      # f.0 is the fundamental frequency of the complex wave

dc.component <- 1
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,0.5,0.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)


## change the fundamental frequency (f.0)
acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 50                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 3/time                      # f.0 is the fundamental frequency of the complex wave

dc.component <- 1
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,0.5,0.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)
## increasing f.0 increases the number of peaks



## change the dc.component 
acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 50                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time                      # f.0 is the fundamental frequency of the complex wave

dc.component <- 500                   # additive constant signal
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,0.5,0.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)
## the DC component is a constant that affects the y axis



## Now I will mess with the components
acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 50                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time                      # f.0 is the fundamental frequency of the complex wave

dc.component <- 500                   # additive constant signal
component.freqs <- c(2,5,3)        # frequency of signal components (Hz)
component.delay <- c(30,75,0)         # delay of signal components (radians)
component.strength <- c(1.5,0.5,0.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)
## Tried lots of different combinations, and you can produce all sorts of patterns



## now fiddle with the delays
acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 50                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time                      # f.0 is the fundamental frequency of the complex wave

dc.component <- 500                   # additive constant signal
component.freqs <- c(5,7,3)        # frequency of signal components (Hz)
component.delay <- c(35,0,20)         # delay of signal components (radians)
component.strength <- c(2.5,2.5,5.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)
# Messing with the delays produces some nice random peaks.


# so I think I need to work out a function that essentially samples random numbers (within bounds) to populate the component frequencies and delays. Then in the function have a test whereby the AUC is calculated and the only ones that are saved are the ones that have approximately the correct AUC.


random_wave <- function(f.0, dc.component, freq, delay, strength){
  
  acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
  time     <- 50                      # measuring time interval (seconds)
  ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
  f.0 <- f.0                      # f.0 is the fundamental frequency of the complex wave
  
  dc.component <- dc.component                   # additive constant signal
  component.freqs <- freq          # frequency of signal components (Hz)
  component.delay <- delay         # delay of signal components (radians)
  component.strength <- strength   # strength of signal components
  
  f   <- function(t,w) { 
    dc.component + 
      sum( strength * sin(freq*w*t + delay)) 
  }
  
  plot.fourier(f,f.0,ts=ts)
}

# test
random_wave(1/time, 500, c(5,7,3), c(35,0,20), c(1.5,0.5,0.75))


# try multiple iterations
par(mfrow=c(4,2))
reps <- 1:8

for(i in 1:length(reps)){
  
  f.0 <- 0.5/50
  dc.component <- 500
  freq  <- sample(1:15,3, replace = FALSE)
  freq1 <- freq[1]
  freq2 <- freq[2]
  freq3 <- freq[3]
  
  delay  <- sample(-180:180,3, replace = FALSE)
  delay1 <- delay[1]
  delay2 <- delay[2]
  delay3 <- delay[3]
  
  str <- seq(0.25, 7.5, 0.2)
  strength1 <- sample(str,1)
  strength2 <- sample(str,1)
  strength3 <- sample(str,1)
  
  random_wave(f.0, dc.component, c(freq1,freq2,freq3), c(delay1,delay2,delay3), 
              c(strength1,strength2,strength3))
}



 #
### Poisson ####

# here I am going to use a Poisson distribution/process to create "events" in the manager's budget. I guess an event will be a spike in the budget, or perhaps the transition from high budget to low budget and vice versa? Because I need the AUC to be approximately the same as the other scenarios, I need the number of events (or peaks and troughs) to be approximately the same otherwise the AUC will be very different.

# Either I could fix the height of the peaks (i.e. the max budget value during a peak period) to be the same as T1 (i.e. regular) and then just vary the timestep in which the event occurs, or I could also allow the height of the peaks to vary, with higher peaks being more unlikely, and lower peaks being more likely. 



q <- seq(0,50,length.out=50)
r <- 500*sin(1*q+0)+1000
plot(q,r,type="l", ylim = c(0,1600))

# These are ways of just generating random numbers drawn from either Poisson or normal distributions, or random sampling
budget <- rpois(n = 1, lambda = r)
budget  <- floor( rnorm(n = 1, mean = r, sd = 500) )
budget <- sample(x = 1000:20000, size = 1)


# this creates random noise around the sin wave
xx <- seq(from = 0, to = 2*3.14, by = 0.1)
yy <- sin(xx)
plot(xx, yy + runif(n = length(xx), min = -0.1, max = 0.1), type = "l")
plot(xx, yy + runif(n = length(xx), min = -0.4, max = 0.4), type = "l")
plot(xx, 10000 + floor(1000 * (yy + runif(n = length(xx), min = -0.4, max = 0.4))), type = "l")


# this changes the wavelength over time
# changing the number that xx is multiplied by in the definition of yy changes the number of peaks. Changing the power fraction of xx changes the amount the frequency changes by. Smaller fractions change the frequency by more
xx <- seq(from = 0, to = 8*pi, by = 0.1);
yy <- sin(6*xx^(3/10));
plot(xx, yy, type = "l");

# change the y values to positive, and the x values 1:50
xx <- seq(from = 0, to = 50, length.out=50)
yy <- 500*sin(6*xx^(3/10))+1000
plot(xx, yy, type = "l")


# this was the final sine wave, but I have made the funding peaks every 5 years, resulting in a total of 10 peaks
q <- seq(0,60,length.out=50)
r <- 500*sin(1*q+0)+1000
plot(q,r,type="l", ylim = c(0,1600))

# now I want to try and create a wave with varying wavelength that also has 10 peaks. Althoug this doesnt work - I cant have the same number of peaks and varying wavelengths. The thing that has to be the same is the AUC
xx <- seq(from = 0, to = 60, length.out=50)
yy <- 500*sin(10*xx^(3/10))+1000
plot(xx, yy, type = "l")

xx <- seq(from = 0, to = 60, length.out=50)
yy <- 500*sin(10*xx^(2/10))+1000
plot(xx, yy, type = "l")



yr <- sample(1:50, 10, replace = F)
yr <- sort(yr)

# dataframe of budget values
df <- data.frame(time = 1:50,
                 mean = rep(500, times=50),
                 y = NA)

# change peak values based on random sample
for(i in 1:length(df$time)){
  if(df$time[i] %in% yr){
    df$y[i] <- 1000 
  } else {
    df$y[i] <- df$y[i]
  }
}

# Create vector for the values either side of the peak values
peak_sides <- vector()
 for(i in 1:length(yr)){
 a <- yr[i]-1
 b <- yr[i]+1
 d <- c(a,b)
 peak_sides <- c(peak_sides,d)
 }

# change values for elements either side of the peaks
for(i in 1:length(df$time)){
  if(df$time[i] %in% peak_sides){
    df$y[i] <- 750 
  } else {
    df$y[i] <- df$y[i]
  }
}

df$y <- ifelse(is.na(df$y),500,df$y)

plot(df$time, df$y, type="l", ylim=c(0,1200))

#
### Setting AUC for different budgets ####

# the below is from Brad and is a way to make sure the AUC for all manager budgets are the same

# Making up some different budgets over 50 time steps
budget1 <- round(1000 + 500*sin(1:50)); # Sine wave
budget2 <- rep(1000, 50); # Constant
budget3 <- exp(-(1:50));  # Exponentially decreasing
budget4 <- exp(1:50)      # Exponentially increasing
budget5 <- 1:50           # Linearly increasing
# These have different budgets; I just fixed the shape

std_1 <- 1000 * (budget1 / sum(budget1));
std_2 <- 1000 * (budget2 / sum(budget2));
std_3 <- 1000 * (budget3 / sum(budget3));
std_4 <- 1000 * (budget4 / sum(budget4));
std_5 <- 1000 * (budget5 / sum(budget5));
# Now all of them have the same total budget 1000 over 50 time steps



