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

# Thresh2 does not find the point where the manager loses all power, because the cull count continues to rise right up until the last time step. That means that the user is still gaining power over the manager, right up to that point. I think I am looking for the point where the cull count levels off, as this suggests the users are 



### Plots - thresh1 & 2 ####

thresh1_summary <- read.csv("outputs/investment/null_scenarios/budget_thresholds/thresh1_summary.csv", header = T)
thresh2_summary <- read.csv("outputs/investment/null_scenarios/budget_thresholds/thresh2_summary.csv", header = T)


# merge dataframes
thresh1_summary$Sim <- "Thresh1_manager"
thresh1_summary <- thresh1_summary %>% rename(Budget = Manager_budget)
thresh2_summary$Sim <- "Thresh2_user"
thresh2_summary <- thresh2_summary %>% rename(Budget = User_budget) 
thresh_all_summary <- rbind(thresh1_summary, thresh2_summary) 


p.cull_count <- ggplot(thresh_all_summary, aes(x=Time, y=Cull_count, group=Sim, color=Sim))+
                geom_line(size=3)+
                theme_classic()+
                theme(axis.text = element_text(size=15),
                      axis.title = element_text(size=15),
                      legend.text = element_text(size=12),
                      legend.title = element_text(size=12))

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

p.cost <- ggplot(thresh_all_summary, aes(x=Cull_cost, y=Cull_count, group=Sim, color=Sim))+
          geom_line(size=3)+        
          theme_classic()+
          theme(axis.text = element_text(size=15),
                axis.title = element_text(size=15),
                legend.text = element_text(size=12),
                legend.title = element_text(size=12))

(p.cull_count + p.budget) / p.cost

# Ok, so when the manager budget is varied (thresh1), we see that around time step 25 the user loses all ability to cull trees. There is some very minor culling up to time step 44, but it's fairly insignificant. The point at which the majority of culling stops, is when the manager budget is 1300. So when the manager budget is about 130% of the user budget (or the user budget is ~ 75-77% of the manager budget).

# When the user budget is varied there is little culling at all until around time step 14, when the user budget is 700 (so 70% of the manager budget). This matches the above. I am not sure exactly where the manager loses all power, but I am assuming it is the point at which they stop varying the cost of culling (i.e. they have fixed the cost of culling at the maximum). This is time step 25, where the user budget is 1250. This is where the manager budget is 80% of the user budget. 

# Brad mentioned that he viewed the point of the manager losing all power as the resource extinction, but seeing as in this landscape set up (millions of trees, few users), this is never going to happen. Myself and Nils think that in terms of mapping to reality, the point at which the manager effectively loses all power is when they are maxing out their budget on setting as high a cost as possible, but it is not really having any effect and they stop trying to adaptively manage. This is represented in the above as around time step 25, when the user budget is 1250. 


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

### Poisson - T2 ####

# here I am going to use a Poisson distribution/process to create "events" in the manager's budget. I guess an event will be a spike in the budget, or perhaps the transition from high budget to low budget and vice versa? Because I need the AUC to be approximately the same as the other scenarios, I need the number of events (or peaks and troughs) to be approximately the same otherwise the AUC will be very different.

# Either I could fix the height of the peaks (i.e. the max budget value during a peak period) to be the same as T1 (i.e. regular) and then just vary the timestep in which the event occurs, or I could also allow the height of the peaks to vary, with higher peaks being more unlikely, and lower peaks being more likely. 

# see here for where I got the below code from: https://stats.stackexchange.com/questions/59823/how-can-i-generate-events-using-the-poisson-distribution-in-r

plot(cumsum(rexp(50, rate=13)))
