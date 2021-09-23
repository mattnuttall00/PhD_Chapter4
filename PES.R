### This script is for the second part of my final PhD chapter. The analysis is investigating the potential effects of PES on the system dynamics of a social-ecological system. I will be using the same landscape and model set up as in part A - i.e., a landscape with farmers, where the human population is increasing, and thus the human pressure on the lsandscape is increasing (via the user budget). I will also start by testing the addition of PES payments on the investment scenarios from part A. 

### Load libraries ####

library('tidyverse')
library('GMSE')
library('patchwork')
library('viridis')
library('scales')
library('devtools')



############## LANDSCAPE DETAILS ##################

# Study period- 50 years

# Landscape dimensions - 100 x 100 cells.  which results in 10,000 cells (or ha). With 30 villages, this results in 333.33 ha or 3.33km2 per village. The number of trees is 100,000

# There is no public land. It does not serve any purpose in this study.

# "Resources" are trees. The resources therefore do not move.

# The density of trees in tropical forest landscapes vary hugely. In previous simulations I have assumed 50 stems/ha which is low, but not implausible (e.g. deciduous dipterocarp woodland). A reference for this value can be found here:https://www.jstor.org/stable/44521915. This resulted in >1M trees, which meant that the differences in trees lost between scenarios, and the total number of trees lost, were too low. In the final scenarios, the number of trees was set to 100,000. Note that the trees are distributed randomly across the landscape, and so there will not be exactly 50/cell. This reflects reality. 

# Trees in a cell reduce the farmer's yield. The amount a tree reduces yield is governed by an exponential function: yield = (1 - % yield reduction per tree) ^ remaining trees. I want a farmer's yield to be reduced by a significant amount if all trees in a cell are standing. But the trees do not completely eliminate yield. This is a balance between the farmer being able to farm and gain some yield even when there are trees on their cell, but also providing an incentive to cull where possible. I have set each tree on a cell to reduce yield by 8%. See the N1:N1f scenario comparisons which were used to decide on the parameter values for res_consume and tendcrop_yield

# The amount a user can increase their yield by tending crops is governed by tend_crop_yld. I have set this at 0.01 (1%) which means they can increase their yield on a cell by 1% in a single time step if they choose to tend crops. This is lower than the yield gain they would make if they felled some trees. This is set up so that there is an incentive to fell trees and expand their farmland, as it will increase their yield. See the N1:N1f scenario comparisons which were used to decide on the parameter values for res_consume and tendcrop_yield  

# For simplicity, I am assuming there is no natural death and no natural birth (forest regeneration). remove_pr is set to 0, and lambda is set to 0, and res_death_type is set to 0 (new value created by Brad that means no natural death at all) 

# The carrying capacity of new resources is set to 1 as it has to be a positive number but I want it as low as possible i.e. there is no real recruitment

# res_death_type is set to 0 (no natural death).

# The observation process is set to density-based sampling, with 1 observation per time step. The manager can move in any direction. Currently the manager can see 150 cells, and move 50 cells. We decided to remove any observation error. 

# There is no minimum age for resources to be acted upon i.e. all trees in the landscape can be observed/culled

# Agents are permitted to move at the end of each time step. Because land_ownership==TRUE I believe this then only relates to the manager.

# User and manager budgets will vary based on the scenario. But the total amount of budget available to the manager for the whole study period will be the same. The total is 25,000. See the "scenario_details_budgets" spreadsheet  

# Group_think == FALSE, and so users act independently

# Both culling and feeding are allowed. Culling is cutting down trees to increase yield. Feeding is going to be the hack to introduce PES. I will use the parameter perceive_feed to alter the way the users view feeding of the resource. Normally, feeding will increase the population of the resource, and therefore usually, in these circumstances, the users would not want that because resources reduce their yield. But we can set perceive_feed to a negative value, which will make the user think that feeding actually reduces the resources (i.e., it is beneficial to them). Therefore, the manager can use some of their budget to set the cost of feeding low, and depending on the value of perceive_feed, the users may choose to "feed" (i.e., collect a PES), rather than culling. So it is a way of offering the users an alternative option to culling.  

# farming is allowed (tend_crops==TRUE, i.e. farmers can increase their yield by tending their crops rather than felling trees)



### Test 1:3 ####

# I will start with the code that Brad sent on GitHub (https://github.com/ConFooBio/gmse/issues/64). This is just to see how it runs

### no feeding
Test_1 <- gmse_apply(land_dim_1 = 150, land_dim_2 = 150, stakeholders = 20,
                   land_ownership = TRUE, res_move_type = 0, res_death_type = 0,
                   lambda = 0, res_consume = 0.08, RESOURCE_ini = 10000,
                   manage_target = 10000, feeding = FALSE)

Test_1$manager_results
Test_1$user_results

### perceive_feed set low 
Test_2 <- gmse_apply(land_dim_1 = 150, land_dim_2 = 150, stakeholders = 20,
                   land_ownership = TRUE, res_move_type = 0, res_death_type = 0,
                   lambda = 0, res_consume = 0.08, RESOURCE_ini = 10000,
                   manage_target = 10000, feeding = TRUE, perceive_feed = -0.1)

Test_2$manager_results
Test_2$user_results
# Users choose to feed once - not really very popular

### perceive_feed set high
Test_3 <- gmse_apply(land_dim_1 = 150, land_dim_2 = 150, stakeholders = 20,
                   land_ownership = TRUE, res_move_type = 0, res_death_type = 0,
                   lambda = 0, res_consume = 0.08, RESOURCE_ini = 10000,
                   manage_target = 10000, feeding = TRUE, perceive_feed = -1.0)

Test_3$manager_results
Test_3$user_results
# Users now choose to take the PES payments instead of culling


### Test 4 ####

# now I will run it over multiple time steps, with a static perceive_feed, set at -0.5

Test_4_sim_old <- gmse_apply(
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
  perceive_feed = -0.5,
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Test_4 <- matrix(data=NA, nrow=30, ncol=7)

# loop the simulation. 
for(time_step in 1:30){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Test_4_sim_old)
  
  Test_4[time_step, 1] <- time_step
  Test_4[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Test_4[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Test_4[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Test_4[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Test_4[time_step, 6] <- sim_new$basic_output$manager_results[5]
  Test_4[time_step, 7] <- sum(sim_new$basic_output$user_results[,5])

  
  Test_4_sim_old <- sim_new
  print(time_step)
}

colnames(Test_4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "Feed_cost", "Feed_count")
Test_4_summary <- data.frame(Test_4)
write.csv(Test_4_summary, file="outputs/pes/test_runs/Test_4_summary.csv")


### Test 5 ####

# now I will run it over multiple time steps, with a static perceive_feed, set at -0.4

Test_5_sim_old <- gmse_apply(
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
  perceive_feed = -0.4,
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Test_5 <- matrix(data=NA, nrow=30, ncol=7)

# loop the simulation. 
for(time_step in 1:30){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Test_5_sim_old)
  
  Test_5[time_step, 1] <- time_step
  Test_5[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Test_5[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Test_5[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Test_5[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Test_5[time_step, 6] <- sim_new$basic_output$manager_results[5]
  Test_5[time_step, 7] <- sum(sim_new$basic_output$user_results[,5])
  
  
  Test_5_sim_old <- sim_new
  print(time_step)
}

colnames(Test_5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "Feed_cost", "Feed_count")
Test_5_summary <- data.frame(Test_5)
write.csv(Test_5_summary, file="outputs/pes/test_runs/Test_5_summary.csv")


### Test 6 ####

# now I will run it over multiple time steps, with a static perceive_feed, set at -0.3

Test_6_sim_old <- gmse_apply(
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
  perceive_feed = -0.3,
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Test_6 <- matrix(data=NA, nrow=30, ncol=7)

# loop the simulation. 
for(time_step in 1:30){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Test_6_sim_old)
  
  Test_6[time_step, 1] <- time_step
  Test_6[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Test_6[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Test_6[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Test_6[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Test_6[time_step, 6] <- sim_new$basic_output$manager_results[5]
  Test_6[time_step, 7] <- sum(sim_new$basic_output$user_results[,5])
  
  
  Test_6_sim_old <- sim_new
  print(time_step)
}

colnames(Test_6) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "Feed_cost", "Feed_count")
Test_6_summary <- data.frame(Test_6)
write.csv(Test_6_summary, file="outputs/pes/test_runs/Test_6_summary.csv")


### Test 7 ####

# now I will run it over multiple time steps, with a static perceive_feed, set at -0.2

Test_7_sim_old <- gmse_apply(
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
  perceive_feed = -0.2,
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Test_7 <- matrix(data=NA, nrow=30, ncol=7)

# loop the simulation. 
for(time_step in 1:30){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Test_7_sim_old)
  
  Test_7[time_step, 1] <- time_step
  Test_7[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Test_7[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Test_7[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Test_7[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Test_7[time_step, 6] <- sim_new$basic_output$manager_results[5]
  Test_7[time_step, 7] <- sum(sim_new$basic_output$user_results[,5])
  
  
  Test_7_sim_old <- sim_new
  print(time_step)
}

colnames(Test_7) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "Feed_cost", "Feed_count")
Test_7_summary <- data.frame(Test_7)
write.csv(Test_7_summary, file="outputs/pes/test_runs/Test_7_summary.csv")

### Results tests 4:10 ####

# load in results 
test4 <- read.csv(file="outputs/pes/test_runs/Test_4_summary.csv")
test5 <- read.csv(file="outputs/pes/test_runs/Test_5_summary.csv")
test6 <- read.csv(file="outputs/pes/test_runs/Test_6_summary.csv")
test7 <- read.csv(file="outputs/pes/test_runs/Test_7_summary.csv")
test8 <- read.csv(file="outputs/pes/test_runs/Test_8_summary.csv")
test9 <- read.csv(file="outputs/pes/test_runs/Test_9_summary.csv")
test10 <- read.csv(file="outputs/pes/test_runs/Test_10_summary.csv")

# add perceive_feeding
test4$perc_feed <- "-0.5"
test8$perc_feed <- "-0.45"
test5$perc_feed <- "-0.4"
test9$perc_feed <- "-0.35"
test6$perc_feed <- "-0.3"
test10$perc_feed <- "-0.25"
test7$perc_feed <- "-0.2"

# merge
test_all <- rbind(test4,test8,test5,test9,test6,test10,test7)
test_all <- test_all[, -1]

# calculate what percentage of all actions were feeding and culling
test_all$feed_percent <- (test_all[,7] / (test_all[,7] + test_all[,5]))*100
test_all$cull_percent <- ifelse(test_all$feed_percent==100,0,100-test_all$feed_percent)

## plots

# proportion of action that were feeding
prop_feed <- ggplot(test_all, aes(x=Time, y=feed_percent, group=perc_feed, color=perc_feed))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=15),
                    legend.text = element_text(size=15),
                    legend.title = element_text(size=15))+
              facet_wrap(~perc_feed)+
              ylab("PES actions (% of total)")+
              ggtitle("a")

# cost of feeding
cost_feed <- ggplot(test_all, aes(x=Time, y=Feed_cost, group=perc_feed, color=perc_feed))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=15),
                    legend.text = element_text(size=15),
                    legend.title = element_text(size=15))+
              facet_wrap(~perc_feed)+
              ylab("Cost of PES action")+
              ggtitle("b")

# proportion of actions that were culling
prop_cull <- ggplot(test_all, aes(x=Time, y=cull_percent, group=perc_feed, color=perc_feed))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=15),
                    legend.text = element_text(size=15),
                    legend.title = element_text(size=15))+
              facet_wrap(~perc_feed)+
              ylab("Felling actions (% of total)")+
              ggtitle("c")

# cost of culling
cost_cull <- ggplot(test_all, aes(x=Time, y=Cull_cost, group=perc_feed, color=perc_feed))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=15),
                    legend.text = element_text(size=15),
                    legend.title = element_text(size=15))+
              facet_wrap(~perc_feed)+
              ylab("Cost of felling action")+
              ggtitle("d")

static_perc_feed <- prop_feed + cost_feed + prop_cull + cost_cull

ggsave(filename = "outputs/pes/test_runs/plots/static_perc_feed.png", static_perc_feed,
       width = 35, height = 30, units="cm", dpi=300)


## from the results above, it is clear that the perceive_feed value cannot be above -0.3, otherwise the users will choose to take the PES every single time step. I guess -0.35 could be the maximum value the parameter should go up to, i.e., the ceiling. And I guess 0 should be the floor? I want to now run some simulations with the PES value changing throughout the simulation. This should give more detail about how decisions change with changing values.


### Test 11 ####

# Here I want to vary the perceive_feed over the time steps, and see what happens.

# create vector of perceive_feed values
perc_f <- seq(0,-0.35, length.out = 30)

PF <- perc_f[1]

Test_11_sim_old <- gmse_apply(
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
  perceive_feed = PF,
  tend_crops = TRUE,
  tend_crop_yld = 0.01, 
  stakeholders = 30, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

# matrix for results
Test_11 <- matrix(data=NA, nrow=30, ncol=7)

# loop the simulation. 
for(time_step in 1:30){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Test_11_sim_old, perceive_feed = PF)
  
  Test_11[time_step, 1] <- time_step
  Test_11[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Test_11[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Test_11[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Test_11[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Test_11[time_step, 6] <- sim_new$basic_output$manager_results[5]
  Test_11[time_step, 7] <- sum(sim_new$basic_output$user_results[,5])
  
  
  Test_11_sim_old <- sim_new
  PF <- perc_f[time_step]
  print(time_step)
}

colnames(Test_11) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "Feed_cost", "Feed_count")
Test_11_summary <- data.frame(Test_11)
Test_11_summary$perceive_feed <- perc_f
write.csv(Test_11_summary, file="outputs/pes/test_runs/Test_11_summary.csv")


count_value <- ggplot(Test_11_summary, aes(x=perceive_feed, y=Feed_count))+
                geom_line(size=1)+
                theme_classic()+
                theme(axis.title = element_text(size=15),
                      axis.text = element_text(size=15),
                      plot.title = element_text(size=20))+
                ylab("Count of PES actions")+
                xlab("perceive_feed value")+
                scale_x_reverse()+
                ggtitle("a")

feed_cost <- ggplot(Test_11_summary, aes(x=perceive_feed, y=Feed_cost))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=15),
                    plot.title = element_text(size=20))+
              ylab("Cost of PES actions")+
              xlab("perceive_feed value")+
              scale_x_reverse()+
              ylim(0,110)+
              ggtitle("b")

fell_count <- ggplot(Test_11_summary, aes(x=perceive_feed, y=Cull_count))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=15),
                    plot.title = element_text(size=20))+
              ylab("Count of felling actions")+
              xlab("perceive_feed value")+
              scale_x_reverse()+
              ggtitle("c")

fell_cost <- ggplot(Test_11_summary, aes(x=perceive_feed, y=Cull_cost))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=15),
                    plot.title = element_text(size=20))+
              ylab("Cost of felling actions")+
              xlab("perceive_feed value")+
              scale_x_reverse()+
              ylim(0,110)+
              ggtitle("d")

test11_plots <- count_value + feed_cost + fell_count + fell_cost