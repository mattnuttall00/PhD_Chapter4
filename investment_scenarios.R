### This script is for the first part of my final PhD chapter. The analysis is investigating scenarios around investment of conservation funds over time, given increasing threats. Threats here represent increasing human population density, which is represented by increasing user budgets.

### Load libraries ####

library('tidyverse')
library('GMSE')
library('patchwork')


############## LANDSCAPE DETAILS ##################

# Study period- 50 years

# Landscape dimensions - 200 x 200 cells. I am assuming each cell is 1 hectare. Therefore the landscape is 40,000 ha or 400 km2. 

# 60% of the landscape is owned by the communities (stakeholders), 40% is public land (i.e. protected from deforestation)

# Stakekholders - There are 20 "stakeholders" which in these scenarios equates to 20 villages. That means that each village controls 12km2 of land (60% of the total landscape divided by 20 - see above). 
# "Resources" are trees. The resources therefore do not move.

# The density of trees in tropical forest landscapes vary hugely. In previous simulations I have assumed 50 stems/ha which is low, but not implausible (e.g. deciduous dipterocarp woodland). A reference for this value can be found here:https://www.jstor.org/stable/44521915. I am keeping this density of trees as it is for now, as this value means that there are already 2,000,000 trees on the landscape, and increasing them will increase run time. Note that the trees are distributed randomly across the landscape, and so there will not be exactly 50/cell. This reflects reality. 

# Trees in a cell reduce the farmer's yield. The amount a tree reduces yield is governed by an exponential function: yield = (1 - % yield reduction per tree) ^ remaining trees. I want a farmer's yield to be reduced by a significant amount if all trees in a cell are standing. But the trees do not completely eliminate yield. This is a balance between the farmer being able to farm and increase their yield even when there are trees on their cell, but also providing an incentive to cull where possible. I have set each tree on a cell to reduce yield by 5%. Therefore if there are 50 trees on a cell yield is reduced to 36% of the total available yield. Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc.

# The amount a user can increase their yield by tending crops is governed by tend_crop_yld. I have set this at 0.02 (2%) which means they can increase their yield on a cell by 2% in a single time step if they choose to tend crops. This is lower than the yield gain they would make if they felled some trees. This is set up so that there is an incentive to fell trees and expand their farmland, as it will incrase their yield. However, the incentive is not so high that it is all-consuming. 

# For simplicity, I am assuming there is no natural death and no natural birth (forest regeneration). remove_pr is set to 0, and lambda is set to 0. Currently there are still a few natural deaths via old age, but this is going to be changed in the code by Brad

# The carrying capacity of new resources is set to 1 as it has to be a positive number but I want it as low as possible i.e. there is no real recruitment

# Resource carrying capacity is set very high (5,000,000) to reduce density-dependent death. Although res_death_type is set to 1 (density-independent) and so I don't think this parameter should be doing anything.

# The max age of trees is set high - 1000. This is to reduce natural death caused by old age (this will hopefully become obsolete when Brad adds new code to remove natural death)

# The observation process is set to density-based sampling, with 1 observation per time step. The manager can move in any direction. Currently the manager can see 10 cells, and move 50 cells. In previous simulations (see "land_tenure_gmse.R" script) this has resulted in observation error of a few percent (max error ~2.3%). Realistically, forest cover monitoring is very accurate thanks to remote sensing. Nevertheless, I want some margin of error to reflect the fact that forest monitoring is not perfect (e.g. small areas of clearance cannot be detected easily from satellite images, and there is also a lag time in image processing so the manager's information on forest loss will not always be up to date). These values can be changed in increase error at a later date. 

# There is no minimum age for resources to be acted upon i.e. all trees in the landscape can be observed/culled

# Agents are permitted to move at the end of each time step. Because land_ownership==TRUE I believe this then only relates to the manager.

# User and manager budgets will vary based on the scenario. But the total amount of budget available to the manager for the whole study period will be the same. The total 

# Currently, group_think == FALSE, and so users act independently

# Only culling is allowed (i.e. cutting down of trees)

# farming is allowed (tend_crops==TRUE, i.e. farmers can increase their yield by tending their crops rather than felling trees)



############## NULL SCENARIOS #####################

# These scenarios will explore the basic manager/user dynamics and will potentially reflect the "null" scenarios, or the counterfactuals.

# I will do the following scenarios:

# N1 - Null - Manager and user budgets do not change throughout the study period, and are equal

# N2 - Optimistic Null - Manager and user budgets both increase linearly over time, at the same rate and from the same starting point

# N2a - Optimistic Null - Variation of N2. Manager and user budget increase linearly over time, but the manager budget rate of increase is lower than the user rate of increase

# N2b - optimistic Null - Variation of N2. Manager and user budget increase linearly over time, but the manager budget rate of increase is higher than the user rate of increase

# N3 - Pessimistic Null - Manager budget remains constant, but user budgets increase linearly 

#### N1 ####

# This null scenario has the manager and user budgets remaining static over the entire study period

# N1 below has each tree reducing yield on a cell by 5%. And by tending crops in a time step, users can increase their yield by 2%. Therefore cutting trees will increase their yield more than tending crops. 

# N1a and N1b below are different variations on N1, where res_consume and tend_crop_yld are different. I wanted to see what the impact of different ratios between these two parameters was. See the "Comparison between N1, N1a, N1b" section below for plots etc. Currently, I think N1 is the best option. 

N1 <- gmse(
  time_max = 50,
  land_dim_1 = 200,
  land_dim_2 = 200, # landscape is 40,000ha or 400km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, 
  agent_move = 50, 
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, 
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.05, # Trees have 5% impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = 200, 
  manager_budget = 200, 
  usr_budget_rng = 20, # introduce variation around the mean user budget (removes step pattern) 
  manage_target = 2000000, 
  RESOURCE_ini = 2000000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.02, # tending crops increases yield by 2% - less than that of culling trees
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0.4, 
  manage_freq = 1, 
  group_think = FALSE
)


# save summary
N1_summary <- as.data.frame(gmse_table(N1))
#write.csv(N1_summary, file="outputs/investment/null_scenarios/N1_summary.csv")

# load data
#N1_summary <- read.csv("outputs/investment/null_scenarios/N1_summary.csv")

# plots
time_cost_N1 <- ggplot(N1_summary, aes(x=time_step,y=cost_culling))+
  geom_line()+
  theme_classic()

time_cull_N1 <- ggplot(N1_summary, aes(x=time_step, y=act_culling))+
  geom_line()+
  theme_classic()

time_res_N1 <- ggplot(N1_summary, aes(x=time_step, y=resources))+
  geom_line()+
  ylim(0,2000000)+
  theme_classic()

time_yield_N1 <- ggplot(N1_summary, aes(x=time_step, y=crop_yield))+
  geom_line()+
  theme_classic()


time_cost_N1 + time_cull_N1 + time_res_N1 + time_yield_N1 

plot_gmse_results(sim_results = N1)


  ## N1a #### 

# here I have made the yield increases from tending crops and felling trees equal. 

N1a <- gmse(
  time_max = 50,
  land_dim_1 = 200,
  land_dim_2 = 200, # landscape is 40,000ha or 400km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, 
  agent_move = 50, 
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, 
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.05, # Trees have 5% impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = 200, 
  manager_budget = 200, 
  usr_budget_rng = 20, # introduce variation around the mean user budget (removes step pattern) 
  manage_target = 2000000, 
  RESOURCE_ini = 2000000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.05, # tending crops increases yield by 5% - same as culling trees
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0.4, 
  manage_freq = 1, 
  group_think = FALSE
)

# As predicted, there is less felling of trees by the users becuase there is no real incentive to do so. 

N1a_summary <- as.data.frame(gmse_table(N1a))
#write.csv(N1a_summary, file="outputs/investment/null_scenarios/N1/N1a_summary.csv")

time_cost_N1a <- ggplot(N1a_summary, aes(x=time_step,y=cost_culling))+
  geom_line()+
  theme_classic()

time_cull_N1a <- ggplot(N1a_summary, aes(x=time_step, y=act_culling))+
  geom_line()+
  theme_classic()

time_res_N1a <- ggplot(N1a_summary, aes(x=time_step, y=resources))+
  geom_line()+
  ylim(0,2000000)+
  theme_classic()

time_yield_N1a <- ggplot(N1a_summary, aes(x=time_step, y=crop_yield))+
  geom_line()+
  theme_classic()


time_cost_N1a + time_cull_N1a + time_res_N1a + time_yield_N1a 
plot_gmse_results(sim_results = N1a)


### calculate the % yield for each timestep

# Extract total yield for all cells from each timestep
yield_ts <- sapply(1:length(N1a$land), function(x) sum(N1a$land[[x]][,,2]))

# into dataframe
yield.df <- data.frame(time_step = 1:50,
                       total_yld = yield_ts,
                       available_yld = 24000)
# add % yield to df
yield.df$perc_yld <- yield.df$total_yld/yield.df$available_yld*100

# add sim number and description
yield.df$sim <- "N1a"
yield.df$desc <- "tend==cull"

  ## N1b ####

# Here I have increased the res_consume to 0.08 (8%)

N1b <- gmse(
  time_max = 50,
  land_dim_1 = 200,
  land_dim_2 = 200, # landscape is 40,000ha or 400km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, 
  agent_move = 50, 
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
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
  manager_budget = 200, 
  usr_budget_rng = 20, # introduce variation around the mean user budget (removes step pattern) 
  manage_target = 2000000, 
  RESOURCE_ini = 2000000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.02, # tending crops increases yield by 2% - less than that of culling trees
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0.4, 
  manage_freq = 1, 
  group_think = FALSE
)

N1b_summary <- as.data.frame(gmse_table(N1b))
#write.csv(N1b_summary, file="outputs/investment/null_scenarios/N1/N1b_summary.csv")


  ## Comparison between N1, N1a, N1b ####

# Here I want to compare the differences in yields at each time step between N1, where res_consume is 0.05 and tend_crops_yld is 0.2, N1a, where res_consume and tend_crop_yld are equal, and N1b, where res_consume is 0.08 and tend_crop_yld is 0,02.

# To do the below you need to have the N1 and N1a sims run and as objects in the environment. I will save the resulting dataframe so that this is not required



# calculate the % yield for each timestep

# Extract total yield for all cells from each timestep for all sims
yield_ts     <- sapply(1:length(N1a$land), function(x) sum(N1a$land[[x]][,,2]))
yield_ts_N1  <- sapply(1:length(N1$land), function(x) sum(N1$land[[x]][,,2]))
yield_ts_N1b <- sapply(1:length(N1b$land), function(x) sum(N1b$land[[x]][,,2]))

# into dataframe
sim <- c("N1","N1a","N1b")
yield.df <- data.frame(time_step = 1:50,
                       sim = rep(sim, each=50),
                       available_yld = 24000,
                       sim_yield = c(yield_ts_N1, yield_ts, yield_ts_N1b),
                       trees = c(N1_summary$resources, N1a_summary$resources, N1b_summary$resources))

# save dataframe
#write.csv(yield.df, file="outputs/investment/null_scenarios/N1/yield_df_N1-N1b.csv")


# add % yield to df
yield.df$perc_yld <- yield.df$sim_yield/yield.df$available_yld*100

# plot
p1 <- ggplot(yield.df, aes(x=time_step, y=trees, group=sim, color=sim))+
      geom_line(size=2)+
      theme_classic()+
      ylab("Number of trees")+
      xlab("Time step")

p2 <- ggplot(yield.df, aes(x=time_step, y=perc_yld, group=sim, color=sim))+
      geom_line(size=2)+
      theme_classic()+
      ylab("% Yield")+
      xlab("Time step")

p1 + p2


## more comparison plots between N1, N1a, N1b

# combine the simulation summaries for easier plotting
N1_summary$sim  <- "N1"
N1a_summary$sim <- "N1a"
N1b_summary$sim <- "N1b"
Nx_summary <- rbind(N1_summary, N1a_summary, N1b_summary)

# number of cull actions
p_cull <- ggplot(Nx_summary, aes(x=time_step, y=act_culling, group=sim, colour=sim))+
          geom_line(size=1.5)+
          xlab("Time step")+
          ylab("Number of cull actions")+
          theme_classic()
# N1a is a very low, flat line. This is because the benefits of tending crops are the same as culling, so there is no real incentive to cull and the manager can put a stop to it easily. N1b shows consistently high culling, because the yield increases from culling are high (res_consume=0.08). There is some conflict with the manager in the early time steps, and then an equalibrium is reached where the users are culling as much as possible and the manager is presumably using all their budget to prevent culling. N1 is far more dynamic. Here there is still an incentive to cull trees (res_consume=0.05, tend_crop_yld=0.02), but it is less than N1b. It looks like in this scenarion the users will cull trees where possible, but as soon as the manager raises the costs they go back to tending crops. N1b (blue line) looks more like a conflict-ridden landscape where there is perhaps a lot of in-migration and extansive forest clearance, whereas N1 looks more like a landscape where opportunistic land clearance would occur, but can relatively easily be controlled. 

p_cost <- ggplot(Nx_summary, aes(x=time_step, y=cost_culling, group=sim, colour=sim))+
          geom_line(size=1.5)+
          xlab("Time step")+
          ylab("Cost of cull actions")+
          theme_classic()
# The costs of N1 and N1b look as I would expect - mirroring the culling actions. I am not sure exactly why the manager continues to fluctuate the cost of culling in N1a, when there are no cull actions at all?  

(p1 + p2) / (p_cull + p_cost)
