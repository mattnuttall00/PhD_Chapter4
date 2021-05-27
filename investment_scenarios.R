### This script is for the first part of my final PhD chapter. The analysis is investigating scenarios around investment of conservation funds over time, given increasing threats. Threats here represent increasing human population density, which is represented by increasing user budgets.

### Load libraries ####

library('tidyverse')
library('GMSE')
library('patchwork')


############## LANDSCAPE DETAILS ##################

# Study period- 50 years

# Landscape dimensions - 150 x 150 cells.  which results in 22,500 cells (or ha). With 20 villages, this results in 1,125 ha or 11.25km2 per village. This means the number of trees will be 1,125,000.

# There is no public land. It does not serve any purpose in this study.

# "Resources" are trees. The resources therefore do not move.

# The density of trees in tropical forest landscapes vary hugely. In previous simulations I have assumed 50 stems/ha which is low, but not implausible (e.g. deciduous dipterocarp woodland). A reference for this value can be found here:https://www.jstor.org/stable/44521915. I am keeping this density of trees as it is for now, as this value means that there are already 2,000,000 trees on the landscape, and increasing them will increase run time. Note that the trees are distributed randomly across the landscape, and so there will not be exactly 50/cell. This reflects reality. 

# Trees in a cell reduce the farmer's yield. The amount a tree reduces yield is governed by an exponential function: yield = (1 - % yield reduction per tree) ^ remaining trees. I want a farmer's yield to be reduced by a significant amount if all trees in a cell are standing. But the trees do not completely eliminate yield. This is a balance between the farmer being able to farm and gain some yield even when there are trees on their cell, but also providing an incentive to cull where possible. I have set each tree on a cell to reduce yield by 8%. See the N1:N1f scenario comparisons which were used to decide on the parameter values for res_consume and tendcrop_yield

# The amount a user can increase their yield by tending crops is governed by tend_crop_yld. I have set this at 0.01 (1%) which means they can increase their yield on a cell by 1% in a single time step if they choose to tend crops. This is lower than the yield gain they would make if they felled some trees. This is set up so that there is an incentive to fell trees and expand their farmland, as it will increase their yield. See the N1:N1f scenario comparisons which were used to decide on the parameter values for res_consume and tendcrop_yield  

# For simplicity, I am assuming there is no natural death and no natural birth (forest regeneration). remove_pr is set to 0, and lambda is set to 0, and res_death_type is set to 0 (new value created by Brad that means no natural death at all) 

# The carrying capacity of new resources is set to 1 as it has to be a positive number but I want it as low as possible i.e. there is no real recruitment

# Resource carrying capacity is set very high (5,000,000) to reduce density-dependent death. Although res_death_type is set to 0 (no natural death) and so this parameter shouldn't be doing anything.

# The max age of trees is set high - 1000. This is to reduce natural death caused by old age (this should now be obsolete)

# The observation process is set to density-based sampling, with 1 observation per time step. The manager can move in any direction. Currently the manager can see 10 cells, and move 50 cells. In previous simulations (see "land_tenure_gmse.R" script) this has resulted in observation error of a few percent (max error ~2.3%). Realistically, forest cover monitoring is very accurate thanks to remote sensing. Nevertheless, I want some margin of error to reflect the fact that forest monitoring is not perfect (e.g. small areas of clearance cannot be detected easily from satellite images, and there is also a lag time in image processing so the manager's information on forest loss will not always be up to date). These values can be changed in increase error at a later date. 

# There is no minimum age for resources to be acted upon i.e. all trees in the landscape can be observed/culled

# Agents are permitted to move at the end of each time step. Because land_ownership==TRUE I believe this then only relates to the manager.

# User and manager budgets will vary based on the scenario. But the total amount of budget available to the manager for the whole study period will be the same. The total is 10,000. This is based on the first Null scenario (N1) where the user and the manager both had 200 per time step, for 50 time steps. Therefore the manager and user can never exceed 10,000 across the whole study period.  

# Currently, group_think == FALSE, and so users act independently

# Only culling is allowed (i.e. cutting down of trees)

# farming is allowed (tend_crops==TRUE, i.e. farmers can increase their yield by tending their crops rather than felling trees)



############## NULL SCENARIOS #####################

# These scenarios will explore the basic manager/user dynamics and will potentially reflect the "null" scenarios, or the counterfactuals.

# I will do the following scenarios:

# N1 - Null - Manager and user budgets do not change throughout the study period, and are equal

# N2a - Decreasing Null - Manager budget remains constant, user budget decreases linearly
# N2b - Decreasing Null - User budget remains constant, manager budget decreases linearly

# N3a - Optimistic Null - Manager budget increases linearly over time, user budget remains constant
# N3b - Optimistic NUll - Manager and user budgets both increase linearly, from the same same starting point and at the same rate
# N3c - Optimistic Null - Variation of N3. Manager and user budget increase linearly over time, but the manager budget rate of increase is lower than the user rate of increase
# N3d - optimistic Null - Variation of N3. Manager and user budget increase linearly over time, but the manager budget rate of increase is higher than the user rate of increase

# N4 - Pessimistic Null - Manager budget remains constant, but user budgets increase linearly 

#### N1 ####

## The original runs of N1 had the landscape set at 200x200 cells, 2 million trees, res_consume set to 0.05, and tend_crop_yield set to 0.02, and 40% public land. This was the set up used to run all the comparisons below between N1a:N1f. All of the comparison plots etc. saved in the N1 comparison folder will be using this set up. 

# Now though, I have changed the set up to reflect the final conclusions and the new landscape set up that all of the null scenarios will use. This is a landscape of 150x150 cells which results in 22,500 cells (or ha). With 20 villages, this results in 1,125 ha or 11.25km2 per village. This means the number of trees will be 1,125,000. Res_consume will be 0.08 and tend_crop_yield will be 0.01. There will be no public land. 


# This null scenario has the manager and user budgets remaining static over the entire study period


N1 <- gmse(
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, # landscape is 40,000ha or 400km2
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
  manager_budget = 200, 
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


# save summary
N1_summary <- as.data.frame(gmse_table(N1))
write.csv(N1_summary, file="outputs/investment/null_scenarios/N1/N1_summary.csv")

# load data
#N1_summary <- read.csv("outputs/investment/null_scenarios/N1/N1_summary.csv")

# custom plots
time_cost_N1 <- ggplot(N1_summary, aes(x=time_step,y=cost_culling))+
  geom_line()+
  theme_classic()

time_cull_N1 <- ggplot(N1_summary, aes(x=time_step, y=act_culling))+
  geom_line()+
  theme_classic()

time_res_N1 <- ggplot(N1_summary, aes(x=time_step, y=resources))+
  geom_line()+
  #ylim(0,1125000)+
  theme_classic()

time_yield_N1 <- ggplot(N1_summary, aes(x=time_step, y=crop_yield))+
  geom_line()+
  theme_classic()


N1_custom_plots <- time_cost_N1 + time_cull_N1 + time_res_N1 + time_yield_N1 
ggsave("outputs/investment/null_scenarios/N1/N1_custom_plots.png",
       width = 30, height = 20, units = "cm", dpi=300)

# gmse plots
N1_gmse_plots <- plot_gmse_results(sim_results = N1)

# remove object
#rm(N1)


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
  res_death_type = 0, # no natural death
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

# load
N1a_summary <- read.csv("outputs/investment/null_scenarios/N1/N1a_summary.csv")

# plots
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


N1a_custom_plots <- time_cost_N1a + time_cull_N1a + time_res_N1a + time_yield_N1a 
ggsave("outputs/investment/null_scenarios/N1/N1a_custom_plots.png", N1a_custom_plots,
       width = 30, height = 20, units="cm", dpi=300)

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
rm(N1b)


# load
#N1b_summary <- read.csv("outputs/investment/null_scenarios/N1/N1b_summary.csv")

  ## N1c ####

# Following on from Nils' email, I am going to run another scenario with more extreme parameters for res_consume and tend_crop_yld. This is to add further contrast so we can better tease apart the scenarios.

# Here I have increased the res_consume to 0.1 (10%)

N1c <- gmse(
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
  res_death_type = 0, # no natural death 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, 
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.1, # Trees have 10% impact on yield
  
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

N1c_summary <- as.data.frame(gmse_table(N1c))
write.csv(N1c_summary, file="outputs/investment/null_scenarios/N1/N1c_summary.csv")

rm(N1c)


  ## N1d ####

# Following on from Nils' email, I am going to run another scenario with more extreme parameters for res_consume and tend_crop_yld. This is to add further contrast so we can better tease apart the scenarios.

# Here I have kept res_consume at 0.08 (8%) but reduced tend_crop_yld to 0.01 (1%)

N1d <- gmse(
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
  manager_budget = 200, 
  usr_budget_rng = 20, # introduce variation around the mean user budget (removes step pattern) 
  manage_target = 2000000, 
  RESOURCE_ini = 2000000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.01, # tending crops increases yield by 1% - less than that of culling trees
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0.4, 
  manage_freq = 1, 
  group_think = FALSE
)

N1d_summary <- as.data.frame(gmse_table(N1d))
write.csv(N1d_summary, file="outputs/investment/null_scenarios/N1/N1d_summary.csv")

rm(N1d)



  ## N1e ####

# After comparing N1:N1d below, there appeared to be a difference between N1:N1a, and N1b:N1d. The latter resulted in users felling as often as possible (see the non-zero plateau in the plot of number of cull actions), whereas the former resulted in the users sometimes choosing to tend crops. N1b:N1d have parameter settings with distance between the values of 0.06, 0.07, and 0.08. N1 and N1a have parameter distances of 0 and 0.03. Therefore, I want to test what happens when the distance between the parameter values are 0.04 and 0.05.

# I am going to keep tend_crop_yld low, at 0.01 and rather change the value of res_consume. This is becuase conceptually, just tending your existing crops is unlikely to have any dramatic increase on your yield beyond what you already produce on that existing land. 

# below I will set res_consume to 0.05 and tend_crop_yld 0.01


N1e <- gmse(
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
  res_death_type = 0, # no natural death 
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
  tend_crop_yld = 0.01, # tending crops increases yield by 1% - less than that of culling trees
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0.4, 
  manage_freq = 1, 
  group_think = FALSE
)

N1e_summary <- as.data.frame(gmse_table(N1e))
write.csv(N1e_summary, file="outputs/investment/null_scenarios/N1/N1e_summary.csv")
rm(N1e)


  ## N1f ####

# see explanation above in N1e

# Here res_consume is 0.06 and tend_crop_yld is 0.01

N1f <- gmse(
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
  res_death_type = 0, # no natural death 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, 
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.06, # Trees have 6% impact on yield
  
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
  tend_crop_yld = 0.01, # tending crops increases yield by 1% - less than that of culling trees
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0.4, 
  manage_freq = 1, 
  group_think = FALSE
)

N1f_summary <- as.data.frame(gmse_table(N1f))
write.csv(N1f_summary, file="outputs/investment/null_scenarios/N1/N1f_summary.csv")

rm(N1f)


  ## Comparison N1:N1f ####

# Here I want to compare the differences in yields at each time step between N1, where res_consume is 0.05 and tend_crops_yld is 0.02, N1a, where res_consume and tend_crop_yld are equal, N1b where res_consume is 0.08 and tend_crop_yld is 0.02, N1c where res_consume is 0.1 and tend_crop_yld is 0.02, N1d where res_consume is 0.08 and tend_crop_yld is 0.01, N1e where res_consume is 0.05 and tend_crop_yld is 0.01, and N1f where res_consume is 0.06 and tend_crop_yld is 0.01. 

# No need to make the dataframe each time. Load the pre-made dataframe
yield.df <- read.csv("outputs/investment/null_scenarios/N1/yield_df_N1-N1b.csv")


# This was just me working out how to find and extraact the yield from the simulation output object. Extract total yield for all cells from each timestep for all sims. This needs the simulation objects to be in the environment, so won't work unles you run all of the sims again
yield_ts     <- sapply(1:length(N1a$land), function(x) sum(N1a$land[[x]][,,2]))
yield_ts_N1  <- sapply(1:length(N1$land), function(x) sum(N1$land[[x]][,,2]))
yield_ts_N1b <- sapply(1:length(N1b$land), function(x) sum(N1b$land[[x]][,,2]))
yield_ts_N1c <- sapply(1:length(N1c$land), function(x) sum(N1c$land[[x]][,,2]))
yield_ts_N1d <- sapply(1:length(N1d$land), function(x) sum(N1d$land[[x]][,,2]))
yield_ts_N1e <- sapply(1:length(N1e$land), function(x) sum(N1e$land[[x]][,,2]))
yield_ts_N1f <- sapply(1:length(N1f$land), function(x) sum(N1f$land[[x]][,,2]))


# load all simulation summaries
N1_summary  <-  read.csv("outputs/investment/null_scenarios/N1/N1_summary.csv", header=TRUE)
N1a_summary <- read.csv("outputs/investment/null_scenarios/N1/N1a_summary.csv", header=TRUE)
N1b_summary <- read.csv("outputs/investment/null_scenarios/N1/N1b_summary.csv", header=TRUE)
N1c_summary <- read.csv("outputs/investment/null_scenarios/N1/N1c_summary.csv", header=TRUE)
N1d_summary <- read.csv("outputs/investment/null_scenarios/N1/N1d_summary.csv", header=TRUE)
N1e_summary <- read.csv("outputs/investment/null_scenarios/N1/N1e_summary.csv", header=TRUE)
N1f_summary <- read.csv("outputs/investment/null_scenarios/N1/N1f_summary.csv", header=TRUE)


# create dataframe
sim <- c("N1","N1a","N1b", "N1c", "N1d","N1e","N1f")
yield.df <- data.frame(time_step = 1:50,
                       sim = rep(sim, each=50),
                       available_yld = 24000,
                       sim_yield = c(N1_summary$crop_yield, N1a_summary$crop_yield,
                                     N1b_summary$crop_yield, N1c_summary$crop_yield,
                                     N1d_summary$crop_yield, N1e_summary$crop_yield,
                                     N1f_summary$crop_yield),
                       trees = c(N1_summary$resources, N1a_summary$resources, N1b_summary$resources,
                                 N1c_summary$resources, N1d_summary$resources, N1e_summary$resources,
                                 N1f_summary$resources))


# add % yield to df
yield.df$perc_yld <- yield.df$sim_yield/yield.df$available_yld*100

# save dataframe
#write.csv(yield.df, file="outputs/investment/null_scenarios/N1/yield_df_N1-N1f.csv")


# first make a plot with all of the different sims and the parameter values
sim.para <- data.frame(sim = c("N1","N1a","N1b", "N1c", "N1d","N1e","N1f"),
                       res_consume = c(0.05,0.05,0.08,0.1,0.08,0.05,0.06),
                       tend_crop_yield = c(0.02,0.05,0.02,0.02,0.01,0.01,0.01))

p.sims <- ggplot(sim.para, aes(x=res_consume, y=tend_crop_yield, color=sim))+
          geom_point(size=7)+
          theme_classic()+
          theme(axis.title = element_text(size=17),
                axis.text = element_text(size=15),
                legend.text = element_text(size=15))



# plot
p1 <- ggplot(yield.df, aes(x=time_step, y=trees, group=sim, color=sim))+
      geom_line(size=2)+
      theme_classic()+
      ylab("Number of trees")+
      xlab("Time step")
# N1a results in the fewest trees being lost, which makes sense as N1a has no incentive to fell trees (equal parameter values). N1 and N1b:f are all fairly similar in their loss of trees. Interestingly, the simulation with the highest res_consume (N1c) does not end up with the fewest trees, and that must be because tend_crop_yield is higher than some of the others and so users will be more likely to choose to tend crops when costs of felling are very high. The simulation with the most trees lost is N1d, where tend_crop_yield is very low (0.01) and res_consume is quite high (0.08). This is closely followed by N1f which although has a lower res_consume than N1b and N1c, it also has a lower tend_crop_yield. This quite nicely shows the interaction between the two parameters I think. Essentially, I think this shows that small incremental changes in tend_crop_yield are actually more influential than similar increases in res_consume. 
 

p2 <- ggplot(yield.df, aes(x=time_step, y=perc_yld, group=sim, color=sim))+
      geom_line(size=2)+
      theme_classic()+
      ylab("% Yield")+
      xlab("Time step")
# Yield is lowest in N1c, as this simulation has the highest value for res_consume (0.1), followed by N1b and N1d (0.08). N1f then sits on it's own in the middle (0.06). The highest yields are for N1, N1a, and N1e, where res_consume is 0.05 for all. For this last group, we see that N1e is increasing slightly faster, as tend_crop_yld is set lower than N1 and N1a, and so users are more keen to fell trees as tending crops has less value. 


## more comparison plots between N1, N1a, N1b, N1c, N1d, N1e, N1f

# combine the simulation summaries for easier plotting
N1_summary$sim  <- "N1"
N1a_summary$sim <- "N1a"
N1b_summary$sim <- "N1b"
N1c_summary$sim <- "N1c"
N1d_summary$sim <- "N1d"
N1e_summary$sim <- "N1e"
N1f_summary$sim <- "N1f"
Nx_summary <- rbind(N1_summary, N1a_summary, N1b_summary, N1c_summary, N1d_summary, N1e_summary, N1f_summary)

# save
#write.csv(Nx_summary, file="outputs/investment/null_scenarios/N1/Nx_summary.csv")

# load
Nx_summary <- read.csv("outputs/investment/null_scenarios/N1/Nx_summary.csv")

# number of cull actions
p_cull <- ggplot(Nx_summary, aes(x=time_step, y=act_culling, group=sim, colour=sim))+
          geom_line(size=1.5)+
          xlab("Time step")+
          ylab("Number of cull actions")+
          theme_classic()
# This plot has changed significantly since the introduction of the new res_death_type. The simulations do appear to be broadly split between N1 and N1a, and the rest. N1 and N1a show much more variation in the number of cull actions, with the number of cull actions regulalry dropping to 0. N1a, which has the two parameters set equally at 0.05, we see regular spells of 0 cull actions, where the users are choosing to tend crops (as that produces the same benefits in terms of yield). This happens less frequently with N1, and in N1 there are no occasions when number of cull actions remain at 0 for more than a single time step. This is becuase tend_crop_yield is lower than res_consume, and so it is more beneficial to fell trees. For all of the other simualtions though, there appears to be a minimum number of culls below which they never drop (just over 100).  Even N1e, which is very similar to N1 in terms of parameters, never drops below a certain value of cull actions.   

p_cost <- ggplot(Nx_summary, aes(x=time_step, y=cost_culling, group=sim, colour=sim))+
          geom_line(size=1.5)+
          xlab("Time step")+
          ylab("Cost of cull actions")+
          theme_classic()
# It's quite difficult to see what is happening in this plot, but as would be expected the manager is adapting the cost of culling regulalry to try and prevent culling, which is being attempted in all scenarios. 

N1_Nf_tests <- p.sims / (p1 + p2) / (p_cull + p_cost)


ggsave("outputs/investment/null_scenarios/N1/N1_Nf_param_tests.png", N1_Nf_tests,
       width = 30, height = 20, units="cm", dpi=300)

  ## N1x - reduced landscape ####

# I want to see what the results look like when I remove the public land, reduce the size of the landscape. I am not sure what purpose the public land is serving, and at the moment it is making the landscape larger, and increasing the number of trees, which is making seeing differences in tree loss more difficult. 

# I will try a landscape ppf 150 x 150 cells. This results in 22,500 cells (or ha). With 20 villages, this results in 1,125 ha or 11.25km2 per village. This seems plausible. This means the number of trees will be 1,125,000

N1x <- gmse(
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
  res_consume = 0.05, # Trees have 5% impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = 200, 
  manager_budget = 200, 
  usr_budget_rng = 20, # introduce variation around the mean user budget (removes step pattern) 
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
  culling = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.02, # tending crops increases yield by 2% - less than that of culling trees
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0, 
  manage_freq = 1, 
  group_think = FALSE
)

plot_gmse_results(sim_results=N1c)
plot_gmse_effort(sim_results = N1c)

N1c_summary <- as.data.frame(gmse_table(N1c))
#write.csv(N1c_summary, file="outputs/investment/null_scenarios/N1/N1c_summary.csv")

# plots
time_cost_N1c <- ggplot(N1c_summary, aes(x=time_step,y=cost_culling))+
  geom_line()+
  theme_classic()

time_cull_N1c <- ggplot(N1c_summary, aes(x=time_step, y=act_culling))+
  geom_line()+
  theme_classic()

time_res_N1c <- ggplot(N1c_summary, aes(x=time_step, y=resources))+
  geom_line()+
  ylim(0,1125000)+
  theme_classic()

time_yield_N1c <- ggplot(N1c_summary, aes(x=time_step, y=crop_yield))+
  geom_line()+
  theme_classic()


time_cost_N1c + time_cull_N1c + time_res_N1c + time_yield_N1c 


  ## N1y - test parameters ####

# following Brad's advice, I am going to test a simulation where res_consume=0, tend_crop_yield=0.01 and user_budget=1. This is just to make sure everything behaves as expected. What I expect is that there will be no cull actions, becuase there is no incentive, nor budget, to cull trees. There will also be no tending of crops because the user budget is lower than the minimum cost

N1y <- gmse(
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
  res_consume = 0, # Trees have 0% impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = 1, 
  manager_budget = 200, 
  usr_budget_rng = 1, # introduce variation around the mean user budget (removes step pattern) 
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

N1y_summary <- read.csv("outputs/investment/null_scenarios/N1/N1_comparison_res_versus_tend/N1y_summary.csv")




  ## N1z - test parameters ####


N1z <- gmse(
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
  res_consume = 0, # Trees have 0% impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, 
  max_ages = 1000, 
  minimum_cost = 10, 
  user_budget = 10, 
  manager_budget = 200, 
  usr_budget_rng = 1, # introduce variation around the mean user budget (removes step pattern) 
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


N1z_summary <- as.data.frame(gmse_table(N1z))
write.csv(N1z_summary, file="outputs/investment/null_scenarios/N1/N1z_summary.csv")

#### N2 ####

# The below calls are for the N2a and N2b null scenarios (see details above in "NULL SCENARIOS"). Currently I am using the reduced landscape from N1x.

# N2c and N2d are the same as a and b above, but I am increasing the agent_move to 100 and agent_view to 50 to see what effect this has on the estiamtes of trees

  ## N2a ####

# user budget decreases linearly, manager budget remains constant. Observation type changed to transect, and agent_view increased so no observation error

# I have changed the value of the user budget range from 1/10 to 1/5 in this scenario (but not in N2b) to see if it would reduce the step pattern in the cull count, but it has not worked. Will need to check with Brad.

UB  <- 200
UBR <- 40

N2a_sim_old <- gmse_apply(
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
  manager_budget = 200, 
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
N2a <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = N2a_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  N2a[time_step, 1] <- time_step
  N2a[time_step, 2] <- sim_new$basic_output$resource_results[1]
  N2a[time_step, 3] <- sim_new$basic_output$observation_results[1]
  N2a[time_step, 4] <- sim_new$basic_output$manager_results[3]
  N2a[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  N2a[time_step, 6] <- UB
  
  N2a_sim_old <- sim_new
  UB <- UB - 3
  UBR <- UB/5
}

colnames(N2a) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count",
                         "User_budget")
N2a_summary <- data.frame(N2a)

write.csv(N2a_summary, file = "outputs/investment/null_scenarios/N2/N2a_summary.csv")

  ## N2b ####

# User budget remains constant, manager budge decreases linearly

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

N2b_summary <- read.csv("outputs/investment/null_scenarios/N2/N2b_summary.csv", header = TRUE)

  ## N2a + b plots ####

# load summary data
N2a_summary <- read.csv("outputs/investment/null_scenarios/N2/N2a_summary.csv", header = TRUE)
N2b_summary <- read.csv("outputs/investment/null_scenarios/N2/N2b_summary.csv", header = TRUE)

N2a_summary <- N2a_summary[,-1] 
N2b_summary <- N2b_summary[,-1]

# change name of budget column and add actor column
N2a_summary <- N2a_summary %>% rename(Budget = User_budget)
N2a_summary$Actor <- "User"
N2b_summary <- N2b_summary %>% rename(Budget = Manager_budget)
N2b_summary$Actor <- "Manager"

# duplicate dataframe without budget and actor columns
N2a_2 <- N2a_summary[ ,c(1:5)]
N2b_2 <- N2b_summary[ ,c(1:5)]

# add alternate actor and budget column
N2a_2$Budget <- 200
N2a_2$Actor  <- "Manager"
N2b_2$Budget <- 200
N2b_2$Actor  <- "User"

# merge
N2a_summary <- rbind(N2a_summary, N2a_2)
N2b_summary <- rbind(N2b_summary, N2b_2)

# add sim
N2a_summary$sim <- "N2a - User budget decline"
N2b_summary$sim <- "N2b - Manager budget decline"

# merge
N2_summary <- rbind(N2a_summary, N2b_summary)


# plots
p.budget.a <- ggplot(N2_summary[N2_summary$sim=="N2a - User budget decline",], 
                     aes(x=Time, y=Budget, group=Actor, color=Actor))+
              geom_line(size=3)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12),
                    legend.text = element_text(size=12),
                    legend.title = element_text(size=15))+
              ggtitle("N2a - User budget decline")

p.budget.b <- ggplot(N2_summary[N2_summary$sim=="N2b - Manager budget decline",], 
                     aes(x=Time, y=Budget, group=Actor, color=Actor))+
              geom_line(size=3)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12),
                    legend.text = element_text(size=12),
                    legend.title = element_text(size=15))+
              ggtitle("N2b - Manager budget decline")


p.trees <- ggplot(N2_summary, aes(x=Time, y=Trees, group=sim, color=sim))+
           geom_line(size=3)+
           theme_classic()+
            theme(axis.title = element_text(size=15),
            axis.text = element_text(size=12),
            legend.text = element_text(size=12),
            legend.title = element_text(size=15))

p.cull <- ggplot(N2_summary, aes(x=Time, y=Cull_count, group=sim, color=sim))+
          geom_line(size=3)+
          theme_classic()+
          theme(axis.title = element_text(size=15),
                axis.text = element_text(size=12),
                legend.text = element_text(size=12),
                legend.title = element_text(size=15))


p.cost <- ggplot(N2_summary, aes(x=Time, y=Cull_cost, group=sim, color=sim))+
          geom_line(size=3)+
          theme_classic()+
          theme(axis.title = element_text(size=15),
                axis.text = element_text(size=12),
                legend.text = element_text(size=12),
                legend.title = element_text(size=15))

p.N2a_b_com <- (p.budget.a + p.budget.b) / (p.cull + p.cost) / p.trees

ggsave("outputs/investment/null_scenarios/N2/N2a_N2b_comparison_plot.png", p.N2a_b_com,
       width = 30, height = 20, units="cm", dpi=300)

target <- data.frame(Time = c(1:50),
                     Trees = 1125000)

p.N2a.obs <- ggplot(N2_summary[N2_summary$sim=="N2a - User budget decline",], aes(x=Time))+
            geom_line(aes(y=Trees, color="Trees"), size=3)+
            geom_line(aes(y=Trees_est, color="Estimate"), size=3)+
            geom_line(data=target, aes(x=Time, y=Trees), linetype="dashed", size=2)+
            scale_color_manual(name="Line colour", values=c(Trees="red", Estimate="blue"))+
            theme_classic()+
            theme(axis.title = element_text(size=15),
                  axis.text = element_text(size=15),
                  legend.text = element_text(size=15),
                  legend.title = element_text(size=15))+
            ggtitle("N2a - User budget decline")

p.N2b.obs <- ggplot(N2_summary[N2_summary$sim=="N2b - Manager budget decline",], aes(x=Time))+
            geom_line(aes(y=Trees, color="Trees"), size=3)+
            geom_line(aes(y=Trees_est, color="Estimate"), size=3)+
            scale_color_manual(name="Line colour", values=c(Trees="red", Estimate="blue"))+
            theme_classic()+
            ylab("")+
            theme(axis.title = element_text(size=15),
                  axis.text = element_text(size=15),
                  legend.text = element_text(size=15),
                  legend.title = element_text(size=15))+
            ggtitle("N2b - Manager budget decline")

p.N2a.obs + p.N2b.obs


#
  ## N2c ####

## This simulation is identical to N2a but will increase agent_view and agent_move

# user budget decreases linearly, manager budget remains constant

UB  <- 200
UBR <- 20

N2c_sim_old <- gmse_apply(
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
  agent_view = 50, 
  agent_move = 100, 
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
  user_budget = UB, 
  manager_budget = 200, 
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
N2c <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = N2c_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  N2c[time_step, 1] <- time_step
  N2c[time_step, 2] <- sim_new$basic_output$resource_results[1]
  N2c[time_step, 3] <- sim_new$basic_output$observation_results[1]
  N2c[time_step, 4] <- sim_new$basic_output$manager_results[3]
  N2c[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  N2c[time_step, 6] <- UB
  
  N2c_sim_old <- sim_new
  UB <- UB - 3
  UBR <- UB/10
}

colnames(N2c) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count",
                   "User_budget")
N2c_summary <- data.frame(N2c)

write.csv(N2c_summary, file="outputs/investment/null_scenarios/N2/N2c_summary.csv")


  ## N2d ####

## this sim is identical to N2b but I will increase agent_move to 100 and agent_view to 50 to see what happens to the precision and accuracy of the observation model

# User budget remains constant, manager budge decreases linearly

MB  <- 200

N2d_sim_old <- gmse_apply(
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
  agent_view = 50, 
  agent_move = 100, 
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
N2d <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = N2d_sim_old, manager_budget=MB)
  
  N2d[time_step, 1] <- time_step
  N2d[time_step, 2] <- sim_new$basic_output$resource_results[1]
  N2d[time_step, 3] <- sim_new$basic_output$observation_results[1]
  N2d[time_step, 4] <- sim_new$basic_output$manager_results[3]
  N2d[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  N2d[time_step, 6] <- MB
  
  N2d_sim_old <- sim_new
  MB <- MB - 3
  
}

colnames(N2d) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count",
                   "Manager_budget")
N2d_summary <- data.frame(N2d)

#write.csv(N2d_summary, file = "outputs/investment/null_scenarios/N2/N2d_summary.csv")

#N2b_summary <- read.csv("outputs/investment/null_scenarios/N2/N2b_summary.csv", header = TRUE)



  ## N2c + d plots ####

N2c_summary <- read.csv("outputs/investment/null_scenarios/N2/N2c_summary.csv", header = TRUE)
N2d_summary <- read.csv("outputs/investment/null_scenarios/N2/N2d_summary.csv", header = TRUE)


N2c_summary <- N2c_summary[,-1] 
N2d_summary <- N2d_summary[,-1]

# change name of budget column and add actor column
N2c_summary <- N2c_summary %>% rename(Budget = User_budget)
N2c_summary$Actor <- "User"
N2d_summary <- N2d_summary %>% rename(Budget = Manager_budget)
N2d_summary$Actor <- "Manager"

# duplicate dataframe without budget and actor columns
N2c_2 <- N2c_summary[ ,c(1:5)]
N2d_2 <- N2d_summary[ ,c(1:5)]

# add alternate actor and budget column
N2c_2$Budget <- 200
N2c_2$Actor  <- "Manager"
N2d_2$Budget <- 200
N2d_2$Actor  <- "User"

# merge
N2c_summary <- rbind(N2c_summary, N2c_2)
N2d_summary <- rbind(N2d_summary, N2d_2)

# add sim
N2c_summary$sim <- "N2c - User budget decline"
N2d_summary$sim <- "N2d - Manager budget decline"

# merge
N2.1_summary <- rbind(N2c_summary, N2d_summary)


# plots
p.budget.c <- ggplot(N2.1_summary[N2.1_summary$sim=="N2c - User budget decline",], 
                     aes(x=Time, y=Budget, group=Actor, color=Actor))+
              geom_line(size=3)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12),
                    legend.text = element_text(size=12),
                    legend.title = element_text(size=15))+
              ggtitle("N2c - User budget decline")

p.budget.d <- ggplot(N2.1_summary[N2.1_summary$sim=="N2d - Manager budget decline",], 
                     aes(x=Time, y=Budget, group=Actor, color=Actor))+
              geom_line(size=3)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12),
                    legend.text = element_text(size=12),
                    legend.title = element_text(size=15))+
              ggtitle("N2d - Manager budget decline")


p.trees.cd <- ggplot(N2.1_summary, aes(x=Time, y=Trees, group=sim, color=sim))+
            geom_line(size=3)+
            theme_classic()+
            theme(axis.title = element_text(size=15),
                  axis.text = element_text(size=12),
                  legend.text = element_text(size=12),
                  legend.title = element_text(size=15))

p.cull.cd <- ggplot(N2.1_summary, aes(x=Time, y=Cull_count, group=sim, color=sim))+
          geom_line(size=3)+
          theme_classic()+
          theme(axis.title = element_text(size=15),
                axis.text = element_text(size=12),
                legend.text = element_text(size=12),
                legend.title = element_text(size=15))


p.cost.cd <- ggplot(N2.1_summary, aes(x=Time, y=Cull_cost, group=sim, color=sim))+
          geom_line(size=3)+
          theme_classic()+
          theme(axis.title = element_text(size=15),
                axis.text = element_text(size=12),
                legend.text = element_text(size=12),
                legend.title = element_text(size=15))

(p.budget.c + p.budget.d) / (p.cull.cd + p.cost.cd) / p.trees.cd


## observation error plots

target <- data.frame(Time = c(1:50),
                     Trees = 1125000)

p.N2c.obs <- ggplot(N2.1_summary[N2.1_summary$sim=="N2c - User budget decline",], aes(x=Time))+
            geom_line(aes(y=Trees, color="Trees"), size=3)+
            geom_line(aes(y=Trees_est, color="Estimate"), size=3)+
            geom_line(data=target, aes(x=Time, y=Trees), linetype="dashed", size=2)+
            scale_color_manual(name="Line colour", values=c(Trees="red", Estimate="blue"))+
            theme_classic()+
            theme(axis.title = element_text(size=15),
                  axis.text = element_text(size=15),
                  legend.text = element_text(size=15),
                  legend.title = element_text(size=15))+
            ggtitle("N2c - User budget decline")

p.N2d.obs <- ggplot(N2.1_summary[N2.1_summary$sim=="N2d - Manager budget decline",], aes(x=Time))+
            geom_line(aes(y=Trees, color="Trees"), size=3)+
            geom_line(aes(y=Trees_est, color="Estimate"), size=3)+
            scale_color_manual(name="Line colour", values=c(Trees="red", Estimate="blue"))+
            theme_classic()+
            ylab("")+
            theme(axis.title = element_text(size=15),
                  axis.text = element_text(size=15),
                  legend.text = element_text(size=15),
                  legend.title = element_text(size=15))+
            ggtitle("N2d - Manager budget decline")

p.N2c.obs + p.N2d.obs


#### N3 ####

# N3 and its variants represent the optimistic null - where the manager budget is increasing over time. The variants introduce different rates of increase for the user and the manager. 


  ## N3a ####

# N3a - The manager budget increases linearly (to a cumulative maximum of 10,000), the user budget remains constant.

# In order for all of the different scenarios that have increasing budgets to be comparable, there needs to be a finite amount of budget for both user and manager in any given scenario. Based on the constant budget of 200 set in N1, and the 50 time steps, the maximum total budget is 10,000. 

UB  <- 200
UBR <- 40

N2a_sim_old <- gmse_apply(
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
  manager_budget = 200, 
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
N2a <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = N2a_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  N2a[time_step, 1] <- time_step
  N2a[time_step, 2] <- sim_new$basic_output$resource_results[1]
  N2a[time_step, 3] <- sim_new$basic_output$observation_results[1]
  N2a[time_step, 4] <- sim_new$basic_output$manager_results[3]
  N2a[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  N2a[time_step, 6] <- UB
  
  N2a_sim_old <- sim_new
  UB <- UB - 3
  UBR <- UB/5
}

colnames(N2a) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count",
                   "User_budget")
N2a_summary <- data.frame(N2a)

write.csv(N2a_summary, file = "outputs/investment/null_scenarios/N2/N2a_summary.csv")

