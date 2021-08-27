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

# The density of trees in tropical forest landscapes vary hugely. In previous simulations I have assumed 50 stems/ha which is low, but not implausible (e.g. deciduous dipterocarp woodland). A reference for this value can be found here:https://www.jstor.org/stable/44521915. I am keeping this density of trees as it is for now, as this value means that there are already 1,125,000 trees on the landscape, and increasing them will increase run time and reduce the overall effect of deforestation. Note that the trees are distributed randomly across the landscape, and so there will not be exactly 50/cell. This reflects reality. 

# Trees in a cell reduce the farmer's yield. The amount a tree reduces yield is governed by an exponential function: yield = (1 - % yield reduction per tree) ^ remaining trees. I want a farmer's yield to be reduced by a significant amount if all trees in a cell are standing. But the trees do not completely eliminate yield. This is a balance between the farmer being able to farm and gain some yield even when there are trees on their cell, but also providing an incentive to cull where possible. I have set each tree on a cell to reduce yield by 8%. See the N1:N1f scenario comparisons which were used to decide on the parameter values for res_consume and tendcrop_yield

# The amount a user can increase their yield by tending crops is governed by tend_crop_yld. I have set this at 0.01 (1%) which means they can increase their yield on a cell by 1% in a single time step if they choose to tend crops. This is lower than the yield gain they would make if they felled some trees. This is set up so that there is an incentive to fell trees and expand their farmland, as it will increase their yield. See the N1:N1f scenario comparisons which were used to decide on the parameter values for res_consume and tendcrop_yield  

# For simplicity, I am assuming there is no natural death and no natural birth (forest regeneration). remove_pr is set to 0, and lambda is set to 0, and res_death_type is set to 0 (new value created by Brad that means no natural death at all) 

# The carrying capacity of new resources is set to 1 as it has to be a positive number but I want it as low as possible i.e. there is no real recruitment

# Resource carrying capacity is set very high (5,000,000) to reduce density-dependent death. Although res_death_type is set to 0 (no natural death) and so this parameter shouldn't be doing anything.

# The max age of trees is set high - 1000. This is to reduce natural death caused by old age (this should now be obsolete)

# The observation process is set to density-based sampling, with 1 observation per time step. The manager can move in any direction. Currently the manager can see 10 cells, and move 50 cells. In previous simulations (see "land_tenure_gmse.R" script) this has resulted in observation error of a few percent (max error ~2.3%). Realistically, forest cover monitoring is very accurate thanks to remote sensing. Nevertheless, I have decided to remove any observation error. 

# There is no minimum age for resources to be acted upon i.e. all trees in the landscape can be observed/culled

# Agents are permitted to move at the end of each time step. Because land_ownership==TRUE I believe this then only relates to the manager.

# User and manager budgets will vary based on the scenario. But the total amount of budget available to the manager for the whole study period will be the same. The total is 25,000. See the "scenario_details_budgets" spreadsheet  

# Group_think == FALSE, and so users act independently

# Only culling is allowed (i.e. cutting down of trees)

# farming is allowed (tend_crops==TRUE, i.e. farmers can increase their yield by tending their crops rather than felling trees)



############## SCENARIOS #####################

# Below are the scenarios that I will run. There are a few "null" scenario, which will explore the basic manager/user dynamics and will potentially reflect the counterfactuals. The scenarios starting with "N.." will not feature in the main text but will be in the supporting information. The scenarios labelled 1 through 5 will be in the main text and will form the main results.


# N1 - Null - Manager and user budgets do not change throughout the study period, and are equal
# N2a - Decreasing Null - Manager budget remains constant, user budget decreases linearly
# N2b - Decreasing Null - User budget remains constant, manager budget decreases linearly
# N3 - Optimistic Null - Manager budget increases linearly over time, user budget remains constant


# 1 - Pessimistic Null - Manager budget remains constant, user budgets increase linearly 
# 2 - Optimistic Null - Manager and user budgets both increase linearly, with the same slope
# 3 - Sine wave - Manager budget increases and decreases in a predictable/regular way above and below a mean (like a sine wave), user budget increases linearly
# 4 - sine wave - As above, but the wavelength is shorter (higher frequency) and the amplitude is smaller
# 5 - Random complex wave(s) - Manager budget increases and decreases unpredictably (using Fourier series to create random complex waves), user budget increases linearly. There are currently 10 waves that constitute this scenario


## below there are more scenarios than I have listed above. These were run to work out some of the parameter values (e.g. res_consume and tend_crop_yield)

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

# N3 represents the optimistic null - where the manager budget is increasing over time but the user budget remains constant


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


### Create and save budgets ####
## Run 1 ####


# these are the budgets used in the first run of 10 reps per scenario, above


# Here I want to create the user and manager budgets and save them so I have them as CSV's

### Scenario 1

## make user budget

# define slope 
xx <- 5204.1/1275

# empty vector
UB <- NULL

# starting value
UB[1] <- 400

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  UB[i] <- UB[i-1] + xx
}


# dataframe
S1_budgets <- data.frame(Time = 1:50,
                         Manager_budget = rep(500, times = 50),
                         User_budget = UB)



### Scenario 2


# define slope 
xx <- 5204.1/1275

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 400

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))


# Dataframe
S2_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB2,
                         User_budget = UB)



### Scenario 3

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 65*sin(1.33*s3+0)+400
MB3 <- MB3[1:50]

# standardise to a total cumulative budget of 25,000
MB3 <- 25000*(MB3/sum(MB3))

# dataframe
S3_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB3,
                         User_budget = UB)



### Scenario 4

## Define manager budget
s4 <- seq(0,50,1)
MB4 <- 50*sin(2.5*s4+0)+500
MB4 <- MB4[1:50]

# Standardise so that cumulative total budget = 25,000
MB4 <- 25000*(MB4/sum(MB4))

# dataframe
S4_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB4,
                         User_budget = UB)




### Scenario 5

## Define manager budgets

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
  
  str <- seq(10, 30, 0.2)
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


# Dataframe
S5_budgets <- data.frame(Time = rep(1:50, times = 10),
                         Manager_budget = c(MB5.1,MB5.2,MB5.3,MB5.4,MB5.5,MB5.6,MB5.7,
                                            MB5.8,MB5.9,MB5.10),
                         User_budget = rep(UB, times=10))



### save budgets
write.csv(S1_budgets, "Budgets/Investment/Run_1/S1_budgets.csv")
write.csv(S2_budgets, "Budgets/Investment/Run_1/S2_budgets.csv")
write.csv(S3_budgets, "Budgets/Investment/Run_1/S3_budgets.csv")
write.csv(S4_budgets, "Budgets/Investment/Run_1/S4_budgets.csv")
write.csv(S5_budgets, "Budgets/Investment/Run_1/S5_budgets.csv")


## Run 2 ####


# Here I want to create the user and manager budgets and save them so I have them as CSV's

### Scenario 1

## make user budget

# define slope 
xx <- 5204.1/1275

# empty vector
UB <- NULL

# starting value
UB[1] <- 400

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  UB[i] <- UB[i-1] + xx
}


# dataframe
S1_budgets <- data.frame(Time = 1:50,
                         Manager_budget = rep(500, times = 50),
                         User_budget = UB)



### Scenario 2


# define slope 
xx <- 5204.1/1275

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 400

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))


# Dataframe
S2_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB2,
                         User_budget = UB)



### Scenario 3

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 65*sin(1.33*s3+0)+400
MB3 <- MB3[1:50]

# standardise to a total cumulative budget of 25,000
MB3 <- 25000*(MB3/sum(MB3))

# dataframe
S3_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB3,
                         User_budget = UB)



### Scenario 4

## Define manager budget
s4 <- seq(0,50,1)
MB4 <- 30*sin(2.5*s4+0)+500
MB4 <- MB4[1:50]

# Standardise so that cumulative total budget = 25,000
MB4 <- 25000*(MB4/sum(MB4))

# dataframe
S4_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB4,
                         User_budget = UB)




### Scenario 5

## Define manager budgets

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
  
  str <- seq(10, 30, 0.2)
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


# Dataframe
S5_budgets <- data.frame(Time = rep(1:50, times = 10),
                         Manager_budget = c(MB5.1,MB5.2,MB5.3,MB5.4,MB5.5,MB5.6,MB5.7,
                                            MB5.8,MB5.9,MB5.10),
                         User_budget = rep(UB, times=10))



### save budgets
write.csv(S1_budgets, "Budgets/Investment/Run_2/S1_budgets.csv")
write.csv(S2_budgets, "Budgets/Investment/Run_2/S2_budgets.csv")
write.csv(S3_budgets, "Budgets/Investment/Run_2/S3_budgets.csv")
write.csv(S4_budgets, "Budgets/Investment/Run_2/S4_budgets.csv")
write.csv(S5_budgets, "Budgets/Investment/Run_2/S5_budgets.csv")

## Run 3 ####

# Here I want to create the user and manager budgets and save them so I have them as CSV's

### Scenario 1

## make user budget

# define slope 
xx <- 20

# empty vector
UB <- NULL

# starting value
UB[1] <- 2000

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  UB[i] <- UB[i-1] + xx
}


# dataframe
S1_budgets <- data.frame(Time = 1:50,
                         Manager_budget = rep(500, times = 50),
                         User_budget = UB)



### Scenario 2


# define slope 
xx <- 1.5

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 500

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))


# Dataframe
S2_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB2,
                         User_budget = UB)



### Scenario 3

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 65*sin(1.33*s3+0)+400
MB3 <- MB3[1:50]

# standardise to a total cumulative budget of 25,000
MB3 <- 25000*(MB3/sum(MB3))

# dataframe
S3_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB3,
                         User_budget = UB)



### Scenario 4

## Define manager budget
s4 <- seq(0,50,1)
MB4 <- 30*sin(2.5*s4+0)+500
MB4 <- MB4[1:50]

# Standardise so that cumulative total budget = 25,000
MB4 <- 25000*(MB4/sum(MB4))

# dataframe
S4_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB4,
                         User_budget = UB)




### Scenario 5

## Define manager budgets

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
  
  str <- seq(10, 30, 0.2)
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


# Dataframe
S5_budgets <- data.frame(Time = rep(1:50, times = 10),
                         Manager_budget = c(MB5.1,MB5.2,MB5.3,MB5.4,MB5.5,MB5.6,MB5.7,
                                            MB5.8,MB5.9,MB5.10),
                         User_budget = rep(UB, times=10))



### save budgets
write.csv(S1_budgets, "Budgets/Investment/Run_3/S1_budgets.csv")
write.csv(S2_budgets, "Budgets/Investment/Run_3/S2_budgets.csv")
write.csv(S3_budgets, "Budgets/Investment/Run_3/S3_budgets.csv")
write.csv(S4_budgets, "Budgets/Investment/Run_3/S4_budgets.csv")
write.csv(S5_budgets, "Budgets/Investment/Run_3/S5_budgets.csv")


# plot budgets
budgets_all <- rbind(S1_budgets,S2_budgets,S3_budgets,S4_budgets,S5_budgets)

s <- rep(c("1","2","3","4"), each=50)
t <- rep("5", times=500)

q <- c(s,t)

budgets_all$Scenario <- q


ggplot(budgets_all, aes(x=Time, y=Manager_budget, group=Scenario))+
  geom_line()+
  facet_wrap(~Scenario)





## Run 4 ####

# In run 3 I have increased the user budget a LOT, to try and make the scenarios more extreme and increase tree loss. I have successfully increased tree loss, but the scenarios are still basically identical. Here I am going to try and make the senarios more extreme and different from one another. I will have to sacrifice reality a bit.

### Scenario 1

## make user budget

# define slope 
xx <- 75

# empty vector
UB <- NULL

# starting value
UB[1] <- 2000

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  UB[i] <- UB[i-1] + xx
}


# dataframe
S1_budgets <- data.frame(Time = 1:50,
                         Manager_budget = rep(500, times = 50),
                         User_budget = UB)



### Scneario 2

# define slope 
xx <- 60

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 500

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))


# Dataframe
S2_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB2,
                         User_budget = UB)



### Scenario 3

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 350*sin(0.5*e+0)+400
MB3 <- MB3[1:50]

# standardise to a total cumulative budget of 25,000
MB3 <- 25000*(MB3/sum(MB3))

# dataframe
S3_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB3,
                         User_budget = UB)




### Scenario 4

# I am keeping S4 as it is because now that I have made S3 more extreme, S4 is as different as it can be in its current form

## Define manager budget
s4 <- seq(0,50,1)
MB4 <- 30*sin(2.5*s4+0)+500
MB4 <- MB4[1:50]

# Standardise so that cumulative total budget = 25,000
MB4 <- 25000*(MB4/sum(MB4))

# dataframe
S4_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB4,
                         User_budget = UB)




### Scenario 5

## Define manager budgets

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


# Dataframe
S5_budgets <- data.frame(Time = rep(1:50, times = 10),
                         Manager_budget = c(MB5.1,MB5.2,MB5.3,MB5.4,MB5.5,MB5.6,MB5.7,
                                            MB5.8,MB5.9,MB5.10),
                         User_budget = rep(UB, times=10))

check <- S5_budgets
check$wave <- rep(c("1","2","3","4","5","6","7","8","9","10"), each=50)

ggplot(check, aes(x=Time, y=Manager_budget, group=wave, color=wave))+
  geom_line(size=1)+
  facet_wrap(~wave)



### save budgets
write.csv(S1_budgets, "Budgets/Investment/Run_4/S1_budgets.csv")
write.csv(S2_budgets, "Budgets/Investment/Run_4/S2_budgets.csv")
write.csv(S3_budgets, "Budgets/Investment/Run_4/S3_budgets.csv")
write.csv(S4_budgets, "Budgets/Investment/Run_4/S4_budgets.csv")
write.csv(S5_budgets, "Budgets/Investment/Run_4/S5_budgets.csv")


# plot budgets
budgets_all <- rbind(S1_budgets,S2_budgets,S3_budgets,S4_budgets,S5_budgets)

s <- rep(c("1","2","3","4"), each=50)
t <- rep("5", times=500)

q <- c(s,t)

budgets_all$Scenario <- q


ggplot(budgets_all, aes(x=Time, y=Manager_budget, group=Scenario))+
  geom_line()+
  facet_wrap(~Scenario)



#
## Run 5 ####

# after a chat with Nils, we have decided on a slightly different approach. We are goin to remove S4, as it is not really contributing much. Thinking about the results, and the message, we are going to have two clean, clear plots. The first one is going to have just S1, S2, and S3. The results are nicely different, and the differences are easy to see. We can talk easily about the issues with S2 and S3. With S2, if you start by underfunding a project, even though the manager budget increases well beyond the MB in S1, you are constantly playing catch up and the two scenarios don't get close to one another until after the 50 years. This sends a clear warning to funders about early funding. Scenario 3 shows that in periods of high funding, you are able to play catchup with S1 a bit, but the longer it goes on, the worse the impacts of the troughs. Also, if a projet is only monitoring for say, 5 years, then you don't know where on that rajectory you are, and this could lead to a false sense of success.

# so we are going to drop S4. But we are going to add a second version of S5, where the uncertainty/variability is even greater. This will then form the second plot, which has only the two random wave scenarios (S5, and the new one). This will hopefully show that if there is not much uncertainty, then actually things aren't that bad, and projects can be close to as successful as S1. However, we assume that when there is much more uncertainty, then things can go badly wrong.

# S1, S2, and S3 will remain the same


### Scenario 1

## make user budget

# define slope 
xx <- 75

# empty vector
UB <- NULL

# starting value
UB[1] <- 2000

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  UB[i] <- UB[i-1] + xx
}


# dataframe
S1_budgets <- data.frame(Time = 1:50,
                         Manager_budget = rep(500, times = 50),
                         User_budget = UB)



### Scneario 2

# define slope 
xx <- 60

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 500

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))


# Dataframe
S2_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB2,
                         User_budget = UB)



### Scenario 3

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 350*sin(0.5*s3+0)+400
MB3 <- MB3[1:50]

# standardise to a total cumulative budget of 25,000
MB3 <- 25000*(MB3/sum(MB3))

# dataframe
S3_budgets <- data.frame(Time = 1:50,
                         Manager_budget = MB3,
                         User_budget = UB)





### Scenario 4 (old S5)

## Define manager budgets

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
names <- c("MB4.1","MB4.2","MB4.3","MB4.4","MB4.5","MB4.6","MB4.7",
           "MB4.8","MB4.9","MB4.10")

names(r_waves_traj) <- names

# standardise to a total cumulative budget of 25,000
r_waves_traj <- lapply(r_waves_traj, function(x){25000*(x/sum(x))})

# extract to global environment
list2env(r_waves_traj, globalenv())


# Dataframe
S4_budgets <- data.frame(Time = rep(1:50, times = 10),
                         Manager_budget = c(MB4.1,MB4.2,MB4.3,MB4.4,MB4.5,MB4.6,MB4.7,
                                            MB4.8,MB4.9,MB4.10),
                         User_budget = rep(UB, times=10))

check <- S4_budgets
check$wave <- rep(c("1","2","3","4","5","6","7","8","9","10"), each=50)

ggplot(check, aes(x=Time, y=Manager_budget, group=wave, color=wave))+
  geom_line(size=1)+
  facet_wrap(~wave)



### Scenario 5 (new)

# NOTE - need to run the "f", "plot.fourier", and "random_wave" functions above in S4 before running the below


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

# extract to global environment
list2env(r_waves_traj2, globalenv())


# Dataframe
S5_budgets <- data.frame(Time = rep(1:50, times = 10),
                         Manager_budget = c(MB5.1,MB5.2,MB5.3,MB5.4,MB5.5,MB5.6,MB5.7,
                                            MB5.8,MB5.9,MB5.10),
                         User_budget = rep(UB, times=10))

check5 <- S5_budgets
check5$wave <- rep(c("1","2","3","4","5","6","7","8","9","10"), each=50)

ggplot(check5, aes(x=Time, y=Manager_budget, group=wave, color=wave))+
  geom_line(size=1)+
  facet_wrap(~wave)


s4.plot <- ggplot(check, aes(x=Time, y=Manager_budget, group=wave, color=wave))+
  geom_line(size=1)+
  ylim(0,1250)

s5.plot <-  ggplot(check5, aes(x=Time, y=Manager_budget, group=wave, color=wave))+
  geom_line(size=1)+
  ylim(0,1250)

s4.plot + s5.plot


### save budgets
write.csv(S1_budgets, "Budgets/Investment/Run_5/S1_budgets.csv")
write.csv(S2_budgets, "Budgets/Investment/Run_5/S2_budgets.csv")
write.csv(S3_budgets, "Budgets/Investment/Run_5/S3_budgets.csv")
write.csv(S4_budgets, "Budgets/Investment/Run_5/S4_budgets.csv")
write.csv(S5_budgets, "Budgets/Investment/Run_5/S5_budgets.csv")

#
  ### FIRST RUN ####
#### SCENARIO 1 ################

# User budget increases, manager budget remains constant

# The simulation code is repeated so that I can let it run and it will re-run the same sim but I have coded it to save outputs with different names


### WARNING - don't run the below scenarios without adding lambda=0


### RUN 1
UB  <- 400
UBR <- 40

Scen1_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = 500, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen1 <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen1_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  Scen1[time_step, 1] <- time_step
  Scen1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen1[time_step, 6] <- UB
  
  Scen1_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
}

colnames(Scen1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget")
Scen1_1_summary <- data.frame(Scen1)

rm(Scen1_sim_old)
rm(Scen1)




### RUN 2
UB  <- 400
UBR <- 40

Scen1_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = 500, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen1 <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen1_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  Scen1[time_step, 1] <- time_step
  Scen1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen1[time_step, 6] <- UB
  
  Scen1_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
}

colnames(Scen1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget")
Scen1_2_summary <- data.frame(Scen1)

rm(Scen1_sim_old)
rm(Scen1)





### RUN 3
UB  <- 400
UBR <- 40

Scen1_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = 500, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen1 <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen1_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  Scen1[time_step, 1] <- time_step
  Scen1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen1[time_step, 6] <- UB
  
  Scen1_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
}

colnames(Scen1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget")
Scen1_3_summary <- data.frame(Scen1)

rm(Scen1_sim_old)
rm(Scen1)




### RUN 4
UB  <- 400
UBR <- 40

Scen1_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = 500, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen1 <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen1_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  Scen1[time_step, 1] <- time_step
  Scen1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen1[time_step, 6] <- UB
  
  Scen1_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
}

colnames(Scen1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget")
Scen1_4_summary <- data.frame(Scen1)

rm(Scen1_sim_old)
rm(Scen1)




### RUN 5
UB  <- 400
UBR <- 40

Scen1_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = 500, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen1 <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen1_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  Scen1[time_step, 1] <- time_step
  Scen1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen1[time_step, 6] <- UB
  
  Scen1_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
}

colnames(Scen1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget")
Scen1_5_summary <- data.frame(Scen1)

rm(Scen1_sim_old)
rm(Scen1)

# combine all summaries
Scen1_all_summary <- rbind(Scen1_1_summary,Scen1_2_summary,Scen1_3_summary,Scen1_4_summary,Scen1_5_summary)

# save
write.csv(Scen1_all_summary, file = "outputs/investment/scenarios/Scen1_all_summary.csv")



#### SCENARIO 2 ####

#### Manager and user budget increase, with the same slope


### define manager budget

# define slope 
xx <- 5204.1/1275

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 400

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))





### RUN 1

UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_1_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)



### RUN 2
UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_2_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)



### RUN 3
UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_3_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)




### RUN 4
UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_4_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)




### RUN 5
UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_5_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)


# combine results
Scen2_all_summary <- rbind(Scen2_1_summary,Scen2_2_summary,Scen2_3_summary,Scen2_4_summary,Scen2_5_summary)

write.csv(Scen2_all_summary, file = "outputs/investment/scenarios/Scen2_all_summary.csv")


#### SCENARIO 3 ####

# Manager budget increases and decreases in a predictable/regular way above and below a mean (like a sine wave), user budget increases linearly

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 65*sin(1.33*s3+0)+400
MB3 <- MB3[1:50]

# standardise to a total cumulative budget of 25,000
MB3 <- 25000*(MB3/sum(MB3))



### RUN 1

UB  <- 400
UBR <- 40

MB <- MB3[1]


Scen3_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen3 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen3[time_step, 1] <- time_step
  Scen3[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen3[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen3[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen3[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen3[time_step, 6] <- UB
  Scen3[time_step, 7] <- MB
  
  Scen3_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB3[time_step]
}

colnames(Scen3) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen3_1_summary <- data.frame(Scen3)

rm(Scen3_sim_old)
rm(Scen3)




### RUN 2
UB  <- 400
UBR <- 40

MB <- MB3[1]


Scen3_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen3 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen3[time_step, 1] <- time_step
  Scen3[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen3[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen3[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen3[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen3[time_step, 6] <- UB
  Scen3[time_step, 7] <- MB
  
  Scen3_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB3[time_step]
}

colnames(Scen3) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen3_2_summary <- data.frame(Scen3)

rm(Scen3_sim_old)
rm(Scen3)



### RUN 3
UB  <- 400
UBR <- 40

MB <- MB3[1]


Scen3_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen3 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen3[time_step, 1] <- time_step
  Scen3[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen3[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen3[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen3[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen3[time_step, 6] <- UB
  Scen3[time_step, 7] <- MB
  
  Scen3_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB3[time_step]
}

colnames(Scen3) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen3_3_summary <- data.frame(Scen3)

rm(Scen3_sim_old)
rm(Scen3)



### RUN 4
UB  <- 400
UBR <- 40

MB <- MB3[1]


Scen3_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen3 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen3[time_step, 1] <- time_step
  Scen3[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen3[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen3[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen3[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen3[time_step, 6] <- UB
  Scen3[time_step, 7] <- MB
  
  Scen3_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB3[time_step]
}

colnames(Scen3) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen3_4_summary <- data.frame(Scen3)

rm(Scen3_sim_old)
rm(Scen3)



### RUN 5
UB  <- 400
UBR <- 40

MB <- MB3[1]


Scen3_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
Scen3 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen3[time_step, 1] <- time_step
  Scen3[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen3[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen3[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen3[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen3[time_step, 6] <- UB
  Scen3[time_step, 7] <- MB
  
  Scen3_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB3[time_step]
}

colnames(Scen3) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen3_5_summary <- data.frame(Scen3)

rm(Scen3_sim_old)
rm(Scen3)


## Combine all summaries
Scen3_all_summary <- rbind(Scen3_1_summary,Scen3_2_summary,Scen3_3_summary,Scen3_4_summary,Scen3_5_summary)

write.csv(Scen3_all_summary, file = "outputs/investment/scenarios/Scen3_all_summary.csv")


#### SCENARIO 4 #####

### As Scenario 3, but the wavelength is shorter (higher frequency) and the amplitude is smaller

## Define manager budget
s4 <- seq(0,50,1)
MB4 <- 50*sin(2.5*s4+0)+500
MB4 <- MB4[1:50]

# Standardise so that cumulative total budget = 25,000
MB4 <- 25000*(MB4/sum(MB4))



### RUN 1
UB  <- 400
UBR <- 40

MB <- MB4[1]


Scen4_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB4[time_step]
}

colnames(Scen4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen4_1_summary <- data.frame(Scen4)

rm(Scen4_sim_old)
rm(Scen4)





### RUN 2
UB  <- 400
UBR <- 40

MB <- MB4[1]


Scen4_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB4[time_step]
}

colnames(Scen4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen4_2_summary <- data.frame(Scen4)

rm(Scen4_sim_old)
rm(Scen4)




### RUN 3
UB  <- 400
UBR <- 40

MB <- MB4[1]


Scen4_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB4[time_step]
}

colnames(Scen4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen4_3_summary <- data.frame(Scen4)

rm(Scen4_sim_old)
rm(Scen4)




### RUN 4
UB  <- 400
UBR <- 40

MB <- MB4[1]


Scen4_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB4[time_step]
}

colnames(Scen4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen4_4_summary <- data.frame(Scen4)

rm(Scen4_sim_old)
rm(Scen4)



### RUN 5
UB  <- 400
UBR <- 40

MB <- MB4[1]


Scen4_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
  res_movement = 0, 
  agent_view = 150, 
  agent_move = 50, 
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
  user_budget = UB, 
  manager_budget = MB, 
  usr_budget_rng = UBR,  
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB4[time_step]
}

colnames(Scen4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen4_5_summary <- data.frame(Scen4)

rm(Scen4_sim_old)
rm(Scen4)


## Combine all summaries
Scen4_all_summary <- rbind(Scen4_1_summary,Scen4_2_summary,Scen4_3_summary,Scen4_4_summary,Scen4_5_summary)


write.csv(Scen4_all_summary, file = "outputs/investment/scenarios/Scen4_all_summary.csv")



#### SCENARIO 5 ####

### Manager budget is a random complex wave produced using an inverse Fourier Transform

## Define manager budget

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
  
  str <- seq(10, 30, 0.2)
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
UB  <- 400
UBR <- 40

MB <- MB5.1[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.1[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_1_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 2 - wave 2
UB  <- 400
UBR <- 40

MB <- MB5.2[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.2[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_2_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)



### RUN 3 - wave 3
UB  <- 400
UBR <- 40

MB <- MB5.3[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.3[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_3_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




## RUN 4 - wave 4
UB  <- 400
UBR <- 40

MB <- MB5.4[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.1[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_4_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 5 - wave 5
UB  <- 400
UBR <- 40

MB <- MB5.5[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.5[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_5_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)



### RUN 6 - wave 6
UB  <- 400
UBR <- 40

MB <- MB5.6[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.6[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_6_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 7 - wave 7
UB  <- 400
UBR <- 40

MB <- MB5.7[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.7[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_7_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 8 - wave 8
UB  <- 400
UBR <- 40

MB <- MB5.8[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.8[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_8_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)





### RUN 9 - wave 9
UB  <- 400
UBR <- 40

MB <- MB5.9[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.9[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_9_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 10 - wave 10
UB  <- 400
UBR <- 40

MB <- MB5.10[1]


Scen5_sim_old <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  time_max = 50,
  land_dim_1 = 150,
  land_dim_2 = 150, 
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
  manage_target = 1125000, 
  RESOURCE_ini = 1125000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.10[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_10_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)



## Add column to all summaries
Scen5_1_summary$wave <- 1
Scen5_2_summary$wave <- 2
Scen5_3_summary$wave <- 3
Scen5_4_summary$wave <- 4
Scen5_5_summary$wave <- 5
Scen5_6_summary$wave <- 6
Scen5_7_summary$wave <- 7
Scen5_8_summary$wave <- 8
Scen5_9_summary$wave <- 9
Scen5_10_summary$wave <- 10

## Combine results
Scen5_all_summary <- rbind(Scen5_1_summary,Scen5_2_summary,Scen5_3_summary,Scen5_4_summary,Scen5_5_summary,
                           Scen5_6_summary,Scen5_7_summary,Scen5_8_summary,Scen5_9_summary,Scen5_10_summary,)

write.csv(Scen5_all_summary, file="outputs/investment/scenarios/Scen5_all_summary.csv")
#### Results ####


### Load .csv files for the first 10 runs from scenarios 1:5. Run by Brad

# Scenario 1
scen1 <- list.files(path = "./outputs/investment/scenarios/Run_1/Scenario_1",
                    pattern = "*.csv",
                    full.names = T) %>% 
          map_df(~read_csv(.,))

scen1 <- scen1[ ,-1]
scen1$Simulation <- rep(as.factor(1:10), each=50)
scen1$Manager_budget <- 500
scen1 <- scen1 %>% select(Time,Trees,Trees_est,Cull_cost,Cull_count,User_budget, Manager_budget,Simulation)


# Scenario 2
scen2 <- list.files(path = "./outputs/investment/scenarios/Run_1/Scenario_2",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen2 <- scen2[ ,-1]
scen2$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 3
scen3 <- list.files(path = "./outputs/investment/scenarios/Run_1/Scenario_3",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen3 <- scen3[ ,-1]
scen3$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 4
scen4 <- list.files(path = "./outputs/investment/scenarios/Run_1/Scenario_4",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen4 <- scen4[ ,-1]
scen4$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 5
scen5 <- read.csv("outputs/investment/scenarios/Run_1/Scenario_5/Scen5_all_summary.csv", header = T)
scen5 <- scen5[ ,-1]
scen5 <- scen5 %>% rename(Simulation = wave)
scen5$Simulation <- as.factor(scen5$Simulation)



### plot individually

# create list of all dataframes
dat_list <- list(scen1,scen2,scen3,scen4,scen5)

# plot functions
plot.trees <- function(dat){
 plot <- ggplot(dat, aes(x=Time, y=Trees, group=Simulation, color=Simulation))+
                      geom_line(size=1)+
                      theme_classic()+
                      ylim(1116000,1125000)
 return(plot)
}

plot.cull.count <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_count, group=Simulation, color=Simulation))+
            geom_line(size=1)+
            theme_classic()+
            ylab("Cull count")
          
  return(plot)
}

plot.cull.cost <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_cost, group=Simulation, color=Simulation))+
            geom_line(size=1)+
            theme_classic()+
            ylab("Cull cost")
            
  return(plot)
}

# for scenarios 1:4
plot.budgets <- function(dat){
  
  require('patchwork')
  
  dat2 <- dat[1:50, ]
  
  plot1 <- ggplot(dat2, aes(x=Time, y=Manager_budget))+
            geom_line(size=1, color="dodgerblue3")+
            theme_classic()+
            ylab("Manager budget")
  
  return(plot1)
}

# for scenario 5
plot.budgets2 <- function(dat){
  
  require('patchwork')
  plot1 <- ggplot(dat, aes(x=Time, y=Manager_budget, group=Simulation, color=Simulation))+
            geom_line(size=1)+
            theme_classic()+
            ylab("Manager budget")
  
  
  return(plot1)
}


# apply functions to list of data
trees_plots      <- lapply(dat_list, plot.trees)
cull_count_plots <- lapply(dat_list, plot.cull.count) 
cull_cost_plots  <- lapply(dat_list, plot.cull.cost)
budget_plots_1_4 <- lapply(dat_list[1:4], plot.budgets)
budget_plots_5   <- lapply(dat_list[5], plot.budgets2)

# re-name list elements
names.trees <- c("scen1.treePlot","scen2.treePlot","scen3.treePlot","scen4.treePlot","scen5.treePlot")
names(trees_plots) <- names.trees

names.count <- c("scen1.countPlot","scen2.countPlot","scen3.countPlot","scen4.countPlot","scen5.countPlot")
names(cull_count_plots) <- names.count

names.cost <- c("scen1.costPlot","scen2.costPlot","scen3.costPlot","scen4.costPlot","scen5.costPlot")
names(cull_cost_plots) <- names.cost

names.budets.14 <- c("scen1.budgetPlots","scen2.budgetPlots","scen3.budgetPlots","scen4.budgetPlots")
names(budget_plots_1_4) <- names.budets.14

names.budgets.5 <- "scen5.budgetPlots"
names(budget_plots_5) <- names.budgets.5

# extract elements to gloal environment
list2env(trees_plots, globalenv())
list2env(cull_count_plots, globalenv())
list2env(cull_cost_plots, globalenv())
list2env(budget_plots_1_4, globalenv())
list2env(budget_plots_5, globalenv())

# tree count plots
tree.plot.all <- scen1.treePlot + scen2.treePlot + scen3.treePlot + scen4.treePlot + scen5.treePlot
tree.plot.all[[1]] <- tree.plot.all[[1]] + ggtitle("Scenario 1")
tree.plot.all[[2]] <- tree.plot.all[[2]] + ggtitle("Scenario 2")
tree.plot.all[[3]] <- tree.plot.all[[3]] + ggtitle("Scenario 3")
tree.plot.all[[4]] <- tree.plot.all[[4]] + ggtitle("Scenario 4")
tree.plot.all[[5]] <- tree.plot.all[[5]] + ggtitle("Scenario 5")

# cull count plots
cull.count.plot.all <- scen1.countPlot+scen2.countPlot+scen3.countPlot+scen4.countPlot+scen5.countPlot
cull.count.plot.all[[1]] <- cull.count.plot.all[[1]] + ggtitle("Scenario 1")
cull.count.plot.all[[2]] <- cull.count.plot.all[[2]] + ggtitle("Scenario 2")
cull.count.plot.all[[3]] <- cull.count.plot.all[[3]] + ggtitle("Scenario 3")
cull.count.plot.all[[4]] <- cull.count.plot.all[[4]] + ggtitle("Scenario 4")
cull.count.plot.all[[5]] <- cull.count.plot.all[[5]] + ggtitle("Scenario 5")

# cull cost plots
cull.cost.plot.all <- scen1.costPlot+scen2.costPlot+scen3.costPlot+scen4.costPlot+scen5.costPlot
cull.cost.plot.all[[1]] <- cull.cost.plot.all[[1]] + ggtitle("Scenario 1")
cull.cost.plot.all[[2]] <- cull.cost.plot.all[[2]] + ggtitle("Scenario 2")
cull.cost.plot.all[[3]] <- cull.cost.plot.all[[3]] + ggtitle("Scenario 3")
cull.cost.plot.all[[4]] <- cull.cost.plot.all[[4]] + ggtitle("Scenario 4")
cull.cost.plot.all[[5]] <- cull.cost.plot.all[[5]] + ggtitle("Scenario 5")

# budget plots

user_budget_p <- ggplot(scen1, aes(x=Time, y=User_budget))+
                  geom_line(size=1, color="firebrick3")+
                  theme_classic()+
                  ylab("User budget")

budget.plot.all <- user_budget_p+scen1.budgetPlots+scen2.budgetPlots+scen3.budgetPlots+
                   scen4.budgetPlots+scen5.budgetPlots
budget.plot.all[[1]] <- budget.plot.all[[1]] + ggtitle("User budget")
budget.plot.all[[2]] <- budget.plot.all[[2]] + ggtitle("Scenario 1")
budget.plot.all[[3]] <- budget.plot.all[[3]] + ggtitle("Scenario 2")
budget.plot.all[[4]] <- budget.plot.all[[4]] + ggtitle("Scenario 3")
budget.plot.all[[5]] <- budget.plot.all[[5]] + ggtitle("Scenario 4")
budget.plot.all[[6]] <- budget.plot.all[[6]] + ggtitle("Scenario 5")

#ggsave("outputs/investment/scenarios/Plots/budget_all.png", budget.plot.all, 
 #      dpi=300, width = 30, height=20, units="cm")

### Plots for each scenario together
scen1_plots <- user_budget_p + scen1.budgetPlots + scen1.countPlot + scen1.costPlot + scen1.treePlot
scen2_plots <- user_budget_p + scen2.budgetPlots + scen2.countPlot + scen2.costPlot + scen2.treePlot
scen3_plots <- user_budget_p + scen3.budgetPlots + scen3.countPlot + scen3.costPlot + scen3.treePlot
scen4_plots <- user_budget_p + scen4.budgetPlots + scen4.countPlot + scen4.costPlot + scen4.treePlot
scen5_plots <- user_budget_p + scen5.budgetPlots + scen5.countPlot + scen5.costPlot + scen5.treePlot

#ggsave("outputs/investment/scenarios/Scenario_1/scen1_plots.png", scen1_plots, 
 #      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Scenario_2/scen2_plots.png", scen2_plots, 
  #     dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Scenario_3/scen3_plots.png", scen3_plots, 
 #      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Scenario_4/scen4_plots.png", scen4_plots, 
 #      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Scenario_5/scen5_plots.png", scen5_plots, 
 #      dpi=300, width = 30, height=20, units="cm")


## plot S5 cull count plot but with facets
S5_cull_count <- ggplot(scen5, aes(x=Time, y=Cull_count))+
                  geom_line()+
                  facet_wrap(~Simulation)+
                  theme_classic()

#ggsave("outputs/investment/scenarios/Scenario_5/S5_cullCount_facets.png", S5_cull_count, 
 #      dpi=300, width = 30, height=20, units="cm")


### get mean and error bars for each scenario

# Function to extract 50, 2.5, and 97.5% quantiles from each scenario
quant.func <- function(dat){
  wide.dat <- pivot_wider(dat,id_cols = Time, names_from = Simulation, values_from = Trees)
  wide.dat <- wide.dat[ ,-1]
  Mean.q   <- apply(wide.dat,1,quantile,probs=0.5)
  LCL.q    <- apply(wide.dat,1,quantile,probs=0.025)
  UCL.q    <- apply(wide.dat,1,quantile,probs=0.975)
  
  quant.df <- data.frame(Time = 1:50,
                         Mean = Mean.q,
                         LCL = LCL.q,
                         UCL = UCL.q)
  return(quant.df)
}

# apply function to all scenario dataframes
quants.ls <- lapply(dat_list, quant.func)
names <- c("scen1_quants","scen2_quants","scen3_quants","scen4_quants","scen5_quants")
names(quants.ls) <- names

# extract to global env
list2env(quants.ls, globalenv())

# add simulation
scen1_quants$Simulation <- "1"
scen2_quants$Simulation <- "2"
scen3_quants$Simulation <- "3"
scen4_quants$Simulation <- "4"
scen5_quants$Simulation <- "5"

# merge 
quants_all <- rbind(scen1_quants,scen2_quants,scen3_quants,scen4_quants,scen5_quants)


# plot (facets)
all_facets_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
                      geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
                      geom_line(size=1,aes(color=Simulation))+
                      facet_wrap(~Simulation)+
                      theme_classic()

#ggsave("outputs/investment/scenarios/Plots/All_facets_ribbons.png", all_facets_ribbon,
 #      dpi=300, width = 30, height = 20, units="cm")
  

# plot no facets
all_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
                    geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
                    geom_line(size=1,aes(color=Simulation))+
                    theme_classic()
#ggsave("outputs/investment/scenarios/Plots/All_ribbons.png", all_ribbon,
 #      dpi=300, width = 30, height = 20, units="cm")


# zoom in on the end
all_zoom <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
            #geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
            geom_line(size=1,aes(color=Simulation))+
            theme_classic()+
            ylim(1115500,1118000)+
            xlim(40,50)


#ggsave("outputs/investment/scenarios/Plots/All_zoom.png", all_zoom,
 #      dpi=300, width = 30, height = 20, units="cm")




### what is the absolute difference in trees lost by the end?
final_trees <- data.frame(Scenario = 1:5,
                          Trees = c(min(quants_all$Mean[quants_all$Simulation=="1"]),
                                    min(quants_all$Mean[quants_all$Simulation=="2"]),
                                    min(quants_all$Mean[quants_all$Simulation=="3"]),
                                    min(quants_all$Mean[quants_all$Simulation=="4"]),
                                    min(quants_all$Mean[quants_all$Simulation=="5"])),
                          LCL = c(min(quants_all$LCL[quants_all$Simulation=="1"]),
                                  min(quants_all$LCL[quants_all$Simulation=="2"]),
                                  min(quants_all$LCL[quants_all$Simulation=="3"]),
                                  min(quants_all$LCL[quants_all$Simulation=="4"]),
                                  min(quants_all$LCL[quants_all$Simulation=="5"])),
                          UCL = c(min(quants_all$UCL[quants_all$Simulation=="1"]),
                                  min(quants_all$UCL[quants_all$Simulation=="2"]),
                                  min(quants_all$UCL[quants_all$Simulation=="3"]),
                                  min(quants_all$UCL[quants_all$Simulation=="4"]),
                                  min(quants_all$UCL[quants_all$Simulation=="5"])))

# plot
final_trees_plot <- ggplot(final_trees, aes(x = Scenario, y=Trees))+
                    geom_point(size=5)+
                    geom_errorbar(aes(ymin=LCL, ymax=UCL, width=0.2))+
                    ylab("Trees remaining")+
                    theme_classic()

#ggsave("outputs/investment/scenarios/Plots/Final_trees.png", final_trees_plot,
 #      dpi=300, width = 30, height = 20, units="cm")


# there is only a differene of 273 trees between the best outcome (S1) and the worst (S3). 


## what is the average number of trees lost over the time period?
s1.diff <- max(quants_all$Mean[quants_all$Simulation=="1"]) - min(quants_all$Mean[quants_all$Simulation=="1"])
s2.diff <- max(quants_all$Mean[quants_all$Simulation=="2"]) - min(quants_all$Mean[quants_all$Simulation=="2"])
s3.diff <- max(quants_all$Mean[quants_all$Simulation=="3"]) - min(quants_all$Mean[quants_all$Simulation=="3"])
s4.diff <- max(quants_all$Mean[quants_all$Simulation=="4"]) - min(quants_all$Mean[quants_all$Simulation=="4"])
s5.diff <- max(quants_all$Mean[quants_all$Simulation=="5"]) - min(quants_all$Mean[quants_all$Simulation=="5"])

mean(c(s1.diff,s2.diff,s3.diff,s4.diff,s5.diff))
# 7755 trees

# what is that in hectares and km2 of forest?
7755/50
# 155 hectares

155/100
# 1.55 km2



### interpretation

# Firstly, I don't think that there are sufficient trees being lost. On average, assuming approximately 50 trees/cell (which is what we have assumed), only 155 ha (or 1.55km2) were lost over the entire 50 years! This is not realistic, nor is it particulaly easy to see differences. This is because there are too many trees and not enough users. Because we are assuming a single user is a village, we only have 20 users. But in GMSE terms, this means that there just aren't that many trees being cut down. I think we need to reduce the size of the landscape, and therefore the number of trees, and perhaps increase the number of villages too.

# The shape of the tree curve is remarkably similar in all scenarios. S2 is the only different one - it is a linear slope. This makes sense as the manager and user budgets track each other. If you look at the S2 plots, you see that the cull count remains stable all the way along - both user and manager are using their budgets, but the difference in budgets never changes and so one never gets the upper hand over the other. S2 does not perform well overall, it is the 2nd worst result. This is not what I was expecting.

# I had assumed S1 would be the worst, but it is in fact the best! So this is likely caused, at least in part, by the fact that for the first 25 time steps of S1, the manager has a higher budget than the user. This is reflected in the cull count - it starts on 120 whereas for S2 it starts on 160. I think I need to change it so that the manager budget is on 400, otherwise that is not fair.I am not entirely sure why I made the manager budget in this scenario start at 500. The user budget starts as 400 in every scenario, and so this one should match I think.

# The next best is S5. Surprisingly not that much variation (seen in the ribbons). If you look at the cull count split up by facets, there is a lot of variation in the cull count (way more than the other scenarios) and so I am not sure why there is less variation than, say S2, which I would have asssumed would have the least variation as the budgets are the same! Am I missing something? 

# The third best is S4. When you look at the manager budgets for S3 and S4 (the two sine waves), they are actually too similar in terms of magnitude. S4 is supposed to be much smaller peaks (i.e. smaller grants). I think I need to change this to make the difference between S3 and S4 larger. 

# S3 is in fact the worst - which is interesting from a conservation funding perspective, as this is the dominant funding model! 
### Harvest under maximum conflict - run 1 ####

# This is Brad's idea to get a better understanding of the power dynamics that are going on under the hood. This was in response to some unexpected results in the above first runs. The harvest under maximum conflict is a single value for each time step that is based on the manager and user budgets in each time step, and it is the maximum number of trees a user can harvest if the manager uses all of their budget to reduce culling and the user uses all of their budget/power to cull.

# The manager uses 10 budget points to increase the cost of culling by 1
# So for example, if the manager and user budgets are both 1000...
# The manager can increase the cost of culling by 100 (1000/10)
# There is always the minimum cost of an action = 10
# Therefore the actual cost of culling is 110 (manager's increase + the minimum cost)
# Therefore the maximum number of trees the user can cull is 9.09 (user budget/cost = 1000/110)


### I will calculate this value for each scenario and plot it on or alongside the manager budget plots for each scenario

# Load budgets
S1_budgets <- read.csv("Budgets/Investment/Run_1/S1_budgets.csv", header = T)
S2_budgets <- read.csv("Budgets/Investment/Run_1/S2_budgets.csv", header = T)
S3_budgets <- read.csv("Budgets/Investment/Run_1/S3_budgets.csv", header = T)
S4_budgets <- read.csv("Budgets/Investment/Run_1/S4_budgets.csv", header = T)
S5_budgets <- read.csv("Budgets/Investment/Run_1/S5_budgets.csv", header = T)

# function to calculate harvest under maximum conflict
HUMC.func <- function(dat){
  
  dat$cost <- (dat$Manager_budget/10)+10
  dat$Max_harvest <- dat$User_budget/dat$cost
  
  return(dat)
}

# Put budgets into a list
budget_list <- list(S1_budgets,S2_budgets,S3_budgets,S4_budgets,S5_budgets)

# apply function
budget_list <- lapply(budget_list, HUMC.func)

# rename list elements
names <- c("S1_budgets","S2_budgets","S3_budgets","S4_budgets","S5_budgets")
names(budget_list) <- names

# unlist to environment
list2env(budget_list, globalenv())


# add simulation to scenario 5
S5_budgets$Simulation <- rep(1:10, each=50)

## Plots

# scenario 1
p.S1.MaxH <- ggplot(S1_budgets, aes(x=Time, y=Max_harvest))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12))+
              ylab("Harvest under maximum conflict")

# Add additional plots from S1 (produced in the "results" section above)
S1.plots <- user_budget_p + scen1.budgetPlots + scen1.countPlot + p.S1.MaxH
S1.plots[[1]] <- S1.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S1.plots[[2]] <- S1.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S1.plots[[3]] <- S1.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S1_plots.png", S1.plots, dpi=300, width = 30, height=20, units="cm")


# scenario 2
p.S2.MaxH <- ggplot(S2_budgets, aes(x=Time, y=Max_harvest))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12))+
              ylab("Harvest under maximum conflict")

# Add additional plots from S2 (produced in the "results" section above)
S2.plots <- user_budget_p + scen2.budgetPlots + scen2.countPlot + p.S2.MaxH
S2.plots[[1]] <- S2.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S2.plots[[2]] <- S2.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S2.plots[[3]] <- S2.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S2_plots.png", S2.plots, dpi=300, width = 30, height=20, units="cm")


# scenario 3
p.S3.MaxH <- ggplot(S3_budgets, aes(x=Time, y=Max_harvest))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12))+
              ylab("Harvest under maximum conflict")

# Add additional plots from S3 (produced in the "results" section above)
S3.plots <- user_budget_p + scen3.budgetPlots + scen3.countPlot + p.S3.MaxH
S3.plots[[1]] <- S3.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S3.plots[[2]] <- S3.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S3.plots[[3]] <- S3.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S3_plots.png", S3.plots, dpi=300, width = 30, height=20, units="cm")


# scenario 4
p.S4.MaxH <- ggplot(S4_budgets, aes(x=Time, y=Max_harvest))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12))+
              ylab("Harvest under maximum conflict")

# Add additional plots from S4 (produced in the "results" section above)
S4.plots <- user_budget_p + scen4.budgetPlots + scen4.countPlot + p.S4.MaxH
S4.plots[[1]] <- S4.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S4.plots[[2]] <- S4.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S4.plots[[3]] <- S4.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S4_plots.png", S4.plots, dpi=300, width = 30, height=20, units="cm")


# scenario 5
S5_budgets$Simulation <- as.factor(S5_budgets$Simulation)
p.S5.MaxH <- ggplot(S5_budgets, aes(x=Time, y=Max_harvest, group=Simulation, color=Simulation))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12))+
              ylab("Harvest under maximum conflict")

# Add additional plots from S5 (produced in the "results" section above)
S5.plots <- user_budget_p + scen5.budgetPlots + scen5.countPlot + p.S5.MaxH
S5.plots[[1]] <- S5.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S5.plots[[2]] <- S5.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
S5.plots[[3]] <- S5.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S5_plots.png", S5.plots, dpi=300, width = 30, height=20, units="cm")


## plot all HUMC together

# first take a mean for S5 to reduce it to a single line
S5_mean <- S5_budgets %>% group_by(Time) %>% summarise_at(.,vars(Max_harvest),mean)
S5_mean$Scenario <- "5"

# merge data
HUMC_all <- data.frame(Time = 1:50,
                       Max_harvest = c(S1_budgets$Max_harvest, S2_budgets$Max_harvest,
                                       S3_budgets$Max_harvest, S4_budgets$Max_harvest,
                                       S5_mean$Max_harvest),
                       Scenario = rep(c("1","2","3","4","5"), each=50))

humc_all <- ggplot(HUMC_all, aes(x=Time, y=Max_harvest, group=Scenario, color=Scenario))+
            geom_line(size=1)+
            theme_classic()+
            theme(axis.title = element_text(size=15),
                  axis.text = element_text(size=12))+
            ylab("Harvest under maximum conflict")

#ggsave("outputs/investment/scenarios/HUMC/humc_all.png", humc_all, dpi=300, width = 30, height=20, units="cm")


## floor all of the Max_harvest values (as users can't harvest half a tree)
HUMC_all$Floor <- floor(HUMC_all$Max_harvest)

humc_all_floor <- ggplot(HUMC_all, aes(x=Time, y=Floor, group=Scenario, color=Scenario))+
                    geom_line(size=1)+
                    facet_wrap(~Scenario)+
                    theme_classic()+
                    theme(axis.title = element_text(size=15),
                          axis.text = element_text(size=12))+
                    ylab("Floored harvest under maximum conflict")

#ggsave("outputs/investment/scenarios/HUMC/humc_all_floor.png", humc_all_floor, dpi=300, width = 30, height=20, units="cm")

  ### SECOND RUN ####

# based on the initial results (from 10 runs of each of the above 5 scenarios) I have decided to make a few changes to the scenarios. Changes detailed below:

# I am going to make the landscape smaller, with fewer trees. This is because there are so many tree and so few users that the total number of trees that are being culled is really small. 

# I will reduce the landscape to 100 x 100, and increase the number of villages to 30. This means the landscape is 10,000ha (100km2). Each village would theoretically have approximately 3.3km2 of land. Although it's worth remembering that this is a simulation, and we are trying to be quite extreme, rather than being super realistic.

# if we assume still 50 trees per cell the we have 500,000 trees

# I was considering changing the manager budget in S1 from 500 to 400, so that the manager started with the same budget as the user. I think I initially chose 500 because most of the other manager budgets started at around 500. I think that it makes more sense for all of the manager budgets to start at around the same value, rather than trying to match the manager budget with the user budget. 

# I am also going to increase the slope of the user budget. Previously, I had the user budget standardised to a total cumulative budget that matched the managers. Seeing as the user and manager budgets are NOT equal, equivalent, or even proportional, there is no need for the users to be standardised. In fact, if I am wanting more extreme results, and more trees cut down, I actually DONT want the user budget to be standardised. The amount of increase in the user budget is fairly arbitrary anyway - the budget isn't explicitly mapped out to represent any particular value of population increase. It is more just conceptually representing population increase.

# Scrap the above, I am going to stick with the same user budget and hope that the smaller landscape, fewer trees, and more user will make the results more extreme. By changing the user budget slope, I then have to change the MB slope in S2, which when standardised to 25000 throws it completely off so that it starts really low and doesn't even have the same slope as the UB. I could increase the TCB for all manager budgets, but then the MB in S2 starts waaay below the others (~300, versus >500). This will throw things off even more I think.


# I also need to fix the S4 sine wave. It's too similar to S3. The amplitude is not small enough



#### SCENARIO 1 ####


# This is what each replicate's code will look like. 

UB  <- 400
UBR <- 40

Scen1_sim_old <- gmse_apply(
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
  manager_budget = 500, 
  usr_budget_rng = UBR,  
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
Scen1 <- matrix(data=NA, nrow=50, ncol=6)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen1_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  Scen1[time_step, 1] <- time_step
  Scen1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen1[time_step, 6] <- UB
  
  Scen1_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
}

colnames(Scen1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget")
Scen1_1_summary <- data.frame(Scen1)

rm(Scen1_sim_old)
rm(Scen1)


#### SCENARIO 2 ####

### define manager budget

# define slope 
xx <- 5204.1/1275

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 400

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))


### RUN 

UB  <- 400
UBR <- 40

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_summary <- data.frame(Scen2)

rm(Scen2_sim_old)
rm(Scen2)

#### SCENARIO 3 ####


# Manager budget increases and decreases in a predictable/regular way above and below a mean (like a sine wave), user budget increases linearly

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 65*sin(1.33*s3+0)+400
MB3 <- MB3[1:50]

# standardise to a total cumulative budget of 25,000
MB3 <- 25000*(MB3/sum(MB3))




UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen3[time_step, 1] <- time_step
  Scen3[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen3[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen3[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen3[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen3[time_step, 6] <- UB
  Scen3[time_step, 7] <- MB
  
  Scen3_sim_old <- sim_new
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB3[time_step]
}

colnames(Scen3) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen3_summary <- data.frame(Scen3)

rm(Scen3_sim_old)
rm(Scen3)

write.csv(Scen3_summary, file = "CHANGE ME/Scen3_summary.csv")

#### SCENARIO 4 ####


### As Scenario 3, but the wavelength is shorter (higher frequency) and the amplitude is smaller

## Define manager budget
s4 <- seq(0,50,1)
MB4 <- 30*sin(2.5*s4+0)+500
MB4 <- MB4[1:50]

# Standardise so that cumulative total budget = 25,000
MB4 <- 25000*(MB4/sum(MB4))



UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB4[time_step]
}

colnames(Scen4) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen4_summary <- data.frame(Scen4)

rm(Scen4_sim_old)
rm(Scen4)


#### SCENARIO 5 ####


## Define manager budget

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
  
  str <- seq(10, 30, 0.2)
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
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.1[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_1_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 2 - wave 2
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.2[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_2_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)



### RUN 3 - wave 3
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.3[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_3_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




## RUN 4 - wave 4
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.1[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_4_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 5 - wave 5
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.5[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_5_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)



### RUN 6 - wave 6
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.6[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_6_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 7 - wave 7
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.7[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_7_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 8 - wave 8
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.8[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_8_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)





### RUN 9 - wave 9
UB  <- 400
UBR <- 40

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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.9[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_9_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)




### RUN 10 - wave 10
UB  <- 400
UBR <- 40

MB <- MB5.10[1]


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
  manage_target = 500000, 
  RESOURCE_ini = 500000, 
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
  UB <- UB + 4.0816
  UBR <- UB/10
  MB <- MB5.10[time_step]
}

colnames(Scen5) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen5_10_summary <- data.frame(Scen5)

rm(Scen5_sim_old)
rm(Scen5)



## Add column to all summaries
Scen5_1_summary$wave <- 1
Scen5_2_summary$wave <- 2
Scen5_3_summary$wave <- 3
Scen5_4_summary$wave <- 4
Scen5_5_summary$wave <- 5
Scen5_6_summary$wave <- 6
Scen5_7_summary$wave <- 7
Scen5_8_summary$wave <- 8
Scen5_9_summary$wave <- 9
Scen5_10_summary$wave <- 10

## Combine results
Scen5_all_summary <- rbind(Scen5_1_summary,Scen5_2_summary,Scen5_3_summary,Scen5_4_summary,Scen5_5_summary,
                           Scen5_6_summary,Scen5_7_summary,Scen5_8_summary,Scen5_9_summary,Scen5_10_summary,)
#### Results ####

### Load .csv files for the first 10 runs from scenarios 1:5 after updating the scenarios and landscape, as per the blurb above in the "Second run" section. Run by Brad

# Scenario 1
scen1 <- list.files(path = "./outputs/investment/scenarios/Run_2/Scenario_1",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen1 <- scen1[ ,-1]
scen1$Simulation <- rep(as.factor(1:10), each=50)
scen1$Manager_budget <- 500
scen1 <- scen1 %>% select(Time,Trees,Trees_est,Cull_cost,Cull_count,User_budget, Manager_budget,Simulation)


# Scenario 2
scen2 <- list.files(path = "./outputs/investment/scenarios/Run_2/Scenario_2",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen2 <- scen2[ ,-1]
scen2$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 3
scen3 <- list.files(path = "./outputs/investment/scenarios/Run_2/Scenario_3",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen3 <- scen3[ ,-1]
scen3$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 4
scen4 <- list.files(path = "./outputs/investment/scenarios/Run_2/Scenario_4",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen4 <- scen4[ ,-1]
scen4$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 5
scen5 <- read.csv("outputs/investment/scenarios/Run_2/Scenario_5/Scen5_all_summary.csv", header = T)
scen5 <- scen5[ ,-1]
scen5 <- scen5 %>% rename(Simulation = wave)
scen5$Simulation <- as.factor(scen5$Simulation)




### plot individually

# create list of all dataframes
dat_list <- list(scen1,scen2,scen3,scen4,scen5)

# plot functions
plot.trees <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Trees, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylim(485500,500000)
  return(plot)
}

plot.cull.count <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_count, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Cull count")
  
  return(plot)
}

plot.cull.cost <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_cost, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Cull cost")
  
  return(plot)
}

# for scenarios 1:4
plot.budgets <- function(dat){
  
  require('patchwork')
  
  dat2 <- dat[1:50, ]
  
  plot1 <- ggplot(dat2, aes(x=Time, y=Manager_budget))+
    geom_line(size=1, color="dodgerblue3")+
    theme_classic()+
    ylab("Manager budget")
  
  return(plot1)
}

# for scenario 5
plot.budgets2 <- function(dat){
  
  require('patchwork')
  plot1 <- ggplot(dat, aes(x=Time, y=Manager_budget, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Manager budget")
  
  
  return(plot1)
}


# apply functions to list of data
trees_plots      <- lapply(dat_list, plot.trees)
cull_count_plots <- lapply(dat_list, plot.cull.count) 
cull_cost_plots  <- lapply(dat_list, plot.cull.cost)
budget_plots_1_4 <- lapply(dat_list[1:4], plot.budgets)
budget_plots_5   <- lapply(dat_list[5], plot.budgets2)

# re-name list elements
names.trees <- c("scen1.treePlot","scen2.treePlot","scen3.treePlot","scen4.treePlot","scen5.treePlot")
names(trees_plots) <- names.trees

names.count <- c("scen1.countPlot","scen2.countPlot","scen3.countPlot","scen4.countPlot","scen5.countPlot")
names(cull_count_plots) <- names.count

names.cost <- c("scen1.costPlot","scen2.costPlot","scen3.costPlot","scen4.costPlot","scen5.costPlot")
names(cull_cost_plots) <- names.cost

names.budets.14 <- c("scen1.budgetPlots","scen2.budgetPlots","scen3.budgetPlots","scen4.budgetPlots")
names(budget_plots_1_4) <- names.budets.14

names.budgets.5 <- "scen5.budgetPlots"
names(budget_plots_5) <- names.budgets.5

# extract elements to gloal environment
list2env(trees_plots, globalenv())
list2env(cull_count_plots, globalenv())
list2env(cull_cost_plots, globalenv())
list2env(budget_plots_1_4, globalenv())
list2env(budget_plots_5, globalenv())

# tree count plots
tree.plot.all <- scen1.treePlot + scen2.treePlot + scen3.treePlot + scen4.treePlot + scen5.treePlot
tree.plot.all[[1]] <- tree.plot.all[[1]] + ggtitle("Scenario 1")
tree.plot.all[[2]] <- tree.plot.all[[2]] + ggtitle("Scenario 2")
tree.plot.all[[3]] <- tree.plot.all[[3]] + ggtitle("Scenario 3")
tree.plot.all[[4]] <- tree.plot.all[[4]] + ggtitle("Scenario 4")
tree.plot.all[[5]] <- tree.plot.all[[5]] + ggtitle("Scenario 5")

# cull count plots
cull.count.plot.all <- scen1.countPlot+scen2.countPlot+scen3.countPlot+scen4.countPlot+scen5.countPlot
cull.count.plot.all[[1]] <- cull.count.plot.all[[1]] + ggtitle("Scenario 1")
cull.count.plot.all[[2]] <- cull.count.plot.all[[2]] + ggtitle("Scenario 2")
cull.count.plot.all[[3]] <- cull.count.plot.all[[3]] + ggtitle("Scenario 3")
cull.count.plot.all[[4]] <- cull.count.plot.all[[4]] + ggtitle("Scenario 4")
cull.count.plot.all[[5]] <- cull.count.plot.all[[5]] + ggtitle("Scenario 5")

# cull cost plots
cull.cost.plot.all <- scen1.costPlot+scen2.costPlot+scen3.costPlot+scen4.costPlot+scen5.costPlot
cull.cost.plot.all[[1]] <- cull.cost.plot.all[[1]] + ggtitle("Scenario 1")
cull.cost.plot.all[[2]] <- cull.cost.plot.all[[2]] + ggtitle("Scenario 2")
cull.cost.plot.all[[3]] <- cull.cost.plot.all[[3]] + ggtitle("Scenario 3")
cull.cost.plot.all[[4]] <- cull.cost.plot.all[[4]] + ggtitle("Scenario 4")
cull.cost.plot.all[[5]] <- cull.cost.plot.all[[5]] + ggtitle("Scenario 5")

# budget plots

user_budget_p <- ggplot(scen1, aes(x=Time, y=User_budget))+
  geom_line(size=1, color="firebrick3")+
  theme_classic()+
  ylab("User budget")

budget.plot.all <- user_budget_p+scen1.budgetPlots+scen2.budgetPlots+scen3.budgetPlots+
  scen4.budgetPlots+scen5.budgetPlots
budget.plot.all[[1]] <- budget.plot.all[[1]] + ggtitle("User budget")
budget.plot.all[[2]] <- budget.plot.all[[2]] + ggtitle("Scenario 1")
budget.plot.all[[3]] <- budget.plot.all[[3]] + ggtitle("Scenario 2")
budget.plot.all[[4]] <- budget.plot.all[[4]] + ggtitle("Scenario 3")
budget.plot.all[[5]] <- budget.plot.all[[5]] + ggtitle("Scenario 4")
budget.plot.all[[6]] <- budget.plot.all[[6]] + ggtitle("Scenario 5")

# change y axis range for scen3 & scen4
budget.plot.all[[4]] <- budget.plot.all[[4]] + ylim(400,600)
budget.plot.all[[5]] <- budget.plot.all[[5]] + ylim(400,600)


#ggsave("outputs/investment/scenarios/Plots/Run_2/budget_all.png", budget.plot.all, 
 #     dpi=300, width = 30, height=20, units="cm")

### Plots for each scenario together
scen1_plots <- user_budget_p + scen1.budgetPlots + scen1.countPlot + scen1.costPlot + scen1.treePlot
scen2_plots <- user_budget_p + scen2.budgetPlots + scen2.countPlot + scen2.costPlot + scen2.treePlot
scen3_plots <- user_budget_p + scen3.budgetPlots + scen3.countPlot + scen3.costPlot + scen3.treePlot
scen4_plots <- user_budget_p + scen4.budgetPlots + scen4.countPlot + scen4.costPlot + scen4.treePlot
scen5_plots <- user_budget_p + scen5.budgetPlots + scen5.countPlot + scen5.costPlot + scen5.treePlot

#ggsave("outputs/investment/scenarios/Run_2/Scenario_1/scen1_plots.png", scen1_plots, 
#     dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Run_2/Scenario_2/scen2_plots.png", scen2_plots, 
#    dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Run_2/Scenario_3/scen3_plots.png", scen3_plots, 
#      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Run_2/Scenario_4/scen4_plots.png", scen4_plots, 
#      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Run_2/Scenario_5/scen5_plots.png", scen5_plots, 
#      dpi=300, width = 30, height=20, units="cm")



### get mean and error bars for each scenario

# Function to extract 50, 2.5, and 97.5% quantiles from each scenario
quant.func <- function(dat){
  wide.dat <- pivot_wider(dat,id_cols = Time, names_from = Simulation, values_from = Trees)
  wide.dat <- wide.dat[ ,-1]
  Mean.q   <- apply(wide.dat,1,quantile,probs=0.5)
  LCL.q    <- apply(wide.dat,1,quantile,probs=0.025)
  UCL.q    <- apply(wide.dat,1,quantile,probs=0.975)
  
  quant.df <- data.frame(Time = 1:50,
                         Mean = Mean.q,
                         LCL = LCL.q,
                         UCL = UCL.q)
  return(quant.df)
}

# apply function to all scenario dataframes
quants.ls <- lapply(dat_list, quant.func)
names <- c("scen1_quants","scen2_quants","scen3_quants","scen4_quants","scen5_quants")
names(quants.ls) <- names

# extract to global env
list2env(quants.ls, globalenv())

# add simulation
scen1_quants$Simulation <- "1"
scen2_quants$Simulation <- "2"
scen3_quants$Simulation <- "3"
scen4_quants$Simulation <- "4"
scen5_quants$Simulation <- "5"

# merge 
quants_all <- rbind(scen1_quants,scen2_quants,scen3_quants,scen4_quants,scen5_quants)


# plot (facets)
all_facets_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
                    geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
                    geom_line(size=1,aes(color=Simulation))+
                    facet_wrap(~Simulation)+
                    theme_classic()

#ggsave("outputs/investment/scenarios/Plots/Run_2/All_facets_ribbons.png", all_facets_ribbon,
#      dpi=300, width = 30, height = 20, units="cm")


# plot no facets
all_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
              geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
              geom_line(size=1,aes(color=Simulation))+
              theme_classic()
ggsave("outputs/investment/scenarios/Plots/Run_2/All_ribbons.png", all_ribbon,
      dpi=300, width = 30, height = 20, units="cm")


# zoom in on the end
all_zoom <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
            #geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
            geom_line(size=1,aes(color=Simulation))+
            theme_classic()+
            ylim(487500,492500)+
            xlim(40,50)


#ggsave("outputs/investment/scenarios/Plots/Run_2/All_zoom.png", all_zoom,
#      dpi=300, width = 30, height = 20, units="cm")





### what is the absolute difference in trees lost by the end?
final_trees <- data.frame(Scenario = 1:5,
                          Trees = c(min(quants_all$Mean[quants_all$Simulation=="1"]),
                                    min(quants_all$Mean[quants_all$Simulation=="2"]),
                                    min(quants_all$Mean[quants_all$Simulation=="3"]),
                                    min(quants_all$Mean[quants_all$Simulation=="4"]),
                                    min(quants_all$Mean[quants_all$Simulation=="5"])),
                          LCL = c(min(quants_all$LCL[quants_all$Simulation=="1"]),
                                  min(quants_all$LCL[quants_all$Simulation=="2"]),
                                  min(quants_all$LCL[quants_all$Simulation=="3"]),
                                  min(quants_all$LCL[quants_all$Simulation=="4"]),
                                  min(quants_all$LCL[quants_all$Simulation=="5"])),
                          UCL = c(min(quants_all$UCL[quants_all$Simulation=="1"]),
                                  min(quants_all$UCL[quants_all$Simulation=="2"]),
                                  min(quants_all$UCL[quants_all$Simulation=="3"]),
                                  min(quants_all$UCL[quants_all$Simulation=="4"]),
                                  min(quants_all$UCL[quants_all$Simulation=="5"])))

# plot
final_trees_plot <- ggplot(final_trees, aes(x = Scenario, y=Trees))+
                    geom_point(size=5)+
                    geom_errorbar(aes(ymin=LCL, ymax=UCL, width=0.2))+
                    ylab("Trees remaining")+
                    theme_classic()

#ggsave("outputs/investment/scenarios/Plots/Run_2/Final_trees.png", final_trees_plot,
#      dpi=300, width = 30, height = 20, units="cm")


# there is only a differene of 378 trees between the best outcome (S1) and the worst (S3). 


## what is the average number of trees lost over the time period?
s1.diff <- max(quants_all$Mean[quants_all$Simulation=="1"]) - min(quants_all$Mean[quants_all$Simulation=="1"])
s2.diff <- max(quants_all$Mean[quants_all$Simulation=="2"]) - min(quants_all$Mean[quants_all$Simulation=="2"])
s3.diff <- max(quants_all$Mean[quants_all$Simulation=="3"]) - min(quants_all$Mean[quants_all$Simulation=="3"])
s4.diff <- max(quants_all$Mean[quants_all$Simulation=="4"]) - min(quants_all$Mean[quants_all$Simulation=="4"])
s5.diff <- max(quants_all$Mean[quants_all$Simulation=="5"]) - min(quants_all$Mean[quants_all$Simulation=="5"])

mean(c(s1.diff,s2.diff,s3.diff,s4.diff,s5.diff))
# 11647 trees

# what is that in hectares and km2 of forest?
11648/50
# 233 hectares

233/100
# 2.33 km2
  ### THIRD RUN ####

# The results of the 2nd run are still not extreme enough. The results are showing some interesting things, but we want the number of trees to be higher, the proportion of the total number of tree cut down to be higher, and ideally the scenarios to be more different. I am going to start by simply reducing the number of trees we start with, plus increasing the user budget.

# I am also going to change the manager budget in S2 so that it start closer to 500. This requires the slope to be different from the user budget. I don't think this is a problem, seeing as the UB and MB are not equivalent or even proportional. This scenario is then just replicating a steadily increasing MB, but one which starts lower than the others.

# Note - I ran one sim each with the user budget going from 800 to 1000. But the number of trees lost was still not enough, so I am going to up it again by quite a lot, and also increase the slope by quite a lot.

# note - after the above (increased user budget to 2000 and increases of 20), there were a lot more trees lost. More like what we're after. But only S2 is different to the rest. All the others are still too similar. I will try and increase the user budget increments to see if this separates them a bit.


#### SCENARIO 1 #####

UB  <- 2000
UBR <- 200

Scen1_sim_old <- gmse_apply(
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
  manager_budget = 500, 
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
Scen1 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen1_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  Scen1[time_step, 1] <- time_step
  Scen1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen1[time_step, 6] <- UB
  Scen1[time_step, 7] <- MB
  
  Scen1_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
}

colnames(Scen1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen1_1_summary <- data.frame(Scen1)

rm(Scen1_sim_old)
rm(Scen1)



#### SCENARIO 2 ####


### define manager budget

# define slope 
xx <- 1.5

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 500

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))


### RUN 

UB  <- 2000
UBR <- 200

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
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
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_summary <- data.frame(Scen2)


write.csv(Scen2_summary, file="outputs/investment/scenarios/Run_3/scen2_summary.csv")




#### SCENARIO 3 ####


# Manager budget increases and decreases in a predictable/regular way above and below a mean (like a sine wave), user budget increases linearly

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 65*sin(1.33*s3+0)+400
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
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
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

write.csv(Scen3_summary, file = "outputs/investment/scenarios/Run_3/Scen3_summary.csv")


#### SCENARIO 4 ####


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



#### SCENARIO 5 ####


## Define manager budget

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
  
  str <- seq(10, 30, 0.2)
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


## Results from single run ####

# load data
scen1 <- read.csv("outputs/investment/scenarios/Run_3/Scen1_1_summary.csv")
scen2 <- read.csv("outputs/investment/scenarios/Run_3/Scen2_summary.csv")
scen3 <- read.csv("outputs/investment/scenarios/Run_3/Scen3_summary.csv")
scen4 <- read.csv("outputs/investment/scenarios/Run_3/Scen4_summary.csv")
scen5 <- read.csv("outputs/investment/scenarios/Run_3/Scen5_summary_all.csv")

scen1$Manager_budget <- 500
scen1$Scenario <- "1"
scen2$Scenario <- "2"
scen3$Scenario <- "3"
scen4$Scenario <- "4"
scen5$Scenario <- rep(c("5.1","5.2","5.3","5.4","5.5"), each=50)

summary_all <- rbind(scen1,scen2,scen3,scen4,scen5)

ggplot(summary_all, aes(x=Time, y=Trees, group=Scenario, color=Scenario))+
  geom_line(size=1)+
  theme_classic()+
  ylim(0,100000)

summary_all %>% filter(Scenario=="2")

  ### FOURTH RUN ####

# The results of the 3rd run were significantly more trees lost, but still very little difference between scenarios. So here I have made the scenarios more extreme. 

# S2 starts lower and ends higher than before
# S3 has much higher/lower peaks and troughs, at the expense of frequency (i.e. much lower frequency)
# S5 has higher amplitude and more variety in the wave forms

#### SCENARIO 1 ####

UB  <- 2000
UBR <- 200

Scen1_sim_old <- gmse_apply(
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
  manager_budget = 500, 
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
Scen1 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen1_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR)
  
  Scen1[time_step, 1] <- time_step
  Scen1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen1[time_step, 6] <- UB
  Scen1[time_step, 7] <- MB
  
  Scen1_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
}

colnames(Scen1) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen1_summary <- data.frame(Scen1)

write.csv(Scen1_summary, file="outputs/investment/scenarios/Run_4/Scen1_summary.csv")


#### SCENARIO 2 ####

### define manager budget

# define slope 
xx <- 60

# empty vector
MB2 <- NULL

# starting value
MB2[1] <- 500

# fill in budget vector by adding the slope onto each value
for(i in 2:50){
  MB2[i] <- MB2[i-1] + xx
}

# standardise to the total cumulative budget = 25,000
MB2 <- 25000*(MB2/sum(MB2))


### RUN 

UB  <- 2000
UBR <- 200

MB <- MB2[1]


Scen2_sim_old <- gmse_apply(
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
Scen2 <- matrix(data=NA, nrow=50, ncol=7)

# loop the simulation. 
for(time_step in 1:50){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen2_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
  Scen2[time_step, 1] <- time_step
  Scen2[time_step, 2] <- sim_new$basic_output$resource_results[1]
  Scen2[time_step, 3] <- sim_new$basic_output$observation_results[1]
  Scen2[time_step, 4] <- sim_new$basic_output$manager_results[3]
  Scen2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  Scen2[time_step, 6] <- UB
  Scen2[time_step, 7] <- MB
  
  Scen2_sim_old <- sim_new
  UB <- UB + 75
  UBR <- UB/10
  MB <- MB2[time_step]
}

colnames(Scen2) <- c("Time", "Trees", "Trees_est", "Cull_cost", "Cull_count", "User_budget", "Manager_budget")
Scen2_summary <- data.frame(Scen2)


write.csv(Scen2_summary, file="outputs/investment/scenarios/Run_4/Scen2_summary.csv")


#### SCENARIO 3 ####



# Manager budget increases and decreases in a predictable/regular way above and below a mean (like a sine wave), user budget increases linearly

# Define manager budget
s3 <- seq(0,50,1)
MB3 <- 350*sin(0.5*e+0)+400
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
  
  sim_new <- gmse_apply(get_res = "Full", old_list = Scen3_sim_old, user_budget=UB, 
                        usr_budget_rng = UBR, manager_budget = MB)
  
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

write.csv(Scen3_summary, file = "outputs/investment/scenarios/Run_4/Scen3_summary.csv")



#### SCENARIO 4 ####

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

write.csv(Scen4_summary, file="outputs/investment/scenarios/Run_4/Scen4_summary.csv")


#### SCENARIO 5 ####

## Define manager budget

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







## Results from a single run ####


# load data
scen1 <- read.csv("outputs/investment/scenarios/Run_4/Single_run/Scen1_summary.csv")
scen2 <- read.csv("outputs/investment/scenarios/Run_4/Single_run/Scen2_summary.csv")
scen3 <- read.csv("outputs/investment/scenarios/Run_4/Single_run/Scen3_summary.csv")
scen4 <- read.csv("outputs/investment/scenarios/Run_4/Single_run/Scen4_summary.csv")
scen5 <- read.csv("outputs/investment/scenarios/Run_4/Single_run/Scen5_summary.csv")

scen1$Manager_budget <- 500
scen1$Scenario <- "1"
scen2$Scenario <- "2"
scen3$Scenario <- "3"
scen4$Scenario <- "4"
scen5$Scenario <- rep(c("5.1","5.2","5.3","5.4","5.5"), each=50)

summary_all <- rbind(scen1,scen2,scen3,scen4,scen5)

ggplot(summary_all, aes(x=Time, y=Trees, group=Scenario, color=Scenario))+
  geom_line(size=1)+
  theme_classic()+
  ylim(0,100000)

## Results from 10 runs ####

### Load .csv files for the first 10 runs from scenarios 1:5 after updating the scenarios as per the blurb above in the "FOURTH RUN" section. Run by Brad

# Scenario 1
scen1 <- list.files(path = "./outputs/investment/scenarios/Run_4/Scenario_1",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen1 <- scen1[ ,-1]
scen1$Simulation <- rep(as.factor(1:10), each=50)
scen1$Manager_budget <- 500
scen1 <- scen1 %>% select(Time,Trees,Trees_est,Cull_cost,Cull_count,User_budget, Manager_budget,Simulation)


# Scenario 2
scen2 <- list.files(path = "./outputs/investment/scenarios/Run_4/Scenario_2",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen2 <- scen2[ ,-1]
scen2$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 3
scen3 <- list.files(path = "./outputs/investment/scenarios/Run_4/Scenario_3",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen3 <- scen3[ ,-1]
scen3$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 4
scen4 <- list.files(path = "./outputs/investment/scenarios/Run_4/Scenario_4",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen4 <- scen4[ ,-1]
scen4$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 5
scen5 <- read.csv("outputs/investment/scenarios/Run_4/Scenario_5/Scen5_summary.csv", header = T)
scen5 <- scen5[ ,-1]
scen5$Simulation <- rep(as.factor(1:10), each=50) 
scen5$Simulation <- as.factor(scen5$Simulation)




### plot individually

# create list of all dataframes
dat_list <- list(scen1,scen2,scen3,scen4,scen5)

# plot functions
plot.trees <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Trees, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylim(0,100000)
  return(plot)
}

plot.cull.count <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_count, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Cull count")
  
  return(plot)
}

plot.cull.cost <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_cost, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Cull cost")
  
  return(plot)
}

# for scenarios 1:4
plot.budgets <- function(dat){
  
  require('patchwork')
  
  dat2 <- dat[1:50, ]
  
  plot1 <- ggplot(dat2, aes(x=Time, y=Manager_budget))+
    geom_line(size=1, color="dodgerblue3")+
    theme_classic()+
    ylab("Manager budget")
  
  return(plot1)
}

# for scenario 5
plot.budgets2 <- function(dat){
  
  require('patchwork')
  plot1 <- ggplot(dat, aes(x=Time, y=Manager_budget, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Manager budget")
  
  
  return(plot1)
}


# apply functions to list of data
trees_plots      <- lapply(dat_list, plot.trees)
cull_count_plots <- lapply(dat_list, plot.cull.count) 
cull_cost_plots  <- lapply(dat_list, plot.cull.cost)
budget_plots_1_4 <- lapply(dat_list[1:4], plot.budgets)
budget_plots_5   <- lapply(dat_list[5], plot.budgets2)

# re-name list elements
names.trees <- c("scen1.treePlot","scen2.treePlot","scen3.treePlot","scen4.treePlot","scen5.treePlot")
names(trees_plots) <- names.trees

names.count <- c("scen1.countPlot","scen2.countPlot","scen3.countPlot","scen4.countPlot","scen5.countPlot")
names(cull_count_plots) <- names.count

names.cost <- c("scen1.costPlot","scen2.costPlot","scen3.costPlot","scen4.costPlot","scen5.costPlot")
names(cull_cost_plots) <- names.cost

names.budets.14 <- c("scen1.budgetPlots","scen2.budgetPlots","scen3.budgetPlots","scen4.budgetPlots")
names(budget_plots_1_4) <- names.budets.14

names.budgets.5 <- "scen5.budgetPlots"
names(budget_plots_5) <- names.budgets.5

# extract elements to gloal environment
list2env(trees_plots, globalenv())
list2env(cull_count_plots, globalenv())
list2env(cull_cost_plots, globalenv())
list2env(budget_plots_1_4, globalenv())
list2env(budget_plots_5, globalenv())

# tree count plots
tree.plot.all <- scen1.treePlot + scen2.treePlot + scen3.treePlot + scen4.treePlot + scen5.treePlot
tree.plot.all[[1]] <- tree.plot.all[[1]] + ggtitle("Scenario 1")
tree.plot.all[[2]] <- tree.plot.all[[2]] + ggtitle("Scenario 2")
tree.plot.all[[3]] <- tree.plot.all[[3]] + ggtitle("Scenario 3")
tree.plot.all[[4]] <- tree.plot.all[[4]] + ggtitle("Scenario 4")
tree.plot.all[[5]] <- tree.plot.all[[5]] + ggtitle("Scenario 5")

#ggsave("outputs/investment/scenarios/Plots/Run_4/tree_plot_all_separate.png", tree.plot.all, 
 #      width = 35, height = 25, dpi=300, units = "cm")

# cull count plots
cull.count.plot.all <- scen1.countPlot+scen2.countPlot+scen3.countPlot+scen4.countPlot+scen5.countPlot
cull.count.plot.all[[1]] <- cull.count.plot.all[[1]] + ggtitle("Scenario 1")
cull.count.plot.all[[2]] <- cull.count.plot.all[[2]] + ggtitle("Scenario 2")
cull.count.plot.all[[3]] <- cull.count.plot.all[[3]] + ggtitle("Scenario 3")
cull.count.plot.all[[4]] <- cull.count.plot.all[[4]] + ggtitle("Scenario 4")
cull.count.plot.all[[5]] <- cull.count.plot.all[[5]] + ggtitle("Scenario 5")

#ggsave("outputs/investment/scenarios/Plots/Run_4/cull_count_plot_all_separate.png", cull.count.plot.all, 
 #      width = 35, height = 25, dpi=300, units = "cm")

# cull cost plots
cull.cost.plot.all <- scen1.costPlot+scen2.costPlot+scen3.costPlot+scen4.costPlot+scen5.costPlot
cull.cost.plot.all[[1]] <- cull.cost.plot.all[[1]] + ggtitle("Scenario 1")
cull.cost.plot.all[[2]] <- cull.cost.plot.all[[2]] + ggtitle("Scenario 2")
cull.cost.plot.all[[3]] <- cull.cost.plot.all[[3]] + ggtitle("Scenario 3")
cull.cost.plot.all[[4]] <- cull.cost.plot.all[[4]] + ggtitle("Scenario 4")
cull.cost.plot.all[[5]] <- cull.cost.plot.all[[5]] + ggtitle("Scenario 5")

#ggsave("outputs/investment/scenarios/Plots/Run_4/cull_cost_plot_all_separate.png", cull.cost.plot.all, 
 #      width = 35, height = 25, dpi=300, units = "cm")

# budget plots

user_budget_p <- ggplot(scen1, aes(x=Time, y=User_budget))+
  geom_line(size=1, color="firebrick3")+
  theme_classic()+
  ylab("User budget")

budget.plot.all <- user_budget_p+scen1.budgetPlots+scen2.budgetPlots+scen3.budgetPlots+
  scen4.budgetPlots+scen5.budgetPlots

budget.plot.all[[1]] <- budget.plot.all[[1]] + ggtitle("User budget")
budget.plot.all[[2]] <- budget.plot.all[[2]] + ggtitle("Scenario 1")
budget.plot.all[[3]] <- budget.plot.all[[3]] + ggtitle("Scenario 2")
budget.plot.all[[4]] <- budget.plot.all[[4]] + ggtitle("Scenario 3")
budget.plot.all[[5]] <- budget.plot.all[[5]] + ggtitle("Scenario 4")
budget.plot.all[[6]] <- budget.plot.all[[6]] + ggtitle("Scenario 5")

# change y axis range for all manager budget plots
budget.plot.all[[2]] <- budget.plot.all[[2]] + ylim(0,1000)
budget.plot.all[[3]] <- budget.plot.all[[3]] + ylim(0,1000)
budget.plot.all[[4]] <- budget.plot.all[[4]] + ylim(0,1000)
budget.plot.all[[5]] <- budget.plot.all[[5]] + ylim(0,1000)
budget.plot.all[[6]] <- budget.plot.all[[6]] + ylim(0,1000)


#ggsave("outputs/investment/scenarios/Plots/Run_4/budget_plot_all_separate.png", budget.plot.all, 
 #      width = 35, height = 25, dpi=300, units = "cm")




### Plots for each scenario together
scen1_plots <- user_budget_p + scen1.budgetPlots + scen1.countPlot + scen1.costPlot + scen1.treePlot
scen2_plots <- user_budget_p + scen2.budgetPlots + scen2.countPlot + scen2.costPlot + scen2.treePlot
scen3_plots <- user_budget_p + scen3.budgetPlots + scen3.countPlot + scen3.costPlot + scen3.treePlot
scen4_plots <- user_budget_p + scen4.budgetPlots + scen4.countPlot + scen4.costPlot + scen4.treePlot
scen5_plots <- user_budget_p + scen5.budgetPlots + scen5.countPlot + scen5.costPlot + scen5.treePlot

ggsave("outputs/investment/scenarios/Run_4/scen1_plots.png", scen1_plots, 
     dpi=300, width = 30, height=20, units="cm")
ggsave("outputs/investment/scenarios/Run_4/scen2_plots.png", scen2_plots, 
    dpi=300, width = 30, height=20, units="cm")
ggsave("outputs/investment/scenarios/Run_4/scen3_plots.png", scen3_plots, 
      dpi=300, width = 30, height=20, units="cm")
ggsave("outputs/investment/scenarios/Run_4/scen4_plots.png", scen4_plots, 
      dpi=300, width = 30, height=20, units="cm")
ggsave("outputs/investment/scenarios/Run_4/scen5_plots.png", scen5_plots, 
      dpi=300, width = 30, height=20, units="cm")



### get mean and error bars for each scenario

# Function to extract 50, 2.5, and 97.5% quantiles from each scenario
quant.func <- function(dat){
  wide.dat <- pivot_wider(dat,id_cols = Time, names_from = Simulation, values_from = Trees)
  wide.dat <- wide.dat[ ,-1]
  Mean.q   <- apply(wide.dat,1,quantile,probs=0.5)
  LCL.q    <- apply(wide.dat,1,quantile,probs=0.025)
  UCL.q    <- apply(wide.dat,1,quantile,probs=0.975)
  
  quant.df <- data.frame(Time = 1:50,
                         Mean = Mean.q,
                         LCL = LCL.q,
                         UCL = UCL.q)
  return(quant.df)
}


## If the trees go extinct during a simulation, the simulation ends, i.e., all subsequent rows are NAs for all parameters. Therefore I need to remove the NA rows in all of the dataframes before running the below function, otherwise it throws an error

# ID the scenarios with NAs
sum(is.na(scen1))
sum(is.na(scen2))
sum(is.na(scen3))
sum(is.na(scen4))
sum(is.na(scen5))
# only scen3

# split scenario 3
scen3_1 <- scen3 %>% filter(Simulation == "1")
scen3_2 <- scen3 %>% filter(Simulation == "2")
scen3_3 <- scen3 %>% filter(Simulation == "3")
scen3_4 <- scen3 %>% filter(Simulation == "4")
scen3_5 <- scen3 %>% filter(Simulation == "5")
scen3_6 <- scen3 %>% filter(Simulation == "6")
scen3_7 <- scen3 %>% filter(Simulation == "7")
scen3_8 <- scen3 %>% filter(Simulation == "8")
scen3_9 <- scen3 %>% filter(Simulation == "9")
scen3_10 <- scen3 %>% filter(Simulation == "10")

# function to change NAs to 0's, and replace Time with the row numbers
na.func <- function(dat){
for(i in 1:nrow(dat)){
  if(is.na(dat$Time)[i]){
    dat$Time[i]           <- row.names(scen3_1)[i]
    dat$Trees[i]          <- 0
    dat$Trees_est[i]      <- 0
    dat$Cull_cost[i]      <- 0
    dat$Cull_count[i]     <- 0
    dat$User_budget[i]    <- 0
    dat$Manager_budget[i] <- 0
  } 
    
}
  return(dat)
}

# create list
scen3_list <- list(scen3_1,scen3_2,scen3_3,scen3_4,scen3_5,scen3_6,scen3_7,scen3_8,scen3_9,scen3_10)

# apply function to list
scen3_list <- lapply(scen3_list, na.func)

# rename list
names(scen3_list) <- c("scen3_1","scen3_2","scen3_3","scen3_4","scen3_5","scen3_6","scen3_7",
                       "scen3_8","scen3_9","scen3_10")

# extract to environment
list2env(scen3_list, globalenv())

# merge
scen3_noNA <- rbind(scen3_1,scen3_2,scen3_3,scen3_4,scen3_5,scen3_6,
               scen3_7,scen3_8,scen3_9,scen3_10)



### apply quantile function to all scenario dataframes

# create new list with updated scen3
dat_list2 <- list(scen1,scen2,scen3_noNA,scen4,scen5)
quants.ls <- lapply(dat_list2, quant.func)
names(quants.ls) <- c("scen1_quants","scen2_quants","scen3_quants","scen4_quants","scen5_quants")

# extract to global env
list2env(quants.ls, globalenv())

# add simulation
scen1_quants$Simulation <- "1"
scen2_quants$Simulation <- "2"
scen3_quants$Simulation <- "3"
scen4_quants$Simulation <- "4"
scen5_quants$Simulation <- "5"

# merge 
quants_all <- rbind(scen1_quants,scen2_quants,scen3_quants,scen4_quants,scen5_quants)


# plot (facets)
all_facets_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
                    geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
                    geom_line(size=1,aes(color=Simulation))+
                    facet_wrap(~Simulation)+
                    theme_classic()

ggsave("outputs/investment/scenarios/Plots/Run_4/All_facets_ribbons.png", all_facets_ribbon,
      dpi=300, width = 30, height = 20, units="cm")


# plot no facets
all_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
              geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
              geom_line(size=1,aes(color=Simulation))+
              theme_classic()+
              ylab("Number of trees")

ggsave("outputs/investment/scenarios/Plots/Run_4/All_ribbons.png", all_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# zoom in on the end
all_zoom <- ggplot(quants_all, aes(x=Time, y=Mean, group=Simulation))+
            geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Simulation),alpha=0.3)+
            geom_line(size=1,aes(color=Simulation))+
            theme_classic()+
            ylim(0,25000)+
            xlim(40,50)

ggsave("outputs/investment/scenarios/Plots/Run_4/Zoom_ribbons.png", all_zoom,
       dpi=300, width = 30, height = 20, units="cm")

  ### FIFTH RUN ####

# see explanation in "Create and save budgets > Run 5"

# S1, S2, S3, and S5 are the same as run 4, except S5 is now called S4. S5 is the only new one

#### SCENARIO 5 ####


## Define manager budget

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
r_waves_traj2 <- lapply(r_waves_traj2, function(x){ifelse(x<10,10,x)})

# extract to global environment
list2env(r_waves_traj2, globalenv())




### wave 1
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


scen5_summary_odd <- rbind(Scen5_1_summary,Scen5_3_summary,Scen5_5_summary,Scen5_7_summary,Scen5_9_summary)

write.csv(scen5_summary_odd, file="outputs/investment/scenarios/Run_5/scen5_summary_odd.csv")








## Results - 10 runs ####


### These are the results from run 4 above, but excluding S4, changing S5 above to S4, and then adding the new S5


# load in the results from the new S5
scen5_odd <- read.csv("outputs/investment/scenarios/Run_5/scen5_summary_odd.csv")
scen5_even <- read.csv("outputs/investment/scenarios/Run_5/scen5_summary_even.csv")

# add simulation
scen5_odd$Simulation <- rep(c(1,3,5,7,9), each=50)
scen5_even$Simulation <- rep(c(2,4,6,8,10), each=50)


# merge
scen5 <- rbind(scen5_odd, scen5_even)

# re-order
scen5 <- scen5 %>% arrange(Simulation)
scen5$Simulation <- as.factor(scen5$Simulation)

scen5 <- scen5[ ,-1]



## load in the results from S1:3, and S5 from run 4 above, but changing S5 to S4.

### Load .csv files for the first 10 runs from scenarios 1:5 after updating the scenarios as per the blurb above in the "FOURTH RUN" section. Run by Brad

# Scenario 1
scen1 <- list.files(path = "./outputs/investment/scenarios/Run_4/Scenario_1",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen1 <- scen1[ ,-1]
scen1$Simulation <- rep(as.factor(1:10), each=50)
scen1$Manager_budget <- 500
scen1 <- scen1 %>% select(Time,Trees,Trees_est,Cull_cost,Cull_count,User_budget, Manager_budget,Simulation)


# Scenario 2
scen2 <- list.files(path = "./outputs/investment/scenarios/Run_4/Scenario_2",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen2 <- scen2[ ,-1]
scen2$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 3
scen3 <- list.files(path = "./outputs/investment/scenarios/Run_4/Scenario_3",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen3 <- scen3[ ,-1]
scen3$Simulation <- rep(as.factor(1:10), each=50) 


# Scenario 4
scen4 <- read.csv("outputs/investment/scenarios/Run_4/Scenario_5/Scen5_summary.csv", header = T)
scen4 <- scen4[ ,-1]
scen4$Simulation <- rep(as.factor(1:10), each=50) 
scen4$Simulation <- as.factor(scen4$Simulation)



### Plots

# put all df's into a list
dat_list <- list(scen1,scen2,scen3,scen4,scen5)

# plot functions
plot.trees <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Trees, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylim(0,100000)
  return(plot)
}

plot.cull.count <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_count, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Cull count")
  
  return(plot)
}

plot.cull.cost <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_cost, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Cull cost")
  
  return(plot)
}

# for scenarios 1:3
plot.budgets <- function(dat){
  
  require('patchwork')
  
  dat2 <- dat[1:50, ]
  
  plot1 <- ggplot(dat2, aes(x=Time, y=Manager_budget))+
    geom_line(size=1, color="dodgerblue3")+
    theme_classic()+
    ylab("Manager budget")+
    theme(axis.title = element_text(size=15),
          axis.text = element_text(size=12))
  
  return(plot1)
}

# for scenarios 4 and 5
plot.budgets2 <- function(dat){
  
  require('patchwork')
  
  plot1 <- ggplot(dat, aes(x=Time, y=Manager_budget, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Manager budget")+
    theme(axis.title = element_text(size=15),
          axis.text = element_text(size=12))
  
  
  return(plot1)
}


# apply functions to list of data
trees_plots      <- lapply(dat_list, plot.trees)
cull_count_plots <- lapply(dat_list, plot.cull.count) 
cull_cost_plots  <- lapply(dat_list, plot.cull.cost)
budget_plots_1_3 <- lapply(dat_list[1:3], plot.budgets)
budget_plots_4_5 <- lapply(dat_list[4:5], plot.budgets2)

# re-name list elements
names(trees_plots) <- c("scen1.treePlot","scen2.treePlot","scen3.treePlot","scen4.treePlot","scen5.treePlot")

names(cull_count_plots) <- c("scen1.countPlot","scen2.countPlot","scen3.countPlot","scen4.countPlot",
                             "scen5.countPlot")

names(cull_cost_plots) <- c("scen1.costPlot","scen2.costPlot","scen3.costPlot","scen4.costPlot",
                            "scen5.costPlot")

names(budget_plots_1_3) <- c("scen1.budgetPlots","scen2.budgetPlots","scen3.budgetPlots")

names(budget_plots_4_5) <- c("scen4.budgetPlots","scen5.budgetPlots")


# extract elements to gloal environment
list2env(trees_plots, globalenv())
list2env(cull_count_plots, globalenv())
list2env(cull_cost_plots, globalenv())
list2env(budget_plots_1_3, globalenv())
list2env(budget_plots_4_5, globalenv())

# tree count plots
tree.plot.all <- scen1.treePlot + scen2.treePlot + scen3.treePlot + scen4.treePlot + scen5.treePlot
tree.plot.all[[1]] <- tree.plot.all[[1]] + ggtitle("Scenario 1")
tree.plot.all[[2]] <- tree.plot.all[[2]] + ggtitle("Scenario 2")
tree.plot.all[[3]] <- tree.plot.all[[3]] + ggtitle("Scenario 3")
tree.plot.all[[4]] <- tree.plot.all[[4]] + ggtitle("Scenario 4")
tree.plot.all[[5]] <- tree.plot.all[[5]] + ggtitle("Scenario 5")

# tree count plot for only S4 and S5
tree.plot.4_5 <- scen4.treePlot + scen5.treePlot

#ggsave("outputs/investment/scenarios/Plots/Run_5/tree_plot_all_separate.png", tree.plot.all, 
 #     width = 35, height = 25, dpi=300, units = "cm")

# cull count plots
cull.count.plot.all <- scen1.countPlot+scen2.countPlot+scen3.countPlot+scen4.countPlot+scen5.countPlot
cull.count.plot.all[[1]] <- cull.count.plot.all[[1]] + ggtitle("Scenario 1")
cull.count.plot.all[[2]] <- cull.count.plot.all[[2]] + ggtitle("Scenario 2")
cull.count.plot.all[[3]] <- cull.count.plot.all[[3]] + ggtitle("Scenario 3")
cull.count.plot.all[[4]] <- cull.count.plot.all[[4]] + ggtitle("Scenario 4")
cull.count.plot.all[[5]] <- cull.count.plot.all[[5]] + ggtitle("Scenario 5")

#ggsave("outputs/investment/scenarios/Plots/Run_5/cull_count_plot_all_separate.png", cull.count.plot.all, 
 #     width = 35, height = 25, dpi=300, units = "cm")

# cull cost plots
cull.cost.plot.all <- scen1.costPlot+scen2.costPlot+scen3.costPlot+scen4.costPlot+scen5.costPlot
cull.cost.plot.all[[1]] <- cull.cost.plot.all[[1]] + ggtitle("Scenario 1")
cull.cost.plot.all[[2]] <- cull.cost.plot.all[[2]] + ggtitle("Scenario 2")
cull.cost.plot.all[[3]] <- cull.cost.plot.all[[3]] + ggtitle("Scenario 3")
cull.cost.plot.all[[4]] <- cull.cost.plot.all[[4]] + ggtitle("Scenario 4")
cull.cost.plot.all[[5]] <- cull.cost.plot.all[[5]] + ggtitle("Scenario 5")

#ggsave("outputs/investment/scenarios/Plots/Run_5/cull_cost_plot_all_separate.png", cull.cost.plot.all, 
#      width = 35, height = 25, dpi=300, units = "cm")



# budget plots

user_budget_p <- ggplot(scen1, aes(x=Time, y=User_budget))+
  geom_line(size=1, color="firebrick3")+
  theme_classic()+
  ylab("Community resources")+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=12))

budget.plot.all <- user_budget_p+scen1.budgetPlots+scen2.budgetPlots+scen3.budgetPlots+
  scen4.budgetPlots+scen5.budgetPlots

budget.plot.all[[1]] <- budget.plot.all[[1]] + ggtitle("All scenarios")
budget.plot.all[[2]] <- budget.plot.all[[2]] + ggtitle("Scenario 1")
budget.plot.all[[3]] <- budget.plot.all[[3]] + ggtitle("Scenario 2")
budget.plot.all[[4]] <- budget.plot.all[[4]] + ggtitle("Scenario 3")
budget.plot.all[[5]] <- budget.plot.all[[5]] + ggtitle("Scenario 4")
budget.plot.all[[6]] <- budget.plot.all[[6]] + ggtitle("Scenario 5")

# change y axis range for all manager budget plots
budget.plot.all[[2]] <- budget.plot.all[[2]] + ylim(0,1150)
budget.plot.all[[3]] <- budget.plot.all[[3]] + ylim(0,1150)
budget.plot.all[[4]] <- budget.plot.all[[4]] + ylim(0,1150)
budget.plot.all[[5]] <- budget.plot.all[[5]] + ylim(0,1150)
budget.plot.all[[6]] <- budget.plot.all[[6]] + ylim(0,1150)


#ggsave("outputs/investment/scenarios/Plots/Run_5/budget_plot_all_separate.png", budget.plot.all, 
 #     width = 35, height = 25, dpi=300, units = "cm")




### Plots for each scenario together
scen1_plots <- user_budget_p + scen1.budgetPlots + scen1.countPlot + scen1.costPlot + scen1.treePlot
scen2_plots <- user_budget_p + scen2.budgetPlots + scen2.countPlot + scen2.costPlot + scen2.treePlot
scen3_plots <- user_budget_p + scen3.budgetPlots + scen3.countPlot + scen3.costPlot + scen3.treePlot
scen4_plots <- user_budget_p + scen4.budgetPlots + scen4.countPlot + scen4.costPlot + scen4.treePlot
scen5_plots <- user_budget_p + scen5.budgetPlots + scen5.countPlot + scen5.costPlot + scen5.treePlot

#ggsave("outputs/investment/scenarios/Run_5/scen1_plots.png", scen1_plots, 
 #      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Run_5/scen2_plots.png", scen2_plots, 
 #      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Run_5/scen3_plots.png", scen3_plots, 
 #      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Run_5/scen4_plots.png", scen4_plots, 
 #      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Run_5/scen5_plots.png", scen5_plots, 
 #      dpi=300, width = 30, height=20, units="cm")



### get mean and error bars for each scenario

# Function to extract 50, 2.5, and 97.5% quantiles from each scenario
quant.func <- function(dat){
  wide.dat <- pivot_wider(dat,id_cols = Time, names_from = Simulation, values_from = Trees)
  wide.dat <- wide.dat[ ,-1]
  Mean.q   <- apply(wide.dat,1,quantile,probs=0.5)
  LCL.q    <- apply(wide.dat,1,quantile,probs=0.025)
  UCL.q    <- apply(wide.dat,1,quantile,probs=0.975)
  
  quant.df <- data.frame(Time = 1:50,
                         Mean = Mean.q,
                         LCL = LCL.q,
                         UCL = UCL.q)
  return(quant.df)
}


## If the trees go extinct during a simulation, the simulation ends, i.e., all subsequent rows are NAs for all parameters. Therefore I need to remove the NA rows in all of the dataframes before running the below function, otherwise it throws an error

# ID the scenarios with NAs
sum(is.na(scen1))
sum(is.na(scen2))
sum(is.na(scen3))
sum(is.na(scen4))
sum(is.na(scen5))
# scen3 & scen5

# split scenario 3
scen3_1 <- scen3 %>% filter(Simulation == "1")
scen3_2 <- scen3 %>% filter(Simulation == "2")
scen3_3 <- scen3 %>% filter(Simulation == "3")
scen3_4 <- scen3 %>% filter(Simulation == "4")
scen3_5 <- scen3 %>% filter(Simulation == "5")
scen3_6 <- scen3 %>% filter(Simulation == "6")
scen3_7 <- scen3 %>% filter(Simulation == "7")
scen3_8 <- scen3 %>% filter(Simulation == "8")
scen3_9 <- scen3 %>% filter(Simulation == "9")
scen3_10 <- scen3 %>% filter(Simulation == "10")

# split scenario 5
scen5_1 <- scen5 %>% filter(Simulation == "1")
scen5_2 <- scen5 %>% filter(Simulation == "2")
scen5_3 <- scen5 %>% filter(Simulation == "3")
scen5_4 <- scen5 %>% filter(Simulation == "4")
scen5_5 <- scen5 %>% filter(Simulation == "5")
scen5_6 <- scen5 %>% filter(Simulation == "6")
scen5_7 <- scen5 %>% filter(Simulation == "7")
scen5_8 <- scen5 %>% filter(Simulation == "8")
scen5_9 <- scen5 %>% filter(Simulation == "9")


# function to change NAs to 0's, and replace Time with the row numbers
na.func <- function(dat){
  for(i in 1:nrow(dat)){
    if(is.na(dat$Time)[i]){
      dat$Time[i]           <- row.names(dat)[i]
      dat$Trees[i]          <- 0
      dat$Trees_est[i]      <- 0
      dat$Cull_cost[i]      <- 0
      dat$Cull_count[i]     <- 0
      dat$User_budget[i]    <- 0
      dat$Manager_budget[i] <- 0
    } 
    
  }
  return(dat)
}

# create lists
scen3_list <- list(scen3_1,scen3_2,scen3_3,scen3_4,scen3_5,scen3_6,scen3_7,scen3_8,scen3_9,scen3_10)

scen5_list <- list(scen5_1,scen5_2,scen5_3,scen5_4,scen5_5,scen5_6,scen5_7,scen5_8,scen5_9)

# apply function to lists
scen3_list <- lapply(scen3_list, na.func)
scen5_list <- lapply(scen5_list, na.func)

# rename lists
names(scen3_list) <- c("scen3_1","scen3_2","scen3_3","scen3_4","scen3_5","scen3_6","scen3_7",
                       "scen3_8","scen3_9","scen3_10")

names(scen5_list) <- c("scen5_1","scen5_2","scen5_3","scen5_4","scen5_5","scen5_6",
                       "scen5_7","scen5_8","scen5_9")

# extract to environment
list2env(scen3_list, globalenv())
list2env(scen5_list, globalenv())

# merge
scen3_noNA <- rbind(scen3_1,scen3_2,scen3_3,scen3_4,scen3_5,scen3_6,
                    scen3_7,scen3_8,scen3_9,scen3_10)

scen5_noNA <- rbind(scen5_1,scen5_2,scen5_3,scen5_4,scen5_5,scen5_6,
                    scen5_7,scen5_8,scen5_9)



### apply quantile function to all scenario dataframes

# create new list with updated scen3 & scen5
dat_list2 <- list(scen1,scen2,scen3_noNA,scen4,scen5_noNA)
quants.ls <- lapply(dat_list2, quant.func)
names(quants.ls) <- c("scen1_quants","scen2_quants","scen3_quants","scen4_quants","scen5_quants")

# extract to global env
list2env(quants.ls, globalenv())

# add scenario
scen1_quants$Scenario <- "1"
scen2_quants$Scenario <- "2"
scen3_quants$Scenario <- "3"
scen4_quants$Scenario <- "4"
scen5_quants$Scenario <- "5"

# merge 
quants_all <- rbind(scen1_quants,scen2_quants,scen3_quants,scen4_quants,scen5_quants)


# plot (facets)
all_facets_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Scenario))+
                    geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
                    geom_line(size=1,aes(color=Scenario))+
                    facet_wrap(~Scenario)+
                    theme_classic()

ggsave("outputs/investment/scenarios/Plots/Run_5/All_facets_ribbons.png", all_facets_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# plot no facets
all_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Scenario))+
              geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
              geom_line(size=1,aes(color=Scenario))+
              theme_classic()+
              ylab("Number of trees")

ggsave("outputs/investment/scenarios/Plots/Run_5/All_ribbons.png", all_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# scenarios 1:3 no facets
quants_1_3 <- quants_all %>% filter(Scenario=="1"|Scenario=="2"|Scenario=="3")

S1_3_ribbon <- ggplot(quants_1_3, aes(x=Time, y=Mean, group=Scenario))+
              geom_ribbon(data=quants_1_3, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
              geom_line(size=1,aes(color=Scenario))+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=15),
                    legend.text = element_text(size=15),
                    legend.title = element_text(size=15),
                    legend.key.size = unit(1, 'cm'))+
              ylab("Number of trees")

ggsave("outputs/investment/scenarios/Plots/Run_5/S1_3_ribbons.png", S1_3_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# scenarios 4 & 5 no facet
quants_4_5 <- quants_all %>% filter(Scenario=="4"|Scenario=="5")

S4_S5_ribbon <- ggplot(quants_4_5, aes(x=Time, y=Mean, group=Scenario))+
                geom_ribbon(data=quants_4_5, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
                geom_line(size=1,aes(color=Scenario))+
                theme_classic()+
                theme(axis.title = element_text(size=15),
                      axis.text = element_text(size=15),
                      legend.text = element_text(size=15),
                      legend.title = element_text(size=15),
                      legend.key.size = unit(1, 'cm'))+
                ylab("Number of trees")

ggsave("outputs/investment/scenarios/Plots/Run_5/S4_S5_ribbon.png", S4_S5_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# zoom in on the end
all_zoom <- ggplot(quants_all, aes(x=Time, y=Mean, group=Scenario))+
            geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
            geom_line(size=1,aes(color=Scenario))+
            theme_classic()+
            ylim(0,25000)+
            xlim(40,50)

ggsave("outputs/investment/scenarios/Plots/Run_5/Zoom_ribbons.png", all_zoom,
       dpi=300, width = 30, height = 20, units="cm")


## Results = 100 runs ####

#### These are the results from Run 5 (final results) that were run by Brad. Each scenario was run 100 times

# Scenario 1
scen1 <- list.files(path = "./outputs/investment/scenarios/Run_5/100_reps/scen1",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen1$Simulation <- rep(as.factor(1:100), each=50)
scen1$Manager_budget <- 500
scen1 <- scen1 %>% select(Time,Trees,Trees_est,Cull_cost,Cull_count,User_budget, Manager_budget,Simulation)


# Scenario 2
scen2 <- list.files(path = "./outputs/investment/scenarios/Run_5/100_reps/scen2",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen2$Simulation <- rep(as.factor(1:100), each=50) 


# Scenario 3
scen3 <- list.files(path = "./outputs/investment/scenarios/Run_5/100_reps/scen3",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen3$Simulation <- rep(as.factor(1:100), each=50) 


# Scenario 4
scen4 <- list.files(path = "./outputs/investment/scenarios/Run_5/100_reps/scen4",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen4$Simulation <- rep(as.factor(1:100), each=50) 


# Scenario 5
scen5 <- list.files(path = "./outputs/investment/scenarios/Run_5/100_reps/scen5",
                    pattern = "*.csv",
                    full.names = T) %>% 
  map_df(~read_csv(.,))

scen5$Simulation <- rep(as.factor(1:100), each=50)



### Plots

# put all df's into a list
dat_list <- list(scen1,scen2,scen3,scen4,scen5)

# plot functions
plot.trees <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Trees, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    theme(legend.position = "none",
          axis.title = element_text(size=15),
          axis.text = element_text(size=15))+
    ylim(0,100000)
  return(plot)
}

plot.cull.count <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_count, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    theme(legend.position = "none",
          axis.title = element_text(size=15),
          axis.text = element_text(size=15))+
    ylab("Cull count")
  
  return(plot)
}

plot.cull.cost <- function(dat){
  plot <- ggplot(dat, aes(x=Time, y=Cull_cost, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    theme(legend.position = "none",
          axis.title = element_text(size=15),
          axis.text = element_text(size=15))+
    ylab("Cull cost")
  
  return(plot)
}

# for scenarios 1:3
plot.budgets <- function(dat){
  
  require('patchwork')
  
  dat2 <- dat[1:50, ]
  
  plot1 <- ggplot(dat2, aes(x=Time, y=Manager_budget))+
    geom_line(size=1, color="dodgerblue3")+
    theme_classic()+
    ylab("Manager budget")+
    theme(axis.title = element_text(size=15),
          axis.text = element_text(size=15))
  
  return(plot1)
}

# for scenarios 4 and 5
plot.budgets2 <- function(dat){
  
  require('patchwork')
  
  plot1 <- ggplot(dat, aes(x=Time, y=Manager_budget, group=Simulation, color=Simulation))+
    geom_line(size=1)+
    theme_classic()+
    ylab("Manager budget")+
    theme(axis.title = element_text(size=15),
          axis.text = element_text(size=15),
          legend.position = "none")
  
  
  return(plot1)
}


# apply functions to list of data
trees_plots      <- lapply(dat_list, plot.trees)
cull_count_plots <- lapply(dat_list, plot.cull.count) 
cull_cost_plots  <- lapply(dat_list, plot.cull.cost)
budget_plots_1_3 <- lapply(dat_list[1:3], plot.budgets)
budget_plots_4_5 <- lapply(dat_list[4:5], plot.budgets2)

# re-name list elements
names(trees_plots) <- c("scen1.treePlot","scen2.treePlot","scen3.treePlot","scen4.treePlot","scen5.treePlot")

names(cull_count_plots) <- c("scen1.countPlot","scen2.countPlot","scen3.countPlot","scen4.countPlot",
                             "scen5.countPlot")

names(cull_cost_plots) <- c("scen1.costPlot","scen2.costPlot","scen3.costPlot","scen4.costPlot",
                            "scen5.costPlot")

names(budget_plots_1_3) <- c("scen1.budgetPlots","scen2.budgetPlots","scen3.budgetPlots")

names(budget_plots_4_5) <- c("scen4.budgetPlots","scen5.budgetPlots")


# extract elements to gloal environment
list2env(trees_plots, globalenv())
list2env(cull_count_plots, globalenv())
list2env(cull_cost_plots, globalenv())
list2env(budget_plots_1_3, globalenv())
list2env(budget_plots_4_5, globalenv())

# tree count plots
tree.plot.all <- scen1.treePlot + scen2.treePlot + scen3.treePlot + scen4.treePlot + scen5.treePlot
tree.plot.all[[1]] <- tree.plot.all[[1]] + ggtitle("Scenario 1")
tree.plot.all[[2]] <- tree.plot.all[[2]] + ggtitle("Scenario 2")
tree.plot.all[[3]] <- tree.plot.all[[3]] + ggtitle("Scenario 3")
tree.plot.all[[4]] <- tree.plot.all[[4]] + ggtitle("Scenario 4")
tree.plot.all[[5]] <- tree.plot.all[[5]] + ggtitle("Scenario 5")

# tree count plot for only S4 and S5
tree.plot.4_5 <- scen4.treePlot + scen5.treePlot

#ggsave("outputs/investment/scenarios/Plots/Run_5/tree_plot_all_separate.png", tree.plot.all, 
 #    width = 35, height = 25, dpi=300, units = "cm")

# cull count plots
cull.count.plot.all <- scen1.countPlot+scen2.countPlot+scen3.countPlot+scen4.countPlot+scen5.countPlot
cull.count.plot.all[[1]] <- cull.count.plot.all[[1]] + ggtitle("Scenario 1")
cull.count.plot.all[[2]] <- cull.count.plot.all[[2]] + ggtitle("Scenario 2")
cull.count.plot.all[[3]] <- cull.count.plot.all[[3]] + ggtitle("Scenario 3")
cull.count.plot.all[[4]] <- cull.count.plot.all[[4]] + ggtitle("Scenario 4")
cull.count.plot.all[[5]] <- cull.count.plot.all[[5]] + ggtitle("Scenario 5")

#ggsave("outputs/investment/scenarios/Plots/Run_5/cull_count_plot_all_separate.png", cull.count.plot.all, 
 #    width = 35, height = 25, dpi=300, units = "cm")

# cull cost plots
cull.cost.plot.all <- scen1.costPlot+scen2.costPlot+scen3.costPlot+scen4.costPlot+scen5.costPlot
cull.cost.plot.all[[1]] <- cull.cost.plot.all[[1]] + ggtitle("Scenario 1")
cull.cost.plot.all[[2]] <- cull.cost.plot.all[[2]] + ggtitle("Scenario 2")
cull.cost.plot.all[[3]] <- cull.cost.plot.all[[3]] + ggtitle("Scenario 3")
cull.cost.plot.all[[4]] <- cull.cost.plot.all[[4]] + ggtitle("Scenario 4")
cull.cost.plot.all[[5]] <- cull.cost.plot.all[[5]] + ggtitle("Scenario 5")

#ggsave("outputs/investment/scenarios/Plots/Run_5/cull_cost_plot_all_separate.png", cull.cost.plot.all, 
 #     width = 35, height = 25, dpi=300, units = "cm")



# budget plots

user_budget_p <- ggplot(scen1, aes(x=Time, y=User_budget))+
  geom_line(size=1, color="firebrick3")+
  theme_classic()+
  ylab("Community resources")+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=12))

budget.plot.all <- user_budget_p+scen1.budgetPlots+scen2.budgetPlots+scen3.budgetPlots+
  scen4.budgetPlots+scen5.budgetPlots

budget.plot.all[[1]] <- budget.plot.all[[1]] + ggtitle("All scenarios")
budget.plot.all[[2]] <- budget.plot.all[[2]] + ggtitle("Scenario 1")
budget.plot.all[[3]] <- budget.plot.all[[3]] + ggtitle("Scenario 2")
budget.plot.all[[4]] <- budget.plot.all[[4]] + ggtitle("Scenario 3")
budget.plot.all[[5]] <- budget.plot.all[[5]] + ggtitle("Scenario 4")
budget.plot.all[[6]] <- budget.plot.all[[6]] + ggtitle("Scenario 5")

# change y axis range for all manager budget plots
budget.plot.all[[2]] <- budget.plot.all[[2]] + ylim(0,1150)
budget.plot.all[[3]] <- budget.plot.all[[3]] + ylim(0,1150)
budget.plot.all[[4]] <- budget.plot.all[[4]] + ylim(0,1150)
budget.plot.all[[5]] <- budget.plot.all[[5]] + ylim(0,1150)
budget.plot.all[[6]] <- budget.plot.all[[6]] + ylim(0,1150)


#ggsave("outputs/investment/scenarios/Plots/Run_5/budget_plot_all_separate.png", budget.plot.all, 
 #    width = 35, height = 25, dpi=300, units = "cm")






### Plots for each scenario together
scen1_plots <- user_budget_p + scen1.budgetPlots + scen1.countPlot + scen1.costPlot + scen1.treePlot
scen2_plots <- user_budget_p + scen2.budgetPlots + scen2.countPlot + scen2.costPlot + scen2.treePlot
scen3_plots <- user_budget_p + scen3.budgetPlots + scen3.countPlot + scen3.costPlot + scen3.treePlot
scen4_plots <- user_budget_p + scen4.budgetPlots + scen4.countPlot + scen4.costPlot + scen4.treePlot
scen5_plots <- user_budget_p + scen5.budgetPlots + scen5.countPlot + scen5.costPlot + scen5.treePlot

#ggsave("outputs/investment/scenarios/Plots/Run_5/scen1_plots.png", scen1_plots, 
#     dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Plots/Run_5/scen2_plots.png", scen2_plots, 
#      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Plots/Run_5/scen3_plots.png", scen3_plots, 
#      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Plots/Run_5/scen4_plots.png", scen4_plots, 
#      dpi=300, width = 30, height=20, units="cm")
#ggsave("outputs/investment/scenarios/Plots/Run_5/scen5_plots.png", scen5_plots, 
#      dpi=300, width = 30, height=20, units="cm")



### get mean and error bars for each scenario

# Function to extract 50, 2.5, and 97.5% quantiles from each scenario
quant.func <- function(dat){
  wide.dat <- pivot_wider(dat,id_cols = Time, names_from = Simulation, values_from = Trees)
  wide.dat <- wide.dat[ ,-1]
  Mean.q   <- apply(wide.dat,1,quantile,probs=0.5)
  LCL.q    <- apply(wide.dat,1,quantile,probs=0.025)
  UCL.q    <- apply(wide.dat,1,quantile,probs=0.975)
  
  quant.df <- data.frame(Time = 1:50,
                         Mean = Mean.q,
                         LCL = LCL.q,
                         UCL = UCL.q)
  return(quant.df)
}


## If the trees go extinct during a simulation, the simulation ends, i.e., all subsequent rows are NAs for all parameters. Therefore I need to remove the NA rows in all of the dataframes before running the below function, otherwise it throws an error

# ID the scenarios with NAs
sum(is.na(scen1))
sum(is.na(scen2))
sum(is.na(scen3))
sum(is.na(scen4))
sum(is.na(scen5))
# scen3 & scen5

# split scenario 3
scen3_1 <- scen3 %>% filter(Simulation == "1")
scen3_2 <- scen3 %>% filter(Simulation == "2")
scen3_3 <- scen3 %>% filter(Simulation == "3")
scen3_4 <- scen3 %>% filter(Simulation == "4")
scen3_5 <- scen3 %>% filter(Simulation == "5")
scen3_6 <- scen3 %>% filter(Simulation == "6")
scen3_7 <- scen3 %>% filter(Simulation == "7")
scen3_8 <- scen3 %>% filter(Simulation == "8")
scen3_9 <- scen3 %>% filter(Simulation == "9")
scen3_10 <- scen3 %>% filter(Simulation == "10")

# split scenario 5
scen5_1 <- scen5 %>% filter(Simulation == "1")
scen5_2 <- scen5 %>% filter(Simulation == "2")
scen5_3 <- scen5 %>% filter(Simulation == "3")
scen5_4 <- scen5 %>% filter(Simulation == "4")
scen5_5 <- scen5 %>% filter(Simulation == "5")
scen5_6 <- scen5 %>% filter(Simulation == "6")
scen5_7 <- scen5 %>% filter(Simulation == "7")
scen5_8 <- scen5 %>% filter(Simulation == "8")
scen5_9 <- scen5 %>% filter(Simulation == "9")


# function to change NAs to 0's, and replace Time with the row numbers
na.func <- function(dat){
  for(i in 1:nrow(dat)){
    if(is.na(dat$Time)[i]){
      dat$Time[i]           <- row.names(dat)[i]
      dat$Trees[i]          <- 0
      dat$Trees_est[i]      <- 0
      dat$Cull_cost[i]      <- 0
      dat$Cull_count[i]     <- 0
      dat$User_budget[i]    <- 0
      dat$Manager_budget[i] <- 0
    } 
    
  }
  return(dat)
}


na.func <- function(dat){
  
    if(is.na(dat$Time)[i]){
      dat$Time[i]           <- row.names(dat)[i]
      dat$Trees[i]          <- 0
      dat$Trees_est[i]      <- 0
      dat$Cull_cost[i]      <- 0
      dat$Cull_count[i]     <- 0
      dat$User_budget[i]    <- 0
      dat$Manager_budget[i] <- 0
    
  }
  return(dat)
}

scen3 <- apply(scen3, 2, na.func)


# create lists
scen3_list <- list(scen3_1,scen3_2,scen3_3,scen3_4,scen3_5,scen3_6,scen3_7,scen3_8,scen3_9,scen3_10)

scen5_list <- list(scen5_1,scen5_2,scen5_3,scen5_4,scen5_5,scen5_6,scen5_7,scen5_8,scen5_9)

# apply function to lists
scen3_list <- lapply(scen3_list, na.func)
scen5_list <- lapply(scen5_list, na.func)

# rename lists
names(scen3_list) <- c("scen3_1","scen3_2","scen3_3","scen3_4","scen3_5","scen3_6","scen3_7",
                       "scen3_8","scen3_9","scen3_10")

names(scen5_list) <- c("scen5_1","scen5_2","scen5_3","scen5_4","scen5_5","scen5_6",
                       "scen5_7","scen5_8","scen5_9")

# extract to environment
list2env(scen3_list, globalenv())
list2env(scen5_list, globalenv())

# merge
scen3_noNA <- rbind(scen3_1,scen3_2,scen3_3,scen3_4,scen3_5,scen3_6,
                    scen3_7,scen3_8,scen3_9,scen3_10)

scen5_noNA <- rbind(scen5_1,scen5_2,scen5_3,scen5_4,scen5_5,scen5_6,
                    scen5_7,scen5_8,scen5_9)



### apply quantile function to all scenario dataframes

# create new list with updated scen3 & scen5
dat_list2 <- list(scen1,scen2,scen3_noNA,scen4,scen5_noNA)
quants.ls <- lapply(dat_list2, quant.func)
names(quants.ls) <- c("scen1_quants","scen2_quants","scen3_quants","scen4_quants","scen5_quants")

# extract to global env
list2env(quants.ls, globalenv())

# add scenario
scen1_quants$Scenario <- "1"
scen2_quants$Scenario <- "2"
scen3_quants$Scenario <- "3"
scen4_quants$Scenario <- "4"
scen5_quants$Scenario <- "5"

# merge 
quants_all <- rbind(scen1_quants,scen2_quants,scen3_quants,scen4_quants,scen5_quants)


# plot (facets)
all_facets_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Scenario))+
  geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
  geom_line(size=1,aes(color=Scenario))+
  facet_wrap(~Scenario)+
  theme_classic()

ggsave("outputs/investment/scenarios/Plots/Run_5/All_facets_ribbons.png", all_facets_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# plot no facets
all_ribbon <- ggplot(quants_all, aes(x=Time, y=Mean, group=Scenario))+
  geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
  geom_line(size=1,aes(color=Scenario))+
  theme_classic()+
  ylab("Number of trees")

ggsave("outputs/investment/scenarios/Plots/Run_5/All_ribbons.png", all_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# scenarios 1:3 no facets
quants_1_3 <- quants_all %>% filter(Scenario=="1"|Scenario=="2"|Scenario=="3")

S1_3_ribbon <- ggplot(quants_1_3, aes(x=Time, y=Mean, group=Scenario))+
  geom_ribbon(data=quants_1_3, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
  geom_line(size=1,aes(color=Scenario))+
  theme_classic()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.size = unit(1, 'cm'))+
  ylab("Number of trees")

ggsave("outputs/investment/scenarios/Plots/Run_5/S1_3_ribbons.png", S1_3_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# scenarios 4 & 5 no facet
quants_4_5 <- quants_all %>% filter(Scenario=="4"|Scenario=="5")

S4_S5_ribbon <- ggplot(quants_4_5, aes(x=Time, y=Mean, group=Scenario))+
  geom_ribbon(data=quants_4_5, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
  geom_line(size=1,aes(color=Scenario))+
  theme_classic()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.size = unit(1, 'cm'))+
  ylab("Number of trees")

ggsave("outputs/investment/scenarios/Plots/Run_5/S4_S5_ribbon.png", S4_S5_ribbon,
       dpi=300, width = 30, height = 20, units="cm")


# zoom in on the end
all_zoom <- ggplot(quants_all, aes(x=Time, y=Mean, group=Scenario))+
  geom_ribbon(data=quants_all, aes(x=Time, ymin=LCL, ymax=UCL,fill=Scenario),alpha=0.3)+
  geom_line(size=1,aes(color=Scenario))+
  theme_classic()+
  ylim(0,25000)+
  xlim(40,50)

ggsave("outputs/investment/scenarios/Plots/Run_5/Zoom_ribbons.png", all_zoom,
       dpi=300, width = 30, height = 20, units="cm")




## Harvest under max conflict - run 5 ####


# This is to get a better understanding of the power dynamics that are going on under the hood. The harvest under maximum conflict is a single value for each time step that is based on the manager and user budgets in each time step, and it is the maximum number of trees a user can harvest if the manager uses all of their budget to reduce culling and the user uses all of their budget/power to cull.

# The manager uses 10 budget points to increase the cost of culling by 1
# So for example, if the manager and user budgets are both 1000...
# The manager can increase the cost of culling by 100 (1000/10)
# There is always the minimum cost of an action = 10
# Therefore the actual cost of culling is 110 (manager's increase + the minimum cost)
# Therefore the maximum number of trees the user can cull is 9.09 (user budget/cost = 1000/110)


### I will calculate this value for each scenario and plot it on or alongside the manager budget plots for each scenario

# Load budgets
S1_budgets <- read.csv("Budgets/Investment/Run_5/S1_budgets.csv", header = T)
S2_budgets <- read.csv("Budgets/Investment/Run_5/S2_budgets.csv", header = T)
S3_budgets <- read.csv("Budgets/Investment/Run_5/S3_budgets.csv", header = T)
S4_budgets <- read.csv("Budgets/Investment/Run_5/S4_budgets.csv", header = T)
S5_budgets <- read.csv("Budgets/Investment/Run_5/S5_budgets.csv", header = T)

# function to calculate harvest under maximum conflict
HUMC.func <- function(dat){
  
  dat$cost <- (dat$Manager_budget/10)+10
  dat$Max_harvest <- dat$User_budget/dat$cost
  
  return(dat)
}

# Put budgets into a list
budget_list <- list(S1_budgets,S2_budgets,S3_budgets,S4_budgets,S5_budgets)

# apply function
budget_list <- lapply(budget_list, HUMC.func)

# rename list elements
names(budget_list) <- c("S1_budgets","S2_budgets","S3_budgets","S4_budgets","S5_budgets")

# unlist to environment
list2env(budget_list, globalenv())


# add simulation to scenario 5
S5_budgets$Simulation <- rep(1:10, each=50)

## Plots

# scenario 1
p.S1.MaxH <- ggplot(S1_budgets, aes(x=Time, y=Max_harvest))+
            geom_line(size=1)+
            theme_classic()+
            theme(axis.title = element_text(size=15),
                  axis.text = element_text(size=12))+
            ylab("Harvest under maximum conflict")

# Add additional plots from S1 (produced in the "results" section above)
#S1.plots <- user_budget_p + scen1.budgetPlots + scen1.countPlot + p.S1.MaxH
#S1.plots[[1]] <- S1.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S1.plots[[2]] <- S1.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S1.plots[[3]] <- S1.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S1_plots.png", S1.plots, dpi=300, width = 30, height=20, units="cm")


# scenario 2
p.S2.MaxH <- ggplot(S2_budgets, aes(x=Time, y=Max_harvest))+
              geom_line(size=1)+
              theme_classic()+
              theme(axis.title = element_text(size=15),
                    axis.text = element_text(size=12))+
              ylab("Harvest under maximum conflict")

# Add additional plots from S2 (produced in the "results" section above)
#S2.plots <- user_budget_p + scen2.budgetPlots + scen2.countPlot + p.S2.MaxH
#S2.plots[[1]] <- S2.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S2.plots[[2]] <- S2.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S2.plots[[3]] <- S2.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S2_plots.png", S2.plots, dpi=300, width = 30, height=20, units="cm")


# scenario 3
p.S3.MaxH <- ggplot(S3_budgets, aes(x=Time, y=Max_harvest))+
  geom_line(size=1)+
  theme_classic()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=12))+
  ylab("Harvest under maximum conflict")

# Add additional plots from S3 (produced in the "results" section above)
#S3.plots <- user_budget_p + scen3.budgetPlots + scen3.countPlot + p.S3.MaxH
#S3.plots[[1]] <- S3.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S3.plots[[2]] <- S3.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S3.plots[[3]] <- S3.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S3_plots.png", S3.plots, dpi=300, width = 30, height=20, units="cm")


# scenario 4
p.S4.MaxH <- ggplot(S4_budgets, aes(x=Time, y=Max_harvest))+
  geom_line(size=1)+
  theme_classic()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=12))+
  ylab("Harvest under maximum conflict")

# Add additional plots from S4 (produced in the "results" section above)
#S4.plots <- user_budget_p + scen4.budgetPlots + scen4.countPlot + p.S4.MaxH
#S4.plots[[1]] <- S4.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S4.plots[[2]] <- S4.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S4.plots[[3]] <- S4.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S4_plots.png", S4.plots, dpi=300, width = 30, height=20, units="cm")


# scenario 5
S5_budgets$Simulation <- as.factor(S5_budgets$Simulation)
p.S5.MaxH <- ggplot(S5_budgets, aes(x=Time, y=Max_harvest, group=Simulation, color=Simulation))+
  geom_line(size=1)+
  theme_classic()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=12))+
  ylab("Harvest under maximum conflict")

# Add additional plots from S5 (produced in the "results" section above)
#S5.plots <- user_budget_p + scen5.budgetPlots + scen5.countPlot + p.S5.MaxH
#S5.plots[[1]] <- S5.plots[[1]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S5.plots[[2]] <- S5.plots[[2]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))
#S5.plots[[3]] <- S5.plots[[3]] + theme(axis.title = element_text(size=15),axis.text = element_text(size=12))

#ggsave("outputs/investment/scenarios/HUMC/S5_plots.png", S5.plots, dpi=300, width = 30, height=20, units="cm")


## plot all HUMC together

# first take a mean for S4 & S5 to reduce it to a single line
S4_mean <- S4_budgets %>% group_by(Time) %>% summarise_at(.,vars(Max_harvest),mean)
S4_mean$Scenario <- "4"

S5_mean <- S5_budgets %>% group_by(Time) %>% summarise_at(.,vars(Max_harvest),mean)
S5_mean$Scenario <- "5"

# merge data
HUMC_all <- data.frame(Time = 1:50,
                       Max_harvest = c(S1_budgets$Max_harvest, S2_budgets$Max_harvest,
                                       S3_budgets$Max_harvest, S4_mean$Max_harvest,
                                       S5_mean$Max_harvest),
                       Scenario = rep(c("1","2","3","4","5"), each=50))

humc_all <- ggplot(HUMC_all, aes(x=Time, y=Max_harvest, group=Scenario, color=Scenario))+
  geom_line(size=1)+
  theme_classic()+
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=12))+
  ylab("Harvest under maximum conflict")

ggsave("outputs/investment/scenarios/HUMC/Run_5/humc_all.png", humc_all, dpi=300, 
       width = 30, height=20, units="cm")


## floor all of the Max_harvest values (as users can't harvest half a tree)
HUMC_all$Floor <- floor(HUMC_all$Max_harvest)

humc_all_floor <- ggplot(HUMC_all, aes(x=Time, y=Floor, group=Scenario, color=Scenario))+
                  geom_line(size=1)+
                  #facet_wrap(~Scenario)+
                  theme_classic()+
                  theme(axis.title = element_text(size=15),
                        axis.text = element_text(size=12))+
                  ylab("Floored harvest under maximum conflict")

ggsave("outputs/investment/scenarios/HUMC/Run_5/humc_all_floor.png", humc_all_floor, 
       dpi=300, width = 30, height=20, units="cm")
