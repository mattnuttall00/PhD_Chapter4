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

# The density of trees in tropical forest landscapes vary hugely. In previous simulations I have assumed 50 stems/ha which is low, but not implausible (e.g. deciduous dipterocarp woodland). A reference for this value can be found here:https://www.jstor.org/stable/44521915. I am keeping this density of trees as it is for now, as this value means that there are already 2,000,000 trees on the landscape, and increasing them will increase run time. Note that the trees are distributed randomly across the landscape, and so there will not be exactly 50/cell. This reflects reality. 

# "Resources" are trees. The resources therefore do not move.

# Trees in a cell reduce the farmer's yield. The amount a tree reduces yield is governed by an exponential function: yield = (1 - % yield reduction per tree) ^ remaining trees. I want a farmer's yield to be reduced by a significant amount if all trees in a cell are standing. But the trees do not completely eliminate yield. This is a balance between the farmer being able to farm and increase their yield even when there are trees on their cell, but also providing an incentive to cull where possible. I have set each tree on a cell to reduce yield by 2%. Therefore if there are 50 trees on a cell yield is reduced to 36% of the total available yield. Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc.

# For simplicity, I am assuming there is no natural death and no natural birth (forest regeneration). remove_pr is set to 0, and lambda is set to 0. Currently there are still a few natural deaths via old age, but this is going to be changed in the code by Brad

# The carrying capacity of new resources is set to 1 as it has to be a positive number but I want it as low as possible i.e. there is no real recruitment

# Resource carrying capacity is set very high (5,000,000) to reduce density-dependent death. Although res_death_type is set to 1 (density-independent) and so I don't think this parameter should be doing anything.

# The max age of trees is set high - 1000. This is to reduce natural death caused by old age (this will hopefully become obsolete when Brad adds new code to remove natural death)

# The observation process is set to density-based sampling, with 1 observation per time step. The manager can move in any direction. Currently the manager can see 10 cells, and move 50 cells. In previous simulations (see "land_tenure_gmse.R" script) this has resulted in observation error of a few percent (max error ~2.3%). Realistically, forest cover monitoring is very accurate thanks to remote sensing. Nevertheless, I want some margin of error to reflect the fact that forest monitoring is not perfect (e.g. small areas of clearance cannot be detected easily from satellite images, and there is also a lag time in image processing so the manager's information on forest loss will not always be up to date). These values can be changed in increase error at a later date. 

# There is no minimum age for resources to be acted upon i.e. all trees in the landscape can be observed/culled

# Agents are permitted to move at the end of each time step. Because land_ownership==TRUE I believe this then only relates to the manager.

# User and manager budgets will vary based on the scenario.



############## NULL SCENARIOS #####################

# These scenarios will explore the basic manager/user dynamics and will potentially reflect the "null" scenarios, or the counterfactuals.

# I will do the following scenarios:

# N1 - Null - Manager and user budgets do not change, and are equal

# N2 - Optimistic Null - Manager and user budgets both increase linearly over time, at the same rate and from the same starting point

# N2a - Optimistic Null - Variation of N2. Manager and user budget increase linearly over time, but the manager budget rate of increase is lower than the user rate of increase

# N2b - optimistic Null - Variation of N2. Manager and user budget increase linearly over time, but the manager budget rate of increase is higher than the user rate of increase

# N3 - Pessimistic Null - Manager budget remains constant, but user budgets increase linearly 

#### N1 ####

# This null scenario has the manager and user budgets remaining static over the entire study period

N1 <- gmse(
  time_max = 50,
  land_dim_1 = 200,
  land_dim_2 = 200, # landscape is 40,000ha or 400km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 5000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # Trees have no impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 20000, # total budget of each stakeholder for performing actions
  manager_budget = 20000, # 
  manage_target = 2000000, # target resource abundance (same as starting value)
  RESOURCE_ini = 2000000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = TRUE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 16, # a village with 50 families
  land_ownership = TRUE, # land ownership
  public_land = 0.6, # 60% of the land is PA, 40% is community land
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)
