### This script is the simulation modelling for land tenure using GMSE.  

## The approach is going to be to build the scenario step by step, increasing complexity as I go.


### Explanation ####

# in this simulation I want to create a system that broadly reflects a situation inside a Cambodian PA where multiple communities exist, and have different levels of land tenure.  This is common in real life in Cambodia but importantly is also common elswhere.  We want to find a balance between a model that is complex enough to reflect real word dynamics, yet general enough to be applicable in many different contexts.  The current plan is to have three different types of land tenure: some communities have individual land tenure (i.e. a single family owns farmland), some communities have communally owned land (ICTs), and some communities have no tenure at all.

# current thinking is to run separate simulations for each tenure type, as this avoids the need for additional coding in GMSE to allow managers to set different policies for each user etc.

# current thinking is, in the ICT scenario, to treat a single "user" as an indigenous community. Initially I had thought to make each user a family, and then just make all the users act together and have the same budgets etc., but it was pointed out that this is precisely what we would achieve by treating a single user as a community.  Another positive of treating a single user as a whole community, is that we can more easily simulate a landscape where there are multiple communities (i.e. multiple users). 

# current thinking is to treat an individual resource as an individual tree.  Brad says this will make more sense in the current set up of GMSE.  

### Load libraries ####
library('GMSE')


### ten_rep_0 - single call, resources do not reduce yield, low manager budget ####
  ## Details ####

# Here I want to see what happens when there is no incentive to cull i.e. having trees in the cells does not decrase yield

  ## Single call ####

system.time(ten_rep_0 <- gmse(
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0, # Trees have no impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = 500, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 30, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
))

plot_gmse_results(sim_results = test)
ten_rep_0_summary <- gmse_table(test)
write.csv(ten_rep_0_summary, file="outputs/Land_tenure/ten_rep_0/ten_rep_0_summary.csv")


### ten_rep_1 & 2 - 10 reps, multiple users, no land ownership, low manager budget, no yield increases ####
  ## Details ####

# In this simulation I want the users to be able to clear forest easily, and the manager to have very little power to stop them, but will have the goal of preserving as much as possible. Users will not gain any additional yield from tending crops (i.e. no real benefit in tending crops)

# Here the manager will have imperfect observation

# I am going to first run a single simulation to see what is happening. Then I will replicate the simulation 10 times to see what happens to the resources over multiple runs. Each simulation will be over 40 time steps

# I have done a very brief search for a roughly appropriate number of trees per landscape cell. Sagar & Singh 2006 ( https://www.jstor.org/stable/44521915) report a range of stem densities for several tropical forest sites which range from 35 - 419 stems per ha. Let's assume for now that a landscape cell is 1 ha.  I will go for a lower density value for now, otherwise simulations will take ages.  When running the more "final" models later on I will do a more thorough search for an appropriate tree density.  I will use 50 trees per cell here

# I am keeping the landscape small to reduce the run time, as each cell has 50 trees.

# I am setting the number of stakekholders to 50 (i.e. assume a village with 50 families) over a landscape of 2500ha / 25km2

  ## Single call ####

# first run one simulation (i.e. no replicates) - 12 mins to run
system.time(ten_rep_1 <- gmse(
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
# all genetic algorithm parameters left to default

  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = 100, # Manager has very little power (10% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
))


# plot results
plot_gmse_results(sim_results = ten_rep_1)

# full results summary
gmse_summary(ten_rep_1)

# key results summary
ten_rep_1_summary <- data.frame(gmse_table(ten_rep_1, hide_unused_options = TRUE, all_time = TRUE))
write.csv(ten_rep_1_summary,  file="outputs/Land_tenure/ten_rep_1/ten_rep_1_summary.csv")


  ## Replicates ####

# Took 1 hour 55 mins
system.time(ten_rep_2 <- gmse_replicates(
  replicates = 10,
  all_time = TRUE,
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = 100, # Manager has very little power (10% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE, # users act independently
))


print(ten_rep_2)
ten_rep_2_summary <- data.frame(ten_rep_2)
write.csv(ten_rep_2_summary, file="outputs/Land_tenure/ten_rep_2/ten_rep_2_summary.csv")

### ten_rep_3 & 4 - 10 reps, multiple users, no land ownshp, slightly higer manager budget, no yield increases ####
  ## Details ####

# this is the same as above but now I will give the manager 50% of the budget of the users, to see what impact this has.


  ## Single call ####


# first run one simulation (i.e. no replicates) - 9 mins to run
system.time(ten_rep_3 <- gmse(
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = 500, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
))


# plot results
plot_gmse_results(sim_results = ten_rep_3)

# key results summary
ten_rep_3_summary <- data.frame(gmse_table(ten_rep_3, hide_unused_options = TRUE, all_time = TRUE))
write.csv(ten_rep_3_summary,  file="outputs/Land_tenure/ten_rep_3/ten_rep_3_summary.csv")


  ## Replicates ####

# 1hr 24 mins
system.time(ten_rep_4 <- gmse_replicates(
  replicates = 10,
  all_time = TRUE,
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = 500, # Manager has very little power (10% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE, # users act independently
))

ten_rep_4_summary <- data.frame(ten_rep_4)
ten_rep_4_summary$sim <- rep(c(1:10), each=40)
write.csv(ten_rep_4_summary, file="outputs/Land_tenure/ten_rep_4/ten_rep_4_summary.csv")

plot_gmse_results(sim_results = ten_rep_4)

### ten_rep_5 - dynamic manager budget, mutple users, no land ownshp, no yield increases ####
  ## Details ####

# Here I want to explore the threshold for the manager budget of when a manager is able to stop culling.  The details will all be the same as the simulation above, but the manager budget will increase incrementally

  ## Single call ####

# manager budget
mb <- 500

# sim_old
ten_rep_5_simold <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = mb, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)

# matrix for results
ten_rep_5 <- matrix(data=NA, nrow=40, ncol=6)

# loop the simulation. Took 11 mins
for(time_step in 1:40){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = ten_rep_5_simold, manager_budget=mb)
  
  ten_rep_5[time_step, 1] <- time_step
  ten_rep_5[time_step, 2] <- sim_new$basic_output$resource_results[1]
  ten_rep_5[time_step, 3] <- sim_new$basic_output$observation_results[1]
  ten_rep_5[time_step, 4] <- sim_new$basic_output$manager_results[3]
  ten_rep_5[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  ten_rep_5[time_step, 6] <- mb
  
  ten_rep_5_simold <- sim_new
  mb <- mb + 20
  
}

colnames(ten_rep_5) <- c("Time", "Pop_size", "Pop_est", "Cull_cost", "Cull_count",
                         "Manager_budget")
ten_rep_5_summary <- data.frame(ten_rep_5)
write.csv(ten_rep_5, file="outputs/Land_tenure/ten_rep_5/ten_rep_5_summary.csv")


### ten_rep_6 - as above, increasing manager budget further ####
  ## Details ####

# In ten_rep_5 I didn't find the manager budget which was high enough to eliminate (or nearly eliminate) cutting of trees. So here I will increase the budget futher to see if I can find that theshold.

  ## Call ####


# manager budget
mb <- 500

# sim_old
ten_rep_6_simold <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = mb, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)

# matrix for results
ten_rep_6 <- matrix(data=NA, nrow=40, ncol=6)

# loop the simulation. Took 11 mins
for(time_step in 1:40){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = ten_rep_6_simold, manager_budget=mb)
  
  ten_rep_6[time_step, 1] <- time_step
  ten_rep_6[time_step, 2] <- sim_new$basic_output$resource_results[1]
  ten_rep_6[time_step, 3] <- sim_new$basic_output$observation_results[1]
  ten_rep_6[time_step, 4] <- sim_new$basic_output$manager_results[3]
  ten_rep_6[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  ten_rep_6[time_step, 6] <- mb
  
  ten_rep_6_simold <- sim_new
  mb <- mb + 50
  
}

colnames(ten_rep_6) <- c("Time", "Pop_size", "Pop_est", "Cull_cost", "Cull_count",
                         "Manager_budget")
ten_rep_6_summary <- data.frame(ten_rep_6)
write.csv(ten_rep_6, file="outputs/Land_tenure/ten_rep_6/ten_rep_6_summary.csv")




### ten_rep_7 - as above, increasing manager budget further ####
  ## Details ####

# increasing manager budget even further

  ## Call ####


# manager budget
mb <- 500

# sim_old
ten_rep_7_simold <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = mb, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)

# matrix for results
ten_rep_7 <- matrix(data=NA, nrow=40, ncol=6)

# loop the simulation. Took 11 mins
for(time_step in 1:40){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = ten_rep_7_simold, manager_budget=mb)
  
  ten_rep_7[time_step, 1] <- time_step
  ten_rep_7[time_step, 2] <- sim_new$basic_output$resource_results[1]
  ten_rep_7[time_step, 3] <- sim_new$basic_output$observation_results[1]
  ten_rep_7[time_step, 4] <- sim_new$basic_output$manager_results[3]
  ten_rep_7[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  ten_rep_7[time_step, 6] <- mb
  
  ten_rep_7_simold <- sim_new
  mb <- mb + 100
  
}

colnames(ten_rep_7) <- c("Time", "Pop_size", "Pop_est", "Cull_cost", "Cull_count",
                         "Manager_budget")
ten_rep_7_summary <- data.frame(ten_rep_7)
write.csv(ten_rep_7, file="outputs/Land_tenure/ten_rep_7/ten_rep_7_summary.csv")


### ten_rep_8 - as ten_rep_7 but increasing variation in user budget (usr_budget_rng) ####
  ## Details ####

# Here I will introduce some variation in user budgets to try and reduce the appearance of steps in the plot of manager budget versus cull counts

  ## Call ####

# manager budget
mb <- 500

# sim_old
ten_rep_8_simold <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  usr_budget_rng = 100, # introduce a range of 100 (budget units) around the mean user budget
  manager_budget = mb, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)

# matrix for results
ten_rep_8 <- matrix(data=NA, nrow=40, ncol=6)

# loop the simulation. Took 11 mins
for(time_step in 1:40){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = ten_rep_8_simold, manager_budget=mb)
  
  ten_rep_8[time_step, 1] <- time_step
  ten_rep_8[time_step, 2] <- sim_new$basic_output$resource_results[1]
  ten_rep_8[time_step, 3] <- sim_new$basic_output$observation_results[1]
  ten_rep_8[time_step, 4] <- sim_new$basic_output$manager_results[3]
  ten_rep_8[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  ten_rep_8[time_step, 6] <- mb
  
  ten_rep_8_simold <- sim_new
  mb <- mb + 100
  
}

colnames(ten_rep_8) <- c("Time", "Pop_size", "Pop_est", "Cull_cost", "Cull_count",
                         "Manager_budget")
ten_rep_8_summary <- data.frame(ten_rep_8)
write.csv(ten_rep_8, file="outputs/Land_tenure/ten_rep_8/ten_rep_8_summary.csv")

### ten_rep_9, 10, 11 - manager budget is static, user budgets static ####
  ## Details ####

# First some basic exploration of the user budget. I will start with 3 simulations where the user budget is static but set to 3 different levels: below the same, and above the manager budget
  ## Calls ####

# ten_rep_9 (manager budget=1000, user budget=100)
ten_rep_9 <- gmse(
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 100, # total budget of each stakeholder for performing actions
  usr_budget_rng = 10, # 10% range
  manager_budget = 1000, # manage has lots of power
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)

# plot results
plot_gmse_results(sim_results = ten_rep_9)

# key results summary
ten_rep_9_summary <- data.frame(gmse_table(ten_rep_9, hide_unused_options = TRUE, all_time = TRUE))
write.csv(ten_rep_9_summary,  file="outputs/Land_tenure/ten_rep_9/ten_rep_9_summary.csv")


# ten_rep_10 (manager and user budgets are the same)
ten_rep_10 <- gmse(
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  usr_budget_rng = 100, # 10% range
  manager_budget = 1000, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)

# plot results
plot_gmse_results(sim_results = ten_rep_10)

# key results summary
ten_rep_10_summary <- data.frame(gmse_table(ten_rep_10, hide_unused_options = TRUE, all_time = TRUE))
write.csv(ten_rep_10_summary,  file="outputs/Land_tenure/ten_rep_10/ten_rep_10_summary.csv")


# ten_rep_11 (manager budget 1000, user budget 10,000)
ten_rep_11 <- gmse(
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 10000, # total budget of each stakeholder for performing actions
  usr_budget_rng = 1000, # 10% range
  manager_budget = 1000, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)

# plot results
plot_gmse_results(sim_results = ten_rep_11)

# key results summary
ten_rep_11_summary <- data.frame(gmse_table(ten_rep_11, hide_unused_options = TRUE, all_time = TRUE))
write.csv(ten_rep_11_summary,  file="outputs/Land_tenure/ten_rep_11/ten_rep_11_summary.csv")
### ten_rep_12 - dynamic user budget ####
  ## Details ####

# here I will use gmse_apply to start the user budget well below the manager's, and the increase it incrementally so that it overtakes the manager budget by quite a lot

  ## Call ####

# user budget
ub <- 100

# user budget range (10% of user budget)
ubr <- (ub/100)*10

# sim_old
ten_rep_12_simold <- gmse_apply(
  res_mod = resource,
  obs_mod = observation,
  man_mod = manager,
  use_mod = user,
  get_res = "FUll",
  land_dim_1 = 50,
  land_dim_2 = 50, # landscape is 2500ha or 25km2
  res_movement = 0, # trees don't move 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 1, # must be positive value, but I want it small i.e. no real recruitment
  res_death_K = 500000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
  res_consume = 0.02, # For now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is based on the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = ub, # total budget of each stakeholder for performing actions
  usr_budget_rng = ubr, # 10% of ub
  manager_budget = 1000, # Manager has static budget
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
)

# matrix for results
ten_rep_12 <- matrix(data=NA, nrow=40, ncol=6)

# loop the simulation. Took 11 mins
for(time_step in 1:40){
  
  sim_new <- gmse_apply(get_res = "Full", old_list = ten_rep_12_simold, user_budget=ub, 
                        usr_budget_rng = ubr)
  
  ten_rep_12[time_step, 1] <- time_step
  ten_rep_12[time_step, 2] <- sim_new$basic_output$resource_results[1]
  ten_rep_12[time_step, 3] <- sim_new$basic_output$observation_results[1]
  ten_rep_12[time_step, 4] <- sim_new$basic_output$manager_results[3]
  ten_rep_12[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  ten_rep_12[time_step, 6] <- mb
  
  ten_rep_12_simold <- sim_new
  ub <- ub + 100
  
}

colnames(ten_rep_12) <- c("Time", "Pop_size", "Pop_est", "Cull_cost", "Cull_count",
                         "Manager_budget")
ten_rep_12_summary <- data.frame(ten_rep_12)
write.csv(ten_rep_12, file="outputs/Land_tenure/ten_rep_12/ten_rep_12_summary.csv")