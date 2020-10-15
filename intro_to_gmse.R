## This script is for learning and experimenting with GMSE

### Load libraries ####
library('GMSE')

## run basic simulation (using defaults) showing ALL parameter settings and explanations ####


sim1 <- gmse(
  time_max = 40,
  land_dim_1 = 100,
  land_dim_2 = 100,
  res_movement = 20, # maximum movement of resources in given time step 
  remove_pr = 0, # density-independent and user-independent mortality (prob of dying) of resources 
  lambda = 0.3, # baseline pop growth of resources (i.e. each resource produces poisson(lambda) offspring in 1 time step)
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 100000, # carrying capactiy for births (i.e. max number of new births in a timestep)
  res_death_K = 2000, # carrying capacity on resources in population. As pop approaches this, mortality prob increases
  edge_effect = 1, # only option - torus landscape (i.e. no edge)  
  res_move_type = 1, # 0=no move, 1=rand uniform any dir, 2=poisson xy, 3=unif but times=res_movement per time step
  res_birth_type = 2, # only option
  res_death_type = 2, # 1=density-independent (remove_pr), 2= density-dependent (res_death_K), 3 = both
  observe_type = 0, # 0=density-based sampling (agent_view,times_observe),1=CMR, 2=transect line (W=agent_view), 
                    # 3=transect block (agent_view)
  #fixed_mark       # only relevant if CMR used above
  #fixed_recapt     # only relevant if CMR used above
  times_observe = 2, # no default listed. Times manager samples when observe_type= 0 or 1
  obs_move_type = 1, # Type of movement of agents.Identical to res_move_type
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = TRUE, # do resources move between sampling periods within a timestep?
  Euclidean_dist = FALSE, # should distance be judged as euclidean distance or number of cells
  plotting = TRUE, # Should simulation results be plotted?
  hunt = FALSE, # whether simulation stops to ask user how many resources they want to hunt
  start_hunting = 95, # the time step in which the human is allowed to start hunting (if hunt=TRUE)
  res_consume = 0.5, # fraction of remaining biomass that a resource consumes whilst occupying a cell
  ga_popsize = 100, # the size of the population of actions (replicate agents) for each user in the genetic algorithm
  ga_mingen = 40, # minimum number of generation in the genetic algorithm. i.e. number of generations the ga_popsize actions
                  # undergo natural selection to find best strategy
  ga_seedrep = 20, # the number of replicate agents in the population of ga_popsize that are exact replicates 
  ga_sampleK = 20, # number of replicate agents from ga_popsize that are selected for tournament to move to next generation
  ga_chooseK = 2, # number of strategies from each tournament selected to move to next generation
  #ga_mutation    # mutation rate of any action within an agents strategy (no default listed)
  #ga_crossover   # rate of any action within agents strategy with a randomly selected different strategy (no default)
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 5, # maximum ages of resources i.e. they die at this age and are removed
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = 1000, # total budget for manager for setting policy
  manage_target = 1000, # target resource abundance
  RESOURCE_ini = 1000, # initial abundance of resources
  scaring = FALSE, # is scaring an option
  culling = TRUE, # is castration an option
  castration = FALSE, # is castration an option
  feeding = FALSE, # is feeding an option
  help_offspring = FALSE, # is supporting resource offspring an option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  tend_crop_yld = 0.5, # per cell proprtional increase in crop yield as a result of TRUE above. 0.5=50%
  kill_crops = FALSE, # stakeholders can remove all crops from a cell, reducing yield to 0. Not controlled by manager
  stakeholders = 10, # number of stakeholders (no default)
  manage_caution = 1, # manager caution - they assume at least 1 (n) of each possible action will be done
  land_ownership = FALSE, # If true, users only act on their land. If FALSE, they can act on any cell
  manage_freq = 1, # frequency of manager setting policy - 1= every time step, 2 = every second time step etc.
  converge_crit = 1, # convergence criteria for stopping the GA (beyond ga_mingen). 1 = GA will continue as long as there
                     # is a > 1% incrase in strategy fitness
  manager_sense = 0.8, # sensitivity of what manager thinks the impacts of new policy will be on actions. e.g if manager
                       # doubles cost of culling, they will assume derease in culling will be by half * 0.8
  public_land = 0, # proprtion of land that is public (not owned by stakeholders)
  group_think = FALSE, # if TRUE all users have identical actions
  age_repr = 1, # age below which resources are unable to reproduce
  usr_budget_rng = 0, # range (normal dist) with user_budget as the mean, for user budgets 
  action_thres = 0, # deviation of the resource pop from the target below which the manager will not take action
  budget_bonus = 0, # % of initial budget that the manager getsif policy not updated in previous time step
  consume_surv = 0, # amount of yield from landscape resource needs to consume to survive
  consume_repr = 0, # amount of yield from landscape resource needs to consume to produce 1 offspring
  #times_feeding    # (no default) number of times resources move per time step to search for food
  ownership_var = 0, # amount of variation in land allocation between users when land_ownership=TRUE
  perceive_scare = NA, # users percieved effect of scaring one resource on the resource population
  perceive_cull = NA, # as above
  perceive_cast = NA, # as above
  perceive_feed = NA, # as above
  perceive_help = NA, # as above
  perceive_tend = NA, # as above
  perceive_kill = NA, # as above
  usr_yld_budget = 0, # increase in user budget as a result of their yield 
  man_yld_budget = 0, # increase in managers budget as result of mean yield of all users land
)


# plot sim1 ####

plot_gmse_results(sim_results = sim1)
plot_gmse_effort(sim_results = sim1)


# explore sim1 ####

str(sim1$resource)
sim1$resource[[1]]
print(sim1$resource)

str(sim1$observation)

str(sim1$paras)
sim1$paras

str(sim1$land)

str(sim1$time_taken)

str(sim1$agents)

str(sim1$cost)

## Land tenure simulation ####

# here I will have a first crack at setting up a simulation for land tenure. I will do this and then contact Nils and Brad and get their input. 

# in this simulation I want to create a system that broadly reflects a situation inside a Cambodian PA where multiple communities exist, and have different levels of land tenure.  This is common in real life in Cambodia.  Some communities have individual land tenure (i.e. a single family owns farmland), some communities have communally owned land (ICTs), and some communities have no tenure at all.  I am not sure if I can have three different sets of tenure options, but I will try. I am not sure how to represent individual land titles becasue the 'population density' doesn't really work if a single user represents only a single family. 

# big question - do users represent individual families or entire communities? I can't see a way of doing it except with each user reprsenting a single family.

# What are the actual differences between tenure types?  Assuming that each user represent a single family: ICTs - the utility of the land and resource is the same across all users, the costs are the same, the goals are all the same, and the budgets are the same. I.e. the group of users all act together. group_think will be set to TRUE.  The users are allowed to clear forest within their land, but not allowed outside their land (i.e. costs will be low up to a certain point, after which costs are high). For individual land titles, the owned land will be contiguous but the users do not act together. The utility of the land and resource will be the same, but the user starting budgets will vary randomly. The goals between users will potentially vary (is this set by the utility?). Again the users are allowed to clear their own land (low cost) but not allowed outside their land (high cost). For communities with no land tenure, I guess everything is the same as the individual land titled communities except that they are not allowed to clear any forest (i.e. costs are always high)? Perhaps the goal is different - perhaps they want to clear more land as fast as possible (to increase chances of getting tenure) rather than focussing on farming?  With all of the different tenures, the common factor is that budgets will increase each time step to simulate increasing population density. This will reflect increasing number of people living on the land, and therefore more pressure to increase farm yield and increasing ability to clear forest and farm (i.e. more hands). Perhaps the yield that can be achieved from farming can increase as budgets increase (to reflect more intensive farming) up to a point (maximum potential yield) after which they will need to clear more forest to increase their farmland.   

# I am not sure how I would replicate the above scenarios if a single user represented an entire community. The ability to make the users act together or separately I think is key.  

# Will it be too complicated to have all three tenure options in the same landscape/simulation?  Or would it be better to run them in separate simulations?  

# The below set up is for indigneous communal titles only
ten1 <- gmse(
  time_max = 40,
  land_dim_1 = 100,
  land_dim_2 = 100,
  res_movement = 0, # RESOURCES CAN'T MOVE 
  remove_pr = 0, # ASSUME TREES DON'T DIE OVER THIS TIME SCALE 
  lambda = 0, # ASSUME NO TREE GROWTH FOR NOW 
  agent_view = 10, # KEEP THIS AS DEFAULT FOR NOW
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 100000, # carrying capactiy for births (i.e. max number of new births in a timestep)
  res_death_K = 2000, # SHOULD DO NOTHING AS MORTALITY IS 0
  edge_effect = 1, # only option - torus landscape (i.e. no edge)  
  res_move_type = 0, # TREES DON'T MOVE
  res_birth_type = 2, # only option
  res_death_type = 1, # 1=density-independent - REMOVE_PR IS 0 SO THIS SHOULD MEAN NO DEATHS
  observe_type = 0, # LEAVE AS DENSITY-BASED SAMPLING FOR NOW
  #fixed_mark       # only relevant if CMR used above
  #fixed_recapt     # only relevant if CMR used above
  times_observe = 2, # no default listed. Times manager samples when observe_type= 0 or 1
  obs_move_type = 1, # Type of movement of agents.Identical to res_move_type
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # TREES DON'T MOVE BETWEEN TIME STEPS
  Euclidean_dist = FALSE, # should distance be judged as euclidean distance or number of cells
  plotting = TRUE, # Should simulation results be plotted?
  hunt = FALSE, # whether simulation stops to ask user how many resources they want to hunt
  start_hunting = 95, # the time step in which the human is allowed to start hunting (if hunt=TRUE)
  res_consume = 0.25, # ASSUMING 4 FOREST PATCHES PER CELL, EACH PATCH CONSUMES 25% OF CELL I.E. NO YIELD IF ALL TREES
  ga_popsize = 100, # the size of the population of actions (replicate agents) for each user in the genetic algorithm
  ga_mingen = 40, # minimum number of generation in the genetic algorithm. i.e. number of generations the ga_popsize actions
  # undergo natural selection to find best strategy
  ga_seedrep = 20, # the number of replicate agents in the population of ga_popsize that are exact replicates 
  ga_sampleK = 20, # number of replicate agents from ga_popsize that are selected for tournament to move to next generation
  ga_chooseK = 2, # number of strategies from each tournament selected to move to next generation
  #ga_mutation    # mutation rate of any action within an agents strategy (no default listed)
  #ga_crossover   # rate of any action within agents strategy with a randomly selected different strategy (no default)
  move_agents = FALSE, # AGENTS DON'T MOVE BETWEEN TIME STEPS
  max_ages = 100, # TREES DON'T NATURALLY DIE OF OLD AGE 
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 1000, # LEAVE AS DEFAULT FOR NOW
  manager_budget = 1000, # LEAVE AS DEFAULT FOR NOW
  manage_target = 1000, # NOT SURE ABOUT THIS
  RESOURCE_ini = 1000, # NOT SURE ABOUT THIS
  scaring = FALSE, # is scaring an option
  culling = TRUE, # is castration an option
  castration = FALSE, # is castration an option
  feeding = FALSE, # is feeding an option
  help_offspring = FALSE, # is supporting resource offspring an option
  tend_crops = TRUE, # USERS CAN INCREASE THEIR YIELD
  tend_crop_yld = 0.2, # DEFAULT OF 0.5 SEEMED HIGH TO ME
  kill_crops = FALSE, # stakeholders can remove all crops from a cell, reducing yield to 0. Not controlled by manager
  stakeholders = 100, # VILLAGE OF 100 FAMILIES
  manage_caution = 1, # manager caution - they assume at least 1 (n) of each possible action will be done
  land_ownership = TRUE, # USERS ACT ON THEIR LAND
  manage_freq = 1, # frequency of manager setting policy - 1= every time step, 2 = every second time step etc.
  converge_crit = 1, # convergence criteria for stopping the GA (beyond ga_mingen). 1 = GA will continue as long as there
  # is a > 1% incrase in strategy fitness
  manager_sense = 0.8, # sensitivity of what manager thinks the impacts of new policy will be on actions. e.g if manager
  # doubles cost of culling, they will assume derease in culling will be by half * 0.8
  public_land = 0.7, # 70% OF LAND IS PA, THE REST IS OWNED BY COMMUNITIES
  group_think = TRUE, # INDIGENOUS COMMUNAL LAND TITLES - COMMUNITY ACT TOGETHER
  age_repr = 1, # age below which resources are unable to reproduce
  usr_budget_rng = 0, # ALL USER BUDGETS ARE THE SAME WITHIN COMMUNITY 
  action_thres = 0, # deviation of the resource pop from the target below which the manager will not take action
  budget_bonus = 0, # % of initial budget that the manager gets if policy not updated in previous time step
  consume_surv = 0, # amount of yield from landscape resource needs to consume to survive
  consume_repr = 0, # amount of yield from landscape resource needs to consume to produce 1 offspring
  #times_feeding    # (no default) number of times resources move per time step to search for food
  ownership_var = 0, # ASSUME LAND IS EQUALLY DIVIDED AMONGST COMMUNITY
  perceive_scare = NA, # users percieved effect of scaring one resource on the resource population
  perceive_cull = NA, # as above
  perceive_cast = NA, # as above
  perceive_feed = NA, # as above
  perceive_help = NA, # as above
  perceive_tend = NA, # as above
  perceive_kill = NA, # as above
  usr_yld_budget = 0.2, # ASSUME USER BUDGET INCREASES DUE TO YEILD AS A PROPORTION 0 SET TO 20% INCREASE
  man_yld_budget = 0, # increase in managers budget as result of mean yield of all users land
)


