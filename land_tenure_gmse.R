### This script is the simulation modelling for land tenure using GMSE.  

## The approach is going to be to build the scenario step by step, increasing complexity as I go.


### Explanation ####

# in this simulation I want to create a system that broadly reflects a situation inside a Cambodian PA where multiple communities exist, and have different levels of land tenure.  This is common in real life in Cambodia but importantly is also common elswhere.  We want to find a balance between a model that is complex enough to reflect real word dynamics, yet general enough to be applicable in many different contexts.  The current plan is to have three different types of land tenure: some communities have individual land tenure (i.e. a single family owns farmland), some communities have communally owned land (ICTs), and some communities have no tenure at all.

# current thinking is to run separate simulations for each tenure type, as this avoids the need for additional coding in GMSE to allow managers to set different policies for each user etc.

# current thinking is, in the ICT scenario, to treat a single "user" as an indigenous community. Initially I had thought to make each user a family, and then just make all the users act together and have the same budgets etc., but it was pointed out that this is precisely what we would achieve by treating a single user as a community.  Another positive of treating a single user as a whole community, is that we can more easily simulate a landscape where there are multiple communities (i.e. multiple users). 

# current thinking is to treat an individual resource as an individual tree.  Brad says this will make more sense in the current set up of GMSE.  

### Load libraries ####
library('GMSE')


### ten_rep_1 - 20 reps, multiple users, no land ownership, low cost, low manager budget, no yield increases ####

# In this simulation I want the users to be able to clear forest easily, and the manager to have very little power to stop them. Users will not gain any additional yeild from tending crops (i.e. no real benefit in tending crops)

# Here the manager will have imperfect observation

# I am going to replicate the simulation 20 times to see what happens to the resources over multiple runs. Each simulation will be over 40 time steps

# I have done a very brief search for a roughly appropriate number of trees per landscape cell. Sagar & Singh 2006 ( https://www.jstor.org/stable/44521915) report a range of stem densities for several tropical forest sites which range from 35 - 419 stems per ha. Let's assume for now that a landscape cell is 1 ha.  I will go for a lower density value for now, otherwise simulations will take ages.  When running the more "final" models later on I will do a more thorough search for an appropriate tree density.


ten_rep_1 <- gmse_replicates(
  time_max = 40,
  land_dim_1 = 100,
  land_dim_2 = 100,
  res_movement = 0, # maximum movement of resources in given time step 
  remove_pr = 0, # Assume no death 
  lambda = 0, # assume no growth
  agent_view = 10, # distance (cells) agent can see (currently only manager during obs process)
  agent_move = 50, # distance (cells) agents can travel (mostly affects managers during obs process)
  res_birth_K = 0, # no new births
  res_death_K = 3000000, # carrying capacity set to way above starting number of resources
  res_move_type = 0, # 0=no move, 
  res_death_type = 1, # 1=density-independent 
  observe_type = 0, # 0=density-based sampling 
  times_observe = 1, # observes once
  obs_move_type = 1, # uniform in any direction
  res_min_age = 0, # age of resources before agents record/act on them
  res_move_obs = FALSE, # trees don't move
  plotting = FALSE, 
 
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
  culling = TRUE, # is culling an option
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