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

# In this simulation I want the users to be able to clear forest easily, and the manager to have very little power to stop them, but will have the goal of preserving as much as possible. Users will not gain any additional yield from tending crops (i.e. no real benefit in tending crops)

# Here the manager will have imperfect observation

# I am going to replicate the simulation 20 times to see what happens to the resources over multiple runs. Each simulation will be over 40 time steps

# I have done a very brief search for a roughly appropriate number of trees per landscape cell. Sagar & Singh 2006 ( https://www.jstor.org/stable/44521915) report a range of stem densities for several tropical forest sites which range from 35 - 419 stems per ha. Let's assume for now that a landscape cell is 1 ha.  I will go for a lower density value for now, otherwise simulations will take ages.  When running the more "final" models later on I will do a more thorough search for an appropriate tree density.  I will use 50 trees per cell here

# I am keeping the landscape small to reduce the run time, as each cell has 50 trees


ten_rep_1 <- gmse_replicates(
  time_max = 40,
  land_dim_1 = 50,
  land_dim_2 = 50,
  res_movement = 0, # trees don't move 
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
  stakeholders = 5, # number of stakeholders (no default)
  land_ownership = FALSE, # no land ownership
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE, # users act independently
)