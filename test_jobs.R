
require(GMSE)

system.time(ten_rep_19 <- gmse(
  time_max = 40,
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
  res_consume = 0, # Trees have no impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  minimum_cost = 10, # minimum cost of any action in user & manager models - improves precision of manager policy(?)
  user_budget = 10000, # total budget of each stakeholder for performing actions
  manager_budget = 20000, # Manager has little power (50% of user)
  manage_target = 2000000, # target resource abundance (same as starting value)
  RESOURCE_ini = 2000000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = TRUE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 4, # a village with 50 families
  land_ownership = TRUE, # land ownership
  public_land = 0.6, # 60% of the land is PA, 40% is community land
  manage_freq = 1, # frequency of manager setting policy 
  group_think = FALSE # users act independently
))