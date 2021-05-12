require(GMSE)

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
  minimum_cost = 50, 
  user_budget = 10000, 
  manager_budget = 200, 
  usr_budget_rng = 20, # introduce variation around the mean user budget (removes step pattern) 
  manage_target = 2000000, 
  RESOURCE_ini = 2000000, 
  culling = TRUE, 
  tend_crops = TRUE, 
  stakeholders = 20, 
  land_ownership = TRUE, 
  public_land = 0.4, 
  manage_freq = 1, 
  group_think = FALSE
)

N1a_summary <- as.data.frame(gmse_table(N1a))