#' ---
#' title: GMSE ongoing analysis
#' author: Matt Nuttall
#' date: 30/10/20
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

#+ include=FALSE
library(GMSE)
library(tidyverse)

# load data
ten_rep_0_summary <- read.csv("outputs/Land_tenure/ten_rep_0/ten_rep_0_summary.csv")
ten_rep_1_summary <- read.csv("outputs/Land_tenure/ten_rep_1/ten_rep_1_summary.csv")
ten_rep_2_summary <- read.csv("outputs/Land_tenure/ten_rep_2/ten_rep_2_summary.csv")
ten_rep_3_summary <- read.csv("outputs/Land_tenure/ten_rep_3/ten_rep_3_summary.csv")
ten_rep_4_summary <- read.csv("outputs/Land_tenure/ten_rep_4/ten_rep_4_summary.csv")
ten_rep_5_summary <- read.csv("outputs/Land_tenure/ten_rep_0/ten_rep_5_summary.csv")

#' This is a summary of my ongoing GMSE analysis which will investigate the social-ecological dynamics surrounding land tenure in a hypothetical conservation landscape that is loosely based on a Cambodian protected area.
#' 
#' As discussed, I have started simple and will build complexity as I go.
#' 
#' I have started with a landscape as follows:
#' 
#' * The landscape dimensions are 50 x 50. I am assuming that a cell represents 1 ha, and therefore this landscape is currently 2500 ha / 25km^2.  I will increase the size of the landscape later, but I am keeping it small at the moment to reduce run time. 
#' * There are 50 individual users.  This can represent a community of 50 families. Currently users act independently.
#' * Land ownership is currently FALSE
#' * Users can move around the landscape
#' * I have found papers that report tree density in tropical forest landscapes, and they vary widely. One paper (Sagar & Singh 2006 https://www.jstor.org/stable/44521915) report a range between 35 and 419 stems per ha.  To keep run time low I have started with 50 trees/ha.  This is low but not implausible. This can be increased later.
#' * The manager has imperfect detection.
#' 
#' I have started with managers wanting to preserve all trees, but having very little power to set policy.  In later simulations I have started increasing manager budgets to see where the threshold for reducing clearance is.
#' 
#' ## Simulation 0 (ten_rep_0)
#' 
#' Here I have set res_consume = 0.  I wanted to start with the presence of trees on a cell not impacting yield at all, i.e. to remove any incentive for the users to clear forest.  
#' 
#' Model set up:
#' 
#+ ten_rep_0

ten_rep_0 <- gmse(
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
  res_consume = 0, # trees have no impact on yield
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = 500, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 30, # a village with 50 families
  land_ownership = FALSE, # no land ownership
  group_think = FALSE # users act independently
)

plot_gmse_results(sim_results = ten_rep_0)

#' This is not what I was expecting. I had assumed that if there was no impact on yield, then users would have no incentive to cull trees. Yet in this simulation they continue to do so.  