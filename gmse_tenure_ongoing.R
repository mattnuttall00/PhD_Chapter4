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
library(ggplot2)
library(knitr)
library(patchwork)

# load data
ten_rep_0_summary <- read.csv("outputs/Land_tenure/ten_rep_0/ten_rep_0_summary.csv")
ten_rep_1_summary <- read.csv("outputs/Land_tenure/ten_rep_1/ten_rep_1_summary.csv")
ten_rep_2_summary <- read.csv("outputs/Land_tenure/ten_rep_2/ten_rep_2_summary.csv")
ten_rep_3_summary <- read.csv("outputs/Land_tenure/ten_rep_3/ten_rep_3_summary.csv")
ten_rep_4_summary <- read.csv("outputs/Land_tenure/ten_rep_4/ten_rep_4_summary.csv")
ten_rep_5_summary <- read.csv("outputs/Land_tenure/ten_rep_5/ten_rep_5_summary.csv")

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
#' I have started with managers wanting to preserve all trees, but having very little power to set policy.  In later simulations I have increased the manager budget a bit, and then started increasing manager budgets to see where the threshold for reducing clearance is.
#' 
#' ## Simulation 0 (ten_rep_0)
#' 
#' Here I have set res_consume = 0.  I wanted to start with the presence of trees on a cell not impacting yield at all, i.e. to remove any incentive for the users to clear forest.  
#' 
#' Model set up:
#' 
#+ ten_rep_0, eval=TRUE, cache=TRUE

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

#+ ten_rep_0_plot, eval=TRUE, echo=FALSE
plot_gmse_results(sim_results = ten_rep_0)

#' This is not what I was expecting. I had assumed that if there was no impact on yield, then users would have no incentive to cull trees. Yet in this simulation they continue to do so. What are the other parameters/mechanisms that drive the incentives for users to cull?
#' 
#' ## Simulations 1 and 2 (ten_rep_1, ten_rep_2)
#'  
#' In these simulations I have changed res_consume to 0.02. So for now I am saying each tree reduces cell yield by 2%. This means that if all of the 50 trees on a cell are standing, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is assuming I have correctly understood the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees 
#' 
#' First I will run a singel call of gmse() with 40 time steps. Then I will use gmse_replicate() to run the simulation 10 times to see the variation in outcomes.
#' 
#' ### Single call
#' 
#' Model set up:
#' 
#+ ten_rep_1_single, eval=TRUE, echo=TRUE, cache=TRUE

ten_rep_1 <- gmse(
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
  res_consume = 0.02,
  
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
)

#+ ten_rep_1_plot, echo=FALSE, eval=TRUE
plot_gmse_results(ten_rep_1)

#' Here we see what I was expecting. The manager uses what little power they have to prevent culling, but is not very effective. Users want to reduce the trees in the cells as they are reducing yield. As trees are culled, the resource population declines and the users yield increases. 
#' 
#' The number of trees each user is harvesting per time step after the costs have been increased is 50. In reality probably a bit low as a family can clear more than that in a year (if we are assuming a time step is a year). But fine for now.
#' 
#' ### Replicate calls
#' 
#' Here I repeat the above call 10 times and extract summary data.  The model set up is the same as above, just using gmse_replicates()
#' 
#+ ten_rep_2_table
knitr::kable(ten_rep_2_summary)

#' The amount of forest remaining at the end of each simulation is very similar, and the costs and actions do not appear to vary. 
#' 
#' ## Simulations 3 & 4 (ten_rep_3, ten_rep_4)
#' 
#' Here the model set up is identical to the above, but I have increased the manager budget from 100 (10% of user budget) to 500 (50% of user budget)
#' 
#' ### Single call
#' 
#' First I ran a single gmse() call
#' 
#+ ten_rep_3_single, echo=FALSE, eval=TRUE, cache=TRUE
ten_rep_3 <- gmse(
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
)

 
#+ ten_rep_3_plot, echo=FALSE, eval=TRUE
plot_gmse_results(ten_rep_3)

#' Here we see the loss of trees is much less than before, as the manager has more power to reduce culling by setting higher costs. Users are still able to clear forest though, with 800 trees being lost each time step once the manager has applied their policy.  
#' 
#' ### Replicate calls
#' 
#+ ten_rep_4_plot_resources, echo=FALSE, eval=TRUE, warnings=FALSE
ten_rep_4_summary$sim <- as.factor(ten_rep_4_summary$sim)
ggplot(ten_rep_4_summary, aes(x=time_step, y=resources, group=sim, colour=sim))+
  geom_line()+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))

#' There is not much variation in the decline in resources.  I guess this is because the model currently has little flexibility - the manager will always set the highest cost possible as they are trying to maintain all resources, and the users will always try to cull. Furthermore the resources are not reproducing or naturally dying (much), and so there is not much stochasticity in the resource population.  Therefore after the inital setting of policy in time step 1, there will be little change between the simulations. 
#' 
#' We do however, see some variation in the initial number of culling actions (see below plot)
#'  
#+ ten_rep_4_plot_actions, echo=FALSE, eval=TRUE, warning=FALSE
ten_rep_4_summary$time_step <- as.numeric(ten_rep_4_summary$time_step)
ggplot(ten_rep_4_summary, aes(x=time_step, y=act_culling, group=sim, colour=sim))+
  geom_line()+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10), limits=c(0,10))+
  ylab("Number of culling actions")
  

#' I am not clear what is behind this variation. Is this driven by the fact that, by chance, some users will find themselves on a cell with more resources than othes?  If there are more resources on a cell, will the user try to cull more, and vice versa?
#' 
#' ## Simulation 5 (ten_rep_5)
#' 
#' Here I have used gmse_apply() to dynamically change the managers budget.  I have increased the manager budget incrementally over the 40 time steps, in quantities that mean the manager budget exceeds the user budget at some point during the time period.
#' 
#' I would like to see what effects increasing manager budgets have on the actions of the users, and on the resources. I would also like to see how large the manager's budget needs to be to completely stop all culling. 
#' 
#' Model set up:
#' 
#+ ten_rep_5, eval=FALSE, echo=TRUE

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
  res_consume = 0.02, 
  
  # all genetic algorithm parameters left to default
  
  move_agents = TRUE, # should agents move at the end of each time step?
  max_ages = 1000, # maximum ages of resources - set very high to reduce natural death
  user_budget = 1000, # total budget of each stakeholder for performing actions
  manager_budget = mb, # Manager has little power (50% of user)
  manage_target = 125000, # target resource abundance (same as starting value)
  RESOURCE_ini = 125000, # initial abundance of resources - 50 trees per cell
  culling = TRUE, # culling is only option
  tend_crops = FALSE, # is tending crops on landscape allowed. if TRUE, user can increase yield each time step
  stakeholders = 50, # a village with 50 families
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

#' Below are some exploratory plots.

#+ ten_rep_5_summary, echo=FALSE, eval=TRUE
costplot <- ggplot(ten_rep_5_summary, aes(x=Manager_budget, y=Cull_cost))+
            geom_line()+
            theme(panel.background = element_blank())+
            theme(axis.line = element_line(colour = "black"))+
            ylab("Cost of culling")

countplot <- ggplot(ten_rep_5_summary, aes(x=Manager_budget, y=Cull_count))+
             geom_line()+
             theme(panel.background = element_blank())+
             theme(axis.line = element_line(colour = "black"))+
             ylab("Count of cull actions")

lostplot <- ggplot(ten_rep_5_summary, aes(x=Manager_budget, y=Pop_diff))+
            geom_line()+
            theme(panel.background = element_blank())+
            theme(axis.line = element_line(colour = "black"))+
            ylim(0,1600)+
            ylab("Resources lost")

#ggplot(ten_rep_5_summary, aes(x=Cull_cost, y=Cull_count))+
  #geom_line()+
  #theme(panel.background = element_blank())+
  #theme(axis.line = element_line(colour = "black"))
  #ylim(0,1600)+
  #ylab("Resources lost")

#timeplot <- ggplot(ten_rep_5_summary, aes(x=Time))+
            #geom_line(aes(y=Pop_diff))+
            #geom_line(aes(y=Manager_budget))+
            #scale_y_continuous(name="Resources lost", 
                               #sec.axis = sec_axis(~.*1,name="Manager budget"))

(costplot+countplot) / lostplot

#' The top left plot shows that there is a linear relationship between the manager's budget and the cost of culling. In other words, because the manager is trying to stop ALL culling, as soon as they have more budget they will use it to increase the cost of culling.
#' 
#' The plot on the top right I was not quite expecting. There appear to be budget/cost thresholds for increases in culling.  This can be seen in the "steps".  For a given manager budget/cull cost, there will be a certain number of culling actions. The number of culling actions will stay the same for a number of time steps, even when the manager budget (and therefore the cost) increases. Then there will be a drop to another level of culling actions. Interestingly, when manager budgets/costs are relatively low, the count of culling actions decrease more quickly in response to increases in cost. Whereas when costs are high, the number of culling actions stay at the same level for longer, despite increases in cost. 
#' 
#' The bottom plot shows how the number of trees lost per time step decreases as manager budgets increase. In other words, as the manager gains power to enact policy, the fewer trees are lost. I guess the stochasticity that can be seen is a result of the changes in the natural die off of trees (otherwise I would expect to see steps, as in the top right plot).  
#' 
#' I have clearly not yet found the value of manager budget that is high enough to completely eliminate culling all together. I had assumed that this would be when the manager budget went above the user budget, but this is not the case. I am not yet clear on how the manager budget relates to the cost of actions. 
