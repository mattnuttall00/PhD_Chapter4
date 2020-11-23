#' ---
#' title: GMSE ongoing analysis
#' author: Matt Nuttall
#' date: 23/11/20
#' output:
#'    word_document:
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
ten_rep_6_summary <- read.csv("outputs/Land_tenure/ten_rep_6/ten_rep_6_summary.csv")
ten_rep_7_summary <- read.csv("outputs/Land_tenure/ten_rep_7/ten_rep_7_summary.csv")
ten_rep_8_summary <- read.csv("outputs/Land_tenure/ten_rep_8/ten_rep_8_summary.csv")
ten_rep_9_summary <- read.csv("outputs/Land_tenure/ten_rep_9/ten_rep_9_summary.csv")
ten_rep_10_summary <- read.csv("outputs/Land_tenure/ten_rep_10/ten_rep_10_summary.csv")
ten_rep_11_summary <- read.csv("outputs/Land_tenure/ten_rep_11/ten_rep_11_summary.csv")
ten_rep_12_summary <- read.csv("outputs/Land_tenure/ten_rep_12/ten_rep_12_summary.csv")
ten_rep_13_summary <- read.csv("outputs/Land_tenure/ten_rep_13/ten_rep_13_summary.csv")
ten_rep_14_summary <- read.csv("outputs/Land_tenure/ten_rep_14_15_16/ten_rep_14_summary.csv")
ten_rep_15_summary <- read.csv("outputs/Land_tenure/ten_rep_14_15_16/ten_rep_15_summary.csv")
ten_rep_16_summary <- read.csv("outputs/Land_tenure/ten_rep_14_15_16/ten_rep_16_summary.csv")

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
#' Here I have set res_consume = 0.  I wanted to start with the presence of trees on a cell not impacting yield at all, i.e. to remove any incentive for the users to clear forest. Although Brad has since confirmed that when land_ownership = FALSE then the users are essentially hunters and will try to cull as much as possible regardless of the effect of trees on yield.
#' 
#' Model set up:
#' 
#+ ten_rep_0, eval=FALSE

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

#+ ten_rep_0_plot, eval=FALSE, echo=FALSE
plot_gmse_results(sim_results = ten_rep_0)

#' This is not what I was initially expecting. I had assumed that if there was no impact on yield, then users would have no incentive to cull trees. But thanks to Brad's explanation, this is now clear. 
#' 
#' ## Simulations 1 and 2 (ten_rep_1, ten_rep_2)
#'  
#' In these simulations I have changed res_consume to 0.02. So for now I am saying each tree reduces cell yield by 2%. This means that if there are 50 trees on a cell, then yield is reduced to 0.36% of the total (vaguely plausible for an open forest e.g. deciduous diptercarp landscape).  Cutting down 10 trees (20% of the trees) increases yield to 0.44, cutting down 20 trees (40%) increases yield to 0.54% etc. This is assuming I have correctly understood the exponential function Brad sent: yield = (1 - %yield reduction per tree)^remaining trees 
#' 
#' First I will run a single call of gmse() with 40 time steps. Then I will use gmse_replicate() to run the simulation 10 times to see the variation in outcomes.
#' 
#' ### Single call
#' 
#' Model set up:
#' 
#+ ten_rep_1_single, eval=FALSE, echo=TRUE

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

#+ ten_rep_1_plot, echo=FALSE, eval=FALSE
plot_gmse_results(ten_rep_1)

#' Here we see what I was expecting. The manager uses what little power they have to prevent culling, but is not very effective. Users want to reduce the trees in the cells as they are reducing yield. As trees are culled, the resource population declines and the users yield increases. 
#' 
#' The number of trees each user is harvesting per time step after the costs have been increased is 50. In reality probably a bit low as a family can clear more than that in a year (if we are assuming a time step is a year). But fine for now.
#' 
#' ### Replicate calls
#' 
#' Here I repeat the above call 10 times and extract summary data.  The model set up is the same as above, just using gmse_replicates()
#' 
#+ ten_rep_2_table, echo=FALSE, include=TRUE
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
#+ ten_rep_3_single, echo=FALSE, eval=FALSE
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

 
#+ ten_rep_3_plot, echo=FALSE, eval=FALSE
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
  

#' I was not clear what was behind this variation. I thought perhaps it was driven by the fact that, by chance, some users will find themselves on a cell with more resources than othes. Brad has since clarified that this is proabbly due to the genetic algorithm not finding the optimal harvesting solution right away (or the users taking a time step to "learn").  
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
            ylab("Resources lost per time step")

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
#' The plot on the top right I was not quite expecting. There appear to be budget/cost thresholds for increases in culling.  This can be seen in the "steps".  For a given manager budget/cull cost, there will be a certain number of culling actions. The number of culling actions will stay the same for a number of time steps, even when the manager budget (and therefore the cost) increases. Then there will be a drop to another level of culling actions. Interestingly, when manager budgets/costs are relatively low, the count of culling actions decrease more quickly in response to increases in cost. Whereas when costs are relatively high, the number of culling actions stay at the same level for longer, despite increases in cost. In other words, the manager's budget appears to have a larger relative impact when it is lower. I am not sure why this is. 
#' 
#' The bottom plot shows how the number of trees lost per time step decreases as manager budgets increase. In other words, as the manager gains power to enact policy, the fewer trees are lost. I guess the stochasticity that can be seen is a result of the changes in the natural die off of trees (otherwise I would expect to see steps, as in the top right plot).  The large decrease between time step 1 and 2 will be presumably because the users will cull as much as possible in the beginning before the manager increases the cost in the next time step.
#' 
#' I have clearly not yet found the value of manager budget that is high enough to completely eliminate culling all together. I had assumed that this would be when the manager budget went above the user budget, but this is not the case. I am not yet clear on how the manager budget relates to the cost of actions. 
#' 
#' ## Simulations 6 & 7 (ten_rep_6, ten_rep_7)
#' 
#' These simulations are exactly the same as the one above, but here I have increased the manager budgets by 50 and 100 in each time step rather than 20.  I am trying to see where the threshold is for the manager to have a signficant impact in reducing culling.
#' 
#+ ten_rep_6 & 7 cost plots, echo=FALSE

costplot6 <- ggplot(ten_rep_6_summary, aes(x=Manager_budget, y=Cull_cost))+
            geom_line()+
            theme(panel.background = element_blank())+
            theme(axis.line = element_line(colour = "black"))+
            ylab("Cost of culling")+
            ggtitle("ten_rep_6 (+50)")

costplot7 <- ggplot(ten_rep_7_summary, aes(x=Manager_budget, y=Cull_cost))+
              geom_line()+
              theme(panel.background = element_blank())+
              theme(axis.line = element_line(colour = "black"))+
              ylab("Cost of culling")+
              ggtitle("ten_rep_7 (+100)")

costplot6 + costplot7

#' The above plots show that the relationship between the manager's budget and the cost of culling is still linear regardless of the size of the budget increases, but when the manager has more budget the increases are larger (note the y axis scales).
#' 
#+ ten_rep_6 & 7 count plots, echo=FALSE

countplot6 <- ggplot(ten_rep_6_summary, aes(x=Manager_budget, y=Cull_count))+
              geom_line()+
              theme(panel.background = element_blank())+
              theme(axis.line = element_line(colour = "black"))+
              ylab("Count of cull actions")+
              ggtitle("ten_rep_6 (+50)")

countplot7 <- ggplot(ten_rep_7_summary, aes(x=Manager_budget, y=Cull_count))+
              geom_line()+
              theme(panel.background = element_blank())+
              theme(axis.line = element_line(colour = "black"))+
              ylab("Count of cull actions")+
              ggtitle("ten_rep_7 (+100)")

countplot6 + countplot7

#' In the above plots we see the same steps (pause in the decline of the cull counts) as we did for ten_rep_5, but they are different shapes. When the manager's budget is increased by larger increments (right hand plot), the steps  don't appear until the manager's budget is higher, when compared to the smaller increments simulation (left plot).  I guess this is something to do with the smaller incremental increase in the manager's budget having less of an impact on the ability of the users to cull, and so between any given two time steps, the users may still be able to take the same actions. Whereas when the manager's budget increase is larger, it is more likely to impact on the users ability to take action from one time step to the next. I am not sure why the steps get larger as the manager's budget gets larger though.  
#' Interestingly, I think that ten_rep_7 has identified the manager budget required to force the minimum number of posible culls.  The cull count reaches 100 when the manager's budget is 3300, and then remaines at 100 until the end of the simulation when the manager's budget is 4400. I guess there is a possibility that it is just another arger step, and that the cull count might continue to fall if the simulation continued.  However, a loss of 100 trees per year equates to 0.08% of the population which is pretty good in a Cambodian PA! 
#' 
#+ ten_rep_5, 6 & 7 lost plots, echo=FALSE, fig.width=9, fig.height=5       

ten_rep_5_summary$label <- "+20 / time step"
ten_rep_6_summary$label <- "+50 / time step"
ten_rep_7_summary$label <- "+100 / time step"

comp_5_6_7 <- rbind(ten_rep_5_summary, ten_rep_6_summary, ten_rep_7_summary)

comp_5_6_7$label <- as.factor(comp_5_6_7$label)
comp_5_6_7$label <- factor(comp_5_6_7$label, levels = c("+100 / time step","+50 / time step","+20 / time step"))

lostplot_comp <- ggplot(comp_5_6_7, aes(x=Manager_budget, y=Pop_diff, group=label, colour=label))+
                  geom_line(size=1)+
                  theme(panel.background = element_blank())+
                  theme(axis.line = element_line(colour = "black"))+
                  ylim(0,2300)+
                  ylab("Resources lost per time step")

lostplot_comp_time <- ggplot(comp_5_6_7, aes(x=Time, y=Pop_diff, group=label, colour=label))+
                      geom_line(size=1)+
                      theme(panel.background = element_blank())+
                      theme(axis.line = element_line(colour = "black"))+
                      ylim(0,2300)+
                      ylab("Resources lost per time step")

lostplot_comp + lostplot_comp_time + plot_layout(ncol=1)


#' In the above plots I have plotted the number of trees lost per time step against manager budget (left) and time (right) for the three simulations with dynamic manager budgets.  All three simulations have very different numbers lost in the first time step, but this is likely due to the variation in number of actions taken in time step 1 resulting from the genetic algorithm still revving up (as Brad explained). The plot on the left shows that the numbers of resources lost per time step decreases as manager budgets increase, which is what I expected. We can see that the blue and green lines stop before they flatten, suggesting that the optimal manager budget for reducing the number of trees lost has not been reached yet. Whereas the pink line looks as though it has flattened out (still with some small fluctuations) suggesting that further manager budget increases will likely have negligible impacts on culling.  The plot on the right shows the same thing (the slopes of the lines at time 40 show the green and blue still decreasing, and the pink flattened out), and also that, as I would have expected, the higher the manager budget, the fewer trees get cut down. 
#' 
#+ ten_rep_7 budget proportion plot, echo=FALSE

ten_rep_7_summary$budget_prop <- (ten_rep_7_summary$Manager_budget/1000)*100

ggplot(ten_rep_7_summary, aes(x=budget_prop, y=Pop_diff))+
geom_line(size=1)+
theme(panel.background = element_blank())+
theme(axis.line = element_line(colour = "black"))+
ylim(0,2300)+
ylab("Resources lost per time step")+
xlab("Manager budget as % of user budget")
          

#' The above plot shows the number of trees lost against the manager's budget as a percentage of the users' budget, and we can see that in this scenario, where users do not own land and are therefore trying their hardest to cull, when the the manager's budget reaches ~300% of the users' budget, the gains in reducing culling essentially disappear i.e. increasing the manager's budget further make little different to the number of trees lost.    
#' 
#' ## Simulation 8 (ten_rep_8)
#' 
#' Here I test the inclusion of a range of 100 user budget units around the mean of 1000 to see what difference it makes to the steps we saw in the plot above.
#' 
#+ ten_rep_8_plots, eval=TRUE, echo=FALSE

ggplot(ten_rep_8_summary, aes(x=Manager_budget, y=Cull_count))+
  geom_line(size=1)+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  ylim(0,1000)+
  ylab("Count of cull actions")+
  xlab("Manager budget")
 
#' We can see that by adding some variation around the user budgets, we smooth out the steps. This is because for any given manager budget there will be some users who can cull more, and some users who can cull less (due to user budget variation), and so there is no absolute number of possible culls for a given manager budget.    
#' 
#' ## Simulations 9, 10, 11 
#' 
#' Here I have done three simple simulations. I have kept the manager's budget fixed at 1000, and then run a simulation where the user budget is 10% of the manager's, a simulation where the user and manager budget is the same, and then a simulation where the manager's budget is 10% of the users'.
#' 
#+ plot resources ten_reps 9:11, eval=TRUE, echo=FALSE

ten_rep_9_summary$sim <- "User budget 10% of Manager"
ten_rep_10_summary$sim <- "Equal budget"
ten_rep_11_summary$sim <- "Manager budget 10% of User"
ten_rep_9_10_11 <- rbind(ten_rep_9_summary,ten_rep_10_summary,ten_rep_11_summary)

ggplot(ten_rep_9_10_11, aes(x=time_step, y=resources, group=sim, colour=sim))+
  geom_line(size=1)+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  ylab("Resources")+
  xlab("Time step")

#' The above plot shows that when the users' budget is only 10% of the manager's, then the manager is able to keep tree loss quite low.  When the user and manager budgets are equal, there is still a fair amount of tree loss (although you could argue that having 82% of your forest left after 40 years is actually quite the conservation win, especially in Cambodia!).  When the manager's budget is only 10% of the users' budget then the resource population goes extinct before the end of the time period. 
#' 
#+ plot cost_culling ten_rep_9:11, eval=TRUE, echo=FALSE

ggplot(ten_rep_9_10_11, aes(x=sim, y=cost_culling))+
  geom_boxplot()+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  xlab("Simulation")+
  ylab("Cost of culling")

#' The above plot shows that when the user budget is the same, or much higher, than the manager's budget, the cost of culling does not vary as the manager is using all their budget in each time step to prevent culling. But when the user budget is well below the manager's (10%), then the manager is more dynamic with the costs. This is because the manager has the budget to almost completely stop culling in a time step, which then allows them to lower the cost of culling for the next time step. This encourages more culling, which prompts another increase in cost and so on and so forth. 
#' 
#' ## Dynamic user budgets (ten_rep_12 & 13)
#' 
#' In the next two simulations I have used gmse_apply() to increase user budgets throughout the time period.  This is important because this is potentially how I am going to try and simulate increases in human population density in the villages over time.  
#' 
#' I have started with two simulations, one with a static manager budget of 1000 and one with a static manager budget of 2000.  In both simulations the users' budgets start below the manager's budget, but overtake it at some point during the simulation. 
#' 
#+ ten_rep_12 & 13 plot, eval=TRUE, echo=FALSE

# temp df to force manager budget line all the way down the plot
mb_df <- data.frame(Manager_budget = rep(2000, times=40),
                    Pop_size = seq(125000, 85000, length.out = 40))

ggplot()+
  geom_line(data=ten_rep_13_summary,aes(x=User_budget, y=Pop_size), colour="red", size=1)+
  geom_line(data=mb_df,aes(Manager_budget, y=Pop_size),linetype="dashed",colour="red", size=1)+
  geom_line(data=ten_rep_12_summary,aes(x=User_budget, y=Pop_size),colour="blue", size=1)+
  geom_line(data=ten_rep_12_summary,aes(Manager_budget, y=Pop_size),colour="blue",
            linetype="dashed", size=1)+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  xlab("User budget")+
  ylab("Resource population")

#' In the above plot the vertical dashed lines show the static manager budgets.  We can see the interaction effect of the manager budget i.e. when the manager budget is lower, the user budget has a larger negative effect on the resource population, whereas when the manager budget is higher, the effect of user budget on the resource population is weaker.  
#' 
#'  I think these simulations show how user budget could be used as a proxy for population density. If we assume that increases in population density means more pressure on the land via a stronger drive for communities to increase their farmland, and more resources (both physical resources and financial resources) to clear forest, then increasing user budgets seems like it would work.  
#'  
#' It would be useful to be able to run multiple gmse_apply() calls in one go, each with a varying range of manager and user budgets.  I will attempt to write a function that does this further down the line, once I am closer to the final land tenure set up.
#' 
#' ## Dynamic user and manager budgets (ten_rep_14,15,16)
#' 
#' In the next three simulations I have sued gmse_apply() to run simulations where both the user and the manager busgets vary over time. I have constructed three scenarios which could be plausible situations in a conservation landscape.
#' 
#' ten_rep_14 - in this simulation I have made the manager and user budgets increase at the same rate. This is almost a null scenario, but also could represent a conservation landscape where human population density (or resources available to the human population) is increasing, but so is the investment / resources of the manager or authority, and that they are increasing in a relatively equal fashion.
#' 
#' ten_rep_15 - in this simulation the manager starts with a much higher budget than the users, representing a PA manager / authority that has significantly more power/resources than the communities. The manager's budget increases slowly (representing small but steady increase in investment), but the users' budget increases quickly, representing rapid inreases in population density or community wealth/resources. 
#' 
#' ten_rep_16 - in this simulation the manager starts with a higher budget as above, but the manager's budget slowly decreases representing falling investment or loss of funds over time. The users' budget increases, representing increases in population density or community wealth/resources.
#' 
#+ ten_rep_14_15_16 plots, eval=TRUE, echo=FALSE, fig.width=8, fig.height=6

# merge the summary files
ten_rep_14_summary$sim <- "ten_rep_14"
ten_rep_15_summary$sim <- "ten_rep_15"
ten_rep_16_summary$sim <- "ten_rep_16"
ten_rep_14_16 <- rbind(ten_rep_14_summary,ten_rep_15_summary,ten_rep_16_summary)

# set factor plotting order
ten_rep_14_16$sim <- as.factor(ten_rep_14_16$sim)
ten_rep_14_16$sim <- factor(ten_rep_14_16$sim, levels=c("ten_rep_14","ten_rep_15","ten_rep_16"))

mbub_plot <- ggplot(ten_rep_14_16, aes(x=User_budget, y=Manager_budget, group=sim, colour=sim))+
            geom_line(size=1)+
            theme(panel.background = element_blank())+
            theme(axis.line = element_line(colour = "black"))+
            xlab("User budget")+
            ylab("Manager budget")

res_plot <- ggplot(ten_rep_14_16, aes(x=Time, y=Pop_size, group=sim, colour=sim))+
            geom_line(size=1)+
            theme(panel.background = element_blank())+
            theme(axis.line = element_line(colour = "black"))+
            xlab("Time step")+
            ylab("Resource population")

cullcost_plot <- ggplot(ten_rep_14_16, aes(x=Time, y=Cull_cost, group=sim, colour=sim))+
                  geom_line(size=1)+
                  theme(panel.background = element_blank())+
                  theme(axis.line = element_line(colour = "black"))+
                  xlab("Time step")+
                  ylab("Cost of culling")

cullcnt_plot <- ggplot(ten_rep_14_16, aes(x=Time, y=Cull_count, group=sim, colour=sim))+
                geom_line(size=1)+
                theme(panel.background = element_blank())+
                theme(axis.line = element_line(colour = "black"))+
                xlab("Time step")+
                ylab("Count of cull actions")

mbub_plot + res_plot + cullcost_plot + cullcnt_plot + plot_layout(ncol=2)

#' In the top left plot we see the relationship between the user budget and the manager budget for the three simulations. These all look how I would expect. In the top right plot we see that the forest gets reduced in all of the simulations, but less so in the last simulation (16). This is interesting because it is the only simulation where the manager's budget is decreasing. But the manager's budget never decreases below the users' budget, and in the other scenarios the user budget is closer (or exceeds) the manager's budget in more time steps. This suggests that the manager needs a budget that is consistently quite a lot higher than the users' budget to maintain decent forest cover. This is what I discovered in the sections above when I was varying the manager budget too.  
#' In the bottom left plot we see what I would expect - when the manager's budget increases, the cost of culling increases too, to meet the demands of the increasing user budgets and trying to maintain as many trees as possible. In the final simulation, the managers budget is decreasing and so the cost of culling decreases. 
#' In the bottom right plot, we see that in the first simulation the cull count reaches a plateau after 9 time steps. I guess the plateau makes sense - becuase the user and manager budgets are increasing in line with each other, neither user nor manager ever has an advantage over the other relative to the previous time step, and so a sort of equalibrium of the number of cull actions is found.  I am not sure though why it takes 9 time steps to reach that point. I am also not sure why there is a mini plateau between time steps 4 and 8. In the other two scenarios the cull counts are continuously increasing, which makes sense because in both of those scenarios the gap between the user budget and the manager budget is continually closing, and therefore the users have more and more power to cull in each time step.
#' We see the step pattern has returned again, which I wasn't expecting as I had set the usr_budget_rng to be 10% of whatever the user budget was in each time step and scenario. Perhaps 10% isn't enough to remove that pattern in these cases?
#' 
#+ ten_rep_14,15,16 proportion, echo=FALSE, eval=TRUE, fig.width=8, fig.height=6

# create proportion column
ten_rep_14_16$prop <- ten_rep_14_16$User_budget/ten_rep_14_16$Manager_budget 

prop1 <- ggplot(ten_rep_14_16, aes(x=prop, y=Cull_count, group=sim, colour=sim))+
          geom_line(size=1)+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black"))+
          xlab("User budget as proportion of manager budget")+
          ylab("Count of cull actions")

prop2 <- ggplot(ten_rep_14_16, aes(x=prop, y=Pop_size, group=sim, colour=sim))+
          geom_line(size=1)+
          theme(panel.background = element_blank())+
          theme(axis.line = element_line(colour = "black"))+
          xlab("User budget as proportion of manager budget")+
          ylab("Resource population")

prop1 + prop2

#' The above plot on the left show the increase in culling actions as the users' budget gets closer to the manager's budget.  We can see that culling actions are always high when the users' budget is close to the manager's budget. The plot on the right shows that regardless of the proportion of the users budget, it is better for the manager to have a higher budget in absolute terms.  I was initially expecting the lines to be the same, because if the proportions between user and manager budget are the same then the impact on resources should be the same. But I guess that absolute budgets are higher, then more trees can be culled (user) or protected (manager). This means that in order to set up a realistic scenario, I'll need to test a range of budget ranges to find ones that represent realistic numbers of trees being lost.   