#' ---
#' title: Budget calculations
#' author: Matt Nuttall
#' date: 27/05/21
#' output:
#'    word_document:
#'      toc: false
#'      highlight: zenburn
#' ---


#+ include=FALSE
library(tidyverse)
library(patchwork)

#' This is a document to explore the budgets for all of the scenarios in the investment section of this chapter. As disucssed previously, in order for all of the scenarios to be comparable, the total budgets for users and the manager need to be equivalent across all scenarios, or at least capped at the top end. This is to prevent the problem of one scenario having "better" or "worse" outcomes simply because the manager/users had access to a larger total budget across the 50 time steps. 
#' 
#' In the first null scenario (N1) where the manager and user both had equal, constant budgets, I set the budgets at 200. Therefore the total available budget for each of the actors was 10,000 (50 time steps). I used this value to try to set the total available budget for all other scenarios. In the N2 null scenarios this was straightforward, as there were no increasing budgets - only constant and decreasing. Therefore the constant budget was 200, and the decreasing budget started at 200 and declined. 
#' 
#' This gets more complicated when we are talking about increasing budgets. For example, in the N3 null scenarios (optimistic null) I am assuming that the manager budget increases over time, with various different scenarios for the user budget (e.g. constant, increasing at the same rate as the manager budget, increasing at a lower rate than the manager). 
#' 
#' To calculate a linear increase (slope) that had a cumulative sum of 10,000 I used the following calculations:
#' 
#' `10000 = x + 2x + 3x + 4x +....50x`
#' Where `10000` is the maximum cumulative sum
#' 
#' Use Gauss equation to simplify the above: `sum of 1 to n = n(n+1)/2`
#' 
#' Therefore `sum of 1 to n = 50*(50+1)/2 = 1275`
#' 
#' Therefore `10000 = 1275x`
#' 
#' So `x = 10000/1275 = 7.843137`
#' 
#' So to produce a linear slope that over 50 time steps sums to 10,000 the increments need to be `7.843137`. This results in the slope in the below two plots:
#' 
#+ echo=FALSE, results=TRUE, fig.width=20, fig.height=10

val <- 10000/1275
df1 <- data.frame(Time = rep(c(1:50), times=2))
Manager_budget <- df1$Time[1:50]*val
User_budget <- rep(200, times=50)
budgets <- c(Manager_budget, User_budget)
df1$Budget <- budgets 
df1$Actor <- rep(c("Manager", "User"), each=50)

p.1 <- ggplot(df1[df1$Actor=="Manager",], aes(x=Time, y=Budget))+
        geom_line(size=2)+
        theme_classic()+
        theme(axis.title = element_text(size=30),
              axis.text = element_text(size=30),
              legend.text = element_text(size=30),
              legend.title = element_text(size=30))

p.2 <- ggplot(df1, aes(x=Time, y=Budget, group=Actor))+
        geom_line(size=2, aes(linetype=Actor))+
        theme_classic()+
        theme(axis.title = element_text(size=30),
              axis.text = element_text(size=30),
              legend.text = element_text(size=30),
              legend.title = element_text(size=30))

p.1 + p.2


#' The issue of course is that the budget starts from 0, and so if the user budget is constant in the scenario above, then the manager budget will be below the user budget for half of the scenario (see right plot), which is not what I'm aiming for. Essentially I can't have the user budget set at 200 if I want the manager budget to start at the same point, to increase, and to not go beyond 10,000.
#' 
#' One solution is to move the starting point to 100. The user can then have a constant budget of 100, and the manager budget can start at 100 and increase without having a cumulative sum greater than 10,000. I did this by simply replacing the `10000` in the above calculations with `5000`. For example:
#' 
#+ echo=FALSE, results=TRUE, fig.width=20, fig.height=10

val <- 5000/1275
df2 <- data.frame(Time = rep(c(1:50), times=2))

# create manager budget
Manager_budget <- vector(length = 50)
x <- 100
for(i in 1:length(Manager_budget)){
 y <- x+val
 Manager_budget[i] <- y
 x <- y
} 
Manager_budget <- Manager_budget[1:49]
Manager_budget <- append(Manager_budget,100,0)


User_budget <- rep(100, times=50)
budgets <- c(Manager_budget, User_budget)
df2$Budget <- budgets 
df2$Actor <- rep(c("Manager", "User"), each=50)

p.3 <- ggplot(df2, aes(x=Time, y=Budget, group=Actor))+
        geom_line(size=2, aes(linetype=Actor))+
        theme_classic()+
        theme(axis.title = element_text(size=30),
              axis.text = element_text(size=30),
              legend.text = element_text(size=30),
              legend.title = element_text(size=30))+
        ylim(0,300)

p.3

#' In the above plot the cumulative budget for the manager is 9803.9. My maths is not good enough to work out how to get it exactly at 10,000! 
#' 
#' The question I have is that if the starting budgets, and the budgets that are held constant (in the example above, this is the user budget), are different between scenarios, is this a problem? So for example is it ok that in the N1 and N2 scenarios the constant budgets were set at 200, and the declining budgets started at 200.  Whereas in the above plot, they are at 100. I believe Brad mentioned once that the absolute values of the budgets are not important, it is the relative budgets between the manager and the users. 
#' 
#' Another question is if we want the budgets to be capped at a certain value, and the total amount of budget in each scenario to be equal, then how do we have different slopes for the increasing budgets?
#' 
#' For example, the below plot shows different slopes for the manager budget increase - the top line (steepest slope) is as above (total cumulative budget = 9803.9), and the middle line (bottom manager line) has a total cumulative budget of 7882.4.
#' 
#+ echo=FALSE, results=TRUE, fig.width=20, fig.height=10

val <- 3000/1275
df3 <- data.frame(Time = rep(c(1:50), times=2))

# create manager budget
Manager_budget <- vector(length = 50)
x <- 100
for(i in 1:length(Manager_budget)){
  y <- x+val
  Manager_budget[i] <- y
  x <- y
} 
Manager_budget <- Manager_budget[1:49]
Manager_budget <- append(Manager_budget,100,0)


User_budget <- rep(100, times=50)
budgets <- c(Manager_budget, User_budget)
df3$Budget <- budgets 
df3$Actor <- rep(c("Manager", "User"), each=50)


p.3 <- p.3 + geom_line(data=df3[df3$Actor=="Manager",], aes(x=Time, y=Budget), size=2)

p.3

#' This is not a problem if the only concern is the maximum available budget (i.e. an upper cap), rather than trying to make the total available budget equal in all scenarios. My thinking at the moment is that for the null scenarios it is not really plausible to have equal available budget across all scenarios, otherwise we are unable to do different variations of null scenarios like in the plot above. I think that there should be a maximum available budget that applies across ALL scenarios (null scenarios and the final scenarios where we actually test our hypothesis). I then think that for the final scenarios where we are testing our hypothesis (what is the best strategy for investing management funds) we can have a total available budget that is the same across the different scenarios.   
#' 
#' To sum up: 
#' 
#' * I propose that there is a 10,000 (or some value) budget cap on all scenarios
#' 
#' * If the differences in **absolute budget** values between scenarios are not important, then we can produce steeper/gentler slopes for increasing budgets by shifting the starting value (and the value of the constant budget, if there is one) down/up. So for example shifting the starting value down should allow for a steeper slope that does not violate the total cumulative budget cap, but will allow for greater relative distance between the static budget and the increasing budget. 
#' 
#' * The final (i.e. non-null) scenarios should have equal total available budgets, because the question is "given increasing pressure, and given a finite budget, what is the best investement strategy?". 
#' 
#' * If all of the above is OK, I think increasing the values of the starting budgets and maximum available budget will be useful, just to give us more wiggle room to manouvre. So instead of the starting values being 200, we should make them, say, 1000. This would give a maximum available budget of 50,000. I woudld need to re-run all the null scenarios with the new values, but that's fine.
#' 
#'     
#' 
#' # UPDATE 07/07/21
#' 
#' Brad's advice is to not think about manager and user budgets being at all equvialent - he suggests not even putting them on the same y axis.They are not directly comparable and are not even always proportional. Brad also said that whether or not I want to set a total available budget (for either user or manager) depends on the question. For example,if you simply set a starting value for the user budget, and then 1) increase, 2) decrease, 3) keep constant, you are asking what happens when the absolute user budget (i.e. their power to affect the system) changes over time. Conversely, if you had different starting values for the three scenarios above, but the area under the curve (i.e. total available budget) was equal, then you are asking what happens when the same amount of user power is allocated unequally over time. 
#' 
#' Therefore, because my user budget is representing increases in human population, I DO want the absoulte power of users to change over time. Therefore when I have user budgets decreasing, remaining stable, and increasing (e.g. in some of the Null scenarios), then I think the starting values should be the same. This also means that in the final simulations (testing hypothesis), if I want user budgets to increase at different rates in different simulations, I want them to be from the same starting points so that the absolute power is increasing by different amounts. 
#' 
#' However, as Brad also suggests, I do need to somehow standardise budgets across scenarios. In other words, standardise all user budgets, and then standardise all manager budgets (rather than standardising them against each other). 
#' 
#' The main question I have at the moment is about the manager budgets. If I ensure the areas under the curves are all the same then I am asking: "given the same total budget over the time period, what is the best strategy for investment?". If I do not set a standard area under the surve, then the question becomes more about fundraising and overall project finances. The issue is how do I set the budgets such that the answer is not simply "the larger the total budget over the time period, the better". 
#' 
#' Currently I think I need to:
#'  
#' * Have the same starting point (intercept) for all user budgets. This is because the user budget is representing human population, and I want all of the scenarios to begin with the same population.
#' *       
