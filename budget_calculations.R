#' ---
#' title: Budget calculations
#' author: Matt Nuttall
#' date: 27/05/21
#' output:
#'    word_document:
#'      toc: true
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
#' '10000 = x + 2x + 3x + 4x +....50x'
#' 
#' Gauss equation to simplify the above: 'sum of 1 to n = n(n+1)/2'
#' 
#' Therefore '50*(50+1)/2 = 1275'
#' 
#' Therefore '10000 = 1275x'
#' 
#' So 'x = 10000/1275 = 7.843137'
#' 
#' So to produce a linear slope that over 50 time steps sums to 10,000 the increments need to be 7.843137. This results in the slope below:
#' 
#+ echo=FALSE, results=TRUE

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
        theme(axis.title = element_text(size=15),
              axis.text = element_text(size=15))

p.2 <- ggplot(df1, aes(x=Time, y=Budget, group=Actor))+
        geom_line(size=2, aes(linetype=Actor))+
        theme_classic()+
        theme(axis.title = element_text(size=15),
              axis.text = element_text(size=15))

p.1 + p.2


#' The issue of course is that the budget starts from 0, and so if the user budget is constant in the scenario above, then the manager budget will be below the user budget for half of the scenario (see left plot), which is not what I'm aiming for. I can't have the user budget set at 200 if I want the manager budget to start at the same point, to increase, and to not go beyond 10,000.
#' 
#' One solution is to move the starting point to 100. The user can then have a constant budget of 100, and the manager budget can start at 100 and increase without having a cumulative sum greater than 10,000. For example:
#' 
#+ echo=FALSE, results=TRUE

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
        theme(axis.title = element_text(size=15),
              axis.text = element_text(size=15))+
        ylim(0,300)

p.3

#' In the above plot the cumulative budget for the manager is 9803.9. My maths is not good enough to work out how to get it exactly at 10,000! 
#' 
#' The question I have is that if the starting budgets, and the budgets that are held constant (in the example above, this is the user budget), are different between scenarios, is this a problem? So for example is it ok that in the N1 and N2 scenarios the constant budgets were set at 200, and the declining budgets started at 200.  Whereas in the above plot, they are at 100. I believe Brad mentioned once that the absolute values of the budgets are not important, it is the relative budgets between the manager and the users. 
