### This script is for the first part of my final PhD chapter. The analysis is investigating scenarios around investment of conservation funds over time, given increasing threats. Threats here represent increasing human population density, which is represented by increasing user budgets.

### Load libraries ####

library('tidyverse')
library('GMSE')
library('patchwork')


############## LANDSCAPE DETAILS ##################





############## NULL SCENARIOS #####################

# These scenarios will explore the basic manager/user dynamics and will potentially reflect the "null" scenarios, or the counterfactuals.

# I will do the following scenarios:

# N1 - Null - Manager and user budgets do not change, and are equal

# N2 - Optimistic Null - Manager and user budgets both increase linearly over time, at the same rate and from the same starting point

# N2a - Optimistic Null - Variation of N2. Manager and user budget increase linearly over time, but the manager budget rate of increase is lower than the user rate of increase

# N2b - optimistic Null - Variation of N2. Manager and user budget increase linearly over time, but the manager budget rate of increase is higher than the user rate of increase

# N3 - Pessimistic Null - Manager budget remains constant, but user budgets increase linearly 

#### N1 ####

# This null scenario has the manager and user budgets remaining static over the entire study period