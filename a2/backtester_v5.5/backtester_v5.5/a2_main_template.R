############################################################################
# This file is setup to use:
# - the data for assignment 2 (a2)
# - the a2_strategy_template
# You can use this file to do run a single parameter combination
# (for which you should set params, start_period and end_period as required)
# In particular you can use this file to test getOrders with the examples in a2.pdf
# For creating results.yaml, you can use main_optimize.R as a starting point
############################################################################
source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')

# Read in data -- here with the A2 direction; subset it as required
dataList <- getData(directory="A2")
# subset data: choose the period to run on 
#dataList <- lapply(dataList, function(x) x[1:200])

# Choose strategy -- this should be called strategy.R when you submit it
strategyFile <- 'strategies/a2_strategy_template.R'

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

# Strategy parameters -- this will not be an empty list when you are done
params <- list()

print("Parameters:")
print(params)

# Do backtest
results <- backtest(dataList,getOrders,params,sMult=0.2)
pfolioPnL <- plotResults(dataList,results)
