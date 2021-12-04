source('framework/data.R')

###############################################################################
# Either source a2_strategy_template.R or strategy.R as appropriate
# Note that, unedited, a2_strategy_template.R will always trigger E01
source('strategies/a2_strategy_template.R')
#source('strategies/strategy.R') 
###############################################################################

test_getTMA <- function(case) { 
    # case should be one of: 'E01', 'E02', 'E03', 'E04', 'E05', 'E06', 'normal'

    dataList <- getData(directory="A2")
    prices_19_rows <- dataList[[1]]$Close[1:19]
    prices_20_rows <- dataList[[1]]$Close[1:20]
    prices_20_rows_renamed <- prices_20_rows
    colnames(prices_20_rows_renamed) <- 'Closed'
    bad_prices <- c(1,2,3) 
    lookbacks_no_names <- list(5,10,25) # list elements not named
    lookbacks_not_integer <- list(short=5,medium=as.integer(10),long=as.integer(20))
    lookbacks_wrong_order <- list(short=as.integer(15),medium=as.integer(10),long=as.integer(20))
    lookbacks <- list(short=as.integer(5),medium=as.integer(10),long=as.integer(20))

    if (case == 'E01') 
        getTMA(prices,lookbacks_no_names) 

    if (case == 'E02') 
        getTMA(prices,lookbacks_not_integer) 

    if (case == 'E03') 
        getTMA(prices,lookbacks_wrong_order) 

    if (case == 'E04') 
        getTMA(bad_prices,lookbacks) 

    if (case == 'E05') 
        getTMA(prices_19_rows,lookbacks) 

    if (case == 'E06') 
        getTMA(prices_20_rows_renamed,lookbacks) 

    if (case == 'normal') 
        getTMA(prices_20_rows,lookbacks)
}
