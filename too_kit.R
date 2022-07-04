library(xts)
library(zoo)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(RiskPortfolios)
library(covRobust)
library(quantmod)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(plotly)


compute_equal_weights <- function(R,type_assets,rebal){

    # Create a vector of equal weights
    equal_weights <- rep(1 / ncol(R), ncol(R))

    # Compute the benchmark returns
    r_benchmark <- Return.portfolio(R = R, weights = equal_weights, rebalance_on = rebal)

    if(is.na(rebal)){
        name_bench <- paste(type_assets,"NA", sep = "_")
    }else {
       name_bench <- paste(type_assets,rebal, sep = "_")
    }
    colnames(r_benchmark) <- name_bench

    return(r_benchmark)


}

visualizing_EF <- function(R, p, EF, n_permut=50000){

    meanReturns <- colMeans(R)
    covMat <- cov(R)

    randomport<- random_portfolios(p, permutations = n_permut, rp_method = "sample")

    minvar.port <- add.objective(p, type = "risk", name = "var")
 
    # Optimize
    minvar.opt <- optimize.portfolio(R, minvar.port, optimize_method = "random", 
                                    rp = randomport)
    
    # Generate maximum return portfolio
    maxret.port <- add.objective(p, type = "return", name = "mean")
    
    # Optimize
    maxret.opt <- optimize.portfolio(R, maxret.port, optimize_method = "random", 
                                    rp = randomport)

    minret <- min(meanReturns)
    maxret <- max(meanReturns)


    
    
    feasible.sd <- apply(randomport, 1, function(x){
        return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
        })

    feasible.means <- apply(randomport, 1, function(x){
        return(x %*% meanReturns)
        })
    
    feasible.sr <- feasible.means / feasible.sd


    eff.frontier <- data.frame("Risk" = as.vector(EF[,2]), "Return" = as.vector(EF[,1]))
    EF_plot <-  plot_ly() %>%

    add_trace(x = feasible.sd, y = feasible.means, color = feasible.sr, 
            mode = "markers", type = "scattergl", showlegend = F,
            
            marker = list(size = 3, opacity = 0.5, 
                        colorbar = list(title = "Sharpe Ratio"))) %>%
    
    add_trace(data = eff.frontier , x = ~Risk, y = ~Return,mode = "lines", type = "scatter")%>% 
    layout(title = "Efficient Frontier",
            # yaxis = list(title = "Mean Returns", tickformat = ".2%"),
            # xaxis = list(title = "Standard Deviation", tickformat = ".2%")

            yaxis = list(title = "Mean Returns"),
            xaxis = list(title = "Standard Deviation")
            
            )
    
    return(EF_plot)
    


}

port_specI <- function(R, opt_method='random',rebal=NULL, traning=NULL, rw=NULL){
    p <- portfolio.spec(assets =  colnames(R))

    p <- add.constraint(portfolio = p, type = "full_investment")

    p <- add.constraint(portfolio = p, type = "long_only")

    p <- add.objective(p,type = "return", name = "mean")

    p <- add.objective(p,type = "risk", name = "StdDev")

    opt <- optimize.portfolio(R, p, optimize_method = opt_method,trace=TRUE)

    if(is.null(rebal)){
        opt <- optimize.portfolio.rebalancing(R, p, optimize_method = opt_method,
                                            trace=TRUE,rebalance_on=rebal,training_period = traning,rolling_window=rw)

    }

    return(list(spec = p, optim = opt))

}

