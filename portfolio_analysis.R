#Packages

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
library(corrplot)
library(GGally)

# Observations: 
# Missing Coins XMR, Dash
# Interval should start at 2015
# Interval should end at July 2022 - recent crypto winter
# Need to covert cryptocurrencies to BRL
# Neto and Colombo use: BTC, ETH,XRP, USDT, LTC, XLM, XMR, DASH >>>> 3 groups
          # BTC
          # Altcoin:  ETH,XRP, LTC, XLM, XMR, DASH
          # Stabble: USDT

# ------------------------------------------------------------------------------------------------------------------------------
# Data


# Cripto - data
ada <- read_excel("Crypto Data/ADA.xlsx")
names(ada)[2:dim(ada)[2]] <- paste("ADA",names(ada)[2:dim(ada)[2]],sep=".")

bnb <- read_excel("Crypto Data/BNB.xlsx")
names(bnb)[2:dim(bnb)[2]] <- paste("BNB",names(bnb)[2:dim(bnb)[2]],sep=".")

btc <- read_excel("Crypto Data/BTC.xlsx")
names(btc)[2:dim(btc)[2]] <- paste("BTC",names(btc)[2:dim(btc)[2]],sep=".")

doge <- read_excel("Crypto Data/DOGE.xlsx")
names(doge)[2:dim(doge)[2]] <- paste("DOGE",names(doge)[2:dim(doge)[2]],sep=".")

eth <- read_excel("Crypto Data/ETH.xlsx")
names(eth)[2:dim(eth)[2]] <- paste("ETH",names(eth)[2:dim(eth)[2]],sep=".")

link <- read_excel("Crypto Data/LINK.xlsx")
names(link)[2:dim(link)[2]] <- paste("LINK",names(link)[2:dim(link)[2]],sep=".")

ltc <- read_excel("Crypto Data/LTC.xlsx")
names(ltc)[2:dim(ltc)[2]] <- paste("LTC",names(ltc)[2:dim(ltc)[2]],sep=".")

trx <- read_excel("Crypto Data/TRX.xlsx")
names(trx)[2:dim(trx)[2]] <- paste("TRX",names(trx)[2:dim(trx)[2]],sep=".")

xlm <- read_excel("Crypto Data/XLM.xlsx")
names(xlm)[2:dim(xlm)[2]] <- paste("XLM",names(xlm)[2:dim(xlm)[2]],sep=".")

xrp <- read_excel("Crypto Data/XRP.xlsx")
names(xrp)[2:dim(xrp)[2]] <- paste("XRP",names(xrp)[2:dim(xrp)[2]],sep=".")

crypto_list <- list(ada,bnb,btc,doge,eth,link,ltc,trx,xlm,xrp)


crypto_dataframe <- crypto_list %>% reduce(full_join,by='Date')

crypto_prices <- crypto_dataframe %>% 
  dplyr::select(Date,ends_with("Close")) %>% 
  arrange(Date) %>% 
  rename_with(~str_remove(., '.Close'))

crypto_prices$Date <- as.POSIXct(crypto_prices$Date)

crypto_prices_xts <- xts(as.data.frame(crypto_prices[,-1]),order.by=crypto_prices$Date)

crypto_prices_xts <- xts(as.data.frame(crypto_prices[,-1]),order.by=crypto_prices$Date)

crypto_returns <- na.omit(Return.calculate(crypto_prices_xts) )

# crypto portfolios


crypto_index <- rowMeans(crypto_returns) # Mean returns of crypto

altcoin_index <-  rowMeans(crypto_returns[,c("ADA",  "BNB" ,"DOGE", "ETH",  "LINK", "LTC",  "TRX" , "XLM" , "XRP")] )

crypto_returns <- cbind(crypto_returns, CryptoIndex = crypto_index)

crypto_returns <- cbind(crypto_returns, AltcoinIndex = altcoin_index)

# Base Portfolio

base <- read_excel("Other Assets Data/index.xlsx") %>% 
  arrange(Date) %>% 
  rename_with(~str_remove(., '.Close')) 

base <- base %>% drop_na()
base[,-1] <- lapply(base[,-1], FUN = function(y){as.numeric(y)})

base$Date <- as.POSIXct(base$Date)

base_xts <- xts(as.data.frame(base[,-1]),order.by=base$Date)

base_returns <- na.omit(Return.calculate(base_xts) )

# ---------------------------------------------------------------------------------------------------------------------------------


# Firts look of the assets
all <- na.omit(merge(base_returns,crypto_returns, join='left'))

all_v2 <- all
all_v2$CryptoIndex <- NULL
all_v2$AltcoinIndex <- NULL



assets_correlation <- GGally::ggcorr(all_v2,method = c("everything", "pearson"),label = TRUE)
assets_pairs <- GGally::ggpairs(as.data.frame(coredata(all_v2)), title="Correlation pairs") 

#Stats


assest_performance_anualized <- table.AnnualizedReturns(all_v2,scale = 252)
assets_skewness <- PerformanceAnalytics::skewness(all_v2)
assets_kurtosis <- PerformanceAnalytics::kurtosis(all_v2)
assets_sortino <- PerformanceAnalytics::SortinoRatio(all_v2)
assets_omega <- PerformanceAnalytics::Omega(all_v2)

assets_stats_v1 <- table.Stats(all_v2)
assets_stats_v2 <- rbind(assest_performance_anualized,assets_skewness,assets_kurtosis,assets_sortino,assets_omega)
chart.TimeSeries(all_v2)

chart.CumReturns(all_v2,legend.loc = 'left')

#Portfolios composition
assets_crypto_basket <- c("OURO", "IFIX", "IBOV", "IVVB11", "IMAG","CryptoIndex")
assets_BTC_portfolio <- c("OURO", "IFIX", "IBOV", "IVVB11", "IMAG","BTC")
assets_altcoin_portfolio <- c("OURO", "IFIX", "IBOV", "IVVB11", "IMAG","AltcoinIndex")
assets_base <- c("OURO", "IFIX", "IBOV", "IVVB11", "IMAG")


CryptoBasket  <- portfolio.spec(assets = assets_crypto_basket)
BitcoinPortfolio <- portfolio.spec(assets = assets_BTC_portfolio)
AltcoinPortfolio <- portfolio.spec(assets = assets_altcoin_portfolio)
BasePortfolio <- portfolio.spec(assets = assets_base)

# Alocation Rules

minStd <- function(p){
  p_minStd <- p %>% 
    add.constraint(type = "full_investment") %>% 
    add.constraint(type = "long_only") %>% 
    add.objective(type = "return",name = "mean") %>% 
    add.objective(type = 'risk',name = 'StdDev')
  return(p_minStd)
}



minVar <- function(p){
  p_minVar <- p %>% 
    add.constraint(type = "full_investment") %>% 
    add.constraint(type = "long_only") %>% 
    add.objective(type = "return",name = "mean") %>% 
    add.objective(type = 'risk',name = 'var')
  return(p_minVar)
}

maxMV <- function(p,lambda){
  p_maxMV <- p %>% 
    add.constraint(type = "full_investment") %>% 
    add.constraint(type = "long_only") %>% 
    add.objective(type = "return",name = "mean") %>% 
    add.objective(type = 'risk',name = 'var',risk_aversion=lambda)
  return(p_maxMV)
}

CB_minStdDev <- CryptoBasket %>% minStd()
CB_minVar <- CryptoBasket %>% minVar()
CB_maxMVcon<- CryptoBasket %>% maxMV(lambda = 10)
CB_maxMVmod <- CryptoBasket %>% maxMV(lambda = 5)
CB_maxMVagr <- CryptoBasket %>% maxMV(lambda = 2)


BP_minStdDev <- BitcoinPortfolio %>% minStd()
BP_minVar <- BitcoinPortfolio %>% minVar()
BP_maxMVcon<- BitcoinPortfolio %>% maxMV(lambda = 10)
BP_maxMVmod <- BitcoinPortfolio %>% maxMV(lambda = 5)
BP_maxMVagr <- BitcoinPortfolio %>% maxMV(lambda = 2)


AP_minStdDev <- AltcoinPortfolio %>% minStd()
AP_minVar <- AltcoinPortfolio %>% minVar()
AP_maxMVcon<- AltcoinPortfolio %>% maxMV(lambda = 10)
AP_maxMVmod <- AltcoinPortfolio %>% maxMV(lambda = 5)
AP_maxMVagr <- AltcoinPortfolio %>% maxMV(lambda = 2)

Base_minStdDev <- BasePortfolio %>% minStd()
Base_minVar <- BasePortfolio %>% minVar()
Base_maxMVcon<- BasePortfolio %>% maxMV(lambda = 10)
Base_maxMVmod <- BasePortfolio %>% maxMV(lambda = 5)
Base_maxMVagr <- BasePortfolio %>% maxMV(lambda = 2)


# in-sample
in_sample_CB_minStdDev <- optimize.portfolio(all,CB_minStdDev, optimize_method = 'random',trace=TRUE)
in_sample_CB_minVar <- optimize.portfolio(all,CB_minVar, optimize_method = 'random',trace=TRUE)
in_sample_CB_maxMVcon <- optimize.portfolio(all,CB_maxMVcon, optimize_method = 'random',trace=TRUE)
in_sample_CB_maxMVmod <- optimize.portfolio(all,CB_maxMVmod, optimize_method = 'random',trace=TRUE)
in_sample_CB_maxMVagr <- optimize.portfolio(all,CB_maxMVagr, optimize_method = 'random',trace=TRUE)


in_sample_BP_minStdDev <- optimize.portfolio(all,BP_minStdDev, optimize_method = 'random',trace=TRUE)
in_sample_BP_minVar <- optimize.portfolio(all,BP_minVar, optimize_method = 'random',trace=TRUE)
in_sample_BP_maxMVcon <- optimize.portfolio(all,BP_maxMVcon, optimize_method = 'random',trace=TRUE)
in_sample_BP_maxMVmod <- optimize.portfolio(all,BP_maxMVmod, optimize_method = 'random',trace=TRUE)
in_sample_BP_maxMVagr <- optimize.portfolio(all,BP_maxMVagr, optimize_method = 'random',trace=TRUE)


in_sample_AP_minStdDev <- optimize.portfolio(all,AP_minStdDev, optimize_method = 'random',trace=TRUE)
in_sample_AP_minVar <- optimize.portfolio(all,AP_minVar, optimize_method = 'random',trace=TRUE)
in_sample_AP_maxMVcon <- optimize.portfolio(all,AP_maxMVcon, optimize_method = 'random',trace=TRUE)
in_sample_AP_maxMVmod <- optimize.portfolio(all,AP_maxMVmod, optimize_method = 'random',trace=TRUE)
in_sample_AP_maxMVagr <- optimize.portfolio(all,AP_maxMVagr, optimize_method = 'random',trace=TRUE)


in_sample_Base_minStdDev <- optimize.portfolio(all,Base_minStdDev, optimize_method = 'random',trace=TRUE)
in_sample_Base_minVar <- optimize.portfolio(all,Base_minVar, optimize_method = 'random',trace=TRUE)
in_sample_Base_maxMVcon <- optimize.portfolio(all,Base_maxMVcon, optimize_method = 'random',trace=TRUE)
in_sample_Base_maxMVmod <- optimize.portfolio(all,Base_maxMVmod, optimize_method = 'random',trace=TRUE)
in_sample_Base_maxMVagr <- optimize.portfolio(all,Base_maxMVagr, optimize_method = 'random',trace=TRUE)


chart.RiskReward(in_sample_CB_minVar,risk.col = 'StdDev',return.col = 'mean')
chart.RiskReward(in_sample_BP_minVar,risk.col = 'StdDev',return.col = 'mean')
chart.RiskReward(in_sample_AP_minVar,risk.col = 'StdDev',return.col = 'mean')
chart.RiskReward(in_sample_Base_minVar,risk.col = 'StdDev',return.col = 'mean')


tableResults_in_sample <- cbind(
  table.AnnualizedReturns(Return.portfolio(all[,assets_crypto_basket],extractWeights(in_sample_CB_minStdDev))),
  table.AnnualizedReturns(Return.portfolio(all[,assets_BTC_portfolio],extractWeights(in_sample_BP_minStdDev))),
  table.AnnualizedReturns(Return.portfolio(all[,assets_altcoin_portfolio],extractWeights(in_sample_AP_minStdDev))),
  table.AnnualizedReturns(Return.portfolio(all[,assets_base],extractWeights(in_sample_Base_minStdDev)))
)

names(tableResults_in_sample) <- c("in_sample_CB_minStdDev","in_sample_BP_minStdDev","in_sample_AP_minStdDev","in_sample_Base_minStdDev")
tableResults_in_sample

library(MASS)

custom_fun <- function(R,portfolio,rob_method='mcd'){
  out <- list()
  out$sigma <- cov.rob(R,method=rob_method)$cov
  return(out)
}


optimize.portfolio(all,Base_minStdDev, optimize_method = 'random',momentFUN = "custom_fun")

optimize.portfolio(all,Base_minStdDev, optimize_method = 'ROI',momentFUN = "custom_fun",rob_method = "mve")
