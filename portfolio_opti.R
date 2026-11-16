# Clear all variables
rm(list = ls())

library(tidyquant)
library(timetk) 
library(tidyr)
library(dplyr)

library(ggplot2)
library(plotly)
library(magrittr)
# Get the stock data from Yahoo Finance
tick <- c('SBIN.NS', 'DMART.NS', 'TATASTEEL.NS', 'HAL.NS')
price_data <- tq_get(tick,
                     from = '2022-01-01',
                     to = '2023-01-01',
                     get = 'stock.prices')

head(price_data)

log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

head(log_ret_tidy)

# Convert to wide-format data frame then to an xts object
log_ret_wide <- spread(log_ret_tidy, symbol, value = ret)
log_ret_xts <- tk_xts(log_ret_wide)
head(log_ret_xts)

mean_ret <- colMeans(log_ret_xts)

# cov matrix
wts <- c(0.25, 0.25, 0.25, 0.25)
cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat,4))

port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Calculate the Sharpe Ratio
sharpe_ratio <- (port_returns-0.07)/port_risk


############### Portfolio Weight Calc ################
num_port <- 50000 #no. of sims

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

port_returns <- vector('numeric', length = num_port)

port_risk <- vector('numeric', length = num_port)

sharpe_ratio <- vector('numeric', length = num_port)

### Loop 50000 times ###
for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  all_wts[i,] <- wts
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  port_returns[i] <- port_ret
  
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Assuming 7% Risk free rate
  
  sr <- (port_ret-0.07)/port_sd
  sharpe_ratio[i] <- sr
}

portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

head(portfolio_values)

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

head(max_sr) # Optimal Portfolio
opt_wts <- as.numeric(max_sr[, 1:4])
head(min_var) # Min Variance Portfolio


# Allocation to Optimal v/s Risk-free portfolio
A <- 0.5
y_risky = (max_sr$Return-0.07)/(A*(max_sr$Risk^2))
final_pf_wts = opt_wts*y_risky/100
final_port_returns <- (sum(final_pf_wts * mean_ret) + 1)^252 - 1
final_port_returns <- final_port_returns + (100-y_risky)*0.07/100
final_port_risk <- sqrt(t(final_pf_wts) %*% (cov_mat %*% final_pf_wts))
final_sharpe_ratio <- (final_port_returns-0.07)/final_port_risk
final_util <- final_port_returns - 0.5*A*(final_port_risk^2)

col_names <- colnames(max_sr)[1:4]
utility_portfolio <- tibble(
  !!col_names[1] := final_pf_wts[1],
  !!col_names[2] := final_pf_wts[2],
  !!col_names[3] := final_pf_wts[3],
  !!col_names[4] := final_pf_wts[4],
  Return = final_port_returns,
  Risk = as.numeric(final_port_risk),
  SharpeRatio = as.numeric(final_sharpe_ratio),
  Utility = as.numeric(final_util)
)
print(utility_portfolio)

# PLOT with Sharpe Ratio gradient
p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier")+
  geom_point(data=max_sr, aes(x=Risk, y=Return), color="purple")+
  geom_point(data=utility_portfolio, aes(x=Risk, y=Return), color="red")+
  geom_point(data=min_var, aes(x=Risk, y=Return), color="green")+
  
  annotate("text", x = 0.6, y = 0.1, label = "Red-> Utility Pf\nPurple-> Opt Pf\nGreen-> Min Var Pf", size = 3, color = "black", hjust = 0, vjust = 0) +
  theme(legend.position = "right")
ggplotly(p)


