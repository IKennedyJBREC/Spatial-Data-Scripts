library(tidyverse)
library(ggthemes)
library(quantmod)
library(PortfolioAnalytics)
library(ggh4x)
library(geofacet)
getSymbols("CURI", auto.assign = TRUE)
CURI <- diff(log(CURI$CURI.Open))

getSymbols("AMD", auto.assign = TRUE)
AMD <- diff(log(AMD$AMD.Open))
AMD <- AMD["2020-01-08/2023-06-08"]

getSymbols("CSCO", auto.assign = TRUE)
CSCO <- diff(log(CSCO$CSCO.Open))
CSCO <- CSCO["2020-01-08/2023-06-08"]

getSymbols("DDOG", auto.assign = TRUE)
DDOG <- diff(log(DDOG$DDOG.Open))
DDOG <- DDOG["2020-01-08/2023-06-08"]

Portfolio <- merge(CURI, AMD, CSCO, DDOG)
Portfolio <- Portfolio[-1,]
WeeklyPortfolio <- apply.weekly(Portfolio, colSums)


ggplot(Portfolio, aes(date, ))

PortfolioDF <- as.data.frame(WeeklyPortfolio) 

PortfolioDF$Date <- index(WeeklyPortfolio)


PortfolioDF %>%
  mutate(Diff = ifelse(CURI.Open >= AMD.Open, CURI.Open - AMD.Open, AMD.Open - CURI.Open)) %>%
ggplot(aes(x = Date)) +
  # One line for Cat rescues
  geom_line(aes(y = CURI.Open, color = "CURI")) +
  # Another line for Not_Cat rescues
  geom_line(aes(y = AMD.Open, color = "AMD")) +
  
  # stat_difference() from ggh4x package applies the conditional fill
  # based on which of Not_Cat and Cat is larger.
  stat_difference(aes(ymin = CURI.Open, ymax = AMD.Open), alpha = 0.3) +
  theme_pomological("Bodoni MT")+
  scale_color_pomological() 

equal_weights <- rep(1 / ncol(Portfolio), ncol(Portfolio))
Benchmark <- Return.portfolio(R = Portfolio, weights = equal_weights, rebalance_on = "months")
colnames(Benchmark) <- "Benchmark"


# Create the portfolio specification
port_spec <- portfolio.spec(colnames(WeeklyPortfolio))
# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)
# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Add a risk budge objective
port_spec <- add.objective(portfolio = port_spec, 
                           type = "risk_budget", 
                           name = "StdDev", 
                           min_prisk = .01, 
                           max_prisk = .75)


bootstrap
# Run the optimization
opt_rebal_rb <- optimize.portfolio.rebalancing(R = Portfolio, 
                                               portfolio = port_spec, 
                                               optimize_method = "random",
                                               trace = TRUE,
                                               rebalance_on = "days", 
                                               training_period = 60,
                                               rolling_window = 60)

opt_custom <- optimize.portfolio(R = WeeklyPortfolio, portfolio = port_spec, 
                                 optimize_method = "random", trace = TRUE)

# Chart the weights
chart.Weights(opt_custom)
sum(opt_custom$weights)
# Chart the percentage contribution to risk
chart.RiskBudget(opt_custom, match.col = "StdDev", risk.type = "percentage")
# Compute the portfolio returns
returns_rb <- Return.portfolio(R = Portfolio, weights = extractWeights(opt_custom))

mean(returns_rb["2022/2023"])
sum(returns_rb["2023"])
sd(returns_rb)

WeeklyReturns <- apply.weekly(returns_rb, sum)
plot(WeeklyReturns)
