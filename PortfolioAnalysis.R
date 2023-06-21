library(tidyverse)
library(ggthemes)
library(quantmod)
library(PortfolioAnalytics)
library(ggh4x)
library(geofacet)
library(astsa)
getSymbols("CURI", auto.assign = TRUE)
CURI <- diff(log(CURI$CURI.Open))

getSymbols("AMD", auto.assign = TRUE)
AMD <- diff(log(AMD$AMD.Open))
AMD <- AMD["2020-01-08/2023-14-08"]

getSymbols("CSCO", auto.assign = TRUE)
CSCO <- diff(log(CSCO$CSCO.Open))
CSCO <- CSCO["2020-01-08/2023-14-08"]

getSymbols("DDOG", auto.assign = TRUE)
DDOG <- diff(log(DDOG$DDOG.Open))
DDOG <- DDOG["2020-01-08/2023-14-08"]

Portfolio <- merge(CURI, AMD, CSCO, DDOG)
Portfolio <- Portfolio[-1,]
WeeklyPortfolio <- apply.weekly(Portfolio, colSums)



PortfolioDF <- as.data.frame(WeeklyPortfolio) 
PortfolioDF$Date <- index(WeeklyPortfolio)

# PortfolioDF %>%
#   mutate(Diff = ifelse(CURI.Open >= AMD.Open, CURI.Open - AMD.Open, AMD.Open - CURI.Open)) %>%
# ggplot(aes(x = Date)) +
#   # One line for Cat rescues
#   geom_line(aes(y = CURI.Open, color = "CURI")) +
#   # Another line for Not_Cat rescues
#   geom_line(aes(y = AMD.Open, color = "AMD")) +
#   # stat_difference() from ggh4x package applies the conditional fill
#   # based on which of Not_Cat and Cat is larger.
#   stat_difference(aes(ymin = CURI.Open, ymax = AMD.Open), alpha = 0.3) +
#   theme_pomological("Bodoni MT")+
#   scale_color_pomological() 

equal_weights <- rep(1 / ncol(Portfolio), ncol(Portfolio))
Benchmark <- Return.portfolio(R = Portfolio, weights = equal_weights, rebalance_on = "months")
colnames(Benchmark) <- "Benchmark"


# Create the portfolio specification
port_spec <- portfolio.spec(colnames(WeeklyPortfolio))
# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec$constraints[[1]]$min_sum <- .99
port_spec$constraints[[1]]$max_sum <- 1.01

port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0, max = 1)
# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Add a risk budge objective
port_spec <- add.objective(portfolio = port_spec, 
                           type = "risk_budget", 
                           name = "StdDev", 
                           min_prisk = .01, 
                           max_prisk = .75)

# Run the optimization
opt_rebal_rb <- optimize.portfolio.rebalancing(R = Portfolio, 
                                               portfolio = port_spec, 
                                               optimize_method = "random",
                                               trace = TRUE,
                                               rebalance_on = "weeks", 
                                               training_period = 52,
                                               rolling_window = 52,
                                               search_size = 5000)

opt_custom <- optimize.portfolio(R = WeeklyPortfolio, portfolio = port_spec, 
                                 optimize_method = "random", trace = TRUE)

# Chart the weights
chart.Weights(opt_rebal_rb)
sum(opt_rebal_rb$opt_rebalancing)
# Chart the percentage contribution to risk
chart.RiskBudget(opt_rebal_rb, match.col = "StdDev", risk.type = "percentage")
# Compute the portfolio returns
returns_rb <- Return.portfolio(R = Portfolio, weights = extractWeights(opt_rebal_rb))

# Forecasted weights
forecasted_weights <- extractWeights(opt_rebal_rb)

# Other statistics
forecasted_stats <- extractStats(optimized_portfolio)

mean(returns_rb["2023"])
sum(returns_rb["2023"])
sd(returns_rb["2023/2024"])

WeeklyReturns <- apply.weekly(returns_rb, sum)
plot(WeeklyReturns)


getSymbols("DDOG", auto.assign = TRUE)
DDOG <- diff(log(DDOG$DDOG.Open))
DDOG <- DDOG["2020-01-08/2023-14-08"]



TimeSeries <-read.csv("C:/Users/ikenn/Downloads/archive/indexData.csv")
TimeSeries <- TimeSeries %>%
  filter(Index == 'NYA')

TimeSeriesXTS <- xts(TimeSeries$Adj.Close, order.by = as_date(TimeSeries$Date))
TimeSeriesXTS_Values <- diff(log(as.numeric(TimeSeriesXTS)))
TimeSeriesXTS <- TimeSeriesXTS[-1]
TimeSeriesXTS <- xts(TimeSeriesXTS_Values, order.by = index(TimeSeriesXTS))
TimeSeriesXTS <- apply.weekly(TimeSeriesXTS, sum)
TimeSeriesXTS <- na.approx(TimeSeriesXTS)

acf2(TimeSeriesXTS)

sarima(TimeSeriesXTS, 0,0,0,0,0,1, S = 52)
ari

sarima(TimeSeriesXT)

Weather <- readxl::read_xlsx("C:/Users/ikenn/Downloads/Weather.xlsx")

Weather <- Weather %>%
  mutate(Date = as.Date(Date))

WeatherIndex <- Weather$Date
Weather <- xts(Weather$Temp, order.by = Weather$Date)
colnames(Weather) <- 'Temp'

Weather <- apply.monthly(Weather, mean)
acf2(diff(Weather), max.lag = 24)

sarima(Weather, 1,1,0,1,0,1, S = 12)
sarima.for(Weather, n.ahead = 50 ,1,1,0,1,0,1,12)

PortfolioDFLong <- PortfolioDF %>%
  pivot_longer(values_to = 'Value', cols = c(CURI.Open, AMD.Open, CSCO.Open, DDOG.Open))
esquisser(PortfolioDFLong)
PortfolioDF <- 
library(esquisse)

ggplot(PortfolioDFLong) +
  aes(x = Date, y = Value, colour = name) +
  geom_line(size = .4) +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  labs(
    x = "X Title",
    y = "Y Title",
    title = "Title",
    subtitle = "Subtitle",
    caption = "Caption",
    color = "Color Label"
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 18L,
                              face = "bold"),
    plot.subtitle = element_text(size = 12L),
    plot.caption = element_text(size = 10L),
    axis.title.y = element_text(size = 14L,
                                face = "bold"),
    axis.title.x = element_text(size = 14L,
                                face = "bold")
  ) +
  facet_grid2(~name)

library(fpp2)
ggseasonplot(WeatherTest, year.labels = T, polar = T) +
  theme_fivethirtyeight()
ggsubseriesplot(WeatherTest) +
  theme_fivethirtyeight()

library(ggpomological)
WeatherTest <- ts(Weather, start = c(1981,1), end = c(1990,12), frequency = 12)

as.ts(Weather)
index(WeatherTest)
