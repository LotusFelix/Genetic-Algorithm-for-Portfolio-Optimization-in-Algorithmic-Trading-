#PART 1A: Construction of a portfolio using the GA package

# Load necessary packages
library(quantmod)
library(GA)

# Define the tickers of the chosen 10 stocks in the portfolio 

assets<-c("AMZN", "PM", "NVDA","UNH","TSLA","WMT","PG","AAPL","XOM","JNJ")

# Fetch historical stock prices for the assets

getSymbols(assets,from="2012-01-01",to="2016-12-31")

# Calculate daily returns for each asset

asset_returns<-lapply(assets,function(stock){
  dailyReturn(Cl(get(stock)))
})

#call merge time series (xts objects) for each stock into a unified file aligned by timestamps

asset_returns<-do.call(merge,asset_returns)

# Convert 'xts' asset_returns object to a regular matrix for processing

return_mat<-as.matrix(asset_returns)

# Calculate the covariance matrix for the returns

covariance_matrix<-cov(return_mat)

# Calculate the average return for each asset

average_stock_return<-colMeans(return_mat)
  
#define upper and lower limits for GA

upper<-rep(1,length(assets))
lower<-rep(0,length(assets))

# Define the fitness function for the GA

fitness_function <- function(weights) {
  
  portfolio_returns <- sum(weights * average_stock_return)
  
  portfolio_risk <- sqrt(sum(sapply(1:length(assets), function(i) {
    sapply(1:length(assets), function(j) {
      weights[i] * weights[j] * covariance_matrix[i,j]
    })
  })))
  
  sharpe_ratio <- portfolio_returns / portfolio_risk
  
  big_penalty_value <- 1000 #enforcing a constraints for weights to sum up to 1
  
  penalty <- abs(sum(weights) - 1) * big_penalty_value 
  
  return(sharpe_ratio - penalty)

}

# Running the GA

ga_results<-ga(type="real-valued",
               fitness=fitness_function,
               upper=upper,
               lower=lower,
               maxiter = 500,
               popSize = 100,
               run=80
               )

# Analyze the results

summary(ga_results)
plot(ga_results)

weights<-ga_results@solution
sum(weights)


#PART 1B: Evaluation of the portfolio on unseen "future" data

#Re-eEstablish portfolio and GA-evolved portfolio weights

assets<-c("AMZN", "PM", "NVDA","UNH","TSLA","WMT","PG","AAPL","XOM","JNJ")

#weights<-c(0.1299235, 0.04698986, 0.1026106, 0.1711794, 0.124141, 0.07032456, 0.06615465, 0.08892481, 0.08913524, 0.110615)


#First, create a function that fetches daily returns for a set of assets (portfolio) over a specified period (timeA to timeB). 

#This function returns a matrix of daily returns, ready for further analysis.

get_returns <- function(portfolio, timeA, timeB) {
  # Fetch daily closing prices for each asset in the portfolio, calculate daily returns,
  # and prevent automatic assignment of data to the environment.
  data_return <- lapply(portfolio, function(i) {
    dailyReturn(Cl(getSymbols(i), from=timeA, to=timeB, auto.assign = FALSE))
  })
  
  # Merge individual xts objects into a single xts object to align dates.
  combined_xts <- do.call(merge, data_return)
  
  # Convert the combined xts object to a matrix for further analysis.
  return_matrix <- as.matrix(combined_xts)
}

#Next, create a function that calculates the Sharpe ratio of a portfolio given its daily returns and weights. 
#The Sharpe ratio is a measure of the risk-adjusted return of the portfolio.

get_sharpe_ratio <- function(portfolio, daily_returns, weights) {
  # Calculate the covariance matrix of the daily returns.
  cov_matrix <- cov(daily_returns)
  
  # Calculate the average returns of the portfolio.
  average_returns <- colMeans(daily_returns)
  
  # Calculate the total return of the portfolio using the provided weights.
  portfolio_return <- sum(weights * average_returns)
  
  # Calculate the portfolio risk as the standard deviation of portfolio returns.
  portfolio_risk <- sqrt(sum(sapply(1:length(portfolio), function(i) {
    sapply(1:length(portfolio), function(j) {
      weights[i] * weights[j] * cov_matrix[i, j]
    })
  })))
  
  # Calculate the Sharpe ratio as the ratio of portfolio return to portfolio risk.
  sharpe_ratio <- portfolio_return / portfolio_risk
  return(sharpe_ratio)
}

#Next, fetch daily returns for the specified assets over the training period 
#Subsequently, calculate the Sharpe ratio to evaluate performance

# Fetch and calculate the daily returns for the training period.
daily_returns_train <- get_returns(assets, "2012-01-01", "2016-12-31")

# Calculate the Sharpe ratio for the training period.
sharpe_ratio_train <- get_sharpe_ratio(assets, daily_returns_train, weights)

#Similar to the training data evaluation, the next part of the code fetches daily returns for the future period
#This calculates the Sharpe ratio to assess the portfolio's performance on unseen data.

# Fetch and calculate the daily returns for the test period (future/unseen data).
daily_returns_test <- get_returns(assets, "2017-01-01", "2017-12-31")

# Calculate the Sharpe ratio for the test period to evaluate how well the portfolio generalizes to new data.
sharpe_ratio_test <- get_sharpe_ratio(assets, daily_returns_test, weights)


#PART 1C: Comparison of the evolved portfolio with balanced and random portfolios 

#Evaluation of Balanced Portfolio

# Define the assets in your portfolio
assets <- c("AMZN", "PM", "NVDA", "UNH", "TSLA", "WMT", "PG", "AAPL", "XOM", "JNJ")

# Create equal weights for a balanced portfolio by dividing 1 by the number of assets

weights_balanced <- rep((1/length(assets)), length(assets))

# Fetch historical data for the defined assets from 2012 to 2016

daily_returns_train <- get_returns(assets, "2012-01-01", "2016-12-31")
  
balanced_sharpe_train<-get_sharpe_ratio(assets,daily_returns_train,weights_balanced)

#Generalisation to unseen data

# Fetch historical data for the same assets for the year 2023

daily_returns_test <- get_returns(assets, "2017-01-01", "2017-12-31")

balanced_sharpe_test<-get_sharpe_ratio(assets,daily_returns_test,weights_balanced)



#Evaluation for random weights

generate_random_weight<-function(n_weights){
  
  random_w<-runif(n_weights,min=0,max=1)
  normalized_weights<-random_w/sum(random_w)
  return (normalized_weights)
}

evaluate_portfolio<-function(n_weights,daily_returns){
  random_weights<-generate_random_weight(n_weights)
  average_returns<-colMeans(daily_returns)
  cov_matrix<-cov(daily_returns)
  portfolio_return<-sum(random_weights*average_returns)
  portfolio_risk<-sqrt(sum(sapply(1:length(assets),function(i){
    (sapply(1:length(assets),function(j){
      random_weights[i]*random_weights[j]*cov_matrix[i,j]
    }))
  })
  ))
  sharpe_ratio<-portfolio_return/portfolio_risk
  return (sharpe_ratio)
}
n_simulation=100

#starting with training data

daily_returns_train <- get_returns(assets, "2012-01-01", "2016-12-31")

random_portfolio_return_train<-replicate(n_simulation,evaluate_portfolio(10,daily_returns_train),simplify = TRUE)

average_random_sharpe_train<-mean(random_portfolio_return_train)

#Generalizing to test data


daily_returns_test <- get_returns(assets, "2017-01-01", "2017-12-31")

random_portfolio_return_test<-replicate(n_simulation,evaluate_portfolio(10,daily_returns_test),simplify = TRUE)

average_random_sharpe_test<-mean(random_portfolio_return_test)


#PART 1D: 


#Adjusting the Fitness Function for Different Risk-Return Balances

# This fitness function is designed to evaluate portfolio performance based on a variable balance between risk and return, determined by the alpha parameter.
# - `weights`: The portfolio weights being optimized.
# - `alpha`: Balances the emphasis between return (alpha) and risk (1-alpha).
# - `daily_returns`: Historical daily returns data for the assets.
fitness_function_modified <- function(weights, alpha=0.5, daily_returns) {
  average_returns <- colMeans(daily_returns)  # Compute average returns for each asset.
  cov_matrix <- cov(daily_returns)  # Compute the covariance matrix for the assets.
  
  # Calculate total portfolio return and risk based on current weights.
  portfolio_return <- sum(weights * average_returns)
  portfolio_risk <- sqrt(sum(sapply(1:length(weights), function(i) {
    sapply(1:length(weights), function(j) {
      weights[i] * weights[j] * cov_matrix[i, j]
    })
  })))
  
  # Adjust return and risk according to alpha.
  adjusted_return <- portfolio_return * alpha
  adjusted_risk <- portfolio_risk * (1 - alpha)
  
  # Calculate an adjusted Sharpe ratio as the performance metric.
  sharpe_ratio_adjusted <- adjusted_return / adjusted_risk
  
  # Apply a penalty if the sum of weights does not equal 1, to enforce a fully-invested portfolio.
  weight_penalty <- 1000  # Penalty multiplier.
  weight_constraint <- (abs(sum(weights) - 1) * weight_penalty)
  
  # Subtract the weight constraint penalty from the Sharpe ratio to penalize non-conforming portfolios.
  final_sharpe_adjusted <- sharpe_ratio_adjusted - weight_constraint
  
  return(final_sharpe_adjusted)  # Return the adjusted Sharpe ratio as the fitness score.
}


#Running GA Optimization for Balanced Risk-Return at Alpha = 0.5

# Fetch daily returns data for training and test periods.
daily_returns_train <- get_returns(assets, "2012-01-01", "2016-12-31")
daily_returns_test <- get_returns(assets, "2017-01-01", "2017-12-31")

# Run GA optimization with a balanced alpha (0.5) using the training data.
ga_results <- ga(type="real-valued",
                 fitness=function(new_weights) fitness_function_modified(new_weights, alpha=0.5, daily_returns_train),
                 upper=upper,
                 lower=lower,
                 maxiter=500,
                 popSize=100,
                 run=80)
evenly_balanced_risk_return <- ga_results@solution  # Store the optimized weights.


#Varying Risk-Return Preferences for Training Data

# Initialize lists to store optimized weights and Sharpe ratios for different alpha values (training data).
weight_preferences_train <- list()
sharpe_ratio_preferences_train <- list()

# Sequence of alpha values to explore different balances between risk and return.
alpha <- seq(0, 1, by=0.1)

# Loop through each alpha value, run GA optimization, and store the results.
for (i in alpha) {
  ga_results_modified_train <- ga(type="real-valued",
                                  fitness=function(weights_mod) fitness_function_modified(weights_mod, i, daily_returns_train),
                                  upper=upper,
                                  lower=lower,
                                  maxiter=500,
                                  popSize=100,
                                  run=80)
  
  # Extract optimized weights and compute the Sharpe ratio for the current alpha.
  weight_attained <- ga_results_modified_train@solution
  sharpe_ratio_attained <- get_sharpe_ratio(assets, daily_returns_train, weight_attained)
  
  # Store the optimized weights and corresponding Sharpe ratio.
  weight_preferences_train[[as.character(i)]] <- weight_attained
  sharpe_ratio_preferences_train[[as.character(i)]] <- sharpe_ratio_attained
}


#Evaluating Test Data with Varying Risk-Return Preferences

# This fitness function is designed to evaluate portfolio performance based on a variable balance between risk and return, determined by the alpha parameter.
# - `weights`: The portfolio weights being optimized.
# - `alpha`: Balances the emphasis between return (alpha) and risk (1-alpha).
# - `daily_returns`: Historical daily returns data for the assets.
fitness_function_modified <- function(weights, alpha=0.5, daily_returns) {
  average_returns <- colMeans(daily_returns)  # Compute average returns for each asset.
  cov_matrix <- cov(daily_returns)  # Compute the covariance matrix for the assets.
  
  # Calculate total portfolio return and risk based on current weights.
  portfolio_return <- sum(weights * average_returns)
  portfolio_risk <- sqrt(sum(sapply(1:length(weights), function(i) {
    sapply(1:length(weights), function(j) {
      weights[i] * weights[j] * cov_matrix[i, j]
    })
  })))
  
  # Adjust return and risk according to alpha.
  adjusted_return <- portfolio_return * alpha
  adjusted_risk <- portfolio_risk * (1 - alpha)
  
  # Calculate an adjusted Sharpe ratio as the performance metric.
  sharpe_ratio_adjusted <- adjusted_return / adjusted_risk
  
  # Apply a penalty if the sum of weights does not equal 1, to enforce a fully-invested portfolio.
  weight_penalty <- 1000  # Penalty multiplier.
  weight_constraint <- (abs(sum(weights) - 1) * weight_penalty)
  
  # Subtract the weight constraint penalty from the Sharpe ratio to penalize non-conforming portfolios.
  final_sharpe_adjusted <- sharpe_ratio_adjusted - weight_constraint
  
  return(final_sharpe_adjusted)  # Return the adjusted Sharpe ratio as the fitness score.
}
