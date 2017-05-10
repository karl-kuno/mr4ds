azure_batch_nme <- "azbatchtoronto"
azure_batch_url <- "https://azbatchtoronto.eastus2.batch.azure.com"
azure_batch_key <- "QTH5HZL4CE8FFijVsxvpqtx4d7c2EDDoD6htoH4kWzhb+c7R/SuwJqgPIQQ5NMqfKDqadCvfsuOp1xpZj11OQg=="

azure_store_nme <- "alizaidi5388"
azure_store_key <- "P6Yr5sO/c9Ini0ExR2vG4KbK1AyvhU2+m0T0kjcKm5hwn0ObIaXSt4E+W8lre3jjWHJyd9n0NYXgMOx8SE+F7w=="

if (!("doAzureParallel" %in% installed.packages()[, 1])) devtools::install_github(c("Azure/rAzureBatch",
                                                                                    "Azure/doAzureParallel"), 
                                                                                  dependencies = TRUE)

library(doAzureParallel)

generateClusterConfig("pool_config.json")
pool <- makeCluster("pool_config.json")

registerDoAzureParallel(pool)

getDoParWorkers()


mean_change = 1.001
volatility = 0.01
opening_price = 100

simulateMovement <- function() {
  days <- 1825 # ~ 5 years
  movement <- rnorm(days, mean=mean_change, sd=volatility)
  path <- cumprod(c(opening_price, movement))
  return(path)
}

simulateMovement()
plot(simulateMovement())

getClosingPrice <- function() {
  days <- 1825 # ~ 5 years
  movement <- rnorm(days, mean=mean_change, sd=volatility)
  path <- cumprod(c(opening_price, movement))
  closingPrice <- path[days]
  return(closingPrice)
}

system.time(
  closingPrices <- foreach(i = 1:10, .combine='c') %do% {
    replicate(100000, getClosingPrice())
  }
)

system.time(
  closingPrices <- foreach(i = 1:50, .combine='c') %dopar% {
    replicate(100000, getClosingPrice())
  }
)

hist(closingPrices)

# shut down your pool
stopCluster(pool)
