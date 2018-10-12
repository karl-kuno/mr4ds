library("mrsdeploy")

# Login to a R server using mrsdeploy package
# Can log into either Linux or SQL Server
remoteLogin("http://localhost:12800", prompt = "L> ", session=TRUE, diff=TRUE, commandline=TRUE)

rm(list=ls()) #will remove ALL objects

# Create a few variables in the remote R session
x <- 1:10
y <- 11:20
z <- x + y

# print z in the remote environment
print(z)

# but w does not exist, yet!!
print(w)

pause()

print(z)

getRemoteObject("z")

print(z)

w <- z

putLocalObject("w")

resume()

print(w)

pause()

getRemoteWorkspace()

# List services that have been published and available to this user on the remote server
listServices()

# Create a simple new service using some embedded R code
apiAdd <- publishService(
  "addition",
  code = "result <- x + y",
  inputs = list(x = "numeric", y = "numeric"),
  outputs = list(result = "numeric"),
  v = "v1.0.1",
  alias = "add"
)

# Try creating it a second time !!

api <- getService("addition","v1.0.0")
print(api)

# Get the capabilities for the web-service
cap <- api$capabilities()
print(cap$name)
print(cap$version)
print(cap$inputs)
print(cap$outputs)
print(cap$swagger)

## Test/Consume the web-service
# Using consume
api$consume(1,2)[1:3]
# Using web-service alias
api$add(1,2)[1:3]
# Using web-service alias to obtain result only
api$add(1,2)[3]

## Get the swagger definition for the API
# In Json format
addSwagger <- api$swagger()
# In List format
addSwaggerNj <- api$swagger(json = FALSE)


### More typical example ###
# Create a simple GLM mode and save to file
View(mtcars)
model <- glm(formula = am ~ hp + wt, data = mtcars, family = binomial)
save(model, file = "transmission.RData")

# Create a function to execute the model
manualTransmission <- function(hp, wt) {
  newdata <- data.frame(hp = hp, wt = wt)
  predict(model, newdata, type = "response")
}

# Delete any prior instances & publish the Model and function to Operationalise as a Web-Service
listServices("transmission", "v1.0.0")
deleteService("transmission", "v1.0.0")

api2 <- publishService(
  "transmission",
  code = manualTransmission,
  model = "transmission.RData",
  inputs = list(hp = "numeric", wt = "numeric"),
  outputs = list(answer = "numeric"),
  v = "v1.0.0"
)

print(api2)

# consume with rpc `alias`. Alias to the `consume` function above
print(  result <- api2$manualTransmission(120, 2.8))

system.time( print(  result <- api2$manualTransmission(120, 2.8)$outputParameters$answer) )

# Create some data for scoring data
hp <- 60:300
wt <- 2.8
View(head(scoreData <- data.frame(hp = hp, wt = wt)))


# Score the data
result <- vector("list",nrow(scoreData)) # pre-allocate list for efficiency
system.time({
for(i in 1:nrow(scoreData))
  {
   result[i] <- api2$manualTransmission(as.numeric(scoreData$hp[i]),as.numeric(scoreData$wt[i]))$outputParameters$answer
  }
})

head(result)
length(result)

remoteLogout()

