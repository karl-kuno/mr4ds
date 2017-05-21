## Lab 2


# Exercise 1 --------------------------------------------------------------

## Use dplyrXdf to summarize taxi_xdf by longitude and latitude and additional columns

## First use mutate and create two new columns
## 1. long = round(longitude, 4)
## 2. lat = round(latitude, 4)

## Next convert long and lat into factor columns
## Check out the handy `factorise` function

## Then use the group_by function to group_by long, lat and pickup_hour

## Summarise and calculate number of counts
## sum(fare_amount)
## sum(tip_amount)


# Solution 1 --------------------------------------------------------------




# Exercise 2 --------------------------------------------------------------

## Where Is Your Data?
## Persist and copy to a data.frame/tbl_df called taxi_cdf


# Solution 2 --------------------------------------------------------------




# Exercise 3 --------------------------------------------------------------

## Reuse the map_nyc function to visualize pickups with this dataset


# Solution 3 --------------------------------------------------------------





# Exercise 4 --------------------------------------------------------------

## Use purrr to map the map_nyc function to each pickup_hour


# Solution 4 --------------------------------------------------------------




# Exercise 5 --------------------------------------------------------------

## Take a look at the help for stat_summary_hex
## Add a stat_summary_hex geometry for the total_fare or total_tips aesthetic
## Use fun = sum, bins = 100, and some alpha between 0.5, 0.75



# Solution 5 --------------------------------------------------------------







# Exercise 6 --------------------------------------------------------------

## Redo the map function, but use the fare_nyc function instead



# Solution 6 --------------------------------------------------------------





# Exercise 7 --------------------------------------------------------------

## Redo the plot, but zoom in closer
## Use logarithmic transformation for scale
## summarize only those values where sum of fares > 1000
## Make any other adjustments


# Solution ----------------------------------------------------------------



fare_nyc <- function(df, pickup_hr) {
  
  library(scales)
  library(ggplot2)
  
  
  total_rev <- function(x, threshold = 10^3) {
    if (sum(x) < threshold) {return (NA)}
    else {return (sum(x))}
  }
  
  gplot <- ggplot(df %>% mutate(long = as.numeric(as.character(long)),
                                lat = as.numeric(as.character(lat))),
                  aes(x = long, y = lat, z = total_fare)) +
    geom_point(size=0.06, color="#999999") +
    stat_summary_hex(fun = total_rev, bins=100, alpha=0.5) +
    scale_x_continuous(limits=c(-74.0224, -73.8521)) +
    scale_y_continuous(limits=c(40.6959, 40.8348)) +
    theme_map_dark() +
    scale_fill_gradient(low="#FFFFFF", 
                        high="#E74C3C", 
                        labels=dollar, 
                        trans="log") +
    labs(title = "Revenue for NYC Taxi Pickups",
         subtitle = paste0("Pickups between ", pickup_hr)) +
    coord_equal() + 
    theme(legend.text=element_text(size=10))
  
  
  return(gplot)
  
}



fare_plots <- taxi_cdf %>% 
  # filter(num_pickups > 1) %>% 
  split(.$pickup_hour) %>% 
  map(~ fare_nyc(.x, pickup_hr = .x$pickup_hour[1]))


