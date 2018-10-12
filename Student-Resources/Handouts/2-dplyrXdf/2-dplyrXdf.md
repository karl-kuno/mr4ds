# Data Manipulation with dplyrXdf
Microsoft Data Science Team  
`r format(Sys.Date(), "%B %d, %Y")`  


# Introduction

## Overview 
### Plan

At the end of this session, you will have learned how to:

* Take advantage of the verbs and syntax you learned from the `dplyr` module to manipulate `RxXdfData` data objects
* Summarize your `RxXdfData` objects quickly and easily
* Create custom functions and use them for mutations and summarizations
* Understand where and when to use the `dplyrXdf` package and when to use functions from the `RevoScaleR` package

## The Microsoft R Family

![Microsoft R Family](images/mr-family.png)

## Microsoft R Component Stack

![Microsoft R Family](images/distros_r.png)

<aside class="notes">
ScaleR: suite of HPA functions for data manipulation and modeling, plus some custom HPC functionality
ConnectR: high speed and direct connectors
DistributedR: framework for cross-platform distributed computation
DeployR: web service development kit through APIs, java, js, .net
</aside>


## Why dplyrXdf?
### Simplify Your Analysis Pipeline

* The `RevoScaleR` package enables R users to manipulate data that is larger than memory
* It introduces a new data type, called an `xdf` (short for eXternal Data Frame), which are highly efficient out-of-memory objects
* However, many of the `RevoScaleR` functions have a dramatically different syntax from base R functions
* The `dplyr` package is an exceptionally popular, due to its appealing syntax, and it's extensibility

## Simpler Analysis with dplyrXdf

* The `dplyrXdf` that exposes most of the `dplyr` functionality to `xdf` objects
* Many data analysis pipelines require creating many intermediate datasets, which are only needed for their role in deriving a final dataset, but have no/little use on their own
* The `dplyrXdf` abstracts this task of file management, so that you can focus on the data itself, rather than the management of intermediate files
* Unlike `dplyr`, or other base R packages, `dplyrXdf` allows you to work with data residing _outside_ of memory, and therefore scales to datasets of arbitrary size


## Requirements 
### What You'll Need

* I expect that you have already covered the `dplyr` training
* Understand the *XDF* data type and how to import data to *XDF*
* If you're working on a different computer than your trianer: have (`devtools`)[github.com/hadley/devtools] (and if on a Windows machine, [Rtools](https://cran.r-project.org/bin/windows/Rtools/))

## Installing dplyrXdf

* The `dplyrXdf` package is not yet on CRAN
* You have to download it from [github](https://github.com/RevolutionAnalytics/dplyrXdf/)
  - if you're on a windows machine, install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) as well
  - the `devtools` package provides a very handy function, `install_github`, for installing R packages saved in github repositories

## Create XDF from taxi data

### Create a local directory to save XDF


```r
your_name <- "alizaidi"
your_dir <- paste0('/datadrive/', your_name)
# File Path to your Data
your_data <- file.path(your_dir, 'tripdata_2015.xdf')
dir.create(your_dir)
```

```
## Warning in dir.create(your_dir): '/datadrive/alizaidi' already exists
```

```r
download.file("http://alizaidi.blob.core.windows.net/training/trainingData/manhattan.xdf", 
              destfile = your_data)
```

## Create a Pointer to XDF


```r
library(dplyrXdf)
taxi_xdf <- RxXdfData(your_data)
taxi_xdf %>% head
```

```
##   VendorID tpep_pickup_datetime tpep_dropoff_datetime passenger_count
## 1        1  2015-07-01 00:00:00   2015-07-01 00:15:26               1
## 2        2  2015-07-01 00:00:06   2015-07-01 00:04:44               1
## 3        2  2015-07-01 00:00:09   2015-07-01 00:06:27               5
## 4        2  2015-07-01 00:00:12   2015-07-01 00:04:18               2
## 5        2  2015-07-01 00:00:16   2015-07-01 00:27:13               1
## 6        1  2015-07-01 00:00:18   2015-07-01 00:15:11               1
##   trip_distance pickup_longitude pickup_latitude RatecodeID
## 1          3.50        -73.99416        40.75113          1
## 2          0.77        -73.98556        40.75554          1
## 3          1.12        -73.97540        40.75190          1
## 4          1.02        -74.01014        40.72051          1
## 5         10.43        -73.87294        40.77415          1
## 6          2.60        -73.98057        40.75100          1
##   store_and_fwd_flag dropoff_longitude dropoff_latitude payment_type
## 1                  N         -73.97682         40.78857            2
## 2                  N         -73.97373         40.75031            2
## 3                  N         -73.99106         40.75073            1
## 4                  N         -74.00793         40.73129            1
## 5                  N         -73.98093         40.76450            1
## 6                  N         -74.00441         40.73099            1
##   fare_amount tip_amount tolls_amount tip_percent pickup_hour pickup_dow
## 1        14.0       0.00         0.00   0.0000000    10PM-1AM        Wed
## 2         5.0       0.00         0.00   0.0000000    10PM-1AM        Wed
## 3         6.5       2.34         0.00   0.3600000    10PM-1AM        Wed
## 4         5.5       1.36         0.00   0.2472727    10PM-1AM        Wed
## 5        32.5      11.80         5.54   0.3630769    10PM-1AM        Wed
## 6        11.5       2.55         0.00   0.2217391    10PM-1AM        Wed
##   dropoff_hour dropoff_dow trip_duration       pickup_nhood
## 1     10PM-1AM         Wed           926   Garment District
## 2     10PM-1AM         Wed           278   Garment District
## 3     10PM-1AM         Wed           378         Turtle Bay
## 4     10PM-1AM         Wed           246            Tribeca
## 5     10PM-1AM         Wed          1617 La Guardia Airport
## 6     10PM-1AM         Wed           893        Murray Hill
##       dropoff_nhood Sample
## 1   Upper West Side   Keep
## 2        Tudor City   Keep
## 3  Garment District   Keep
## 4      West Village   Keep
## 5           Midtown   Keep
## 6 Greenwich Village   Keep
```

```r
taxi_xdf %>% nrow
```

```
## [1] 13550925
```



```r
class(taxi_xdf)
```

```
## [1] "RxXdfData"
## attr(,"package")
## [1] "RevoScaleR"
```


# Simplified Pipelines for Data Summaries

## Data Transforms 
### The rxDataStep Way

* All the functionality exposed by the `dplyrXdf` package can also be completed
by using the `rxDataStep` function in the `RevoScaleR` package included with your MRS installation
* In fact, `dplyrXdf` consists almost entirely of wrapper functions that call on other RevoScaleR functions
* Let's compare the workflow for adding a new column to a dataset with `rxDataStep` vs `dplyrXdf`

---


```r
taxi_xdf %>% rxGetInfo(getVarInfo = TRUE, numRows = 4)
```

```
## File name: /datadrive/alizaidi/tripdata_2015.xdf 
## Number of observations: 13550925 
## Number of variables: 24 
## Number of blocks: 326 
## Compression type: zlib 
## Variable information: 
## Var 1: VendorID, Type: integer, Low/High: (1, 2)
## Var 2: tpep_pickup_datetime, Type: character
## Var 3: tpep_dropoff_datetime, Type: character
## Var 4: passenger_count, Type: integer, Low/High: (1, 9)
## Var 5: trip_distance, Type: numeric, Storage: float32, Low/High: (0.0000, 29.9900)
## Var 6: pickup_longitude, Type: numeric, Storage: float32, Low/High: (-74.2413, -73.7004)
## Var 7: pickup_latitude, Type: numeric, Storage: float32, Low/High: (40.5256, 40.9128)
## Var 8: RatecodeID, Type: integer, Low/High: (1, 99)
## Var 9: store_and_fwd_flag, Type: character
## Var 10: dropoff_longitude, Type: numeric, Storage: float32, Low/High: (-74.2485, -73.7007)
## Var 11: dropoff_latitude, Type: numeric, Storage: float32, Low/High: (40.5027, 40.9138)
## Var 12: payment_type, Type: integer, Low/High: (1, 5)
## Var 13: fare_amount, Type: numeric, Storage: float32, Low/High: (0.0100, 2020.3700)
## Var 14: tip_amount, Type: numeric, Storage: float32, Low/High: (0.0000, 854.8500)
## Var 15: tolls_amount, Type: numeric, Storage: float32, Low/High: (0.0000, 912.5000)
## Var 16: tip_percent, Type: numeric, Low/High: (0.0000, 11600.0003)
## Var 17: pickup_hour
##        7 factor levels: 1AM-5AM 5AM-9AM 9AM-12PM 12PM-4PM 4PM-6PM 6PM-10PM 10PM-1AM
## Var 18: pickup_dow
##        7 factor levels: Sun Mon Tue Wed Thu Fri Sat
## Var 19: dropoff_hour
##        7 factor levels: 1AM-5AM 5AM-9AM 9AM-12PM 12PM-4PM 4PM-6PM 6PM-10PM 10PM-1AM
## Var 20: dropoff_dow
##        7 factor levels: Sun Mon Tue Wed Thu Fri Sat
## Var 21: trip_duration, Type: integer, Low/High: (1, 86399)
## Var 22: pickup_nhood
##        269 factor levels: Annadale Arden Heights Arrochar Arverne Astoria ... Wingate Woodhaven Woodlawn Woodrow Woodside
## Var 23: dropoff_nhood
##        269 factor levels: Annadale Arden Heights Arrochar Arverne Astoria ... Wingate Woodhaven Woodlawn Woodrow Woodside
## Var 24: Sample
##        2 factor levels: Keep Drop
## Data (4 rows starting with row 1):
##   VendorID tpep_pickup_datetime tpep_dropoff_datetime passenger_count
## 1        1  2015-07-01 00:00:00   2015-07-01 00:15:26               1
## 2        2  2015-07-01 00:00:06   2015-07-01 00:04:44               1
## 3        2  2015-07-01 00:00:09   2015-07-01 00:06:27               5
## 4        2  2015-07-01 00:00:12   2015-07-01 00:04:18               2
##   trip_distance pickup_longitude pickup_latitude RatecodeID
## 1          3.50        -73.99416        40.75113          1
## 2          0.77        -73.98556        40.75554          1
## 3          1.12        -73.97540        40.75190          1
## 4          1.02        -74.01014        40.72051          1
##   store_and_fwd_flag dropoff_longitude dropoff_latitude payment_type
## 1                  N         -73.97682         40.78857            2
## 2                  N         -73.97373         40.75031            2
## 3                  N         -73.99106         40.75073            1
## 4                  N         -74.00793         40.73129            1
##   fare_amount tip_amount tolls_amount tip_percent pickup_hour pickup_dow
## 1        14.0       0.00            0   0.0000000    10PM-1AM        Wed
## 2         5.0       0.00            0   0.0000000    10PM-1AM        Wed
## 3         6.5       2.34            0   0.3600000    10PM-1AM        Wed
## 4         5.5       1.36            0   0.2472727    10PM-1AM        Wed
##   dropoff_hour dropoff_dow trip_duration     pickup_nhood    dropoff_nhood
## 1     10PM-1AM         Wed           926 Garment District  Upper West Side
## 2     10PM-1AM         Wed           278 Garment District       Tudor City
## 3     10PM-1AM         Wed           378       Turtle Bay Garment District
## 4     10PM-1AM         Wed           246          Tribeca     West Village
##   Sample
## 1   Keep
## 2   Keep
## 3   Keep
## 4   Keep
```

---

```r
taxi_transform <- RxXdfData(your_data)
```

---



```r
system.time(rxDataStep(inData = taxi_xdf,
           outFile = taxi_transform,
           transforms = list(tip_pct = tip_amount/fare_amount),
           overwrite = TRUE))
```

```
##    user  system elapsed 
##  18.184   1.340  73.184
```

## Data Transforms 
### The rxDataStep Way


```r
rxGetInfo(RxXdfData(taxi_transform), numRows = 2)
```

```
## File name: /datadrive/alizaidi/tripdata_2015.xdf 
## Number of observations: 13550925 
## Number of variables: 25 
## Number of blocks: 326 
## Compression type: zlib 
## Data (2 rows starting with row 1):
##   VendorID tpep_pickup_datetime tpep_dropoff_datetime passenger_count
## 1        1  2015-07-01 00:00:00   2015-07-01 00:15:26               1
## 2        2  2015-07-01 00:00:06   2015-07-01 00:04:44               1
##   trip_distance pickup_longitude pickup_latitude RatecodeID
## 1          3.50        -73.99416        40.75113          1
## 2          0.77        -73.98556        40.75554          1
##   store_and_fwd_flag dropoff_longitude dropoff_latitude payment_type
## 1                  N         -73.97682         40.78857            2
## 2                  N         -73.97373         40.75031            2
##   fare_amount tip_amount tolls_amount tip_percent pickup_hour pickup_dow
## 1          14          0            0           0    10PM-1AM        Wed
## 2           5          0            0           0    10PM-1AM        Wed
##   dropoff_hour dropoff_dow trip_duration     pickup_nhood   dropoff_nhood
## 1     10PM-1AM         Wed           926 Garment District Upper West Side
## 2     10PM-1AM         Wed           278 Garment District      Tudor City
##   Sample tip_pct
## 1   Keep       0
## 2   Keep       0
```

## Data Transforms 
### The dplyrXdf Way

* We could do the same operation with `dplyrXdf`, using the exact same syntax 
that we learned in the `dplyr` module and taking advantage of the `%>%` operator


```r
system.time(taxi_transform <- taxi_xdf %>% mutate(tip_pct = tip_amount/fare_amount))
```

```
##    user  system elapsed 
##  16.672   1.284  72.019
```

```r
taxi_transform %>% rxGetInfo(numRows = 2)
```

```
## File name: /tmp/RtmptR5PAR/file4e7e31120a5d.xdf 
## Number of observations: 13550925 
## Number of variables: 25 
## Number of blocks: 326 
## Compression type: zlib 
## Data (2 rows starting with row 1):
##   VendorID tpep_pickup_datetime tpep_dropoff_datetime passenger_count
## 1        1  2015-07-01 00:00:00   2015-07-01 00:15:26               1
## 2        2  2015-07-01 00:00:06   2015-07-01 00:04:44               1
##   trip_distance pickup_longitude pickup_latitude RatecodeID
## 1          3.50        -73.99416        40.75113          1
## 2          0.77        -73.98556        40.75554          1
##   store_and_fwd_flag dropoff_longitude dropoff_latitude payment_type
## 1                  N         -73.97682         40.78857            2
## 2                  N         -73.97373         40.75031            2
##   fare_amount tip_amount tolls_amount tip_percent pickup_hour pickup_dow
## 1          14          0            0           0    10PM-1AM        Wed
## 2           5          0            0           0    10PM-1AM        Wed
##   dropoff_hour dropoff_dow trip_duration     pickup_nhood   dropoff_nhood
## 1     10PM-1AM         Wed           926 Garment District Upper West Side
## 2     10PM-1AM         Wed           278 Garment District      Tudor City
##   Sample tip_pct
## 1   Keep       0
## 2   Keep       0
```

## Differences

* The major difference between the `rxDataStep` operation and the `dplyrXdf` method, is that we do not specify an `outFile` argument anywhere in the `dplyrXdf` pipeline
* In our case, we have assigned our `mutate` value to a new variable called `taxi_transform`
* This creates a temporary file to save the intermediate `xdf`, and only saves the most recent output of a pipeline, where a pipeline is defined as all operations starting from a raw xdf file.
* To copy an *xdf* from the temporary directory to permanent storage, use the `persist` verb

---

```r
taxi_transform@file
```

```
## [1] "/tmp/RtmptR5PAR/file4e7e31120a5d.xdf"
```


```r
persist(taxi_transform, outFile = "/datadrive/alizaidi/taxiTransform.xdf") -> taxi_transform
```

## Using dplyrXdf for Aggregations 
### dplyrXdf Way

* The `dplyrXdf` package really shines when used for data aggregations and summarizations
* Whereas `rxSummary`, `rxCube`, and `rxCrossTabs` can compute a few summary statistics and do aggregations very quickly, they are not sufficiently general to be used in all places
---

```r
taxi_group <- taxi_transform %>%
  group_by(pickup_nhood) %>%
  summarise(ave_tip = mean(tip_pct))
taxi_group %>% head
```

```
##      pickup_nhood    ave_tip
## 1   Arden Heights 0.03018716
## 2        Arrochar 0.12730094
## 3         Arverne 0.10823474
## 4         Astoria 0.13915798
## 5 Astoria Heights 0.09144881
## 6      Auburndale 0.09283292
```

## Using dplyrXdf for Aggregations 
### rxCube Way

* The above could have been done with `rxCube` as well, but would require additional considerations
* We would have to make sure that the `pickup_nhood` column was a factor (can't mutate in place because of different data types)
* `rxCube` can only provide summations and averages, so we cannot get standard deviations for instance.
* Creating your own factors is never a pleasant experience. You may feel like everything is going right until

![faceplant](http://www.ohmagif.com/wp-content/uploads/2015/02/dude-front-flip-epic-face-plant.gif)
---

```r
rxFactors(inData = taxi_transform, 
          outFile = "/datadrive/alizaidi/taxi_factor.xdf", 
          factorInfo = c("pickup_nhood"), 
          overwrite = TRUE)
```

```
## Warning in factorInfoVarList(factorInfo[i], varInfo, sortLevelsDefault = sortLevels, : 
##   No changes will be made to the factor variable 'pickup_nhood'
##   because 'sortLevels = FALSE' and there is no 'indexMap'.
```

```
## Warning in rxFactorsBase(inData = dataIO[["inData"]], factorInfo =
## factorInfo, : No changes made to the data set.
```

```r
head(rxCube(tip_pct ~ pickup_nhood, 
            means = TRUE, 
            data = "/datadrive/alizaidi/taxi_factor.xdf"))
```

```
##      pickup_nhood    tip_pct Counts
## 1        Annadale        NaN      0
## 2   Arden Heights 0.03018716      5
## 3        Arrochar 0.12730094      5
## 4         Arverne 0.10823474     16
## 5         Astoria 0.13915798  63662
## 6 Astoria Heights 0.09144881   1512
```

```r
# file.remove("data/taxi_factor.xdf")
```

# Creating Functional Pipelines with dplyrXdf
As we saw above, it's pretty easy to create a summarization or aggregation script. We can encapsulate our aggregation into it's own function.
Suppose we wanted to calculate average tip as a function of dropoff and pickup neighborhoods. In the `dplyr` nonmenclature, this means grouping by dropoff and pickup neighborhoods, and summarizing/averaging tip percent.


```r
rxGetInfo(taxi_transform, numRows = 5)
```

```
## File name: /datadrive/alizaidi/taxiTransform.xdf 
## Number of observations: 13550925 
## Number of variables: 25 
## Number of blocks: 326 
## Compression type: zlib 
## Data (5 rows starting with row 1):
##   VendorID tpep_pickup_datetime tpep_dropoff_datetime passenger_count
## 1        1  2015-07-01 00:00:00   2015-07-01 00:15:26               1
## 2        2  2015-07-01 00:00:06   2015-07-01 00:04:44               1
## 3        2  2015-07-01 00:00:09   2015-07-01 00:06:27               5
## 4        2  2015-07-01 00:00:12   2015-07-01 00:04:18               2
## 5        2  2015-07-01 00:00:16   2015-07-01 00:27:13               1
##   trip_distance pickup_longitude pickup_latitude RatecodeID
## 1          3.50        -73.99416        40.75113          1
## 2          0.77        -73.98556        40.75554          1
## 3          1.12        -73.97540        40.75190          1
## 4          1.02        -74.01014        40.72051          1
## 5         10.43        -73.87294        40.77415          1
##   store_and_fwd_flag dropoff_longitude dropoff_latitude payment_type
## 1                  N         -73.97682         40.78857            2
## 2                  N         -73.97373         40.75031            2
## 3                  N         -73.99106         40.75073            1
## 4                  N         -74.00793         40.73129            1
## 5                  N         -73.98093         40.76450            1
##   fare_amount tip_amount tolls_amount tip_percent pickup_hour pickup_dow
## 1        14.0       0.00         0.00   0.0000000    10PM-1AM        Wed
## 2         5.0       0.00         0.00   0.0000000    10PM-1AM        Wed
## 3         6.5       2.34         0.00   0.3600000    10PM-1AM        Wed
## 4         5.5       1.36         0.00   0.2472727    10PM-1AM        Wed
## 5        32.5      11.80         5.54   0.3630769    10PM-1AM        Wed
##   dropoff_hour dropoff_dow trip_duration       pickup_nhood
## 1     10PM-1AM         Wed           926   Garment District
## 2     10PM-1AM         Wed           278   Garment District
## 3     10PM-1AM         Wed           378         Turtle Bay
## 4     10PM-1AM         Wed           246            Tribeca
## 5     10PM-1AM         Wed          1617 La Guardia Airport
##      dropoff_nhood Sample   tip_pct
## 1  Upper West Side   Keep 0.0000000
## 2       Tudor City   Keep 0.0000000
## 3 Garment District   Keep 0.3600000
## 4     West Village   Keep 0.2472727
## 5          Midtown   Keep 0.3630769
```
---

```r
mht_url <- "http://alizaidi.blob.core.windows.net/training/manhattan.rds"
manhattan_hoods <- readRDS(gzcon(url(mht_url)))
```
---

```r
taxi_transform %>% 
    filter(pickup_nhood %in% mht_hoods,
           dropoff_nhood %in% mht_hoods, 
           .rxArgs = list(transformObjects = list(mht_hoods = manhattan_hoods))) %>% 
    group_by(dropoff_nhood, pickup_nhood) %>% 
    summarize(ave_tip = mean(tip_pct), 
              ave_dist = mean(trip_distance)) %>% 
    filter(ave_dist > 3, ave_tip > 0.05) -> sum_df
```



---


```r
sum_df %>% rxGetInfo(getVarInfo = TRUE, numRows = 5)
```

```
## File name: /tmp/RtmptR5PAR/file4e7e2eb0c46a.xdf 
## Number of observations: 326 
## Number of variables: 4 
## Number of blocks: 1 
## Compression type: zlib 
## Variable information: 
## Var 1: dropoff_nhood
##        269 factor levels: Annadale Arden Heights Arrochar Arverne Astoria ... Wingate Woodhaven Woodlawn Woodrow Woodside
## Var 2: pickup_nhood
##        269 factor levels: Annadale Arden Heights Arrochar Arverne Astoria ... Wingate Woodhaven Woodlawn Woodrow Woodside
## Var 3: ave_tip, Type: numeric, Low/High: (0.0714, 0.2558)
## Var 4: ave_dist, Type: numeric, Low/High: (3.0054, 13.9174)
## Data (5 rows starting with row 1):
##      dropoff_nhood pickup_nhood   ave_tip ave_dist
## 1     Central Park Battery Park 0.1027538 6.046687
## 2          Clinton Battery Park 0.1218306 3.896076
## 3      East Harlem Battery Park 0.1495259 9.396812
## 4     East Village Battery Park 0.1433366 4.054086
## 5 Garment District Battery Park 0.1237102 3.905760
```

```r
class(sum_df)
```

```
## [1] "grouped_tbl_xdf"
## attr(,"package")
## [1] "dplyrXdf"
```

---

Alternatively, we can encapsulate this script into a function, so that we can easily call it in a functional pipeline.


```r
taxi_hood_sum <- function(taxi_data = taxi_df, ...) {
  
  taxi_data %>% 
    filter(pickup_nhood %in% manhattan_hoods,
           dropoff_nhood %in% manhattan_hoods, ...) %>% 
    group_by(dropoff_nhood, pickup_nhood) %>% 
    summarize(ave_tip = mean(tip_pct), 
              ave_dist = mean(trip_distance)) %>% 
    filter(ave_dist > 3, ave_tip > 0.05) -> sum_df
  
  return(sum_df)
  
}
```

---

The resulting summary object isn't very large (about 408 rows in this case), so it shouldn't cause any memory overhead issues if we covert it now to a `data.frame`. We can plot our results using our favorite plotting library. 


```r
tile_plot_hood <- function(df = taxi_hood_sum()) {
  
  library(ggplot2)
  
  ggplot(data = df, aes(x = pickup_nhood, y = dropoff_nhood)) + 
    geom_tile(aes(fill = ave_tip), colour = "white") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = 'bottom') + 
    scale_fill_gradient(low = "white", high = "steelblue") -> gplot
  
  return(gplot)
}
```

---


```r
# tile_plot_hood(as.data.frame(sum_df))
taxi_transform <- taxi_xdf %>% mutate(tip_pct = tip_amount/fare_amount)
library(plotly)
sum_df <- taxi_hood_sum(taxi_transform, 
                        .rxArgs = list(transformObjects = list(manhattan_hoods = manhattan_hoods))) %>% 
  persist("/datadrive/alizaidi/summarized.xdf")
ggplotly(tile_plot_hood(as.data.frame(sum_df)))
```

<!--html_preserve--><div id="htmlwidget-4638f76672c2a936a1a8" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-4638f76672c2a936a1a8">{"x":{"data":[{"x":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24],"y":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24],"z":[[null,0.21876672077539,null,null,0.242542091750931,0.50226927436474,0.400084526685049,null,0.246254366857864,0.394175441316335,null,0.168418693309661,0.356287650664918,null,null,null,0.253167972338023,0.383914756194739,0.356011909087942,null,0.358006224629888,0.37477470033647,0.275205115120222,null],[0.169926405972566,null,null,0.101014531646206,null,null,0.378464825474443,0.136109818599051,null,0.349030969366242,0.305302537702167,0.294879354178335,null,0.57141150512435,0.204132803199917,0.319934009507136,null,null,null,0.25545372311318,null,null,0.233796063739397,0.309669824957983],[null,0.307387370303819,null,null,null,0.213650806933849,null,0.367956662628431,null,null,null,0.290061184719798,0.253417853049172,0.173250443816125,null,null,null,0.378140608759837,null,null,0.358994685876169,0.37514703752149,0.333786748393593,null],[null,0.103710483575556,null,null,0.0553263309838307,0.134467982688647,null,null,0.036175187919009,null,null,0.230481446272424,0.104857271353983,0.172841032822464,null,null,0.110203307534866,0.235460167598568,0.193270269911841,null,0.224227750915789,0.181185796375537,0.104970819078243,null],[0.2734096713041,null,null,0.132618873292607,null,0.188896482752819,0.339562661853175,0.218199469263326,null,null,null,0.168806349028352,0.17797891808303,0.319123016290713,0.243399162500523,0.320618385335483,null,0.281966934230309,null,0.290983780726423,null,null,0.222882538324782,null],[0.423645456384521,null,0.237450720663706,0.104616129339713,0.183295969482873,null,0.255562345455507,0.303826066615006,0.168883836008737,0.204481763971987,0.259630160769384,null,null,0.0352352107607445,0.250576847004412,0.210633087115103,0.266856655027847,null,0.254316964945787,0.295012931130902,null,null,0.0930090521794109,0.253386598406555],[0.390070577059879,0.355863787001001,null,null,0.33330184587461,0.151164291373646,null,0.407496935094448,null,null,null,0.0546782588950551,0.182599360609132,0.311343520355379,null,null,null,0.359846926525465,null,null,0.372776786322302,0.36076998894196,0.190417666153447,null],[null,0.228854542632922,0.373166858215777,null,0.245315408536707,0.227585961944404,0.410555182136466,null,0.269998082988603,0.365148563004107,null,0.240383231894759,0.226483646935614,null,null,null,0.261257189687329,0.425857958457206,0.331802718538965,null,0.332141515456488,0.339959916430325,0.114416925260059,null],[0.283605974197389,null,null,0.0289953610545288,null,0.211727294696159,null,0.225348077201696,null,null,null,0.234473978824582,0.199035098184876,0.254384889588623,null,0.250526199582526,null,0.307347502013138,null,null,null,null,0.31213263387203,null],[0.413429917941833,0.337495512052763,null,null,null,0.203953629506926,null,0.399982854573331,null,null,null,0.2488818182725,0.235387271702403,0.39031391894895,null,null,null,0.379210313258145,null,0.43358295078163,null,0.341081228193309,0.293675741293338,null],[null,0.309289244072086,null,null,null,0.294203391624686,null,null,null,null,null,0.210430873740698,0.337703436321914,0.584412479459981,null,null,null,0.402742452910529,null,null,0.364490092067758,0.363004274481218,0.362257843614771,null],[0.430679318151484,0.267555013903647,0.317682562327846,0.233983992389775,0.24111207707075,null,0.31789874863817,0.306233852937553,0.174664540145756,0.190476898693174,0.317245126502787,null,null,0.110261606230937,0.312951214178658,0.257899417188704,0.273179478378036,null,0.280489322503542,0.242072474883559,0.225473525894317,0.211801741425775,null,0.299860985691275],[0.339053899616073,null,0.271031245484091,0.132554649819842,0.246185210290932,null,0.320699910264749,0.337980745168127,0.181533885384422,0.223827986238085,0.28594808742523,null,null,0.206637851829096,0.322801309287644,0.24692650744326,0.314743556525445,null,0.311320570242314,0.341197441012311,0.256312082889,null,null,0.294204168888863],[0.312954055685336,0.324390073209634,0.313606495209662,0.343706764694405,0.260331839107982,0.0353138611535026,0.295139513045031,0.364391322616497,0.259181899182184,0.348202060655944,0.46898840101964,0.119274596257304,0.139591670167948,null,0.201436891755486,0.295904502627182,0.291557363810919,0.316104800016668,0.369081730148144,0.340794678918198,0.242286910410513,0.261905232392184,null,0.281722942808429],[null,0.183460974143547,null,null,0.27204020590632,0.234832040939622,null,null,null,null,null,0.160534583795963,0.182229720372393,null,null,null,0.257747018332517,0.4072915006489,null,null,0.328131376529774,0.362578821909155,0.143791748366032,null],[null,0.223111012623064,null,null,0.232441006101549,0.0727802147677371,null,null,0.221748266214672,null,null,0.172914278578631,0.18595187482771,0,null,null,0.282611417941859,0.364511861655225,0.330282234256818,null,0.297300757207175,0.335053270250239,0.293817473821456,null],[0.273312203030928,null,null,0.0344786400337398,null,0.256390619259486,null,0.216419018263452,null,null,null,0.236548603099676,0.263533035555945,0.245919692040686,0.159235275638465,0.231085274538317,null,0.315178280406239,null,0.294835543662189,null,null,0.287530516157034,null],[0.439327571939316,null,0.344999831830161,0.249588184538324,0.307516348369296,null,0.385646302856449,0.440226005938578,0.290905757683477,0.338828324454686,0.386250050315272,null,null,0.318093211229571,0.3944009971967,0.394715693081175,0.314699345000853,null,0.35417025219054,0.379791076656107,0.341153969423913,null,null,0.384508169358361],[0.367845044855216,null,null,null,null,0.232730445258415,null,0.327209086858148,null,null,null,0.356759241020351,0.220230714111099,null,null,null,null,0.312370717395998,null,0.342866604088463,null,null,0.322332346737827,null],[null,0.272988793546944,null,null,0.306001965606775,0.277437804805407,null,null,0.259429964384011,0.417984633429029,null,0.159316700759423,0.307329751286131,1,null,null,0.335610497581969,0.393820035856853,0.368205600490364,null,0.370583621893137,0.351840089090044,0.353904165576527,null],[0.381821151338526,null,0.372532255851347,0.284566140789157,0.303988085429001,null,0.417984175942974,0.387598358738273,null,null,0.372424320572597,0.294368246962675,null,0.457740280273799,0.367986258779345,0.391236803291682,null,0.348786212597456,null,0.379785545164435,null,null,0.298720145518413,0.365120046397277],[0.412763322985067,null,0.4041503423676,0.259320479407943,null,null,0.394931880975646,0.400996000801793,null,0.357107293449337,0.400981244996785,null,null,0.31014711560392,0.383163170137528,0.38807934594172,null,null,null,0.411973767065961,null,null,0.293578671654792,0.398494436853402],[0.427361436357013,0.267397841958387,0.329581149375875,0.048680492449399,0.245891959628878,0.0141741702995772,0.258566085689541,0.299258924353923,0.24017075658906,0.326846954447621,0.287428886446621,null,null,null,0.330115146710938,0.219898003953837,0.290616091348695,0.276520559870454,0.31101929304998,0.275373344573669,0.266086074065613,0.267048744058972,null,0.269233136503186],[null,0.30436889046536,null,null,null,0.298256802677193,null,null,null,null,null,0.235381597882862,0.238245504670596,0.474290569209267,null,null,null,0.396219679325111,null,null,0.349134242556448,0.362121366980957,0.320545544036915,null]],"text":[[null,"ave_tip: 0.11<br>pickup_nhood: Central Park<br>dropoff_nhood: Battery Park",null,null,"ave_tip: 0.12<br>pickup_nhood: Clinton<br>dropoff_nhood: Battery Park","ave_tip: 0.16<br>pickup_nhood: East Harlem<br>dropoff_nhood: Battery Park","ave_tip: 0.15<br>pickup_nhood: East Village<br>dropoff_nhood: Battery Park",null,"ave_tip: 0.12<br>pickup_nhood: Garment District<br>dropoff_nhood: Battery Park","ave_tip: 0.14<br>pickup_nhood: Gramercy<br>dropoff_nhood: Battery Park",null,"ave_tip: 0.1<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Battery Park","ave_tip: 0.14<br>pickup_nhood: Harlem<br>dropoff_nhood: Battery Park",null,null,null,"ave_tip: 0.12<br>pickup_nhood: Midtown<br>dropoff_nhood: Battery Park","ave_tip: 0.14<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Battery Park","ave_tip: 0.14<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Battery Park",null,"ave_tip: 0.14<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Battery Park","ave_tip: 0.14<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Battery Park","ave_tip: 0.12<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Battery Park",null],["ave_tip: 0.1<br>pickup_nhood: Battery Park<br>dropoff_nhood: Central Park",null,null,"ave_tip: 0.09<br>pickup_nhood: Chinatown<br>dropoff_nhood: Central Park",null,null,"ave_tip: 0.14<br>pickup_nhood: East Village<br>dropoff_nhood: Central Park","ave_tip: 0.1<br>pickup_nhood: Financial District<br>dropoff_nhood: Central Park",null,"ave_tip: 0.14<br>pickup_nhood: Gramercy<br>dropoff_nhood: Central Park","ave_tip: 0.13<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: Central Park","ave_tip: 0.13<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Central Park",null,"ave_tip: 0.18<br>pickup_nhood: Inwood<br>dropoff_nhood: Central Park","ave_tip: 0.11<br>pickup_nhood: Little Italy<br>dropoff_nhood: Central Park","ave_tip: 0.13<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Central Park",null,null,null,"ave_tip: 0.12<br>pickup_nhood: Tribeca<br>dropoff_nhood: Central Park",null,null,"ave_tip: 0.11<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Central Park","ave_tip: 0.13<br>pickup_nhood: West Village<br>dropoff_nhood: Central Park"],[null,"ave_tip: 0.13<br>pickup_nhood: Central Park<br>dropoff_nhood: Chelsea",null,null,null,"ave_tip: 0.11<br>pickup_nhood: East Harlem<br>dropoff_nhood: Chelsea",null,"ave_tip: 0.14<br>pickup_nhood: Financial District<br>dropoff_nhood: Chelsea",null,null,null,"ave_tip: 0.12<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Chelsea","ave_tip: 0.12<br>pickup_nhood: Harlem<br>dropoff_nhood: Chelsea","ave_tip: 0.1<br>pickup_nhood: Inwood<br>dropoff_nhood: Chelsea",null,null,null,"ave_tip: 0.14<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Chelsea",null,null,"ave_tip: 0.14<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Chelsea","ave_tip: 0.14<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Chelsea","ave_tip: 0.13<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Chelsea",null],[null,"ave_tip: 0.09<br>pickup_nhood: Central Park<br>dropoff_nhood: Chinatown",null,null,"ave_tip: 0.08<br>pickup_nhood: Clinton<br>dropoff_nhood: Chinatown","ave_tip: 0.1<br>pickup_nhood: East Harlem<br>dropoff_nhood: Chinatown",null,null,"ave_tip: 0.08<br>pickup_nhood: Garment District<br>dropoff_nhood: Chinatown",null,null,"ave_tip: 0.11<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Chinatown","ave_tip: 0.09<br>pickup_nhood: Harlem<br>dropoff_nhood: Chinatown","ave_tip: 0.1<br>pickup_nhood: Inwood<br>dropoff_nhood: Chinatown",null,null,"ave_tip: 0.09<br>pickup_nhood: Midtown<br>dropoff_nhood: Chinatown","ave_tip: 0.11<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Chinatown","ave_tip: 0.11<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Chinatown",null,"ave_tip: 0.11<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Chinatown","ave_tip: 0.1<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Chinatown","ave_tip: 0.09<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Chinatown",null],["ave_tip: 0.12<br>pickup_nhood: Battery Park<br>dropoff_nhood: Clinton",null,null,"ave_tip: 0.1<br>pickup_nhood: Chinatown<br>dropoff_nhood: Clinton",null,"ave_tip: 0.11<br>pickup_nhood: East Harlem<br>dropoff_nhood: Clinton","ave_tip: 0.13<br>pickup_nhood: East Village<br>dropoff_nhood: Clinton","ave_tip: 0.11<br>pickup_nhood: Financial District<br>dropoff_nhood: Clinton",null,null,null,"ave_tip: 0.1<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Clinton","ave_tip: 0.1<br>pickup_nhood: Harlem<br>dropoff_nhood: Clinton","ave_tip: 0.13<br>pickup_nhood: Inwood<br>dropoff_nhood: Clinton","ave_tip: 0.12<br>pickup_nhood: Little Italy<br>dropoff_nhood: Clinton","ave_tip: 0.13<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Clinton",null,"ave_tip: 0.12<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Clinton",null,"ave_tip: 0.13<br>pickup_nhood: Tribeca<br>dropoff_nhood: Clinton",null,null,"ave_tip: 0.11<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Clinton",null],["ave_tip: 0.15<br>pickup_nhood: Battery Park<br>dropoff_nhood: East Harlem",null,"ave_tip: 0.12<br>pickup_nhood: Chelsea<br>dropoff_nhood: East Harlem","ave_tip: 0.09<br>pickup_nhood: Chinatown<br>dropoff_nhood: East Harlem","ave_tip: 0.11<br>pickup_nhood: Clinton<br>dropoff_nhood: East Harlem",null,"ave_tip: 0.12<br>pickup_nhood: East Village<br>dropoff_nhood: East Harlem","ave_tip: 0.13<br>pickup_nhood: Financial District<br>dropoff_nhood: East Harlem","ave_tip: 0.1<br>pickup_nhood: Garment District<br>dropoff_nhood: East Harlem","ave_tip: 0.11<br>pickup_nhood: Gramercy<br>dropoff_nhood: East Harlem","ave_tip: 0.12<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: East Harlem",null,null,"ave_tip: 0.08<br>pickup_nhood: Inwood<br>dropoff_nhood: East Harlem","ave_tip: 0.12<br>pickup_nhood: Little Italy<br>dropoff_nhood: East Harlem","ave_tip: 0.11<br>pickup_nhood: Lower East Side<br>dropoff_nhood: East Harlem","ave_tip: 0.12<br>pickup_nhood: Midtown<br>dropoff_nhood: East Harlem",null,"ave_tip: 0.12<br>pickup_nhood: Murray Hill<br>dropoff_nhood: East Harlem","ave_tip: 0.13<br>pickup_nhood: Tribeca<br>dropoff_nhood: East Harlem",null,null,"ave_tip: 0.09<br>pickup_nhood: Washington Heights<br>dropoff_nhood: East Harlem","ave_tip: 0.12<br>pickup_nhood: West Village<br>dropoff_nhood: East Harlem"],["ave_tip: 0.14<br>pickup_nhood: Battery Park<br>dropoff_nhood: East Village","ave_tip: 0.14<br>pickup_nhood: Central Park<br>dropoff_nhood: East Village",null,null,"ave_tip: 0.13<br>pickup_nhood: Clinton<br>dropoff_nhood: East Village","ave_tip: 0.1<br>pickup_nhood: East Harlem<br>dropoff_nhood: East Village",null,"ave_tip: 0.15<br>pickup_nhood: Financial District<br>dropoff_nhood: East Village",null,null,null,"ave_tip: 0.08<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: East Village","ave_tip: 0.11<br>pickup_nhood: Harlem<br>dropoff_nhood: East Village","ave_tip: 0.13<br>pickup_nhood: Inwood<br>dropoff_nhood: East Village",null,null,null,"ave_tip: 0.14<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: East Village",null,null,"ave_tip: 0.14<br>pickup_nhood: Upper East Side<br>dropoff_nhood: East Village","ave_tip: 0.14<br>pickup_nhood: Upper West Side<br>dropoff_nhood: East Village","ave_tip: 0.11<br>pickup_nhood: Washington Heights<br>dropoff_nhood: East Village",null],[null,"ave_tip: 0.11<br>pickup_nhood: Central Park<br>dropoff_nhood: Financial District","ave_tip: 0.14<br>pickup_nhood: Chelsea<br>dropoff_nhood: Financial District",null,"ave_tip: 0.12<br>pickup_nhood: Clinton<br>dropoff_nhood: Financial District","ave_tip: 0.11<br>pickup_nhood: East Harlem<br>dropoff_nhood: Financial District","ave_tip: 0.15<br>pickup_nhood: East Village<br>dropoff_nhood: Financial District",null,"ave_tip: 0.12<br>pickup_nhood: Garment District<br>dropoff_nhood: Financial District","ave_tip: 0.14<br>pickup_nhood: Gramercy<br>dropoff_nhood: Financial District",null,"ave_tip: 0.12<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Financial District","ave_tip: 0.11<br>pickup_nhood: Harlem<br>dropoff_nhood: Financial District",null,null,null,"ave_tip: 0.12<br>pickup_nhood: Midtown<br>dropoff_nhood: Financial District","ave_tip: 0.15<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Financial District","ave_tip: 0.13<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Financial District",null,"ave_tip: 0.13<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Financial District","ave_tip: 0.13<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Financial District","ave_tip: 0.09<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Financial District",null],["ave_tip: 0.12<br>pickup_nhood: Battery Park<br>dropoff_nhood: Garment District",null,null,"ave_tip: 0.08<br>pickup_nhood: Chinatown<br>dropoff_nhood: Garment District",null,"ave_tip: 0.11<br>pickup_nhood: East Harlem<br>dropoff_nhood: Garment District",null,"ave_tip: 0.11<br>pickup_nhood: Financial District<br>dropoff_nhood: Garment District",null,null,null,"ave_tip: 0.11<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Garment District","ave_tip: 0.11<br>pickup_nhood: Harlem<br>dropoff_nhood: Garment District","ave_tip: 0.12<br>pickup_nhood: Inwood<br>dropoff_nhood: Garment District",null,"ave_tip: 0.12<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Garment District",null,"ave_tip: 0.13<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Garment District",null,null,null,null,"ave_tip: 0.13<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Garment District",null],["ave_tip: 0.15<br>pickup_nhood: Battery Park<br>dropoff_nhood: Gramercy","ave_tip: 0.13<br>pickup_nhood: Central Park<br>dropoff_nhood: Gramercy",null,null,null,"ave_tip: 0.11<br>pickup_nhood: East Harlem<br>dropoff_nhood: Gramercy",null,"ave_tip: 0.15<br>pickup_nhood: Financial District<br>dropoff_nhood: Gramercy",null,null,null,"ave_tip: 0.12<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Gramercy","ave_tip: 0.11<br>pickup_nhood: Harlem<br>dropoff_nhood: Gramercy","ave_tip: 0.14<br>pickup_nhood: Inwood<br>dropoff_nhood: Gramercy",null,null,null,"ave_tip: 0.14<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Gramercy",null,"ave_tip: 0.15<br>pickup_nhood: Tribeca<br>dropoff_nhood: Gramercy",null,"ave_tip: 0.13<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Gramercy","ave_tip: 0.13<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Gramercy",null],[null,"ave_tip: 0.13<br>pickup_nhood: Central Park<br>dropoff_nhood: Greenwich Village",null,null,null,"ave_tip: 0.13<br>pickup_nhood: East Harlem<br>dropoff_nhood: Greenwich Village",null,null,null,null,null,"ave_tip: 0.11<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Greenwich Village","ave_tip: 0.13<br>pickup_nhood: Harlem<br>dropoff_nhood: Greenwich Village","ave_tip: 0.18<br>pickup_nhood: Inwood<br>dropoff_nhood: Greenwich Village",null,null,null,"ave_tip: 0.15<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Greenwich Village",null,null,"ave_tip: 0.14<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Greenwich Village","ave_tip: 0.14<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Greenwich Village","ave_tip: 0.14<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Greenwich Village",null],["ave_tip: 0.15<br>pickup_nhood: Battery Park<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.12<br>pickup_nhood: Central Park<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.13<br>pickup_nhood: Chelsea<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.11<br>pickup_nhood: Chinatown<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.12<br>pickup_nhood: Clinton<br>dropoff_nhood: Hamilton Heights",null,"ave_tip: 0.13<br>pickup_nhood: East Village<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.13<br>pickup_nhood: Financial District<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.1<br>pickup_nhood: Garment District<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.11<br>pickup_nhood: Gramercy<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.13<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: Hamilton Heights",null,null,"ave_tip: 0.09<br>pickup_nhood: Inwood<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.13<br>pickup_nhood: Little Italy<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.12<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.12<br>pickup_nhood: Midtown<br>dropoff_nhood: Hamilton Heights",null,"ave_tip: 0.12<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.12<br>pickup_nhood: Tribeca<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.11<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Hamilton Heights","ave_tip: 0.11<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Hamilton Heights",null,"ave_tip: 0.13<br>pickup_nhood: West Village<br>dropoff_nhood: Hamilton Heights"],["ave_tip: 0.13<br>pickup_nhood: Battery Park<br>dropoff_nhood: Harlem",null,"ave_tip: 0.12<br>pickup_nhood: Chelsea<br>dropoff_nhood: Harlem","ave_tip: 0.1<br>pickup_nhood: Chinatown<br>dropoff_nhood: Harlem","ave_tip: 0.12<br>pickup_nhood: Clinton<br>dropoff_nhood: Harlem",null,"ave_tip: 0.13<br>pickup_nhood: East Village<br>dropoff_nhood: Harlem","ave_tip: 0.13<br>pickup_nhood: Financial District<br>dropoff_nhood: Harlem","ave_tip: 0.1<br>pickup_nhood: Garment District<br>dropoff_nhood: Harlem","ave_tip: 0.11<br>pickup_nhood: Gramercy<br>dropoff_nhood: Harlem","ave_tip: 0.12<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: Harlem",null,null,"ave_tip: 0.11<br>pickup_nhood: Inwood<br>dropoff_nhood: Harlem","ave_tip: 0.13<br>pickup_nhood: Little Italy<br>dropoff_nhood: Harlem","ave_tip: 0.12<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Harlem","ave_tip: 0.13<br>pickup_nhood: Midtown<br>dropoff_nhood: Harlem",null,"ave_tip: 0.13<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Harlem","ave_tip: 0.13<br>pickup_nhood: Tribeca<br>dropoff_nhood: Harlem","ave_tip: 0.12<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Harlem",null,null,"ave_tip: 0.13<br>pickup_nhood: West Village<br>dropoff_nhood: Harlem"],["ave_tip: 0.13<br>pickup_nhood: Battery Park<br>dropoff_nhood: Inwood","ave_tip: 0.13<br>pickup_nhood: Central Park<br>dropoff_nhood: Inwood","ave_tip: 0.13<br>pickup_nhood: Chelsea<br>dropoff_nhood: Inwood","ave_tip: 0.13<br>pickup_nhood: Chinatown<br>dropoff_nhood: Inwood","ave_tip: 0.12<br>pickup_nhood: Clinton<br>dropoff_nhood: Inwood","ave_tip: 0.08<br>pickup_nhood: East Harlem<br>dropoff_nhood: Inwood","ave_tip: 0.13<br>pickup_nhood: East Village<br>dropoff_nhood: Inwood","ave_tip: 0.14<br>pickup_nhood: Financial District<br>dropoff_nhood: Inwood","ave_tip: 0.12<br>pickup_nhood: Garment District<br>dropoff_nhood: Inwood","ave_tip: 0.14<br>pickup_nhood: Gramercy<br>dropoff_nhood: Inwood","ave_tip: 0.16<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: Inwood","ave_tip: 0.09<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Inwood","ave_tip: 0.1<br>pickup_nhood: Harlem<br>dropoff_nhood: Inwood",null,"ave_tip: 0.11<br>pickup_nhood: Little Italy<br>dropoff_nhood: Inwood","ave_tip: 0.13<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Inwood","ave_tip: 0.13<br>pickup_nhood: Midtown<br>dropoff_nhood: Inwood","ave_tip: 0.13<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Inwood","ave_tip: 0.14<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Inwood","ave_tip: 0.13<br>pickup_nhood: Tribeca<br>dropoff_nhood: Inwood","ave_tip: 0.12<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Inwood","ave_tip: 0.12<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Inwood",null,"ave_tip: 0.12<br>pickup_nhood: West Village<br>dropoff_nhood: Inwood"],[null,"ave_tip: 0.11<br>pickup_nhood: Central Park<br>dropoff_nhood: Little Italy",null,null,"ave_tip: 0.12<br>pickup_nhood: Clinton<br>dropoff_nhood: Little Italy","ave_tip: 0.11<br>pickup_nhood: East Harlem<br>dropoff_nhood: Little Italy",null,null,null,null,null,"ave_tip: 0.1<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Little Italy","ave_tip: 0.11<br>pickup_nhood: Harlem<br>dropoff_nhood: Little Italy",null,null,null,"ave_tip: 0.12<br>pickup_nhood: Midtown<br>dropoff_nhood: Little Italy","ave_tip: 0.15<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Little Italy",null,null,"ave_tip: 0.13<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Little Italy","ave_tip: 0.14<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Little Italy","ave_tip: 0.1<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Little Italy",null],[null,"ave_tip: 0.11<br>pickup_nhood: Central Park<br>dropoff_nhood: Lower East Side",null,null,"ave_tip: 0.11<br>pickup_nhood: Clinton<br>dropoff_nhood: Lower East Side","ave_tip: 0.08<br>pickup_nhood: East Harlem<br>dropoff_nhood: Lower East Side",null,null,"ave_tip: 0.11<br>pickup_nhood: Garment District<br>dropoff_nhood: Lower East Side",null,null,"ave_tip: 0.1<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Lower East Side","ave_tip: 0.11<br>pickup_nhood: Harlem<br>dropoff_nhood: Lower East Side","ave_tip: 0.07<br>pickup_nhood: Inwood<br>dropoff_nhood: Lower East Side",null,null,"ave_tip: 0.12<br>pickup_nhood: Midtown<br>dropoff_nhood: Lower East Side","ave_tip: 0.14<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Lower East Side","ave_tip: 0.13<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Lower East Side",null,"ave_tip: 0.13<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Lower East Side","ave_tip: 0.13<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Lower East Side","ave_tip: 0.13<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Lower East Side",null],["ave_tip: 0.12<br>pickup_nhood: Battery Park<br>dropoff_nhood: Midtown",null,null,"ave_tip: 0.08<br>pickup_nhood: Chinatown<br>dropoff_nhood: Midtown",null,"ave_tip: 0.12<br>pickup_nhood: East Harlem<br>dropoff_nhood: Midtown",null,"ave_tip: 0.11<br>pickup_nhood: Financial District<br>dropoff_nhood: Midtown",null,null,null,"ave_tip: 0.12<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Midtown","ave_tip: 0.12<br>pickup_nhood: Harlem<br>dropoff_nhood: Midtown","ave_tip: 0.12<br>pickup_nhood: Inwood<br>dropoff_nhood: Midtown","ave_tip: 0.1<br>pickup_nhood: Little Italy<br>dropoff_nhood: Midtown","ave_tip: 0.11<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Midtown",null,"ave_tip: 0.13<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Midtown",null,"ave_tip: 0.13<br>pickup_nhood: Tribeca<br>dropoff_nhood: Midtown",null,null,"ave_tip: 0.12<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Midtown",null],["ave_tip: 0.15<br>pickup_nhood: Battery Park<br>dropoff_nhood: Morningside Heights",null,"ave_tip: 0.14<br>pickup_nhood: Chelsea<br>dropoff_nhood: Morningside Heights","ave_tip: 0.12<br>pickup_nhood: Chinatown<br>dropoff_nhood: Morningside Heights","ave_tip: 0.13<br>pickup_nhood: Clinton<br>dropoff_nhood: Morningside Heights",null,"ave_tip: 0.14<br>pickup_nhood: East Village<br>dropoff_nhood: Morningside Heights","ave_tip: 0.15<br>pickup_nhood: Financial District<br>dropoff_nhood: Morningside Heights","ave_tip: 0.13<br>pickup_nhood: Garment District<br>dropoff_nhood: Morningside Heights","ave_tip: 0.13<br>pickup_nhood: Gramercy<br>dropoff_nhood: Morningside Heights","ave_tip: 0.14<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: Morningside Heights",null,null,"ave_tip: 0.13<br>pickup_nhood: Inwood<br>dropoff_nhood: Morningside Heights","ave_tip: 0.14<br>pickup_nhood: Little Italy<br>dropoff_nhood: Morningside Heights","ave_tip: 0.14<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Morningside Heights","ave_tip: 0.13<br>pickup_nhood: Midtown<br>dropoff_nhood: Morningside Heights",null,"ave_tip: 0.14<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Morningside Heights","ave_tip: 0.14<br>pickup_nhood: Tribeca<br>dropoff_nhood: Morningside Heights","ave_tip: 0.13<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Morningside Heights",null,null,"ave_tip: 0.14<br>pickup_nhood: West Village<br>dropoff_nhood: Morningside Heights"],["ave_tip: 0.14<br>pickup_nhood: Battery Park<br>dropoff_nhood: Murray Hill",null,null,null,null,"ave_tip: 0.11<br>pickup_nhood: East Harlem<br>dropoff_nhood: Murray Hill",null,"ave_tip: 0.13<br>pickup_nhood: Financial District<br>dropoff_nhood: Murray Hill",null,null,null,"ave_tip: 0.14<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Murray Hill","ave_tip: 0.11<br>pickup_nhood: Harlem<br>dropoff_nhood: Murray Hill",null,null,null,null,"ave_tip: 0.13<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Murray Hill",null,"ave_tip: 0.13<br>pickup_nhood: Tribeca<br>dropoff_nhood: Murray Hill",null,null,"ave_tip: 0.13<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Murray Hill",null],[null,"ave_tip: 0.12<br>pickup_nhood: Central Park<br>dropoff_nhood: Tribeca",null,null,"ave_tip: 0.13<br>pickup_nhood: Clinton<br>dropoff_nhood: Tribeca","ave_tip: 0.12<br>pickup_nhood: East Harlem<br>dropoff_nhood: Tribeca",null,null,"ave_tip: 0.12<br>pickup_nhood: Garment District<br>dropoff_nhood: Tribeca","ave_tip: 0.15<br>pickup_nhood: Gramercy<br>dropoff_nhood: Tribeca",null,"ave_tip: 0.1<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Tribeca","ave_tip: 0.13<br>pickup_nhood: Harlem<br>dropoff_nhood: Tribeca","ave_tip: 0.26<br>pickup_nhood: Inwood<br>dropoff_nhood: Tribeca",null,null,"ave_tip: 0.13<br>pickup_nhood: Midtown<br>dropoff_nhood: Tribeca","ave_tip: 0.14<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Tribeca","ave_tip: 0.14<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Tribeca",null,"ave_tip: 0.14<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Tribeca","ave_tip: 0.14<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Tribeca","ave_tip: 0.14<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Tribeca",null],["ave_tip: 0.14<br>pickup_nhood: Battery Park<br>dropoff_nhood: Upper East Side",null,"ave_tip: 0.14<br>pickup_nhood: Chelsea<br>dropoff_nhood: Upper East Side","ave_tip: 0.12<br>pickup_nhood: Chinatown<br>dropoff_nhood: Upper East Side","ave_tip: 0.13<br>pickup_nhood: Clinton<br>dropoff_nhood: Upper East Side",null,"ave_tip: 0.15<br>pickup_nhood: East Village<br>dropoff_nhood: Upper East Side","ave_tip: 0.14<br>pickup_nhood: Financial District<br>dropoff_nhood: Upper East Side",null,null,"ave_tip: 0.14<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: Upper East Side","ave_tip: 0.13<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: Upper East Side",null,"ave_tip: 0.16<br>pickup_nhood: Inwood<br>dropoff_nhood: Upper East Side","ave_tip: 0.14<br>pickup_nhood: Little Italy<br>dropoff_nhood: Upper East Side","ave_tip: 0.14<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Upper East Side",null,"ave_tip: 0.14<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Upper East Side",null,"ave_tip: 0.14<br>pickup_nhood: Tribeca<br>dropoff_nhood: Upper East Side",null,null,"ave_tip: 0.13<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Upper East Side","ave_tip: 0.14<br>pickup_nhood: West Village<br>dropoff_nhood: Upper East Side"],["ave_tip: 0.15<br>pickup_nhood: Battery Park<br>dropoff_nhood: Upper West Side",null,"ave_tip: 0.15<br>pickup_nhood: Chelsea<br>dropoff_nhood: Upper West Side","ave_tip: 0.12<br>pickup_nhood: Chinatown<br>dropoff_nhood: Upper West Side",null,null,"ave_tip: 0.14<br>pickup_nhood: East Village<br>dropoff_nhood: Upper West Side","ave_tip: 0.15<br>pickup_nhood: Financial District<br>dropoff_nhood: Upper West Side",null,"ave_tip: 0.14<br>pickup_nhood: Gramercy<br>dropoff_nhood: Upper West Side","ave_tip: 0.15<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: Upper West Side",null,null,"ave_tip: 0.13<br>pickup_nhood: Inwood<br>dropoff_nhood: Upper West Side","ave_tip: 0.14<br>pickup_nhood: Little Italy<br>dropoff_nhood: Upper West Side","ave_tip: 0.14<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Upper West Side",null,null,null,"ave_tip: 0.15<br>pickup_nhood: Tribeca<br>dropoff_nhood: Upper West Side",null,null,"ave_tip: 0.13<br>pickup_nhood: Washington Heights<br>dropoff_nhood: Upper West Side","ave_tip: 0.14<br>pickup_nhood: West Village<br>dropoff_nhood: Upper West Side"],["ave_tip: 0.15<br>pickup_nhood: Battery Park<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: Central Park<br>dropoff_nhood: Washington Heights","ave_tip: 0.13<br>pickup_nhood: Chelsea<br>dropoff_nhood: Washington Heights","ave_tip: 0.08<br>pickup_nhood: Chinatown<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: Clinton<br>dropoff_nhood: Washington Heights","ave_tip: 0.07<br>pickup_nhood: East Harlem<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: East Village<br>dropoff_nhood: Washington Heights","ave_tip: 0.13<br>pickup_nhood: Financial District<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: Garment District<br>dropoff_nhood: Washington Heights","ave_tip: 0.13<br>pickup_nhood: Gramercy<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: Greenwich Village<br>dropoff_nhood: Washington Heights",null,null,null,"ave_tip: 0.13<br>pickup_nhood: Little Italy<br>dropoff_nhood: Washington Heights","ave_tip: 0.11<br>pickup_nhood: Lower East Side<br>dropoff_nhood: Washington Heights","ave_tip: 0.13<br>pickup_nhood: Midtown<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: Washington Heights","ave_tip: 0.13<br>pickup_nhood: Murray Hill<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: Tribeca<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: Upper East Side<br>dropoff_nhood: Washington Heights","ave_tip: 0.12<br>pickup_nhood: Upper West Side<br>dropoff_nhood: Washington Heights",null,"ave_tip: 0.12<br>pickup_nhood: West Village<br>dropoff_nhood: Washington Heights"],[null,"ave_tip: 0.13<br>pickup_nhood: Central Park<br>dropoff_nhood: West Village",null,null,null,"ave_tip: 0.13<br>pickup_nhood: East Harlem<br>dropoff_nhood: West Village",null,null,null,null,null,"ave_tip: 0.11<br>pickup_nhood: Hamilton Heights<br>dropoff_nhood: West Village","ave_tip: 0.12<br>pickup_nhood: Harlem<br>dropoff_nhood: West Village","ave_tip: 0.16<br>pickup_nhood: Inwood<br>dropoff_nhood: West Village",null,null,null,"ave_tip: 0.14<br>pickup_nhood: Morningside Heights<br>dropoff_nhood: West Village",null,null,"ave_tip: 0.14<br>pickup_nhood: Upper East Side<br>dropoff_nhood: West Village","ave_tip: 0.14<br>pickup_nhood: Upper West Side<br>dropoff_nhood: West Village","ave_tip: 0.13<br>pickup_nhood: Washington Heights<br>dropoff_nhood: West Village",null]],"colorscale":[[0,"#FFFFFF"],[0.0141741702995772,"#FDFDFE"],[0.0289953610545288,"#FAFBFD"],[0.0344786400337398,"#F9FAFC"],[0.0352352107607445,"#F9FAFC"],[0.0353138611535026,"#F9FAFC"],[0.036175187919009,"#F9FAFC"],[0.048680492449399,"#F6F9FB"],[0.0546782588950551,"#F5F8FB"],[0.0553263309838307,"#F5F8FB"],[0.0727802147677371,"#F2F5F9"],[0.0930090521794109,"#EFF3F8"],[0.101014531646206,"#EDF2F7"],[0.103710483575556,"#EDF1F7"],[0.104616129339713,"#EDF1F7"],[0.104857271353983,"#EDF1F7"],[0.104970819078243,"#EDF1F7"],[0.110203307534866,"#ECF0F7"],[0.110261606230937,"#ECF0F7"],[0.114416925260059,"#EBF0F6"],[0.119274596257304,"#EAEFF6"],[0.132554649819842,"#E8EEF5"],[0.132618873292607,"#E8EEF5"],[0.134467982688647,"#E7EDF5"],[0.136109818599051,"#E7EDF5"],[0.139591670167948,"#E7EDF4"],[0.143791748366032,"#E6ECF4"],[0.151164291373646,"#E4EBF4"],[0.159235275638465,"#E3EAF3"],[0.159316700759423,"#E3EAF3"],[0.160534583795963,"#E3EAF3"],[0.168418693309661,"#E1E9F2"],[0.168806349028352,"#E1E9F2"],[0.168883836008737,"#E1E9F2"],[0.169926405972566,"#E1E9F2"],[0.172841032822464,"#E1E8F2"],[0.172914278578631,"#E1E8F2"],[0.173250443816125,"#E1E8F2"],[0.174664540145756,"#E0E8F2"],[0.17797891808303,"#E0E8F2"],[0.181185796375537,"#DFE7F1"],[0.181533885384422,"#DFE7F1"],[0.182229720372393,"#DFE7F1"],[0.182599360609132,"#DFE7F1"],[0.183295969482873,"#DFE7F1"],[0.183460974143547,"#DFE7F1"],[0.18595187482771,"#DEE7F1"],[0.188896482752819,"#DEE6F1"],[0.190417666153447,"#DEE6F1"],[0.190476898693174,"#DEE6F1"],[0.193270269911841,"#DDE6F0"],[0.199035098184876,"#DCE5F0"],[0.201436891755486,"#DCE5F0"],[0.203953629506926,"#DBE4F0"],[0.204132803199917,"#DBE4F0"],[0.204481763971987,"#DBE4F0"],[0.206637851829096,"#DBE4EF"],[0.210430873740698,"#DAE3EF"],[0.210633087115103,"#DAE3EF"],[0.211727294696159,"#DAE3EF"],[0.211801741425775,"#DAE3EF"],[0.213650806933849,"#DAE3EF"],[0.216419018263452,"#D9E3EF"],[0.218199469263326,"#D9E2EF"],[0.21876672077539,"#D9E2EE"],[0.219898003953837,"#D8E2EE"],[0.220230714111099,"#D8E2EE"],[0.221748266214672,"#D8E2EE"],[0.222882538324782,"#D8E2EE"],[0.223111012623064,"#D8E2EE"],[0.223827986238085,"#D8E2EE"],[0.224227750915789,"#D8E2EE"],[0.225348077201696,"#D7E1EE"],[0.225473525894317,"#D7E1EE"],[0.226483646935614,"#D7E1EE"],[0.227585961944404,"#D7E1EE"],[0.228854542632922,"#D7E1EE"],[0.230481446272424,"#D7E1EE"],[0.231085274538317,"#D6E1EE"],[0.232441006101549,"#D6E1ED"],[0.232730445258415,"#D6E0ED"],[0.233796063739397,"#D6E0ED"],[0.233983992389775,"#D6E0ED"],[0.234473978824582,"#D6E0ED"],[0.234832040939622,"#D6E0ED"],[0.235381597882862,"#D6E0ED"],[0.235387271702403,"#D6E0ED"],[0.235460167598568,"#D6E0ED"],[0.236548603099676,"#D6E0ED"],[0.237450720663706,"#D5E0ED"],[0.238245504670596,"#D5E0ED"],[0.24017075658906,"#D5E0ED"],[0.240383231894759,"#D5DFED"],[0.24111207707075,"#D5DFED"],[0.242072474883559,"#D5DFED"],[0.242286910410513,"#D5DFED"],[0.242542091750931,"#D4DFED"],[0.243399162500523,"#D4DFED"],[0.245315408536707,"#D4DFEC"],[0.245891959628878,"#D4DFEC"],[0.245919692040686,"#D4DFEC"],[0.246185210290932,"#D4DFEC"],[0.246254366857864,"#D4DFEC"],[0.24692650744326,"#D4DFEC"],[0.2488818182725,"#D3DEEC"],[0.249588184538324,"#D3DEEC"],[0.250526199582526,"#D3DEEC"],[0.250576847004412,"#D3DEEC"],[0.253167972338023,"#D3DEEC"],[0.253386598406555,"#D3DEEC"],[0.253417853049172,"#D3DEEC"],[0.254316964945787,"#D2DEEC"],[0.254384889588623,"#D2DEEC"],[0.25545372311318,"#D2DEEC"],[0.255562345455507,"#D2DEEC"],[0.256312082889,"#D2DDEC"],[0.256390619259486,"#D2DDEC"],[0.257747018332517,"#D2DDEC"],[0.257899417188704,"#D2DDEC"],[0.258566085689541,"#D2DDEB"],[0.259181899182184,"#D2DDEB"],[0.259320479407943,"#D2DDEB"],[0.259429964384011,"#D2DDEB"],[0.259630160769384,"#D1DDEB"],[0.260331839107982,"#D1DDEB"],[0.261257189687329,"#D1DDEB"],[0.261905232392184,"#D1DDEB"],[0.263533035555945,"#D1DCEB"],[0.266086074065613,"#D0DCEB"],[0.266856655027847,"#D0DCEB"],[0.267048744058972,"#D0DCEB"],[0.267397841958387,"#D0DCEB"],[0.267555013903647,"#D0DCEB"],[0.269233136503186,"#D0DCEB"],[0.269998082988603,"#D0DCEB"],[0.271031245484091,"#CFDCEB"],[0.27204020590632,"#CFDBEA"],[0.272988793546944,"#CFDBEA"],[0.273179478378036,"#CFDBEA"],[0.273312203030928,"#CFDBEA"],[0.2734096713041,"#CFDBEA"],[0.275205115120222,"#CFDBEA"],[0.275373344573669,"#CFDBEA"],[0.276520559870454,"#CFDBEA"],[0.277437804805407,"#CEDBEA"],[0.280489322503542,"#CEDAEA"],[0.281722942808429,"#CEDAEA"],[0.281966934230309,"#CEDAEA"],[0.282611417941859,"#CDDAEA"],[0.283605974197389,"#CDDAEA"],[0.284566140789157,"#CDDAEA"],[0.28594808742523,"#CDDAE9"],[0.287428886446621,"#CDD9E9"],[0.287530516157034,"#CDD9E9"],[0.290061184719798,"#CCD9E9"],[0.290616091348695,"#CCD9E9"],[0.290905757683477,"#CCD9E9"],[0.290983780726423,"#CCD9E9"],[0.291557363810919,"#CCD9E9"],[0.293578671654792,"#CCD9E9"],[0.293675741293338,"#CCD9E9"],[0.293817473821456,"#CBD9E9"],[0.294203391624686,"#CBD9E9"],[0.294204168888863,"#CBD9E9"],[0.294368246962675,"#CBD9E9"],[0.294835543662189,"#CBD8E9"],[0.294879354178335,"#CBD8E9"],[0.295012931130902,"#CBD8E9"],[0.295139513045031,"#CBD8E9"],[0.295904502627182,"#CBD8E9"],[0.297300757207175,"#CBD8E9"],[0.298256802677193,"#CBD8E9"],[0.298720145518413,"#CBD8E8"],[0.299258924353923,"#CBD8E8"],[0.299860985691275,"#CAD8E8"],[0.303826066615006,"#CAD7E8"],[0.303988085429001,"#CAD7E8"],[0.30436889046536,"#CAD7E8"],[0.305302537702167,"#C9D7E8"],[0.306001965606775,"#C9D7E8"],[0.306233852937553,"#C9D7E8"],[0.307329751286131,"#C9D7E8"],[0.307347502013138,"#C9D7E8"],[0.307387370303819,"#C9D7E8"],[0.307516348369296,"#C9D7E8"],[0.309289244072086,"#C9D7E8"],[0.309669824957983,"#C9D7E8"],[0.31014711560392,"#C9D6E8"],[0.31101929304998,"#C8D6E8"],[0.311320570242314,"#C8D6E8"],[0.311343520355379,"#C8D6E8"],[0.31213263387203,"#C8D6E7"],[0.312370717395998,"#C8D6E7"],[0.312951214178658,"#C8D6E7"],[0.312954055685336,"#C8D6E7"],[0.313606495209662,"#C8D6E7"],[0.314699345000853,"#C8D6E7"],[0.314743556525445,"#C8D6E7"],[0.315178280406239,"#C8D6E7"],[0.316104800016668,"#C8D6E7"],[0.317245126502787,"#C7D6E7"],[0.317682562327846,"#C7D6E7"],[0.31789874863817,"#C7D5E7"],[0.318093211229571,"#C7D5E7"],[0.319123016290713,"#C7D5E7"],[0.319934009507136,"#C7D5E7"],[0.320545544036915,"#C7D5E7"],[0.320618385335483,"#C7D5E7"],[0.320699910264749,"#C7D5E7"],[0.322332346737827,"#C6D5E7"],[0.322801309287644,"#C6D5E7"],[0.324390073209634,"#C6D5E7"],[0.326846954447621,"#C6D4E6"],[0.327209086858148,"#C6D4E6"],[0.328131376529774,"#C5D4E6"],[0.329581149375875,"#C5D4E6"],[0.330115146710938,"#C5D4E6"],[0.330282234256818,"#C5D4E6"],[0.331802718538965,"#C5D4E6"],[0.332141515456488,"#C5D4E6"],[0.33330184587461,"#C5D4E6"],[0.333786748393593,"#C4D3E6"],[0.335053270250239,"#C4D3E6"],[0.335610497581969,"#C4D3E6"],[0.337495512052763,"#C4D3E6"],[0.337703436321914,"#C4D3E6"],[0.337980745168127,"#C4D3E6"],[0.338828324454686,"#C4D3E5"],[0.339053899616073,"#C4D3E5"],[0.339562661853175,"#C3D3E5"],[0.339959916430325,"#C3D3E5"],[0.340794678918198,"#C3D3E5"],[0.341081228193309,"#C3D3E5"],[0.341153969423913,"#C3D3E5"],[0.341197441012311,"#C3D3E5"],[0.342866604088463,"#C3D2E5"],[0.343706764694405,"#C3D2E5"],[0.344999831830161,"#C2D2E5"],[0.348202060655944,"#C2D2E5"],[0.348786212597456,"#C2D2E5"],[0.349030969366242,"#C2D2E5"],[0.349134242556448,"#C2D1E5"],[0.351840089090044,"#C1D1E4"],[0.353904165576527,"#C1D1E4"],[0.35417025219054,"#C1D1E4"],[0.355863787001001,"#C1D1E4"],[0.356011909087942,"#C1D1E4"],[0.356287650664918,"#C1D1E4"],[0.356759241020351,"#C0D1E4"],[0.357107293449337,"#C0D0E4"],[0.358006224629888,"#C0D0E4"],[0.358994685876169,"#C0D0E4"],[0.359846926525465,"#C0D0E4"],[0.36076998894196,"#C0D0E4"],[0.362121366980957,"#BFD0E4"],[0.362257843614771,"#BFD0E4"],[0.362578821909155,"#BFD0E4"],[0.363004274481218,"#BFD0E4"],[0.364391322616497,"#BFD0E4"],[0.364490092067758,"#BFD0E4"],[0.364511861655225,"#BFD0E4"],[0.365120046397277,"#BFCFE3"],[0.365148563004107,"#BFCFE3"],[0.367845044855216,"#BECFE3"],[0.367956662628431,"#BECFE3"],[0.367986258779345,"#BECFE3"],[0.368205600490364,"#BECFE3"],[0.369081730148144,"#BECFE3"],[0.370583621893137,"#BECFE3"],[0.372424320572597,"#BECFE3"],[0.372532255851347,"#BECFE3"],[0.372776786322302,"#BECEE3"],[0.373166858215777,"#BECEE3"],[0.37477470033647,"#BDCEE3"],[0.37514703752149,"#BDCEE3"],[0.378140608759837,"#BDCEE2"],[0.378464825474443,"#BDCEE2"],[0.379210313258145,"#BCCEE2"],[0.379785545164435,"#BCCEE2"],[0.379791076656107,"#BCCEE2"],[0.381821151338526,"#BCCDE2"],[0.383163170137528,"#BCCDE2"],[0.383914756194739,"#BCCDE2"],[0.384508169358361,"#BCCDE2"],[0.385646302856449,"#BBCDE2"],[0.386250050315272,"#BBCDE2"],[0.387598358738273,"#BBCDE2"],[0.38807934594172,"#BBCDE2"],[0.390070577059879,"#BBCCE2"],[0.39031391894895,"#BBCCE2"],[0.391236803291682,"#BACCE2"],[0.393820035856853,"#BACCE1"],[0.394175441316335,"#BACCE1"],[0.3944009971967,"#BACCE1"],[0.394715693081175,"#BACCE1"],[0.394931880975646,"#BACCE1"],[0.396219679325111,"#B9CCE1"],[0.398494436853402,"#B9CBE1"],[0.399982854573331,"#B9CBE1"],[0.400084526685049,"#B9CBE1"],[0.400981244996785,"#B9CBE1"],[0.400996000801793,"#B9CBE1"],[0.402742452910529,"#B8CBE1"],[0.4041503423676,"#B8CAE1"],[0.4072915006489,"#B8CAE0"],[0.407496935094448,"#B8CAE0"],[0.410555182136466,"#B7CAE0"],[0.411973767065961,"#B7CAE0"],[0.412763322985067,"#B7C9E0"],[0.413429917941833,"#B6C9E0"],[0.417984175942974,"#B6C9E0"],[0.417984633429029,"#B6C9E0"],[0.423645456384521,"#B5C8DF"],[0.425857958457206,"#B4C8DF"],[0.427361436357013,"#B4C8DF"],[0.430679318151484,"#B3C7DF"],[0.43358295078163,"#B3C7DE"],[0.439327571939316,"#B2C6DE"],[0.440226005938578,"#B2C6DE"],[0.457740280273799,"#AFC4DD"],[0.46898840101964,"#ADC2DC"],[0.474290569209267,"#ACC2DB"],[0.50226927436474,"#A7BED9"],[0.57141150512435,"#9AB6D4"],[0.584412479459981,"#98B4D3"],[1,"#4682B4"]],"type":"heatmap","showscale":false,"autocolorscale":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","name":[],"frame":null},{"x":[0.4,24.6],"y":[0.4,24.6],"name":"99_44cbb6d91617f30d6a1c5050b341a8e9","type":"scatter","mode":"markers","opacity":0,"hoverinfo":"none","showlegend":false,"marker":{"color":[0,1],"colorscale":[[0,"#F7F9FB"],[0.0588235294117648,"#EDF2F7"],[0.117647058823529,"#E4EBF3"],[0.176470588235294,"#DAE4EF"],[0.235294117647059,"#D1DCEB"],[0.294117647058824,"#C7D6E7"],[0.352941176470588,"#BECFE3"],[0.411764705882353,"#B4C8DF"],[0.470588235294118,"#ABC1DB"],[0.529411764705882,"#A1BAD7"],[0.588235294117647,"#97B3D3"],[0.647058823529412,"#8DADCF"],[0.705882352941177,"#83A6CB"],[0.764705882352941,"#799FC7"],[0.823529411764706,"#6F99C2"],[0.882352941176471,"#6492BE"],[0.941176470588236,"#598CBA"],[1,"#4D86B6"]],"colorbar":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"thickness":23.04,"title":"ave_tip","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"tickmode":"array","ticktext":["0.10","0.15","0.20","0.25"],"tickvals":[0.117647058823529,0.411764705882353,0.705882352941177,1],"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"ticklen":2,"len":0.5}},"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":87.1525845003701,"l":136.62100456621},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[0.4,24.6],"ticktext":["Battery Park","Central Park","Chelsea","Chinatown","Clinton","East Harlem","East Village","Financial District","Garment District","Gramercy","Greenwich Village","Hamilton Heights","Harlem","Inwood","Little Italy","Lower East Side","Midtown","Morningside Heights","Murray Hill","Tribeca","Upper East Side","Upper West Side","Washington Heights","West Village"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"pickup_nhood","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[0.4,24.6],"ticktext":["Battery Park","Central Park","Chelsea","Chinatown","Clinton","East Harlem","East Village","Financial District","Garment District","Gramercy","Greenwich Village","Hamilton Heights","Harlem","Inwood","Little Italy","Lower East Side","Midtown","Morningside Heights","Murray Hill","Tribeca","Upper East Side","Upper West Side","Washington Heights","West Village"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"dropoff_nhood","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","dragmode":"zoom"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"modeBarButtonsToRemove":["sendDataToCloud"]},"source":"A","attrs":{"4e7e4dc71023":{"fill":{},"x":{},"y":{},"type":"heatmap"}},"cur_data":"4e7e4dc71023","visdat":{"4e7e4dc71023":["function (y) ","x"]},"highlight":{"on":"plotly_selected","off":"plotly_relayout","persistent":false,"dynamic":false,"color":null,"selectize":false,"defaultValues":null,"opacityDim":0.2,"hoverinfo":null,"showInLegend":false},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) {\n    var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_selected\",\"off\":\"plotly_relayout\",\"persistent\":false,\"dynamic\":false,\"color\":{},\"selectize\":false,\"defaultValues\":{},\"opacityDim\":0.2,\"hoverinfo\":{},\"showInLegend\":false});\n  }","data":null}]}}</script><!--/html_preserve-->

# Split and Combining Operations with doXdf

## Custom functions across groups

The `do` verb is an exception to the rule that dplyrXdf verbs write their output as xdf files. This is because do executes arbitrary R code, and can return arbitrary R objects; while a data frame is capable of storing these objects, an xdf file is limited to character and numeric vectors only.

## Custom functions across groups

The doXdf verb is similar to do, but where do splits its input into one data frame per group, doXdf splits it into one xdf file per group. This allows do-like functionality with grouped data, where each group can be arbitrarily large. The syntax for the two functions is essentially the same, although the code passed to doXdf must obviously know how to handle xdfs.

---


```r
taxi_models <- taxi_xdf %>% group_by(pickup_dow) %>% doXdf(model = rxLinMod(tip_amount ~ fare_amount, data = .))
```


```r
taxi_models
```

```
## Source: local data frame [7 x 2]
## Groups: <by row>
## 
## # A tibble: 7 × 2
##   pickup_dow          model
## *     <fctr>         <list>
## 1        Fri <S3: rxLinMod>
## 2        Mon <S3: rxLinMod>
## 3        Sat <S3: rxLinMod>
## 4        Sun <S3: rxLinMod>
## 5        Thu <S3: rxLinMod>
## 6        Tue <S3: rxLinMod>
## 7        Wed <S3: rxLinMod>
```

```r
taxi_models$model[[1]]
```

```
## Call:
## rxLinMod(formula = tip_amount ~ fare_amount, data = .)
## 
## Linear Regression Results for: tip_amount ~ fare_amount
## Data: . (RxXdfData Data Source)
## File name: /tmp/RtmptR5PAR/tripdata_2015.pickup_dow.Fri.xdf
## Dependent variable(s): tip_amount
## Total independent variables: 2 
## Number of valid observations: 2030949
## Number of missing observations: 0 
##  
## Coefficients:
##             tip_amount
## (Intercept)  0.1226685
## fare_amount  0.1260427
```


## Memory Issues

All the caveats that go with working with `data.frames` apply here. While each grouped partition is it's own `RxXdfData` object, the return value must be a `data.frame`, and hence, must fit in memory.
Moreover, the function you apply against the splits will determine how they are operated. If you use an `rx` function, you'll get the nice fault-tolerant, parallel execution strategies the `RevoScaleR` package provides, but for any vanilla/CRAN function will work with data.frames and can easily cause your session to crash.

---


```r
library(broom)
taxi_broom <- taxi_xdf %>% group_by(pickup_dow) %>% doXdf(model = lm(tip_amount ~ fare_amount, data = .))
```
Now we can apply the `broom::tidy` function at the row level to get summary statistics:


```r
library(broom)
tbl_df(taxi_broom) %>% tidy(model)
```

```
## Source: local data frame [14 x 6]
## Groups: pickup_dow [7]
## 
##    pickup_dow        term   estimate    std.error statistic       p.value
##        <fctr>       <chr>      <dbl>        <dbl>     <dbl>         <dbl>
## 1         Sun (Intercept) 0.03730041 0.0024937318  14.95767  1.397698e-50
## 2         Sun fare_amount 0.12631622 0.0001514979 833.78190  0.000000e+00
## 3         Mon (Intercept) 0.02654646 0.0023435719  11.32735  9.629607e-30
## 4         Mon fare_amount 0.13543983 0.0001409857 960.66361  0.000000e+00
## 5         Tue (Intercept) 0.06650145 0.0022397638  29.69128 1.103022e-193
## 6         Tue fare_amount 0.13526525 0.0001384011 977.34249  0.000000e+00
## 7         Wed (Intercept) 0.03476736 0.0023254903  14.95055  1.554454e-50
## 8         Wed fare_amount 0.13861577 0.0001423052 974.07407  0.000000e+00
## 9         Thu (Intercept) 0.06643491 0.0024278106  27.36413 7.858925e-165
## 10        Thu fare_amount 0.13565675 0.0001462961 927.27526  0.000000e+00
## 11        Fri (Intercept) 0.12266850 0.0023882664  51.36299  0.000000e+00
## 12        Fri fare_amount 0.12604272 0.0001445679 871.85803  0.000000e+00
## 13        Sat (Intercept) 0.17156504 0.0020854509  82.26760  0.000000e+00
## 14        Sat fare_amount 0.11094274 0.0001353946 819.40287  0.000000e+00
```

