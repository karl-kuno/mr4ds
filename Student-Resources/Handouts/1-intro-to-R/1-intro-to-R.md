# Introduction to R: _Functional, Object-Based Computing for Data Analysis_
[Ali Zaidi, alizaidi@microsoft.com](mailto:alizaidi@microsoft.com)  
`r format(Sys.Date(), "%B %d, %Y")`  



# Course Logistics

## Day One

### R U Ready?

* Overview of The R Project for Statistical Computing
* The Microsoft R Family
* R's capabilities and it's limitations
* What types of problems R might be useful for
* How to manage data with the exceptionally popular open source package `dplyr`
* How to develop models and write functions in R

## Day Two 

### Scalable Data Analysis with Microsoft R

* Moving the compute to your data
* WODA - Write Once, Deploy Anywhere
* High Performance Analytics
* High Performance Computing
* Machine Learning with Microsoft R

## Day Three

### Distributing Computing on Spark Clusters with R

* Overview of the Apache Spark Project
* Taming the Hadoop Zoo with HDInsight
* Provisioing and Managing HDInsight Clusters
* Spark DataFrames, `SparkR`, and the `sparklyr` package
* Developing Machine Learning Pipelines with `Spark` and Microsoft R

## Prerequisites
### Computing Environments

* R Server 8.0.5 or above (most recent version is 9.0.2)
* Azure Credits
* Can use the [Linux DSVM](https://azure.microsoft.com/en-us/documentation/articles/machine-learning-data-science-linux-dsvm-intro/)
* Or the [Windows DSVM](https://azure.microsoft.com/en-us/documentation/articles/machine-learning-data-science-provision-vm/)
* For IDE: choises are [RTVS](https://www.visualstudio.com/vs/rtvs/), [RStudio Server](https://www.rstudio.com/products/rstudio/download3/), [JupyterHub](https://jupyterhub.readthedocs.io/en/latest/), [JupyterLab](http://jupyterlab-tutorial.readthedocs.io/en/latest/)... 
    + Whatever you're comfortable with!

## Development Environments 
### Where to Write R Code

* The most popular integrated development environment for R is [RStudio](https://www.rstudio.com/)
* The RStudio IDE is entirely html/javascript based, so completely cross-platform
* RStudio Server provides a full IDE in your browser: perfect for cloud instances
* For Windows machines, 2016 provided general availability of [R Tools for Visual Studio, RTVS](https://www.visualstudio.com/en-us/features/rtvs-vs.aspx)
* RTVS supports connectivity to Azure and SQL Server for remote connectivity


## What is R? 
### Why should I care?

* R is the successor to the S Language, originated at Bell Labs AT&T
* It is based on the Scheme interpreter
* Originally designed by two University of Auckland Professors for their introductory statistics course

![Robert Gentleman and Ross Ihaka](http://revolution-computing.typepad.com/.a/6a010534b1db25970b016766fdae38970b-800wi)

## R's Philosophy 
### What R Thou?

R follows the [Unix philosophy](http://en.wikipedia.org/wiki/Unix_philosophy)

* Write programs that do one thing and do it well (modularity)
* Write programs that work together (cohesiveness)
* R is extensible with more than 10,000 packages available at CRAN (http://crantastic.org/packages)


## The aRt of Being Lazy
### Lazy Evaluation in R

![](http://i.imgur.com/5GbW690.gif)


* R, like it's inspiration, Scheme, is a _functional_ programming language
* R evaluates lazily, delaying evaluation until necessary, which can make it very flexible
* R is a highly interpreted dynamically typed language, allowing you to mutate variables and analyze datasets quickly, but is significantly slower than low-level, statically typed languages like C or Java
* R has a high memory footprint, and can easily lead to crashes if you aren't careful

## R's Programming Paradigm
### Keys to R

<span class="fragment">Everything that exist in R is an *object*</span>
<br>
<span class="fragment">Everything that happens in R is a *function call*</span>
<br>
<span class="fragment">R was born to *interface*</span>
<br>

<span class="fragment">_—John Chambers_</span>

## Strengths of R 
### Where R Succeeds

* Expressive
* Open source 
* Extendable -- nearly 10,000 packages with functions to use, and that list continues to grow
* Focused on statistics and machine learning -- cutting-edge algorithms and powerful data manipulation packages
* Advanced data structures and graphical capabilities
* Large user community, both within academia and industry
* It is designed by statisticians 


## Weaknesses of R 
### Where R Falls Short

* It is designed by statisticians
* Inefficient at element-by-element computations
* May make large demands on system resources, namely memory
* Data capacity limited by memory
* Single-threaded

## Distributions of R

![](images/distros_r.png)

## Some Essential Open Source Packages

* There are over 10,000 R packages to choose from, what do I start with?
* Data Management: `dplyr`, `tidyr`, `data.table`
* Visualization: `ggplot2`, `ggvis`, `htmlwidgets`, `shiny`
* Data Importing: `haven`, `RODBC`, `readr`, `foreign`
* Other favorites: `magrittr`, `rmarkdown`, `caret`

# R Foundations

## Command line prompts

Symbol | Meaning
------ | -------
 `<-`   | assignment operator
  `>`   | ready for a new command
  `+`   | awaiting the completion of an existing command 
  `?`   | get help for following function
  
Can change options either permanently at startup (see `?Startup`) or manually at each session with the `options` function, `options(repos = " ")` for example.

Check your CRAN mirror with `getOption("repos")`.

## I'm Lost! 
### Getting Help for R

* [Stack Overflow](http://stackoverflow.com/questions/tagged/r)
* [R Reference Card](https://cran.r-project.org/doc/contrib/Short-refcard.pdf)
* [RStudio Cheat Sheets](https://www.rstudio.com/resources/cheatsheets/)
* [R help mailing list and archives](https://stat.ethz.ch/mailman/listinfo/r-help)
* [Revolutions Blog](http://blog.revolutionanalytics.com/)
* [R-Bloggers](http://www.r-bloggers.com/)
* [RSeek](rseek.org)
* [RDocumentation](https://www.rdocumentation.org/)

## Quick Tour of Things You Need to Know
### Data Structures

> "Bad programmers worry about the code. 
> Good programmers worry about data structures and their relationships."
> - Linus Torvalds

* R's data structures can be described by their dimensionality, and their type.


|    | Homogeneous   | Heterogeneous |
|----|---------------|---------------|
| 1d | Atomic vector | List          |
| 2d | Matrix        | Data frame    |
| nd | Array         |               |

## Quick Tour of Things You Need to Know 

### Data Types

* Atomic vectors come in one of four types
* `logical` (boolean). Values: `TRUE` | `FALSE`
* `integer`
* `double` (often called numeric)
* `character`
* Rare types:
* `complex` 
* `raw`

## Manipulating Data Structures

### Subsetting Operators

* To create a vector, use `c`: `c(1, 4, 1, 3)`
* To create a list, use `list`: `list(1, 'hi', data.frame(1:10, letters[1:10]))`
* To subset a vector or list, use the `[ ]`
  - inside the brackets: 
    + positive integer vectors for indices you want 
    + negative integer vectors for indices you want to drop
    + logical vectors for indices you want to keep/drop (TRUE/FALSE)
    + character vectors for named vectors corresponding to which named elements you want to keep
    + subsetting a list with a single square bracket always returns a list
+ To subset a list and get back just that element, use `[[ ]]`

### Object Representation

+ To find the type of an object, use `class` (_higher level representation_)
+ To find how the object is stored in memory, use `typeof` (_lower level representation_)
+ Good time to do Lab 1!

# Data Manipulation with the `dplyr` Package

## Overview

Rather than describing the nitty gritty details of writing R code, I'd like you to get started at immediately writing R code.

As most of you are data scientists/data enthusiasts, I will showcase one of the most useful data manipulation packages in R, `dplyr`.
At the end of this session, you will have learned:

* How to manipulate data quickly with `dplyr` using a very intuitive _"grammar"_
* How to use `dplyr` to perform common exploratory analysis data manipulation procedures
* How to apply your own custom functions to group manipulations `dplyr` with `mutate()`, `summarise()` and `do()`
* Connect to remote databases to work with larger than memory datasets

## Why use dplyr? 
### The Grammar of Data Manipulation

* `dplyr` is currently the [most downloaded package](https://www.rdocumentation.org/packages/dplyr/versions/0.5.0?) from CRAN
* `dplyr` makes data manipulation easier by providing a few functions for the most common tasks and procedures
* `dplyr` achieves remarkable speed-up gains by using a C++ backend
* `dplyr` has multiple backends for working with data stored in various sources: SQLite, MySQL, bigquery, SQL Server, and many more
* `dplyr` was inspired to give data manipulation a simple, cohesive grammar (similar philosophy to `ggplot` - grammar of graphics)
* `dplyr` has inspired many new packages, which now adopt it's easy to understand syntax. 
* The recent packages `dplyrXdf` and `SparkR/sparklyr` brings much of the same functionality of `dplyr` to `XDF`s data and Spark `DataFrames`


## Tidy Data and Happier Coding
### Premature Optimization 

![](https://imgs.xkcd.com/comics/the_general_problem.png)

+ For a dats scientist, the most important parameter to optimize in a data science development cycle is YOUR time
+ It is therefore important to be able to write efficient code, quickly
+ Goals: writing fast code that is: portable, platform invariant, easy to understand, and easy to debug
    - __Be serious about CReUse__!

## Manipulation verbs

`filter`

:    select rows based on matching criteria

`slice`

:    select rows by number

`select`

:    select columns by column names

`arrange`

:    reorder rows by column values

`mutate`

:    add new variables based on transformations of existing variables

`transmute`

:    transform and drop other variables



## Aggregation verbs

`group_by`

:    identify grouping variables for calculating groupwise summary statistics


`count`

:    count the number of records per group


`summarise` | `summarize`

:    calculate one or more summary functions per group, returning one row of results per group (or one for the entire dataset)

## NYC Taxi Data
### Data for Class

* The data we will be examining in this module is dervided from the [NYC Taxi and Limousine Commission](http://www.nyc.gov/html/tlc/html/home/home.shtml)
* Data contains taxi trips in NYC, and includes spatial features (pickup and dropoff neighborhoods), temporal features, and monetary features (fare and tip amounts)
* The dataset for this module is saved as an _rds_ file in a public facing Azure storage blob
* An _rds_ file is a compressed, serialized R object
* Save an object to _rds_ by using the `saveRDS` function; read an _rds_ object with the `readRDS` object

## Viewing Data
### tibble

* `dplyr` includes a wrapper called `tbl_df` that adds an additional class attribute onto `data.frames` that provides some better data manipulation aesthetics (there's now a dedicated package [`tibble`](www.github.com/hadley/tibble) for this wrapper and it's class)
* Most noticeable differential between `tbl_df` and `data.frame`s is the console output: `tbl_df`s will only print what the current R console window can display
* Can change the default setting for number of displayed columns by changing the options parameter: `options(dplyr.width = Inf)` 


```r
library(dplyr)
library(stringr)
taxi_url <- "http://alizaidi.blob.core.windows.net/training/trainingData/manhattan_df.rds"
taxi_df  <- readRDS(gzcon(url(taxi_url)))
(taxi_df <- tbl_df(taxi_df))
```

```
## # A tibble: 5,000,000 × 21
##    VendorID passenger_count trip_distance pickup_longitude pickup_latitude
##       <int>           <int>         <dbl>            <dbl>           <dbl>
## 1         1               1          1.40        -74.00555        40.72562
## 2         2               2          4.58        -73.98419        40.73022
## 3         2               5          9.63        -73.97183        40.76273
## 4         1               1          1.70        -73.96131        40.77190
## 5         1               2          0.60        -73.99568        40.72430
## 6         1               1          4.10        -73.95731        40.76214
## 7         2               5          1.76        -73.99208        40.72919
## 8         2               1          2.68        -73.95257        40.78289
## 9         2               1          1.69        -73.94909        40.78134
## 10        2               1          1.16        -73.98251        40.76832
## # ... with 4,999,990 more rows, and 16 more variables: RatecodeID <int>,
## #   store_and_fwd_flag <chr>, dropoff_longitude <dbl>,
## #   dropoff_latitude <dbl>, payment_type <int>, fare_amount <dbl>,
## #   tip_amount <dbl>, tolls_amount <dbl>, tip_percent <dbl>,
## #   pickup_hour <fctr>, pickup_dow <fctr>, dropoff_hour <fctr>,
## #   dropoff_dow <fctr>, trip_duration <int>, pickup_nhood <fctr>,
## #   dropoff_nhood <fctr>
```

# Filtering and Reordering Data

## Subsetting Data

* `dplyr` makes subsetting by rows very easy
* The `filter` verb takes conditions for filtering rows based on conditions
* **every** `dplyr` function uses a data.frame/tbl as it's first argument
* Additional conditions are passed as new arguments (no need to make an insanely complicated expression, split em up!)

## Filter


```r
filter(taxi_df,
       dropoff_dow %in% c("Fri", "Sat", "Sun"),
       tip_amount > 1)
```

```
## # A tibble: 1,120,381 × 21
##    VendorID passenger_count trip_distance pickup_longitude pickup_latitude
##       <int>           <int>         <dbl>            <dbl>           <dbl>
## 1         2               5          1.76        -73.99208        40.72919
## 2         1               1          3.20        -73.98829        40.76429
## 3         2               3          1.86        -73.97189        40.75711
## 4         1               2         17.30        -73.78173        40.64448
## 5         1               1          1.30        -73.94249        40.79044
## 6         1               1          2.80        -73.96978        40.75972
## 7         2               1          4.65        -73.98398        40.73781
## 8         1               2          1.50        -73.96262        40.77548
## 9         2               2          0.83        -73.97236        40.78106
## 10        2               6         11.32        -73.98249        40.76269
## # ... with 1,120,371 more rows, and 16 more variables: RatecodeID <int>,
## #   store_and_fwd_flag <chr>, dropoff_longitude <dbl>,
## #   dropoff_latitude <dbl>, payment_type <int>, fare_amount <dbl>,
## #   tip_amount <dbl>, tolls_amount <dbl>, tip_percent <dbl>,
## #   pickup_hour <fctr>, pickup_dow <fctr>, dropoff_hour <fctr>,
## #   dropoff_dow <fctr>, trip_duration <int>, pickup_nhood <fctr>,
## #   dropoff_nhood <fctr>
```

## Exercise

Your turn: 

* How many observations started in Harlem?
  - pick both sides of Harlem, including east harlem
  - *hint*: it might be useful to use the `str_detect` function from `stringr`
* How many observations that started in Harlem ended in the Financial District?

## Solution


```r
library(stringr)
table(taxi_df$pickup_nhood)
```

```
## 
##                              Annadale 
##                                     0 
##                         Arden Heights 
##                                     0 
##                              Arrochar 
##                                     3 
##                               Arverne 
##                                     6 
##                               Astoria 
##                                 23432 
##                       Astoria Heights 
##                                   552 
##                            Auburndale 
##                                    16 
##                            Bath Beach 
##                                    46 
##                          Battery Park 
##                                 50258 
##                             Bay Ridge 
##                                   154 
##                           Bay Terrace 
##                                     7 
##                            Baychester 
##                                     4 
##                               Bayside 
##                                    25 
##                          Bedford Park 
##                                    48 
##                    Bedford Stuyvesant 
##                                  4560 
##                          Belle Harbor 
##                                     1 
##                             Bellerose 
##                                    23 
##                               Belmont 
##                                    33 
##                           Bensonhurst 
##                                    70 
##                          Bergen Beach 
##                                     5 
##                            Blissville 
##                                   132 
##                            Bloomfield 
##                                     3 
##                           Boerum Hill 
##                                  3734 
##                          Borough Park 
##                                    60 
##                          Breezy Point 
##                                     2 
##                             Briarwood 
##                                   285 
##                        Brighton Beach 
##                                    34 
##                         Broad Channel 
##                                     2 
##                            Bronx Park 
##                                    14 
##                      Brooklyn Heights 
##                                  5876 
##                           Brownsville 
##                                    78 
##                            Bulls Head 
##                                     3 
##                              Bushwick 
##                                  1877 
##                          Butler Manor 
##                                     0 
##                       Cambria Heights 
##                                    12 
##                              Canarsie 
##                                   102 
##                         Carnegie Hill 
##                                 56133 
##                       Carroll Gardens 
##                                  3807 
##                           Castle Hill 
##                                    19 
##                     Castleton Corners 
##                                     2 
##                          Central Park 
##                                 68716 
##                            Charleston 
##                                     0 
##                               Chelsea 
##                                237331 
##                        Chelsea-Travis 
##                                     1 
##                             Chinatown 
##                                 16261 
##                           City Island 
##                                     5 
##                          Clason Point 
##                                     0 
##                               Clifton 
##                                     8 
##                               Clinton 
##                                154194 
##                          Clinton Hill 
##                                  2007 
##                            Clove Lake 
##                                     0 
##                            Co-op City 
##                                    20 
##                           Cobble Hill 
##                                  2122 
##                         College Point 
##                                    55 
##   Columbia Street Waterfront District 
##                                   286 
##                       Columbus Circle 
##                                 80557 
##                             Concourse 
##                                   862 
##                          Coney Island 
##                                    20 
##                                Corona 
##                                   434 
##                          Country Club 
##                                     3 
##                         Crown Heights 
##                                  2001 
##                          Dongan Hills 
##                                     1 
##                Douglaston-Little Neck 
##                                     7 
##                              Downtown 
##                                  9444 
##                                 DUMBO 
##                                  1720 
##                         Dyker Heights 
##                                    32 
##                         East Elmhurst 
##                                  2887 
##                         East Flatbush 
##                                   278 
##                           East Harlem 
##                                 49635 
##              East Jamaica Bay Islands 
##                                     1 
##                         East New York 
##                                   287 
##                          East Tremont 
##                                    54 
##                          East Village 
##                                180604 
##                           Eastchester 
##                                     5 
##                              Elm Park 
##                                     0 
##                              Elmhurst 
##                                  2244 
##                           Eltingville 
##                                     1 
##                          Emerson Hill 
##                                     1 
##                          Far Rockaway 
##                                    24 
##                             Fieldston 
##                                    11 
##                    Financial District 
##                                 96110 
##                              Flatbush 
##                                   896 
##                     Flatiron District 
##                                319998 
##                             Flatlands 
##                                    85 
##                           Floral park 
##                                    13 
##                   Floyd Bennett Field 
##                                     6 
##                              Flushing 
##                                   341 
##          Flushing Meadows Corona Park 
##                                   280 
##                               Fordham 
##                                    67 
##                          Forest Hills 
##                                   997 
##                  Forest Hills Gardens 
##                                     8 
##                           Fort Greene 
##                                  5164 
##                         Fort Hamilton 
##                                    86 
##                           Fort Tilden 
##                                     0 
##                        Fort Wadsworth 
##                                     3 
##                           Fresh Kills 
##                                     0 
##                      Fresh Kills Park 
##                                     0 
##                         Fresh Meadows 
##                                    29 
##                      Garment District 
##                                296479 
##                            Georgetown 
##                                     6 
##                       Gerritsen Beach 
##                                     1 
##                             Glen Oaks 
##                                     5 
##                              Glendale 
##                                    42 
##                      Governors Island 
##                                     1 
##                               Gowanus 
##                                  2226 
##                              Gramercy 
##                                170535 
##                          Graniteville 
##                                     0 
##                            Grant City 
##                                     0 
##                    Grasmere - Concord 
##                                     3 
##                             Gravesend 
##                                    59 
##                           Great Kills 
##                                     1 
##                            Greenpoint 
##                                  4106 
##                            Greenridge 
##                                     0 
##                     Greenwich Village 
##                                210108 
##                             Greenwood 
##                                   647 
##                           Grymes Hill 
##                                     2 
##                      Hamilton Heights 
##                                  6657 
##                                Harlem 
##                                 22344 
##                     Heartland Village 
##                                     1 
##                           High Bridge 
##                                   153 
##                             Hillcrest 
##                                    43 
##                                Hollis 
##                                    93 
##                            Holliswood 
##                                     5 
##                             Homecrest 
##                                    40 
##                          Howard Beach 
##                                    29 
##                              Huguenot 
##                                     0 
##                         Hunters Point 
##                                  7163 
##                           Hunts Point 
##                                    11 
##                                Inwood 
##                                   438 
##                       Jackson Heights 
##                                  1478 
##                       Jacob Riis Park 
##                                     0 
##                               Jamaica 
##                                  1386 
##                       Jamaica Estates 
##                                    22 
##                         Jamaica Hills 
##                                    64 
## John F. Kennedy International Airport 
##                                111064 
##                            Kensington 
##                                   447 
##                           Kew Gardens 
##                                   250 
##                     Kew Gardens Hills 
##                                   102 
##                           Kingsbridge 
##                                   106 
##                    La Guardia Airport 
##                                127232 
##                      La Tourette Park 
##                                     0 
##                             Laurelton 
##                                    25 
##                        Liberty Island 
##                                     0 
##                       Lighthouse Hill 
##                                     0 
##                          Little Italy 
##                                 38778 
##                              Longwood 
##                                    67 
##                       Lower East Side 
##                                103048 
##                                 Malba 
##                                     1 
##                       Manhattan Beach 
##                                     6 
##                        Manhattanville 
##                                  3181 
##                           Marble Hill 
##                                    54 
##                           Marine Park 
##                                    12 
##                      Mariner's Harbor 
##                                     0 
##                               Maspeth 
##                                   801 
##                        Meiers Corners 
##                                     0 
##                               Melrose 
##                                   197 
##                        Middle Village 
##                                    76 
##                         Midland Beach 
##                                     3 
##                               Midtown 
##                                575401 
##                               Midwood 
##                                    82 
##                            Mill Basin 
##                                    11 
##                        Mill Rock Park 
##                                     0 
##                   Morningside Heights 
##                                 23385 
##                        Morris Heights 
##                                   106 
##                           Morris Park 
##                                    46 
##                            Mott Haven 
##                                   911 
##                           Murray Hill 
##                                125374 
##                             Navy Yard 
##                                    72 
##                              Neponsit 
##                                     0 
##                          New Brighton 
##                                    37 
##                              New Dorp 
##                                     0 
##                        New Dorp Beach 
##                                     0 
##                       New Springville 
##                                     1 
##                           New Utrecht 
##                                    20 
##                                  NoHo 
##                                 46345 
##                       North Riverdale 
##                                    20 
##                               Norwood 
##                                    43 
##                       Oakland Gardens 
##                                     8 
##                               Oakwood 
##                                     3 
##                            Ocean Hill 
##                                   115 
##                         Ocean Parkway 
##                                   167 
##                              Old Town 
##                                     0 
##                            Ozone Park 
##                                   124 
##                             Park Hill 
##                                     2 
##                            Park Slope 
##                                  6947 
##                           Parkchester 
##                                    82 
##                            Pelham Bay 
##                                    23 
##                       Pelham Bay Park 
##                                     2 
##                        Pelham Gardens 
##                                    21 
##                       Pleasant Plains 
##                                     0 
##                               Pomonok 
##                                     8 
##                            Port Ivory 
##                                     1 
##                           Port Morris 
##                                   296 
##                         Port Richmond 
##                                     2 
##                          Prince's Bay 
##                                     1 
##                      Prospect Heights 
##                                  2594 
##             Prospect Lefferts Gardens 
##                                   638 
##                         Prospect Park 
##                                   157 
##                   Prospect Park South 
##                                    37 
##                        Queens Village 
##                                    69 
##                         Randall Manor 
##                                     1 
##                  Randals-Wards Island 
##                                   327 
##                              Red Hook 
##                                   445 
##                             Rego Park 
##                                   510 
##                         Richmond Hill 
##                                   150 
##                         Richmond Town 
##                                     1 
##                       Richmond Valley 
##                                     0 
##                             Ridgewood 
##                                   248 
##                         Rikers Island 
##                                     3 
##                             Riverdale 
##                                    40 
##                              Rochdale 
##                                    44 
##                        Rockaway Beach 
##                                     0 
##                         Rockaway Park 
##                                     1 
##                      Roosevelt Island 
##                                   367 
##                              Rosebank 
##                                    13 
##                              Rosedale 
##                                    15 
##                             Rossville 
##                                     1 
##                               Roxbury 
##                                     0 
##                        Sheepshead Bay 
##                                    83 
##                           Shore Acres 
##                                     0 
##                           Silver Lake 
##                                     6 
##                                  SoHo 
##                                 87588 
##                             Soundview 
##                                   106 
##                           South Beach 
##                                     1 
##                           South Bronx 
##                                   168 
##                      South Ozone Park 
##                                   755 
##                   Springfield Gardens 
##                                    98 
##                        Spuyten Duyvil 
##                                    34 
##                            St. Albans 
##                                    17 
##                            St. George 
##                                     3 
##                             Stapleton 
##                                    25 
##                       Stuyvesant Town 
##                                  9438 
##                             Sunnyside 
##                                  8704 
##                           Sunset Park 
##                                   325 
##                          Sutton Place 
##                                 81233 
##                          Throggs Neck 
##                                    34 
##                             Todt Hill 
##                                     0 
##                         Tompkinsville 
##                                     6 
##                           Tottenville 
##                                     0 
##                               Tremont 
##                                    89 
##                               Tribeca 
##                                 97257 
##                            Tudor City 
##                                 49655 
##                            Turtle Bay 
##                                128532 
##                             Unionport 
##                                    50 
##                    University Heights 
##                                    82 
##                       Upper East Side 
##                                680256 
##                       Upper West Side 
##                                414113 
##                                Utopia 
##                                     1 
##                    Van Cortlandt Park 
##                                    10 
##                              Van Nest 
##                                    16 
##                          Vinegar Hill 
##                                    23 
##                             Wakefield 
##                                     9 
##                    Washington Heights 
##                                  6025 
##                         West Brighton 
##                                     5 
##                            West Farms 
##                                    34 
##              West Jamaica Bay Islands 
##                                     1 
##                          West Village 
##                                124493 
##                   Westchester Heights 
##                                    28 
##                           Westerleigh 
##                                     0 
##                            Whitestone 
##                                    24 
##                        Williamsbridge 
##                                    64 
##                          Williamsburg 
##                                 24258 
##                           Willowbrook 
##                                     0 
##                       Windsor Terrace 
##                                   178 
##                               Wingate 
##                                    71 
##                             Woodhaven 
##                                    64 
##                              Woodlawn 
##                                    21 
##                               Woodrow 
##                                     0 
##                              Woodside 
##                                  3482
```

```r
harlem_pickups <- filter(taxi_df, str_detect(pickup_nhood, "Harlem"))
harlem_pickups
```

```
## # A tibble: 71,979 × 21
##    VendorID passenger_count trip_distance pickup_longitude pickup_latitude
##       <int>           <int>         <dbl>            <dbl>           <dbl>
## 1         1               1          1.30        -73.94249        40.79044
## 2         1               1          4.70        -73.93792        40.81836
## 3         2               1          4.42        -73.93603        40.80380
## 4         2               1          5.89        -73.93388        40.80276
## 5         1               1          4.10        -73.94268        40.81198
## 6         2               3          3.20        -73.95286        40.78902
## 7         1               2          1.40        -73.93806        40.80351
## 8         1               2          1.10        -73.95318        40.79869
## 9         2               1          0.32        -73.95840        40.80272
## 10        2               1          1.81        -73.94090        40.80830
## # ... with 71,969 more rows, and 16 more variables: RatecodeID <int>,
## #   store_and_fwd_flag <chr>, dropoff_longitude <dbl>,
## #   dropoff_latitude <dbl>, payment_type <int>, fare_amount <dbl>,
## #   tip_amount <dbl>, tolls_amount <dbl>, tip_percent <dbl>,
## #   pickup_hour <fctr>, pickup_dow <fctr>, dropoff_hour <fctr>,
## #   dropoff_dow <fctr>, trip_duration <int>, pickup_nhood <fctr>,
## #   dropoff_nhood <fctr>
```

```r
# uncomment the line below (ctrl+shift+c) and filter harlem_pickups on Financial District
# how many rows?
# fidi <- filter(harlem_pickups, ...)
```

## Select a set of columns

* You can use the `select()` verb to specify which columns of a dataset you want
* This is similar to the `keep` option in SAS's data step.
* Use a colon `:` to select all the columns between two variables (inclusive)
* Use `contains` to take any columns containing a certain word/phrase/character

## Select Example


```r
select(taxi_df, pickup_nhood, dropoff_nhood, 
       fare_amount, dropoff_hour, trip_distance)
```

```
## # A tibble: 5,000,000 × 5
##       pickup_nhood      dropoff_nhood fare_amount dropoff_hour
##             <fctr>             <fctr>       <dbl>       <fctr>
## 1             SoHo       Battery Park         7.0     6PM-10PM
## 2     East Village Bedford Stuyvesant        18.0     6PM-10PM
## 3          Midtown           Downtown        34.0     10PM-1AM
## 4  Upper East Side        Murray Hill         9.0     12PM-4PM
## 5     Little Italy          Chinatown         4.5     6PM-10PM
## 6  Upper East Side  Greenwich Village        15.5     10PM-1AM
## 7             NoHo       West Village        10.0      1AM-5AM
## 8  Upper East Side    Upper West Side        13.0     12PM-4PM
## 9  Upper East Side        East Harlem         8.5     12PM-4PM
## 10 Upper West Side            Midtown         9.0      4PM-6PM
## # ... with 4,999,990 more rows, and 1 more variables: trip_distance <dbl>
```

## Select: Other Options

starts_with(x, ignore.case = FALSE)

:    name starts with `x`

ends_with(x, ignore.case = FALSE)

:    name ends with `x`

matches(x, ignore.case = FALSE)

:    selects all variables whose name matches the regular expression `x`

num_range("V", 1:5, width = 1)

:    selects all variables (numerically) from `V1` to `V5`.

* You can also use a `-` to drop variables.

## Reordering Data

* You can reorder your dataset based on conditions using the `arrange()` verb
* Use the `desc` function to sort in descending order rather than ascending order (default)

## Arrange


```r
select(arrange(taxi_df, desc(fare_amount), pickup_nhood), 
       fare_amount, pickup_nhood)
```

```
## # A tibble: 5,000,000 × 2
##    fare_amount                          pickup_nhood
##          <dbl>                                <fctr>
## 1      2020.37                         East New York
## 2       991.16                              Glendale
## 3       889.97                      Garment District
## 4       750.00                               Midwood
## 5       606.00                              Gramercy
## 6       600.00                            Tudor City
## 7       534.00                               Astoria
## 8       495.70                       Upper East Side
## 9       445.00 John F. Kennedy International Airport
## 10      420.00                       Upper East Side
## # ... with 4,999,990 more rows
```

```r
head(select(arrange(taxi_df, desc(fare_amount), pickup_nhood), 
       fare_amount, pickup_nhood), 10)
```

```
## # A tibble: 10 × 2
##    fare_amount                          pickup_nhood
##          <dbl>                                <fctr>
## 1      2020.37                         East New York
## 2       991.16                              Glendale
## 3       889.97                      Garment District
## 4       750.00                               Midwood
## 5       606.00                              Gramercy
## 6       600.00                            Tudor City
## 7       534.00                               Astoria
## 8       495.70                       Upper East Side
## 9       445.00 John F. Kennedy International Airport
## 10      420.00                       Upper East Side
```




## Exercise
Use `arrange()` to  sort on the basis of `tip_amount`, `dropoff_nhood`, and `pickup_dow`, with descending order for tip amount

## Summary

filter

:    Extract subsets of rows. See also `slice()`

select

:    Extract subsets of columns. See also `rename()`

arrange

:    Sort your data

# Data Aggregations and Transformations

## Transformations

* The `mutate()` verb can be used to make new columns


```r
taxi_df <- mutate(taxi_df, tip_pct = tip_amount/fare_amount)
select(taxi_df, tip_pct, fare_amount, tip_amount)
```

```
## # A tibble: 5,000,000 × 3
##      tip_pct fare_amount tip_amount
##        <dbl>       <dbl>      <dbl>
## 1  0.2928571         7.0       2.05
## 2  0.1388889        18.0       2.50
## 3  0.2076471        34.0       7.06
## 4  0.0000000         9.0       0.00
## 5  0.0000000         4.5       0.00
## 6  0.0000000        15.5       0.00
## 7  0.2260000        10.0       2.26
## 8  0.2276923        13.0       2.96
## 9  0.2188235         8.5       1.86
## 10 0.3000000         9.0       2.70
## # ... with 4,999,990 more rows
```

```r
transmute(taxi_df, tip_pct = tip_amount/fare_amount)
```

```
## # A tibble: 5,000,000 × 1
##      tip_pct
##        <dbl>
## 1  0.2928571
## 2  0.1388889
## 3  0.2076471
## 4  0.0000000
## 5  0.0000000
## 6  0.0000000
## 7  0.2260000
## 8  0.2276923
## 9  0.2188235
## 10 0.3000000
## # ... with 4,999,990 more rows
```

## Summarise Data by Groups

* The `group_by` verb creates a grouping by a categorical variable
* Functions can be placed inside `summarise` to create summary functions


```r
grouped_taxi <- group_by(taxi_df, dropoff_nhood)
class(grouped_taxi)
```

```
## [1] "grouped_df" "tbl_df"     "tbl"        "data.frame"
```

```r
grouped_taxi
```

```
## Source: local data frame [5,000,000 x 22]
## Groups: dropoff_nhood [260]
## 
##    VendorID passenger_count trip_distance pickup_longitude pickup_latitude
##       <int>           <int>         <dbl>            <dbl>           <dbl>
## 1         1               1          1.40        -74.00555        40.72562
## 2         2               2          4.58        -73.98419        40.73022
## 3         2               5          9.63        -73.97183        40.76273
## 4         1               1          1.70        -73.96131        40.77190
## 5         1               2          0.60        -73.99568        40.72430
## 6         1               1          4.10        -73.95731        40.76214
## 7         2               5          1.76        -73.99208        40.72919
## 8         2               1          2.68        -73.95257        40.78289
## 9         2               1          1.69        -73.94909        40.78134
## 10        2               1          1.16        -73.98251        40.76832
## # ... with 4,999,990 more rows, and 17 more variables: RatecodeID <int>,
## #   store_and_fwd_flag <chr>, dropoff_longitude <dbl>,
## #   dropoff_latitude <dbl>, payment_type <int>, fare_amount <dbl>,
## #   tip_amount <dbl>, tolls_amount <dbl>, tip_percent <dbl>,
## #   pickup_hour <fctr>, pickup_dow <fctr>, dropoff_hour <fctr>,
## #   dropoff_dow <fctr>, trip_duration <int>, pickup_nhood <fctr>,
## #   dropoff_nhood <fctr>, tip_pct <dbl>
```



```r
summarize(group_by(taxi_df, dropoff_nhood), 
          Num = n(), ave_tip_pct = mean(tip_pct))
```

```
## # A tibble: 260 × 3
##      dropoff_nhood   Num ave_tip_pct
##             <fctr> <int>       <dbl>
## 1         Annadale     5  0.24561406
## 2    Arden Heights    12  0.15648199
## 3         Arrochar    36  0.11437722
## 4          Arverne    69  0.11855379
## 5          Astoria 53770  0.16893957
## 6  Astoria Heights   594  0.08350706
## 7       Auburndale   390  0.09936931
## 8       Bath Beach   802  0.10006194
## 9     Battery Park 53613  0.20848133
## 10       Bay Ridge  2846  0.13369090
## # ... with 250 more rows
```

## Group By Neighborhoods Example


```r
summarise(group_by(taxi_df, pickup_nhood, dropoff_nhood), 
          Num = n(), ave_tip_pct = mean(tip_pct))
```

```
## Source: local data frame [13,089 x 4]
## Groups: pickup_nhood [?]
## 
##    pickup_nhood   dropoff_nhood   Num ave_tip_pct
##          <fctr>          <fctr> <int>       <dbl>
## 1      Arrochar        Arrochar     1  0.00000000
## 2      Arrochar   Crown Heights     2  0.18914665
## 3       Arverne         Arverne     4  0.12575000
## 4       Arverne    Far Rockaway     2  0.00000000
## 5       Astoria         Astoria  9896  0.34644699
## 6       Astoria Astoria Heights   161  0.07155575
## 7       Astoria      Auburndale     4  0.05240741
## 8       Astoria      Bath Beach     1  0.00000000
## 9       Astoria    Battery Park    19  0.14814670
## 10      Astoria       Bay Ridge     8  0.09474953
## # ... with 13,079 more rows
```

## Chaining/Piping

* A `dplyr` installation includes the `magrittr` package as a dependency 
* The `magrittr` package includes a pipe operator that allows you to pass the current dataset to another function
* This makes interpreting a nested sequence of operations much easier to understand

## Standard Code

* Code is executed inside-out.
* Let's arrange the above average tips in descending order, and only look at the locations that had at least 10 dropoffs and pickups.


```r
filter(arrange(summarise(group_by(taxi_df, pickup_nhood, dropoff_nhood), Num = n(), ave_tip_pct = mean(tip_pct)), desc(ave_tip_pct)), Num >= 10)
```

```
## Source: local data frame [5,333 x 4]
## Groups: pickup_nhood [151]
## 
##          pickup_nhood                         dropoff_nhood   Num
##                <fctr>                                <fctr> <int>
## 1             Melrose                               Melrose    23
## 2        Central Park John F. Kennedy International Airport   548
## 3          Mott Haven                            Mott Haven   242
## 4  La Guardia Airport                    La Guardia Airport  1665
## 5        Battery Park                          Battery Park  1666
## 6      Pelham Gardens                        Pelham Gardens    13
## 7            Longwood                              Longwood    12
## 8    Garment District                        Brighton Beach    21
## 9        Far Rockaway                          Far Rockaway    16
## 10          Flatlands                             Flatlands    15
## # ... with 5,323 more rows, and 1 more variables: ave_tip_pct <dbl>
```

--- 

![damn](http://www.ohmagif.com/wp-content/uploads/2015/01/lemme-go-out-for-a-walk-oh-no-shit.gif)

## Reformatted


```r
filter(
  arrange(
    summarise(
      group_by(taxi_df, 
               pickup_nhood, dropoff_nhood), 
      Num = n(), 
      ave_tip_pct = mean(tip_pct)), 
    desc(ave_tip_pct)), 
  Num >= 10)
```

```
## Source: local data frame [5,333 x 4]
## Groups: pickup_nhood [151]
## 
##          pickup_nhood                         dropoff_nhood   Num
##                <fctr>                                <fctr> <int>
## 1             Melrose                               Melrose    23
## 2        Central Park John F. Kennedy International Airport   548
## 3          Mott Haven                            Mott Haven   242
## 4  La Guardia Airport                    La Guardia Airport  1665
## 5        Battery Park                          Battery Park  1666
## 6      Pelham Gardens                        Pelham Gardens    13
## 7            Longwood                              Longwood    12
## 8    Garment District                        Brighton Beach    21
## 9        Far Rockaway                          Far Rockaway    16
## 10          Flatlands                             Flatlands    15
## # ... with 5,323 more rows, and 1 more variables: ave_tip_pct <dbl>
```

## Magrittr

![](https://github.com/smbache/magrittr/raw/master/inst/logo.png)

* Inspired by unix `|`, and F# forward pipe `|>`, `magrittr` introduces the funny character (`%>%`, the _then_ operator)
* `%>%` pipes the object on the left hand side to the first argument of the function on the right hand side
* Every function in `dplyr` has a slot for `data.frame/tbl` as it's first argument, so this works beautifully!

## Put that Function in Your Pipe and...


```r
taxi_df %>% 
  group_by(pickup_nhood, dropoff_nhood) %>% 
  summarize(Num = n(),
            ave_tip_pct = mean(tip_pct)) %>% 
  arrange(desc(ave_tip_pct)) %>% 
  filter(Num >= 10)
```

```
## Source: local data frame [5,333 x 4]
## Groups: pickup_nhood [151]
## 
##          pickup_nhood                         dropoff_nhood   Num
##                <fctr>                                <fctr> <int>
## 1             Melrose                               Melrose    23
## 2        Central Park John F. Kennedy International Airport   548
## 3          Mott Haven                            Mott Haven   242
## 4  La Guardia Airport                    La Guardia Airport  1665
## 5        Battery Park                          Battery Park  1666
## 6      Pelham Gardens                        Pelham Gardens    13
## 7            Longwood                              Longwood    12
## 8    Garment District                        Brighton Beach    21
## 9        Far Rockaway                          Far Rockaway    16
## 10          Flatlands                             Flatlands    15
## # ... with 5,323 more rows, and 1 more variables: ave_tip_pct <dbl>
```

---

![hellyeah](http://i.giphy.com/lF1XZv45kIwMw.gif)

## Pipe + group_by()

* The pipe operator is very helpful for group by summaries
* Let's calculate average tip amount, and average trip distance, controlling for dropoff day of the week and dropoff location
* First filter with the vector `manhattan_hoods`

---


```r
mht_url <- "http://alizaidi.blob.core.windows.net/training/manhattan.rds"
manhattan_hoods <- readRDS(gzcon(url(mht_url)))
taxi_df %>% 
  filter(pickup_nhood %in% manhattan_hoods,
         dropoff_nhood %in% manhattan_hoods) %>% 
  group_by(dropoff_nhood, pickup_nhood) %>% 
  summarize(ave_tip = mean(tip_pct), 
            ave_dist = mean(trip_distance)) %>% 
  filter(ave_dist > 3, ave_tip > 0.05)
```

```
## Source: local data frame [325 x 4]
## Groups: dropoff_nhood [24]
## 
##    dropoff_nhood        pickup_nhood   ave_tip ave_dist
##           <fctr>              <fctr>     <dbl>    <dbl>
## 1   Battery Park        Central Park 0.1176756 6.173279
## 2   Battery Park             Clinton 0.1172045 3.989862
## 3   Battery Park         East Harlem 0.1774509 8.880230
## 4   Battery Park        East Village 0.1418621 3.556687
## 5   Battery Park    Garment District 0.1194719 4.041875
## 6   Battery Park            Gramercy 0.1453468 4.743278
## 7   Battery Park    Hamilton Heights 0.1386226 9.067857
## 8   Battery Park              Harlem 0.1356608 8.968276
## 9   Battery Park             Midtown 0.1166259 5.423574
## 10  Battery Park Morningside Heights 0.1344809 8.083016
## # ... with 315 more rows
```

## Pipe and Plot

Piping is not limited to dplyr functions, can be used everywhere!


```r
library(ggplot2)
taxi_df %>% 
  filter(pickup_nhood %in% manhattan_hoods,
         dropoff_nhood %in% manhattan_hoods) %>% 
  group_by(dropoff_nhood, pickup_nhood) %>% 
  summarize(ave_tip = mean(tip_pct), 
            ave_dist = mean(trip_distance)) %>% 
  filter(ave_dist > 3, ave_tip > 0.05) %>% 
  ggplot(aes(x = pickup_nhood, y = dropoff_nhood)) + 
    geom_tile(aes(fill = ave_tip), colour = "white") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = 'bottom') +
    scale_fill_gradient(low = "white", high = "steelblue")
```

---

<img src="/datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/Handouts/1-intro-to-R/1-intro-to-R_files/figure-html/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />


## Piping to other arguments

* Although `dplyr` takes great care to make it particularly amenable to piping, other functions may not reserve the first argument to the object you are passing into it.
* You can use the special `.` placeholder to specify where the object should enter


```r
taxi_df %>% 
  filter(pickup_nhood %in% manhattan_hoods,
         dropoff_nhood %in% manhattan_hoods) %>% 
  group_by(dropoff_nhood, pickup_nhood) %>% 
  summarize(ave_tip = mean(tip_pct), 
            ave_dist = mean(trip_distance)) %>% 
  lm(ave_tip ~ ave_dist, data = .) -> taxi_model
summary(taxi_model)
```

```
## 
## Call:
## lm(formula = ave_tip ~ ave_dist, data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.12962 -0.01573 -0.00397  0.00826  2.10714 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.147818   0.006793   21.76  < 2e-16 ***
## ave_dist    -0.003279   0.001247   -2.63  0.00878 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0927 on 571 degrees of freedom
## Multiple R-squared:  0.01197,	Adjusted R-squared:  0.01024 
## F-statistic: 6.915 on 1 and 571 DF,  p-value: 0.008776
```

## Exercise
  
Your turn: 

* Use the pipe operator to group by day of week and dropoff neighborhood
* Filter to Manhattan neighborhoods 
* Make tile plot with average fare amount in dollars as the fill

# Functional Programming

## Creating Functional Pipelines 
### Too Many Pipes?

![whoaaaaaaaaahhhhh](http://www.ohmagif.com/wp-content/uploads/2015/02/the-scariest-electrical-repair-ever.gif)

---

### Reusable code

* The examples above create a rather messy pipeline operation
* Can be very hard to debug
* The operation is pretty readable, but lacks reusability
* Since R is a functional language, we benefit by splitting these operations into functions and calling them separately
* This allows resuability; don't write the same code twice!

## Functional Pipelines 
### Summarization

* Let's create a function that takes an argument for the data, and applies the summarization by neighborhood to calculate average tip and trip distance

---


```r
taxi_hood_sum <- function(taxi_data = taxi_df) {
  
  mht_url <- "http://alizaidi.blob.core.windows.net/training/manhattan.rds"
  
  manhattan_hoods <- readRDS(gzcon(url(mht_url)))
  taxi_data %>% 
    filter(pickup_nhood %in% manhattan_hoods,
           dropoff_nhood %in% manhattan_hoods) %>% 
    group_by(dropoff_nhood, pickup_nhood) %>% 
    summarize(ave_tip = mean(tip_pct), 
              ave_dist = mean(trip_distance)) %>% 
    filter(ave_dist > 3, ave_tip > 0.05) -> sum_df
  
  return(sum_df)
  
}
```

## Functional Pipelines

### Plotting Function

* We can create a second function for the plot


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

## Calling Our Pipeline

* Now we can create our plot by simply calling our two functions


```r
library(plotly)
taxi_hood_sum(taxi_df) %>% tile_plot_hood %>% ggplotly
```

Let's make that baby interactive.

## Creating Complex Pipelines with do

* The `summarize` function is fun, can summarize many numeric/scalar quantities
* But what if you want multiple values/rows back, not just a scalar summary?
* Meet the `do` verb -- arbitrary `tbl` operations

---


```r
taxi_df %>% group_by(dropoff_dow) %>%
  filter(!is.na(dropoff_nhood), !is.na(pickup_nhood)) %>%
  arrange(desc(tip_pct)) %>% 
  do(slice(., 1:2)) %>% 
  select(dropoff_dow, tip_amount, tip_pct, 
         fare_amount, dropoff_nhood, pickup_nhood)
```

```
## Source: local data frame [14 x 6]
## Groups: dropoff_dow [7]
## 
##    dropoff_dow tip_amount  tip_pct fare_amount
##         <fctr>      <dbl>    <dbl>       <dbl>
## 1          Sun       4.60  460.000        0.01
## 2          Sun      80.70   32.280        2.50
## 3          Mon      62.00 6200.000        0.01
## 4          Mon      38.00 3800.000        0.01
## 5          Tue      35.00 3500.000        0.01
## 6          Tue      21.00 2100.000        0.01
## 7          Wed      49.69 4969.000        0.01
## 8          Wed     100.00   40.000        2.50
## 9          Thu      10.80 1080.000        0.01
## 10         Thu     655.58   65.558       10.00
## 11         Fri      53.61 5361.000        0.01
## 12         Fri      22.00 2200.000        0.01
## 13         Sat      20.00 2000.000        0.01
## 14         Sat      10.00 1000.000        0.01
## # ... with 2 more variables: dropoff_nhood <fctr>, pickup_nhood <fctr>
```

## Estimating Multiple Models with do

* A common use of `do` is to calculate many different models by a grouping variable


```r
dow_lms <- taxi_df %>% sample_n(10^4) %>% 
  group_by(dropoff_dow) %>% 
  do(lm_tip = lm(tip_pct ~ pickup_nhood + passenger_count + pickup_hour,
     data = .))
```

---


```r
dow_lms
```

```
## Source: local data frame [7 x 2]
## Groups: <by row>
## 
## # A tibble: 7 × 2
##   dropoff_dow   lm_tip
## *      <fctr>   <list>
## 1         Sun <S3: lm>
## 2         Mon <S3: lm>
## 3         Tue <S3: lm>
## 4         Wed <S3: lm>
## 5         Thu <S3: lm>
## 6         Fri <S3: lm>
## 7         Sat <S3: lm>
```


Where are our results? 
![digging](http://i.giphy.com/oEnTTI3ZdK6ic.gif)

## Cleaning Output


```r
summary(dow_lms$lm_tip[[1]])
```

```
## 
## Call:
## lm(formula = tip_pct ~ pickup_nhood + passenger_count + pickup_hour, 
##     data = .)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.20009 -0.11627  0.00828  0.09640  1.28912 
## 
## Coefficients:
##                                                    Estimate Std. Error
## (Intercept)                                        0.065740   0.033660
## pickup_nhoodBattery Park                           0.059202   0.049867
## pickup_nhoodBedford Stuyvesant                     0.039336   0.060695
## pickup_nhoodBoerum Hill                            0.182271   0.129431
## pickup_nhoodBrooklyn Heights                       0.144378   0.129431
## pickup_nhoodBushwick                               0.175288   0.129528
## pickup_nhoodCarnegie Hill                          0.041913   0.051365
## pickup_nhoodCentral Park                           0.027697   0.043315
## pickup_nhoodChelsea                                0.062381   0.035184
## pickup_nhoodChinatown                              0.094590   0.052839
## pickup_nhoodClinton                                0.040395   0.037789
## pickup_nhoodClinton Hill                           0.166556   0.129431
## pickup_nhoodColumbus Circle                        0.012963   0.043260
## pickup_nhoodCorona                                -0.079116   0.129408
## pickup_nhoodDowntown                              -0.072303   0.079371
## pickup_nhoodDUMBO                                  0.176127   0.094329
## pickup_nhoodEast Elmhurst                         -0.064445   0.129456
## pickup_nhoodEast Harlem                            0.022619   0.045069
## pickup_nhoodEast Village                           0.119681   0.035650
## pickup_nhoodElmhurst                              -0.059265   0.129817
## pickup_nhoodFinancial District                     0.061521   0.038672
## pickup_nhoodFlatiron District                      0.063356   0.035308
## pickup_nhoodForest Hills                           0.082649   0.095069
## pickup_nhoodFort Greene                           -0.080411   0.129380
## pickup_nhoodGarment District                       0.036150   0.035239
## pickup_nhoodGramercy                               0.077708   0.036878
## pickup_nhoodGreenpoint                            -0.064445   0.129456
## pickup_nhoodGreenwich Village                      0.061200   0.035699
## pickup_nhoodHamilton Heights                       0.083187   0.079184
## pickup_nhoodHarlem                                 0.023384   0.064949
## pickup_nhoodHunters Point                          0.077032   0.070463
## pickup_nhoodJamaica                                0.158228   0.130116
## pickup_nhoodJohn F. Kennedy International Airport  0.052496   0.039453
## pickup_nhoodKew Gardens                            0.146754   0.129701
## pickup_nhoodLa Guardia Airport                     0.090945   0.040207
## pickup_nhoodLittle Italy                           0.154225   0.048575
## pickup_nhoodLower East Side                        0.065713   0.037012
## pickup_nhoodMidtown                                0.053764   0.033979
## pickup_nhoodMorningside Heights                    0.009282   0.129514
## pickup_nhoodMurray Hill                            0.057507   0.041593
## pickup_nhoodNoHo                                   0.069474   0.052907
## pickup_nhoodPark Slope                             0.033598   0.070618
## pickup_nhoodProspect Heights                       0.237155   0.094281
## pickup_nhoodRego Park                             -0.077861   0.129701
## pickup_nhoodRoosevelt Island                      -0.069191   0.094483
## pickup_nhoodSoHo                                   0.049192   0.040734
## pickup_nhoodStuyvesant Town                       -0.028451   0.094306
## pickup_nhoodSunnyside                              0.029028   0.079184
## pickup_nhoodSutton Place                           0.097974   0.044402
## pickup_nhoodTremont                               -0.064445   0.129456
## pickup_nhoodTribeca                                0.092489   0.040710
## pickup_nhoodTudor City                             0.118788   0.048561
## pickup_nhoodTurtle Bay                             0.066920   0.038943
## pickup_nhoodUpper East Side                        0.064816   0.034047
## pickup_nhoodUpper West Side                        0.051358   0.034640
## pickup_nhoodWashington Heights                    -0.069423   0.079206
## pickup_nhoodWest Village                           0.081799   0.037472
## pickup_nhoodWilliamsburg                           0.062827   0.051224
## pickup_nhoodWoodside                               0.242994   0.129404
## passenger_count                                   -0.001295   0.002651
## pickup_hour5AM-9AM                                 0.013416   0.016631
## pickup_hour9AM-12PM                                0.012560   0.013829
## pickup_hour12PM-4PM                                0.002816   0.012948
## pickup_hour4PM-6PM                                -0.013121   0.014572
## pickup_hour6PM-10PM                                0.015966   0.012864
## pickup_hour10PM-1AM                               -0.011002   0.013249
##                                                   t value Pr(>|t|)    
## (Intercept)                                         1.953  0.05102 .  
## pickup_nhoodBattery Park                            1.187  0.23536    
## pickup_nhoodBedford Stuyvesant                      0.648  0.51704    
## pickup_nhoodBoerum Hill                             1.408  0.15930    
## pickup_nhoodBrooklyn Heights                        1.115  0.26485    
## pickup_nhoodBushwick                                1.353  0.17620    
## pickup_nhoodCarnegie Hill                           0.816  0.41466    
## pickup_nhoodCentral Park                            0.639  0.52266    
## pickup_nhoodChelsea                                 1.773  0.07646 .  
## pickup_nhoodChinatown                               1.790  0.07366 .  
## pickup_nhoodClinton                                 1.069  0.28527    
## pickup_nhoodClinton Hill                            1.287  0.19838    
## pickup_nhoodColumbus Circle                         0.300  0.76449    
## pickup_nhoodCorona                                 -0.611  0.54106    
## pickup_nhoodDowntown                               -0.911  0.36249    
## pickup_nhoodDUMBO                                   1.867  0.06210 .  
## pickup_nhoodEast Elmhurst                          -0.498  0.61870    
## pickup_nhoodEast Harlem                             0.502  0.61584    
## pickup_nhoodEast Village                            3.357  0.00081 ***
## pickup_nhoodElmhurst                               -0.457  0.64809    
## pickup_nhoodFinancial District                      1.591  0.11188    
## pickup_nhoodFlatiron District                       1.794  0.07298 .  
## pickup_nhoodForest Hills                            0.869  0.38481    
## pickup_nhoodFort Greene                            -0.622  0.53437    
## pickup_nhoodGarment District                        1.026  0.30515    
## pickup_nhoodGramercy                                2.107  0.03529 *  
## pickup_nhoodGreenpoint                             -0.498  0.61870    
## pickup_nhoodGreenwich Village                       1.714  0.08670 .  
## pickup_nhoodHamilton Heights                        1.051  0.29366    
## pickup_nhoodHarlem                                  0.360  0.71888    
## pickup_nhoodHunters Point                           1.093  0.27449    
## pickup_nhoodJamaica                                 1.216  0.22418    
## pickup_nhoodJohn F. Kennedy International Airport   1.331  0.18355    
## pickup_nhoodKew Gardens                             1.131  0.25806    
## pickup_nhoodLa Guardia Airport                      2.262  0.02386 *  
## pickup_nhoodLittle Italy                            3.175  0.00153 ** 
## pickup_nhoodLower East Side                         1.775  0.07606 .  
## pickup_nhoodMidtown                                 1.582  0.11382    
## pickup_nhoodMorningside Heights                     0.072  0.94288    
## pickup_nhoodMurray Hill                             1.383  0.16702    
## pickup_nhoodNoHo                                    1.313  0.18937    
## pickup_nhoodPark Slope                              0.476  0.63431    
## pickup_nhoodProspect Heights                        2.515  0.01201 *  
## pickup_nhoodRego Park                              -0.600  0.54840    
## pickup_nhoodRoosevelt Island                       -0.732  0.46411    
## pickup_nhoodSoHo                                    1.208  0.22740    
## pickup_nhoodStuyvesant Town                        -0.302  0.76294    
## pickup_nhoodSunnyside                               0.367  0.71399    
## pickup_nhoodSutton Place                            2.207  0.02752 *  
## pickup_nhoodTremont                                -0.498  0.61870    
## pickup_nhoodTribeca                                 2.272  0.02325 *  
## pickup_nhoodTudor City                              2.446  0.01457 *  
## pickup_nhoodTurtle Bay                              1.718  0.08596 .  
## pickup_nhoodUpper East Side                         1.904  0.05716 .  
## pickup_nhoodUpper West Side                         1.483  0.13842    
## pickup_nhoodWashington Heights                     -0.876  0.38092    
## pickup_nhoodWest Village                            2.183  0.02922 *  
## pickup_nhoodWilliamsburg                            1.227  0.22023    
## pickup_nhoodWoodside                                1.878  0.06063 .  
## passenger_count                                    -0.489  0.62520    
## pickup_hour5AM-9AM                                  0.807  0.42001    
## pickup_hour9AM-12PM                                 0.908  0.36391    
## pickup_hour12PM-4PM                                 0.218  0.82785    
## pickup_hour4PM-6PM                                 -0.900  0.36806    
## pickup_hour6PM-10PM                                 1.241  0.21478    
## pickup_hour10PM-1AM                                -0.830  0.40647    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1251 on 1316 degrees of freedom
## Multiple R-squared:  0.06135,	Adjusted R-squared:  0.01499 
## F-statistic: 1.323 on 65 and 1316 DF,  p-value: 0.04649
```

```r
library(broom)
dow_lms %>% tidy(lm_tip)
```

```
## Source: local data frame [434 x 6]
## Groups: dropoff_dow [7]
## 
##    dropoff_dow                           term   estimate  std.error
##         <fctr>                          <chr>      <dbl>      <dbl>
## 1          Sun                    (Intercept) 0.06574038 0.03365972
## 2          Sun       pickup_nhoodBattery Park 0.05920213 0.04986723
## 3          Sun pickup_nhoodBedford Stuyvesant 0.03933570 0.06069477
## 4          Sun        pickup_nhoodBoerum Hill 0.18227069 0.12943128
## 5          Sun   pickup_nhoodBrooklyn Heights 0.14437838 0.12943116
## 6          Sun           pickup_nhoodBushwick 0.17528770 0.12952842
## 7          Sun      pickup_nhoodCarnegie Hill 0.04191264 0.05136518
## 8          Sun       pickup_nhoodCentral Park 0.02769683 0.04331530
## 9          Sun            pickup_nhoodChelsea 0.06238084 0.03518380
## 10         Sun          pickup_nhoodChinatown 0.09459021 0.05283873
## # ... with 424 more rows, and 2 more variables: statistic <dbl>,
## #   p.value <dbl>
```


* By design, every function in `dplyr` returns a `data.frame`
* In the example above, we get back a spooky `data.frame` with a column of `S3` `lm` objects
* You can still modify each element as you would normally, or pass it to a `mutate` function to extract intercept or statistics
* But there's also a very handy `broom` package for cleaning up such objects into `data.frames`

## Brooming Up the Mess

### Model Metrics

```r
library(broom)
taxi_df %>% sample_n(10^5) %>%  
  group_by(dropoff_dow) %>% 
  do(glance(lm(tip_pct ~ pickup_nhood + passenger_count + pickup_hour,
     data = .)))
```

```
## Source: local data frame [7 x 12]
## Groups: dropoff_dow [7]
## 
##   dropoff_dow  r.squared adj.r.squared     sigma statistic      p.value
##        <fctr>      <dbl>         <dbl>     <dbl>     <dbl>        <dbl>
## 1         Sun 0.02370978   0.016175094 0.1278958  3.146751 2.699941e-24
## 2         Mon 0.01895253   0.011683249 0.1637930  2.607207 2.297242e-15
## 3         Tue 0.01912515   0.012548672 0.1271263  2.908114 3.717353e-19
## 4         Wed 0.02831251   0.022054848 0.1248618  4.524453 2.119818e-42
## 5         Thu 0.01708457   0.010603074 0.2162880  2.635899 4.808393e-16
## 6         Fri 0.01550050   0.008719925 0.1480675  2.286016 3.049875e-12
## 7         Sat 0.01707519   0.010660680 0.1569214  2.661964 1.545285e-16
## # ... with 6 more variables: df <int>, logLik <dbl>, AIC <dbl>, BIC <dbl>,
## #   deviance <dbl>, df.residual <int>
```

### Model Coefficients

The most commonly used function in the `broom` package is the `tidy` function. This will expand our data.frame and give us the model coefficients


```r
taxi_df %>% sample_n(10^5) %>%  
  group_by(dropoff_dow) %>% 
  do(tidy(lm(tip_pct ~ pickup_nhood + passenger_count + pickup_hour,
     data = .)))
```

```
## Source: local data frame [694 x 6]
## Groups: dropoff_dow [7]
## 
##    dropoff_dow                           term    estimate  std.error
##         <fctr>                          <chr>       <dbl>      <dbl>
## 1          Sun                    (Intercept)  0.11895457 0.02350875
## 2          Sun       pickup_nhoodBattery Park  0.03163324 0.03027825
## 3          Sun          pickup_nhoodBay Ridge -0.10737294 0.20951439
## 4          Sun            pickup_nhoodBayside -0.11383714 0.20953271
## 5          Sun pickup_nhoodBedford Stuyvesant  0.02406192 0.05000438
## 6          Sun        pickup_nhoodBoerum Hill  0.04162640 0.06975837
## 7          Sun          pickup_nhoodBriarwood -0.07724149 0.10664184
## 8          Sun   pickup_nhoodBrooklyn Heights  0.05697923 0.06978770
## 9          Sun        pickup_nhoodBrownsville -0.11042552 0.20957708
## 10         Sun           pickup_nhoodBushwick  0.05413353 0.06442802
## # ... with 684 more rows, and 2 more variables: statistic <dbl>,
## #   p.value <dbl>
```


## Spatial Visualizations with `ggplot2` and `purrr`

### Visualizing Pickups by Time

+ Let's try another example
+ We will visualize pickups and index them by time



```r
# min and max coordinates:
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

pickups <- taxi_df %>% 
  filter(pickup_longitude > min_long,
         pickup_latitude < max_lat,
         dropoff_longitude > min_long,
         dropoff_latitude < max_lat) %>% 
  group_by(pickup_hour, 
           pickup_longitude, 
           pickup_latitude) %>% 
  summarise(num_pickups = n())
```

## Load Additional Libraries


```r
library(purrr)
library(lubridate)
library(RColorBrewer)
library(magick)
```


## Visualize Pickups

### ggplot2 Theme

+ `ggplot` will give very aesthetically appealing plots by default
+ However, it really shines in it's ability to customize
+ See the `ggthemes` for some template themes
+ We'll use the theme below inspired from [Max Woolf's Tweet on this dataset](https://twitter.com/minimaxir/status/628921315198566400)



```r
theme_map_dark <- function(palate_color = "Greys") {
  
  palate <- brewer.pal(palate_color, n=9)
  color.background = "black"
  color.grid.minor = "black"
  color.grid.major = "black"
  color.axis.text = palate[1]
  color.axis.title = palate[1]
  color.title = palate[1]
  
  font.title <- "Source Sans Pro"
  font.axis <- "Open Sans Condensed Bold"
  
  theme_bw(base_size=5) +
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=3,colour=color.axis.title,family=font.axis)) +
    theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal") +
    theme(legend.key.width=unit(1, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm")) +
    theme(plot.title=element_text(colour=color.title,family=font.title, size=14)) +
    theme(plot.subtitle = element_text(colour=color.title,family=font.title, size=12)) +
    theme(axis.text.x=element_blank()) +
    theme(axis.text.y=element_blank()) +
    theme(axis.title.y=element_blank()) +
    theme(axis.title.x=element_blank()) +
    theme(strip.background = element_rect(fill=color.background, 
                                          color=color.background),
          strip.text=element_text(size=7,colour=color.axis.title,family=font.title))
  
}
```

## Plot Function

### Complete the Function Below


```r
# x axis should be longitude
# y axis should be latitude
map_nyc <- function(df, pickup_hr) {
  
  gplot <- ggplot(df, 
                  aes(x=..., 
                      y=...)) +
    geom_point(color="white", size=0.06) +
    scale_x_continuous(limits=c(min_long, max_long)) +
    scale_y_continuous(limits=c(min_lat, max_lat)) +
    theme_map_dark() + 
    labs(title = "Map of NYC Taxi Pickups",
         subtitle = paste0("Pickups between ", pickup_hr))
  
  return(gplot)
  
}
```

## Iterate and Plot!
### Now we can Iterate and Plot


```r
hour_plots <- ungroup(pickups) %>% 
  filter(num_pickups > 1) %>% 
  split(.$pickup_hour) %>% 
  map(~ map_nyc(.x, pickup_hr = .x$pickup_hour[1]))

hour_plots
```



## Summary

mutate

:    Create transformations

summarise

:    Aggregate

group_by

:    Group your dataset by levels

do

:    Evaluate complex operations on a tbl

Chaining with the `%>%` operator can result in more readable code.

## What We Didn't Cover

* There are many additional topics that fit well into the `dplyr` and functional programming landscape
* There are too many to cover in one session. Fortunately, most are well documented. The most notable omissions:
  1. Connecting to remote databases, see `vignette('databases', package = 'dplyr')`
  2. Merging and Joins, see `vignette('two-table', package = 'dplyr')`
  3. Programming with `dplyr`,`vignette('nse', package = 'dplyr')`
  4. `summarize_each` and `mutate_each`

## Thanks for Attending!

- Any questions?
