#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

# package insallation -----------------------------------------------------

# update cran
r <- getOption('repos')
# set mirror to something a bit more recent

if (length(args) != 0 & args[1] == "latest") {
       mran_date <- Sys.Date() - 1
       r[["CRAN"]] <- paste0("https://mran.revolutionanalytics.com/snapshot/", mran_date)

} else {
       r[["CRAN"]] <- "https://mran.revolutionanalytics.com/snapshot/2017-04-15/"
}


options(repos = r)

# If you have issues installing the rgeos package on linux:
# on RHEL, centos `sudo yum install geos geos-devel`
# on ubuntu `sudo apt-get install libgeos libgeos-dev`
pkgs_to_install <- c("devtools", 
                     # "data.table",
                     # "stringr", 
                     # "broom", "magrittr", "dplyr",
                     # "lubridate",
                     # "rgeos", "sp", "maptools",
                     # "seriation",
                     # "ggplot2",
                     # "gridExtra",
                     # "ggrepel",
                     # "tidyr",
                     "curl", "httr",
                     "tidyverse",
                     "revealjs",
                     "plotly", 'sparklyr'
                     )
pks_missing <- pkgs_to_install[!(pkgs_to_install %in% installed.packages()[, 1])]

install.packages(c(pks_missing, 'knitr', 'formatR', 'rmarkdown'))


# install-dplyrXdf --------------------------------------------------------

dev_pkgs <- c("RevolutionAnalytics/dplyrXdf", "rstudio/rtutor")
devtools::install_github(dev_pkgs)


# check package versions --------------------------------------------------

pkgs <- c(pkgs_to_install, "dplyrXdf", "rtutor")

Map(packageVersion, pkgs)
