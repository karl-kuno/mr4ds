library(mrsdeploy)

remoteLogin(
    deployr_endpoint = "http://localhost:12800",
    username = "admin",
    password = passwd
)

library(RevoScaleR)
rxHadoopListFiles("/")


spark_cc <- RxSpark(consoleOutput = TRUE,
                    persistentRun = TRUE)

rxSetComputeContext(spark_cc)


# switch to local, press ESC
download.file("https://alizaidi.blob.core.windows.net/training/sample_taxi.csv",
              "sample_taxi.csv")
local_taxi <- RxTextData("sample_taxi.csv")


### ----- function for tips

ave_tip_dow <- function(taxi_data = local_taxi) {

    dows <- c("Mon", "Tue", "Wed", "Thu",
              "Fri", "Sat", "Sun")

    rxCube(tip_amount ~ pickup_dow,
           transforms = list(pickup_dow = factor(pickup_dow,
                                                 levels = dow_lvls)),
           transformObjects = list(dow_lvls = dows),
           data = taxi_data)

}

ave_tip_dow()

#### -------- copy to remote
putLocalObject("ave_tip_dow")
resume()

wasb_taxi <- "/NYCTaxi/sample/"
taxi_large <- RxTextData(wasb_taxi, fileSystem = RxHdfsFileSystem())


ave_tip_dow(taxi_large)

taxi_xdf <- "/user/RevoShare/sshuser/taxiXdf"
taxi_xdf <- RxXdfData(taxi_xdf,
                      fileSystem = RxHdfsFileSystem())

ave_tip_dow(taxi_xdf)