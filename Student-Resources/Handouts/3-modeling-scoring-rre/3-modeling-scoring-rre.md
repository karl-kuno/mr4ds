# Modeling and Scoring with RevoScaleR
Ali Zaidi  
`r format(Sys.Date(), "%B %d, %Y")`  


# Introduction 

## URL for Today

Please refer to the github repository for course materials [github.com/akzaidi/R-cadence](https://github.com/akzaidi/R-cadence/)

## Agenda

- We will learn in this tutorial how to train and test models with the `RevoScaleR` package.
- Use your knowledge of data manipulation to create **train** and **test** sets.
- Use the modeling functions in `RevoScaleR` to train a model.
- Use the `rxPredict` function to test/score a model.
- We will see how you can score models on a variety of data sources.
- Use a functional methodology, i.e., we will create functions to automate the modeling, validation, and scoring process.

## Prerequisites

- Understanding of `rxDataStep` and `xdfs`
- Familiarity with `RevoScaleR` modeling and datastep functions: `rxLinMod`, `rxGlm`, `rxLogit`, `rxDTree`, `rxDForest`, `rxSplit`, and `rxPredict`
- Understand how to write functions in R
- Access to at least one interesting dataset

## Typical Lifecycle

![](images/revo-split-life-cycle.png)

Typical Modeling Lifecycle:

- Start with a data set
- Split into a training set and validation set(s)
- Use the `ScaleR` modeling functions on the train set to estimate your model
- Use `rxPredict` to validate/score your results

## Mortgage Dataset

- We will work with a mortgage dataset, which contains mortgage and credit profiles for various mortgage holders


```r
mort_path <- paste(rxGetOption("sampleDataDir"), "mortDefaultSmall.xdf", sep = "/")
file.copy(mort_path, "mortgage.xdf", overwrite = TRUE)
```

```
## [1] TRUE
```

```r
mort_xdf <- RxXdfData("mortgage.xdf")
rxGetInfo(mort_xdf, getVarInfo = TRUE, numRows = 5)
```

```
## File name: /datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/rmarkdown/mortgage.xdf 
## Number of observations: 1e+05 
## Number of variables: 6 
## Number of blocks: 10 
## Compression type: zlib 
## Variable information: 
## Var 1: creditScore, Type: integer, Low/High: (470, 925)
## Var 2: houseAge, Type: integer, Low/High: (0, 40)
## Var 3: yearsEmploy, Type: integer, Low/High: (0, 14)
## Var 4: ccDebt, Type: integer, Low/High: (0, 14094)
## Var 5: year, Type: integer, Low/High: (2000, 2009)
## Var 6: default, Type: integer, Low/High: (0, 1)
## Data (5 rows starting with row 1):
##   creditScore houseAge yearsEmploy ccDebt year default
## 1         691       16           9   6725 2000       0
## 2         691        4           4   5077 2000       0
## 3         743       18           3   3080 2000       0
## 4         728       22           1   4345 2000       0
## 5         745       17           3   2969 2000       0
```

## Transform Default to Categorical

- We might be interested in estimating a classification model for predicting defaults based on credit attributes


```r
rxDataStep(inData = mort_xdf,
           outFile = mort_xdf,
           overwrite = TRUE, 
           transforms = list(default_flag = factor(ifelse(default == 1,
                                                          "default",
                                                          "current"))
                             )
           )
rxGetInfo(mort_xdf, numRows = 3, getVarInfo = TRUE)
```

```
## File name: /datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/rmarkdown/mortgage.xdf 
## Number of observations: 1e+05 
## Number of variables: 7 
## Number of blocks: 10 
## Compression type: zlib 
## Variable information: 
## Var 1: creditScore, Type: integer, Low/High: (470, 925)
## Var 2: houseAge, Type: integer, Low/High: (0, 40)
## Var 3: yearsEmploy, Type: integer, Low/High: (0, 14)
## Var 4: ccDebt, Type: integer, Low/High: (0, 14094)
## Var 5: year, Type: integer, Low/High: (2000, 2009)
## Var 6: default, Type: integer, Low/High: (0, 1)
## Var 7: default_flag
##        2 factor levels: current default
## Data (3 rows starting with row 1):
##   creditScore houseAge yearsEmploy ccDebt year default default_flag
## 1         691       16           9   6725 2000       0      current
## 2         691        4           4   5077 2000       0      current
## 3         743       18           3   3080 2000       0      current
```


# Modeling
## Generating Training and Test Sets 

- The first step to estimating a model is having a tidy training dataset.
- We will work with the mortgage data and use `rxSplit` to create partitions.
- `rxSplit` splits an input `.xdf` into multiple `.xdfs`, similar in spirit to the `split` function in base R
- output is a list
- First step is to create a split variable
- We will randomly partition the data into a train and test sample, with 75% in the former, and 25% in the latter

## Partition Function


```r
create_partition <- function(xdf = mort_xdf,
                             partition_size = 0.75, ...) {
  rxDataStep(inData = xdf,
             outFile = xdf,
             transforms = list(
               trainvalidate = factor(
                   ifelse(rbinom(.rxNumRows, 
                                 size = 1, prob = splitperc), 
                          "train", "validate")
               )
           ),
           transformObjects = list(splitperc = partition_size),
           overwrite = TRUE, ...)
  
  splitDS <- rxSplit(inData = xdf, 
                     #outFilesBase = ,
                     outFileSuffixes = c("train", "validate"),
                     splitByFactor = "trainvalidate",
                     overwrite = TRUE)
  
  return(splitDS) 
  
}
```

## Minimizing IO
### Transforms in rxSplit

While the above example does what we want it to do, it's not very efficient. It requires two passes over the data, first to add the `trainvalidate` column, and then another to split it into train and validate sets. We could do all of that in a single step if we pass the transforms directly to `rxSplit`.


```r
create_partition <- function(xdf = mort_xdf,
                             partition_size = 0.75, ...) {

  splitDS <- rxSplit(inData = xdf, 
                     transforms = list(
                       trainvalidate = factor(
                         ifelse(rbinom(.rxNumRows,
                                       size = 1, prob = splitperc),
                                "train", "validate")
                         )
                       ),
                     transformObjects = list(splitperc = partition_size),
                     outFileSuffixes = c("train", "validate"),
                     splitByFactor = "trainvalidate",
                     overwrite = TRUE)
  
  return(splitDS) 
  
}
```


## Generating Training and Test Sets
### List of xdfs

- The `create_partition` function will output a list `xdfs`


```r
mort_split <- create_partition(reportProgress = 0)
names(mort_split) <- c("train", "validate")
lapply(mort_split, rxGetInfo)
```

```
## $train
## File name: /datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/rmarkdown/mortgage.trainvalidate.train.xdf 
## Number of observations: 75153 
## Number of variables: 8 
## Number of blocks: 10 
## Compression type: zlib 
## 
## $validate
## File name: /datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/rmarkdown/mortgage.trainvalidate.validate.xdf 
## Number of observations: 24847 
## Number of variables: 8 
## Number of blocks: 10 
## Compression type: zlib
```


## Build Your Model 
### Model Formula

- Once you have a training dataset, the most appropriate next step is to estimate your model
- `RevoScaleR` provides a plethora of modeling functions to choose from: decision trees, ensemble trees, linear models, and generalized linear models
- All take a formula as the first object in their call


```r
make_form <- function(xdf = mort_xdf,
                      resp_var = "default_flag",
                      vars_to_skip = c("default", "trainvalidate")) {
  
  library(stringr)
  
  non_incl <- paste(vars_to_skip, collapse = "|")
  
  x_names <- names(xdf)
  
  features <- x_names[!str_detect(x_names, resp_var)]
  features <- features[!str_detect(features, non_incl)]
  
  form <- as.formula(paste(resp_var, paste0(features, collapse = " + "),
                           sep  = " ~ "))
  
  return(form)
}

## Turns out, RevoScaleR already has a function for this
formula(mort_xdf, depVar = "default_flag", varsToDrop = c("defaultflag", "trainvalidate"))
```

```
## default_flag ~ creditScore + houseAge + yearsEmploy + ccDebt + 
##     year + default
## <environment: 0x2f5a4318>
```

## Build Your Model 
### Modeling Function

- Use the `make_form` function inside your favorite `rx` modeling function
- Default value will be a logistic regression, but can update the `model` parameter to any `rx` modeling function


```r
make_form()
```

```
## default_flag ~ creditScore + houseAge + yearsEmploy + ccDebt + 
##     year
## <environment: 0xbb5f7fc8>
```

```r
estimate_model <- function(xdf_data = mort_split[["train"]],
                           form = make_form(xdf_data),
                           model = rxLogit, ...) {
  
  rx_model <- model(form, data = xdf_data, ...)
  
  return(rx_model)
  
  
}
```

## Build Your Model 
### Train Your Model with Our Modeling Function

- Let us now train our logistic regression model for defaults using the `estimate_model` function from the last slide


```r
default_model_logit <- estimate_model(mort_split$train, 
                                      reportProgress = 0)
summary(default_model_logit)
```

```
## Call:
## model(formula = form, data = xdf_data, reportProgress = 0)
## 
## Logistic Regression Results for: default_flag ~ creditScore +
##     houseAge + yearsEmploy + ccDebt + year
## Data: xdf_data (RxXdfData Data Source)
## File name:
##     /datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/rmarkdown/mortgage.trainvalidate.train.xdf
## Dependent variable(s): default_flag
## Total independent variables: 6 
## Number of valid observations: 75153
## Number of missing observations: 0 
## -2*LogLikelihood: 2412.1336 (Residual deviance on 75147 degrees of freedom)
##  
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.275e+03  7.166e+01 -17.796 2.22e-16 ***
## creditScore -7.064e-03  1.202e-03  -5.877 4.19e-09 ***
## houseAge     2.819e-02  7.897e-03   3.570 0.000357 ***
## yearsEmploy -2.683e-01  3.087e-02  -8.692 2.22e-16 ***
## ccDebt       1.228e-03  4.165e-05  29.495 2.22e-16 ***
## year         6.313e-01  3.567e-02  17.700 2.22e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Condition number of final variance-covariance matrix: 2.485 
## Number of iterations: 9
```


## Building Additional Models

- We can change the parameters of the `estimate_model` function to create a different model relatively quickly


```r
default_model_tree <- estimate_model(mort_split$train, 
                                     model = rxDTree,
                                     minBucket = 10,
                                     reportProgress = 0)
summary(default_model_tree)
```

```
##                     Length Class      Mode     
## frame                9     data.frame list     
## where                0     -none-     NULL     
## call                 5     -none-     call     
## cptable             20     -none-     numeric  
## method               1     -none-     character
## parms                3     -none-     list     
## control              9     -none-     list     
## splits              60     -none-     numeric  
## xvars                5     -none-     character
## variable.importance  5     -none-     numeric  
## ordered              5     -none-     logical  
## valid.obs            1     -none-     numeric  
## missing.obs          1     -none-     numeric  
## params              65     -none-     list     
## formula              3     formula    call
```

```r
library(RevoTreeView)
plot(createTreeView(default_model_tree))
```


# Validation
## How Does it Perform on Unseen Data 
### rxPredict for Logistic Regression


```
## [1] TRUE
```

- Now that we have built our model, our next step is to see how it performs on data it has yet to see
- We can use the `rxPredict` function to score/validate our results


```r
default_logit_scored <- rxPredict(default_model_logit,
                                   mort_split$validate,
                                   "scored.xdf",
                                  writeModelVars = TRUE, 
                                  extraVarsToWrite = "default",
                                  predVarNames = c("pred_logit_default"))
rxGetInfo(default_logit_scored, numRows = 2)
```

```
## File name: /datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/rmarkdown/scored.xdf 
## Number of observations: 24847 
## Number of variables: 8 
## Number of blocks: 10 
## Compression type: zlib 
## Data (2 rows starting with row 1):
##   pred_logit_default default default_flag creditScore houseAge yearsEmploy
## 1       8.729900e-08       0      current         683       22           3
## 2       4.187452e-05       0      current         708       32           2
##   ccDebt year
## 1   1143 2000
## 2   5864 2000
```


## Visualize Model Results


```r
plot(rxRoc(actualVarName = "default", 
      predVarNames ="pred_logit_default", 
      data = default_logit_scored))
```

![](/datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/Handouts/3-modeling-scoring-rre/3-modeling-scoring-rre_files/figure-html/roc_curve-1.png)<!-- -->


## Testing a Second Model 
### rxPredict for Decision Tree

- We saw how easy it was to train on different in the previous sections
- Similary simple to test different models


```r
default_tree_scored <- rxPredict(default_model_tree,
                                  mort_split$validate,
                                  "scored.xdf",
                                  writeModelVars = TRUE,
                                 predVarNames = c("pred_tree_current",
                                                  "pred_tree_default"))
```

## Visualize Multiple ROCs


```r
rxRocCurve("default",
           c("pred_logit_default", "pred_tree_default"),
           data = default_tree_scored)
```

![](/datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/Handouts/3-modeling-scoring-rre/3-modeling-scoring-rre_files/figure-html/roc_multiple-1.png)<!-- -->

# Lab - Estimate Other Models Using the Functions Above

## Ensemble Tree Algorithms

Two of the most predictive algorithms in the `RevoScaleR` package are the `rxBTrees` and `rxDForest` algorithms, for gradient boosted decision trees and random forests, respectively.

Use the above functions and estimate a model for each of those algorithms, and add them to the `default_tree_scored` dataset to visualize ROC and AUC metrics.


```r
## Starter code

default_model_forest <- estimate_model(mort_split$train, 
                                       model = ?,
                                       nTree = 100,
                                       importance = ,
                                       ### any other args?,
                                       reportProgress = 0)

default_forest_scored <- rxPredict(default_model_forest,
                                  mort_split$validate,
                                 "scored.xdf", 
                                type = 'prob',
                                predVarNames = c("pred_forest_current", "pred_forest_default", "pred_default"))

## same for rxBTrees

default_model_gbm <- estimate_model(mort_split$train,
                                    model = ,
                                    importance = TRUE,
                                    nTree = ,
                                    ### any other args?,
                                    reportProgress = 0)

default_gbm_scored <- rxPredict(default_model_gbm,
                                  mort_split$validate,
                                 "scored.xdf",
                                predVarNames = c("pred_gbm_default"))
```


```r
# 
# rxRocCurve(actualVarName = "default",
#            predVarNames = c("pred_tree_default",
#                             "pred_logit_default",
#                             "pred_forest_default",
#                             "pred_gbm_default"),
#            data = 'scored.xdf')
```



# More Advanced Topics

## Scoring on Non-XDF Data Sources 
### Using a CSV as a Data Source

- The previous slides focused on using xdf data sources
- Most of the `rx` functions will work on non-xdf data sources
- For training, which is often an iterative process, it is recommended to use xdfs
- For scoring/testing, which requires just one pass through the data, feel free to use raw data!


```r
csv_path <- paste(rxGetOption("sampleDataDir"),
                   "mortDefaultSmall2009.csv",
                   sep = "/")
file.copy(csv_path, "mortDefaultSmall2009.csv", overwrite = TRUE)
```

```
## [1] TRUE
```

```r
mort_csv <- RxTextData("mortDefaultSmall2009.csv")
```

## Regression Tree

- For a slightly different model, we will estimate a regression tree.
- Just change the parameters in the `estimate_model` function


```r
tree_model_ccdebt <- estimate_model(xdf_data = mort_split$train,
                                    form = make_form(mort_split$train,
                                                     "ccDebt",
                                                     vars_to_skip = c("default_flag",
                                                                      "trainvalidate")),
                                    model = rxDTree)
# plot(RevoTreeView::createTreeView(tree_model_ccdebt))
```


## Test on CSV


```r
if (file.exists("mort2009predictions.xdf")) file.remove("mort2009predictions.xdf")
```

```
## [1] TRUE
```



```r
rxPredict(tree_model_ccdebt,
          data = mort_csv,
          outData = "mort2009predictions.xdf",
          writeModelVars = TRUE)

mort_2009_pred <- RxXdfData("mort2009predictions.xdf")
rxGetInfo(mort_2009_pred, numRows = 1)
```

```
## File name: /datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/rmarkdown/mort2009predictions.xdf 
## Number of observations: 10000 
## Number of variables: 7 
## Number of blocks: 1 
## Compression type: zlib 
## Data (1 row starting with row 1):
##   ccDebt_Pred ccDebt creditScore houseAge yearsEmploy year default
## 1    4951.689   3661         701       23           2 2009       0
```

# Multiclass Classification
## Convert Year to Factor

- We have seen how to estimate a binary classification model and a regression tree
- How would we estimate a multiclass classification model?
- Let's try to predict mortgage origination based on other variables
- Use `rxFactors` to convert *year* to a _factor_ variable


```r
mort_xdf_factor <- rxFactors(inData = mort_xdf,
                             factorInfo = c("year"),
                             outFile = "mort_year.xdf",
                             overwrite = TRUE)
```

## Convert Year to Factor

```r
rxGetInfo(mort_xdf_factor, getVarInfo = TRUE, numRows = 4)
```

```
## File name: /datadrive/LearnAnalytics-mr4ds-spark/mr4ds/Student-Resources/rmarkdown/mort_year.xdf 
## Number of observations: 1e+05 
## Number of variables: 7 
## Number of blocks: 10 
## Compression type: zlib 
## Variable information: 
## Var 1: creditScore, Type: integer, Low/High: (470, 925)
## Var 2: houseAge, Type: integer, Low/High: (0, 40)
## Var 3: yearsEmploy, Type: integer, Low/High: (0, 14)
## Var 4: ccDebt, Type: integer, Low/High: (0, 14094)
## Var 5: year
##        10 factor levels: 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009
## Var 6: default, Type: integer, Low/High: (0, 1)
## Var 7: default_flag
##        2 factor levels: current default
## Data (4 rows starting with row 1):
##   creditScore houseAge yearsEmploy ccDebt year default default_flag
## 1         691       16           9   6725 2000       0      current
## 2         691        4           4   5077 2000       0      current
## 3         743       18           3   3080 2000       0      current
## 4         728       22           1   4345 2000       0      current
```

## Estimate Multiclass Classification

- You know the drill! Change the parameters in `estimate_model`:


```r
tree_multiclass_year <- estimate_model(xdf_data = mort_xdf_factor,
                                    form = make_form(mort_xdf_factor,
                                                     "year",
                                                     vars_to_skip = c("default",
                                                                      "trainvalidate")),
                                    model = rxDTree)
```

## Predict Multiclass Classification

- Score the results


```r
multiclass_preds <- rxPredict(tree_multiclass_year,
                              data = mort_xdf_factor,
                              writeModelVars = TRUE,
                              outData = "multi.xdf",
                              overwrite = TRUE)
```

## Predict Multiclass Classification

- View the results
- Predicted/scored column for each level of the response
- Sum up to one

```r
rxGetInfo(multiclass_preds, numRows = 3)
```


## Conclusion
### Thanks for Attending!

- Any questions?
- Try different models!
- Try modeling with `rxDForest`, `rxBTrees`: have significantly higher predictive accuracy, somewhat less interpretability
