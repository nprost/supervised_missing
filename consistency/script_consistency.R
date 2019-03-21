#!/usr/bin/env Rscript
cmd.args = commandArgs(trailingOnly=TRUE)
cmd.args = sapply(cmd.args, as.numeric)

if (length(cmd.args) > 0) {
    n.jobs = cmd.args[1]
} else n.jobs = 1

if (length(cmd.args) > 1) {
    num.threads = cmd.args[2]
} else num.threads = 1


# This runs consistency results (score: MSE) for different parameters,
# in parallel, with n.jobs jobs and num.threads.ranger cores per forest

sizes <- floor(10**seq(2,5,length.out=20))
n_rep <- 200
prob <- 0.4
n_features <- 10
noise <- 0.1
min_samples_leaf <- 30
rho <- 0.5

dir.create(file.path('results'), showWarnings=FALSE)

cat("This script runs consistency results for
    * three datasets: 1. linear, 2. gaussian Friedman, 3. nongaussian Friedman
    * one mechanism: MCAR
    * four models: rpart, ctree, ranger (RF), xgboost
    * several methods: surrogates (rpart and ctree) (+ mask), gaussian imputation (+ mask),
    mean imputation (+ mask), MIA, block propagation (xgboost)
    ")
cat("Starting R script\n")

set.seed(42)  # so .Random.seed exists

library(norm)
rngseed(123)  # for the EM imputation

################################################################################
library(doParallel)
library(doSNOW)

cl <- makeCluster(n.jobs, outfile="")
registerDoSNOW(cl)

iter.seed <- 15

foreach (param = list(
    list(dataset="make_data1", model='rpart', strategy='none', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='rpart', strategy='none', withpattern=TRUE)
    ,
    list(dataset="make_data1", model='rpart', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='rpart', strategy='mean', withpattern=TRUE)
    ,
    list(dataset="make_data1", model='rpart', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='rpart', strategy='gaussian', withpattern=TRUE)
    ,
    list(dataset="make_data1", model='rpart', strategy='mia', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='ctree', strategy='none', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='ranger', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='ranger', strategy='mean', withpattern=TRUE)
    ,
    list(dataset="make_data1", model='ranger', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='ranger', strategy='gaussian', withpattern=TRUE)
    ,
    list(dataset="make_data1", model='ranger', strategy='mia', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='xgboost', strategy='none', withpattern=FALSE)
    , 
    list(dataset="make_data1", model='xgboost', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='xgboost', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data1", model='xgboost', strategy='mia', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='rpart', strategy='none', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='rpart', strategy='none', withpattern=TRUE)
    ,
    list(dataset="make_data2", model='rpart', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='rpart', strategy='mean', withpattern=TRUE)
    ,
    list(dataset="make_data2", model='rpart', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='rpart', strategy='gaussian', withpattern=TRUE)
    ,
    list(dataset="make_data2", model='rpart', strategy='mia', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='ctree', strategy='none', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='ranger', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='ranger', strategy='mean', withpattern=TRUE)
    ,
    list(dataset="make_data2", model='ranger', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='ranger', strategy='gaussian', withpattern=TRUE)
    ,
    list(dataset="make_data2", model='ranger', strategy='mia', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='xgboost', strategy='none', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='xgboost', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='xgboost', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data2", model='xgboost', strategy='mia', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='rpart', strategy='none', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='rpart', strategy='none', withpattern=TRUE)
    ,
    list(dataset="make_data3", model='rpart', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='rpart', strategy='mean', withpattern=TRUE)
    ,
    list(dataset="make_data3", model='rpart', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='rpart', strategy='gaussian', withpattern=TRUE)
    ,
    list(dataset="make_data3", model='rpart', strategy='mia', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='ctree', strategy='none', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='ranger', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='ranger', strategy='mean', withpattern=TRUE)
    ,
    list(dataset="make_data3", model='ranger', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='ranger', strategy='gaussian', withpattern=TRUE)
    ,
    list(dataset="make_data3", model='ranger', strategy='mia', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='xgboost', strategy='none', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='xgboost', strategy='mean', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='xgboost', strategy='gaussian', withpattern=FALSE)
    ,
    list(dataset="make_data3", model='xgboost', strategy='mia', withpattern=FALSE)
)) %dopar% {
    iter.seed <- iter.seed + 1
    source('functions_consistency.R')
    run_scores(model=param$model, strategy=param$strategy, withpattern=param$withpattern,
               dataset=param$dataset,
               sizes=sizes, n_rep=n_rep, prob=prob, n_features=n_features, noise=noise,
               min_samples_leaf=min_samples_leaf, rho=rho, seed=iter.seed,
               num.threads.ranger=num.threads, debugging=FALSE)
}

stopCluster(cl)
