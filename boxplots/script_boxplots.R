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

dir.create(file.path('results'), showWarnings=FALSE)

cat("This script runs consistency results for
    * one dataset: Gaussian X, Y=X*2+noise
    * three mechanisms: 1. MCAR, 2. MAR, 3. MNAR
    * two tree models: rpart, ctree, and two forest models: ranger, xgboost
    * several methods: surrogates (not RF) (+ mask), gaussian imputation (+ mask),
    mean imputation (+ mask), MIA, block propagation (xgboost)
    ")
cat("Starting R script\n")

set.seed(42)  # so .Random.seed exists

library(norm)
rngseed(123)  # for the EM imputation

# 1 for figure 3, 2 for figure 4, -1 for both
boxplot_choice <- -1

################################################################################
library(doParallel)
library(doSNOW)

cl <- makeCluster(n.jobs, outfile="")
registerDoSNOW(cl)

Parallel <- function(dataset, n_features, num.threads.ranger=num.threads) {
    iter.seed <- 15
    sizes <- c(1000)
    n_rep <- 500
    prob <- 0.2
    noise = 0.1
    min_samples_leaf = 30
    rho = 0.5
    results.list <- foreach (param = list(
        #list(dataset=dataset, model='rpart', strategy='none', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='rpart', strategy='none', withpattern=TRUE)
        #,
        #list(dataset=dataset, model='rpart', strategy='mean', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='rpart', strategy='mean', withpattern=TRUE)
        #,
        #list(dataset=dataset, model='rpart', strategy='gaussian', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='rpart', strategy='gaussian', withpattern=TRUE)
        #,
        #list(dataset=dataset, model='rpart', strategy='mia', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='ctree', strategy='none', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='ctree', strategy='none', withpattern=TRUE)
        #,
        #list(dataset=dataset, model='xgboost_onetree', strategy='none', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='ranger', strategy='mean', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='ranger', strategy='mean', withpattern=TRUE)
        #,
        #list(dataset=dataset, model='ranger', strategy='gaussian', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='ranger', strategy='gaussian', withpattern=TRUE)
        #,
        #list(dataset=dataset, model='ranger', strategy='mia', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='xgboost', strategy='none', withpattern=FALSE)
        #,
        #list(dataset=dataset, model='xgboost', strategy='mean', withpattern=FALSE)
        #,
        list(dataset=dataset, model='xgboost', strategy='mean', withpattern=TRUE)
        ,
        #list(dataset=dataset, model='xgboost', strategy='gaussian', withpattern=FALSE)
        #,
        list(dataset=dataset, model='xgboost', strategy='gaussian', withpattern=TRUE)
        #,
        #list(dataset=dataset, model='xgboost', strategy='mia', withpattern=FALSE)
    )) %dopar% {
        iter.seed <- iter.seed + 1
        source('functions_boxplots.R')
        run_scores(model=param$model, strategy=param$strategy, withpattern=param$withpattern,
                   dataset=param$dataset,
                   sizes=sizes, n_rep=n_rep, prob=prob, n_features=n_features, noise=noise,
                   min_samples_leaf=min_samples_leaf, rho=rho, seed=iter.seed,
                   num.threads.ranger=num.threads.ranger)
    }
    # quick and very dirty
    names(results.list) <- c("xgboost mean + mask", "xgboost gaussian + mask")
    return(results.list)
}

if (boxplot_choice == 1 | boxplot_choice == -1) {
    if (file.exists("results/boxplot_MCAR.RData")) {
        load("results/boxplot_MCAR.RData")
        res <- Parallel("make_data1", n_features=3)
        scores_mcar <- modifyList(scores_mcar, res)
    } else scores_mcar <- Parallel("make_data1", n_features=3)
    save(scores_mcar, file="results/boxplot_MCAR.RData")

    #if (file.exists("results/boxplot_MAR.RData")) {
    #    load("results/boxplot_MAR.RData")
    #    scores_mar <- modifyList(scores_mar, Parallel("make_data2", n_features=3))
    #} else scores_mar <- Parallel("make_data2", n_features=3)
    #save(scores_mar, file="results/boxplot_MAR.RData")

    if (file.exists("results/boxplot_MNAR.RData")) {
        load("results/boxplot_MNAR.RData")
        scores_mnar <- modifyList(scores_mnar, Parallel("make_data3", n_features=3))
    } else scores_mnar <- Parallel("make_data3", n_features=3)
    save(scores_mnar, file="results/boxplot_MNAR.RData")

    if (file.exists("results/boxplot_PRED.RData")) {
        load("results/boxplot_PRED.RData")
        scores_pred <- modifyList(scores_pred, Parallel("make_data3bis", n_features=3))
    } else scores_pred <- Parallel("make_data3bis", n_features=3)
    save(scores_pred, file="results/boxplot_PRED.RData")

} 

if (boxplot_choice == 2 || boxplot_choice == -1) {
    if (file.exists("results/boxplot2_1.RData")) {
        load("results/boxplot2_1.RData")
        scores_21 <- modifyList(scores_21, Parallel("make_data4", n_features=10))
    } else scores_21 <- Parallel("make_data4", n_features=10)
    save(scores_21, file="results/boxplot2_1.RData")

    if (file.exists("results/boxplot2_2.RData")) {
        load("results/boxplot2_2.RData")
        scores_22 <- modifyList(scores_22, Parallel("make_data5", n_features=10))
    } else scores_22 <- Parallel("make_data5", n_features=10)
    save(scores_22, file="results/boxplot2_2.RData")

    if (file.exists("results/boxplot2_3.RData")) {
        load("results/boxplot2_3.RData")
        scores_23 <- modifyList(scores_23, Parallel("make_data6", n_features=10))
    } else scores_23 <- Parallel("make_data6", n_features=10)
    save(scores_23, file="results/boxplot2_3.RData")

} else stop("Invalid boxplot_choice")

stopCluster(cl)
