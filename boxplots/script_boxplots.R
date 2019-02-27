#!/usr/bin/env Rscript
cmd.args = commandArgs(trailingOnly=TRUE)
cmd.args = sapply(cmd.args, as.numeric)
n_cores = cmd.args[1]

# This runs consistency results (score: MSE) for different parameters,
# in parallel, with n_cores cores.

dir.create(file.path('results'), showWarnings=FALSE)

cat("This script runs consistency results for
    * one dataset: Gaussian X, Y=X*2+noise
    * three mechanisms: 1. MCAR, 2. MAR, 3. MNAR
    * three models: rpart, ctree, ranger (RF)
    * several methods: surrogates (not RF) (+ mask), gaussian imputation (+ mask),
    mean imputation (+ mask), MIA
    ")
#Sys.sleep(5)
cat("Starting R script\n")

set.seed(42)  # so .Random.seed exists

library(norm)
rngseed(123)  # for the EM imputation

################################################################################
library(doParallel)
library(doSNOW)

cl <- makeCluster(n_cores, outfile="")
registerDoSNOW(cl)

Parallel <- function(dataset) {
    iter.seed <- 15
    sizes = c(1000)
    n_rep = 500
    prob = 0.2
    #n_features = 3  # for boxplot 1
    n_features = 10  # forboxplot 2
    #noise = 2
    noise = 0.1
    min_samples_leaf = 30
    rho = 0.5
    results.list <- foreach (param = list(
        list(dataset=dataset, model='rpart', strategy='none', withpattern=FALSE),
        list(dataset=dataset, model='rpart', strategy='none', withpattern=TRUE),
        list(dataset=dataset, model='rpart', strategy='mean', withpattern=FALSE),
        list(dataset=dataset, model='rpart', strategy='mean', withpattern=TRUE),
        list(dataset=dataset, model='rpart', strategy='gaussian', withpattern=FALSE),
        list(dataset=dataset, model='rpart', strategy='gaussian', withpattern=TRUE),
        list(dataset=dataset, model='rpart', strategy='mia', withpattern=FALSE),
        list(dataset=dataset, model='ctree', strategy='none', withpattern=FALSE),
        list(dataset=dataset, model='ctree', strategy='none', withpattern=TRUE),
        list(dataset=dataset, model='ranger', strategy='mean', withpattern=FALSE),
        list(dataset=dataset, model='ranger', strategy='mean', withpattern=TRUE),
        list(dataset=dataset, model='ranger', strategy='gaussian', withpattern=FALSE),
        list(dataset=dataset, model='ranger', strategy='gaussian', withpattern=TRUE),
        list(dataset=dataset, model='ranger', strategy='mia', withpattern=FALSE)
    )) %dopar% {
        iter.seed <- iter.seed + 1
        source('functions_boxplots.R')
        run_scores(model=param$model, strategy=param$strategy, withpattern=param$withpattern,
                   dataset=param$dataset,
                   sizes=sizes, n_rep=n_rep, prob=prob, n_features=n_features, noise=noise,
                   min_samples_leaf=min_samples_leaf, rho=rho, seed=iter.seed, debug=FALSE)
    }
    names(results.list) <- c(
        "rpart (surrogates)", "rpart (surrogates) + mask", "impute mean", "impute mean + mask",
        "impute Gaussian", "impute Gaussian + mask", "MIA",
        "ctree (surrogates)", "ctree (surrogates) + mask",
        "impute mean (forest)", "impute mean + mask (forest)",
        "impute Gaussian (forest)", "impute Gaussian + mask (forest)", "MIA (forest)")
    return(results.list)
}

# boxplot 1
#scores_mcar <- Parallel("make_data1")
#save(scores_mcar, file="results/boxplot_MCAR.RData")
#scores_mar <- Parallel("make_data2")
#save(scores_mar, file="results/boxplot_MAR.RData")
#scores_mnar <- Parallel("make_data3")
#save(scores_mnar, file="results/boxplot_MNAR.RData")
# boxplot 2
scores_21 <- Parallel("make_data4")
save(scores_21, file="results/boxplot2_1.RData")
scores_22 <- Parallel("make_data5")
save(scores_22, file="results/boxplot2_2.RData")
scores_23 <- Parallel("make_data6")
save(scores_23, file="results/boxplot2_3.RData")

stopCluster(cl)
