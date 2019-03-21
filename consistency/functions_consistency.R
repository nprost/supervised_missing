#' These are the functions used in script_consistency.R

library(rpart)
library(party)
library(ranger)
library(xgboost)
library(MASS)
library(norm)

################################################################################
################################## DATA ########################################
################################################################################
make_data1 <- function(size=100, noise=0.1, prob=0.4, rho=0.5, dim=10) {
    # gaussian features, linear regression, mcar
    Sigma <- rho*matrix(rep(1, dim**2), nrow=dim)+(1-rho)*diag(1, nrow=dim)
    X <- as.data.frame(mvrnorm(size, mu=rep(1, dim), Sigma=Sigma))
    y <- as.matrix(X)%*%c(1, 2, -1, 3, -0.5, -1, 0.3, 1.7, 0.4, -0.3)
    y <- y + rnorm(size, sd=noise)
    M <- matrix(rbinom(size*dim, 1, prob=prob), ncol=dim)
    X[M==1] <- NA
    result <- cbind.data.frame(y, as.data.frame(X))
    colnames(result)[1] <- "y"
    return(result)
}

make_data2 <- function(size=100, noise=0.1, prob=0.4, rho=0.5, dim=10) {
    # gaussian features, nonlinear regression, mcar
    Sigma <- rho*matrix(rep(1, dim**2), nrow=dim)+(1-rho)*diag(1, nrow=dim)
    X <- as.data.frame(mvrnorm(size, mu=rep(1, dim), Sigma=Sigma))
    y <- 10*sin(pi*X[, 1]*X[, 2]) + 20*(X[, 3]-0.5)**2 + 10*X[, 4] + 5*X[, 5]
    y <- y + noise*rnorm(size)
    M <- matrix(rbinom(size*dim, size=1, prob=prob), ncol=dim)
    X[M==1] <- NA
    result <- cbind.data.frame(y, as.data.frame(X))
    colnames(result)[1] <- "y"
    return(result)
}

make_data3 <- function(size=100, noise=0.1, prob=0.4, rho=0.5, dim=10) {
    # nonlinear features, nonlinear regression, mcar
    xnoise <- 0.05
    Xi <- 3*(runif(size)-1)
    X0 <- Xi**2 + rnorm(size, sd=xnoise)
    X1 <- sin(Xi)  + rnorm(size, sd=xnoise)
    X2 <- tanh(Xi) * exp(Xi) * sin(Xi) + rnorm(size, sd=xnoise)
    X3 <- sin(Xi-2) + cos(Xi-3)**3 + rnorm(size, sd=xnoise)
    X4 <- (1-Xi)**3 + rnorm(size, sd=xnoise)
    X5 <- sqrt(sin(Xi**2) + 2) + rnorm(size, sd=xnoise)
    X6 <- Xi - 3 + rnorm(size, sd=xnoise)
    X7 <- (1-Xi) * sin(Xi) * cosh(Xi) + rnorm(size, sd=xnoise)
    X8 <- 1 / (sin(2*Xi) - 2) + rnorm(size, sd=xnoise)
    X9 <- - Xi**4 + rnorm(size, sd=xnoise)
    X <- cbind.data.frame(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9)
    y <- 10*sin(pi*X[, 1]*X[, 2]) + 20*(X[, 3]-0.5)**2 + 10*X[, 4] + 5*X[, 5]
    y <- y + noise*rnorm(size)
    M <- matrix(rbinom(size*dim, size=1, prob=prob), ncol=dim)
    X[M==1] <- NA
    result <- cbind.data.frame(y, as.data.frame(X))
    colnames(result)[1] <- "y"
    return(result)
}


###############################################################################
################################ PREPROCESSING ################################
###############################################################################
imputeEllP <- function(point, Sigma.inv){
    #' Row-wise imputation, vectorized in impute.gaussian.
    point.new <- point
    dim <- length(point)
    index <- which(is.na(point))
    if (length(index) == 1){
        point.new[index] <- - point.new[-index] %*% Sigma.inv[index, -index] /
            Sigma.inv[index, index]
    } else {
        index <- which(is.na(point))
        A <- Sigma.inv[index, index]
        b <- - Sigma.inv[index, (1:dim)[-index], drop=FALSE] %*% point[-index]
        point.new[index] <- solve(A) %*% b
    }
    return(point.new)
}


impute.gaussian <- function(X, parameters) {
    mu <- parameters[["mu"]]
    sigma <- parameters[["sigma"]]
    # ledoit-wolf
    trace.sigma <- sum(diag(sigma))
    shrinkage <- 0.01
    sigma <- (1 - shrinkage) * sigma + shrinkage * trace.sigma * diag(dim(X)[2])

    X.prep <- t(t(as.matrix(X)) - mu)
    Inv.Sigma.tmp <- solve(sigma)
    miss.rowi <- which(rowSums(is.na(X.prep)) > 0.5)
    X.new <- X.prep[miss.rowi, , drop=FALSE]
    X.prep[miss.rowi, ] <- t(apply(X.new, 1, imputeEllP, Inv.Sigma.tmp))
    X.imput <- as.data.frame(t(t(X.prep) + mu))
    colnames(X.imput) <- colnames(X)
    return(X.imput)
}


fit.preprocess <- function(df, strategy, withpattern) {
    #' Get imputation parameters on the train set.
    size <- dim(df)[1]
    dim <- dim(df)[2]
    X <- as.data.frame(df[, 2:dim])
    y <- df[, 1]
    indicators <- is.na(X)

    if (strategy == "mean") {
        mu <- apply(X, 2, mean, na.rm = TRUE)
        parameters <- list(mean=mu)

    } else if (strategy == "gaussian") {
        s <- prelim.norm(as.matrix(X))
        thetahat <- em.norm(s, showits= FALSE, criterion=sqrt(.Machine$double.eps))
        parameters <- getparam.norm(s, thetahat)

    } else parameters <- list()

    return(parameters)
}


preprocess <- function(df, strategy, withpattern, parameters) {
    size <- dim(df)[1]
    dim <- dim(df)[2]
    X <- as.data.frame(df[, 2:dim])
    y <- df[, 1]
    indicators <- is.na(X)

    if (strategy == "mean") {
        mu <- parameters[["mean"]]
        X.imput <- sapply(1:ncol(X),
            function(x) ifelse(is.na(X[, x]), mu[x], X[, x]))
        X.imput <- as.data.frame(X.imput)
        colnames(X.imput) <- colnames(X)

    } else if (strategy == "gaussian") {
        X.imput <- impute.gaussian(X, parameters)

    } else if (strategy == "mia") {
        # encode MIA by duplicating columns and imputing by +/- "inf"
        Xm <- X
        Xm[indicators] <- -10**100
        Xp <- X
        Xp[indicators] <- +10**100
        X.imput <- cbind.data.frame(Xm, Xp)
    } else if (strategy == "none") {
        X.imput <- X
    }

    if (withpattern==TRUE) {
        indicators.factor <- data.frame(apply(indicators, 2, as.factor))
        indicators.addfactor <- data.frame(lapply(indicators.factor,
            function(col) factor(col, levels=c("FALSE", "TRUE"))))
        indsansNA <- which((sapply(indicators.factor, nlevels)) == 1)
        X.imput <- cbind.data.frame(X.imput, indicators.addfactor[, -indsansNA])
    }

    result <- data.frame(cbind(y, X.imput))
    colnames(result)[1] <- "y"
    return(result)
}



################################################################################
################################ SCORES ########################################
################################################################################
run_scores <- function(model, strategy, withpattern, dataset,
                       sizes, n_rep, prob, n_features, noise, rho,
                       min_samples_leaf, seed, num.threads.ranger, debugging=FALSE) {
    #' model: "rpart", "ctree", "ranger", "xgboost"
    #' strategy: "none", "mean", "gaussian", "mia"
    #' withpattern: TRUE, FALSE
    #' dataset: "make_model1", ..., "make_model6"

    #' ctree with mia is buggy. TODO: implement partykit's mia

    # local seed
    old <- .Random.seed
    on.exit( { .Random.seed <<- old } )
    set.seed(seed)

    result <- matrix(0, nrow=n_rep, ncol=length(sizes))
    iter.size <- 0
    for (size in sizes) {
        iter.size <- iter.size + 1

        for (k in 1:n_rep) {

            # get or match.fun do not seem to work in parallel...
            if (dataset == "make_data1") {
                data.raw <- make_data1(size, noise, prob, rho, n_features)
            } else if (dataset == "make_data2") {
                data.raw <- make_data2(size, noise, prob, rho, n_features)
            } else if (dataset == "make_data3") {
                data.raw <- make_data3(size, noise, prob, rho, n_features)
            } else if (dataset == "make_data4") {
                data.raw <- make_data4(size, noise, prob, rho, n_features)
            } else if (dataset == "make_data5") {
                data.raw <- make_data5(size, noise, prob, rho, n_features)
            } else if (dataset == "make_data6") {
                data.raw <- make_data6(size, noise, prob, rho, n_features)
            } else stop("Invalid dataset")

            n_test <- floor(size * 0.2)
            test.raw <- data.raw[1:n_test, ]
            train.raw <- data.raw[(n_test+1):size, ]

            param <- fit.preprocess(train.raw, strategy, withpattern)
            train <- preprocess(train.raw, strategy, withpattern, param)
            test <- preprocess(test.raw, strategy, withpattern, param)

            if (model == "rpart") {
                reg <- rpart(y~., data=train, control=rpart.control(
                    minbucket=min_samples_leaf,cp=0.0, xval=1))
                res <- predict(reg, subset(test, select=-c(y)), type="vector")
            } else if (model == "ctree") {
                reg <- ctree(y~., data=train, controls=ctree_control(
                    minbucket=min_samples_leaf, mincriterion=0.0))
                res <- predict(reg, subset(test, select=-c(y)))
            } else if (model == "ranger") {
                reg <- ranger(y~., data=train, num.threads=num.threads.ranger, verbose=F)
                res <- predict(reg, subset(test, select=-c(y)))$predictions
            } else if (model == "xgboost") {
                sink('sink')
                reg <- xgboost(data=as.matrix(subset(train, select=-c(y))) ,label=as.matrix(train$y)
                    ,nrounds=500 ,nthread=num.threads.ranger
                    )
                res <- predict(reg, as.matrix(subset(test, select=-c(y))))
                sink()
           } else stop("Invalid model")

            result[k, iter.size] = mean((test$y - res)**2)
        }

        print.mess <- paste0(
            "\nDataset: ", dataset, ", model: ", model, ", strategy: ",
            strategy, ", withpattern: ", withpattern, ", size: ", size, "...\n")
        message(print.mess,"\r",appendLF=FALSE)
        flush.console()
    }

    print.mess <- paste0(
        "\nDataset: ", dataset, ", model: ", model, ", strategy: ",
        strategy, ", withpattern: ", withpattern, ", DONE!\n")
    message(print.mess,"\r",appendLF=FALSE)
    flush.console()

    if (debugging) {
        return(result)
    } else {
        write.table(result, sep=" ", row.names=FALSE,
            file=paste0("results/", dataset, "_", model, "_",
                        strategy, "_", withpattern, ".csv"))
    }
}

