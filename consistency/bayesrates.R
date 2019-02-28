library(rpart)
library(party)
library(ranger)
library(MASS)
library(norm)

# results:
# bayes_data1() = 0.8087995
# bayes_data2() = 0.7484916
# bayes_data3() = 0.9951507

megasize <- 10**6
n_repmc <- 10
n_repmi <- 100

rngseed(42)

bayes_data1 <- function(
size=megasize, noise=0.1, prob=0.4, rho=0.5, dim=10, verbose=F) {
    VARY <- 25
    thisscore <- rep(0, n_repmc)
    for (k in 1:n_repmc) {
        cat(paste0(k,"..."))
        # gaussian features, linear regression, mcar
        Sigma <- rho*matrix(rep(1, dim**2), nrow=dim)+(1-rho)*diag(1, nrow=dim)
        X <- as.data.frame(mvrnorm(size, mu=rep(1, dim), Sigma=Sigma))
        y <- as.matrix(X)%*%c(1, 2, -1, 3, -0.5, -1, 0.3, 1.7, 0.4, -0.3)
        y <- y + rnorm(size, sd=noise)
        M <- matrix(rbinom(size*dim, 1, prob=prob), ncol=dim)
        X[M==1] <- NA

        # multiple imputation
        res <- rep(0, megasize)
        cat('\n')
        s <- prelim.norm(as.matrix(X))
        thetahat <- em.norm(s, showits=FALSE)
        for (t in 1:n_repmi) {
            if (verbose) cat(paste0(t,"..."))
            X_imp <- imp.norm(s, thetahat, as.matrix(X))
            res <- res + X_imp%*%c(1, 2, -1, 3, -0.5, -1, 0.3, 1.7, 0.4, -0.3)
        }
        if (verbose) cat('\n')
        res <- res / n_repmi

        thisscore[k] <- 1 - mean((y - res)**2) / VARY
    }
    cat('\n')
    return(mean(thisscore))
}

bayes_data2 <- function(
size=megasize, noise=0.1, prob=0.4, rho=0.5, dim=10, verbose=F) {
    VARY <- 1702
    thisscore <- rep(0, n_repmc)
    for (k in 1:n_repmc) {
        cat(paste0(k,"..."))
        # gaussian features, nonlinear regression, mcar
        Sigma <- rho*matrix(rep(1, dim**2), nrow=dim)+(1-rho)*diag(1, nrow=dim)
        X <- as.data.frame(mvrnorm(size, mu=rep(1, dim), Sigma=Sigma))
        y <- 10 * sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3]-0.5)**2 + 10 * X[, 4] + 5 * X[, 5]
        y <- y + noise*rnorm(size)
        M <- matrix(rbinom(size*dim, size=1, prob=prob), ncol=dim)
        X[M==1] <- NA

        # multiple imputation
        res <- rep(0, megasize)
        cat('\n')
        s <- prelim.norm(as.matrix(X))
        thetahat <- em.norm(s, showits=FALSE)
        for (t in 1:n_repmi) {
            if (verbose) cat(paste0(t,"..."))
            X_imp <- imp.norm(s, thetahat, as.matrix(X))
            res <- res + 10*sin(pi * X_imp[, 1] * X_imp[, 2]) + 20 * (X_imp[, 3]-0.5)**2 +
                10 * X_imp[, 4] + 5 * X_imp[, 5]
        }
        if (verbose) cat('\n')
        res <- res / n_repmi

        thisscore[k] <- 1 - mean((y - res)**2) / VARY
    }
    cat("\n")
    return(mean(thisscore))
}

bayes_data3 <- function(
size=megasize, noise=0.1, prob=0.4, rho=0.5, dim=10, verbose=F) {
    VARY <- 10825
    thisscore <- rep(0, n_repmc)
    for (k in 1:n_repmc) {
        cat(paste0(k,"..."))
        # nonlinear features, nonlinear regression, mcar
        xnoise <- 0.05
        Xi <- 3*(runif(size)-1)
        X0 <- Xi**2 + rnorm(size, sd=xnoise)
        X1 <- sin(Xi)  + rnorm(size, sd=xnoise)
        X2 <- tanh(Xi)*exp(Xi)*sin(Xi) + rnorm(size, sd=xnoise)
        X3 <- sin(Xi-2)+cos(Xi-3)**3 + rnorm(size, sd=xnoise)
        X4 <- (1-Xi)**3 + rnorm(size, sd=xnoise)
        X5 <- sqrt(sin(Xi**2)+2) + rnorm(size, sd=xnoise)
        X6 <- Xi-3 + rnorm(size, sd=xnoise)
        X7 <- (1-Xi)*sin(Xi)*cosh(Xi) + rnorm(size, sd=xnoise)
        X8 <- 1/(sin(2*Xi)-2) + rnorm(size, sd=xnoise)
        X9 <- -Xi**4 + rnorm(size, sd=xnoise)
        X <- cbind.data.frame(X0,X1,X2,X3,X4,X5,X6,X7,X8, X9)
        y <- 10*sin(pi * X[, 1] * X[, 2]) + 20 * (X[, 3] - 0.5)**2 + 10 * X[, 4] + 5 * X[, 5]
        y <- y + noise*rnorm(size)
        M <- matrix(rbinom(size*dim, size=1, prob=prob), ncol=dim)
        X[M==1] <- NA

        # multiple imputation
        res <- rep(0, megasize)
        cat('\n')
        s <- prelim.norm(as.matrix(X))
        thetahat <- em.norm(s, showits=FALSE)
        for (t in 1:n_repmi) {
            if (verbose) cat(paste0(t,"..."))
            X_imp <- imp.norm(s, thetahat, as.matrix(X))
            res <- res + 10 * sin(pi * X_imp[, 1] * X_imp[, 2]) + 20 * (X_imp[, 3] - 0.5)**2 +
                10 * X_imp[, 4] + 5 * X_imp[, 5]
        }
        if (verbose) cat('\n')
        res <- res / n_repmi

        thisscore[k] <- 1 - mean((y - res)**2) / VARY
    }
    cat('\n')
    return(mean(thisscore))
}

