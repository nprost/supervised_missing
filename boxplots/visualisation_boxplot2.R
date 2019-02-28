load("results/boxplot2_1.RData")
load("results/boxplot2_2.RData")
load("results/boxplot2_3.RData")
# these are of lists of matrices of shape n_rep*length(sizes)

names(scores_21) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "1. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "1. MIA (forest)")
names(scores_22) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "1. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "1. MIA (forest)")
names(scores_23) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "1. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "1. MIA (forest)")

library(ggplot2)
library(gridExtra)
library(viridis)

# mse to explained variance
VARY1 <- 25.4
scores_21 <- lapply(scores_21, function(x) 1-x/VARY1)
VARY2 <- 1710
scores_22 <- lapply(scores_22, function(x) 1-x/VARY2)
VARY3 <- 10820
scores_23 <- lapply(scores_23, function(x) 1-x/VARY3)

# relative
scores_21 <- lapply(scores_21, function(x) x-Reduce("+", scores_21) / length(scores_21))
scores_22 <- lapply(scores_22, function(x) x-Reduce("+", scores_22) / length(scores_22))
scores_23 <- lapply(scores_23, function(x) x-Reduce("+", scores_23) / length(scores_23))

pdf('../figures/boxplot_linearlinear.pdf', width=9.5, height=13)
aa <- cbind.data.frame(unlist(scores_21), rep(names(scores_21), each = 500))
colnames(aa) <- c("score", "method")
aa$forest <- as.factor(ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE"))
aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\+\\ mask", "\n\\+\\ mask", aa$method))

ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    geom_boxplot(outlier.alpha = 0.5, fill = viridis(14)) +
    scale_y_continuous(breaks=c(-0.1, -0.05, 0, 0.05, 0.1),
                       labels=c("-0,1", "-0,05", "0,0", "+0.05", "+0,1")) +
    theme(axis.text.x = element_text(face="bold", size=22),
          axis.text.y = element_text(face="bold", size=20, angle=15),
          axis.title.x = element_text(face="bold", size=22),
          panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
          strip.text.x = element_text(size=22)) +
    labs(title = "", x  = "", y = "Relative explained variance") +
    coord_flip() +
    facet_wrap(~forest, nrow=2, scales="free_y")
dev.off()

pdf('../figures/boxplot_linearnonlinear.pdf', height=13)
aa <- cbind.data.frame(unlist(scores_22), rep(names(scores_22), each = 500))
colnames(aa) <- c("score", "method")
aa$forest <- as.factor(ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE"))
aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\+\\ mask", "\n\\+\\ mask", aa$method))
ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    geom_boxplot(outlier.alpha = 0.5, fill = viridis(14)) +
    scale_y_continuous(breaks=c(-0.4, -0.2, 0, 0.2, 0.4),
                       labels=c("-0,4", "-0,2", "0,0", "+0.2", "+0,4")) +
    theme(axis.text.x = element_text(face="bold", size=22),
          axis.text.y = element_blank(),
          axis.title.x = element_text(face="bold", size=22),
          panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
          strip.text.x = element_text(size=22)) +
    labs(title = "", x  = "", y = "Relative explained variance") +
    coord_flip() +
    facet_wrap(~forest, nrow=2, scales = "free_y")
dev.off()

pdf('../figures/boxplot_nonlinearnonlinear.pdf', height=13)
aa <- cbind.data.frame(unlist(scores_23), rep(names(scores_23), each = 500))
colnames(aa) <- c("score", "method")
aa$forest <- as.factor(ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE"))
aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\+\\ mask", "\n\\+\\ mask", aa$method))
ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    geom_boxplot(outlier.alpha = 0.5, fill = viridis(14)) +
    scale_y_continuous(breaks=c(-0.002, -0.001, 0, 0.001, 0.002),
                       labels=c("-0,002", "-0,001", "0,0", "+0,001", "+0,002")) +
    theme(axis.text.x = element_text(face="bold", size=22),
          axis.text.y = element_blank(),
          axis.title.x = element_text(face="bold", size=22),
          panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
          strip.text.x = element_text(size=22)) +
    labs(title = "", x  = "", y = "Relative explained variance") +
    coord_flip() +
    facet_wrap(~forest, nrow=2, scales = "free_y")
dev.off()


