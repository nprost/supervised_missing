load("results/boxplot_MCAR.RData")
load("results/boxplot_MAR.RData")
load("results/boxplot_MNAR.RData")
# these are of lists of matrices of shape n_rep*length(sizes)

names(scores_mcar) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "1. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "1. MIA (forest)")
names(scores_mar) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "1. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "1. MIA (forest)")
names(scores_mnar) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "1. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "1. MIA (forest)")

dir.create(file.path('../figures'), showWarnings=FALSE)

library(ggplot2)
library(gridExtra)
library(viridis)

# mse to explained variance
VARY <- 10
scores_mcar <- lapply(scores_mcar, function(x) 1-x/VARY)
scores_mar <- lapply(scores_mar, function(x) 1-x/VARY)
scores_mnar <- lapply(scores_mnar, function(x) 1-x/VARY)

# relative
scores_mcar <- lapply(scores_mcar, function(x) x-Reduce("+", scores_mcar) / length(scores_mcar))
scores_mar <- lapply(scores_mar, function(x) x-Reduce("+", scores_mar) / length(scores_mar))
scores_mnar <- lapply(scores_mnar, function(x) x-Reduce("+", scores_mnar) / length(scores_mnar))

pdf('../figures/boxplot_mcar.pdf', width=9.5, height=13)
aa <- cbind.data.frame(unlist(scores_mcar), rep(names(scores_mcar), each = 500))
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
    facet_wrap(~forest, nrow=2, scales = "free_y")
dev.off()

pdf('../figures/boxplot_mar.pdf', height=13)
aa <- cbind.data.frame(unlist(scores_mar), rep(names(scores_mar), each = 500))
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

pdf('../figures/boxplot_mnar.pdf', height=13)
aa <- cbind.data.frame(unlist(scores_mnar), rep(names(scores_mnar), each = 500))
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

