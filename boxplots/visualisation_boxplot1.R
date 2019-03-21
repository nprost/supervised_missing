load("results/boxplot_MCAR.RData")
load("results/boxplot_MNAR.RData")
load("results/boxplot_PRED.RData")
# these are of lists of matrices of shape n_rep*length(sizes)

names(scores_mcar) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "0. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "0. MIA (forest)",
    "1. block (forest)", "1.block", "1. block (xgboost)",
    "3. impute mean (xgboost)",
    "5. impute Gaussian (xgboost)", "0. MIA (xgboost)"
    ,"2. impute mean + mask (xgboost)", "4. impute Gaussian + mask (xgboost)"
    )
names(scores_mnar) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "0. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "0. MIA (forest)",
    "1. block (forest)", "1.block", "1. block (xgboost)",
    "3. impute mean (xgboost)",
    "5. impute Gaussian (xgboost)", "0. MIA (xgboost)"
    ,"2. impute mean + mask (xgboost)", "4. impute Gaussian + mask (xgboost)"
    )
names(scores_pred) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", "0. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", "0. MIA (forest)",
    "1. block (forest)", "1.block", "1. block (xgboost)",
    "3. impute mean (xgboost)",
    "5. impute Gaussian (xgboost)", "0. MIA (xgboost)"
    ,"2. impute mean + mask (xgboost)", "4. impute Gaussian + mask (xgboost)"
    )

# remove block / block forest
scores_mcar <- scores_mcar[-c(15, 16)]
scores_mnar <- scores_mnar[-c(15, 16)]
scores_pred <- scores_pred[-c(15, 16)]


dir.create(file.path('../figures'), showWarnings=FALSE)

library(ggplot2)
library(gridExtra)
library(viridis)
library(grid)

# mse to explained variance
VARY <- 10
scores_mcar <- lapply(scores_mcar, function(x) 1-x/VARY)
scores_mnar <- lapply(scores_mnar, function(x) 1-x/VARY)
scores_pred <- lapply(scores_pred, function(x) 1-x/VARY)

cols <- c('#1f77b4','#ff7f0e','#2ca02c','#d62728','#9467bd',
          '#8c564b','#e377c2','#7f7f7f','#bcbd22','#17becf')

# relative
scores_mcar[1:9] <- lapply(scores_mcar[1:9], function(x) x-Reduce("+", scores_mcar[1:9]) / length(scores_mcar[1:9]))
scores_mcar[10:14] <- lapply(scores_mcar[10:14], function(x) x-Reduce("+", scores_mcar[10:14]) / length(scores_mcar[10:14]))
scores_mcar[15:20] <- lapply(scores_mcar[15:20], function(x) x-Reduce("+", scores_mcar[15:20]) / length(scores_mcar[15:20]))
scores_mnar[1:9] <- lapply(scores_mnar[1:9], function(x) x-Reduce("+", scores_mnar[1:9]) / length(scores_mnar[1:9]))
scores_mnar[10:14] <- lapply(scores_mnar[10:14], function(x) x-Reduce("+", scores_mnar[10:14]) / length(scores_mnar[10:14]))
scores_mnar[15:20] <- lapply(scores_mnar[15:20], function(x) x-Reduce("+", scores_mnar[15:20]) / length(scores_mnar[15:20]))
scores_pred[1:9] <- lapply(scores_pred[1:9], function(x) x-Reduce("+", scores_pred[1:9]) / length(scores_pred[1:9]))
scores_pred[10:14] <- lapply(scores_pred[10:14], function(x) x-Reduce("+", scores_pred[10:14]) / length(scores_pred[10:14]))
scores_pred[15:20] <- lapply(scores_pred[15:20], function(x) x-Reduce("+", scores_pred[15:20]) / length(scores_mnar[15:20]))

pdf('../figures/boxplot_mcar.pdf', width=9.5, height=13)
aa <- cbind.data.frame(unlist(scores_mcar), rep(names(scores_mcar), each = 500))
colnames(aa) <- c("score", "method")
aa$forest <- as.factor(ifelse(grepl("xgboost", aa$method), "XGBOOST", ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE")))
aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\(xgboost\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\+\\ mask", "\n\\+\\ mask", aa$method))
g <- ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    geom_boxplot(outlier.alpha = 0.5, fill = c(cols[3],cols[2],cols[2],cols[4],cols[4],cols[1],cols[1],cols[7],cols[7],cols[3],cols[2],cols[2],cols[4],cols[4],cols[3],cols[6],cols[2],cols[2],cols[4],cols[4])) +
    scale_y_continuous(labels=function(x) paste0(symnum(x, c(-Inf, 0, Inf), c("", "+")), x)) +
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
    facet_wrap(~forest, nrow=3, scales = "free")
gt <- ggplot_gtable(ggplot_build(g))
i=13; gt$heights[i] = 5/9 * gt$heights[i]
gt$heights[18] = 6/9 * gt$heights[18]
grid.draw(gt)
dev.off()

pdf('../figures/boxplot_mnar.pdf', height=13)
aa <- cbind.data.frame(unlist(scores_mnar), rep(names(scores_mnar), each = 500))
colnames(aa) <- c("score", "method")
aa$forest <- as.factor(ifelse(grepl("xgboost", aa$method), "XGBOOST", ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE")))
aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\(xgboost\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\+\\ mask", "\n\\+\\ mask", aa$method))
g <- ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    #geom_boxplot(outlier.alpha = 0.5, fill = viridis(20)) +
    geom_boxplot(outlier.alpha = 0.5, fill = c(cols[3],cols[2],cols[2],cols[4],cols[4],cols[1],cols[1],cols[7],cols[7],cols[3],cols[2],cols[2],cols[4],cols[4],cols[3],cols[6],cols[2],cols[2],cols[4],cols[4])) +
    scale_y_continuous(labels=function(x) paste0(symnum(x, c(-Inf, 0, Inf), c("", "+")), x)) +
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
    facet_wrap(~forest, nrow=3, scales = "free")
gt <- ggplot_gtable(ggplot_build(g))
i=13; gt$heights[i] = 5/9 * gt$heights[i]
gt$heights[18] = 6/9 * gt$heights[18]
grid.draw(gt)
dev.off()

pdf('../figures/boxplot_pred.pdf', height=13)
aa <- cbind.data.frame(unlist(scores_pred), rep(names(scores_pred), each = 500))
colnames(aa) <- c("score", "method")
aa$forest <- as.factor(ifelse(grepl("xgboost", aa$method), "XGBOOST", ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE")))
aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\(xgboost\\)", "", aa$method))
aa$method <- as.factor(sub("\\ \\+\\ mask", "\n\\+\\ mask", aa$method))
g <- ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    #geom_boxplot(outlier.alpha = 0.5, fill = viridis(20)) +
    geom_boxplot(outlier.alpha = 0.5, fill = c(cols[3],cols[2],cols[2],cols[4],cols[4],cols[1],cols[1],cols[7],cols[7],cols[3],cols[2],cols[2],cols[4],cols[4],cols[3],cols[6],cols[2],cols[2],cols[4],cols[4])) +
    scale_y_continuous(labels=function(x) paste0(symnum(x, c(-Inf, 0, Inf), c("", "+")), x)) +
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
    facet_wrap(~forest, nrow=3, scales = "free")
gt <- ggplot_gtable(ggplot_build(g))
i=13; gt$heights[i] = 5/9 * gt$heights[i]
gt$heights[18] = 6/9 * gt$heights[18]
grid.draw(gt)
dev.off()

