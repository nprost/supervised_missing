load("results/boxplot2_1.RData")
load("results/boxplot2_2.RData")
load("results/boxplot2_3.RData")
# these are of lists of matrices of shape n_rep*length(sizes)

names(scores_21) <- c(
    "7. rpart (surrogates)", "6. rpart (surrogates) + mask", 
    "3. impute mean", "2. impute mean + mask",
    "5. impute Gaussian", "4. impute Gaussian + mask", 
    "0. MIA",
    "9. ctree (surrogates)", "8. ctree (surrogates) + mask",
    "3. impute mean (forest)", "2. impute mean + mask (forest)",
    "5. impute Gaussian (forest)", "4. impute Gaussian + mask (forest)", 
    "0. MIA (forest)",
    "1. block (xgboost)",
    "3. impute mean (xgboost)", "2. impute mean + mask (xgboost)", 
    "5. impute Gaussian (xgboost)", "4. impute Gaussian + mask (xgboost)"
    "0. MIA (xgboost)"
    )
names(scores_22) <- names(scores_21)
names(scores_23) <- names(scores_21)

dir.create(file.path('../figures'), showWarnings=FALSE)

library(ggplot2)
library(gridExtra)
library(viridis)
library(grid)

# mse to explained variance
VARY1 <- 25.4
scores_21 <- lapply(scores_21, function(x) 1-x/VARY1)
VARY2 <- 1710
scores_22 <- lapply(scores_22, function(x) 1-x/VARY2)
VARY3 <- 10820
scores_23 <- lapply(scores_23, function(x) 1-x/VARY3)

cols <- c('#1f77b4','#ff7f0e','#2ca02c','#d62728','#9467bd',
          '#8c564b','#e377c2','#7f7f7f','#bcbd22','#17becf')
collist <- c(
    cols[3],cols[2],cols[2],cols[4],cols[4],cols[1],cols[1],cols[7],cols[7],
    cols[3],cols[2],cols[2],cols[4],cols[4],
    cols[3],cols[6],cols[2],cols[2],cols[4],cols[4])

# relative
scores_21[1:9] <- lapply(scores_21[1:9], function(x) x-Reduce("+", scores_21[1:9]) / 9)
scores_21[10:14] <- lapply(scores_21[10:14], function(x) x-Reduce("+", scores_21[10:14]) / 5)
scores_21[15:20] <- lapply(scores_21[15:20], function(x) x-Reduce("+", scores_21[15:20]) / 6)
scores_22[1:9] <- lapply(scores_22[1:9], function(x) x-Reduce("+", scores_22[1:9]) / 9)
scores_22[10:14] <- lapply(scores_22[10:14], function(x) x-Reduce("+", scores_22[10:14]) / 5)
scores_22[15:20] <- lapply(scores_22[15:20], function(x) x-Reduce("+", scores_22[15:20]) / 6)
scores_23[1:9] <- lapply(scores_23[1:9], function(x) x-Reduce("+", scores_23[1:9]) / 9)
scores_23[10:14] <- lapply(scores_23[10:14], function(x) x-Reduce("+", scores_23[10:14]) / 5)
scores_23[15:20] <- lapply(scores_23[15:20], function(x) x-Reduce("+", scores_23[15:20]) / 6)

df_for_ggplot <- function(scores) {
    aa <- cbind.data.frame(unlist(scores), rep(names(scores), each = length(scores[[1]])))
    colnames(aa) <- c("score", "method")
    aa$forest <- as.factor(ifelse(grepl("xgboost", aa$method), "XGBOOST", ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE")))
    aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
    aa$method <- as.factor(sub("\\ \\(xgboost\\)", "", aa$method))
    aa$method <- as.factor(sub("\\ \\+\\ mask", "\n\\+\\ mask", aa$method))
    return(aa)
}

width <- 9.5
height <- 13
pdf('../figures/boxplot_linearlinear.pdf', width=width, height=height)
aa <- df_for_ggplot(scores_21)
g <- ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    geom_boxplot(outlier.alpha = 0.5, fill=collist) +
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
    facet_wrap(~forest, nrow=3, scales="free")
gt <- ggplot_gtable(ggplot_build(g))
gt$heights[13] = 5/9 * gt$heights[13]
gt$heights[18] = 6/9 * gt$heights[18]
grid.draw(gt)
dev.off()

pdf('../figures/boxplot_linearnonlinear.pdf', height=height)
aa <- df_for_ggplot(scores_22)
g <- ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    geom_boxplot(outlier.alpha = 0.5, fill=collist) +
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
gt$heights[13] = 5/9 * gt$heights[13]
gt$heights[18] = 6/9 * gt$heights[18]
grid.draw(gt)
dev.off()

pdf('../figures/boxplot_nonlinearnonlinear.pdf', height=height)
aa <- df_for_ggplot(scores_23)
g <- ggplot(data=aa, aes(method, score)) +
    geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
    geom_boxplot(outlier.alpha = 0.5, fill=collist) +
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
gt$heights[13] = 5/9 * gt$heights[13]
gt$heights[18] = 6/9 * gt$heights[18]
grid.draw(gt)
dev.off()


