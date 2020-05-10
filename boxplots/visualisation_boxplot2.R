load("boxplots/results/boxplot2_1.RData")
load("boxplots/results/boxplot2_2.RData")
load("boxplots/results/boxplot2_3.RData")
# these are of lists of matrices of shape n_rep*length(sizes)

scores_21 <- scores_21
scores_22 <- scores_22
scores_23 <- scores_23

names(scores_21) <- c(
  "i. rpart", "h. rpart + mask", 
  "c.  mean", "b.  mean + mask", "e.  oor", "d.  oor + mask",
  "g.  Gaussian", "f.  Gaussian + mask", 
  "a. MIA",
  "k. ctree", "j. ctree + mask",
  "c.  mean (forest)", "b.  mean + mask (forest)", "e.  oor (forest)", "d.  oor + mask (forest)",
  "g.  Gaussian (forest)", "f.  Gaussian + mask (forest)", 
  "a. MIA (forest)",
  "c.  mean (xgboost)", "b.  mean + mask (xgboost)", "e.  oor (xgboost)", "d.  oor + mask (xgboost)",
  "g.  Gaussian (xgboost)", "f.  Gaussian + mask (xgboost)", 
  "a. MIA (xgboost)"
)
names(scores_22) <- names(scores_21)
names(scores_23) <- names(scores_21)

#dir.create(file.path('../figures'), showWarnings=FALSE)

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

cols <- c('#377eb8',
          '#cc6633',
          '#00ff33',
          '#ffccff',
          '#a65628',
          '#660033',
          '#cccccc',
          '#e41a1c',
          '#dede00', "#009999")

collist <- c(
  cols[3],cols[2],cols[2], cols[8],cols[8] , cols[4],cols[4],cols[1],cols[1],cols[7],cols[7],
  cols[3],cols[2],cols[2], cols[8],cols[8] ,cols[4],cols[4],
  cols[3],cols[2],cols[2],cols[8], cols[8], cols[4],cols[4])


# relative
scores_21[1:11] <- lapply(scores_21[1:11], function(x) x-Reduce("+", scores_21[1:11]) / 11)
scores_21[12:18] <- lapply(scores_21[12:18], function(x) x-Reduce("+", scores_21[12:18]) / 7)
scores_21[19:25] <- lapply(scores_21[19:25], function(x) x-Reduce("+", scores_21[19:25]) / 7)

scores_22[1:11] <- lapply(scores_22[1:11], function(x) x-Reduce("+", scores_22[1:11]) / 11)
scores_22[12:18] <- lapply(scores_22[12:18], function(x) x-Reduce("+", scores_22[12:18]) / 7)
scores_22[19:25] <- lapply(scores_22[19:25], function(x) x-Reduce("+", scores_22[19:25]) / 7)
scores_23[1:11] <- lapply(scores_23[1:11], function(x) x-Reduce("+", scores_23[1:11]) / 11)
scores_23[12:18] <- lapply(scores_23[12:18], function(x) x-Reduce("+", scores_23[12:18]) / 7)
scores_23[19:25] <- lapply(scores_23[19:25], function(x) x-Reduce("+", scores_23[19:25]) / 7)


df_for_ggplot <- function(scores) {
  aa <- cbind.data.frame(unlist(scores), rep(names(scores), each = length(scores[[1]])))
  colnames(aa) <- c("score", "method")
  aa$forest <- as.factor(
    ifelse(grepl("xgboost", aa$method), "XGBOOST", 
           ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE")))
  aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
  aa$method <- as.factor(sub("\\ \\(xgboost\\)", "", aa$method))
  aa$method <- as.factor(sub("\\ \\+\\ mask", "\\ +\\ mask", aa$method))
  return(aa)
}

pdf('figures/boxplot_linearlinear.pdf')
aa <- df_for_ggplot(scores_21)
ggplot(data=aa, aes(method, score)) +
  geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
  geom_boxplot(outlier.alpha = 0.5, fill=collist) +
  theme(axis.text.y = element_text(face="bold", 
                                   size=15), axis.title = element_text(face="bold", size=16))+
  labs(title = "", x  = "", y = "Relative explained variance") +
  coord_flip() +
  facet_wrap(~forest, nrow=3, scales = "free")  
dev.off()

pdf('figures/boxplot_linearnonlinear.pdf')
aa <- df_for_ggplot(scores_22)
ggplot(data=aa, aes(method, score)) +
  geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
  geom_boxplot(outlier.alpha = 0.5, fill=collist) +
  theme(axis.text.y = element_text(face="bold", 
                                   size=15), axis.title = element_text(face="bold", size=16))+
  labs(title = "", x  = "", y = "Relative explained variance") +
  coord_flip() +
  facet_wrap(~forest, nrow=3, scales = "free")  
dev.off()

pdf('figure/boxplot_nonlinearnonlinear.pdf')
aa <- df_for_ggplot(scores_23)
ggplot(data=aa, aes(method, score)) +
  geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
  geom_boxplot(outlier.alpha = 0.5, fill=collist) +
  theme(axis.text.y = element_text(face="bold", 
                                   size=15), axis.title = element_text(face="bold", size=16))+
  labs(title = "", x  = "", y = "Relative explained variance") +
  coord_flip() +
  facet_wrap(~forest, nrow=3, scales = "free")  
dev.off()

