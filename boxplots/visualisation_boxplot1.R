load("boxplots/results/boxplot_MCAR.RData")
load("boxplots/results/boxplot_MNAR.RData")
load("boxplots/results/boxplot_PRED.RData")
# these are of lists of matrices of shape n_rep*length(sizes)

scores_mcar = scores_mcar
scores_mnar = scores_mnar
scores_pred = scores_pred

names(scores_mcar) <- c(
  "n rpart", "m rpart + mask", 
  "c mean", "b mean + mask", "e oor", "d oor + mask",
  "g Gaussian", "f Gaussian + mask", 
  "a MIA",
  "y ctree", "x ctree + mask",
  "c mean (forest)", "b mean + mask (forest)", "e oor (forest)", "d oor + mask (forest)",
  "g Gaussian (forest)", "f Gaussian + mask (forest)", 
  "a MIA (forest)",
  "c mean (xgboost)", "b mean + mask (xgboost)", "e oor (xgboost)", "d oor + mask (xgboost)",
  "g Gaussian (xgboost)", "f Gaussian + mask (xgboost)", 
  "a MIA (xgboost)"
)

labels = c(
    "n rpart"="rpart",
    "m rpart + mask"="rpart + mask", 
    "c mean"="mean",
    "b mean + mask"="mean + mask",
    "e oor"="oor",
    "d oor + mask"="oor + mask",
    "g Gaussian"="Gaussian",
    "f Gaussian + mask"="Gaussian + mask", 
    "a MIA"="MIA",
    "y ctree"="ctree",
    "x ctree + mask"="ctree + mask"
)
names(scores_mnar) <- names(scores_mcar)
names(scores_pred) <- names(scores_mcar)
     
dir.create(file.path('figures'), showWarnings=FALSE)

library(ggplot2)
library(gridExtra)
library(viridis)
library(grid)

# mse to explained variance
VARY <- 10
scores_mcar <- lapply(scores_mcar, function(x) 1-x/VARY)
scores_mnar <- lapply(scores_mnar, function(x) 1-x/VARY)
scores_pred <- lapply(scores_pred, function(x) 1-x/VARY)
cols <- c('#377eb8',
          '#cc6633',
          '#00ff33',
          '#ffccff',
          '#a65628',
          '#660033',
          '#cccccc',
          '#e41a1c',
          '#dede00',
	  "#009999")

collist <- c(
    cols[3],cols[2],cols[2], cols[8],cols[8] , cols[4],cols[4],cols[1],cols[1],cols[7],cols[7],
    cols[3],cols[2],cols[2], cols[8],cols[8] ,cols[4],cols[4],
    cols[3],cols[2],cols[2],cols[8], cols[8], cols[4],cols[4])

# relative
scores_mcar[1:11] <- lapply(scores_mcar[1:11], function(x) x-Reduce("+", scores_mcar[1:11]) / 11)
scores_mcar[12:18] <- lapply(scores_mcar[12:18], function(x) x-Reduce("+", scores_mcar[12:18]) / 7)
scores_mcar[19:25] <- lapply(scores_mcar[19:25], function(x) x-Reduce("+", scores_mcar[19:25]) / 7)

scores_mnar[1:11] <- lapply(scores_mnar[1:11], function(x) x-Reduce("+", scores_mnar[1:11]) / 11)
scores_mnar[12:18] <- lapply(scores_mnar[12:18], function(x) x-Reduce("+", scores_mnar[12:18]) / 7)
scores_mnar[19:25] <- lapply(scores_mnar[19:25], function(x) x-Reduce("+", scores_mnar[19:25]) / 7)
scores_pred[1:11] <- lapply(scores_pred[1:11], function(x) x-Reduce("+", scores_pred[1:11]) / 11)
scores_pred[12:18] <- lapply(scores_pred[12:18], function(x) x-Reduce("+", scores_pred[12:18]) / 7)
scores_pred[19:25] <- lapply(scores_pred[19:25], function(x) x-Reduce("+", scores_pred[19:25]) / 7)

df_for_ggplot <- function(scores) {
    aa <- cbind.data.frame(unlist(scores), rep(names(scores), each=length(scores[[1]])))
    colnames(aa) <- c("score", "method")
    aa$forest <- as.factor(
        ifelse(grepl("xgboost", aa$method), "XGBOOST", 
               ifelse(grepl("forest", aa$method), "RANDOM FOREST", "DECISION TREE")))
    aa$method <- as.factor(sub("\\ \\(forest\\)", "", aa$method))
    aa$method <- as.factor(sub("\\ \\(xgboost\\)", "", aa$method))
    aa$method <- as.factor(sub("\\ \\+\\ mask", "\\ +\\ mask", aa$method))
    return(aa)
}

# Now do the plotting

aa <- df_for_ggplot(scores_mcar)
ggplot(data=aa, aes(x=method, y=score)) +
  geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
  geom_boxplot(outlier.alpha=0.5, fill=collist) +
  theme_minimal() +
  theme(axis.text.y = element_text(face="bold", size=15, color='black'),
	axis.title = element_text(size=15, color='black'),
	strip.text.x = element_text(size=13, color='black'),
        plot.margin = unit(c(0, 0, 0, 0), "cm") #top, right, bottom, left
	) +
  scale_x_discrete(labels=labels, drop=TRUE) +
  labs(title="", x="", y="Relative explained variance") +
  coord_flip() +
  facet_wrap(~forest, nrow=3, scales = "free")
ggsave(filename="figures/boxplot_mcar.pdf", width=5.4, height=7.5)

aa <- df_for_ggplot(scores_mnar)
ggplot(data=aa, aes(method, score)) +
  geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
  geom_boxplot(outlier.alpha = 0.5, fill=collist) +
  theme_minimal() +
  theme(axis.text.y = element_text(face="bold", size=15, color='black'),
	axis.title = element_text(size=15, color='black'),
	strip.text.x = element_text(size=13, color='black'),
        plot.margin = unit(c(0, 0, 0, 0), "cm") #top, right, bottom, left
	) +
  scale_x_discrete(labels=labels, drop=TRUE) +
  labs(title="", x="", y="Relative explained variance") +
  coord_flip() +
  facet_wrap(~forest, nrow=3, scales = "free")  
ggsave(filename="figures/boxplot_mnar.pdf", width=5.4, height=7.5)

aa <- df_for_ggplot(scores_pred)
ggplot(data=aa, aes(method, score)) +
  geom_hline(yintercept=0, color='grey', size=3, linetype=1) +
  geom_boxplot(outlier.alpha=0.5, fill=collist) +
  theme_minimal() +
  theme(axis.text.y = element_text(face="bold", size=15, color='black'),
	axis.title = element_text(size=15, color='black'),
	strip.text.x = element_text(size=13, color='black'),
        plot.margin = unit(c(0, 0, 0, 0), "cm") #top, right, bottom, left
	) +
  scale_x_discrete(labels=labels, drop=TRUE) +
  labs(title="", x="", y="Relative explained variance") +
  coord_flip() +
  facet_wrap(~forest, nrow=3, scales="free")
ggsave(filename="figures/boxplot_pred.pdf", width=5.4, height=7.5)

