load("boxplots/results/boxplot_MCAR.RData")
load("boxplots/results/boxplot_MNAR.RData")
load("boxplots/results/boxplot_PRED.RData")
load("boxplots/results/boxplot2_1.RData")
load("boxplots/results/boxplot2_2.RData")
load("boxplots/results/boxplot2_3.RData")
# these are of lists of matrices of shape n_rep*length(sizes)

names(scores_mcar) <- c(
  "rpart", "rpart + mask", 
  "mean", "mean + mask", "oor", "oor + mask",
  "Gaussian", "Gaussian + mask", 
  "MIA",
  "ctree", "ctree + mask",
  "mean (forest)", "mean + mask (forest)", "oor (forest)", "oor + mask (forest)",
  "Gaussian (forest)", "Gaussian + mask (forest)", 
  "MIA (forest)",
  "mean (xgboost)", "mean + mask (xgboost)", "oor (xgboost)", "oor + mask (xgboost)",
  "Gaussian (xgboost)", "Gaussian + mask (xgboost)", 
  "MIA (xgboost)"
)

names(scores_mnar) <- names(scores_mcar)
names(scores_pred) <- names(scores_mcar)
names(scores_21) <- names(scores_mcar)
names(scores_22) <- names(scores_mcar)
names(scores_23) <- names(scores_mcar)

     
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


aa <- df_for_ggplot(scores_mcar)
write.csv(aa,'boxplots/results/scores_mcar.csv')
aa <- df_for_ggplot(scores_mnar)
write.csv(aa,'boxplots/results/scores_mnar.csv')
aa <- df_for_ggplot(scores_pred)
write.csv(aa,'boxplots/results/scores_pred.csv')
aa <- df_for_ggplot(scores_21)
write.csv(aa,'boxplots/results/scores_linearlinear.csv')
aa <- df_for_ggplot(scores_22)
write.csv(aa,'boxplots/results/scores_linearnonlinear.csv')
aa <- df_for_ggplot(scores_23)
write.csv(aa,'boxplots/results/scores_nonlinearnonlinear.csv')
