# On the consistency of supervised learning with missing values

**Authors: Julie Josse (CMAP, Inria), Nicolas Prost (CMAP, Inria), Erwan Scornet (CMAP), Gaël Varoquaux (Inria).**

This repository contains the code for the paper:

Julie Josse, Nicolas Prost, Erwan Scornet, Gaël Varoquaux. On the consistency of supervised learning with missing values. 2019. 〈hal-02024202〉https://arxiv.org/abs/1902.06931

The directory **analysis** contains the code for figures 1 and 2 (section 5).

**boxplots** corresponds to figures 3 and 4 (section 6). There are three separate files: one containing the functions, one containing the script for computation, and two for the visualisation (one of each of the two boxplots).

**consistency** is used for figure 5 (section 6). There are three files as for the boxplot, but in addition, approximate Bayes rates are computed in *bayesrates.R* with oracle multiple imputation, as detailed in the paper. 

The scripts require the following R packages:
```r
rpart
party
ranger
xgboost
MASS
norm
doParallel
doSNOW
```

To run *script_boxplots.R* or *script_consistency.R* with, say, 20 jobs to parallelize the "for" loop and 10 threads per forest/boosting, do

```bash
Rscript boxplots/script_boxplots.R 20 10
Rscript consistency/script_consistency.R 20 10
```

To build the figures, just run the scripts,

```bash
Rscript boxplots/visualisation_boxplot1.R
Rscript boxplots/visualisation_boxplot2.R
python consistency/visualisation_consistency.R
```

All figure outputs go to the directory **figures** (created when nonexistent).

Nicolas Prost

March 21, 2019

**Update**: the directory **Notebook** contains a tutorial on key results of the paper.

# Notebook instructions

[![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/nprost/supervised_missing/master?filepath=Notebook%2FA%20toy%20regression%20model%20with%20missing%20values.ipynb)

A Binder-compatible repo with a `requirements.txt` file.

Access this Binder at the following URL 

https://mybinder.org/v2/gh/nprost/supervised_missing/master?filepath=Notebook%2FA%20toy%20regression%20model%20with%20missing%20values.ipynb

## Notes
Install the required Python libraries via
```
pip install -r requirements.txt
```
