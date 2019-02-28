# On the consistency of supervised learning with missing values

**Authors: Julie Josse (CMAP), Nicolas Prost (CMAP, Inria), Erwan Scornet (CMAP), Gaël Varoquaux (Inria).**

This repository contains the code for the paper:

Julie Josse, Nicolas Prost, Erwan Scornet, Gaël Varoquaux. On the consistency of supervised learning with missing values. 2019. 〈hal-02024202〉

The folder **analysis** contains the code for figures 1 and 2 (section 5).

**boxplots** corresponds to figures 3 and 4 (section 6). There are three separate files: one containing the functions, one containing the script for computation (parallelized "for" loop, but with one core per forest), and two for the visualisation, one of each of the two boxplots.

**consistency** is used for figure 5 (section 6). There are three files as for the boxplot, but in addition, approximate Bayes rates are computed in *bayesrates.R* with oracle multiple imputation, as detailed in the paper. 

All figure outputs go to the directory **figure**.

February 28, 2019
