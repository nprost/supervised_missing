# On the consistency of supervised learning with missing values

Authors: Julie Josse (CMAP), Nicolas Prost (CMAP, Inria), Erwan Scornet (CMAP), Gaël Varoquaux (Inria).

This is the code corresponding to the preprint arXiv:1902.06931, especially section 6: simulations.

There are two boxplots and one consistency figure. For each one, there are three separate files: one containing the functions, one containing the script for computation (parallelized for loop, but with one core per forest), and one for the visualisation.

For the consistency, approximate Bayes rates are computed in *bayesrates.R* with oracle multiple imputation, as detailed in the paper.  
