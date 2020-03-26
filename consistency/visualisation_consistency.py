"""With the output of script_consistency.R, get figure 5."""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
matplotlib.rcParams.update({'font.size': 12})

if not os.path.exists("figures"):
    os.mkdir("figures")

###############################################################################
# GET CORRECT VECTOR FOR PLOTTING


def method_name_func(model, strategy, withpattern):
    method_name = ""
    if strategy == "mia":
        method_name = "MIA"
    elif strategy == "mean":
        method_name = "Mean imputation"
    elif strategy == "gaussian":
        method_name = "Gaussian imputation"
    elif strategy == "none":
        if model == "rpart":
            method_name = "Surrogates (rpart)"
        elif model == "ctree":
            method_name = "ctree (surrogates)"
        elif model == "xgboost":
            method_name = "Block (XGBoost)"
    if withpattern == "TRUE":
        method_name += " + mask"
    if model == "ranger":
        method_name += " - forest"
    if model == "xgboost":
        method_name += " - xgboost"
    return method_name


# remove the three first values: too small sample sizes
sizes = np.logspace(2, 5, 20).astype('int64')[3:]

methods_dict = {
    'rpart': [
        {'model': 'rpart', 'strategy': 'none', 'withpattern': "FALSE"},
        {'model': 'rpart', 'strategy': 'mean', 'withpattern': "FALSE"},
        {'model': 'rpart', 'strategy': 'gaussian', 'withpattern': "FALSE"},
        {'model': 'rpart', 'strategy': 'mia', 'withpattern': "FALSE"}],
    'ranger': [
        {'model': 'ranger', 'strategy': 'mean', 'withpattern': "FALSE"},
        {'model': 'ranger', 'strategy': 'gaussian', 'withpattern': "FALSE"},
        {'model': 'ranger', 'strategy': 'mia', 'withpattern': "FALSE"}],
    'xgboost': [
        {'model': 'xgboost', 'strategy': 'mean', 'withpattern': "FALSE"},
        {'model': 'xgboost', 'strategy': 'gaussian', 'withpattern': "FALSE"},
        {'model': 'xgboost', 'strategy': 'none', 'withpattern': "FALSE"},
        {'model': 'xgboost', 'strategy': 'mia', 'withpattern': "FALSE"}]
    }

scores_raw = []
for name, methods in methods_dict.items():
    for i in range(3):
        scorerawi = {}
        dataset = "make_data{}".format(i+1)
        for method in methods:
            model, strategy, withpattern = tuple(method.values())

            csvfile = "consistency/results/{}_{}_{}_{}.csv".format(
                dataset, model, strategy, withpattern)
            if os.path.exists(csvfile):
                scores_method = pd.read_csv(csvfile, sep=" ")
                method_name = method_name_func(model, strategy, withpattern)
                scorerawi[method_name] = np.array(scores_method)[:, 3:]
        scores_raw.append(scorerawi)


# Transformation of scores, mse to explained variance
VARY = [25, 1702, 10823.94, 25, 1702, 10823.94, 25, 1702, 10823.94]
scores_expvar = [
    {key: 1 - scr/VARY[datanum]for key, scr in score.items()}
    for datanum, score in enumerate(scores_raw)]

# sufficient statistics
scores = [
    {key: (np.percentile(val, 50, 0),
           np.percentile(val, 25, 0),
           np.percentile(val, 75, 0))
     for key, val in score.items()}
    for score in scores_expvar]

# these values come from bayesrate.R
L = len(sizes)
scores[0]['Bayes rate'] = (np.repeat(0.8087995, L), np.repeat(0, L),
                           np.repeat(0, L))
scores[1]['Bayes rate'] = (np.repeat(0.7484916, L), np.repeat(0, L),
                           np.repeat(0, L))
scores[3]['Bayes rate'] = (np.repeat(0.8087995, L), np.repeat(0, L),
                           np.repeat(0, L))
scores[4]['Bayes rate'] = (np.repeat(0.7484916, L), np.repeat(0, L),
                           np.repeat(0, L))
scores[6]['Bayes rate'] = (np.repeat(0.8087995, L), np.repeat(0, L),
                           np.repeat(0, L))
scores[7]['Bayes rate'] = (np.repeat(0.7484916, L), np.repeat(0, L),
                           np.repeat(0, L))


###############################################################################
###############################################################################
###############################################################################
# PLOT

# CB_color_cycle = ['#377eb8', '#ff7f00', '#4daf4a',
#                   '#f781bf', '#a65628', '#984ea3',
#                   '#999999', '#e41a1c', '#dede00']
CB_color_cycle = [
    '#377eb8',
    '#cc6633',
    '#00cc00',  # '#4daf4a',  # '#00cc33',
    '#ff99ff',
    '#a65628',
    '#660033',
    '#cccccc',
    '#e41a1c',
    '#dede00']


plt.clf()
fig, ax = plt.subplots(3, 3, figsize=(10, 10))
# colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
colors = CB_color_cycle
colors_dict = {
    'Surrogates (rpart)': colors[0],
    'Mean imputation': colors[1],
    'Block (XGBoost) - xgboost': colors[5],
    'MIA': colors[2],
    'MIA - forest': colors[2],
    'MIA - xgboost': colors[2],
    'Gaussian imputation': colors[3],
    'Mean imputation - forest': colors[1],
    'Mean imputation - xgboost': colors[1],
    'Gaussian imputation - forest': colors[3],
    'Gaussian imputation - xgboost': colors[3],
    'Bayes rate': colors[4],
}

###############################################################################
# There are 3 x 3 plots: trees, forests, boosting for models 1, 2, 3
###############################################################################
yoffset = [(0.05, 0.05), (0.05, 0.05), (0.01, 0.05),
           (0.01, 0.01), (0.015, 0.015), (0.005, 0.005),
           (0.01, 0.01), (0.015, 0.015), (0.005, 0.005)]

for enum, score in enumerate(scores):
    ax_x = enum // 3
    ax_y = enum % 3

    for name, scr in score.items():
        means = np.array(scr[0])
        q1 = np.array(scr[1])
        q2 = np.array(scr[2])
        ax[ax_x, ax_y].fill_between(sizes, q1, q2, alpha=0.2,
                                    color=colors_dict[name])

    for name, scr in score.items():
        means = np.array(scr[0])
        # if '+ mask' in name:
        #     linestyle = ':'
        # else:
        #     linestyle = '-'
        linestyle = '-'

        if enum == 0:
            labelname = name
        elif name == 'Block (XGBoost) - xgboost' and enum == 6:
            labelname = 'Block (XGBoost)'
        else:
            labelname = None

        if name == 'Bayes rate':
            linewidth = 3
        else:
            linewidth = 1

        if 'Surrogates' in name:
            marker = '.'
            markersize = 5
            markevery = 1
            markerfacecolor = colors_dict[name]
        elif 'Gaussian imputation' in name:
            marker = '2'
            markersize = 5
            markevery = 1
            markerfacecolor = colors_dict[name]
        elif 'Mean imputation' in name:
            marker = 's'
            markersize = 4
            markevery = 3
            markerfacecolor = colors_dict[name]
        elif 'MIA' in name:
            marker = 'v'
            markersize = 5
            markevery = 2
            markerfacecolor = '#4daf4a'
        elif 'Block' in name:
            marker = ','
            markersize = 5
            markevery = 1
            markerfacecolor = colors_dict[name]
        else:
            marker = ','
            markersize = 5
            markevery = 1
            markerfacecolor = colors_dict[name]

        ax[ax_x, ax_y].semilogx(sizes, means, label=labelname,
                                linestyle=linestyle, linewidth=linewidth,
                                marker=marker, markersize=markersize,
                                markerfacecolor=markerfacecolor,
                                markevery=markevery,
                                c=colors_dict[name])

    ax[ax_x, ax_y].set_xlabel("Sample size")
    ax[ax_x, ax_y].set_ylabel("Explained variance")
    ax[ax_x, ax_y].set_ylim(
        np.array([s[0] for _, s in score.items()]).min() - yoffset[enum][0],
        np.array([s[0] for _, s in score.items()]).max() + yoffset[enum][1]
    )
    ax[ax_x, ax_y].set_xlim(297.63514416, 10**5)
    ax[ax_x, ax_y].grid()
###############################################################################

ax[0, 0].text(10**3, 0.95, "Linear problem\n(high noise)")
ax[0, 1].text(10**3, 0.88, "Friedman problem\n(high noise)")
ax[0, 2].text(10**3, 1.12, "Non-linear problem\n(low noise)")
ax[0, 2].text(2*10**5, 0.7, "DECISION TREE", rotation=-90, fontweight='bold')
ax[1, 2].text(2*10**5, 0.96, "RANDOM FOREST", rotation=-90, fontweight='bold')
ax[2, 2].text(2*10**5, 0.97, "XGBOOST", rotation=-90, fontweight='bold')

###############################################################################

fig.legend(loc=(0.17, 0.01), ncol=3, frameon=False)
plt.tight_layout()
fig.subplots_adjust(bottom=0.13, top=0.9, right=0.95)
fig.savefig('figures/consistency_log_merge.pdf')
plt.close(fig)
