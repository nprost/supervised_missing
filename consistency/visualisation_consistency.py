"""With the output of script_consistency.R, get figure 5."""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
matplotlib.rcParams.update({'font.size': 12})

if not os.path.exists("figures"): os.mkdir("figures")

###############################################################################
# GET CORRECT VECTOR FOR PLOTTING


def method_name_func(model, strategy, withpattern):
    method_name = ""
    if strategy == "mia": method_name = "MIA"
    elif strategy == "mean": method_name = "Mean imputation"
    elif strategy == "gaussian": method_name = "Gaussian imputation"
    elif strategy == "none":
        if model == "rpart": method_name = "Surrogates (rpart)"
        elif model == "ctree": method_name = "ctree (surrogates)"
        elif model == "xgboost": method_name = "Block (XGBoost)"
    if withpattern == "TRUE": method_name += " + mask"
    if model == "ranger": method_name += " - forest"
    if model == "xgboost": method_name += " - xgboost"
    return method_name


# remove the three first values: too small sample sizes
sizes = np.logspace(2, 5, 20).astype('int64')[3:]

scores_raw = []
# trees
for i in range(3):
    scorerawi = {}
    dataset="make_data{}".format(i+1)
    for method in [
        {'model': 'rpart', 'strategy': 'none', 'withpattern': "FALSE"},
        {'model': 'rpart', 'strategy': 'mean', 'withpattern': "FALSE"},
        {'model': 'rpart', 'strategy': 'gaussian', 'withpattern': "FALSE"},
        {'model': 'rpart', 'strategy': 'mia', 'withpattern': "FALSE"},
        ]:
        model, strategy, withpattern = tuple(method.values())

        csvfile = "consistency/results/{}_{}_{}_{}.csv".format(dataset, model, strategy, withpattern)
        if os.path.exists(csvfile): 
            scores_method = pd.read_csv(csvfile, sep=" ")
            method_name = method_name_func(model, strategy, withpattern)
            scorerawi[method_name] = np.array(scores_method)[:,3:]
    scores_raw.append(scorerawi)
    
# forests
for i in range(3):
    scorerawi = {}
    dataset="make_data{}".format(i+1)
    for method in [
        {'model': 'ranger', 'strategy': 'mean', 'withpattern': "FALSE"},
        {'model': 'ranger', 'strategy': 'gaussian', 'withpattern': "FALSE"},
        {'model': 'ranger', 'strategy': 'mia', 'withpattern': "FALSE"},
        ]:
        model, strategy, withpattern = tuple(method.values())

        csvfile = "consistency/results/{}_{}_{}_{}.csv".format(dataset, model, strategy, withpattern)
        if os.path.exists(csvfile): 
            scores_method = pd.read_csv(csvfile, sep=" ")
            method_name = method_name_func(model, strategy, withpattern)
            scorerawi[method_name] = np.array(scores_method)[:,3:]
    scores_raw.append(scorerawi)

# xgboost
for i in range(3):
    scorerawi = {}
    dataset="make_data{}".format(i+1)
    for method in [
        {'model': 'xgboost', 'strategy': 'mean', 'withpattern': "FALSE"},
        {'model': 'xgboost', 'strategy': 'gaussian', 'withpattern': "FALSE"},
        {'model': 'xgboost', 'strategy': 'mia', 'withpattern': "FALSE"},
        {'model': 'xgboost', 'strategy': 'none', 'withpattern': "FALSE"},
        ]:
        model, strategy, withpattern = tuple(method.values())

        csvfile = "consistency/results/{}_{}_{}_{}.csv".format(dataset, model, strategy, withpattern)
        if os.path.exists(csvfile): 
            scores_method = pd.read_csv(csvfile, sep=" ")
            method_name = method_name_func(model, strategy, withpattern)
            scorerawi[method_name] = np.array(scores_method)[:,3:]
    scores_raw.append(scorerawi)


# Transformation of scores, mse to explained variance
VARY = [25, 1702, 10823.94, 25, 1702, 10823.94, 25, 1702, 10823.94]
scores_expvar = [{key: 1 - scr/VARY[datanum] 
    for key, scr in score.items()} for datanum, score in enumerate(scores_raw)]

# sufficient statistics
scores = [{key: (np.percentile(val, 50, 0), np.percentile(val, 25, 0), np.percentile(val, 75, 0)) 
    for key, val in score.items()} for score in scores_expvar]

# these values come from bayesrate.R
L = len(sizes)
scores[0]['Bayes rate'] = (np.repeat(0.8087995, L), np.repeat(0, L), np.repeat(0, L))
scores[1]['Bayes rate'] = (np.repeat(0.7484916, L), np.repeat(0, L), np.repeat(0, L))
scores[3]['Bayes rate'] = (np.repeat(0.8087995, L), np.repeat(0, L), np.repeat(0, L))
scores[4]['Bayes rate'] = (np.repeat(0.7484916, L), np.repeat(0, L), np.repeat(0, L))
scores[6]['Bayes rate'] = (np.repeat(0.8087995, L), np.repeat(0, L), np.repeat(0, L))
scores[7]['Bayes rate'] = (np.repeat(0.7484916, L), np.repeat(0, L), np.repeat(0, L))


###############################################################################
###############################################################################
###############################################################################
# PLOT

plt.clf()
fig, ax = plt.subplots(3, 3, figsize=(10, 10))
colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
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
for name, scr in scores[0].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    ax[0,0].semilogx(sizes, means, label=name, linestyle=linestyle, c=colors_dict[name])
    ax[0,0].fill_between(sizes, q1, q2, alpha=0.2)
ax[0,0].set_xlabel("Sample size")
ax[0,0].set_ylabel("Explained variance")
ax[0,0].set_ylim(
    np.array([s[0] for _, s in scores[0].items()]).min()-0.05,
    np.array([s[0] for _, s in scores[0].items()]).max()+0.05
)
ax[0,0].set_xlim(297.63514416, 10**5)
ax[0,0].text(10**3, 0.95, "Linear problem\n(high noise)")
ax[0,0].grid()
###############################################################################
for name, scr in scores[1].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    ax[0,1].semilogx(sizes, means, label=None, linestyle=linestyle, c=colors_dict[name])
    ax[0,1].fill_between(sizes, q1, q2, alpha=0.2)
ax[0,1].set_xlabel("Sample size")
ax[0,1].set_ylabel("Explained variance")
ax[0,1].set_ylim(
    np.array([s[0] for _, s in scores[1].items()]).min()-0.05,
    np.array([s[0] for _, s in scores[1].items()]).max()+0.05
)
ax[0,1].set_xlim(297.63514416, 10**5)
ax[0,1].text(10**3, 0.88, "Friedman problem\n(high noise)")
ax[0,1].grid()
###############################################################################
for name, scr in scores[2].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    ax[0,2].semilogx(sizes, means, label=None, linestyle=linestyle, c=colors_dict[name])
    ax[0,2].fill_between(sizes, q1, q2, alpha=0.2)
ax[0,2].set_xlabel("Sample size")
ax[0,2].set_ylabel("Explained variance")
ax[0,2].set_ylim(
    np.array([s[0] for _, s in scores[2].items()]).min()-0.01,
    np.array([s[0] for _, s in scores[2].items()]).max()+0.05
)
ax[0,2].set_xlim(297.63514416, 10**5)
ax[0,2].text(10**3, 1.12, "Non-linear problem\n(low noise)")
ax[0,2].text(2*10**5, 0.95, "DECISION TREE", rotation=-90, fontweight='bold')
ax[0,2].grid()
###############################################################################
for name, scr in scores[3].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    ax[1,0].semilogx(sizes, means, label=None, linestyle=linestyle, c=colors_dict[name])
    ax[1,0].fill_between(sizes, q1, q2, alpha=0.2)
ax[1,0].set_xlabel("Sample size")
ax[1,0].set_ylabel("Explained variance")
ax[1,0].set_ylim(
    np.array([s[0] for _, s in scores[3].items()]).min()-0.01,
    np.array([s[0] for _, s in scores[3].items()]).max()+0.01
)
ax[1,0].set_xlim(297.63514416, 10**5)
ax[1,0].grid()
###############################################################################
for name, scr in scores[4].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    ax[1,1].semilogx(sizes, means, label=None, linestyle=linestyle, c=colors_dict[name])
    ax[1,1].fill_between(sizes, q1, q2, alpha=0.2)
ax[1,1].set_xlabel("Sample size")
ax[1,1].set_ylabel("Explained variance")
ax[1,1].set_ylim(
    np.array([s[0] for _, s in scores[4].items()]).min()-0.015,
    np.array([s[0] for _, s in scores[4].items()]).max()+0.015
)
ax[1,1].set_xlim(297.63514416, 10**5)
ax[1,1].grid()
###############################################################################
for name, scr in scores[5].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    ax[1,2].semilogx(sizes, means, label=None, linestyle=linestyle, c=colors_dict[name])
    ax[1,2].fill_between(sizes, q1, q2, alpha=0.2)
ax[1,2].set_xlabel("Sample size")
ax[1,2].set_ylabel("Explained variance")
ax[1,2].set_ylim(
    np.array([s[0] for _, s in scores[5].items()]).min()-0.005,
    np.array([s[0] for _, s in scores[5].items()]).max()+0.005
)
ax[1,2].set_xlim(297.63514416, 10**5)
ax[1,2].text(2*10**5, 0.993, "RANDOM FOREST", rotation=-90, fontweight='bold')
ax[1,2].grid()
###############################################################################
for name, scr in scores[6].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    if name == 'Block (XGBoost) - xgboost':
        ax[2,0].semilogx(sizes, means, label='Block (XGBoost)', linestyle=linestyle, c=colors_dict[name])
    else:
        ax[2,0].semilogx(sizes, means, label=None, linestyle=linestyle, c=colors_dict[name])

    ax[2,0].fill_between(sizes, q1, q2, alpha=0.2)
ax[2,0].set_xlabel("Sample size")
ax[2,0].set_ylabel("Explained variance")
ax[2,0].set_ylim(
    np.array([s[0] for _, s in scores[6].items()]).min()-0.01,
    np.array([s[0] for _, s in scores[6].items()]).max()+0.01
)
ax[2,0].set_xlim(297.63514416, 10**5)
ax[2,0].grid()
###############################################################################
for name, scr in scores[7].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    ax[2,1].semilogx(sizes, means, label=None, linestyle=linestyle, c=colors_dict[name])
    ax[2,1].fill_between(sizes, q1, q2, alpha=0.2)
ax[2,1].set_xlabel("Sample size")
ax[2,1].set_ylabel("Explained variance")
ax[2,1].set_ylim(
    np.array([s[0] for _, s in scores[7].items()]).min()-0.015,
    np.array([s[0] for _, s in scores[7].items()]).max()+0.015
)
ax[2,1].set_xlim(297.63514416, 10**5)
ax[2,1].grid()
###############################################################################
for name, scr in scores[8].items():
    means = np.array(scr[0])
    q1 = np.array(scr[1])
    q2 = np.array(scr[2])
    if '+ mask' in name:
        linestyle = ':'
    else:
        linestyle = '-'
    ax[2,2].semilogx(sizes, means, label=None, linestyle=linestyle, c=colors_dict[name])
    ax[2,2].fill_between(sizes, q1, q2, alpha=0.2)
ax[2,2].set_xlabel("Sample size")
ax[2,2].set_ylabel("Explained variance")
ax[2,2].set_ylim(
    np.array([s[0] for _, s in scores[8].items()]).min()-0.005,
    np.array([s[0] for _, s in scores[8].items()]).max()+0.005
)
ax[2,2].set_xlim(297.63514416, 10**5)
ax[2,2].text(2*10**5, 0.993, "XGBOOST", rotation=-90, fontweight='bold')
ax[2,2].grid()
###############################################################################
fig.legend(loc=(0.17, 0.01), ncol=3, frameon=False)
plt.tight_layout()
fig.subplots_adjust(bottom=0.13, top=0.9, right=0.95)
fig.savefig('figures/consistency_log_merge.pdf')
plt.close(fig)  
