import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

def format_float(x):
    fmt = '%.2f'
    for i in range(-1, 1):
        if abs(x) > 10**i:
            fmt = '%%.%if' % (1 - i)
    for i in range(2, 4):
        if abs(x) > 10**i:
            x = np.round(x, i)
            fmt = '%i'
    return fmt % x


sns.set_style("whitegrid",
        {'axes.spines.left': False,
         'axes.spines.bottom': False,
         'axes.spines.right': False,
         'axes.spines.top': False,
         'text.color': 'k',
         'xtick.color': 'k',
         'ytick.color': 'k',
         })

plt.rcParams['ytick.labelsize'] = 17
plt.rcParams['xtick.major.pad'] = .02

color_mapping = {
  "ctree":              '#cccccc',
  "ctree + mask":       '#cccccc',
  "rpart":              '#377eb8',
  "rpart + mask":       '#377eb8',
  "Gaussian":           '#cc6633',
  "Gaussian + mask":    '#cc6633',
  "oor":                'r',
  "oor + mask":         'r',
  "mean":               '#ffccff',
  "mean + mask":        '#ffccff',
  "MIA":                '#00ff33',
}

FORESTS = ['DECISION TREE', 'RANDOM FOREST', 'XGBOOST']

y_variances = {
    'mcar': 10,
    'mnar': 10,
    'pred': 10,
    'linearlinear': 25.4,
    'linearnonlinear': 1710,
    'nonlinearnonlinear': 10820,
}

for name in ('mcar', 'mnar', 'pred', 'linearlinear', 'linearnonlinear',
             'nonlinearnonlinear'):
    data = pd.read_csv(f'results/scores_{name}.csv', header=1,
                    names=['index', 'score', 'method', 'forest'])

    # Knonwing the variance of y, we can extract the R2
    data['R2'] = 1 - data['score'] / y_variances[name]
    # The fold number is encoded at the end of the name of the index
    data['fold'] = data['index'].str.extract('(\d+)$').astype(int)

    data['rel_R2'] = data.groupby(['fold', 'forest'])['R2'].apply(
        lambda df: df - df.mean())


    height_ratios = [data.query('forest == @forest')['method'].nunique()
                    for forest in FORESTS]

    #from matplotlib import colors
    #color_mapping = {k: colors.to_rgb(v) for k, v in color_mapping.items()}

    fig, axes = plt.subplots(3, 1, figsize=(5.6, 8),
                            gridspec_kw=dict(height_ratios=height_ratios))


    for forest, ax in zip(FORESTS, axes):
        this_data = data.query('forest == @forest')
        order = [k for k in color_mapping.keys()
                 if k in this_data['method'].unique()]
        g = sns.boxplot(x="rel_R2", y="method",
                        data=this_data,
                        ax=ax, fliersize=0, palette=color_mapping,
                        order=order,
                        saturation=1,
                        boxprops=dict(ec='k'),
                        medianprops=dict(color='k'))
        ax.set_title(forest, pad=0.5, size=15, loc='right')
        ax.set_xlabel('')
        ax.set_ylabel('')
        ax.axvline(0, color='.8', zorder=0, linewidth=3)
        if name == 'mcar':
            ax.set_xlim(-.065, .07)
        elif name == 'mnar':
            ax.set_xlim(-.29, .14)
        elif name == 'pred':
            ax.set_xlim(-.22, .18)
        elif name == 'linearlinear':
            ax.set_xlim(-.24, .18)
        elif name == 'linearnonlinear':
            ax.set_xlim(-18, 17)
        elif name == 'nonlinearnonlinear':
            ax.set_xlim(-50, 55)
        sns.despine(bottom=True, left=False)
        for i in range(len(order)):
            if i % 2:
                ax.axhspan(i - .5, i + .5, color='.9', zorder=-2)

    plt.tight_layout(pad=.01, h_pad=1)

    # We need to do the ticks in a second pass, as they get modified by
    # the tight_layout
    for forest, ax in zip(FORESTS, axes):
        if name == 'mcar':
            ax.set_xlim(-.065, .07)
        elif name == 'mnar':
            ax.set_xlim(-.29, .14)
        elif name == 'pred':
            ax.set_xlim(-.22, .18)
        elif name == 'linearlinear':
            ax.set_xlim(-.07, .07)
        elif name == 'linearnonlinear':
            ax.set_xlim(-.1, .1)
        elif name == 'nonlinearnonlinear':
            ax.set_xlim(-.065, .07)
        this_data = data.query('forest == @forest')
        ticks = ax.get_xticks()
        ticklabels = list()
        for t in ticks:
            if t < 0:
                ticklabels.append(format_float(t))
            elif t > 0:
                ticklabels.append('+' + format_float(t))
            else:
                ticklabels.append('$%s$' % format_float(
                                  this_data['R2'].mean()))
        ax.set_xticklabels(ticklabels)

    plt.tight_layout(pad=.01, h_pad=1)
    plt.savefig(f'../figures/boxplot_{name}.pdf')
