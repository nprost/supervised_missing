import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

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


for name in ('mcar', 'mnar', 'pred', 'linearlinear', 'linearnonlinear',
             'nonlinearnonlinear'):
    data = pd.read_csv(f'results/scores_{name}.csv', header=1,
                    names=['index', 'score', 'method', 'forest'])

    # The variance of y is 10, we can extract the R2
    var_y = 10
    data['R2'] = 1 - data['score'] / 10
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
            ax.set_xlim(-.065, .065)
        elif name == 'mnar':
            ax.set_xlim(-.29, .14)
        elif name == 'pred':
            ax.set_xlim(-.22, .12)
        sns.despine(bottom=True, left=False)
        for i in range(len(order)):
            if i % 2:
                ax.axhspan(i - .5, i + .5, color='.9', zorder=-2)

    plt.tight_layout(pad=.01, h_pad=1)
    plt.savefig(f'../figures/boxplot_{name}.pdf')
