"""This is the code for figure 2."""

import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib

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


def critmia(s, p):
    t1 = 1/3
    t2 = (1/4) * (p + (1-p)*s**2)**2 / (p + (1-p)*s)
    t3 = (1/4) * (1-p)*(1-s)*(1+s)**2
    return t1 - t2 - t3


def riskmia(p, eta):
    Rp = min(critmia(s, p) for s in np.linspace(0.001, 1, 1000))
    Reta = min(critmia(s, eta) for s in np.linspace(0.001, 1, 1000))
    return Rp*(p <= eta) + Reta*(p > eta)


def riskblock(p):
    return critmia(1/2, p)


def risksurr(p, eta):
    return 1/48 + p/4 * eta * (1 - eta)


def risksurr2(p, eta):
    return 1/48 + eta * p * 6/48


def riskproba(p):
    return 1/48 + p/8 - p*p/16


if __name__ == '__main__':
    matplotlib.rcParams.update({'font.size': 12})

    if not os.path.exists("figures"):
        os.mkdir("figures")

    plt.clf()
    fig, axs = plt.subplots(1, 3, figsize=(10, 4))
    for count, (ax, eta) in enumerate(zip(axs, [0., 0.25, 0.75])):
        p = np.linspace(0.001, 0.999, 100)
        rmia = np.array([riskmia(pi, eta) for pi in p])
        rsurr2 = np.array([risksurr2(pi, eta) for pi in p])
        rblock = np.array([riskblock(pi) for pi in p])
        rproba = np.array([riskproba(pi) for pi in p])

        ax.set_xlim((0, 1))
        ax.set_ylim(top=0.12)
        if count == 0:
            ax.plot(p, rproba,
                    color=CB_color_cycle[7],
                    label='Probabilistic split')
            ax.plot(p, rblock,
                    '-o', color=CB_color_cycle[5], markevery=10,
                    label='Block propagation')
            ax.plot(p, rmia,
                    '-s', color=CB_color_cycle[2], markevery=12,
                    label='MIA')
            ax.plot(p, rsurr2,
                    '-^', color=CB_color_cycle[0], markevery=14,
                    label='Surrogate splits')
        else:
            ax.plot(p, rproba,
                    color=CB_color_cycle[7],
                    label=None)
            ax.plot(p, rblock,
                    '-o', color=CB_color_cycle[5], markevery=10,
                    label=None)
            ax.plot(p, rmia,
                    '-s', color=CB_color_cycle[2], markevery=12,
                    label=None)
            ax.plot(p, rsurr2,
                    '-^', color=CB_color_cycle[0], markevery=14,
                    label=None)
        ax.set_xlabel(r"Fraction $p$ of missing values")
        ax.set_ylabel(r"Quadratic risk $R(p)$")
        ax.set_title(r"Coupling across variables $\eta=${}".format(eta),
                     fontsize=12)
    fig.legend(loc=(0.15, 0.05), ncol=4, frameon=False)
    plt.tight_layout()
    fig.subplots_adjust(bottom=0.3)
    fig.savefig("figures/theo_risk2.pdf")
    plt.close(fig)
