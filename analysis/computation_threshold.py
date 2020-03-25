"""This is the code for figure 1."""

import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib

CB_color_cycle = ['#377eb8', '#ff7f00', '#4daf4a',
                  '#f781bf', '#a65628', '#984ea3',
                  '#999999', '#e41a1c', '#dede00']


def critmia(s, p):
    t1 = 1/3
    t2 = (1/4) * (p + (1-p)*s**2)**2 / (p + (1-p)*s)
    t3 = (1/4) * (1-p)*(1-s)*(1+s)**2
    return t1 - t2 - t3


def argmincritmia(p):
    return min(((s, critmia(s, p)) for s in np.linspace(0, 1, 1001)),
               key=lambda x: x[1])[0]


if __name__ == '__main__':
    matplotlib.rcParams.update({'font.size': 16})

    if not os.path.exists("../figures"):
        os.mkdir("../figures")

    plt.clf()
    # p = np.linspace(0.001, 0.999, 999)
    p = np.linspace(0.001, 0.999, 20)
    argminfp = np.array([argmincritmia(pi) for pi in p])
    plt.plot(argminfp, p,
             '--', color=CB_color_cycle[0], label='MIA')
    plt.plot(np.repeat(0.5, len(p)), p,
             color=CB_color_cycle[1], label='CART')
    plt.plot(1-argminfp, p,
             '--', color=CB_color_cycle[0])
    plt.ylabel(r"Fraction $p$ of missing values")
    plt.xlabel(r"Split position $s^\star_{MIA}(p)$")
    plt.legend(loc='best', framealpha=1)
    plt.tight_layout()
    plt.savefig("../figures/threshold.pdf")
