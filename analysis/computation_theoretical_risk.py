"""This is the code for figure 2."""

import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
matplotlib.rcParams.update({'font.size': 12})

if not os.path.exists("../figures"): os.mkdir("../figures")

def critmia(s, p):
    t1 = 1/3
    t2 = (1/4) * (p + (1-p)*s**2)**2 / (p + (1-p)*s)
    t3 = (1/4) * (1-p)*(1-s)*(1+s)**2
    return t1 - t2 - t3

def riskmia(p, eta):
    Rp = min(critmia(s, p) for s in np.linspace(0.001, 1, 1000))
    Reta = min(critmia(s, 1-eta) for s in np.linspace(0.001, 1, 1000))
    return Rp*(p<=1-eta) + Reta*(p>1-eta)

def riskblock(p):
    return critmia(1/2, p)

def risksurr(p, eta):
    return 1/48 + p/4 * eta * (1 - eta)

def risksurr2(p, eta):
    return 1/48 + eta * p * 6/48

def riskproba(p):
    return 1/48 + p/8 - p*p/16


plt.clf()
fig, axs = plt.subplots(1, 3, figsize=(10, 4))
for count, (ax, eta) in enumerate(zip(axs, [0., 0.25, 0.75])):
    p = np.linspace(0.001, 0.999, 999)
    rmia = np.array([riskmia(pi, eta) for pi in p])
    rsurr2 = np.array([risksurr2(pi, eta) for pi in p])
    rblock = np.array([riskblock(pi) for pi in p])
    rproba = np.array([riskproba(pi) for pi in p])

    ax.set_ylim(top=0.12)
    if count == 0:
        ax.plot(p, rproba, label='Probabilistic split')
        ax.plot(p, rblock, label='Block propagation')
        ax.plot(p, rmia, label='MIA')
        ax.plot(p, rsurr2, label='Surrogate splits')
    else:
        ax.plot(p, rproba, label=None)
        ax.plot(p, rblock, label=None)
        ax.plot(p, rmia, label=None)
        ax.plot(p, rsurr2, label=None)
    ax.set_xlabel(r"Fraction $p$ of missing values")
    ax.set_ylabel(r"Quadratic risk $R(p)$")
    ax.set_title(r"$\eta=${}".format(eta))
fig.legend(loc=(0.35, 0.05), ncol=2, frameon=False)
plt.tight_layout()
fig.subplots_adjust(bottom=0.3)
fig.savefig("../figures/theo_risk2.pdf")
plt.close(fig)
