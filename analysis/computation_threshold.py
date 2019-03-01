"""This is the code for figure 1."""

import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
matplotlib.rcParams.update({'font.size': 16})

if not os.path.exists("../figures"): os.mkdir("../figures")

def critmia(s, p):
    t1 = 1/3
    t2 = (1/4) * (p + (1-p)*s**2)**2 / (p + (1-p)*s)
    t3 = (1/4) * (1-p)*(1-s)*(1+s)**2
    return t1 - t2 - t3

def argmincritmia(p):
    return min(((s, critmia(s, p)) for s in np.linspace(0, 1, 1001)), key=lambda x: x[1])[0]

p = np.linspace(0.001, 0.999, 999)
argminfp = np.array([argmincritmia(pi) for pi in p])
plt.plot(p, argminfp)
plt.xlabel(r"$p$")
plt.ylabel(r"$s^\star_{MIA}(p)$")
plt.tight_layout()
plt.savefig("../figures/threshold.png")
