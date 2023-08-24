import matrix as mt
from matplotlib import pyplot as plt
import numpy as np
# draw the local projection
# 1S0

matrix=mt.Matrix(0,0,0,kl=0,ku=70,N=700)
r = np.linspace(0,4,100)
v=np.zeros((r.size))
for i in range(r.size):
    v[i]=matrix.local_projection(r[i])

fig, ax = plt.subplots(ncols=1, nrows=1)
vmin, vmax = -160, 300 # min and max
ax.plot(r, v, ls='-',c='blue')
ax.set_xlim(0,4)
ax.set_ylim(vmin,vmax)
ax.set_xlabel(r'$r$ (fm)',fontsize=13)
ax.set_ylabel(r'$V(r)$ (MeV)',fontsize=13)
plt.savefig('cdbonn_5.png',dpi=512)