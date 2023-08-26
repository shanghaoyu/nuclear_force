import matrix as mt
from matplotlib import pyplot as plt
import numpy as np
import force_module as pot

## general variables 
force=pot.cdbonnpot


# draw the local projection
# 1S0

# matrix=mt.Matrix(force,2,1,0,single=False,kl=0,ku=8,N=100)
# r = np.linspace(0,4,100)
# v=np.zeros((r.size))
# # for i in range(r.size):
# v=matrix.local_projection(r)[2]

# fig, ax = plt.subplots(ncols=1, nrows=1)
# vmin, vmax = -160, 300 # min and max
# ax.plot(r, v, ls='-',c='blue')
# ax.set_xlim(0,4)
# ax.set_ylim(vmin,vmax)
# ax.set_xlabel(r'$r$ (fm)',fontsize=13)
# ax.set_ylabel(r'$V(r)$ (MeV)',fontsize=13)
# plt.savefig('cdbonn_6.png',dpi=512)


###################################################################
# draw the 11 local projection
r = np.linspace(0,4,100)
v=list(range(11))
# generate the 10*r_i numbers
# 1S0
matrix=mt.Matrix(force,0,0,0,single=True,kl=0,ku=8,N=100)
v[0]=matrix.local_projection(r)
# 3P0
matrix=mt.Matrix(force,0,1,0,single=True,kl=0,ku=8,N=100)
v[1]=matrix.local_projection(r)
# 1P1
matrix=mt.Matrix(force,1,0,0,single=True,kl=0,ku=8,N=100)
v[2]=matrix.local_projection(r)
# 3P1
matrix=mt.Matrix(force,1,1,0,single=True,kl=0,ku=8,N=100)
v[3]=matrix.local_projection(r)
# 3S1  3DS1(3SD1) 3D1
matrix=mt.Matrix(force,1,1,0,single=False,kl=0,ku=8,N=100)
v[4]=matrix.local_projection(r)[0]
v[5]=matrix.local_projection(r)[1]
v[6]=matrix.local_projection(r)[3]    
# 3P2  3PF2 3FP2 3F2 
matrix=mt.Matrix(force,2,1,0,single=False,kl=0,ku=8,N=100)
v[7]=matrix.local_projection(r)[0]
v[8]=matrix.local_projection(r)[1]
v[9]=matrix.local_projection(r)[2]
v[10]=matrix.local_projection(r)[3]

# draw the 11 graphs
fig=plt.figure()
ax=[0 for i in range(11)]
for i in range(11):
    ax[i] = fig.add_subplot(3, 4,i+1)
    ax[i].plot(r,v[i],ls='-',c='blue')
plt.savefig('cdbonn_7.png',dpi=512)