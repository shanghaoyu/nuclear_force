import numpy as np
import gausslegendremesh as gl
import force_module as pot
from scipy.special import spherical_jn
from scipy.special import gamma


class Matrix:
    # matrix class:generate the matrix we need
####################################################################
    # momentum space    
    # kp,k ------ fm^-1
    # V    ------ Mev fm^-3
####################################################################
    def __init__(self,J,S,Tz,single=True,kl=0,ku=8,N=100,mesh_type='gauleg_finite'):
        # original in momentum space
        # when single == True ,the matrix elements are saved in self.vsingle
        # when single == False ,the matrix elements are saved in self.vcouple(self.vpp,self.vpm,self.vmp,self.vmm)
        
        self.J=J
        self.S=S
        self.Tz=Tz
        self.single=single
        self.kl=kl
        self.ku=ku
        self.N=N
        self.Ndim=0
        
        if mesh_type == 'gauleg_infinite':
            self.MeshPoints, self.MeshWeights =gl.gauss_legendre_inf_mesh(N)
        elif mesh_type == 'gauleg_finite':
            self.MeshPoints, self.MeshWeights =gl.gauss_legendre_line_mesh(N,kl,ku)
        else:
            print("error in gausslegendre mesh points generation.")

        # single channel matrix
        self.vsingle=np.zeros((N,N))

        # couple channel matrix
        self.vpp=np.zeros((N,N))
        self.vpm=np.zeros((N,N))
        self.vmp=np.zeros((N,N))
        self.vmm=np.zeros((N,N))
        self.vcouple=np.zeros((2*N,2*N))
        
        #cauculate the matrix element
        if single== True:
            self.Ndim=N
            if J ==0:
                if S==0:
                    for i in range(N):
                        for j in range(N):
                            self.vsingle[i][j]=pot.cdbonnpot(J,J,self.MeshPoints[i],self.MeshPoints[j],J,S,Tz)
                elif S==1:
                    for i in range(N):
                        for j in range(N):
                            self.vsingle[i][j]=pot.cdbonnpot(J+1,J+1,self.MeshPoints[i],self.MeshPoints[j],J,S,Tz)
                else:
                    print("This channel is prohibited!")
            elif J >=1:
                for i in range(N):
                        for j in range(N):
                            self.vsingle[i][j]=pot.cdbonnpot(J,J,self.MeshPoints[i],self.MeshPoints[j],J,S,Tz) 
            else:
                print("This channel is prohibited!")
        elif single== False:
            self.Ndim=2*N
            for i in range(N):
                for j in range(N):
                    self.vpp[i][j]=pot.cdbonnpot(J+1,J+1,self.MeshPoints[i],self.MeshPoints[j],J,S,Tz) 
                    self.vpm[i][j]=pot.cdbonnpot(J+1,J-1,self.MeshPoints[i],self.MeshPoints[j],J,S,Tz)
                    self.vmp[i][j]=pot.cdbonnpot(J-1,J+1,self.MeshPoints[i],self.MeshPoints[j],J,S,Tz) 
                    self.vmm[i][j]=pot.cdbonnpot(J-1,J-1,self.MeshPoints[i],self.MeshPoints[j],J,S,Tz) 
            self.vcouple=np.block([[self.vmm,self.vmp],[self.vpm,self.vpp]])
############################################################################
#        local project(momentum space to coordinate space)
############################################################################
    def local_projection(self,r):
        # only 1S0 3S1 and 3SD1 using the first formula
        vsingle=0
        if self.single == True:
            if (self.J == 0) and (self.S == 0): 
                #1S0
                for idx,k in enumerate(self.MeshPoints):
                    vsingle += k**2 * self.MeshWeights[idx] * spherical_jn(0, k*r) *self.vsingle[idx][0]
                return vsingle