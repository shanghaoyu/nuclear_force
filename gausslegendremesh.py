import numpy as np

def gauss_legendre_line_mesh(Np,a,b):
    x, w = np.polynomial.legendre.leggauss(Np)
    # Translate x values from the interval [-1, 1] to [a, b]
    t = 0.5*(x + 1)*(b - a) + a
    u = w * 0.5*(b - a)

    return t,u

def gauss_legendre_inf_mesh(Np):

    scale=100.0
    
    x, w = np.polynomial.legendre.leggauss(Np)
    
    # Translate x values from the interval [-1, 1] to [0, inf)
    pi_over_4 = np.pi/4.0
    
    t = scale*np.tan(pi_over_4*(x+1.0))
    u = scale*pi_over_4/np.cos(pi_over_4*(x+1.0))**2*w
    
    return t,u