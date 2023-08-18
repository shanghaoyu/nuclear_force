import numpy as np
import matrix as mt
import partialwaves as pw
import gausslegendremesh as gl

##########################################
#set the parameters
Jmax=8
NMesh=100
kl=0
ku=8
output_file="name.d"

##########################################
#generate partial waves we need

pws=pw.Partialwaves(Jmax)
print("hlello")
##########################################
#generate gausslegendre meshpoints and mesh weights

mesh_points,mesh_weights=gl.gauss_legendre_line_mesh(NMesh,kl,ku)

##########################################
#write the matrix

with open(output_file, "w") as outfile:
    outfile.write(f"NMesh:\n{NMesh}\n")
    outfile.write(f"Jmax:\n{Jmax}\n")
    outfile.write(f"NChan:\n{pws.NChan}\n")
    outfile.write("Momentum Mesh Points:\n")
    np.savetxt(outfile, mesh_points, fmt="%.17f")
    outfile.write("Momentum Mesh Weights:\n")
    np.savetxt(outfile, mesh_weights, fmt="%.17f")

    for i in range(pws.NChan):
        outfile.write(f"J:\n{pws.J[i]}\n")
        outfile.write(f"Prty:\n{pws.Prty[i]}\n")
        outfile.write(f"S:\n{pws.S[i]}\n")
        outfile.write(f"Tz:\n{pws.Tz[i]}\n")

        #generate the matrix
        matrix=mt.Matrix(pws.J[i],pws.S[i],pws.Tz[i],pws.single[i],kl,ku,NMesh)
        outfile.write(f"Ndim:\n{matrix.Ndim}\n")
        outfile.write("V:\n")
        if pws.single[i]==True:
            np.savetxt(outfile, matrix.vsingle, fmt="%.17f")
        elif pws.single[i]==False:
            np.savetxt(outfile, matrix.vcouple, fmt="%.17f")