class Partialwaves:
    # generate the information about prtialwaves we need
    def __init__(self, Jmax=8):
        print("Building partialwaves with Jmax = %i" % Jmax)
        self.Jmax=Jmax
        self.single= []
        self.J= []
        self.S= []
        self.Tz= []
        self.Prty= []
        self.NChan=0

        for inn in range(3):
            j=0
            # 1S0
            self.J.append(j)
            self.S.append(0)
            self.Tz.append(inn-1)
            self.Prty.append(1)
            self.single.append(True)
            self.NChan +=1
            # 3P0
            self.J.append(j)
            self.S.append(1)
            self.Tz.append(inn-1)
            self.Prty.append(-1)
            self.single.append(True)
            self.NChan +=1
            # nn and pp
            if inn != 1:
                for j in range(Jmax):
                    j=j+1
                    if j%2 ==0:
                    # single channel    
                        self.J.append(j)
                        self.S.append(0)
                        self.Tz.append(inn-1)
                        self.Prty.append(1)
                        self.single.append(True)
                        self.NChan +=1
                    # couple channel
                        self.J.append(j)
                        self.S.append(1)
                        self.Tz.append(inn-1)
                        self.Prty.append(-1)
                        self.single.append(False)
                        self.NChan +=1
                    else:
                    # single channel
                        self.J.append(j)
                        self.S.append(1)
                        self.Tz.append(inn-1)
                        self.Prty.append(-1)
                        self.single.append(True)
                        self.NChan +=1
            # np
            else:
                for j in range(Jmax):
                    j=j+1
                    # single channel S=0
                    self.J.append(j)
                    self.S.append(0)
                    self.Tz.append(inn-1)
                    if j%2 == 0:
                        self.Prty.append(1)
                    else:
                        self.Prty.append(-1)
                    self.single.append(True)
                    self.NChan +=1
                    # single channel S=1
                    self.J.append(j)
                    self.S.append(1)
                    self.Tz.append(inn-1)
                    if j%2 == 0:
                        self.Prty.append(1)
                    else:
                        self.Prty.append(-1)
                    self.single.append(True)
                    self.NChan +=1
                    # couple channel
                    self.J.append(j)
                    self.S.append(1)
                    self.Tz.append(inn-1)
                    if j%2 == 0:
                        self.Prty.append(-1)
                    else:
                        self.Prty.append(1)
                    self.single.append(False)
                    self.NChan +=1              