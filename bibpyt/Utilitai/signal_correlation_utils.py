# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# person_in_charge: irmela.zentner at edf.fr

# Routines for ground motion spatial variability
"""spatial_variability.py

A collection of general-purpose routines using Numpy

CALC_COHE        ---   generation of coherency matrix
                       MITA_LUCO(Exponential Mita and Luco model)
                       ABRAHAMSON(Abrahamson rock coherency model)
                       CORRCOEF: correlation coefficient (not frequency dependent)
COHE_DATA_POINTS  ---  evaluate distances from nodal coordinates for coherency
DSP2ACCEND        ---   simulation of vector valued random process
"""

from Utilitai.Utmess import UTMESS
from math import pi, exp, sqrt, log, tanh
from cmath import sqrt as csqrt
from cmath import exp as cexp
import numpy as NP
from Utilitai.random_signal_utils import (calc_dsp_FR,calc_dsp_KT )

# -------------------------------------------------------------------
# COHERENCY MATRIX
# --------------------------------------------------------------------
def CALC_COHE(self, freqk): 
    XX=self.noe_interf[:,0]
    YY=self.noe_interf[:,1]
    nbno=len(XX)
    # # ----MITA & LUCO
    if self.model=='MITA_LUCO' :
       # PARAMETRES fonction de coherence
        VITE_ONDE = self.VITE_ONDE
        alpha = self.PARA_ALPHA
        XN=NP.repeat(XX,nbno)
        YN=NP.repeat(YY,nbno)
        XR=NP.reshape(XN,(nbno,nbno))
        YR=NP.reshape(YN,(nbno,nbno))
        XRT=NP.transpose(XR)
        YRT=NP.transpose(YR)
        DX=XR-XRT
        DY=YR-YRT
        DIST=DX**2+DY**2
        COHE=NP.exp(-(DIST*(alpha*freqk/VITE_ONDE)**2.))
     #----ABRAHAMSON ROCK (EPRI)      
    elif self.model=='ABRAHAMSON' :
        p_a1=1.647
        p_a2=1.01
        p_a3=0.4
        p_n1=7.02
        COHE=NP.zeros((nbno,nbno))
        for no1 in range(nbno):
            for no2 in range(nbno):
                dist_xi=sqrt((XX[no1]-XX[no2])**2+(YY[no1]-YY[no2])**2)
                p_n2=5.1-0.51*log(dist_xi+10.)
                pfc=-1.886+2.221*log(4000./(dist_xi+1.)+1.5)
                term1=1.+(freqk*tanh(p_a3*dist_xi)/(p_a1*pfc))**p_n1
                term2=1.+(freqk*tanh(p_a3*dist_xi)/(p_a2*pfc))**p_n2
                COHE[no1,no2]=1./sqrt(term1* term2)

    return COHE


def COHE_DATA_POINTS(self):

    from SD.sd_maillage   import sd_maillage

    maillage = sd_maillage(self.nom_mail)
    # coordonnees des noeuds
    l_coordo = maillage.COORDO.VALE.get()
    t_coordo = NP.array(l_coordo)
    t_coordo.shape = nbnot, 3
    # groupno
    coll_grno = maillage.GROUPENO.get()
    group = self.GROUP_NO_COHE 
    l_ind = NP.array(coll_grno.get('%-24s' % group, [])) - 1
    noe_interf = NP.take(t_coordo, l_ind, axis=0)
    nbno, nbval = noe_interf.shape
    if INFO==2:
       aster.affiche('MESSAGE','NBNO INTERFACE : '+str(nbno))

    return noe_interf

# -------------------------------------------------------------------
# CORRELATION MATRIX
# --------------------------------------------------------------------
def CALC_CORRE(rho): 
    Mat_cor = NP.matrix([[1.0 ,rho ],[rho ,1.0]])
    return Mat_cor

# -------------------------------------------------------------------
# ALGORITHME DE GENERATION DE SIGNAUX GAUSSIENS POUR LE CAS VECTORIEL
# --------------------------------------------------------------------
def DSP2ACCE_ND(f_dsp,cohe, rv=None):
   # ----------------------------------
   # IN: f_dsp: dsp function for of list frequencies lw2 on (0, OM)
   #     rv: realisation des N (2) vecteurs 
   #         de variables aleatoires gaussiennes complexe
   #     cohe : coherency matrix or correlation coefficient
   # OUT: Xt: trajectoire du processus gaussien stationnaire normalise (m=0, ect=1)
   # ----------------------------------
    import aster_core

    vale_dsp = f_dsp.vale_y
    lw2 = f_dsp.vale_x
    DW = lw2[1] - lw2[0]
    nbfreq2 = len(lw2)
    nbfreq = nbfreq2 * 2
    dim=cohe.shape[0]
    aster_core.matfpe(-1)
    cohec=NP.linalg.cholesky(cohe)
    aster_core.matfpe(1)
    print  'test coherency matrix', cohec

    CS=NP.matrix([0.0+0j] * dim * nbfreq)    
    CS.resize(nbfreq, dim)  
    Xt=NP.matrix([0.0] * dim * nbfreq)    
    Xt.resize(nbfreq, dim)  

#    if rv == None:
#        rv = NP.random.normal(0.0, 1., nbfreq) + \
 #           1j * NP.random.normal(0.0, 1., nbfreq)
    if rv != None:
        rv1 = rv[:][0:nbfreq2]
        rv2 = rv[:][nbfreq2:]
    for (iifr) in range(nbfreq2):
        dsps = sqrt(vale_dsp[iifr]) * (cohec)
        if rv == None:
            vecc1 = NP.matrix(NP.random.normal(0.0,1.,dim) + 1j * NP.random.normal(0.0,1.,dim))
            vecc2 = NP.matrix(NP.random.normal(0.0,1.,dim) + 1j * NP.random.normal(0.0,1.,dim))
        else :
            vecc1 = rv1[:][iifr]
            vecc2 = rv2[:][iifr]
        vale_xp = dsps * NP.transpose(vecc1)
        vale_xn = dsps * NP.transpose(vecc2)
        CS[nbfreq2 + iifr] = NP.transpose(vale_xp)
        CS[nbfreq2-iifr-1] = NP.transpose(vale_xn)
#        vale_i = sqrt(vale_dsp[iifr])*
#        CS[nbfreq2 + iifr] = vale_i * rv1[iifr]
#        CS[nbfreq2 - iifr - 1] = vale_i * rv2[iifr]
#    SX = NP.fft.ifft(CS) * nbfreq
#    ha = NP.exp(-1.j * pi * NP.arange(nbfreq) * (1. - 1. / nbfreq))
#    Xt = sqrt(DW) * (SX * ha).real

    CS = NP.transpose(CS)
    SX = NP.fft.ifft(CS,nbfreq,1) * nbfreq
    ha = NP.exp(-1.j * pi * NP.arange(nbfreq) * (1.-1. / nbfreq))      
    for kkk in range(nbfreq): 
        Xt[kkk]=sqrt(DW) * (SX[:,kkk] * ha[kkk]).real
    Xt=NP.transpose(Xt) 
    return Xt.tolist()

# --------------------------------------------------------------------------
#  ALGORITHME DE GENERATION DE SIGNAUX GAUSSIENS DSP evolutive non separable
#---------------------------------------------------------------------------

def gene_traj_gauss_evol_D(self, cohe, rv=None, **kwargs):
   #---------------------------------------------
   # IN: calc_dsp_KT: function for the definition of the PSD matrix (KT ou rational type)
   #      lw2: the list of frequencies corresponding to spec (0, OM)
   #      cohe: correlation matrix   
   #      wg, wn: fond freq and evolution [rad/s],
   #      fcp [Hz]: corner frequency for Clough & Penzien filter
   # OUT: Xt: trajectoire du processus gaussien stationnaire normalise (m=0, ect=1)
   #---------------------------------------------
    import aster_core
    from math import cos, sin
    nbfreq2 = len(self.sampler.liste_w2)
    nbfreq = 2 * nbfreq2
    DW = self.sampler.DW
    dim=cohe.shape[0]

    Xt=NP.matrix([0.0]*dim*nbfreq)    
    Xt.resize(dim, nbfreq) 

    fg = kwargs['FREQ_FOND']
    amo = kwargs['AMORT']
    fp = kwargs['FREQ_PENTE']
    TYPE = kwargs['TYPE_DSP']
#    cohe = kwargs['MAT_CORR']

    aster_core.matfpe(-1)
    cohec=NP.linalg.cholesky(cohe)
    aster_core.matfpe(1)

    print  'test coherency matrix', cohec

    if TYPE == 'FR':
        R0 = kwargs['para_R0']
        R2 = kwargs['para_R2']
        l_FIT = kwargs['fonc_FIT'].vale_y
        assert len(
            l_FIT) == nbfreq2,  "ERREUR listes frequences: emettre une fiche anomalie!"
        dsp_fr_refe = calc_dsp_FR(self.sampler.liste_w2,
                                  fg, amo, R0, R2, self.FREQ_CORNER)
      #   calcul de la variance (sigma^2) de normalisation mof
        if 'ALEA_DSP' in kwargs:
            l_ALPHA = kwargs['ALEA_DSP']
            mof = NP.trapz(
                dsp_fr_refe * l_FIT * l_ALPHA, self.sampler.liste_w2) * 2.
            l_FIT = l_FIT * l_ALPHA
        else:
            mof = NP.trapz(dsp_fr_refe * l_FIT, self.sampler.liste_w2) * 2.

    if rv == None:
        vecc1 = [NP.transpose(NP.matrix(NP.random.normal(0.0, 1., dim) + \
            1j * NP.random.normal(0.0, 1., dim)))]*nbfreq
        vecc2 = [NP.transpose(NP.matrix(NP.random.normal(0.0, 1., dim) + \
            1j * NP.random.normal(0.0, 1., dim)))]*nbfreq
#      vecc1=(NP.random.normal(0.0,1.,nbfreq2)+1j*NP.random.normal(0.0,1.,nbfreq2))
#      vecc2=(NP.random.normal(0.0,1.,nbfreq2)+1j*NP.random.normal(0.0,1.,nbfreq2))
    else:
        vecc1 = rv[:][0: nbfreq2]
        vecc2 = rv[:][nbfreq2:]
    t_mid = 0.5 * (self.modulator.T1 + self.modulator.T2)
    fg_fin = fg + fp * (self.modulator.T2 - t_mid)
    fg_ini = fg + fp * (self.modulator.T1 - t_mid)

    for (nii,tii) in enumerate(self.sampler.liste_temps):
        if tii < self.modulator.T1:
            fgt = fg_ini
        elif tii > self.modulator.T2:
            fgt = fg_fin
        else:
            fgt = fg + fp * (tii - t_mid)
        if fgt <= 0.0:
            UTMESS('F', 'SEISME_35', valk=(str(tii)))
        # calcul du facteur de normalisation
        if TYPE == 'KT':
            dsp = calc_dsp_KT(self, fgt, amo)
            # constante de normalisation pour que ecart_type=1 pour tout t
            S_cst = 1. / NP.trapz(dsp, self.sampler.liste_w2) * 0.5
            dsp = calc_dsp_KT(self, fgt, amo, S_cst)
        elif TYPE == 'FR':
            dsp = calc_dsp_FR(self.sampler.liste_w2,
                              fgt, amo, R0, R2, self.FREQ_CORNER)
            # constante de normalisation pour que ecart_type=1 a pour tout t
            S_cst = mof / (NP.trapz(dsp * l_FIT, self.sampler.liste_w2) * 2.)
            dsp = calc_dsp_FR(
                self.sampler.liste_w2, fgt, amo, R0, R2, self.FREQ_CORNER, So=S_cst) * l_FIT

# attention: a verifier
        vale_Xt = 0.0 + 0.0j
        for (iifr, freq) in enumerate(self.sampler.liste_w2):
#            MAT = cohec * sqrt(dsp[iifr])
            vsin = 1.j * sin(freq * tii)
            vcos = cos(freq * tii)
#                vale_xp = (MAT * vecc1[iifr]) * (vcos + vsin) 
#                vale_xn = (MAT * vecc2[iifr]) * (vcos - vsin) 
            vale_Xt = vale_Xt + 2.*(cohec * sqrt(dsp[iifr]) * vecc1[iifr]) * (vcos + vsin)# + \
#                          (MAT * vecc2[iifr]) * (vcos - vsin) 
# attention: a verifier

        Xt[:, nii] = NP.real(vale_Xt) * sqrt(DW) 
        print 'Xt[:, nii]',  Xt[:, nii]
#        vale_xp = NP.sqrt(MAT) * vecc1 * NP.exp(
#            1.j * self.sampler.liste_w2 * tii)
#        vale_xn = NP.sqrt(MAT) * vecc2 * NP.exp(
#            -1.j * self.sampler.liste_w2 * tii)
#        vale_Xt = sum(vale_xp) + sum(vale_xn)
#        Xt.append(vale_Xt.real * sqrt(DW))
    return Xt.tolist()



# --------------------------------------------------------------------------
#  ALGORITHME DE GENERATION DE SIGNAUX GAUSSIENS DSP evolutive non separable
#---------------------------------------------------------------------------
def gene_traj_gauss_evol_ND(self,cohe,  rv=None, **kwargs):
   #---------------------------------------------
   # IN: calc_dsp_KT: function for the definition of the PSD matrix (KT ou rational type)
   #      lw2: the list of frequencies corresponding to spec (0, OM)
   #      wg, wn: fond freq and evolution [rad/s],
   #      fcp [Hz]: corner frequency for Clough & Penzien filter
   # OUT: Xt: trajectoire du processus gaussien stationnaire normalise (m=0, ect=1)
   #---------------------------------------------
    import aster_core
    nbfreq2 = len(self.sampler.liste_w2)
    nbfreq = 2 * nbfreq2
    DW = self.sampler.DW
    Xt = []
    fg = kwargs['FREQ_FOND']
    amo = kwargs['AMORT']
    fp = kwargs['FREQ_PENTE']
    TYPE = kwargs['TYPE_DSP']
    dim=cohe.shape[0]
    aster_core.matfpe(-1)
    cohec=NP.linalg.cholesky(cohe)
    aster_core.matfpe(1)
    print  'test coherency matrix', cohec

    if TYPE == 'FR':
        R0 = kwargs['para_R0']
        R2 = kwargs['para_R2']
        l_FIT = kwargs['fonc_FIT'].vale_y
        assert len(
            l_FIT) == nbfreq2,  "ERREUR listes frequences: emettre une fiche anomalie!"
        dsp_fr_refe = calc_dsp_FR(self.sampler.liste_w2,
                                  fg, amo, R0, R2, self.FREQ_CORNER)
      #   calcul de la variance (sigma^2) de normalisation mof
        if 'ALEA_DSP' in kwargs:
            l_ALPHA = kwargs['ALEA_DSP']
            mof = NP.trapz(
                dsp_fr_refe * l_FIT * l_ALPHA, self.sampler.liste_w2) * 2.
            l_FIT = l_FIT * l_ALPHA
        else:
            mof = NP.trapz(dsp_fr_refe * l_FIT, self.sampler.liste_w2) * 2.
    if rv == None:
#        rv = NP.random.normal(0.0, 1., nbfreq) + \
#            1j * NP.random.normal(0.0, 1., nbfreq)
      vecc1=(NP.random.normal(0.0,1.,nbfreq2) + 1j*NP.random.normal(0.0,1.,nbfreq2))
      vecc2=(NP.random.normal(0.0,1.,nbfreq2) + 1j*NP.random.normal(0.0,1.,nbfreq2))
    else:
        vecc1 = rv[0: nbfreq2]
        vecc2 = rv[nbfreq2:]
    t_mid = 0.5 * (self.modulator.T1 + self.modulator.T2)
    fg_fin = fg + fp * (self.modulator.T2 - t_mid)
    fg_ini = fg + fp * (self.modulator.T1 - t_mid)

    for tii in self.sampler.liste_temps:
        if tii < self.modulator.T1:
            fgt = fg_ini
        elif tii > self.modulator.T2:
            fgt = fg_fin
        else:
            fgt = fg + fp * (tii - t_mid)
        if fgt <= 0.0:
            UTMESS('F', 'SEISME_35', valk=(str(tii)))
        # calcul du facteur de normalisation
        if TYPE == 'KT':
            dsp = calc_dsp_KT(self, fgt, amo)
            # constante de normalisation pour que ecart_type=1 a pour tout t
            S_cst = 1. / NP.trapz(dsp, self.sampler.liste_w2) * 0.5
            vale_dsp = calc_dsp_KT(self, fgt, amo, S_cst)
        elif TYPE == 'FR':
            dsp = calc_dsp_FR(self.sampler.liste_w2,
                              fgt, amo, R0, R2, self.FREQ_CORNER)
            # constante de normalisation pour que ecart_type=1 a pour tout t
            S_cst = mof / (NP.trapz(dsp * l_FIT, self.sampler.liste_w2) * 2.)
            vale_dsp = calc_dsp_FR(
                self.sampler.liste_w2, fgt, amo, R0, R2, self.FREQ_CORNER, So=S_cst) * l_FIT
#        vsin = 1.j * NP.sin(self.sampler.liste_w2 * tii)
#        vcos = NP.cos(self.sampler.liste_w2 * tii)
        vale_xp = NP.sqrt(vale_dsp) * vecc1 * NP.exp( 1.j * self.sampler.liste_w2 * tii)
        vale_xn = NP.sqrt(vale_dsp) * vecc2 * NP.exp( -1.j * self.sampler.liste_w2 * tii)
#        vale_x1 = NP.sqrt(vale_dsp) * vecc1[0] * (vcos+vsin) + \
#         NP.sqrt(vale_dsp) * vecc2[0] * (vcos-vsin)
#        vale_x2 = NP.sqrt(vale_dsp) * vecc1[1] * (vcos+vsin) + \
#         NP.sqrt(vale_dsp) * vecc2[1] * (vcos-vsin)

#        for vdim in range(dim):
#            vale_Xt = cohec[vdim][0]*sum(vale_x1) + cohec[vdim][0]*sum(vale_x2)
#            print cohec[vdim][0], cohec[vdim][1]
#            vale_Xt = cohec[vdim][0]*sum(vale_x1) + cohec[vdim][1]*sum(vale_x2)
#            vale_Xt = sum(vale_x1) + sum(vale_x2)
        vale_Xt = sum(vale_xp) + sum(vale_xn)
        Xt.append(vale_Xt.real * sqrt(DW))
#        Xto=[NP.array(Xt)]*2
        print 'test vale_Xt',  type(vale_Xt)
    print 'test Xt',  type(Xt), len(Xt), type(Xt[0]), type(Xt[1])
    return [Xt]*2




