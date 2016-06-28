# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# person_in_charge: irmela.zentner at edf.fr

# Routines for correlated signals ground motion spatial variability
"""spatial_variability.py

A collection of general-purpose routines using Numpy

CALC_COHE        ---   generation of coherency matrix
                       MITA_LUCO(Exponential Mita and Luco model)
                       ABRAHAMSON(Abrahamson rock coherency model)
                       CORRCOEF: correlation coefficient (not frequency dependent)
COHE_DATA_POINTS  ---  evaluate distances from nodal coordinates for coherency
DSP2ACCE_ND        ---   simulation of vector valued random process
gene_traj_gauss_evol_ND        ---   simulation of vector valued random process
"""
from Cata_Utils.t_fonction import t_fonction
from Utilitai.Utmess import UTMESS
from math import pi, exp, sqrt, log, tanh
from cmath import sqrt as csqrt
from cmath import exp as cexp
import numpy as NP
import aster_core
from Utilitai.random_signal_utils import (calc_dsp_FR, calc_dsp_KT, 
               acce_filtre_CP, ACCE2SROM, dsp_filtre_CP)
import aster

# -------------------------------------------------------------------
# COHERENCY MATRIX
# --------------------------------------------------------------------
def CALC_COHE(freq, **kwargs):
#    Frequency is in rad/s: freq= f*2*pi 
#    kwargs: VITE_ONDE, PARA_ALPHA, TYPE, MAILLAGE, 
    model = kwargs['TYPE']
    nom_mail = kwargs['MAILLAGE']
    nom_group_inter = kwargs['GROUP_NO_INTERF']
    if 'NOEUDS_INTERF' in kwargs:
        noe_interf = kwargs['NOEUDS_INTERF']
    else:
        liste_nom, noe_interf = get_group_nom_coord(nom_group_inter, nom_mail)
    if 'DIST' in kwargs:
        DIST2 = kwargs['DIST']
    else:
        DIST2 = calc_dist2(noe_interf)
    # # ----MITA & LUCO
    if model == 'MITA_LUCO' :
       # PARAMETRES fonction de coherence
        VITE_ONDE = kwargs['VITE_ONDE']
        alpha = kwargs['PARA_ALPHA']
        COHE = NP.exp(- (DIST2 * (alpha * freq / VITE_ONDE)**2.))
     #----ABRAHAMSON ROCK (EPRI)      
    elif model == 'ABRAHAMSON' :
        p_a1 = 1.647
        p_a2 = 1.01
        p_a3 = 0.4
        p_n1 = 7.02
        nbno =len(noe_interf)
        freqk = freq / (2.*pi)
        COHE = NP.zeros((nbno, nbno))
        for no1 in range(nbno):
            for no2 in range(nbno):
#                dist_xi = sqrt((XX[no1] - XX[no2])**2 + (YY[no1] - YY[no2])**2)
                dist_xi = sqrt(DIST2[no1,no2])
                p_n2 = 5.1 - 0.51 * log(dist_xi + 10.)
                pfc = -1.886 + 2.221 * log(4000. / (dist_xi + 1.) + 1.5)
                term1 = 1. + (freqk * tanh(p_a3 * dist_xi) / (p_a1 * pfc))**p_n1
                term2 = 1. + (freqk * tanh(p_a3 * dist_xi) / (p_a2 * pfc))**p_n2
                COHE[no1,no2] = 1. / sqrt(term1 * term2)
    elif model == 'ABRA_ROCHER' :
        p_a1 = 1.0
        p_a2 = 40.
        p_a3 = 0.4
        p_n2 = 16.4
        nbno =len(noe_interf)
        freqk = freq / (2.*pi)
        COHE = NP.zeros((nbno, nbno))
        for no1 in range(nbno):
            for no2 in range(nbno):
                dist_xi = sqrt(DIST2[no1,no2])
                p_n1 = 3.8 - 0.04 * log(dist_xi + 1.) + 0.0105 * (log(dist_xi + 1.) - 3.6)**2
                pfc = 27.9 - 4.82 * log(dist_xi + 1.) + 1.24 * (log(dist_xi + 1.) - 3.6)**2
                term1 = 1. + (freqk * tanh(p_a3 * dist_xi) / (p_a1 * pfc))**p_n1
                term2 = 1. + (freqk * tanh(p_a3 * dist_xi) / (p_a2))**p_n2
                COHE[no1,no2] = 1. / sqrt(term1 * term2)
    elif model == 'ABRA_SOLMOYEN' :
        p_a1 = 1.0
        p_a3 = 0.4
        p_n1 = 3.0
        p_n2 = 15.
        nbno =len(noe_interf)
        freqk = freq / (2.*pi)
        COHE = NP.zeros((nbno, nbno))
        for no1 in range(nbno):
            for no2 in range(nbno):
                dist_xi = sqrt(DIST2[no1,no2])
                p_a2 = 15.8-0.044*dist_xi
                pfc = 14.3 - 2.35 * log(dist_xi + 1.)
                term1 = 1. + (freqk * tanh(p_a3 * dist_xi) / (p_a1 * pfc))**p_n1
                term2 = 1. + (freqk * tanh(p_a3 * dist_xi) / (p_a2))**p_n2
                COHE[no1,no2] = 1. / sqrt(term1 * term2)
    return COHE

def get_group_nom_coord(group_inter, nom_mail):
    from Utilitai.partition import MAIL_PY
    mm = MAIL_PY()
    mm.FromAster(nom_mail)
    noeuds_maillage= NP.array(mm.correspondance_noeuds)
    # no des noeuds
    liste_no_interf = list(mm.gno[group_inter])
    nom_no_interf = noeuds_maillage[liste_no_interf]
    # nom des noeuds
    liste_nom_no_int = tuple([ nom.strip() for nom in  nom_no_interf    ])
    COORD_3D = mm.cn
    coord_no_interf = COORD_3D[liste_no_interf]
    if len(coord_no_interf[0])== 2:
        z = NP.zeros((len(coord_no_interf[:,0]),1))
        coord_no_interf = NP.append(coord_no_interf, z, axis=1)
    return liste_nom_no_int, coord_no_interf

def get_no_refe(PHASE_DATA):
    nom_mail = PHASE_DATA['MAILLAGE']
    group_inter = PHASE_DATA['GROUP_NO_INTERF']
    direction = PHASE_DATA['DIRECTION']
    l_nom, coord_no = get_group_nom_coord(group_inter, nom_mail)
    ldir = NP.dot(NP.array(coord_no), NP.array(direction))
    index_min = ldir.argmin()
    coord_refe =  coord_no[index_min]
    return coord_refe



def calc_dist2(noe_interf):
    XX = noe_interf[:,0]
    YY = noe_interf[:,1]
    nbno = len(XX)
    XN=NP.repeat(XX, nbno)
    YN=NP.repeat(YY, nbno)
    XR=NP.reshape(XN, (nbno, nbno))
    YR=NP.reshape(YN, (nbno, nbno))
    XRT = NP.transpose(XR)
    YRT = NP.transpose(YR)
    DX = XR - XRT
    DY = YR - YRT
    DIST = DX**2 + DY**2
    return DIST
      

# -------------------------------------------------------------------
# CORRELATION MATRIX
# --------------------------------------------------------------------
def CALC_CORRE(rho, dim, RATIO_HV = 1.0): 
    if dim == 2:
        Mat_cor = NP.matrix([[1.0 ,rho ],[rho ,1.0]])
    elif dim == 3:
        Mat_cor = NP.matrix([[1.0 ,rho,0.0], [rho ,1.0, 0.0], [0.0 ,0.0, RATIO_HV]  ])
    return Mat_cor

# -------------------------------------------------------------------
# ALGORITHME DE GENERATION DE SIGNAUX GAUSSIENS POUR LE CAS VECTORIEL
# --------------------------------------------------------------------
def DSP2ACCE_ND(f_dsp, data_cohe, rv=None):
   # ----------------------------------
   # IN: f_dsp: dsp function for of list frequencies lw2 on (0, OM)
   #     rv: realisation des N (2) vecteurs 
   #         de variables aleatoires gaussiennes complexe
   #     cohe : coherency matrix or correlation coefficient
   # OUT: Xt: trajectoire du processus gaussien stationnaire normalise (m=0, ect=1)
   # ----------------------------------
    vale_dsp = f_dsp.vale_y
    lw2 = f_dsp.vale_x
    DW = lw2[1] - lw2[0]
    nbfreq2 = len(lw2)
    nbfreq = nbfreq2 * 2
    if data_cohe['TYPE'] == 'COEF_CORR': 
        cohec = data_cohe['MAT_COHE']
        dim = cohec.shape[0]
    else :
        liste_nom, l2 = get_group_nom_coord(data_cohe['GROUP_NO_INTERF'], 
                                       data_cohe['MAILLAGE']) 
        dim = len(liste_nom)
    CS = NP.array([0.0+0j] * dim * nbfreq)    
    CS.resize(nbfreq, dim)  
    Xt = NP.array([0.0] * dim * nbfreq)    
    Xt.resize(dim, nbfreq)  
 
    if rv == None:
        vecc1 = (NP.random.normal(0.0,1.,nbfreq2*dim) + 1j*NP.random.normal(0.0,1.,nbfreq2*dim))
        vecc2 = (NP.random.normal(0.0,1.,nbfreq2*dim) + 1j*NP.random.normal(0.0,1.,nbfreq2*dim))
        vecc1.resize(dim, nbfreq2) 
        vecc2.resize(dim, nbfreq2) 
    elif rv != None:
        rva = NP.array(rv)       
        vecc1 = rva[:,:nbfreq2]
        vecc2 = rva[:,nbfreq2:]
    for (iifr) in range(nbfreq2):
        if data_cohe['TYPE'] != 'COEF_CORR': 
            cohec = CALC_COHE(lw2[iifr], **data_cohe)
            aster_core.matfpe(-1)
            eigv, vec = NP.linalg.eig(cohec)
            aster_core.matfpe(1)
            vec = NP.transpose(vec).real
            eigv = NP.sqrt(NP.where(eigv.real < 1.E-10, 0.0, eigv.real))
            vale_xp = 0.0 + 0.0j
            vale_xn = 0.0 + 0.0j
            for ii in range(dim):
                if eigv[ii] > 1.E-10:
                    dsps = sqrt(vale_dsp[iifr]) * eigv[ii] * vec[ii]
                    vale_xp = NP.dot(dsps, vecc1[ii,iifr]) + vale_xp
                    vale_xn = NP.dot(dsps, vecc2[ii,iifr]) + vale_xn
        else:
            dsps = sqrt(vale_dsp[iifr]) * (cohec)
            vale_xp = NP.dot(dsps, vecc1[:,iifr])
            vale_xn = NP.dot(dsps, vecc2[:,iifr])
        CS[nbfreq2 + iifr] = (vale_xp)
        CS[nbfreq2 - iifr-1] = (vale_xn)

    CS = NP.transpose(CS)
    SX = NP.fft.ifft(CS,nbfreq,1) * nbfreq
    ha = NP.exp(-1.j * pi * NP.arange(nbfreq) * (1. - 1. / nbfreq))      
    for kkk in range(dim): 
        Xt[kkk]=sqrt(DW) * (SX[kkk] * ha).real
    return Xt.tolist()

# --------------------------------------------------------------------------
#  ALGORITHME DE GENERATION DE SIGNAUX GAUSSIENS DSP evolutive non separable
#---------------------------------------------------------------------------
def gene_traj_gauss_evol_ND(self, data_cohe, rv=None, **kwargs):
   #---------------------------------------------
   # IN: calc_dsp_KT: function for the definition of the PSD matrix (KT ou rational type)
   #      lw2: the list of frequencies corresponding to spec (0, OM)
   #      wg, wn: fond freq and evolution [rad/s],
   #      fcp [Hz]: corner frequency for Clough & Penzien filter
   # OUT: Xt: trajectoire du processus gaussien stationnaire normalise (m=0, ect=1)
   #---------------------------------------------
    nbfreq2 = len(self.sampler.liste_w2)
    nbfreq = 2 * nbfreq2
    DW = self.sampler.DW
    fg = kwargs['FREQ_FOND']
    amo = kwargs['AMORT']
    fp = kwargs['FREQ_PENTE']
    TYPE = kwargs['TYPE_DSP']

    if data_cohe['TYPE'] == 'COEF_CORR':  
        cohec = data_cohe['MAT_COHE']
    else :
        cohec = CALC_COHE(DW*10., **data_cohe)
        aster_core.matfpe(-1)
        Mat_cohe = NP.linalg.cholesky(cohec)
        aster_core.matfpe(1)

    dim = cohec.shape[0]
    Xt = NP.array([0.0]*dim*nbfreq)    
    Xt.resize(dim, nbfreq) 

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
      vecc1=(NP.random.normal(0.0,1.,nbfreq2*dim) + 1j*NP.random.normal(0.0,1.,nbfreq2*dim))
      vecc2=(NP.random.normal(0.0,1.,nbfreq2*dim) + 1j*NP.random.normal(0.0,1.,nbfreq2*dim))
      vecc1.resize(dim, nbfreq2) 
      vecc2.resize(dim, nbfreq2) 
    else:
        rva=NP.array(rv)
        vecc1 = rva[:,0: nbfreq2]
        vecc2 = rva[:,nbfreq2:]
    t_mid = 0.5 * (self.modulator.T1 + self.modulator.T2)
    fg_fin = fg + fp * (self.modulator.T2 - t_mid)
    fg_ini = fg + fp * (self.modulator.T1 - t_mid)

    for nii,tii in enumerate(self.sampler.liste_temps):
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
            vale_dsp = calc_dsp_FR(self.sampler.liste_w2,
                       fgt, amo, R0, R2, self.FREQ_CORNER, So=S_cst) * l_FIT   

        vsin = 1.j * NP.sin(self.sampler.liste_w2 * tii)
        vcos = NP.cos(self.sampler.liste_w2 * tii)
        vale_x=[]
        for vdim in range(dim):
             vale_x.append( sum(NP.sqrt(vale_dsp) * vecc1[vdim] * (vcos+vsin) + \
                     NP.sqrt(vale_dsp) * vecc2[vdim] * (vcos-vsin)))
        vale_Xt = NP.dot(cohec,vale_x)
        Xt[:, nii] = NP.real(vale_Xt) * sqrt(DW)
    return Xt.tolist()


def itersimcor_SRO(self, FONC_DSP, data_cohe, **SRO_args):
    # ---------------------------------------------
    # IN  : FONC_DSP: DSP [rad/s], FONC_SPEC: spectre cible [Hz],
    #    amort: amortissement sro,  meme disretisation
    #    type_mod: type de fonction de modulation     niter: nombre d'iterations,
    #    FMIN: fequence min pour fit et filtrage ("corner frequency" Hz)
    # OUT : f_out: accelerogramme apres iterations pour fitter au mieux le spectre cible
    # ---------------------------------------------
   #  dsp in
    FMIN = SRO_args['FMIN']
    amort = SRO_args['AMORT']
    dico_err = SRO_args['DICO_ERR']
    NB_ITER = SRO_args['NB_ITER']
    NB_TIRAGE = data_cohe['DIM']
# dsp initiale
    para_dsp = FONC_DSP.para
    freq_dsp = FONC_DSP.vale_x
    vale_dsp = FONC_DSP.vale_y
    nbfreq2 = len(freq_dsp)
    nbfreq = 2 * nbfreq2
# sro cible
    freq_sro = freq_dsp / (2. * pi)
    vale_sro_ref = SRO_args['FONC_SPEC'].evalfonc(freq_sro).vale_y
    #  fonction de modulation
    hmod = self.modulator.fonc_modul.vale_y
    dt = self.sampler.DT

#  FMIN pour le calcul de l'erreur relative
    FMINM = max(FMIN, 0.1)
    FC = max(self.FREQ_FILTRE, FMINM)
    N1 = NP.searchsorted(freq_sro, FMINM) + 1
    FRED = freq_sro[N1:]
    ZPA = vale_sro_ref[-1]
    vpsum = sum([err_listes[0] for err_listes in dico_err.values()])
    coef_ZPA = dico_err['ERRE_ZPA'][0] / vpsum
    coef_MAX = dico_err['ERRE_MAX'][0] / vpsum
    coef_RMS = dico_err['ERRE_RMS'][0] / vpsum
    rv = NP.random.normal(0.0, 1., nbfreq) + \
        1j * NP.random.normal(0.0, 1., nbfreq)
    list_rv = [rv]
    ntir = 1
    while ntir < NB_TIRAGE:
            rv = NP.random.normal(
                0.0, 1., nbfreq) + 1j * NP.random.normal(0.0, 1., nbfreq)
            list_rv.append(rv)
            ntir = ntir + 1

#  INITIALISATION
    errmult = []
    l_dsp = [FONC_DSP]
    liste_valesro = []
    Xt = DSP2ACCE_ND(FONC_DSP, data_cohe, list_rv)
    for acce in Xt:
        acce = acce * hmod  # modulation
        if self.FREQ_FILTRE > 0.0:
            acce = acce_filtre_CP(acce, dt, self.FREQ_FILTRE)
        f_acce = t_fonction(
             self.sampler.liste_temps, acce, para=self.modulator.para_fonc_modul)
        f_sroi = ACCE2SROM(self, f_acce, amort, freq_sro, 2, 
                                 SRO_args['METHODE_SRO'])
        liste_valesro.append(f_sroi.vale_y)

    if SRO_args['TYPE_ITER'] == 'SPEC_MEDIANE':
        valesro = NP.median(NP.array(liste_valesro), axis=0)
    elif SRO_args['TYPE_ITER'] == 'SPEC_MOYENNE':
        valesro = NP.mean(NP.array(liste_valesro), axis=0)

    l_sro = [valesro]
    err_zpa, err_max, err_min, err_rms, freq_err = erre_spectre(
        FRED, valesro[N1:], vale_sro_ref[N1:])
    #  erreur multiobjectif
    err_ZPA = coef_ZPA * err_zpa
    err_MAX = coef_MAX * err_max
    err_RMS = coef_RMS * err_rms
    errmult.append(
        sqrt(1. / 3. * (err_ZPA ** 2 + err_MAX ** 2 + err_RMS ** 2)))
    if self.INFO == 2:
        UTMESS('I', 'SEISME_43', valr=(
            err_zpa, err_max, err_rms, errmult[-1]))

# ITERATIONS
    for kk in range(NB_ITER):
        #  CALCUL CORRECTION des DSP et mise a jour f_dsp
        nz = NP.nonzero(valesro)
        factm = NP.ones(nbfreq2)
        factm[nz] = vale_sro_ref[nz] / valesro[nz]
        vale_dspi = vale_dsp * factm ** 2
#          vale_dsp[N1:]= vale_dspi[N1:]
        vale_dsp = vale_dspi
        f_dsp = t_fonction(freq_dsp, vale_dsp, para=para_dsp)
        f_dsp = dsp_filtre_CP(f_dsp, FC)
        l_dsp.append(f_dsp)

        #  ITERATION DSP ACCE

        liste_valesro = []
        Xt = DSP2ACCE_ND(f_dsp, data_cohe, list_rv)
        for acce in Xt:
            acce = acce * hmod  # modulation
            if self.FREQ_FILTRE > 0.0:
                acce = acce_filtre_CP(acce, dt, self.FREQ_FILTRE)
            f_acce = t_fonction(
                self.sampler.liste_temps, acce, para=self.modulator.para_fonc_modul)
            f_sroi = ACCE2SROM(
                self, f_acce, amort, freq_sro, 2, SRO_args['METHODE_SRO'])
            liste_valesro.append(f_sroi.vale_y)
        if SRO_args['TYPE_ITER'] == 'SPEC_MEDIANE':
            valesro = NP.median(NP.array(liste_valesro), axis=0)
        elif SRO_args['TYPE_ITER'] == 'SPEC_MOYENNE':
            valesro = NP.mean(NP.array(liste_valesro), axis=0)

        #  CALCUL DES ERREURS
        l_sro.append(valesro)
        err_zpa, err_max, err_min, err_rms, freq_err = erre_spectre(
            FRED, valesro[N1:], vale_sro_ref[N1:])
#         print 'err_zpa, err_max,  err_RMS:', err_zpa, err_max, err_rms
        #  erreur multionjectif
        err_ZPA = coef_ZPA * err_zpa
        err_MAX = coef_MAX * err_max
        err_RMS = coef_RMS * err_rms
        errmult.append(
            sqrt(1. / 3. * (err_ZPA ** 2 + err_MAX ** 2 + err_RMS ** 2)))
        if self.INFO == 2:
            UTMESS('I', 'SEISME_42', vali=(kk + 1, NB_ITER), valr=errmult[-1])
# OPTIMUM
    ind_opt = NP.argmin(NP.array(errmult))
    f_dsp_opt = l_dsp[ind_opt]
    valesro_opt = l_sro[ind_opt]
    err_zpa, err_max, err_min, err_rms, freq_err = erre_spectre(
        FRED, valesro_opt[N1:], vale_sro_ref[N1:])
    dico_err['ERRE_ZPA'].append(err_zpa)
    dico_err['ERRE_MAX'].append(err_max)
    dico_err['ERRE_RMS'].append(err_rms)
    if self.INFO == 2:
        UTMESS('I', 'SEISME_41', vali=ind_opt,
               valr=(errmult[ind_opt], err_max, freq_err[
                     0], err_min, freq_err[1], err_zpa, err_rms)
               )
    for keys, listev in dico_err.items():
        tole = listev[1] * 100.
        erre = abs(listev[-1])
        if abs(erre) > tole:
            nbi = ind_opt
            UTMESS('A', 'SEISME_36', vali=nbi,  valk=keys, valr=(erre, tole))
    return f_dsp_opt, list_rv


# calcul de l'erreur
# ---------------------------------------------
def erre_spectre(Freq, valesro, vale_sro_ref):
    errlin = (valesro - vale_sro_ref) / vale_sro_ref * 100.
    errzpa = errlin[-1]
    errmax = max(abs(errlin))
    errmin = min(errlin)
    errms = sqrt(1. / len(Freq) * NP.sum(errlin ** 2))
    freqerr = ([Freq[NP.argmax(abs(errlin))], Freq[NP.argmin((errlin))]])
    return errzpa, errmax, errmin, errms, freqerr


def itersimcortir_SRO(self, FONC_DSP, data_cohe, NB_TIR, **SRO_args):
    # ---------------------------------------------
    # IN  : FONC_DSP: DSP [rad/s], FONC_SPEC: spectre cible [Hz],
    #    amort: amortissement sro,  meme disretisation
    #    type_mod: type de fonction de modulation     niter: nombre d'iterations,
    #    FMIN: fequence min pour fit et filtrage ("corner frequency" Hz)
    # OUT : f_out: accelerogramme apres iterations pour fitter au mieux le spectre cible
    # ---------------------------------------------
   #  dsp in
    FMIN = SRO_args['FMIN']
    amort = SRO_args['AMORT']
    dico_err = SRO_args['DICO_ERR']
    NB_ITER = SRO_args['NB_ITER']
    dim = data_cohe['DIM']
# dsp initiale
    para_dsp = FONC_DSP.para
    freq_dsp = FONC_DSP.vale_x
    vale_dsp = FONC_DSP.vale_y
    nbfreq2 = len(freq_dsp)
    nbfreq = 2 * nbfreq2
# sro cible
    freq_sro = freq_dsp / (2. * pi)
    vale_sro_ref = SRO_args['FONC_SPEC'].evalfonc(freq_sro).vale_y
    #  fonction de modulation
    hmod = self.modulator.fonc_modul.vale_y
    dt = self.sampler.DT

#  FMIN pour le calcul de l'erreur relative
    FMINM = max(FMIN, 0.1)
    FC = max(self.FREQ_FILTRE, FMINM)
    N1 = NP.searchsorted(freq_sro, FMINM) + 1
    FRED = freq_sro[N1:]
    ZPA = vale_sro_ref[-1]
    vpsum = sum([err_listes[0] for err_listes in dico_err.values()])
    coef_ZPA = dico_err['ERRE_ZPA'][0] / vpsum
    coef_MAX = dico_err['ERRE_MAX'][0] / vpsum
    coef_RMS = dico_err['ERRE_RMS'][0] / vpsum

 #   rv = NP.random.normal(0.0, 1., nbfreq) + \
 #       1j * NP.random.normal(0.0, 1., nbfreq)
    list_rv = []
    ntir = 0
    while ntir < NB_TIR:
        rv=[]
        for kk in range(dim):
            rv.append(NP.random.normal(
                0.0, 1., nbfreq) + 1j * NP.random.normal(0.0, 1., nbfreq))
        list_rv.append(rv)
        ntir = ntir + 1
#  INITIALISATION
    errmult = []
    l_dsp = [FONC_DSP]
    liste_valesro = []
    liste_Xt = []
    for nbtir in range(NB_TIR):    
        Xt = DSP2ACCE_ND(FONC_DSP, data_cohe, list_rv[nbtir])
        liste_Xt.extend(Xt)
    for acce in liste_Xt:
        acce = acce * hmod  # modulation
        if self.FREQ_FILTRE > 0.0:
            acce = acce_filtre_CP(acce, dt, self.FREQ_FILTRE)
        f_acce = t_fonction(
             self.sampler.liste_temps, acce, para=self.modulator.para_fonc_modul)
        f_sroi = ACCE2SROM(self, f_acce, amort, freq_sro, 2, 
                                 SRO_args['METHODE_SRO'])
        liste_valesro.append(f_sroi.vale_y)
    if SRO_args['TYPE_ITER'] == 'SPEC_MEDIANE':
        valesro = NP.median(NP.array(liste_valesro), axis=0)
    elif SRO_args['TYPE_ITER'] == 'SPEC_MOYENNE':
        valesro = NP.mean(NP.array(liste_valesro), axis=0)
    l_sro = [valesro]
    err_zpa, err_max, err_min, err_rms, freq_err = erre_spectre(
        FRED, valesro[N1:], vale_sro_ref[N1:])
    #  erreur multiobjectif
    err_ZPA = coef_ZPA * err_zpa
    err_MAX = coef_MAX * err_max
    err_RMS = coef_RMS * err_rms
    errmult.append(
        sqrt(1. / 3. * (err_ZPA ** 2 + err_MAX ** 2 + err_RMS ** 2)))
    if self.INFO == 2:
        UTMESS('I', 'SEISME_43', valr=(
            err_zpa, err_max, err_rms, errmult[-1]))

# ITERATIONS
    for kk in range(NB_ITER):
        #  CALCUL CORRECTION des DSP et mise a jour f_dsp
        nz = NP.nonzero(valesro)
        factm = NP.ones(nbfreq2)
        factm[nz] = vale_sro_ref[nz] / valesro[nz]
        vale_dspi = vale_dsp * factm ** 2
#          vale_dsp[N1:]= vale_dspi[N1:]
        vale_dsp = vale_dspi
        f_dsp = t_fonction(freq_dsp, vale_dsp, para=para_dsp)
        f_dsp = dsp_filtre_CP(f_dsp, FC)
        l_dsp.append(f_dsp)

        #  ITERATION DSP ACCE

        liste_valesro = []
        liste_Xt = []
        for nbtir in range(NB_TIR):    
            Xt = DSP2ACCE_ND(f_dsp, data_cohe, list_rv[nbtir])
            liste_Xt.extend(Xt)
        for acce in liste_Xt:
            acce = acce * hmod  # modulation
            if self.FREQ_FILTRE > 0.0:
                acce = acce_filtre_CP(acce, dt, self.FREQ_FILTRE)
            f_acce = t_fonction(
                self.sampler.liste_temps, acce, para=self.modulator.para_fonc_modul)
            f_sroi = ACCE2SROM(
                self, f_acce, amort, freq_sro, 2, SRO_args['METHODE_SRO'])
            liste_valesro.append(f_sroi.vale_y)
        if SRO_args['TYPE_ITER'] == 'SPEC_MEDIANE':
            valesro = NP.median(NP.array(liste_valesro), axis=0)
        elif SRO_args['TYPE_ITER'] == 'SPEC_MOYENNE':
            valesro = NP.mean(NP.array(liste_valesro), axis=0)
        #  CALCUL DES ERREURS
        l_sro.append(valesro)
        err_zpa, err_max, err_min, err_rms, freq_err = erre_spectre(
            FRED, valesro[N1:], vale_sro_ref[N1:])
        #  erreur multionjectif
        err_ZPA = coef_ZPA * err_zpa
        err_MAX = coef_MAX * err_max
        err_RMS = coef_RMS * err_rms
        errmult.append(
            sqrt(1. / 3. * (err_ZPA ** 2 + err_MAX ** 2 + err_RMS ** 2)))
        if self.INFO == 2:
            UTMESS('I', 'SEISME_42', vali=(kk + 1, NB_ITER), valr=errmult[-1])
# OPTIMUM
    ind_opt = NP.argmin(NP.array(errmult))
    f_dsp_opt = l_dsp[ind_opt]
    valesro_opt = l_sro[ind_opt]
    err_zpa, err_max, err_min, err_rms, freq_err = erre_spectre(
        FRED, valesro_opt[N1:], vale_sro_ref[N1:])
    dico_err['ERRE_ZPA'].append(err_zpa)
    dico_err['ERRE_MAX'].append(err_max)
    dico_err['ERRE_RMS'].append(err_rms)
    if self.INFO == 2:
        UTMESS('I', 'SEISME_41', vali=ind_opt,
               valr=(errmult[ind_opt], err_max, freq_err[
                     0], err_min, freq_err[1], err_zpa, err_rms)
               )
    for keys, listev in dico_err.items():
        tole = listev[1] * 100.
        erre = abs(listev[-1])
        if abs(erre) > tole:
            nbi = ind_opt
            UTMESS('A', 'SEISME_36', vali=nbi,  valk=keys, valr=(erre, tole))
    return f_dsp_opt, list_rv


