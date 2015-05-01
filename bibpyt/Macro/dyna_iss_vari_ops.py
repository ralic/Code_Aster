# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
import os


def dyna_iss_vari_ops(
    self, NOM_CMP, PRECISION, INTERF, MATR_COHE, UNITE_RESU_FORC,
    UNITE_RESU_IMPE, TYPE, MATR_GENE, INFO, ISSF,
        **args):
    """
       Macro DYNA_ISS_VARI
    """
    ier = 0
    import numpy as NP
    from numpy import linalg
    from math import pi, ceil, sqrt, floor, log, tanh
    import aster_core
    import aster
    from Accas import _F
    from Utilitai.Table import Table
    from Utilitai.Utmess import UTMESS
    from Utilitai.force_iss_vari import force_iss_vari

   #-------------------------------------------------------------------------
   # On importe les definitions des commandes a utiliser dans la macro
   #
    COMB_MATR_ASSE = self.get_cmd('COMB_MATR_ASSE')
    LIRE_IMPE_MISS = self.get_cmd('LIRE_IMPE_MISS')
    LIRE_FORC_MISS = self.get_cmd('LIRE_FORC_MISS')
    DYNA_LINE_HARM = self.get_cmd('DYNA_LINE_HARM')

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    CALC_FONCTION = self.get_cmd('CALC_FONCTION')
    DEFI_INTE_SPEC = self.get_cmd('DEFI_INTE_SPEC')
    REST_SPEC_TEMP = self.get_cmd('REST_SPEC_TEMP')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
    
    from SD.sd_nume_ddl_gene import sd_nume_ddl_gene
    from SD.sd_mode_meca import sd_mode_meca
    from SD.sd_resultat import sd_resultat
    from SD.sd_cham_gene import sd_cham_gene
    
    v_refa_rigi = MATR_GENE['MATR_RIGI'].sdj.REFA.get()
    # MAILLAGE
    nom_bamo = v_refa_rigi[0]
    # MODELE, DDLGENE
    nom_ddlgene = v_refa_rigi[1]
    resultat = self.get_concept(nom_bamo)
    nume_ddlgene = self.get_concept(nom_ddlgene)
    iret,nbmodt,kbid=aster.dismoi('NB_MODES_TOT',nom_bamo,'RESULTAT','F')

    # Comptage commandes + declaration concept sortant
    self.set_icmd(1)
    macro = 'DYNA_ISS_VARI'
    # Type de résultat
    fonc_acce = args['FONC_SIGNAL']
    if fonc_acce != None:
        TYPE_RESU = 'TRANS'
        self.DeclareOut('dyha', self.sd)
    else:
        TYPE_RESU = 'SPEC'
        self.DeclareOut('inte_out', self.sd)

#--------------------------------------------------------------------------------
   # -------- DISCRETISATION frequentielle ou temporelle --------

    if TYPE_RESU == 'SPEC':
        FREQ_INIT = args['FREQ_INIT']
        NB_FREQ = args['NB_FREQ']
        PAS = args['FREQ_PAS']
        OPTION = args['OPTION']
        abscisse = [None] * NB_FREQ
        for k in range(NB_FREQ):
          abscisse[k] = FREQ_INIT + PAS * k
        FMAX = FREQ_INIT + PAS * (NB_FREQ-1)

    if TYPE_RESU == 'TRANS':
        tt, vale_s = fonc_acce.Valeurs()
        DT = tt[1] - tt[0]

        __foint = CALC_FONCTION(
            FFT=_F(FONCTION=fonc_acce,
                   #                             METHODE='COMPLET',
                   METHODE='PROL_ZERO',
                   ),)

        vale_fre, vale_re, vale_im = __foint.Valeurs()
        NB_FREQ2 = len(vale_fre)
        PAS = 1. / (NB_FREQ2 * DT)
        NB_FREQ = int(floor(len(vale_fre) / 2))
                      #signal nombre impair (floor(N/2)) ou signal nombre pair avec REST_SPEC_TEMP (prend N/2 pour N pair)
# NB_FREQ= int(floor(len(vale_fre)/2)+1)  # signal nombre pair: N/2+1
        OMF = 1. / (2. * DT)
        FREQ_INIT = 0.0
        FREQ_COUP = ((NB_FREQ - 1) * PAS)
        FMAX = FREQ_COUP
       # liste des frequences complete
        l_freq_sig = []
        for k in range(NB_FREQ):
            freqk = FREQ_INIT + PAS * k
            l_freq_sig.append(freqk)

        FREQ_FIN = args['FREQ_MAX']
        if FREQ_FIN != None:
# assert (FREQ_FIN > (NB_FREQ-1)*PAS),  'FREQ_FIN = ' + str(FREQ_FIN)  +'
# < frequence de coupure: augmenter FREQ_FIN'
            if FREQ_FIN < FREQ_COUP:
                print 'FREQ_FIN = ', FREQ_FIN, ' < ', 'FREQUENCE DE COUPURE =',  FREQ_COUP, ' on complete par zero'

            PAS = args['FREQ_PAS']
            NB_FREQ = int(ceil(FREQ_FIN / PAS)) + 1
            FREQ_INIT = 0.0
            FMAX = ((NB_FREQ - 1) * PAS)
        abscisse = [None] * NB_FREQ
        for k in range(NB_FREQ):
            freqk = FREQ_INIT + PAS * k
            abscisse[k] = freqk

        if INFO == 2:
            aster.affiche('MESSAGE', 'DISCRETISATION UTILISATEUR :  NB_FREQ, PAS, FREQ_COUP' + str(
                NB_FREQ) + ' ,' + str(PAS) + ' ,' + str(FREQ_COUP))
#---------------------------------------------------------------------------------------

#--------------------------------------------
# stockage de résultats pour toutes les fréquences calculées
    if TYPE_RESU == "SPEC":  # SPEC = (NB_FREQ,nbmodt,nbmodt)
       imod=3
       SPEC = force_iss_vari(self,imod,MATR_GENE,NOM_CMP,ISSF,INFO,UNITE_RESU_FORC,
               UNITE_RESU_IMPE,PRECISION,INTERF,MATR_COHE,TYPE,FREQ_INIT,PAS,FMAX)

    if TYPE_RESU == "TRANS":  # VEC (NB_FREQ,nbmodt)
       imod=2
       VEC = force_iss_vari(self,imod,MATR_GENE,NOM_CMP,ISSF,INFO,UNITE_RESU_FORC,
              UNITE_RESU_IMPE,PRECISION,INTERF,MATR_COHE,TYPE,FREQ_INIT,PAS,FMAX)


# -------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------
#  Creation des sorties : table_fonction pour SPEC ou tran_gene pour TRANS
#--------------------------------------------------------------------------------------------------------------------------
#-
    aster.affiche('MESSAGE', 'TYPE_RESU : ' + TYPE_RESU)
# ---------------------------------------------------------------------
#  Si SPEC: Ecriture des tables
#---------------------------------------------------------------------
    if TYPE_RESU == 'SPEC':

#   ------ CREATION DE L OBJET TABLE
        mcfact = []
        for k2 in range(nbmodt):
            if OPTION == 'DIAG':  # on ecrit uniquement les termes diagonaux (autospectres) de la matrice
                foncc = []
                for k in range(NB_FREQ):
                    foncc.append(abscisse[k])
                    foncc.append(SPEC[k][k2, k2].real)
                    foncc.append(SPEC[k][k2, k2].imag)
                _f = DEFI_FONCTION(NOM_PARA='FREQ',
                                   NOM_RESU='SPEC',
                                   VALE_C=foncc)
            # Ajout d'une ligne dans la Table
                mcfact.append(_F(NUME_ORDRE_I=k2 + 1,
                                 NUME_ORDRE_J=k2 + 1,
                                 FONCTION=_f),)

            else:  # on ecrit tout
                for k1 in range(k2 + 1):
                    foncc = []
                    for k in range(NB_FREQ):
                        foncc.append(abscisse[k])
                        foncc.append(SPEC[k][k1, k2].real)
                        foncc.append(SPEC[k][k1, k2].imag)
                    _f = DEFI_FONCTION(NOM_PARA='FREQ',
                                       NOM_RESU='SPEC',
                                       VALE_C=foncc)
                # Ajout d'une ligne dans la Table
                    mcfact.append(_F(NUME_ORDRE_I=k1 + 1,
                                  NUME_ORDRE_J=k2 + 1,
                                  FONCTION=_f),)

    # Creation du concept en sortie
        inte_out = DEFI_INTE_SPEC(PAR_FONCTION=mcfact,
                                  TITRE='DSP',)
#-
# ---------------------------------------------------------------------
#  Si TRANS: Ecriture de  tran_gene
#---------------------------------------------------------------------
# 1) on cree un concept harm_gene (factice) et le remplit a l'aide de putvectjev avec les bonnes valeurs,
# 2) On interpole les valeurs non calculés (l_freq_sig)
# 3) puis on fait la FFT pour obtenir le signal temporel

    elif TYPE_RESU == 'TRANS':

        __lfre = DEFI_LIST_REEL(VALE=list(l_freq_sig), )
        # on cree la SD resultat - factice (le champ ACCE sera remplace
        # dans la suit par celui calcule)
        __impe = LIRE_IMPE_MISS(BASE=resultat,
                                TYPE=TYPE,
                                NUME_DDL_GENE=nume_ddlgene,
                                UNITE_RESU_IMPE=UNITE_RESU_IMPE,
                                ISSF=ISSF,
                                FREQ_EXTR=PAS,
                                )
        __rito = COMB_MATR_ASSE(COMB_C=(
                                _F(MATR_ASSE=__impe,
                                   COEF_C=1.0 + 0.j,),
                                _F(MATR_ASSE=MATR_GENE['MATR_RIGI'],
                                   COEF_C=1.0 + 0.j,),
                                ),
                                SANS_CMP='LAGR',
                                )

        #    on cree __fosi  pour  RECU_VECT_GENE_C   plus loin
        __fosi = LIRE_FORC_MISS(BASE=resultat,
                                NUME_DDL_GENE=nume_ddlgene,
                                NOM_CMP=NOM_CMP,
                                NOM_CHAM='DEPL',
                                UNITE_RESU_FORC=UNITE_RESU_FORC,
                                ISSF=ISSF,
                                FREQ_EXTR=PAS,)
                                
        __dyge0 = DYNA_LINE_HARM(
            MATR_MASS=MATR_GENE['MATR_MASS'],
            MATR_RIGI=__rito,
            LIST_FREQ=__lfre,  # tuple(l_freq_sig),
            #MATR_AMOR=__ma_amort,
            EXCIT=_F(VECT_ASSE_GENE=__fosi,
                     COEF_MULT_C=1.,
                     ),
        )

#   ATTENTION:  on sort le champ en dépalcement: c'est équivalent au champ en acceleration car on a applique un signal en ACCE pour fosi en acce
        # cela evite de diviser pr w2 pour intégrer l'acceleration (erreurs numeriques au point 0)
        # on remplace donc le champ en acceleration

#      si tous les point on été calculés: pas d'interpolation
        if FREQ_FIN == None:
            for k, freqk in enumerate(l_freq_sig):
                coef_a = (vale_re[k] + vale_im[k] * 1.j)
                VEC_comp = VEC[k] * coef_a
                tup_re = tuple(VEC_comp.real)
                tup_im = tuple(VEC_comp.imag)
                #                                     1         2         3
                #                                   8901234567890123456789012
                aster.putvectjev(__dyge0.get_name() + '           .ACCE        ', nbmodt, tuple(
                    range(nbmodt * k + 1, nbmodt * (k + 1) + 1)), tup_re, tup_im, 1)
        else:

            for k, freqk in enumerate(l_freq_sig):
                coef_a = (vale_re[k] + vale_im[k] * 1.j)
   #  ------------ interpolation du vecteur POD  VEC (NB_FREQ, nbmodt)
                if freqk >= FREQ_FIN:
                    VEC_real = VEC[-1] * 0.0
                    VEC_imag = VEC[-1] * 0.0
                else:
                    vale_i = NP.searchsorted(abscisse, freqk)
                    if vale_i == 0:
                        VEC_comp = VEC[0] * coef_a
                        VEC_real = VEC_comp.real
                        VEC_imag = VEC_comp.imag
                    else:
                        dfp = (freqk - abscisse[vale_i - 1]) / (
                            abscisse[vale_i] - abscisse[vale_i - 1])
                        VEC_comp = (
                            VEC[vale_i - 1] + dfp * (VEC[vale_i] - VEC[vale_i - 1])) * coef_a
                        VEC_real = VEC_comp.real
                        VEC_imag = VEC_comp.imag
                tup_re = tuple(VEC_real)
                tup_im = tuple(VEC_imag)
                #                                     1         2         3
                #                                   8901234567890123456789012
                aster.putvectjev(__dyge0.get_name() + '           .ACCE        ', nbmodt, tuple(
                    range(nbmodt * k + 1, nbmodt * (k + 1) + 1)), tup_re, tup_im, 1)

        print 'REST_SPEC_TEMP'

        dyha = REST_SPEC_TEMP(RESU_GENE=__dyge0,
                              #                        METHODE = 'PROL_ZERO' ,
                              SYMETRIE='NON',
                              NOM_CHAM='ACCE')

    return ier
