# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

from geomec_utils import *

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#               Essai Triaxial Monotone Draine (TD)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------


def essai_TD(self, str_n_essai, DicoEssai, MATER, COMPORTEMENT, CONVERGENCE, INFO):
    """
    Essai Triaxial Monotone Draine (TD)
    """
    import numpy as NP
    from Accas import _F
    import aster

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    PRES_CONF = DicoEssai['PRES_CONF']
    EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
    KZERO = DicoEssai['KZERO']
    NB_INST = DicoEssai['NB_INST']

    # dict permettant la gestion des graphiques
    Courbes = {}
    NomsFich = {}
    Leg_x = {}
    Leg_y = {}

    # dict permettant la gestion des tables en sortie
    Resu_Essai = {}
    Resu_Essai['INST'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['EPS_AXI'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['EPS_LAT'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['EPS_VOL'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['SIG_AXI'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['SIG_LAT'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['P'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['Q'] = [[] for k in xrange(len(PRES_CONF))]

    # preparation des graphiques
    str_fich = "Essai_TD_" + str_n_essai + "_"
    preparer_graphique('2', DicoEssai, str_fich, Courbes, NomsFich,
                       Leg_x, Leg_y, {}, {})

    # Creation de la liste d'instants
    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=_F(JUSQU_A=100.,
                                           NOMBRE=NB_INST,),
                             INFO=INFO)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),
                             INFO=INFO,)

    # ---
    # Boucle sur les pressions de confinement PRES_CONF
    # ---
    for i in xrange(len(PRES_CONF)):

        affiche_infos_essai(str_n_essai, "TD", PRES_CONF[i], EPSI_IMPOSE[i])

        # ---
        # Definition des chargements
        # ---
        __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                VALE=(0., 0., 100., EPSI_IMPOSE[i],),)

        __CHAR2 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                VALE=(0., KZERO * PRES_CONF[i], 100., KZERO * PRES_CONF[i],),)

        # ---
        # Calcul
        # ---
        __EVOL = SIMU_POINT_MAT(
            INFO=INFO,
            COMPORTEMENT=COMPORTEMENT.List_F(),
            CONVERGENCE=CONVERGENCE.List_F(),
            MATER=MATER,
            INCREMENT=_F(LIST_INST=__DLIST,
                         INST_INIT=0.,
                         INST_FIN=100.,),
            NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
            ARCHIVAGE=_F(LIST_INST=__RLIST,),
            SIGM_IMPOSE=_F(SIXX=__CHAR2,
                           SIYY=__CHAR2,),
            EPSI_IMPOSE=_F(EPZZ=__CHAR1,),
            SIGM_INIT=_F(SIXX=KZERO * PRES_CONF[i],
                         SIYY=KZERO * PRES_CONF[i],
                         SIZZ=PRES_CONF[i],),
            EPSI_INIT=_F(EPXX=0,
                         EPYY=0,
                         EPZZ=0,
                         EPXY=0,
                         EPXZ=0,
                         EPYZ=0,),)

        # ---
        # Post-traitements
        # ---
        TabRes = __EVOL.EXTR_TABLE().values()
        sig_xx = NP.array(TabRes['SIXX'])
        sig_yy = NP.array(TabRes['SIYY'])
        sig_zz = NP.array(TabRes['SIZZ'])
        eps_xx = NP.array(TabRes['EPXX'])
        eps_yy = NP.array(TabRes['EPYY'])
        eps_zz = NP.array(TabRes['EPZZ'])
        inst = TabRes['INST']
        eps_vol = eps_xx + eps_yy + eps_zz
        p = (sig_xx + sig_yy + sig_zz) / 3.
        q = abs(sig_zz - sig_xx)

        # remplissage des graphiques
        str_leg = "PRES_CONF = " + str("%E" % (PRES_CONF[i])) + ", EPSI_IMPOSE = "\
                                 + str("%E" % (EPSI_IMPOSE[i]))
        remplir_graphique(DicoEssai, Courbes, list(p), list(q), str_leg, "P-Q")
        remplir_graphique(DicoEssai, Courbes, list(
            eps_zz), list(q), str_leg, "EPS_AXI-Q")
        remplir_graphique(DicoEssai, Courbes, list(
            eps_zz), list(eps_vol), str_leg, "EPS_AXI-EPS_VOL")
        remplir_graphique(DicoEssai, Courbes, list(
            p), list(eps_vol), str_leg, "P-EPS_VOL")

        # stockage pour ecriture dans les tables
        Resu_Essai['INST'][i] = inst
        Resu_Essai['EPS_AXI'][i] = list(eps_zz)
        Resu_Essai['EPS_LAT'][i] = list(eps_xx)
        Resu_Essai['EPS_VOL'][i] = list(eps_vol)
        Resu_Essai['SIG_AXI'][i] = list(sig_zz)
        Resu_Essai['SIG_LAT'][i] = list(sig_xx)
        Resu_Essai['P'][i] = list(p)
        Resu_Essai['Q'][i] = list(q)

        DETRUIRE(CONCEPT=_F(NOM=(__CHAR1, __CHAR2, __EVOL),), INFO=1)
    # ---
    # Fin boucle sur les pressions de confinement PRES_CONF
    # ---

    # remplissage des tables
    remplir_tables(self, "TD", str_n_essai, DicoEssai, Resu_Essai)

    # impression des graphiques
    impr_graphique(self, DicoEssai, Courbes, NomsFich, Leg_x, Leg_y, {}, {})

    DETRUIRE(CONCEPT=_F(NOM=(__RLIST, __DLIST),), INFO=1)


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#              Essai Triaxial Monotone Non Draine (TND)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
def essai_TND(self, str_n_essai, DicoEssai, MATER, COMPORTEMENT, CONVERGENCE, INFO):
    """
    Essai Triaxial Monotone Non Draine (TND)
    """
    import numpy as NP
    from Accas import _F
    import aster

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    PRES_CONF = DicoEssai['PRES_CONF']
    EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
    KZERO = DicoEssai['KZERO']
    BIOT_COEF = DicoEssai['BIOT_COEF']
    NB_INST = DicoEssai['NB_INST']

   # dict permettant la gestion des graphiques
    Courbes = {}
    NomsFich = {}
    Leg_x = {}
    Leg_y = {}

    # dict permettant la gestion des tables en sortie
    Resu_Essai = {}
    Resu_Essai['INST'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['EPS_AXI'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['EPS_LAT'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['SIG_AXI'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['SIG_LAT'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['P'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['Q'] = [[] for k in xrange(len(PRES_CONF))]
    Resu_Essai['PRE_EAU'] = [[] for k in xrange(len(PRES_CONF))]

    # preparation des graphiques
    str_fich = "Essai_TND_" + str_n_essai + "_"
    preparer_graphique('2', DicoEssai, str_fich, Courbes, NomsFich,
                       Leg_x, Leg_y, {}, {})

    # Creation de la liste d'instants
    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=_F(JUSQU_A=100.,
                                           NOMBRE=NB_INST,),
                             INFO=INFO)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),
                             INFO=INFO,)

    # ---
    # Boucle sur les pressions de confinement PRES_CONF
    # ---
    for i in xrange(len(PRES_CONF)):

        affiche_infos_essai(str_n_essai, "TND", PRES_CONF[i], EPSI_IMPOSE[i])

        # ---
        # Definition des chargements
        # ---
        __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                VALE=(0., 0., 100., EPSI_IMPOSE[i],),)

        __CHAR2 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                VALE=(0., 0., 100., -0.5 * EPSI_IMPOSE[i],),)

        # ---
        # Calcul, avec tr(eps) = 0 impose
        # ---
        __EVOL = SIMU_POINT_MAT(
            INFO=INFO,
            COMPORTEMENT=COMPORTEMENT.List_F(),
            CONVERGENCE=CONVERGENCE.List_F(),
            MATER=MATER,
            INCREMENT=_F(LIST_INST=__DLIST,
                         INST_INIT=0.,
                         INST_FIN=100.,),
            NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
            ARCHIVAGE=_F(LIST_INST=__RLIST,),
            EPSI_IMPOSE=_F(EPZZ=__CHAR1,
                           EPXX=__CHAR2,
                           EPYY=__CHAR2,),
            SIGM_INIT=_F(SIXX=KZERO * PRES_CONF[i],
                         SIYY=KZERO * PRES_CONF[i],
                         SIZZ=PRES_CONF[i],),
            EPSI_INIT=_F(EPXX=0,
                         EPYY=0,
                         EPZZ=0,
                         EPXY=0,
                         EPXZ=0,
                         EPYZ=0,),)

        # ---
        # Post-traitements
        # ---
        TabRes = __EVOL.EXTR_TABLE().values()
        sig_xx = NP.array(TabRes['SIXX'])
        sig_yy = NP.array(TabRes['SIYY'])
        sig_zz = NP.array(TabRes['SIZZ'])
        eps_xx = NP.array(TabRes['EPXX'])
        eps_yy = NP.array(TabRes['EPYY'])
        eps_zz = NP.array(TabRes['EPZZ'])
        inst = TabRes['INST']
        eps_vol = eps_xx + eps_yy + eps_zz
        p = (sig_xx + sig_yy + sig_zz) / 3.
        q = abs(sig_zz - sig_xx)

        pprime = -1. * q / 3. + PRES_CONF[i]
        pre_eau = (p - pprime) / BIOT_COEF

        # remplissage des graphiques
        str_leg = "PRES_CONF = " + str("%E" % (PRES_CONF[i])) + ", EPSI_IMPOSE = "\
                                 + str("%E" % (EPSI_IMPOSE[i]))
        remplir_graphique(DicoEssai, Courbes, list(p), list(q), str_leg, "P-Q")
        remplir_graphique(DicoEssai, Courbes, list(
            eps_zz), list(q), str_leg, "EPS_AXI-Q")
        remplir_graphique(DicoEssai, Courbes, list(
            eps_zz), list(pre_eau), str_leg, "EPS_AXI-PRE_EAU")

        # stockage pour ecriture dans les tables
        Resu_Essai['INST'][i] = inst
        Resu_Essai['EPS_AXI'][i] = list(eps_zz)
        Resu_Essai['EPS_LAT'][i] = list(eps_xx)
        Resu_Essai['SIG_AXI'][i] = list(sig_zz)
        Resu_Essai['SIG_LAT'][i] = list(sig_xx)
        Resu_Essai['P'][i] = list(p)
        Resu_Essai['Q'][i] = list(q)
        Resu_Essai['PRE_EAU'][i] = list(pre_eau)

        DETRUIRE(CONCEPT=_F(NOM=(__CHAR1, __CHAR2, __EVOL),), INFO=1)
    # ---
    # Fin boucle sur les pressions de confinement PRES_CONF
    # ---

    # remplissage des tables
    remplir_tables(self, "TND", str_n_essai, DicoEssai, Resu_Essai)

    # impression des graphiques
    impr_graphique(self, DicoEssai, Courbes, NomsFich, Leg_x, Leg_y, {}, {})

    DETRUIRE(CONCEPT=_F(NOM=(__RLIST, __DLIST),), INFO=1)


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#             Essai de Cisaillement Cyclique Draine (CISA_C)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
def essai_CISA_C(self, str_n_essai, DicoEssai, MATER, COMPORTEMENT, CONVERGENCE, INFO):
    """
    Essai de Cisaillement Cyclique Draine (CISA_C)
    """
    import numpy as NP
    import math as M
    from Accas import _F
    import aster
    from Utilitai.Utmess import UTMESS

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    PRES_CONF = DicoEssai['PRES_CONF']
    GAMMA_IMPOSE = DicoEssai['GAMMA_IMPOSE']
    GAMMA_ELAS = DicoEssai['GAMMA_ELAS']
    KZERO = DicoEssai['KZERO']
    NB_CYCLE = DicoEssai['NB_CYCLE']
    NB_INST = DicoEssai['NB_INST']

    # dict permettant la gestion des graphiques
    Courbes_niv1 = {}
    NomsFich_niv1 = {}
    Leg_x_niv1 = {}
    Leg_y_niv1 = {}
    Ech_x_niv1 = {}
    Ech_y_niv1 = {}
    Courbes_niv2 = {}
    NomsFich_niv2 = {}
    Leg_x_niv2 = {}
    Leg_y_niv2 = {}

    # preparation des graphiques (niveau 1)
    str_fich_niv1 = "Essai_CISA_C_" + str_n_essai + "_"
    preparer_graphique(
        '1', DicoEssai, str_fich_niv1, Courbes_niv1, NomsFich_niv1,
        Leg_x_niv1, Leg_y_niv1, Ech_x_niv1, Ech_y_niv1)

    # ---
    # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/4 de cycle)
    # ---
    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=[_F(JUSQU_A=10., NOMBRE=NB_INST,), ] +
                             [_F(JUSQU_A=10. * (2 * k + 1), NOMBRE=2 * NB_INST,)
                              for k in xrange(
                              1, 2 * NB_CYCLE + 1)],
                             INFO=INFO)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),
                             INFO=INFO,)

    Gs_list = [[] for k in xrange(len(PRES_CONF))]
    D_list = [[] for k in xrange(len(PRES_CONF))]
    inst_list = [[] for k in xrange(len(PRES_CONF))]
    gamma_list = [[] for k in xrange(len(PRES_CONF))]
    sigxy_list = [[] for k in xrange(len(PRES_CONF))]

    # ---
    # Boucle sur les pressions de confinement PRES_CONF
    # ---
    for i in xrange(len(PRES_CONF)):

        # preparation des graphiques (niveau 2)
        str_fich_niv2 = "Essai_CISA_C_" + str_n_essai + "_"\
            + "PRES_CONF_" + int_2_str(i + 1, len(PRES_CONF)) + "_"
        preparer_graphique(
            '2', DicoEssai, str_fich_niv2, Courbes_niv2, NomsFich_niv2,
            Leg_x_niv2, Leg_y_niv2, {}, {})

        # ---
        # Calcul du module de cisaillement secant maximal (elasticite)
        # ---
        Gs_max = Calc_Gs_max(self, GAMMA_ELAS, PRES_CONF[
                             i], KZERO, MATER, COMPORTEMENT, CONVERGENCE)

        # ---
        # Boucle sur les amplitudes de distorsion GAMMA_IMPOSE
        # ---
        for j in xrange(len(GAMMA_IMPOSE)):

            affiche_infos_essai(
                str_n_essai, "CISA_C", PRES_CONF[i], GAMMA_IMPOSE[j])

            # ---
            # Definition des chargements
            # ---
            __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    ABSCISSE=[0.] + [10. * (2 * k + 1)
                                                     for k in xrange(
                                                     2 * NB_CYCLE + 1)],
                                    ORDONNEE=[0.] + [0.5 * GAMMA_IMPOSE[j] * (-1) ** (k + 1)
                                                     for k in xrange(
                                                     2 * NB_CYCLE + 1)],
                                    )

            __CHAR2 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    VALE=(0., PRES_CONF[i],
                                          10. * (4 * NB_CYCLE + 1), PRES_CONF[i],),)

            __CHAR3 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    VALE=(0., KZERO * PRES_CONF[i],
                                          10. * (4 * NB_CYCLE + 1), KZERO * PRES_CONF[i],),)
            # ---
            # Calcul
            # ---
            __EVOL = SIMU_POINT_MAT(
                INFO=INFO,
                COMPORTEMENT=COMPORTEMENT.List_F(),
                CONVERGENCE=CONVERGENCE.List_F(),
                MATER=MATER,
                INCREMENT=_F(LIST_INST=__DLIST,
                             INST_INIT=0.,
                             INST_FIN=10. * (4 * NB_CYCLE + 1),),
                NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
                ARCHIVAGE=_F(LIST_INST=__RLIST,),
                SIGM_IMPOSE=_F(SIXX=__CHAR3,
                               SIYY=__CHAR3,
                               SIZZ=__CHAR2,),
                EPSI_IMPOSE=_F(EPXY=__CHAR1,),
                SIGM_INIT=_F(SIXX=KZERO * PRES_CONF[i],
                             SIYY=KZERO * PRES_CONF[i],
                             SIZZ=PRES_CONF[i],),
                EPSI_INIT=_F(EPXX=0,
                             EPYY=0,
                             EPZZ=0,
                             EPXY=0,
                             EPXZ=0,
                             EPYZ=0,),)

            # ---
            # Post-traitements
            # ---
            TabRes = __EVOL.EXTR_TABLE().values()
            inst = TabRes['INST']
            sig_xx = NP.array(TabRes['SIXX'])
            sig_yy = NP.array(TabRes['SIYY'])
            sig_zz = NP.array(TabRes['SIZZ'])
            sig_xy = NP.array(TabRes['SIXY'])
            eps_xx = NP.array(TabRes['EPXX'])
            eps_yy = NP.array(TabRes['EPYY'])
            eps_zz = NP.array(TabRes['EPZZ'])
            eps_xy = NP.array(TabRes['EPXY'])
            gamma = 2. * eps_xy
            eps_vol = eps_xx + eps_yy + eps_zz
            p = (sig_xx + sig_yy + sig_zz) / 3.
            q = abs(sig_zz - sig_xx)

            gamma_list[i].append(list(gamma))
            sigxy_list[i].append(list(sig_xy))
            inst_list[i].append(inst)

            #   ind1 -> indice au debut du dernier cycle
            #   ind2 -> indice a la moitie du dernier cycle
            #   ind3 -> indice a la fin du dernier cycle
            ind1 = inst.index(10. * (2 * (2 * NB_CYCLE - 2) + 1))
            ind2 = inst.index(10. * (2 * (2 * NB_CYCLE - 1) + 1))
            ind3 = inst.index(10. * (2 * (2 * NB_CYCLE) + 1))

            # module de cisaillement secant normalise Gs
            Gs = 0.5 * abs(sig_xy[ind3] - sig_xy[ind2]) / GAMMA_IMPOSE[j]
            Gs = Gs / Gs_max
            if not(Gs <= 1. or abs(Gs - 1.) <= 1.e-8):
                UTMESS('F', 'COMPOR2_36', valk=(
                    'CISA_C', 'cisaillement', 'EPSI_ELAS'), valr=(GAMMA_ELAS))
            Gs_list[i].append(Gs)

            # taux d'amortissement D (aire delta_W : methode des trapezes)
            I = NP.sum(0.5 * (sig_xy[ind1:ind2] + sig_xy[ind1 + 1:ind2 + 1]) * (
                gamma[ind1 + 1:ind2 + 1] - gamma[ind1:ind2]))  # 1ere moitié de la boucle
            J = NP.sum(0.5 * (sig_xy[ind2:ind3] + sig_xy[ind2 + 1:ind3 + 1]) * (
                gamma[ind2 + 1:ind3 + 1] - gamma[ind2:ind3]))  # 2eme moitié de la boucle
            delta_W = abs(I + J)

            W = 0.5 * abs(sig_xy[ind3] * GAMMA_IMPOSE[j])
                        # aire du triangle, énergie élastique sur le 1/4 du
                        # cycle

            D = delta_W / (4. * M.pi * W)
            D_list[i].append(D)

            # remplissage des graphiques (niveau 2)
            str_leg2 = "PRES_CONF = " + str("%E" % (PRES_CONF[i])) + ", GAMMA_IMPOSE = "\
                                      + str("%E" % (GAMMA_IMPOSE[j]))
            remplir_graphique(
                DicoEssai, Courbes_niv2, list(p), list(q), str_leg2, "P-Q")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                gamma), list(sig_xy), str_leg2, "GAMMA-SIGXY")

            DETRUIRE(
                CONCEPT=_F(NOM=(__CHAR1, __CHAR2, __CHAR3, __EVOL),), INFO=1)
        # ---
        # Fin boucle sur les amplitudes de cisaillement EPSI_IMPOSE
        # ---

        # remplissage des graphiques (niveau 1)
        str_leg1 = "PRES_CONF = " + str("%E" % (PRES_CONF[i]))

        remplir_graphique(
            DicoEssai, Courbes_niv1, GAMMA_IMPOSE, Gs_list[i], str_leg1, "GAMMA-G")
        remplir_graphique(
            DicoEssai, Courbes_niv1, GAMMA_IMPOSE, D_list[i], str_leg1, "GAMMA-D")
        remplir_graphique(
            DicoEssai, Courbes_niv1, Gs_list[i], D_list[i], str_leg1, "G-D")

        # impression des graphiques (niveau 2)
        impr_graphique(
            self, DicoEssai, Courbes_niv2, NomsFich_niv2, Leg_x_niv2,
            Leg_y_niv2, {}, {})

    # ---
    # Fin boucle sur les sur les pressions de confinement PRES_CONF
    # ---

    # remplissage des tables
    Resu_Essai = {}
    Resu_Essai['INST'] = inst_list
    Resu_Essai['GAMMA'] = gamma_list
    Resu_Essai['SIG_XY'] = sigxy_list
    Resu_Essai['G_SUR_GMAX'] = Gs_list
    Resu_Essai['DAMPING'] = D_list
    remplir_tables(self, "CISA_C", str_n_essai, DicoEssai, Resu_Essai)

    # impression des graphiques (niveau 1)
    impr_graphique(
        self, DicoEssai, Courbes_niv1, NomsFich_niv1, Leg_x_niv1, Leg_y_niv1,
        Ech_x_niv1, Ech_y_niv1)

    DETRUIRE(CONCEPT=_F(NOM=(__RLIST, __DLIST),), INFO=1)


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#             Essai Triaxial Non Draine Cyclique (TND_C)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
def essai_TND_C(self, str_n_essai, DicoEssai, MATER, COMPORTEMENT, CONVERGENCE, INFO):
    """
    Essai Triaxial Non Draine Cyclique Draine (TND_C)
    """
    import numpy as NP
    import math as M
    from Accas import _F
    import aster
    from Utilitai.Utmess import UTMESS
    from Comportement import catalc

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    CALC_POINT_MAT = self.get_cmd('CALC_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    PRES_CONF = DicoEssai['PRES_CONF']
    SIGM_IMPOSE = DicoEssai['SIGM_IMPOSE']
    BIOT_COEF = DicoEssai['BIOT_COEF']
    UN_SUR_K = DicoEssai['UN_SUR_K']
    KZERO = DicoEssai['KZERO']
    NB_CYCLE = DicoEssai['NB_CYCLE']
    NB_INST = DicoEssai['NB_INST']
    RU_MAX = DicoEssai['RU_MAX']
    K_EAU = 1. / UN_SUR_K

    # recuperation du nombre de VI associe a la LdC
    nom_lc = COMPORTEMENT.List_F()[0]['RELATION']
    num_lc, nb_vari = catalc.get_info(nom_lc)
    assert type(nb_vari) is int and nb_vari > 0

    # critere de liquefaction (ru = abs(DeltaPeau/Sig'vertical) >= ru_max)
    ru_max = RU_MAX

    # dict permettant la gestion des graphiques
    Courbes_niv1 = {}
    NomsFich_niv1 = {}
    Leg_x_niv1 = {}
    Leg_y_niv1 = {}
    Courbes_niv2 = {}
    NomsFich_niv2 = {}
    Leg_x_niv2 = {}
    Leg_y_niv2 = {}

    # preparation des graphiques (niveau 1)
    str_fich_niv1 = "Essai_TND_C_" + str_n_essai + "_"
    preparer_graphique(
        '1', DicoEssai, str_fich_niv1, Courbes_niv1, NomsFich_niv1,
        Leg_x_niv1, Leg_y_niv1, {}, {})

    # ---
    # Creation de la liste d'instants / 4*NB_INST : nombre d'instants par cycles
    #                                   duree d'un cycle = 4*10
    # ---
    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=[_F(JUSQU_A=10. * (k + 1), NOMBRE=NB_INST,)
                                         for k in xrange(4 * NB_CYCLE)],
                             INFO=INFO)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),
                             INFO=INFO,)

    inst_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
                 for i in xrange(len(PRES_CONF))]
    epsxx_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
                  for i in xrange(len(PRES_CONF))]
    epszz_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
                  for i in xrange(len(PRES_CONF))]
    epsv_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
                 for i in xrange(len(PRES_CONF))]
    sigxx_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
                  for i in xrange(len(PRES_CONF))]
    sigzz_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
                  for i in xrange(len(PRES_CONF))]
    p_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
              for i in xrange(len(PRES_CONF))]
    q_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
              for i in xrange(len(PRES_CONF))]
    preau_list = [[[] for j in xrange(len(SIGM_IMPOSE))]
                  for i in xrange(len(PRES_CONF))]

    # listes qui serviront uniquement pour GRAPHIQUE
    NCyc_list_graph = [[] for i in xrange(len(PRES_CONF))]
    DSig_list_graph = [[] for i in xrange(len(PRES_CONF))]
    # liste qui servira uniquement pour TABLE_RESU
    NCyc_list_table = [[0] * len(SIGM_IMPOSE) for i in xrange(len(PRES_CONF))]

    # ---
    # Boucle sur les pressions de confinement PRES_CONF
    # ---
    for i in xrange(len(PRES_CONF)):

        # preparation des graphiques (niveau 2)
        str_fich_niv2 = "Essai_TND_C_" + str_n_essai + "_"\
            + "PRES_CONF_" + int_2_str(i + 1, len(PRES_CONF)) + "_"
        preparer_graphique(
            '2', DicoEssai, str_fich_niv2, Courbes_niv2, NomsFich_niv2,
            Leg_x_niv2, Leg_y_niv2, {}, {})

        # ---
        # Boucle sur les amplitudes de variation SIGM_IMPOSE
        # ---
        for j in xrange(len(SIGM_IMPOSE)):

            affiche_infos_essai(
                str_n_essai, "TND_C", PRES_CONF[i], SIGM_IMPOSE[j])

            # ---
            # Definition des chargements
            # ---
            char1_absc = [0.] + [10. * (2 * k + 1)
                                 for k in xrange(2 * NB_CYCLE)] + [10. * (4 * NB_CYCLE)]
            char1_ordo = [PRES_CONF[i]] + [PRES_CONF[i] + SIGM_IMPOSE[j] * (-1) ** k
                                           for k in xrange(2 * NB_CYCLE)] + [PRES_CONF[i]]

            __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    ABSCISSE=char1_absc,
                                    ORDONNEE=char1_ordo,
                                    )

            __CHAR2 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    VALE=(0., KZERO * PRES_CONF[i],
                                          10. * (4 * NB_CYCLE), KZERO * PRES_CONF[i],),)

            # ---
            # Calcul
            # ---
            calc_ok = True

            try:

                __EVOL = CALC_POINT_MAT(
                    INFO=INFO,
                    COMPORTEMENT=COMPORTEMENT.List_F(),
                    CONVERGENCE=CONVERGENCE.List_F(),
                    MATER=MATER,
                    INCREMENT=_F(LIST_INST=__DLIST,
                                 INST_INIT=0.,
                                 INST_FIN=10. * (4 * NB_CYCLE),),
                    NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
                    ARCHIVAGE=_F(LIST_INST=__RLIST,),
                    VECT_IMPO=(_F(NUME_LIGNE=1, VALE=__CHAR2),
                               _F(NUME_LIGNE=2, VALE=__CHAR2),
                               _F(NUME_LIGNE=3, VALE=__CHAR1),),
                    MATR_C1=(_F(NUME_LIGNE=1, NUME_COLONNE=1, VALE=1.),
                             _F(NUME_LIGNE=2, NUME_COLONNE=2, VALE=1.),
                             _F(NUME_LIGNE=3, NUME_COLONNE=3, VALE=1.),),
                    MATR_C2=(_F(NUME_LIGNE=1, NUME_COLONNE=1, VALE=K_EAU),
                             _F(NUME_LIGNE=1,
                                NUME_COLONNE=2, VALE=K_EAU),
                             _F(NUME_LIGNE=1,
                                NUME_COLONNE=3, VALE=K_EAU),
                             _F(NUME_LIGNE=2,
                                NUME_COLONNE=1, VALE=K_EAU),
                             _F(NUME_LIGNE=2,
                                NUME_COLONNE=2, VALE=K_EAU),
                             _F(NUME_LIGNE=2,
                                NUME_COLONNE=3, VALE=K_EAU),
                             _F(NUME_LIGNE=3,
                                NUME_COLONNE=1, VALE=K_EAU),
                             _F(NUME_LIGNE=3,
                                NUME_COLONNE=2, VALE=K_EAU),
                             _F(NUME_LIGNE=3, NUME_COLONNE=3, VALE=K_EAU),),
                    SIGM_INIT= _F(SIXX=KZERO * PRES_CONF[i],
                                  SIYY=KZERO * PRES_CONF[i],
                                  SIZZ=PRES_CONF[i],),
                )

            except aster.NonConvergenceError:
                calc_ok = False
                __EVPOST = self.get_last_concept()
                TabRes = __EVPOST.EXTR_TABLE().values()
                DETRUIRE(CONCEPT=_F(NOM=(__EVPOST)), INFO=1)

            else:
                TabRes = __EVOL.EXTR_TABLE().values()
                DETRUIRE(CONCEPT=_F(NOM=(__EVOL)), INFO=1)

            # post-traitements
            inst = TabRes['INST']
            sig_xx = NP.array(TabRes['SIXX'])
            sig_yy = NP.array(TabRes['SIYY'])
            sig_zz = NP.array(TabRes['SIZZ'])
            eps_xx = NP.array(TabRes['EPXX'])
            eps_yy = NP.array(TabRes['EPYY'])
            eps_zz = NP.array(TabRes['EPZZ'])
            p = (sig_xx + sig_yy + sig_zz) / 3.
            q = sig_zz - sig_xx
            eps_vol = eps_xx + eps_yy + eps_zz
            coef = K_EAU / BIOT_COEF
            pre_eau = -1. * coef * eps_vol
            ru = abs(pre_eau / PRES_CONF[i])

            # le critere de liquefaction est-il atteint?
            rubool = ru >= ru_max
            crit = rubool.any()

            # codret '0' : CALC_POINT_MAT va jusqu'au bout et critere atteint
            if calc_ok and crit:
                codret = '0'
            # codret '1' : CALC_POINT_MAT va jusqu'au bout et critere non
            # atteint
            elif calc_ok and not crit:
                codret = '1'
            # codret '2' : CALC_POINT_MAT s'arrete en NonConvergenceError et
            # critere atteint
            elif not calc_ok and crit:
                codret = '2'
            # codret '3' : CALC_POINT_MAT s'arrete en NonConvergenceError et
            # critere non atteint
            elif not calc_ok and not crit:
                codret = '3'

            # si critere atteint -> MAJ des listes pour "NCYCL-DSIGM"
            ncycrit = 0
            if crit:
                indcrit = list(rubool).index(True)
                ncycrit = int(inst[indcrit] / 40.) + 1
                NCyc_list_graph[i].append(ncycrit)
                DSig_list_graph[i].append(SIGM_IMPOSE[j])
                NCyc_list_table[i][j] = ncycrit

            # si CALC_POINT_MAT s'est arrete en NonConvergenceError
            # -> recup du numero de cycle d'arret du calcul
            ncyerro = 0
            if not calc_ok:
                ncyerro = int(inst[-1] / 40.) + 1

            # stockage pour ecriture dans les tables
            inst_list[i][j] = list(inst)
            sigxx_list[i][j] = list(sig_xx)
            sigzz_list[i][j] = list(sig_zz)
            epsxx_list[i][j] = list(eps_xx)
            epszz_list[i][j] = list(eps_zz)
            epsv_list[i][j] = list(eps_vol)
            p_list[i][j] = list(p)
            q_list[i][j] = list(q)
            preau_list[i][j] = list(pre_eau)

            # remplissage des graphiques (niveau 2)
            str_leg2 = "PRES_CONF = " + str("%E" % (PRES_CONF[i])) + ", SIGM_IMPOSE = "\
                                      + str("%E" % (SIGM_IMPOSE[j]))
            remplir_graphique(
                DicoEssai, Courbes_niv2, list(p), list(q), str_leg2, "P-Q")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                sig_zz), list(pre_eau), str_leg2, "SIG_AXI-PRE_EAU")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                eps_zz), list(pre_eau), str_leg2, "EPS_AXI-PRE_EAU")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                eps_zz), list(q), str_leg2, "EPS_AXI-Q")
            # pour la gestion des alarmes
            affiche_alarm_TND_C(str_n_essai, PRES_CONF[
                                i], SIGM_IMPOSE[j], codret, NB_CYCLE, ncycrit, ncyerro)

            DETRUIRE(CONCEPT=_F(NOM=(__CHAR1, __CHAR2)), INFO=1)
        # ---
        # Fin boucle sur les amplitudes de variation SIGM_IMPOSE
        # ---

        # remplissage des graphiques (niveau 1)
        str_leg1 = "PRES_CONF = " + str("%E" % (PRES_CONF[i]))
        remplir_graphique(
            DicoEssai, Courbes_niv1, NCyc_list_graph[i], DSig_list_graph[i],
            str_leg1, "NCYCL-DSIGM")

        # impression des graphiques (niveau 2)
        impr_graphique(
            self, DicoEssai, Courbes_niv2, NomsFich_niv2, Leg_x_niv2,
            Leg_y_niv2, {}, {})
    # ---
    # Fin boucle sur les sur les pressions de confinement PRES_CONF
    # ---

    # remplissage des tables
    Resu_Essai = {}
    Resu_Essai['INST'] = inst_list
    Resu_Essai['EPS_AXI'] = epszz_list
    Resu_Essai['EPS_LAT'] = epsxx_list
    Resu_Essai['EPS_VOL'] = epsv_list
    Resu_Essai['SIG_AXI'] = sigzz_list
    Resu_Essai['SIG_LAT'] = sigxx_list
    Resu_Essai['P'] = p_list
    Resu_Essai['Q'] = q_list
    Resu_Essai['PRE_EAU'] = preau_list
    Resu_Essai['NCYCL'] = NCyc_list_table
    Resu_Essai['DSIGM'] = [SIGM_IMPOSE for i in xrange(len(PRES_CONF))]
    remplir_tables(self, "TND_C", str_n_essai, DicoEssai, Resu_Essai)

    # impression des graphiques (niveau 1)
    impr_graphique(self, DicoEssai, Courbes_niv1,
                   NomsFich_niv1, Leg_x_niv1, Leg_y_niv1, {}, {})

    DETRUIRE(CONCEPT=_F(NOM=(__RLIST, __DLIST),), INFO=1)


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#             Essai Triaxial Draine Cyclique Alterné (TD_A)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
def essai_TD_A(self, str_n_essai, DicoEssai, MATER, COMPORTEMENT, CONVERGENCE, INFO):
    """
    Essai Triaxial Draine Cyclique Alterné (TD_A)
    """
    import numpy as NP
    import math as M
    from Accas import _F
    import aster
    from Utilitai.Utmess import UTMESS
    from Comportement import catalc

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    PRES_CONF = DicoEssai['PRES_CONF']
    EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
    EPSI_ELAS = DicoEssai['EPSI_ELAS']
    KZERO = DicoEssai['KZERO']
    NB_CYCLE = DicoEssai['NB_CYCLE']
    NB_INST = DicoEssai['NB_INST']

    # recuperation du nombre de VI associe a la LdC
    nom_lc = COMPORTEMENT.List_F()[0]['RELATION']
    num_lc, nb_vari = catalc.get_info(nom_lc)
    assert type(nb_vari) is int and nb_vari > 0

    # dict permettant la gestion des graphiques
    Courbes_niv1 = {}
    NomsFich_niv1 = {}
    Leg_x_niv1 = {}
    Leg_y_niv1 = {}
    Ech_x_niv1 = {}
    Ech_y_niv1 = {}
    Courbes_niv2 = {}
    NomsFich_niv2 = {}
    Leg_x_niv2 = {}
    Leg_y_niv2 = {}

    # preparation des graphiques (niveau 1)
    str_fich_niv1 = "Essai_TD_A_" + str_n_essai + "_"
    preparer_graphique(
        '1', DicoEssai, str_fich_niv1, Courbes_niv1, NomsFich_niv1,
        Leg_x_niv1, Leg_y_niv1, Ech_x_niv1, Ech_y_niv1)
    # ---
    # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/4 de cycle )
    # ---
    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=[_F(JUSQU_A=10., NOMBRE=NB_INST,), ] +
                             [_F(JUSQU_A=10. * (2 * k + 1), NOMBRE=2 * NB_INST,)
                              for k in xrange(
                              1, 2 * NB_CYCLE + 1)],
                             INFO=INFO)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),
                             INFO=INFO,)

    Es_list = [[] for k in xrange(len(PRES_CONF))]
    D_list = [[] for k in xrange(len(PRES_CONF))]
    inst_list = [[] for k in xrange(len(PRES_CONF))]
    epsxx_list = [[] for k in xrange(len(PRES_CONF))]
    epszz_list = [[] for k in xrange(len(PRES_CONF))]
    epsv_list = [[] for k in xrange(len(PRES_CONF))]
    sigxx_list = [[] for k in xrange(len(PRES_CONF))]
    sigzz_list = [[] for k in xrange(len(PRES_CONF))]
    p_list = [[] for k in xrange(len(PRES_CONF))]
    q_list = [[] for k in xrange(len(PRES_CONF))]

    # ---
    # Boucle sur les pressions de confinement PRES_CONF
    # ---
    for i in xrange(len(PRES_CONF)):

        # ---
        # Calcul du module d'Young cyclique equivalent maximal (elasticite)
        # ---
        Es_max = Calc_Es_max_TA(self, EPSI_ELAS, PRES_CONF[
                                i], KZERO, MATER, COMPORTEMENT, CONVERGENCE)

        # preparation des graphiques (niveau 2)
        str_fich_niv2 = "Essai_TD_A_" + str_n_essai + "_"\
            + "PRES_CONF_" + int_2_str(i + 1, len(PRES_CONF)) + "_"
        preparer_graphique(
            '2', DicoEssai, str_fich_niv2, Courbes_niv2, NomsFich_niv2,
            Leg_x_niv2, Leg_y_niv2, {}, {})

        # ---
        # Boucle sur les amplitudes de variation EPSI_IMPOSE
        # ---
        for j in xrange(len(EPSI_IMPOSE)):

            affiche_infos_essai(
                str_n_essai, "TD_A", PRES_CONF[i], EPSI_IMPOSE[j])

            # ---
            # Definition des chargements
            # ---

            __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    ABSCISSE=[0.] + [10. * (2 * k + 1)
                                                     for k in xrange(
                                                     2 * NB_CYCLE + 1)],
                                    ORDONNEE=[0.] + [EPSI_IMPOSE[j] * (-1) ** (k + 1)
                                                     for k in xrange(
                                                     2 * NB_CYCLE + 1)],
                                    )

            __CHAR2 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    VALE=(0., KZERO * PRES_CONF[i],
                                          10. * (4 * NB_CYCLE + 1), KZERO * PRES_CONF[i],),)

            # ---
            # Calcul
            # ---
            __EVOL = SIMU_POINT_MAT(
                INFO=INFO,
                COMPORTEMENT=COMPORTEMENT.List_F(),
                CONVERGENCE=CONVERGENCE.List_F(),
                MATER=MATER,
                INCREMENT=_F(LIST_INST=__DLIST,
                             INST_INIT=0.,
                             INST_FIN=10. * (4 * NB_CYCLE + 1),
                             ),
                NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
                ARCHIVAGE=_F(LIST_INST=__RLIST,),
                SIGM_IMPOSE=_F(SIXX=__CHAR2,
                               SIYY=__CHAR2,),
                EPSI_IMPOSE=_F(EPZZ=__CHAR1,),
                SIGM_INIT=_F(SIXX=KZERO * PRES_CONF[i],
                             SIYY=KZERO * PRES_CONF[i],
                             SIZZ=PRES_CONF[i],),
                EPSI_INIT=_F(EPXX=0,
                             EPYY=0,
                             EPZZ=0,
                             EPXY=0,
                             EPXZ=0,
                             EPYZ=0,),)

            TabRes = __EVOL.EXTR_TABLE().values()

            # post-traitements
            inst = TabRes['INST']
            sig_xx = NP.array(TabRes['SIXX'])
            sig_xy = NP.array(TabRes['SIXY'])
            sig_yy = NP.array(TabRes['SIYY'])
            sig_zz = NP.array(TabRes['SIZZ'])
            eps_xx = NP.array(TabRes['EPXX'])
            eps_yy = NP.array(TabRes['EPYY'])
            eps_zz = NP.array(TabRes['EPZZ'])
            p = (sig_xx + sig_yy + sig_zz) / 3.
            q = sig_zz - sig_xx
            eps_vol = eps_xx + eps_yy + eps_zz

            # stockage pour ecriture dans les tables
            inst_list[i].append(list(inst))
            sigxx_list[i].append(list(sig_xx))
            sigzz_list[i].append(list(sig_zz))
            epsxx_list[i].append(list(eps_xx))
            epszz_list[i].append(list(eps_zz))
            epsv_list[i] .append(list(eps_vol))
            p_list[i]   .append(list(p))
            q_list[i]  .append(list(q))

            #   ind1 -> indice au debut du dernier cycle
            #   ind2 -> indice a la moitie du dernier cycle
            #   ind3 -> indice a la fin du dernier cycle
            ind1 = inst.index(10. * (2 * (2 * NB_CYCLE - 2) + 1))
            ind2 = inst.index(10. * (2 * (2 * NB_CYCLE - 1) + 1))
            ind3 = inst.index(10. * (2 * (2 * NB_CYCLE) + 1))

            # module d'Young cyclique equivalent normalise Es
            Es = 0.5 * abs(q[ind3] - q[ind2]) / EPSI_IMPOSE[j]
            Es = Es / Es_max
            if not(Es <= 1. or abs(Es - 1.) <= 1.e-8):
                UTMESS('F', 'COMPOR2_36', valk=(
                    'TD_A', 'young', 'EPSI_ELAS'), valr=(EPSI_ELAS))
            Es_list[i].append(Es)

# taux d'amortissement D (aire delta_W : methode des trapezes)

# I=NP.sum(0.5*(q[ind1:ind2]+q[ind1+1:ind2+1])*(eps_zz[ind1+1:ind2+1]-eps_zz[ind1:ind2]))  # 1ere moitié de la boucle
# J=NP.sum(0.5*(q[ind2:ind3]+q[ind2+1:ind3+1])*(eps_zz[ind2+1:ind3+1]-eps_zz[ind2:ind3]))
# # 2eme moitié de la boucle

#      W = 0.5*abs(q[ind3]*EPSI_IMPOSE[j])
#      D = delta_W/(4.*M.pi*W)

#      D_list[i].append(D)

            # remplissage des graphiques (niveau 2)
            str_leg2 = "PRES_CONF = " + str("%E" % (PRES_CONF[i])) + ", EPSI_IMPOSE = "\
                                      + str("%E" % (EPSI_IMPOSE[j]))
            remplir_graphique(
                DicoEssai, Courbes_niv2, list(p), list(q), str_leg2, "P-Q")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                eps_zz), list(eps_vol), str_leg2, "EPS_AXI-EPS_VOL")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                eps_zz), list(q), str_leg2, "EPS_AXI-Q")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                eps_vol), list(q), str_leg2, "EPS_VOL-Q")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                p), list(eps_vol), str_leg2, "P-EPS_VOL")

            DETRUIRE(CONCEPT=_F(NOM=(__EVOL, __CHAR1, __CHAR2)), INFO=1)
        # ---
        # Fin boucle sur les amplitudes de variation EPSI_IMPOSE
        # ---

        # courbe tracée en double amplitude
        EPSI_AXI_DA = 2 * NP.array(EPSI_IMPOSE)
        EPSI_AXI_DA = list(EPSI_AXI_DA)

        # remplissage des graphiques (niveau 1)
        str_leg1 = "PRES_CONF = " + str("%E" % (PRES_CONF[i]))

        remplir_graphique(
            DicoEssai, Courbes_niv1, EPSI_AXI_DA, Es_list[i], str_leg1, "EPSI-E")

        # impression des graphiques (niveau 2)
        impr_graphique(
            self, DicoEssai, Courbes_niv2, NomsFich_niv2, Leg_x_niv2,
            Leg_y_niv2, {}, {})
    # ---
    # Fin boucle sur les sur les pressions de confinement PRES_CONF
    # ---

    # remplissage des tables
    Resu_Essai = {}
    Resu_Essai['INST'] = inst_list
    Resu_Essai['EPS_AXI'] = epszz_list
    Resu_Essai['EPS_LAT'] = epsxx_list
    Resu_Essai['EPS_VOL'] = epsv_list
    Resu_Essai['SIG_AXI'] = sigzz_list
    Resu_Essai['SIG_LAT'] = sigxx_list
    Resu_Essai['P'] = p_list
    Resu_Essai['Q'] = q_list
    Resu_Essai['E_SUR_EMAX'] = Es_list

    remplir_tables(self, "TD_A", str_n_essai, DicoEssai, Resu_Essai)

    # impression des graphiques (niveau 1)
    impr_graphique(
        self, DicoEssai, Courbes_niv1, NomsFich_niv1, Leg_x_niv1, Leg_y_niv1,
        Ech_x_niv1, Ech_y_niv1)

    DETRUIRE(CONCEPT=_F(NOM=(__RLIST, __DLIST),), INFO=1)


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#             Essai Triaxial Draine Cyclique non Alterné (TD_NA)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
def essai_TD_NA(self, str_n_essai, DicoEssai, MATER, COMPORTEMENT, CONVERGENCE, INFO):
    """
    Essai Triaxial Draine Cyclique non Alterné (TD_NA)
    """
    import numpy as NP
    import math as M
    from Accas import _F
    import aster
    from Utilitai.Utmess import UTMESS
    from Comportement import catalc

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    PRES_CONF = DicoEssai['PRES_CONF']
    EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
    EPSI_ELAS = DicoEssai['EPSI_ELAS']
    KZERO = DicoEssai['KZERO']
    NB_CYCLE = DicoEssai['NB_CYCLE']
    NB_INST = DicoEssai['NB_INST']

    # recuperation du nombre de VI associe a la LdC
    nom_lc = COMPORTEMENT.List_F()[0]['RELATION']
    num_lc, nb_vari = catalc.get_info(nom_lc)
    assert type(nb_vari) is int and nb_vari > 0

    # dict permettant la gestion des graphiques
    Courbes_niv1 = {}
    NomsFich_niv1 = {}
    Leg_x_niv1 = {}
    Leg_y_niv1 = {}
    Ech_x_niv1 = {}
    Ech_y_niv1 = {}
    Courbes_niv2 = {}
    NomsFich_niv2 = {}
    Leg_x_niv2 = {}
    Leg_y_niv2 = {}

    # preparation des graphiques (niveau 1)
    str_fich_niv1 = "Essai_TD_NA_" + str_n_essai + "_"
    preparer_graphique(
        '1', DicoEssai, str_fich_niv1, Courbes_niv1, NomsFich_niv1,
        Leg_x_niv1, Leg_y_niv1, Ech_x_niv1, Ech_y_niv1)
    # ---
    # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/4 de cycle )
    # ---
    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=[_F(JUSQU_A=20. * k, NOMBRE=2 * NB_INST,)
                                         for k in xrange(
                                         1, 2 * (NB_CYCLE + 1))],
                             INFO=INFO)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),
                             INFO=INFO,)

    Es_list = [[] for k in xrange(len(PRES_CONF))]
    inst_list = [[] for k in xrange(len(PRES_CONF))]
    epsxx_list = [[] for k in xrange(len(PRES_CONF))]
    epszz_list = [[] for k in xrange(len(PRES_CONF))]
    epsv_list = [[] for k in xrange(len(PRES_CONF))]
    sigxx_list = [[] for k in xrange(len(PRES_CONF))]
    sigzz_list = [[] for k in xrange(len(PRES_CONF))]
    p_list = [[] for k in xrange(len(PRES_CONF))]
    q_list = [[] for k in xrange(len(PRES_CONF))]

    # ---
    # Boucle sur les pressions de confinement PRES_CONF
    # ---
    for i in xrange(len(PRES_CONF)):

        # ---
        # Calcul du module d'Young cyclique esuivalent maximal (elasticite)
        # ---
        Es_max = Calc_Es_max_TNA(
            self, EPSI_ELAS, PRES_CONF[i], KZERO, MATER, COMPORTEMENT, CONVERGENCE)

        # preparation des graphiques (niveau 2)
        str_fich_niv2 = "Essai_TD_NA_" + str_n_essai + "_"\
            + "PRES_CONF_" + int_2_str(i + 1, len(PRES_CONF)) + "_"
        preparer_graphique(
            '2', DicoEssai, str_fich_niv2, Courbes_niv2, NomsFich_niv2,
            Leg_x_niv2, Leg_y_niv2, {}, {})

        # ---
        # Boucle sur les amplitudes de variation EPSI_IMPOSE
        # ---
        for j in xrange(len(EPSI_IMPOSE)):

            affiche_infos_essai(
                str_n_essai, "TD_NA", PRES_CONF[i], EPSI_IMPOSE[j])

            # ---
            # Definition des chargements
            # ---

            __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    ABSCISSE=[
                                        20. * k for k in xrange(
                                            2 * (NB_CYCLE + 1))],
                                    ORDONNEE=[0., EPSI_IMPOSE[j]] * (
                                        NB_CYCLE + 1),
                                    )

            __CHAR2 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                    VALE=(0., KZERO * PRES_CONF[i],
                                          20. * (2 * NB_CYCLE + 1), KZERO * PRES_CONF[i],),)

            # ---
            # Calcul
            # ---
            __EVOL = SIMU_POINT_MAT(
                INFO=INFO,
                COMPORTEMENT=COMPORTEMENT.List_F(),
                CONVERGENCE=CONVERGENCE.List_F(),
                MATER=MATER,
                INCREMENT=_F(LIST_INST=__DLIST,
                             INST_INIT=0.,
                             INST_FIN=20. * (2 * NB_CYCLE + 1),
                             ),
                NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
                ARCHIVAGE=_F(LIST_INST=__RLIST,),
                SIGM_IMPOSE=_F(SIXX=__CHAR2,
                               SIYY=__CHAR2,),
                EPSI_IMPOSE=_F(EPZZ=__CHAR1,),
                SIGM_INIT=_F(SIXX=KZERO * PRES_CONF[i],
                             SIYY=KZERO * PRES_CONF[i],
                             SIZZ=PRES_CONF[i],),
                EPSI_INIT=_F(EPXX=0,
                             EPYY=0,
                             EPZZ=0,
                             EPXY=0,
                             EPXZ=0,
                             EPYZ=0,),)

            TabRes = __EVOL.EXTR_TABLE().values()

            # post-traitements
            inst = TabRes['INST']
            sig_xx = NP.array(TabRes['SIXX'])
            sig_xy = NP.array(TabRes['SIXY'])
            sig_yy = NP.array(TabRes['SIYY'])
            sig_zz = NP.array(TabRes['SIZZ'])
            eps_xx = NP.array(TabRes['EPXX'])
            eps_yy = NP.array(TabRes['EPYY'])
            eps_zz = NP.array(TabRes['EPZZ'])
            p = (sig_xx + sig_yy + sig_zz) / 3.
            q = sig_zz - sig_xx
            eps_vol = eps_xx + eps_yy + eps_zz

            # stockage pour ecriture dans les tables
            inst_list[i].append(list(inst))
            sigxx_list[i].append(list(sig_xx))
            sigzz_list[i].append(list(sig_zz))
            epsxx_list[i].append(list(eps_xx))
            epszz_list[i].append(list(eps_zz))
            epsv_list[i] .append(list(eps_vol))
            p_list[i]   .append(list(p))
            q_list[i]  .append(list(q))

            #   ind1 -> indice au debut du dernier cycle
            #   ind2 -> indice a la moitie du dernier cycle
            #   ind3 -> indice a la fin du dernier cycle
            ind1 = inst.index(20. * ((2 * NB_CYCLE - 2) + 1))
            ind2 = inst.index(20. * ((2 * NB_CYCLE - 1) + 1))
            ind3 = inst.index(20. * ((2 * NB_CYCLE) + 1))

            # module d'Young cyclique equivalent normalise Es
            Es = abs(q[ind3] - q[ind2]) / abs(EPSI_IMPOSE[j])
            Es = Es / Es_max
            if not(Es <= 1. or abs(Es - 1.) <= 1.e-8):
                UTMESS('F', 'COMPOR2_36', valk=(
                    'TD_NA', 'Young', 'EPSI_ELAS'), valr=(EPSI_ELAS))
            Es_list[i].append(Es)

            # remplissage des graphiques (niveau 2)
            str_leg2 = "PRES_CONF = " + str("%E" % (PRES_CONF[i])) + ", EPSI_IMPOSE = "\
                                      + str("%E" % (EPSI_IMPOSE[j]))
            remplir_graphique(
                DicoEssai, Courbes_niv2, list(p), list(q), str_leg2, "P-Q")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                eps_zz), list(eps_vol), str_leg2, "EPS_AXI-EPS_VOL")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                eps_zz), list(q), str_leg2, "EPS_AXI-Q")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                eps_vol), list(q), str_leg2, "EPS_VOL-Q")
            remplir_graphique(DicoEssai, Courbes_niv2, list(
                p), list(eps_vol), str_leg2, "P-EPS_VOL")

            DETRUIRE(CONCEPT=_F(NOM=(__EVOL, __CHAR1, __CHAR2)), INFO=1)
        # ---
        # Fin boucle sur les amplitudes de variation EPSI_IMPOSE
        # ---

        # courbe tracée en simple amplitude
        EPSI_AXI = abs(NP.array(EPSI_IMPOSE))
        EPSI_AXI = list(EPSI_AXI)

        # remplissage des graphiques (niveau 1)
        str_leg1 = "PRES_CONF = " + str("%E" % (PRES_CONF[i]))

        remplir_graphique(
            DicoEssai, Courbes_niv1, EPSI_AXI, Es_list[i], str_leg1, "EPSI-E")

        # impression des graphiques (niveau 2)
        impr_graphique(
            self, DicoEssai, Courbes_niv2, NomsFich_niv2, Leg_x_niv2,
            Leg_y_niv2, {}, {})
    # ---
    # Fin boucle sur les sur les pressions de confinement PRES_CONF
    # ---

    # remplissage des tables
    Resu_Essai = {}
    Resu_Essai['INST'] = inst_list
    Resu_Essai['EPS_AXI'] = epszz_list
    Resu_Essai['EPS_LAT'] = epsxx_list
    Resu_Essai['EPS_VOL'] = epsv_list
    Resu_Essai['SIG_AXI'] = sigzz_list
    Resu_Essai['SIG_LAT'] = sigxx_list
    Resu_Essai['P'] = p_list
    Resu_Essai['Q'] = q_list
    Resu_Essai['E_SUR_EMAX'] = Es_list

    remplir_tables(self, "TD_NA", str_n_essai, DicoEssai, Resu_Essai)

    # impression des graphiques (niveau 1)
    impr_graphique(
        self, DicoEssai, Courbes_niv1, NomsFich_niv1, Leg_x_niv1, Leg_y_niv1,
        Ech_x_niv1, Ech_y_niv1)

    DETRUIRE(CONCEPT=_F(NOM=(__RLIST, __DLIST),), INFO=1)


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#             Essai Oedometrique  Draine Cyclique  (OEDO_C)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
def essai_OEDO_C(self, str_n_essai, DicoEssai, MATER, COMPORTEMENT, CONVERGENCE, INFO):
    """
    Essai Oedometrique Draine Cyclique (OEDO_C)
    """
    import numpy as NP
    import math as M
    from Accas import _F
    import aster
    from Utilitai.Utmess import UTMESS
    from Comportement import catalc

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    PRES_CONF = DicoEssai['PRES_CONF']
    SIGM_DECH = DicoEssai['SIGM_DECH']
    SIGM_IMPOSE = DicoEssai['SIGM_IMPOSE']
    KZERO = DicoEssai['KZERO']
    NB_INST = DicoEssai['NB_INST']

    # recuperation du nombre de VI associe a la LdC
    nom_lc = COMPORTEMENT.List_F()[0]['RELATION']
    num_lc, nb_vari = catalc.get_info(nom_lc)
    assert type(nb_vari) is int and nb_vari > 0

    # dict permettant la gestion des graphiques
    Courbes_niv1 = {}
    NomsFich_niv1 = {}
    Leg_x_niv1 = {}
    Leg_y_niv1 = {}

    # preparation des graphiques (niveau 1)
    str_fich_niv1 = "Essai_OEDO_C_" + str_n_essai + "_"
    preparer_graphique(
        '1', DicoEssai, str_fich_niv1, Courbes_niv1, NomsFich_niv1,
        Leg_x_niv1, Leg_y_niv1, {}, {})

    # ---
    # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/2 de cycle)
    # ---

    inst_list = [[] for k in xrange(len(PRES_CONF))]
    sigxx_list = [[] for k in xrange(len(PRES_CONF))]
    sigzz_list = [[] for k in xrange(len(PRES_CONF))]
    epsv_list = [[] for k in xrange(len(PRES_CONF))]
    p_list = [[] for k in xrange(len(PRES_CONF))]
    sigm_impose_list = [[] for k in xrange(len(PRES_CONF))]

    # ---
    # Boucle sur les pressions de consolidation initiale PRES_CONF
    # ---
    for i in xrange(len(PRES_CONF)):

        long_cyc = 2 * 10.
        inst_ini = -1. * long_cyc
        pas_inst = long_cyc / (2. * NB_INST)

        # ---
        # Boucle sur les amplitudes de variation SIGM_IMPOSE: un cycle correspond a
        # une contrainte SIGM_IMPOSE
        # ---
        for j in xrange(len(SIGM_IMPOSE)):

            affiche_infos_essai(
                str_n_essai, "OEDO_C", PRES_CONF[i], SIGM_IMPOSE[j], SIGM_DECH[i])

            Ltmp_inst = []
            Ltmp_sixx = []
            Ltmp_sizz = []
            Ltmp_epv = []
            Ltmp_p = []

            # ---
            # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/2 de cycle )
            # ---
            inst_ini += long_cyc
            L_inst = [
                inst_ini + k * pas_inst for k in xrange(2 * NB_INST + 1)]

            __RLIST = DEFI_LIST_REEL(VALE=L_inst, INFO=INFO)

            __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                                     ECHEC=_F(SUBD_METHODE='MANUEL',
                                              SUBD_PAS=10,
                                              SUBD_NIVEAU=10,),
                                     INFO=INFO,)

            # ---
            # Definition des chargements
            # ---
            if j == 0:

                __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                        VALE=(inst_ini, PRES_CONF[i],
                                              inst_ini + long_cyc /
                                              2., PRES_CONF[
                                              i] + SIGM_IMPOSE[j],
                                              inst_ini + long_cyc, SIGM_DECH[i],))

            else:
                __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                        VALE=(inst_ini, SIGM_DECH[i],
                                              inst_ini + long_cyc /
                                              2., SIGM_DECH[
                                              i] + SIGM_IMPOSE[j],
                                              inst_ini + long_cyc, SIGM_DECH[i],))

            # s'il s'agit du premier cycle, Cond_Init suivantes:
            if j == 0:
                Cond_Init = {}
                Cond_Init['VARI'] = [0.] * nb_vari
                Cond_Init['SIXX'] = KZERO * PRES_CONF[i]
                Cond_Init['SIYY'] = KZERO * PRES_CONF[i]
                Cond_Init['SIZZ'] = PRES_CONF[i]
                Cond_Init['EPXX'] = 0.
                Cond_Init['EPYY'] = 0.
                Cond_Init['EPZZ'] = 0.
                Cond_Init['EPXY'] = 0.
                Cond_Init['EPXZ'] = 0.
                Cond_Init['EPYZ'] = 0.

            # ---
            # Calcul
            # ---

            __EVOL = SIMU_POINT_MAT(
                INFO=INFO,
                COMPORTEMENT=COMPORTEMENT.List_F(),
                CONVERGENCE=CONVERGENCE.List_F(),
                MATER=MATER,
                SUPPORT='POINT',
                INCREMENT=_F(LIST_INST=__DLIST,
                             INST_INIT=inst_ini,
                             INST_FIN=inst_ini + long_cyc,),
                NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
                ARCHIVAGE=_F(LIST_INST=__RLIST,),
                VECT_IMPO=(
                    _F(NUME_LIGNE=3, VALE=__CHAR1),),
                MATR_C1=(_F(NUME_LIGNE=1, NUME_COLONNE=1, VALE=0.),
                         _F(NUME_LIGNE=2, NUME_COLONNE=2, VALE=0.),
                         _F(NUME_LIGNE=3, NUME_COLONNE=3, VALE=1.),),
                MATR_C2=(_F(NUME_LIGNE=1, NUME_COLONNE=1, VALE=1.),
                         _F(NUME_LIGNE=2, NUME_COLONNE=2, VALE=1.),
                         _F(NUME_LIGNE=3, NUME_COLONNE=3, VALE=1.),
                         _F(NUME_LIGNE=4, NUME_COLONNE=4, VALE=1.),
                         _F(NUME_LIGNE=5, NUME_COLONNE=5, VALE=1.),
                         _F(NUME_LIGNE=6, NUME_COLONNE=6, VALE=1.),),
                SIGM_INIT= _F(SIXX=Cond_Init['SIXX'],
                              SIYY=Cond_Init['SIYY'],
                              SIZZ=Cond_Init['SIZZ'],),
                EPSI_INIT= _F(EPXX=Cond_Init['EPXX'],
                              EPYY=Cond_Init['EPYY'],
                              EPZZ=Cond_Init['EPZZ'],
                              EPXY=Cond_Init['EPXY'],
                              EPXZ=Cond_Init['EPXZ'],
                              EPYZ=Cond_Init['EPYZ'],),
                VARI_INIT= _F(VALE=Cond_Init['VARI']),)

            TabRes = __EVOL.EXTR_TABLE().values()
            # PostTraiter les grandeurs d'interet...
            sig_xx = NP.array(TabRes['SIXX'])
            sig_yy = NP.array(TabRes['SIYY'])
            sig_zz = NP.array(TabRes['SIZZ'])
            eps_xx = NP.array(TabRes['EPXX'])
            eps_yy = NP.array(TabRes['EPYY'])
            eps_zz = NP.array(TabRes['EPZZ'])
            eps_xy = NP.array(TabRes['EPXY'])
            eps_xz = NP.array(TabRes['EPXZ'])
            eps_yz = NP.array(TabRes['EPYZ'])
            inst = TabRes['INST']
            p = (sig_xx + sig_yy + sig_zz) / 3.
            eps_vol = eps_xx + eps_yy + eps_zz

            # .. et les stocker
            Ltmp_inst += inst
            Ltmp_sixx += list(sig_xx)
            Ltmp_epv += list(eps_vol)
            Ltmp_sizz += list(sig_zz)
            Ltmp_p += list(p)

            # Cond_Init : vi, eps, et sig au dernier inst pr CI cycle suivant
            Cond_Init = {'VARI': []}
            for i_vari in xrange(1, nb_vari + 1):
                VarTmp = TabRes["V" + str(i_vari)]
                Cond_Init['VARI'].append(VarTmp[-1])
            Cond_Init['SIXX'] = sig_xx[-1]
            Cond_Init['SIYY'] = sig_yy[-1]
            Cond_Init['SIZZ'] = sig_zz[-1]
            Cond_Init['EPXX'] = eps_xx[-1]
            Cond_Init['EPYY'] = eps_yy[-1]
            Cond_Init['EPZZ'] = eps_zz[-1]
            Cond_Init['EPXY'] = eps_xy[-1]
            Cond_Init['EPXZ'] = eps_xz[-1]
            Cond_Init['EPYZ'] = eps_yz[-1]

            DETRUIRE(
                CONCEPT=_F(NOM=(__RLIST, __DLIST, __CHAR1, __EVOL)), INFO=1)

            #

            # stockage pour ecriture dans les tables
            inst_list[i] += Ltmp_inst
            sigxx_list[i] += Ltmp_sixx
            sigzz_list[i] += Ltmp_sizz
            epsv_list[i] += Ltmp_epv
            p_list[i] += Ltmp_p
            sigm_impose_list[i] += [SIGM_IMPOSE[j]] * len(Ltmp_inst)

        # ---
        # Fin boucle sur les amplitudes de variation SIGM_IMPOSE
        # ---
        # remplissage des graphiques (niveau 1)
        str_leg1 = "PRES_CONF = " + str("%E" % (PRES_CONF[i])) \
            + ", SIGM_DECH = " + str("%E" % (SIGM_DECH[i]))
        remplir_graphique(DicoEssai, Courbes_niv1, p_list[i], epsv_list[i],
                          str_leg1, "P-EPS_VOL")
        remplir_graphique(DicoEssai, Courbes_niv1, sigzz_list[i], epsv_list[i],
                          str_leg1, "SIG_AXI-EPS_VOL")

    # ---
    # Fin boucle sur les pressions de consolidation isotrope initiale PRES_CONF
    # ---
    # remplissage des tables
    Resu_Essai = {}
    Resu_Essai['INST'] = inst_list
    Resu_Essai['EPS_VOL'] = epsv_list
    Resu_Essai['SIG_AXI'] = sigzz_list
    Resu_Essai['SIG_LAT'] = sigxx_list
    Resu_Essai['P'] = p_list
    Resu_Essai['SIGM_IMPOSE'] = sigm_impose_list

    remplir_tables(self, "OEDO_C", str_n_essai, DicoEssai, Resu_Essai)

    # impression des graphiques (niveau 1)
    impr_graphique(self, DicoEssai, Courbes_niv1,
                   NomsFich_niv1, Leg_x_niv1, Leg_y_niv1, {}, {})


# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#             Essai De Consolidation Isotrope  Drainee Cyclique  (ISOT_C)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
def essai_ISOT_C(self, str_n_essai, DicoEssai, MATER, COMPORTEMENT, CONVERGENCE, INFO):
    """
    Essai De Consolidation Isotrope  Drainee Cyclique  (ISOT_C)
    """
    import numpy as NP
    import math as M
    from Accas import _F
    import aster
    from Utilitai.Utmess import UTMESS
    from Comportement import catalc

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    PRES_CONF = DicoEssai['PRES_CONF']
    SIGM_DECH = DicoEssai['SIGM_DECH']
    SIGM_IMPOSE = DicoEssai['SIGM_IMPOSE']
    KZERO = DicoEssai['KZERO']
    NB_INST = DicoEssai['NB_INST']

    # recuperation du nombre de VI associe a la LdC
    nom_lc = COMPORTEMENT.List_F()[0]['RELATION']
    num_lc, nb_vari = catalc.get_info(nom_lc)
    assert type(nb_vari) is int and nb_vari > 0

    # dict permettant la gestion des graphiques
    Courbes_niv1 = {}
    NomsFich_niv1 = {}
    Leg_x_niv1 = {}
    Leg_y_niv1 = {}

    # preparation des graphiques (niveau 1)
    str_fich_niv1 = "Essai_ISOT_C_" + str_n_essai + "_"
    preparer_graphique(
        '1', DicoEssai, str_fich_niv1, Courbes_niv1, NomsFich_niv1,
        Leg_x_niv1, Leg_y_niv1, {}, {})

    # ---
    # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/2 de cycle)
    # ---
    inst_list = [[] for k in xrange(len(PRES_CONF))]
    epsv_list = [[] for k in xrange(len(PRES_CONF))]
    p_list = [[] for k in xrange(len(PRES_CONF))]
    q_list = [[] for k in xrange(len(PRES_CONF))]
    sigm_impose_list = [[] for k in xrange(len(PRES_CONF))]

    # ---
    # Boucle sur les pressions de consolidation isotrope initiale PRES_CONF
    # ---
    for i in xrange(len(PRES_CONF)):

        long_cyc = 2 * 10.
        inst_ini = -1. * long_cyc
        pas_inst = long_cyc / (2. * NB_INST)

        # ---
        # Boucle sur les amplitudes de variation SIGM_IMPOSE: un cycle correspond a
        # une contrainte SIGM_IMPOSE
        # ---
        for j in xrange(len(SIGM_IMPOSE)):

            affiche_infos_essai(
                str_n_essai, "ISOT_C", PRES_CONF[i], SIGM_IMPOSE[j], SIGM_DECH[i])

            Ltmp_inst = []
            Ltmp_epv = []
            Ltmp_p = []
            Ltmp_q = []

            # ---
            # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/2 de cycle )
            # ---
            inst_ini += long_cyc
            L_inst = [
                inst_ini + k * pas_inst for k in xrange(2 * NB_INST + 1)]

            __RLIST = DEFI_LIST_REEL(VALE=L_inst, INFO=INFO)

            __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                                     ECHEC=_F(SUBD_METHODE='MANUEL',
                                              SUBD_PAS=10,
                                              SUBD_NIVEAU=10,),
                                     INFO=INFO,)

            # ---
            # Definition des chargements
            # ---

            if j == 0:

                __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                        VALE=(inst_ini, PRES_CONF[i],
                                              inst_ini + long_cyc /
                                              2., PRES_CONF[
                                              i] + SIGM_IMPOSE[j],
                                              inst_ini + long_cyc, SIGM_DECH[i],))

                __CHAR2 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                        VALE=(inst_ini, KZERO * PRES_CONF[i],
                                              inst_ini + long_cyc / 2., KZERO *
                                              PRES_CONF[
                                              i] + SIGM_IMPOSE[j],
                                              inst_ini + long_cyc, KZERO * SIGM_DECH[i],))

            else:
                __CHAR1 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                        VALE=(inst_ini, SIGM_DECH[i],
                                              inst_ini + long_cyc /
                                              2., SIGM_DECH[
                                              i] + SIGM_IMPOSE[j],
                                              inst_ini + long_cyc, SIGM_DECH[i],))

                __CHAR2 = DEFI_FONCTION(INFO=INFO, NOM_PARA='INST',
                                        VALE=(inst_ini, KZERO * SIGM_DECH[i],
                                              inst_ini + long_cyc / 2., KZERO *
                                              SIGM_DECH[
                                              i] + SIGM_IMPOSE[j],
                                              inst_ini + long_cyc, KZERO * SIGM_DECH[i],))

            # s'il s'agit du premier cycle, Cond_Init suivantes:
            if j == 0:
                Cond_Init = {}
                Cond_Init['VARI'] = [0.] * nb_vari
                Cond_Init['SIXX'] = KZERO * PRES_CONF[i]
                Cond_Init['SIYY'] = KZERO * PRES_CONF[i]
                Cond_Init['SIZZ'] = PRES_CONF[i]
                Cond_Init['EPXX'] = 0.
                Cond_Init['EPYY'] = 0.
                Cond_Init['EPZZ'] = 0.
                Cond_Init['EPXY'] = 0.
                Cond_Init['EPXZ'] = 0.
                Cond_Init['EPYZ'] = 0.

            # ---
            # Calcul
            # ---
            __EVOL = SIMU_POINT_MAT(
                INFO=INFO,
                COMPORTEMENT=COMPORTEMENT.List_F(),
                CONVERGENCE=CONVERGENCE.List_F(),
                MATER=MATER,
                SUPPORT='POINT',
                INCREMENT=_F(LIST_INST=__DLIST,
                             INST_INIT=inst_ini,
                             INST_FIN=inst_ini + long_cyc,),
                NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
                ARCHIVAGE=_F(LIST_INST=__RLIST,),
                VECT_IMPO=(_F(NUME_LIGNE=1, VALE=__CHAR2),
                           _F(NUME_LIGNE=2, VALE=__CHAR2),
                           _F(NUME_LIGNE=3, VALE=__CHAR1),),
                MATR_C1=(_F(NUME_LIGNE=1, NUME_COLONNE=1, VALE=1.),
                         _F(NUME_LIGNE=2, NUME_COLONNE=2, VALE=1.),
                         _F(NUME_LIGNE=3, NUME_COLONNE=3, VALE=1.),),
                MATR_C2=(_F(NUME_LIGNE=1, NUME_COLONNE=1, VALE=0.),
                         _F(NUME_LIGNE=2, NUME_COLONNE=2, VALE=0.),
                         _F(NUME_LIGNE=3, NUME_COLONNE=3, VALE=0.),),
                SIGM_INIT= _F(SIXX=Cond_Init['SIXX'],
                              SIYY=Cond_Init['SIYY'],
                              SIZZ=Cond_Init['SIZZ'],),
                EPSI_INIT= _F(EPXX=Cond_Init['EPXX'],
                              EPYY=Cond_Init['EPYY'],
                              EPZZ=Cond_Init['EPZZ'],
                              EPXY=Cond_Init['EPXY'],
                              EPXZ=Cond_Init['EPXZ'],
                              EPYZ=Cond_Init['EPYZ'],),
                VARI_INIT= _F(VALE=Cond_Init['VARI']),)

            TabRes = __EVOL.EXTR_TABLE().values()
            # PostTraiter les grandeurs d'interet...
            sig_xx = NP.array(TabRes['SIXX'])
            sig_yy = NP.array(TabRes['SIYY'])
            sig_zz = NP.array(TabRes['SIZZ'])
            eps_xx = NP.array(TabRes['EPXX'])
            eps_yy = NP.array(TabRes['EPYY'])
            eps_zz = NP.array(TabRes['EPZZ'])
            eps_xy = NP.array(TabRes['EPXY'])
            eps_xz = NP.array(TabRes['EPXZ'])
            eps_yz = NP.array(TabRes['EPYZ'])
            inst = TabRes['INST']
            p = (sig_xx + sig_yy + sig_zz) / 3.
            q = abs(sig_zz - sig_xx)
            eps_vol = eps_xx + eps_yy + eps_zz
            # .. et les stocker
            Ltmp_inst += inst
            Ltmp_epv += list(eps_vol)
            Ltmp_p += list(p)
            Ltmp_q += list(q)

            # Cond_Init : vi, eps, et sig au dernier inst pr CI cycle suivant
            Cond_Init = {'VARI': []}
            for i_vari in xrange(1, nb_vari + 1):
                VarTmp = TabRes["V" + str(i_vari)]
                Cond_Init['VARI'].append(VarTmp[-1])
            Cond_Init['SIXX'] = sig_xx[-1]
            Cond_Init['SIYY'] = sig_yy[-1]
            Cond_Init['SIZZ'] = sig_zz[-1]
            Cond_Init['EPXX'] = eps_xx[-1]
            Cond_Init['EPYY'] = eps_yy[-1]
            Cond_Init['EPZZ'] = eps_zz[-1]
            Cond_Init['EPXY'] = eps_xy[-1]
            Cond_Init['EPXZ'] = eps_xz[-1]
            Cond_Init['EPYZ'] = eps_yz[-1]

            DETRUIRE(
                CONCEPT=_F(NOM=(__RLIST, __DLIST, __CHAR1, __EVOL)), INFO=1)

            #
            # stockage pour ecriture dans les tables
            inst_list[i] += Ltmp_inst
            epsv_list[i] += Ltmp_epv
            p_list[i] += Ltmp_p
            q_list[i] += Ltmp_q
            sigm_impose_list[i] += [SIGM_IMPOSE[j]] * len(Ltmp_inst)

        # ---
        # Fin boucle sur les amplitudes de variation SIGM_IMPOSE
        # ---
        # remplissage des graphiques (niveau 1)
        str_leg1 = "PRES_CONF = " + str("%E" % (PRES_CONF[i])) \
            + ", SIGM_DECH = " + str("%E" % (SIGM_DECH[i]))
        remplir_graphique(DicoEssai, Courbes_niv1, p_list[i], epsv_list[i],
                          str_leg1, "P-EPS_VOL")

    # ---
    # Fin boucle sur les sur les pressions de consolidation isotrope initiale PRES_CONF
    # ---

    # remplissage des tables
    Resu_Essai = {}
    Resu_Essai['INST'] = inst_list
    Resu_Essai['EPS_VOL'] = epsv_list
    Resu_Essai['P'] = p_list
    Resu_Essai['Q'] = q_list
    Resu_Essai['SIGM_IMPOSE'] = sigm_impose_list

    remplir_tables(self, "ISOT_C", str_n_essai, DicoEssai, Resu_Essai)

    # impression des graphiques (niveau 1)
    impr_graphique(self, DicoEssai, Courbes_niv1,
                   NomsFich_niv1, Leg_x_niv1, Leg_y_niv1, {}, {})
