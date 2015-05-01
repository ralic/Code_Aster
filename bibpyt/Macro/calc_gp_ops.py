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
#
#
# CAS 2D: FORMULES PERMETTANT LA DEFINITION ET LE CALCUL DES COPEAUX DANS LE CAS SS_COPEAU
#           (Pour plus de renseignement, voir CR-AMA-12-272)
#


def SEUIL(X, Y, X0, Y0, R, lc, Nume_cop, ccos, ssin):
    f1 = 0
    f2 = 0
    f3 = 0
    # DY1,DY2,DX3,DX2
    if (-(X - X0) * ccos <= (Y - Y0) * ssin)\
        and((X - X0 - Nume_cop * lc * ccos) * ccos
            <= -ssin * (Y - Y0 - Nume_cop * lc * ssin))\
        and((Y - Y0 + R * ccos) * ccos >= (X - X0 - R * ssin) * ssin)\
            and((Y - Y0 - R * ccos) * ccos <= (X - X0 + R * ssin) * ssin):
        f1 = 1
    # C2,DY2
    if ((X - X0 - Nume_cop * lc * ccos) ** 2 + (Y - Y0 - Nume_cop * lc * ssin) ** 2
       <= R ** 2)\
       and ((X - X0 - Nume_cop * lc * ccos) * ccos
            >= -ssin * (Y - Y0 - Nume_cop * lc * ssin)):
        f2 = 1
    # C1,DY1
    if ((X - X0) ** 2 + (Y - Y0) ** 2 <= R ** 2) and (-(X - X0) * ccos <= (Y - Y0) * ssin):
        f3 = 1
    f = f1 + f2 - f3
    return f

#--


def NRJ(ENEL_ELGA, X, Y, X0, Y0, R, lc, Nume_cop, ccos, ssin):
    nr = ENEL_ELGA * SEUIL(X, Y, X0, Y0, R, lc, Nume_cop, ccos, ssin)
    return nr

#
# CAS 3D: METHODES PERMETTANT LE CALCUL DE LA SURFACE DES MAILLES DU PLAN DE SYMETRIE
#


def CalDist(coor_nd, coor_nds):
    # on calcule la distance du noeud 1 aux autres noeuds
    import numpy as NP

    dist = NP.sqrt((coor_nds[0] - coor_nd[0]) ** 2 +
                   (coor_nds[1] - coor_nd[1]) ** 2 +
                   (coor_nds[2] - coor_nd[2]) ** 2)
    return dist

#--


def Recup_Noeuds_Copeaux(maya, Copeau_k):

    # Recuperation des noeuds appartenant a la surface de symetrie
    # et aux copeaux
    from Accas import _F
    dicno = []
    dicno.append(_F(NOM=Copeau_k))
    dicno.append(_F(NOM='Cop_Pl'))
    DEFI_GROUP(reuse=maya, MAILLAGE=maya, DETR_GROUP_NO=dicno)

    dicno = []
    dicno.append(_F(NOM=Copeau_k, GROUP_MA=Copeau_k))
    dicno.append(_F(NOM='Cop_Pl', INTERSEC=(Copeau_k, 'Nds_Plan',)))
    DEFI_GROUP(reuse=maya, MAILLAGE=maya, CREA_GROUP_NO=dicno)

#--


def Crea_grp_ma(maya, C_k):
    from Accas import _F
    if C_k == 0:
        dicma = {'NOM': 'Mai_Plan'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),

        dicma = {'NOM': 'Mai_Plan', 'OPTION': 'APPUI', 'GROUP_NO':
                 'Cop_Pl', 'TYPE_APPUI': 'TOUT', 'TYPE_MAILLE': '2D'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),

        dicma = {'NOM': 'Mai_Pla2'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),

        dicma = {'NOM': 'Mai_Pla2', 'GROUP_MA': 'Mai_Plan'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),

    else:
        dicma = {'NOM': 'Mai_Pla1'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),

        dicma = {'NOM': 'Mai_Pla1', 'OPTION': 'APPUI',
                 'GROUP_NO': 'Cop_Pl', 'TYPE_APPUI': 'TOUT'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),

        dicma = {'NOM': 'Mai_Plan'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),

        dicma = {'NOM': 'Mai_Plan', 'DIFFE':
                 ('Mai_Pla1', 'Mai_Pla2'), 'TYPE_MAILLE': '2D'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),

        dicma = {'NOM': 'Mai_Pla2'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),

        dicma = {'NOM': 'Mai_Pla2', 'GROUP_MA': 'Mai_Pla1'}
        DEFI_GROUP(reuse=maya, MAILLAGE=maya, CREA_GROUP_MA=dicma)

#--


def Calcul_mesure_3D(maya, nbcop, l_copo_tot, ltyma, nd_fiss, normale):
    # Calcul de la mesure des mailles appartenant au plan de symetrie
    # On est en petites deformations alors on ne tient pas compte de la deformee
    # lors du calcul de la surface

    import numpy as NP
    from Accas import _F
    from Utilitai.Utmess import UTMESS

    mesure = [0] * len(l_copo_tot)

    COOR = maya.sdj.COORDO.VALE.get()

    # Recuperation des noeuds appartenant a la surface de symetrie
    DEFI_GROUP(reuse=maya, MAILLAGE=maya, DETR_GROUP_NO={'NOM': 'Nds_Plan'})
    dicno = {'NOM': 'Nds_Plan', 'OPTION': 'PLAN', 'VECT_NORMALE':
             normale, 'PRECISION': 1e-6, 'NOEUD_CENTRE': nd_fiss}
    DEFI_GROUP(reuse=maya, MAILLAGE=maya, CREA_GROUP_NO=dicno)

    # boucle sur l'ensemble des copeaux
    for C_k, Copeau_k in enumerate(l_copo_tot):

        # Recuperation des groupes de noeuds appartenant au copeau courant
        Recup_Noeuds_Copeaux(maya, Copeau_k)
        Crea_grp_ma(maya, C_k % nbcop)

        # La mesure de la surface est cumulee
        if C_k % nbcop != 0:
            mesure[C_k] = mesure[C_k - 1]

        # Recuperation des coordonnees des noeuds appartenant au copeau courant
        maille_courante = maya.sdj.GROUPEMA.get()['Mai_Plan'.ljust(24)][0]
        if ltyma[maya.sdj.TYPMAIL.get()[maille_courante]][0:4] == 'QUAD':
            connexe = maya.sdj.CONNEX.get()[maille_courante]
            if C_k % nbcop != 0:
                if len(set(connexe[:4] + connexe_C_k_moins1)) != 6:
                    UTMESS('F', 'RUPTURE1_27')
            connexe_C_k_moins1 = connexe[:4]
        else:
            UTMESS('F', 'RUPTURE1_22')

        # Calcul de la surface de la maille 2D du copeau courant appartenant au
        # plan de symetrie
        Coord_nds = [(COOR[(connexe[x] - 1) * 3], COOR[(connexe[x] - 1) * 3 + 1], COOR[(connexe[x] - 1) * 3 + 2])
                     for x in range(4)]

        dAB = CalDist(Coord_nds[1], Coord_nds[0])
        dBC = CalDist(Coord_nds[2], Coord_nds[1])
        dCD = CalDist(Coord_nds[3], Coord_nds[2])
        dDA = CalDist(Coord_nds[0], Coord_nds[3])

        if abs(dAB - dCD) / dAB > 0.2 or abs(dDA - dBC) / dDA > 0.2:
            UTMESS('A', 'RUPTURE1_29')

        mesure[C_k] = mesure[C_k] + (dAB + dCD) / 2. * (dBC + dDA) / 2.

    # Destruction des groupes de noeuds et de mailles temporaires
    dicno = [_F(NOM='Cop_Pl'), _F(NOM='Nds_Plan')]
    dicma = [_F(NOM='Mai_Plan'), _F(NOM='Mai_Pla1'), _F(NOM='Mai_Pla2')]

    DEFI_GROUP(reuse=maya, MAILLAGE=maya,
               DETR_GROUP_NO=dicno, DETR_GROUP_MA=dicma)

    return mesure


#
# DEBUT DE LA MACRO PROPREMENT DITE
#
def calc_gp_ops(self, **args):
    """Corps de CALC_GP"""
    from numpy import *
    import aster
    from Accas import _F
    from Utilitai.Utmess import UTMESS, MasquerAlarme, RetablirAlarme
    MasquerAlarme('CALCCHAMP_1')

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    # On importe les definitions des commandes a utiliser dans la macro
    global DEFI_GROUP
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    POST_ELEM = self.get_cmd('POST_ELEM')
    FORMULE = self.get_cmd('FORMULE')
    CALC_TABLE = self.get_cmd('CALC_TABLE')
    CALC_CHAMP = self.get_cmd('CALC_CHAMP')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
    DEFI_GROUP = self.get_cmd('DEFI_GROUP')
#

#
# RECUPERATION DU MODELE, DU MAILLAGE ET DU MATERIAU A PARTIR DU RESULTAT
#

    __RESU = self['RESULTAT']

#  modele
    iret, ibid, n_modele = aster.dismoi('MODELE', __RESU.nom, 'RESULTAT', 'F')
    n_modele = n_modele.rstrip()
    if len(n_modele) == 0 or n_modele == "#PLUSIEURS":
        UTMESS('F', 'RUPTURE1_58')
    __model = self.get_concept(n_modele)
    # Dimension du modele
    iret, ndim, rbid = aster.dismoi('DIM_GEOM', __model.nom, 'MODELE', 'F')
#
#  maillage
    iret, ibid, nom_ma = aster.dismoi(
        'NOM_MAILLA', __RESU.nom, 'RESULTAT', 'F')
    __maillage = self.get_concept(nom_ma.strip())
#
#  champ materiau (inutile mais permet d eviter des alarmes)
    iret, ibid, nom_cham_mater = aster.dismoi(
        'CHAM_MATER', __RESU.nom, 'RESULTAT', 'F')
    __cham_mater = self.get_concept(nom_cham_mater.strip())

#
# RECUPERATION DES DONNEES DE SYMETRIE ET DU FOND DE FISSURE
#

# mult=Coefficient multiplicatif suivant la symetrie du probleme
    mult = 1.
    if self['TRANCHE_2D'] != None:
        TRANCHE_2D = self['TRANCHE_2D']
        if ndim != 2:
            UTMESS('F', 'RUPTURE1_19', ['TRANCHE_2D', '2D'])
#    symetrie
        if self['SYME'] == 'OUI':
            mult = 2.
    else:
        TRANCHE_3D = self['TRANCHE_3D']
        if ndim != 3:
            UTMESS('F', 'RUPTURE1_19', ['TRANCHE_3D', '3D'])

#    liste des noeuds du fond de fissure
        l_noeuds_fissure = self['FOND_FISS'].sdj.FOND_NOEU.get()
        if l_noeuds_fissure == None:
# Cas double fond de fissure : par convention les noeuds sont ceux de
# fond_inf
            l_noeuds_fissure = self['FOND_FISS'].sdj.FONDINF_NOEU.get()

#    normale au plan de la fissure
        lnormale = self['FOND_FISS'].sdj.NORMALE.get()
        if (lnormale == None):
            UTMESS('F', 'POST0_39')

#    symetrie
        iret, ibid, syme = aster.dismoi(
            'SYME', self['FOND_FISS'].nom, 'FOND_FISS', 'F')
        if syme == 'OUI':
            mult = 2

#
# VERIFICATION DE LA LISTE DES INSTANTS
#

# Verification que les instants demandes sont bien dans le resultat
# Construction des instants de calcul par la meme occasion
    list_inst = __RESU.LIST_VARI_ACCES()['INST']
    l_inst_final = []

    for inst in self['LIST_INST'].Valeurs():
        if self['CRITERE'] == 'ABSOLU':
            prec = self['PRECISION']
        elif self['CRITERE'] == 'RELATIF':
            prec = self['PRECISION'] * inst

        match = [x for x in list_inst if (
            (x + prec >= inst) and (x - prec <= inst))]
        if len(match) == 0:
            UTMESS('F', 'RUPTURE0_38', valr=inst)
        if len(match) >= 2:
            UTMESS('F', 'RUPTURE0_39', valr=inst)

        l_inst_final.append(match[0])

    nb_inst = len(l_inst_final)
    __linstr8 = DEFI_LIST_REEL(VALE=l_inst_final)

#
# PREPARATION DES SORTIES SI GPMAX
#

# Definition du concept sortant systematique dans le contexte de la macro
# L'eventuel champ de copeaux est cree plus tard si besoin
    self.DeclareOut('tabout', self.sd)

# Definition de la sortie facultative GP_MAX
    GPMAX = self['GPMAX']
    if GPMAX != None:
        self.DeclareOut('tabgpmax', GPMAX)
# Creation des colonnes de la table de sortie gpmax
        tabinstmax = []
        tabcopmax = []
        tabenelmax = []
        tablcopmax = []
        tabgpmax = []

#

#
# CALCUL DES GP
#

#
#                      1/ CAS 2D
#

    if ndim == 2:

#
# 1.1/ CAS OU L UTILISATEUR A DEFINI DES GROUPES DE MAILLE COPEAU
#      IL SUFFIT ALORS DE CALCULER L ENERGIE DANS CES GROUPES ET D EN DEDUIRE LE GP
#
        if TRANCHE_2D['ZONE_MAIL'] == 'OUI':
            lgroupma = TRANCHE_2D['GROUP_MA']
            lcopeau = TRANCHE_2D['TAILLE'].Valeurs()
            if len(lgroupma) != len(lcopeau):
                UTMESS('F', 'RUPTURE1_21')
            nbcop = len(lcopeau)

            tabmax = [0] * nbcop * nb_inst
            tabcop = lgroupma * nb_inst
            tablcop = lcopeau * nb_inst

            __enertemp = POST_ELEM(MODELE=__model,
                                   RESULTAT=__RESU,
                                   LIST_INST=__linstr8,
                                   ENER_ELAS=_F(GROUP_MA=lgroupma)
                                   )
            enerel = __enertemp.EXTR_TABLE()
            tabenel = [mult * x for x in enerel.TOTALE.values()]
            tabgp = [tabenel[x] / tablcop[x] for x in range(len(tabenel))]
            tabinst = enerel.INST.values()

            for i in range(nb_inst):
                maxinst = max(tabgp[i * nbcop:(i + 1) * nbcop])
                index1 = tabgp[i * nbcop:(i + 1) * nbcop].index(maxinst)
                index = index1 + i * nbcop
                tabmax[index] = 1
                if GPMAX != None:
                    tabinstmax.append(tabinst[index])
                    tabcopmax.append(tabcop[index])
                    tabenelmax.append(tabenel[index])
                    tablcopmax.append(tablcop[index])
                    tabgpmax.append(tabgp[index])

#
# 1.2/ CAS OU L UTILISATEUR N A PAS DEFINI DES GROUPES DE MAILLE COPEAU
#      IL FAUT CREER UN DES COPEAUX PAR ENSEMBLE DE POINTS DE GAUSS ET JOUER AVEC
#
        elif TRANCHE_2D['ZONE_MAIL'] == 'NON':
            nbcop = TRANCHE_2D['NB_ZONE']
            theta = TRANCHE_2D['ANGLE']
            taille = TRANCHE_2D['TAILLE']
# Manipulation obligatoire pour pouvoir se servir des grandeurs dans les
# formules
            self.update_const_context({'origine': TRANCHE_2D['CENTRE']})
            self.update_const_context({'rayon': TRANCHE_2D['RAYON']})
            self.update_const_context({'taille': taille})
            self.update_const_context({'theta': theta})
            self.update_const_context({'NRJ': NRJ})
            nom_cmp = ['X%d' % k for k in range(1, nbcop + 1)]
            nom_cop = ['COPS_%d' % k for k in range(1, nbcop + 1)]

# champ de geometrie et de points de gauss (coordonnees des points de gauss)
            __CHXN = CREA_CHAMP(OPERATION='EXTR',
                                TYPE_CHAM='NOEU_GEOM_R',
                                NOM_CHAM='GEOMETRIE',
                                MAILLAGE=__maillage)

            __CHXG = CREA_CHAMP(OPERATION='DISC',
                                TYPE_CHAM='ELGA_GEOM_R',
                                MODELE=__model,
                                CHAM_GD=__CHXN)

# construction du champ copeau pour visualisation par utilisateur s'il le
# souhaite
            if TRANCHE_2D['CHAMP_VISU'] != 0:
                self.DeclareOut('chp_cop', TRANCHE_2D['CHAMP_VISU'])

                self.update_const_context({'SEUIL': SEUIL})
                self.update_const_context({'ccos': cos(theta * pi / 180.)})
                self.update_const_context({'ssin': sin(theta * pi / 180.)})

                __seuil = [None for i in range(nbcop)]
                for cop in range(nbcop):
                    __seuil[cop] = FORMULE(
                        VALE='''SEUIL(X,Y,origine[0],origine[1],rayon,taille,%d,ccos,ssin)''' % (
                            cop + 1),
                        NOM_PARA=('X', 'Y'),)

                __formule_seuil = CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_F',
                                             MODELE=__model,
                                             OPERATION='AFFE',
                                             PROL_ZERO='OUI',
                                             AFFE=_F(TOUT='OUI',
                                                     NOM_CMP=nom_cmp,
                                                     VALE_F=__seuil,),)

                chp_cop = CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_R',
                                     OPERATION='EVAL',
                                     CHAM_F=__formule_seuil,
                                     CHAM_PARA=(__CHXG),)

# calcul des energies et du gp
            __ener = [None for cop in range(nbcop)]
            for cop in range(nbcop):
                __ener[cop] = FORMULE(
                    VALE='''NRJ(TOTALE,X,Y,origine[0],origine[1],rayon,taille,%d,ccos,ssin)''' % (
                        cop + 1),
                    NOM_PARA=('TOTALE', 'X', 'Y'),)

            __formule_ener = CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_F',
                                        MODELE=__model,
                                        OPERATION='AFFE',
                                        PROL_ZERO='OUI',
                                        AFFE=_F(TOUT='OUI',
                                                NOM_CMP=nom_cmp,
                                                VALE_F=__ener,),)

            __RESU = CALC_CHAMP(reuse=__RESU,
                                RESULTAT=__RESU,
                                LIST_INST=__linstr8,
                                ENERGIE=('ENEL_ELGA'),)

            tabmax = [0] * nbcop * nb_inst
            tabcop = nom_cop * nb_inst
            tablcop = [taille * (cop + 1) for cop in range(nbcop)] * nb_inst
            tabinst = []
            tabenel = []
            tabgp = []

            for i, inst in enumerate(l_inst_final):

                __energa = CREA_CHAMP(OPERATION='EXTR',
                                      TYPE_CHAM='ELGA_ENER_R',
                                      NOM_CHAM='ENEL_ELGA',
                                      RESULTAT=__RESU,
                                      INST=inst)

                __resinter = CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_R',
                                        OPERATION='EVAL',
                                        CHAM_F=__formule_ener,
                                        CHAM_PARA=(__energa, __CHXG),)

                __tabnrj = POST_ELEM(CHAM_GD=__resinter,
                                     MODELE=__model,
                                     CHAM_MATER=__cham_mater,
                                     INTEGRALE=_F(TOUT='OUI',
                                                  NOM_CHAM='ENEL_ELGA',
                                                  NOM_CMP=nom_cmp,
                                                  DEJA_INTEGRE='NON',
                                                  TYPE_MAILLE='2D'),)

                tabenerel = __tabnrj.EXTR_TABLE().values()

                tabinst = tabinst + [inst] * nbcop

                enerel = [mult * tabenerel['INTE_X%d' % (cop + 1)][0]
                          for cop in range(nbcop)]
                tabenel += enerel
                gp = [enerel[cop] / (taille * (cop + 1))
                      for cop in range(nbcop)]
                tabgp += gp

                maxinst = max(tabgp[i * nbcop:(i + 1) * nbcop])
                index1 = tabgp[i * nbcop:(i + 1) * nbcop].index(maxinst)
                index = index1 + i * nbcop
                tabmax[index] = 1
                if GPMAX != None:
                    tabinstmax.append(tabinst[index])
                    tabcopmax.append(tabcop[index])
                    tabenelmax.append(tabenel[index])
                    tablcopmax.append(tablcop[index])
                    tabgpmax.append(tabgp[index])

#
#                      2/ CAS 3D
#
    elif ndim == 3:

#    type des mailles
        ltyma = aster.getvectjev("&CATA.TM.NOMTM")

#    liste des copeaux
        l_copo_tot = []
        for tmpocc in TRANCHE_3D:
            dMCT = tmpocc.cree_dict_valeurs(tmpocc.mc_liste)
            l_copo_tot += dMCT['GROUP_MA']

        # le nombre de copeaux est suppose identique sur toutes les tranches
        nbcoptot = len(l_copo_tot)
        nbcop = nbcoptot / len(TRANCHE_3D)

# calcul de la surface des mailles appartenant au plan de symetrie de
# l'entaille
        mesure = Calcul_mesure_3D(
            __maillage, nbcop, l_copo_tot, ltyma, l_noeuds_fissure[0], lnormale)

# calcul des energies et du gp
        __enertemp = POST_ELEM(MODELE=__model,
                               RESULTAT=__RESU,
                               LIST_INST=__linstr8,
                               ENER_ELAS=_F(GROUP_MA=l_copo_tot),
                               TITRE='Energie elastique',)

        enerel = __enertemp.EXTR_TABLE()
        tabcop = enerel.LIEU.values()
        tabenel = [mult * x for x in enerel.TOTALE.values()]
        tabinst = enerel.INST.values()
        tablcop = mesure * nb_inst
        tabgp = [tabenel[x] / tablcop[x] for x in range(len(tabenel))]

        tabmax = [0] * nbcoptot * nb_inst
        for i in range(nb_inst):
            maxinst = max(tabgp[nbcoptot * i:nbcoptot * (i + 1)])
            index1 = tabgp[nbcoptot * i:nbcoptot * (i + 1)].index(maxinst)
            index = index1 + i * nbcoptot
            tabmax[index] = 1
            if GPMAX != None:
                tabinstmax.append(tabinst[index])
                tabcopmax.append(tabcop[index])
                tabenelmax.append(tabenel[index])
                tablcopmax.append(tablcop[index])
                tabgpmax.append(tabgp[index])

#

#
# CREATION DE LA TABLE DE SORTIE
#

    tabout = CREA_TABLE(LISTE=(
                        _F(PARA='INST',      LISTE_R=tabinst),
                        _F(PARA='ZONE',      LISTE_K=tabcop,),
                        _F(PARA='ENER ELAS', LISTE_R=tabenel,),
                        _F(PARA='DELTA L',   LISTE_R=tablcop,),
                        _F(PARA='GP',        LISTE_R=tabgp,),
                        _F(PARA='MAX_INST',  LISTE_I=tabmax,),
                        ),)
    if GPMAX != None:
        tabgpmax = CREA_TABLE(LISTE=(
            _F(PARA='INST',      LISTE_R=tabinstmax),
            _F(PARA='ZONE',      LISTE_K=tabcopmax,),
            _F(PARA='ENER ELAS', LISTE_R=tabenelmax,),
            _F(PARA='DELTA L',   LISTE_R=tablcopmax,),
            _F(PARA='GP',        LISTE_R=tabgpmax,),
        ),)
    RetablirAlarme('CALCCHAMP_1')
    return ier
