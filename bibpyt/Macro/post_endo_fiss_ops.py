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
# person_in_charge: marina.bottoni at edf.fr
# ---------------------------------------------------------------------------
#                  POST_ENDO_FISS
# PROCEDURE PYTHON DU RECHERCHE DU TRAJET DE FISSURATION
#   ET D'EXTRACTION DE L'OUVERTURE DE FISSURE D'UN CHAMP D'ENDOMMAGEMENT

import numpy as NP
import os
from post_endo_fiss_utils import unitVector, crea_mail_lin, crea_sd_mail, euler_angles
from post_endo_fiss_utils import conv_smoothing_arc, conv_smoothing1D
from Utilitai.UniteAster import UniteAster
from post_endo_fiss_utils import ThresholdTooHighError, NoMaximaError


def cherche_trajet(self, NOM_CMP, NOM_CHAM, dRECHERCHE, __ENDO, __mail, typeChampTrajet, infoPlan, inst):

    from math import radians
    from Utilitai.Utmess import UTMESS
    from Accas import _F
    import aster
    from Macro.macr_lign_coupe_ops import crea_mail_lig_coup

    # --------------------------------------------------
    # IMPORT OF ASTER COMMANDS
    #
    AFFE_MODELE = self.get_cmd('AFFE_MODELE')
    PROJ_CHAMP = self.get_cmd('PROJ_CHAMP')
    CREA_RESU = self.get_cmd('CREA_RESU')
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')

    # --------------------------------------------------
    # MESH AND MESH PROPERTIES
    #
    coorIni1 = infoPlan[0]
    coorIni2 = infoPlan[1]
    dnor = infoPlan[2]
    dplan1 = infoPlan[3]
    dplan2 = infoPlan[4]

    dime = __mail.sdj.DIME.get()[5]

    # ---------------------------------
    # SEARCH PARAMETERS
    #
    lort = dRECHERCHE['LONG_ORTH']
    nbPoints = dRECHERCHE['NB_POINT']
    lreg = dRECHERCHE['LONG_REG']
    seuil = dRECHERCHE['BORNE_MIN']
    alpha = dRECHERCHE['ANGL_MAX']
    pas = dRECHERCHE['PAS']
    pas0 = lreg
    if pas0 < dRECHERCHE['PAS']:
        pas0 = dRECHERCHE['PAS']

    # --------------------------------------------------------------
    # RESTRICTION OF THE FIELD ON THE SELECTED ELEMENT GROUP
    #  ("__ENDOGM" = ENDO Groupe de Mailles)
    #   AND CONSTRUCTION OF THE WORKING "RESULT" CONCEPT
    #
    motclefs1 = {}
    motclefs1['MAILLAGE'] = __mail

    if 'GROUP_MA' in dRECHERCHE.keys():
        groupma = dRECHERCHE['GROUP_MA']
        __ENDOGM = CREA_CHAMP(OPERATION='ASSE',
                              TYPE_CHAM=typeChampTrajet,
                              ASSE=_F(CHAM_GD=__ENDO,
                                      GROUP_MA=groupma,
                                      NOM_CMP=NOM_CMP,
                                      ),
                              **motclefs1)
    else:
        __ENDOGM = __ENDO

    __resu = CREA_RESU(OPERATION='AFFE',
                       TYPE_RESU='EVOL_NOLI',
                       NOM_CHAM=NOM_CHAM,
                       AFFE=(_F(CHAM_GD=__ENDO,
                                INST=inst),),)

    # --------------------------------------------------
    # BOOT OF THE CRACK SEARCH PROCEDURE
    #
    methodeProj = 'COLLOCATION'
    Coortot = __mail.sdj.COORDO.VALE.get()
    Xtot = Coortot[coorIni1:len(Coortot):3]
    Ytot = Coortot[coorIni2:len(Coortot):3]
    Endono = __ENDOGM.EXTR_COMP(NOM_CMP, [], 1)
    Endo = Endono.valeurs
    Noeybar = Endono.noeud
    idxNoeud = NP.array(Noeybar) - 1
    Coorx = NP.take(Xtot, idxNoeud)
    Coory = NP.take(Ytot, idxNoeud)
    coorIni3 = int(NP.nonzero(dnor)[0])
    NormTot = Coortot[coorIni3:len(Coortot):3]
    CoorNor = NP.take(NormTot, idxNoeud)
    zCoupe = CoorNor[
        0]  # On dit que dans la coupe tous les valeurs sont constants
    idxmax = NP.argmax(Endo)
    xmax = Coorx[idxmax]
    ymax = Coory[idxmax]
    endomax = Endo[idxmax]

    Endono2 = __ENDO.EXTR_COMP(NOM_CMP, [], 1)
    Endo2 = Endono2.valeurs

    # First crack path point
    PtMax = xmax * dplan1 + ymax * dplan2 + zCoupe * dnor
    CoxAmo = NP.array([PtMax[0]])
    CoyAmo = NP.array([PtMax[1]])
    CozAmo = NP.array([PtMax[2]])
    EndoAmo = NP.array([endomax], float)

    # Creation of a circle and projection of the field on it
    lignes = []
    groups = []
    arcs = []
    pt2 = (xmax + pas) * dplan1 + ymax * dplan2 + zCoupe * dnor
    pt3 = (xmax - pas) * dplan1 + ymax * dplan2 + zCoupe * dnor
    arcs.append((pt2, PtMax, nbPoints, 180., dnor))
    arcs.append((pt3, PtMax, nbPoints, 180., dnor))

    resu_mail0, arcgma0, angles0, nbno0 = crea_mail_lig_coup(
        dime, lignes, groups, arcs)
    __MAI = crea_sd_mail(self, os.linesep.join(resu_mail0))

    motclefs2 = {}
    motclefs2['MAILLAGE_1'] = __mail
    motclefs2['MAILLAGE_2'] = __MAI
    nbPrec = NP.finfo(NP.float).precision
    distMax = 10. ** (-nbPrec + 2)

    __YBARPR = PROJ_CHAMP(METHODE=methodeProj,
                          RESULTAT=__resu,
                          DISTANCE_MAX=distMax,
                          TYPE_CHAM='NOEU',
                          NOM_CHAM=NOM_CHAM,
                          NUME_ORDRE=1,
                          **motclefs2)

    __YBARCH = CREA_CHAMP(TYPE_CHAM=typeChampTrajet,
                          OPERATION='EXTR',
                          NOM_CHAM=NOM_CHAM,
                          RESULTAT=__YBARPR,
                          NUME_ORDRE=1, )

    dx0 = __YBARCH.EXTR_COMP(NOM_CMP, [], 1)

    # "NonVide" : list of the circle nodes
    #   with associated values, i.e. inside the material
    # "idxpred" : connections between the two half-circles
    NonVide = NP.array(list(dx0.noeud))
    idxpred1 = NP.where(NonVide == 2 * nbPoints - 1)[0]
    idxpred2 = NP.where(NonVide == nbPoints)[0]

    EndoOrth = dx0.valeurs
    Coor0 = __MAI.sdj.COORDO.VALE.get()
    CoorxOrth = NP.array(Coor0[coorIni1:len(Coor0):3], float)
    CooryOrth = NP.array(Coor0[coorIni2:len(Coor0):3], float)

    # We eliminate nodes without field values
    CoorxOrth = NP.take(CoorxOrth, NonVide - 1)
    CooryOrth = NP.take(CooryOrth, NonVide - 1)
    CoorxOrth = NP.delete(CoorxOrth, idxpred1)
    CoorxOrth = NP.delete(CoorxOrth, idxpred2)
    CooryOrth = NP.delete(CooryOrth, idxpred1)
    CooryOrth = NP.delete(CooryOrth, idxpred2)
    EndoOrth = NP.delete(EndoOrth, idxpred1)
    EndoOrth = NP.delete(EndoOrth, idxpred2)

    # Smoothing of the field on the circle
    # EndoReg = conv_smoothing_arc_old(pas,CoorxOrth, CooryOrth,EndoOrth)
    EndoReg = conv_smoothing_arc(lreg, CoorxOrth, CooryOrth, EndoOrth)

    # Second crack path point
    idxmax = NP.argmax(EndoReg)
    endomax = EndoOrth[idxmax]
    cox = CoorxOrth[idxmax]
    coy = CooryOrth[idxmax]
    PtMax = cox * dplan1 + coy * dplan2 + zCoupe * dnor
    CoxAmo = NP.append(CoxAmo, PtMax[0])
    CoyAmo = NP.append(CoyAmo, PtMax[1])
    CozAmo = NP.append(CozAmo, PtMax[2])
    EndoAmo = NP.append(CozAmo, endomax)

    # Creation of a search vector
    CoxLast = NP.array([CoxAmo[1], CoxAmo[0]])
    CoyLast = NP.array([CoyAmo[1], CoyAmo[0]])
    CozLast = NP.array([CozAmo[1], CozAmo[0]])

    VersAvan = NP.array(
        [CoxLast[1] - CoxLast[0], CoyLast[1] - CoyLast[0], CozLast[1] - CozLast[0]])
    VersAvan = unitVector(VersAvan)

    PStart = NP.array([CoxLast[0], CoyLast[0], CozLast[0]])
    Ppred = PStart + pas * VersAvan
    VersNorm = unitVector(NP.cross(VersAvan, dnor))
    PPlus = Ppred + (lort / 2.) * VersNorm
    PMoin = Ppred - (lort / 2.) * VersNorm

    # Creation of an orthogonal profile and projection on it
    lignes = []
    groups = []
    arcs = []
    lignes = []
    lignes.append((PMoin.tolist(), Ppred.tolist(), nbPoints))
    lignes.append((Ppred.tolist(), PPlus.tolist(), nbPoints))

    resu_mail0, arcgma0, angles0, nbno0 = crea_mail_lig_coup(
        dime, lignes, groups, arcs)
    __MAI = crea_sd_mail(self, os.linesep.join(resu_mail0))

    motclefs2 = {}
    motclefs2['MAILLAGE_1'] = __mail
    motclefs2['MAILLAGE_2'] = __MAI

    __YBARPR = PROJ_CHAMP(METHODE='COLLOCATION',
                          RESULTAT=__resu,
                          DISTANCE_MAX=distMax,
                          TYPE_CHAM='NOEU',
                          NOM_CHAM=NOM_CHAM,
                          NUME_ORDRE=1,
                          **motclefs2)

    __YBARCH = CREA_CHAMP(TYPE_CHAM=typeChampTrajet,
                          OPERATION='EXTR',
                          NOM_CHAM=NOM_CHAM,
                          RESULTAT=__YBARPR,
                          NUME_ORDRE=1,)

    dx0 = __YBARCH.EXTR_COMP(NOM_CMP, [], 1)

    #  Search for the prediction point among the projected points
    #   and elimination of doubled point in the middle
    NonVide = NP.array(list(dx0.noeud))
    idxpred = NP.where(NonVide == nbPoints)[0]
    EndoOrth = dx0.valeurs

    Coor0 = __MAI.sdj.COORDO.VALE.get()
    CoorxOrth = NP.array(Coor0[coorIni1:len(Coor0):3], float)
    CooryOrth = NP.array(Coor0[coorIni2:len(Coor0):3], float)
    CoorxOrth = NP.take(CoorxOrth, NonVide - 1)
    CooryOrth = NP.take(CooryOrth, NonVide - 1)
    CoorxOrth = NP.delete(CoorxOrth, idxpred)
    CooryOrth = NP.delete(CooryOrth, idxpred)
    EndoOrth = NP.delete(EndoOrth, idxpred)
    EndoReg = conv_smoothing1D(lreg, CoorxOrth, CooryOrth, EndoOrth)

    # Refined coordinates of the first crack path point
    idxmax = NP.argmax(EndoReg)
    endomax = EndoOrth[idxmax]
    cox = CoorxOrth[idxmax]
    coy = CooryOrth[idxmax]
    PtMax = cox * dplan1 + coy * dplan2 + zCoupe * dnor
    CoxAmo[0] = PtMax[0]
    CoyAmo[0] = PtMax[1]
    CozAmo[0] = PtMax[2]
    EndoAmo[0] = endomax

    # --------------------------------------------------
    #  BEGIN OF THE CRACK PATH SEARCH
    #
    # Definition of two search directions
    VersAvn1 = NP.array(
        [CoxAmo[1] - CoxAmo[0], CoyAmo[1] - CoyAmo[0], CozAmo[1] - CozAmo[0]])
    VersAvn1 = unitVector(VersAvn1)
    VersAvn2 = -VersAvn1

    # Initialisation of the ridge vectors
    CoxCrete1 = NP.array([CoxAmo[1]])
    CoyCrete1 = NP.array([CoyAmo[1]])
    CozCrete1 = NP.array([CozAmo[1]])
    EndoCrete1 = NP.array([EndoAmo[1]])

    CoxCrete = NP.array([CoxAmo[0]])
    CoyCrete = NP.array([CoyAmo[0]])
    CozCrete = NP.array([CozAmo[0]])
    EndoCrete = NP.array([EndoAmo[0]])

    # ----
    # Loop on the crack path points
    #     Loop variables :
    #        dirRech   --> search direction index (1 or 2)
    #        condSort  --> loop exit condition
    #
    dirRech = 1
    i = 0
    condSort = seuil * 1.1
    while (condSort > seuil or dirRech == 1):
        i = i + 1
        # Definition of ths search vector
        if i == 1:
            if dirRech == 1:
                VersAvan = VersAvn1
            else:
                VersAvan = VersAvn2
        else:
            if dirRech == 1:
                CoxLast = NP.array([CoxCrete1[-2], CoxCrete1[-1]])
                CoyLast = NP.array([CoyCrete1[-2], CoyCrete1[-1]])
                CozLast = NP.array([CozCrete1[-2], CozCrete1[-1]])
            else:
                CoxLast = NP.array([CoxCrete[-2], CoxCrete[-1]])
                CoyLast = NP.array([CoyCrete[-2], CoyCrete[-1]])
                CozLast = NP.array([CozCrete[-2], CozCrete[-1]])
            VersAvan = NP.array(
                [CoxLast[1] - CoxLast[0], CoyLast[1] - CoyLast[0], CozLast[1] - CozLast[0]])
            VersAvan = unitVector(VersAvan)

        if dirRech == 1:
            PStart = NP.array([CoxCrete1[-1], CoyCrete1[-1], CozCrete1[-1]])
        else:
            PStart = NP.array([CoxCrete[-1], CoyCrete[-1], CozCrete[-1]])

        # Definition of the prediction point
        Ppred = PStart + pas * VersAvan
        VersNorm = unitVector(NP.cross(VersAvan, dnor))
        PPlus = Ppred + (lort / 2.) * VersNorm
        PMoin = Ppred - (lort / 2.) * VersNorm

        # Creation of the orthogonal profile
        lignes = []
        groups = []
        arcs = []
        lignes.append((PMoin.tolist(), Ppred.tolist(), nbPoints))
        lignes.append((Ppred.tolist(), PPlus.tolist(), nbPoints))

        resu_mail0, arcgma0, angles0, nbno0 = crea_mail_lig_coup(
            dime, lignes, groups, arcs)
        __MAI = crea_sd_mail(self, os.linesep.join(resu_mail0))

        motclefs2 = {}
        motclefs2['MAILLAGE_1'] = __mail
        motclefs2['MAILLAGE_2'] = __MAI
        try:
            # we try to project, except the case of no points "in the material"
            __YBARPR = PROJ_CHAMP(METHODE='COLLOCATION',
                                  RESULTAT=__resu,
                                  DISTANCE_MAX=distMax,
                                  TYPE_CHAM='NOEU',
                                  NOM_CHAM=NOM_CHAM,
                                  NUME_ORDRE=1,
                                  **motclefs2)

        except E_Exception.error, e:
            # All points outside the material
            if (e.id_message != 'CALCULEL3_97'):
                if dirRech == 1:
                    dirRech = 2
                    i = 0
                else:
                    condSort = seuil * 0.1

        else:
            __YBARCH = CREA_CHAMP(TYPE_CHAM=typeChampTrajet,
                                  OPERATION='EXTR',
                                  NOM_CHAM=NOM_CHAM,
                                  RESULTAT=__YBARPR,
                                  NUME_ORDRE=1,)

            dx0 = __YBARCH.EXTR_COMP(NOM_CMP, [], 1)
            NonVide = NP.array(list(dx0.noeud))

            # Search of the prediction point among projected points
            idxpred = NP.where(NonVide == nbPoints)[0]

            # Prediction point outside material
            if len(idxpred) == 0:
                if dirRech == 1:
                    dirRech = 2
                    i = 0
                    continue
                else:
                    condSort = seuil * 0.1
                    break

            EndoOrth = dx0.valeurs
            Coor0 = __MAI.sdj.COORDO.VALE.get()
            CoorxOrth = NP.array(Coor0[coorIni1:len(Coor0):3], float)
            CooryOrth = NP.array(Coor0[coorIni2:len(Coor0):3], float)
            CoorxOrth = NP.take(CoorxOrth, NonVide - 1)
            CooryOrth = NP.take(CooryOrth, NonVide - 1)
            CoorxOrth = NP.delete(CoorxOrth, idxpred)
            CooryOrth = NP.delete(CooryOrth, idxpred)
            EndoOrth = NP.delete(EndoOrth, idxpred)

            # Smoothing of the projected field on the orthogonal profile
            # EndoReg  = conv_smoothing1D(lreg/3, CoorxOrth,
            # CooryOrth,EndoOrth)
            EndoReg = conv_smoothing1D(lreg, CoorxOrth, CooryOrth, EndoOrth)

            # New crack path point!
            idxmax = NP.argmax(EndoReg)
            endomax = EndoOrth[idxmax]
            cox = CoorxOrth[idxmax]
            coy = CooryOrth[idxmax]
            PtMax = cox * dplan1 + coy * dplan2 + zCoupe * dnor
            condSort = endomax
            # We controle that the point is inside the maximum accepted angle "ANGL_MAX"
            #    otherwise we stop the search in "dirRech" direction

            if round(alpha) != 180.:
                alphar = radians(alpha)
                blim = pas * NP.tan(alphar / 2.)
                btest = ((PtMax[0] - Ppred[0]) ** 2. + (
                    PtMax[1] - Ppred[1]) ** 2. + (PtMax[2] - Ppred[2]) ** 2.) ** 0.5
                if btest > blim:
                    if dirRech == 1:
                        dirRech = 2
                        i = 0
                        condSort = seuil * 1.1
                        continue
                    else:
                        condSort = seuil * 0.1
                        break

            if dirRech == 1:
                if condSort >= seuil:
                    CoxCrete1 = NP.append(CoxCrete1, [PtMax[0]])
                    CoyCrete1 = NP.append(CoyCrete1, [PtMax[1]])
                    CozCrete1 = NP.append(CozCrete1, [PtMax[2]])
                    EndoCrete1 = NP.append(EndoCrete1, [endomax])
                else:
                    dirRech = 2
                    condSort = seuil * 1.1
                    i = 0
            else:
                if condSort >= seuil:
                    cox = NP.append(cox, [2, 6])
                    CoxCrete = NP.append(CoxCrete, [PtMax[0]])
                    CoyCrete = NP.append(CoyCrete, [PtMax[1]])
                    CozCrete = NP.append(CozCrete, [PtMax[2]])
                    EndoCrete = NP.append(EndoCrete, [endomax])
                else:
                    pass

    CoxCrete1 = CoxCrete1.tolist()
    CoxCrete = CoxCrete.tolist()
    CoyCrete1 = CoyCrete1.tolist()
    CoyCrete = CoyCrete.tolist()
    CozCrete1 = CozCrete1.tolist()
    CozCrete = CozCrete.tolist()
    EndoCrete1 = EndoCrete1.tolist()
    EndoCrete = EndoCrete.tolist()
    CoxCrete.reverse()
    CoyCrete.reverse()
    CozCrete.reverse()
    EndoCrete.reverse()
    CoxCrete.extend(CoxCrete1)
    CoyCrete.extend(CoyCrete1)
    CozCrete.extend(CozCrete1)
    EndoCrete.extend(EndoCrete1)

    nbNoeud = len(CoxCrete)
    Connex = []
    for idxNo in range(nbNoeud - 1):
        no1 = idxNo + 1
        no2 = idxNo + 2
        ma = (no1, no2)
        Connex.append(ma)

    return CoxCrete, CoyCrete, CozCrete, EndoCrete, Connex


def calcul_ouverture(self, NOM_CHAM, NOM_CMP, dRECHERCHE, __RESUIN, __mail, infoPlan, inst, CoxCrete, CoyCrete, CozCrete, dime, strong_flag):

    import numpy as NP
    import aster
    from Utilitai.Utmess import UTMESS
    from Accas import _F
    from post_endo_fiss_utils import versDirMoy, findExtr, crackOpeningStrong, crea_sd_mail
    from Macro.macr_lign_coupe_ops import crea_mail_lig_coup

    # --------------------------------------------------
    # IMPORT DES COMMANDES ASTER
    #
    LIRE_MAILLAGE = self.get_cmd('LIRE_MAILLAGE')
    AFFE_MODELE = self.get_cmd('AFFE_MODELE')
    PROJ_CHAMP = self.get_cmd('PROJ_CHAMP')
    CREA_RESU = self.get_cmd('CREA_RESU')
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')
    MODI_REPERE = self.get_cmd('MODI_REPERE')

    # ---------------------------
    # DEVELOPER PARAMETERS
    #
    if strong_flag == True:
        # for the "strong" method, give here necessary parameters :
        #   caracteristic- and orthogonal line lengths
        #   default values : those used for crack path search
        lortOuv = dRECHERCHE['LONG_ORTH']
        lregOuv = dRECHERCHE['LONG_REG']

    # ---------------------------
    # PARAMETRES DE LA RECHERCHE
    #
    nbPoints = dRECHERCHE['NB_POINT']
    lortOuv = dRECHERCHE['LONG_ORTH']
    endoMin = dRECHERCHE['BORNE_MAX']
    champEndo = NOM_CHAM
    cmpEndo = NOM_CMP

    if (champEndo == 'DEPL'):
        typechampEndo = 'NOEU_DEPL_R'
    else:
        typechampEndo = 'NOEU' + '_' + NOM_CHAM[0:4] + '_R'
        if (champEndo == 'VARI_NOEU'):
            typechampEndo = 'NOEU_VAR2_R'

    # ---------------------------
    #  2D AND MESH PARAMETERS
    #
    #__mail   = modelisa['MAILLAGE']
    #__modtot = modelisa['MODELE']
    coorIni1 = infoPlan[0]
    coorIni2 = infoPlan[1]
    dnor = infoPlan[2]
    dplan1 = infoPlan[3]
    dplan2 = infoPlan[4]

    (lst_tanPoi, lst_normPoi) = versDirMoy(CoxCrete, CoyCrete, CozCrete, dnor)

    motclefs = {}
    ChampsResu = __RESUIN.LIST_CHAMPS()
    lstChampsResu = ChampsResu.keys()

    methodeProj = 'COLLOCATION'
    methodeProjVI = 'COLLOCATION'
    composante = 'DY'
    champ = 'DEPL'
    typeChamp = 'NOEU_DEPL_R'
    motclefs['MODI_CHAM'] = [_F(NOM_CHAM='DEPL',
                                NOM_CMP=('DX', 'DY',),
                                TYPE_CHAM = 'VECT_2D',),
                             ]

    nbPrec = NP.finfo(NP.float).precision
    distMax = 10. ** (-nbPrec + 2)

    if strong_flag == True:
        composante = 'EPYY'
        champ = 'EPSI_NOEU'
        typeChamp = 'NOEU_EPSI_R'
        motclefs['MODI_CHAM'] = [_F(NOM_CHAM=champ,
                                    NOM_CMP=(
                                    'EPXX', 'EPYY', 'EPZZ', 'EPXY',),
                                    TYPE_CHAM = 'TENS_2D',), ]

    # ---------------------
    # LOOP CRACK PATH POINTS
    #     "lstOuvFiss" : list of crack openings on the crack path
    #     "lstErr"     : list on errors for each crack opening ("STRONG" method)

    lstOuvFiss = []
    lstErr = []
    for idxPoi in range(len(CoxCrete)):
        # Creation of the mesh and model on the considered orthogonal profile
        PtCrete = NP.array(
            [CoxCrete[idxPoi], CoyCrete[idxPoi], CozCrete[idxPoi]])
        PGort = PtCrete + (lortOuv / 2.) * lst_normPoi[idxPoi]
        PDort = PtCrete - (lortOuv / 2.) * lst_normPoi[idxPoi]
        lignort = []
        groups0 = []
        arcs0 = []
        lignort.append((PGort, PtCrete, nbPoints))
        lignort.append((PtCrete, PDort, nbPoints))

        resu_mail0, arcgma0, angles0, nbno0 = crea_mail_lig_coup(
            dime, lignort, groups0, arcs0)
        __MAI = crea_sd_mail(self, os.linesep.join(resu_mail0))

        CoorTotOrtho = __MAI.sdj.COORDO.VALE.get()
        XtotOrtho = NP.array(CoorTotOrtho[coorIni1:len(CoorTotOrtho):3])
        YtotOrtho = NP.array(CoorTotOrtho[coorIni2:len(CoorTotOrtho):3])

        # Rotation of the diplacement or strain field on the crack reference
        # system
        vec_tan = lst_tanPoi[idxPoi]
        vec_nor = lst_normPoi[idxPoi]
        M = NP.concatenate(([vec_tan], [vec_nor], [dnor]), axis=0)
        M = NP.transpose(M)

        Angles = {}
        (alpha, beta, gamma) = euler_angles(M)
        __RESROT = MODI_REPERE(RESULTAT=__RESUIN,
                               INST=inst,
                               REPERE='UTILISATEUR',
                               AFFE=_F(
                               ANGL_NAUT=(alpha, beta, gamma),
                               ),
                               **motclefs)

        # Projection of displ. or strain field on the orthogonal profile
        #    and extraction of this field from the result concept
        motclefs2 = {}
        motclefs2['MAILLAGE_1'] = __mail
        motclefs2['MAILLAGE_2'] = __MAI
        except1 = 'False'
        except2 = 'False'
        try:
            __OUVEPR = PROJ_CHAMP(METHODE=methodeProj,
                                  RESULTAT=__RESROT,
                                  DISTANCE_MAX=distMax,
                                  NOM_CHAM=champ,
                                  INST=inst,
                                  **motclefs2)
        except:
            lstOuvFiss.append('-')
            except1 = 'True'

        else:

            __OUVECH = CREA_CHAMP(TYPE_CHAM=typeChamp,
                                  OPERATION='EXTR',
                                  NOM_CHAM=champ,
                                  RESULTAT=__OUVEPR,
                                  INST=inst,)

            # Retrieving to Python objects the strain or displacement field values
            #    on the orthogonal profile, and the orthogonal mesh.
            ChampOrthoCH = __OUVECH.EXTR_COMP(composante, [], 1)
            ChampOrtho = ChampOrthoCH.valeurs
            NonVide = NP.array(list(ChampOrthoCH.noeud))
            idxCentre = NP.where(NonVide == nbPoints)[0]
            XtotOrtho1 = NP.take(XtotOrtho, NonVide - 1)
            YtotOrtho1 = NP.take(YtotOrtho, NonVide - 1)
            XtotOrtho1 = NP.delete(XtotOrtho1, idxCentre)
            YtotOrtho1 = NP.delete(YtotOrtho1, idxCentre)
            ChampOrtho = NP.delete(ChampOrtho, idxCentre)

        if strong_flag == False:
            try:
        # Projection of the damage field on the orthogonal profile
        #    and extraction of this field from the result concept
                __ENDOPR = PROJ_CHAMP(METHODE=methodeProjVI,
                                      RESULTAT=__RESUIN,
                                      DISTANCE_MAX=distMax,
                                      NOM_CHAM=champEndo,
                                      INST=inst,
                                      **motclefs2)
            except:
                if except1:
                    pass
                else:
                    lstOuvFiss.append('-')
                    except2 = 'True'
            else:

                __ENDOCH = CREA_CHAMP(TYPE_CHAM=typechampEndo,
                                      OPERATION='EXTR',
                                      NOM_CHAM=champEndo,
                                      RESULTAT=__ENDOPR,
                                      INST=inst,)

                EndoOrthoCH = __ENDOCH.EXTR_COMP(cmpEndo, [], 1)
                EndoOrtho = EndoOrthoCH.valeurs
                NonVide = NP.array(list(EndoOrthoCH.noeud))
                idxCentre = NP.where(NonVide == nbPoints)[0]
                XtotOrtho2 = NP.take(XtotOrtho, NonVide - 1)
                YtotOrtho2 = NP.take(YtotOrtho, NonVide - 1)
                XtotOrtho2 = NP.delete(XtotOrtho2, idxCentre)
                YtotOrtho2 = NP.delete(YtotOrtho2, idxCentre)
                EndoOrtho = NP.delete(EndoOrtho, idxCentre)

            # -----Extraction de l'ouverture de fissure / erreur ---------
            #
                try:
                    idxG, idxD = findExtr(EndoOrtho, endoMin, idxCentre)
                except (ThresholdTooHighError, NoMaximaError):
                    if except1 or except2:
                        pass
                    else:
                        lstOuvFiss.append('-')
                else:
                    ouvFiss = float(abs(ChampOrtho[idxD] - ChampOrtho[idxG]))
                    lstOuvFiss.append(ouvFiss)
        else:  # methode strong
            ouvFiss, errOuv = crackOpeningStrong(
                lortOuv, XtotOrtho, XtotOrtho, ChampOrtho)
            lstOuvFiss.append(ouvFiss)
            lstErr.append(errOuv)

    if strong_flag == False:
        return lstOuvFiss
    else:  # methode strong
        return lstOuvFiss, lstErr


def post_endo_fiss_ops(self,
                       TABLE,
                       OUVERTURE,
                       NOM_CMP,
                       NOM_CHAM,
                       RECHERCHE,
                       **args):

    import aster
    from Utilitai.Utmess import UTMESS, MasquerAlarme, RetablirAlarme
    from Accas import _F

    # --------------------------------------------------
    # DEVELOPER OPTIONS
    #
    # "strong_flag" must be set to True if computing crack opening with the "strong" method
    strong_flag = False

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    MasquerAlarme('CALCULEL5_48')
    MasquerAlarme('ALGORITH12_43')
    MasquerAlarme('CALCULEL2_12')
    MasquerAlarme('CALCULEL5_7')

    # --------------------------------------------------
    # OUTPUT DECLARATION
    #
    self.DeclareOut('MAFISS', self.sd)
    self.DeclareOut('tabRes', TABLE)

    # --------------------------------------------------
    # IMPORT OF ASTER COMMANDS
    #
    LIRE_MAILLAGE = self.get_cmd('LIRE_MAILLAGE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')
    CO = self.get_cmd('CO')
    IMPR_RESU = self.get_cmd('IMPR_RESU')
    RECU_TABLE = self.get_cmd('RECU_TABLE')

    # --------------------------------------------------
    #  INPUT PARAMETERS
    #
    l_dRECHERCHE = []
    for recherche in RECHERCHE:
        dRECHERCHE = recherche.cree_dict_valeurs(recherche.mc_liste)
        for i in dRECHERCHE.keys():
            if dRECHERCHE[i] == None:
                del dRECHERCHE[i]
        l_dRECHERCHE.append(dRECHERCHE)

    # --------------------------------------------------
    # INPUT PARAMETERS, MESH AND MODEL
    #
    motscles = {}

    for dRECHERCHE in l_dRECHERCHE:
        if (OUVERTURE == 'OUI') and ('BORNE_MAX' not in dRECHERCHE.keys()):
            UTMESS('F', 'POST0_44')

    if args['CHAM_GD'] != None:
        build = 'champ'
        __ENDO = args['CHAM_GD']
        inst = 1.
        motscles['INST'] = inst

        n_mail = __ENDO.sdj.REFE.get()[0].strip()
        __mail = self.get_concept(n_mail)

    else:
        build = 'resu'
        __RESUIN = args['RESULTAT']
        nomresu = __RESUIN.nom
        dicResu = __RESUIN.LIST_PARA()
        dicVarAcc = __RESUIN.LIST_VARI_ACCES()
        if args['NUME_ORDRE'] != None:
            nume_ordre = args['NUME_ORDRE']
            if nume_ordre not in dicVarAcc['NUME_ORDRE']:
                UTMESS('F', 'POST0_41')
            else:
                inst = (dicVarAcc['INST'])[nume_ordre]
            motscles['NUME_ORDRE'] = nume_ordre
        else:
            inst = args['INST']
            motscles['INST'] = inst
            nume_ordre = None
            for champ_inst_index, champ_inst in enumerate(dicVarAcc['INST']):
                if round(champ_inst, 12) == round(inst, 12):
                    nume_ordre = dicVarAcc['NUME_ORDRE'][champ_inst_index]
                    break
            if nume_ordre is None:
                UTMESS('F', 'POST0_41')

        # Maillage pour projections
        iret, ibid, n_mail = aster.dismoi(
            'NOM_MAILLA', __RESUIN.nom, 'RESULTAT', 'F')
        __mail = self.get_concept(n_mail)

    dime = __mail.sdj.DIME.get()[5]

    # --------------------------------------------------
    # CONTROLS ON THE INPUT FIELDS
    #
    if build == 'resu':
        ChampsResu = __RESUIN.LIST_CHAMPS()
        lstChampsResu = ChampsResu.keys()
        if (NOM_CHAM not in lstChampsResu):
            UTMESS('F', 'POST0_42')
        elif (nume_ordre not in ChampsResu[NOM_CHAM]):
            UTMESS('F', 'POST0_41')
        else:
            pass

    if build == 'champ' and OUVERTURE == 'OUI':
        UTMESS('F', 'POST0_43')

    if ('NOEU' in NOM_CHAM) or (NOM_CHAM == 'DEPL'):
        typeChampTrajet = 'NOEU' + '_' + NOM_CHAM[0:4] + '_R'
        if NOM_CHAM == 'VARI_NOEU':
            typeChampTrajet = 'NOEU_VAR2_R'
    else:
        UTMESS('F', 'POST0_35')

    # --------------------------------------------------
    # QUANTITIES FOR THE 2D PROCEDURE
    #
    __TABG = RECU_TABLE(CO=__mail,
                        NOM_TABLE='CARA_GEOM',)

    xmin = __TABG['X_MIN', 1]
    xmax = __TABG['X_MAX', 1]
    ymin = __TABG['Y_MIN', 1]
    ymax = __TABG['Y_MAX', 1]
    zmin = __TABG['Z_MIN', 1]
    zmax = __TABG['Z_MAX', 1]

    nbPrec = NP.finfo(NP.float).precision
    delta_x = NP.round(xmax - xmin, nbPrec)
    delta_y = NP.round(ymax - ymin, nbPrec)
    delta_z = NP.round(zmax - zmin, nbPrec)

    Ddim = [delta_x, delta_y, delta_z]
    delta_min = min(Ddim)
    if NP.round(delta_min, nbPrec - 2) != 0.:
        UTMESS('F', 'POST0_34')
    else:
        idx_plan = Ddim.index(delta_min)

    # PLAN == 'XY' :
    if idx_plan == 2:
        coorIni1 = 0
        coorIni2 = 1
        dnor = NP.array([0., 0., 1.], float)
        dplan1 = NP.array([1., 0., 0.], float)
        dplan2 = NP.array([0., 1., 0.], float)
    # PLAN == 'XZ':
    elif idx_plan == 1:
        coorIni1 = 0
        coorIni2 = 2
        dnor = NP.array([0., 1., 0.], float)
        dplan1 = NP.array([1., 0., 0.], float)
        dplan2 = NP.array([0., 0., 1.], float)
    # PLAN == 'YZ':
    else:
        coorIni1 = 1
        coorIni2 = 2
        dnor = NP.array([1., 0., 0.], float)
        dplan1 = NP.array([0., 1., 0.], float)
        dplan2 = NP.array([0., 0., 1.], float)

    infoPlan = (coorIni1, coorIni2, dnor, dplan1, dplan2)

    # --------------------------------------------------
    # FIELD FOR CRACK PATH SEARCH
    #
    if build == 'resu':
        __ENDO = CREA_CHAMP(TYPE_CHAM=typeChampTrajet,
                            OPERATION='EXTR',
                            RESULTAT=__RESUIN,
                            NOM_CHAM=NOM_CHAM,
                            **motscles)

    # --------------------------------------------------
    # LOOP ON THE FPZs (INSTANCES OF KEYWORD "RECHERCHE")
    #
    XcreteTot = []
    YcreteTot = []
    ZcreteTot = []
    ConnTot = []
    EndocreteTot = []
    lstFissure = []
    lstOuverture = []
    lstErreur = []
    lstNomFiss = []

    for idxRech in range(len(l_dRECHERCHE)):

        dRECHERCHE = l_dRECHERCHE[idxRech]

        (CoxCrete, CoyCrete, CozCrete, EndoCrete, Connex) = cherche_trajet(
            self, NOM_CMP, NOM_CHAM, dRECHERCHE, __ENDO, __mail, typeChampTrajet, infoPlan, inst)
        if OUVERTURE == 'OUI':
            if strong_flag == False:
                lstOuvFiss = calcul_ouverture(
                    self, NOM_CHAM, NOM_CMP, dRECHERCHE, __RESUIN, __mail, infoPlan, inst, CoxCrete, CoyCrete, CozCrete, dime, strong_flag)
            else:
                lstOuvFiss, lstErr = calcul_ouverture(
                    self, NOM_CHAM, NOM_CMP, dRECHERCHE, __RESUIN, __mail, infoPlan, inst, nume_ordre, CoxCrete, CoyCrete, CozCrete, dime, strong_flag)
        XcreteTot.append(CoxCrete)
        YcreteTot.append(CoyCrete)
        ZcreteTot.append(CozCrete)
        EndocreteTot.append(EndoCrete)
        ConnTot.append(Connex)
        if 'GROUP_MA' in dRECHERCHE.keys():
            nomFissure = dRECHERCHE['GROUP_MA']
        else:
            nomFissure = 'FISS' + str(idxRech + 1)
        lstFissure = lstFissure + ([nomFissure] * len(CoxCrete))
        lstNomFiss.append(nomFissure)

        if OUVERTURE == 'OUI':
            if '-' in lstOuvFiss:
                UTMESS('A', 'POST0_33', nomFissure)
            lstOuverture.append(lstOuvFiss)
            if strong_flag == True:
                lstErreur.append(lstErr)

    lstX = []
    lstY = []
    lstZ = []
    lstEndo = []
    if OUVERTURE == 'OUI':
        lstO = []
        if strong_flag == True:
            lstE = []

    for i in range(len(XcreteTot)):
        lstX = lstX + XcreteTot[i]
        lstY = lstY + YcreteTot[i]
        lstZ = lstZ + ZcreteTot[i]
        lstEndo = lstEndo + EndocreteTot[i]
        if OUVERTURE == 'OUI':
            lstO = lstO + lstOuverture[i]
            if strong_flag == True:
                lstE = lstE + lstErreur[i]

  # -----------------------------------------------------
  # CREATION OF A TABLE TO STOCK CRACK PATH COORDINATES
  #   AND CRACK OPENING
  #
    if OUVERTURE == 'NON':
        tabRes = CREA_TABLE(LISTE=(
            _F(PARA='FISSURE', LISTE_K=lstFissure),
            _F(PARA='COORX', LISTE_R=lstX),
            _F(PARA='COORY', LISTE_R=lstY),
            _F(PARA='COORZ', LISTE_R=lstZ),
            _F(PARA='CHAMP', LISTE_R=lstEndo),
        ),)

    else:
        if strong_flag == False:
            tabRes = CREA_TABLE(LISTE=(
                _F(PARA='FISSURE', LISTE_K=lstFissure),
                _F(PARA='COORX', LISTE_R=lstX),
                _F(PARA='COORY', LISTE_R=lstY),
                _F(PARA='COORZ', LISTE_R=lstZ),
                _F(PARA='CHAMP', LISTE_R=lstEndo),
                _F(PARA='OUVERTURE', LISTE_R=lstO),
            ),)

        else:  # STRONG Method
            tabRes = CREA_TABLE(LISTE=(
                _F(PARA='FISSURE', LISTE_K=lstFissure),
                _F(PARA='COORX', LISTE_R=lstX),
                _F(PARA='COORY', LISTE_R=lstY),
                _F(PARA='COORZ', LISTE_R=lstZ),
                _F(PARA='CHAMP', LISTE_R=lstEndo),
                _F(PARA='OUVERTURE', LISTE_R=lstO),
                _F(PARA='ERREUR', LISTE_R=lstE),
            ),)

    # --------------------------------------------------
    # CREATION OF DATA STRUCTURE "MESH"
    #
    resu_mail0 = crea_mail_lin(
        XcreteTot, YcreteTot, ZcreteTot, ConnTot, lstNomFiss, dime)
    nomFichierSortie = os.path.join(os.getcwd(), 'maillage.mail')
    fproc = open(nomFichierSortie, 'w')
    fproc.write(resu_mail0)
    fproc.close()
    UL = UniteAster()
    uniteMail = UL.Libre(action='ASSOCIER', nom=nomFichierSortie)
    MAFISS = LIRE_MAILLAGE(UNITE=uniteMail,)
    UL.EtatInit(uniteMail)

    RetablirAlarme('CALCULEL5_48')
    RetablirAlarme('ALGORITH12_43')
    RetablirAlarme('CALCULEL2_12')
    RetablirAlarme('CALCULEL5_7')

    return ier
