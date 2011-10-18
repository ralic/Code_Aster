#@ MODIF calc_ecrevisse_ops Macro  DATE 17/10/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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


# ------------------OUTILS ------------------------------

# Determination de la direction de la fissure
#   a partir du points initial et final :
# theta : angle par rapport a la verticale ascendente (degres)
#         positif si sens horaire
#   -180< theta <=180
# beta : angle par rapport a la horizontale (degres)
#         positif si sens anti-horaire
#         -180< beta <=180
def dirfiss(Xa,Ya,Xb,Yb) :
    from math import atan2, degrees

    xia = Xa[0]
    yia = Ya[0]
    xea = Xa[-1]
    yea = Ya[-1]
    xib = Xb[0]
    yib = Yb[0]
    xeb = Xb[-1]
    yeb = Yb[-1]

    xi = (xia+xib)/2.
    yi = (yia+yib)/2.
    xe = (xea+xeb)/2.
    ye = (yea+yeb)/2.

    dx = xe -xi
    dy = ye -yi

    try :
        tangA= dy/dx
    except ZeroDivisionError :
        if (dy>0.) :
            theta = 0.
            beta  = 90.
        else :
            theta = 180.
            beta  = -90.
    else :
        beta = atan2(dy,dx)
        beta = degrees(beta)
        theta = 90.-beta
        if (theta>180.):
            theta=theta-360.

    if (abs(beta)<45. or abs(beta)>135.) :
        DIR_PREV = 'X'
    else:
        DIR_PREV = 'Y'

    if (round(abs(beta))==0. or round(abs(beta))==180.):
        DIR_FISS = 'X'
    elif (round(abs(beta))==90.):
        DIR_FISS = 'Y'
    else :
        DIR_FISS = 'GEN'

    return DIR_FISS, DIR_PREV, beta, theta, xi, yi


# Determination de l ouverture de la fissure
def ouvFiss(DIR_FISS,beta,Xa,Ya,Xb,Yb) :
    from math import sin, cos, sqrt, radians

    if DIR_FISS=='X' :
        Ouv = map(lambda y1,y2: abs(y2-y1),Ya,Yb)
        Gli = map(lambda x1,x2: abs(x2-x1),Xa,Xb)
    elif DIR_FISS=='Y' :
        Ouv = map(lambda x1,x2: abs(x2-x1),Xa,Xb)
        Gli = map(lambda y1,y2: abs(y2-y1),Ya,Yb)
    else :
        xi  = (Xa[0]+Xb[0])*0.5
        yi  = (Ya[0]+Yb[0])*0.5
        Xa1 = [x-y for (x,y) in zip(Xa,[xi]*len(Xa))]
        Ya1 = [x-y for (x,y) in zip(Ya,[yi]*len(Ya))]
        Xb1 = [x-y for (x,y) in zip(Xb,[xi]*len(Xb))]
        Yb1 = [x-y for (x,y) in zip(Yb,[yi]*len(Yb))]
        Xa2 = [ x*cos(radians(beta)) + y*sin(radians(beta)) for (x,y) in zip(Xa,Ya)]
        Ya2 = [-x*sin(radians(beta)) + y*cos(radians(beta)) for (x,y) in zip(Xa,Ya)]
        Xb2 = [ x*cos(radians(beta)) + y*sin(radians(beta)) for (x,y) in zip(Xb,Yb)]
        Yb2 = [-x*sin(radians(beta)) + y*cos(radians(beta)) for (x,y) in zip(Xb,Yb)]
        Ouv = map(lambda x,y: abs(y-x), Ya2, Yb2)
        Gli = map(lambda x,y: abs(y-x), Xa2, Xb2)
    return Ouv, Gli





# ------------------DEBUT MACRO ------------------------------
# Debut de la macro, on impose en parametre les donnees placer
#     dans T_EC, l'appel a ecrevisse
def calc_ecrevisse_ops(self,
    CHARGE_MECA,
    CHARGE_THER1,
    CHARGE_THER2,
    TABLE,
    DEBIT,
    MODELE_MECA,
    MODELE_THER,
    RESULTAT,
    FISSURE,
    ECOULEMENT,
    MODELE_ECRE,
    CONVERGENCE,
    LOGICIEL,
    VERSION,
    ENTETE,
    IMPRESSION,
    INFO,
    COURBES,
    **args):

    """
        Procedure de couplage Aster-Ecrevisse
        Recuperation du profil de la fissure , appel de MACR_ECRE_CALC,
        creation des tableaux de resultats et des chargements pour AsterGeneration par Aster
    """

    import os, string, types, shutil
    import aster
    from Accas import _F
    from Noyau.N_utils import AsType
    from Utilitai.Utmess import UTMESS, MasquerAlarme, RetablirAlarme
    from Utilitai.Table import Table, merge
    from copy import copy
    from math import atan, pi, sqrt, atan2, degrees, sin, cos

    ier=0

    # Concepts sortant
    self.DeclareOut('__TAB',TABLE)
    self.DeclareOut('__ECR_F1',CHARGE_THER1)
    self.DeclareOut('__ECR_F2',CHARGE_THER2)
    self.DeclareOut('__ECR_P',CHARGE_MECA)
    self.DeclareOut('__DEB',DEBIT)

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Parametres debug
    debug = False

    # Fonctionnalitees cachees :
    # 1- oldVersion : "=True" permet de retourner un chargement thermique defini selon l'ancien mode (flux thermique)
    #                 ATTENTION!! L'ancienne version ne marche que avec la fissure orientee dans la direction Y,
    #                 et avec le point initila en y=0!!
    # 2- chargeLin : "=True" permet d'appliquer le chargement d'Ecrevisse avec interpolation lineaire sur le mailles,
    #                 et non constant par maille, comme c'est par defaut
    oldVersion = False
    chargeLin  = False


    # Parametres developpeur
    tmp_ecrevisse = "tmp_ecrevisse"

    # Info
    info2 = (INFO==2)
    InfoAster = 1
    if debug :
        info2=True

    # IMPORTATION DE COMMANDES ASTER
    DEFI_GROUP       = self.get_cmd("DEFI_GROUP")
    POST_RELEVE_T    = self.get_cmd("POST_RELEVE_T")
    MACR_ECRE_CALC   = self.get_cmd("MACR_ECRE_CALC")
    IMPR_TABLE       = self.get_cmd("IMPR_TABLE")
    DETRUIRE         = self.get_cmd("DETRUIRE")
    IMPR_CO          = self.get_cmd("IMPR_CO")
    DEFI_FONCTION    = self.get_cmd("DEFI_FONCTION")
    CREA_TABLE       = self.get_cmd("CREA_TABLE")
    IMPR_TABLE       = self.get_cmd("IMPR_TABLE")
    CO               = self.get_cmd("CO")
    AFFE_CHAR_THER_F = self.get_cmd("AFFE_CHAR_THER_F")
    AFFE_CHAR_MECA_F = self.get_cmd("AFFE_CHAR_MECA_F")
    DEFI_FICHIER     = self.get_cmd("DEFI_FICHIER")
    DEFI_CONSTANTE   = self.get_cmd("DEFI_CONSTANTE")

    # RECUPERATION DES MOTS-CLES FACTEURS
    dRESULTAT=RESULTAT[0].cree_dict_valeurs(RESULTAT[0].mc_liste)
    for i in dRESULTAT.keys():
        if dRESULTAT[i]==None : del dRESULTAT[i]

    dECOULEMENT=ECOULEMENT[0].cree_dict_valeurs(ECOULEMENT[0].mc_liste)
    for i in dECOULEMENT.keys():
        if dECOULEMENT[i]==None : del dECOULEMENT[i]

    dMODELE_ECRE=MODELE_ECRE[0].cree_dict_valeurs(MODELE_ECRE[0].mc_liste)
    for i in dMODELE_ECRE.keys():
        if dMODELE_ECRE[i]==None : dMODELE_ECRE[i]=None #del dMODELE_ECRE[i]

    dCONVERGENCE=CONVERGENCE[0].cree_dict_valeurs(CONVERGENCE[0].mc_liste)
    for i in dCONVERGENCE.keys():
        if dCONVERGENCE[i]==None : del dCONVERGENCE[i]


    # INSTANTS
    _l_inst = dRESULTAT['MECANIQUE'].LIST_VARI_ACCES()
    if dRESULTAT.has_key('INST'):
        Inst_Ecrevisse = dRESULTAT['INST']
    else:
        pass


    # INITIATION DES CHARGEMENTS ECREVISSE --> ASTER :
    #     liste des mots cles facteurs de PRES_REP pour chaque fissure
    l_PRES_REP = []
    if oldVersion :
        # liste des mots cles facteurs de FLUX_REP pour le flux F1/F2
        #     de chaque fissure (soit sur les deux levres :GROUP_MA[0], GROUP_MA[1])
        l_FLUX_REP_F1 = []
        l_FLUX_REP_F2 = []
    else :
        # liste des mots cles facteurs de ECHANGE pour le flux F1/F2
        #     de chaque fissure (soit sur les deux levres :GROUP_MA[0], GROUP_MA[1])
        l_ECHANGE_F1 = []
        l_ECHANGE_F2 = []



    # ON CREE LES GROUP_NO ORDONNES DES LEVRES DE FISSURE
    #     Liste des noms des groupes de noeuds du maillage :
    _lgno = map(lambda x: x[0], MODELE_MECA['MAILLAGE'].LIST_GROUP_NO() )

    for k, fissure in enumerate(FISSURE):
        dFISSURE=fissure.cree_dict_valeurs(fissure.mc_liste)
        for i in dFISSURE.keys():
            if dFISSURE[i]==None : del dFISSURE[i]

        # On cree les group_no correspondant aux group_ma des levres de la fissure dans le cas ou ils n'existent pas deja
        if not dFISSURE['GROUP_MA'][0] in _lgno:
            DEFI_GROUP(reuse = MODELE_MECA['MAILLAGE'],
                       MAILLAGE=MODELE_MECA['MAILLAGE'],
                       CREA_GROUP_NO=_F(GROUP_MA=(dFISSURE['GROUP_MA'][0]),),);

        if not dFISSURE['GROUP_MA'][1] in _lgno:
            DEFI_GROUP(reuse = MODELE_MECA['MAILLAGE'],
                       MAILLAGE=MODELE_MECA['MAILLAGE'],
                       CREA_GROUP_NO=_F(GROUP_MA=(dFISSURE['GROUP_MA'][1]),),);

        # Test sur le nombre de caracteres du nom des group_ma
        if (len(dFISSURE['GROUP_MA'][0]) >7 or len(dFISSURE['GROUP_MA'][1]) >7):
            sys.exit(1)

        # Creation des group_no ordonnes des levres des fissures
        _nom_gno_1 = '_' + dFISSURE['GROUP_MA'][0]
        if not _nom_gno_1 in _lgno:
            DEFI_GROUP(reuse = MODELE_MECA['MAILLAGE'],
                       MAILLAGE=MODELE_MECA['MAILLAGE'],
                       CREA_GROUP_NO=_F(OPTION='SEGM_DROI_ORDO',
                                        NOM=_nom_gno_1,
                                        GROUP_NO=dFISSURE['GROUP_MA'][0],
                                        GROUP_NO_ORIG=dFISSURE['GROUP_NO_ORIG'][0],
                                        GROUP_NO_EXTR=dFISSURE['GROUP_NO_EXTR'][0],
                                        PRECISION=0.01,
                                        CRITERE='RELATIF',),
                       INFO=InfoAster,);

        _nom_gno_2 = '_' + dFISSURE['GROUP_MA'][1]
        if not _nom_gno_2 in _lgno:
            DEFI_GROUP(reuse = MODELE_MECA['MAILLAGE'],
                        MAILLAGE=MODELE_MECA['MAILLAGE'],
                        CREA_GROUP_NO=_F(OPTION='SEGM_DROI_ORDO',
                                        NOM=_nom_gno_2,
                                        GROUP_NO=dFISSURE['GROUP_MA'][1],
                                        GROUP_NO_ORIG=dFISSURE['GROUP_NO_ORIG'][1],
                                        GROUP_NO_EXTR=dFISSURE['GROUP_NO_EXTR'][1],
                                        PRECISION=0.01,
                                        CRITERE='RELATIF',),
                        INFO=InfoAster,);



        # EXTRACTIONS DE QUANTITES NECESSAIRES DES RESULTATS THERMIQUE ET MECANIQUE
        #  On cree les chargements Aster --> Ecrevisse :
        #     ouverture de fissure et temperature sur le materiau

        # premiere levre de fissure
        _T_DPL=POST_RELEVE_T(ACTION=_F(INTITULE='DEP_FIS1',
                                      GROUP_NO=_nom_gno_1,
                                      RESULTAT=dRESULTAT['MECANIQUE'],
                                      NOM_CHAM='DEPL',
                                      NOM_CMP=('DX','DY',),
                                      INST = Inst_Ecrevisse,
                                      OPERATION='EXTRACTION',),
                                      );

        _T_TEMP=POST_RELEVE_T(ACTION=_F(INTITULE='TEMP_FI1',
                                        GROUP_NO=_nom_gno_1,
                                        RESULTAT=dRESULTAT['THERMIQUE'],
                                        NOM_CHAM='TEMP',
                                        TOUT_CMP='OUI',
                                        INST = Inst_Ecrevisse,
                                        OPERATION='EXTRACTION',),);

        # deuxieme levre de la fissure
        _T_DPL_B=POST_RELEVE_T(ACTION=_F(INTITULE='DEP_FIS2',
                                        GROUP_NO=_nom_gno_2,
                                        RESULTAT=dRESULTAT['MECANIQUE'],
                                        NOM_CHAM='DEPL',
                                        NOM_CMP=('DX','DY',),
                                        INST = Inst_Ecrevisse,
                                        OPERATION='EXTRACTION',),);

        _T_TEMPB=POST_RELEVE_T(ACTION=_F(INTITULE='TEMP_FI2',
                                          GROUP_NO=_nom_gno_2,
                                          RESULTAT=dRESULTAT['THERMIQUE'],
                                          NOM_CHAM='TEMP',
                                          TOUT_CMP='OUI',
                                          INST = Inst_Ecrevisse,
                                          OPERATION='EXTRACTION',),);

        if ( debug ) :
            print '_T_DPL ==================================================='
            print  _T_DPL.EXTR_TABLE()
            print '_T_DPL_B ================================================='
            print  _T_DPL_B.EXTR_TABLE()
            print '_T_TEMP ================================================='
            print  _T_TEMP.EXTR_TABLE()
            print '_T_TEMP_B ==============================================='
            print  _T_TEMPB.EXTR_TABLE()

        # Extraction des tables Temperatures + deplacement levres fissure
        _tbl_temp = _T_TEMP.EXTR_TABLE()
        _tbl_dpl   = _T_DPL.EXTR_TABLE()
        DETRUIRE(CONCEPT=_F(NOM=(_T_DPL,_T_TEMP,)),INFO=1,ALARME='NON')
        _tbl_dpl_b  = _T_DPL_B.EXTR_TABLE()
        _tbl_temp_b = _T_TEMPB.EXTR_TABLE()
        DETRUIRE(CONCEPT=_F(NOM=(_T_DPL_B,_T_TEMPB,)),INFO=1,ALARME='NON')


        # --Determination des cotes a donner a ecrevisse--
        #   a partir des resultats mecanique et thermique :
        _l_tang   = _tbl_dpl.values()['ABSC_CURV']
        _l_tang_b = _tbl_dpl_b.values()['ABSC_CURV']
        try :
            _l_absz_m   = map(lambda x,y: 0.5*(x+y), _l_tang,_l_tang_b)
        except TypeError :
            UTMESS('F','ECREVISSE0_40')
        #
        _l_tang_t  = _tbl_temp.values()['ABSC_CURV']
        _l_tang_t_b  = _tbl_temp_b.values()['ABSC_CURV']
        _l_absz_t   = map(lambda x,y: 0.5*(x+y), _l_tang_t,_l_tang_t_b)


       # Coordonnees des points des levres (initiales et a l instant actuel
        _X0    = _tbl_dpl.values()['COOR_X']
        _Y0    = _tbl_dpl.values()['COOR_Y']
        _X0_b  = _tbl_dpl_b.values()['COOR_X']
        _Y0_b  = _tbl_dpl_b.values()['COOR_Y']
        _X    = [x + y for (x,y) in zip(_tbl_dpl.values()['DX'],_X0)]
        _Y    = [x + y for (x,y) in zip(_tbl_dpl.values()['DY'],_Y0)]
        _X_b    = [x + y for (x,y) in zip(_tbl_dpl_b.values()['DX'],_X0_b)]
        _Y_b    = [x + y for (x,y) in zip(_tbl_dpl_b.values()['DY'],_Y0_b)]


        # Determination de la direction de la fissure
        (DIR_FISS, DIR_PREV, beta, theta, _xi, _yi) = dirfiss(_X,_Y,_X_b,_Y_b)
        if oldVersion :
            (DIR_FISS, DIR_PREV, beta, theta, _xi, _yi) = dirfiss(_X0,_Y0,_X0_b,_Y0_b)

        if (DIR_FISS == 'GEN') and oldVersion :
            UTMESS('F','ECREVISSE0_23', valr=[theta])

        # --Calcul de l ouverture de fissure--
        #     une fissure refermee a une ouverture
        #     egale a l'ouverture remanente
        (_l_ouv0,_l_gli0) = ouvFiss(DIR_FISS,beta,_X0,_Y0,_X0_b,_Y0_b)
        for i in range(len(_l_ouv0)) :
            if _l_ouv0[i] < dFISSURE['OUVERT_REMANENTE'] :
                UTMESS('A','ECREVISSE0_39')


        (_l_ouv,_l_gli)   = ouvFiss(DIR_FISS,beta,_X,_Y,_X_b,_Y_b)
        if dFISSURE['OUVERT_REMANENTE'] :
            _l_ouv = map(lambda x: max(dFISSURE['OUVERT_REMANENTE'],x), _l_ouv)
            if info2 :
                nbOuvRem = _l_ouv.count(dFISSURE['OUVERT_REMANENTE'])
                if nbOuvRem != 0 :
                    UTMESS('I', 'ECREVISSE0_41',valr=[nbOuvRem])



        # Controle sur l entite du glissement entre les levres
        DeltaMaille = [abs(y-x) for (x,y) in zip(_l_absz_m[1:len(_l_absz_m)], _l_absz_m[0:len(_l_absz_m)-1])]
        for i in range(len(DeltaMaille)) :
            deltamai = DeltaMaille[i]
            if (deltamai <= _l_gli[i]) or (deltamai <= _l_gli[i+1]) :
                UTMESS('A', 'ECREVISSE0_38')
                break


        # -- Calcul de la temperature sur le materiau (levres de la fissure) --
        #     on fait la moyenne des temperatures des deux levres
        _l_t2  = _tbl_temp.values()['TEMP']
        _l_t2_b  = _tbl_temp_b.values()['TEMP']
        _l_temp_aster  = map(lambda x,y: 0.5*(x+y), _l_t2_b,_l_t2)


        # Infos / Debug : fichier .mess ou .resu
        if (info2) :
            UTMESS('I', 'ECREVISSE0_1', valk=["Premiere levre"],
                valr=[Inst_Ecrevisse, min(_l_tang_t), max(_l_tang_t), min(_l_t2), max(_l_t2), min(_l_tang), max(_l_tang)])
            UTMESS('I', 'ECREVISSE0_1',valk=["Deuxieme levre"],
                    valr=[Inst_Ecrevisse,min(_l_tang_t_b), max(_l_tang_t_b), min(_l_t2_b), max(_l_t2_b), min(_l_tang_b), max(_l_tang_b)])
            UTMESS('I', 'ECREVISSE0_2',
                    valr=[Inst_Ecrevisse, min(_l_absz_t), max(_l_absz_t), min(_l_temp_aster), max(_l_temp_aster), min(_l_absz_m), max(_l_absz_m), min(_l_ouv), max(_l_ouv), min(_l_gli),max(_l_gli)])


        if ( debug ) :
            print "\n INFORMATIONS DE DEBUG: "
            print 'Inst_Ecrevisse=', Inst_Ecrevisse
            print 'theta:', theta
            print 'beta:', beta
            print 'DIR_FISS:', DIR_FISS
            print 'DIR_PREV:', DIR_PREV
            print 'point initial de la fissure: (xi,yi) :', _xi,_yi
            print len(_X0),   '_X0  =', _X0
            print len(_X0_b), '_X0_b=', _X0_b
            print len(_Y0),   '_Y0  =', _Y0
            print len(_Y0_b), '_Y0_b=', _Y0_b
            print len(_X),    '_X   =', _X
            print len(_Y),    '_Y   =', _Y
            print len(_X_b),  '_X_b =', _X_b
            print len(_Y_b),  '_Y_b =', _Y_b
            print 'Controle sur les abszisses curvilignes (mecaniques/thermiques) '
            print '_l_absz_m==_l_absz_t?', _l_absz_m==_l_absz_t
            print '_l_absz_m=', len(_l_absz_m),_l_absz_m
            print '_l_absz_t=', len(_l_absz_t),_l_absz_t
            print '_l_temp_aster=', len(_l_temp_aster),_l_temp_aster
            print '_l_ouv=', len(_l_ouv),_l_ouv
            print '_l_gli=', len(_l_gli),_l_gli
            print '_l_tang=', _l_tang
            print '_l_tang_b=', _l_tang


# ----------------------------------------------------------------------------
#       PREPARATION ET LANCEMENT D ECREVISSE

        # TESTS de non divergence, les messages sont assez explicites :
        # Si toutes les listes sont bien definies
        if len(_l_absz_m)*len(_l_ouv)*len(_l_absz_t)*len(_l_temp_aster) == 0:
            UTMESS('F','ECREVISSE0_3', valr=[Inst_Ecrevisse])
            __TAB = None
            break
        # Si les ouvertures ne sont pas trop faibles
        elif min(_l_ouv) < 1e-20:
            UTMESS('F','ECREVISSE0_4', valr=[Inst_Ecrevisse, min(_l_ouv)])
            __TAB = None
            break
        elif max(_l_t2) > 700:
            UTMESS('F','ECREVISSE0_5', valr=[Inst_Ecrevisse, max(_l_t2)])
            __TAB = None
            break
        elif max(_l_t2_b) > 700:
            UTMESS('F','ECREVISSE0_5', valr=[Inst_Ecrevisse, max(_l_t2_b )])
            __TAB = None
            break
        elif min(_l_t2) < 0:
            UTMESS('F','ECREVISSE0_6', valr=[Inst_Ecrevisse, min(_l_t2 )])
            __TAB = None
            break
        elif min(_l_t2_b) < 0:
            UTMESS('F','ECREVISSE0_6', valr=[Inst_Ecrevisse, min(_l_t2_b )])
            __TAB = None
            break
        elif abs( float(dECOULEMENT['PRES_ENTREE']) - float(dECOULEMENT['PRES_SORTIE']) )< 5:
            UTMESS('F','ECREVISSE0_7', valr=[Inst_Ecrevisse, abs( float(dECOULEMENT['PRES_ENTREE']) - float(dECOULEMENT['PRES_SORTIE']) ) ] )
            __TAB = None
            break
        # On lance Ecrevisse!
        else:
            UTMESS('I','ECREVISSE0_8', valr=[Inst_Ecrevisse])

            # On efface le rep
            try:
                for fic in os.listdir(os.path.join(os.getcwd(),tmp_ecrevisse)):
                    try:
                        os.remove( os.path.join(os.getcwd(),tmp_ecrevisse) + '/' + fic )
                    except:
                        pass
            except:
                pass

            # Recalcul des cotes par rapport a la tortuoiste
            tort = dFISSURE['TORTUOSITE']
            try :
                _l_absz_m = [x/tort for x in _l_absz_m]
                _l_absz_t = [x/tort for x in _l_absz_t]
            except ZeroDivisionError :
                UTMESS('F','ECREVISSE0_42')


            if dFISSURE.has_key('LISTE_COTES_BL'):
                __LISTE_COTES_BL = dFISSURE['LISTE_COTES_BL']
            else:
                __LISTE_COTES_BL = (0., max(_l_absz_m))

            # Mot-cle ECOULEMENT
            txt = {}
            txt = { 'PRES_ENTREE' : dECOULEMENT['PRES_ENTREE'],
                    'PRES_SORTIE' : dECOULEMENT['PRES_SORTIE'],
                    'FLUIDE_ENTREE'   : dECOULEMENT['FLUIDE_ENTREE'],
                    }
            if int(dECOULEMENT['FLUIDE_ENTREE']) in [1, 3, 4, 6]:
                txt['TEMP_ENTREE'] = dECOULEMENT['TEMP_ENTREE']
            if int(dECOULEMENT['FLUIDE_ENTREE']) in [2, 5]:
                txt['TITR_MASS'] = dECOULEMENT['TITR_MASS']
            if int(dECOULEMENT['FLUIDE_ENTREE']) in [4, 5]:
                txt['PRES_PART'] = dECOULEMENT['PRES_PART']


            # Traitement des cas ou les mots cles reynold, xminch, etc... ne doivent pas apparaitre
            # Mot-cle MODELE_ECRE
            txt2 = {}
            txt2['ECOULEMENT'] = dMODELE_ECRE['ECOULEMENT']
            if dMODELE_ECRE['ECOULEMENT'] == 'GELE' :
                txt2['PRESS_EBULLITION'] = dMODELE_ECRE['PRESS_EBULLITION']

            txt2['FROTTEMENT'] = dMODELE_ECRE['FROTTEMENT']

            if int(dMODELE_ECRE['FROTTEMENT']) in [-4,-3,-2,-1] :
                txt2['REYNOLDS_LIM'] = dMODELE_ECRE['REYNOLDS_LIM']
                txt2['FROTTEMENT_LIM'] = dMODELE_ECRE['FROTTEMENT_LIM']

            txt2['TRANSFERT_CHAL'] = dMODELE_ECRE['TRANSFERT_CHAL']
            if int(dMODELE_ECRE['TRANSFERT_CHAL']) in [-2,-1] :
                txt2['XMINCH'] = dMODELE_ECRE['XMINCH']
                txt2['XMAXCH'] = dMODELE_ECRE['XMAXCH']

            try :
                if dMODELE_ECRE['IVENAC'] in [0, 1]:
                    txt2['IVENAC'] = dMODELE_ECRE['IVENAC']
                else :
                    txt2['IVENAC'] = 0
            except :
                txt2['IVENAC'] = 0

            motscle2= {'ECOULEMENT': txt, 'MODELE_ECRE' : txt2 }

            DETRUIRE(OBJET=_F(CHAINE = '_TAB2' ),INFO=1,ALARME='NON')
            DETRUIRE(OBJET=_F(CHAINE = '_DEB2' ),INFO=1,ALARME='NON')
            __TAB_i = CO('_TAB2')
            __DEB_i = CO('_DEB2')

            MACR_ECRE_CALC(TABLE      = __TAB_i,
                           DEBIT      = __DEB_i,
                           ENTETE     = ENTETE,
                           COURBES    = COURBES,
                           IMPRESSION = IMPRESSION,
                           INFO       = INFO,
                           LOGICIEL   = LOGICIEL,
                           VERSION    = VERSION,
                           FISSURE    = _F(LONGUEUR        = max(_l_absz_m),
                                            ANGLE              = theta,
                                            RUGOSITE           = dFISSURE['RUGOSITE'],
                                            ZETA               = dFISSURE['ZETA'],
                                            SECTION            = dFISSURE['SECTION'],
                                            LISTE_COTES_AH     = _l_absz_m,
                                            LISTE_VAL_AH       = _l_ouv,
                                            LISTE_COTES_BL     = __LISTE_COTES_BL,
                                            LISTE_VAL_BL       = dFISSURE['LISTE_VAL_BL'],
                                            ),
                           TEMPERATURE=_F(GRADIENT       = 'FOURNI',
                                        LISTE_COTES_TEMP = _l_absz_t,
                                        LISTE_VAL_TEMP   = _l_temp_aster,
                                       ),
                           CONVERGENCE=_F(KGTEST         = dCONVERGENCE['KGTEST'],
                                          ITER_GLOB_MAXI = dCONVERGENCE['ITER_GLOB_MAXI'],
                                          CRIT_CONV_DEBI = dCONVERGENCE['CRIT_CONV_DEBI'],
                                       ),
                           **motscle2
                           );


#-------------------------------------------------------------
#           EXTRACTION DES RESULTATS D ECREVISSE



            # Creation de la table
            __TABFISS_i = __TAB_i.EXTR_TABLE()

            nb_lignes_table = len(__TABFISS_i["COTES"])
            # Re-definition des cotes utilisateur (on elimine l effet de la tortuosite)
            _lst_c   =  __TABFISS_i.COTES.values()
            _l_cotes = [x*tort for x in _lst_c]
            dictTab =  __TABFISS_i.dict_CREA_TABLE()['LISTE']

            __TABFISS_i = CREA_TABLE(LISTE = (
                            _F(PARA = "COTES",     LISTE_R = _l_cotes,),
                            _F(PARA = "FLUX",      LISTE_R = dictTab[1]['LISTE_R'],),
                            _F(PARA = "PRESSION",  LISTE_R = dictTab[2]['LISTE_R'],),
                            _F(PARA = "TEMP",      LISTE_R = dictTab[3]['LISTE_R'],),
                            _F(PARA = "COEF_CONV", LISTE_R = dictTab[4]['LISTE_R'],),
                            ),)

            DETRUIRE(OBJET=_F(CHAINE = '__TAB_i' ),INFO=1,ALARME='NON')
            DETRUIRE(OBJET=_F(CHAINE = '__DEB_i' ),INFO=1,ALARME='NON')

            if ( debug ):
               os.system('ls -al ' + os.path.join(os.getcwd(),tmp_ecrevisse) )

            if dFISSURE['PREFIXE_FICHIER'] :
                pref_fic = dFISSURE['PREFIXE_FICHIER']
            else :
                pref_fic = 'FISSURE'+str(k+1)

            ## Ecriture du fichier debits en fonction du temps:
            #try:
                ## on lit le fichier debit produit par ecrevisse
                #f_ast = open(os.path.join(tmp_ecrevisse, 'debits'),'r')
                #_txt = f_ast.read()
                #f_ast.close()
                #nomfic = str(pref_fic) + '_debits'
                ## on concatene
                #fw = open( os.getcwd() + os.sep + 'REPE_OUT' + os.sep + nomfic, 'a')
                #fw.write( str(Inst_Ecrevisse) + ' ' + _txt )
                #fw.close()
                ## On recopie le fichier debits pour reprise ulterieure
                #nomfic2 = 'debits_dernier'
                #fw = open( os.getcwd() + os.sep + 'REPE_OUT' + os.sep + nomfic2, 'w')
                #fw.write( _txt )
                #fw.close()
            #except Exception, e:
                #print e

            # CREATION DES COURBES:
            if COURBES != "AUCUNE":
                # Pour masquer certaines alarmes

                MasquerAlarme('TABLE0_6')

                nomfic = os.path.join( os.getcwd(), 'REPE_OUT', str(pref_fic) + '_flux'+ '_' + str(Inst_Ecrevisse))
                if not os.path.isfile(nomfic): acces='NEW'
                else: acces='APPEND'

                DEFI_FICHIER(ACTION='ASSOCIER', UNITE=55, TYPE='ASCII', ACCES=acces, FICHIER=nomfic)
                IMPR_TABLE(FORMAT='XMGRACE',
                          TABLE=__TABFISS_i,
                          UNITE=55,
                          PILOTE=COURBES,
                          NOM_PARA=('COTES','FLUX',),
                          TITRE='Flux de chaleur a l\'instant %s' % Inst_Ecrevisse,
                          LEGENDE_X='Abscisse curviligne (m)',
                          LEGENDE_Y='Flux (W/m2)',
                          COULEUR  = 1,
                          MARQUEUR = 1,
                          );
                DEFI_FICHIER(ACTION='LIBERER', UNITE=55)

                nomfic = os.path.join( os.getcwd(), 'REPE_OUT', str(pref_fic) + '_temperature'+ '_' + str(Inst_Ecrevisse))
                if not os.path.isfile(nomfic): acces='NEW'
                else: acces='APPEND'

                DEFI_FICHIER(ACTION='ASSOCIER', UNITE=55, TYPE='ASCII', ACCES=acces, FICHIER=nomfic)
                IMPR_TABLE(FORMAT='XMGRACE',
                          TABLE=__TABFISS_i,
                          UNITE=55,
                          PILOTE=COURBES,
                          NOM_PARA=('COTES','TEMP',),
                          TITRE='Temperature a l\'instant %s' % Inst_Ecrevisse,
                          LEGENDE_X='Abscisse curviligne (m)',
                          LEGENDE_Y='Temperature (degres C)',
                          COULEUR  = 2,
                          MARQUEUR = 2,
                          );
                DEFI_FICHIER(ACTION='LIBERER', UNITE=55)

                nomfic = os.path.join( os.getcwd(), 'REPE_OUT', str(pref_fic) + '_coeffconv'+ '_' + str(Inst_Ecrevisse))
                if not os.path.isfile(nomfic): acces='NEW'
                else: acces='APPEND'

                DEFI_FICHIER(ACTION='ASSOCIER', UNITE=55, TYPE='ASCII', ACCES=acces, FICHIER=nomfic)
                IMPR_TABLE(FORMAT='XMGRACE',
                          TABLE=__TABFISS_i,
                          UNITE=55,
                          PILOTE=COURBES,
                          NOM_PARA=('COTES','COEF_CONV',),
                          TITRE='Coefficient de convection a l\'instant %s' % Inst_Ecrevisse,
                          LEGENDE_X='Abscisse curviligne (m)',
                          LEGENDE_Y='Coefficient de convection (W/m2/K)',
                          COULEUR  = 3,
                          MARQUEUR = 3,
                          );
                DEFI_FICHIER(ACTION='LIBERER', UNITE=55)


                nomfic = os.path.join( os.getcwd(), 'REPE_OUT', str(pref_fic) + '_pression'+ '_' + str(Inst_Ecrevisse))
                if not os.path.isfile(nomfic): acces='NEW'
                else: acces='APPEND'

                DEFI_FICHIER(ACTION='ASSOCIER', UNITE=55, TYPE='ASCII', ACCES=acces, FICHIER=nomfic)
                IMPR_TABLE(FORMAT='XMGRACE',
                          TABLE=__TABFISS_i,
                          UNITE=55,
                          PILOTE=COURBES,
                          NOM_PARA=('COTES','PRESSION',),
                          TITRE='Pression a l\'instant %s' % Inst_Ecrevisse,
                          LEGENDE_X='Abscisse curviligne (m)',
                          LEGENDE_Y='Pression (Pa)',
                          COULEUR  = 4,
                          MARQUEUR = 4,
                          );
                DEFI_FICHIER(ACTION='LIBERER', UNITE=55)

                # Pour la gestion des alarmes
                RetablirAlarme('TABLE0_6')



            # On recopie dans REPE_OUT les fichiers resultats d'Ecrevisse
            tmp_ecrevisse_absolu = os.path.join(os.getcwd(),tmp_ecrevisse)
            repe_out_absolu = os.path.join(os.getcwd(),'REPE_OUT')
            for file in os.listdir(tmp_ecrevisse_absolu):
                if not file in ['ecrevisse', 'ecrevisse.sh']:
                    old_file = os.path.join(tmp_ecrevisse_absolu, file)
                    new_file = os.path.join(repe_out_absolu, str(pref_fic) + '_' + file + '_' + str(Inst_Ecrevisse))
                    shutil.copy(old_file, new_file)

            # Creation de la table resultat sur toutes les fissures :
            #  Nom de la fissure
            nom_fiss = dFISSURE['GROUP_MA'][0] + "-" + dFISSURE['GROUP_MA'][1]
            __TABFISS_i = __TABFISS_i.EXTR_TABLE()
            __DEBFISS_i = __DEB_i.EXTR_TABLE()
            __TABFISS_i["FISSURE"] = [nom_fiss]*nb_lignes_table
            __DEBFISS_i["FISSURE"] = [nom_fiss]

            if k==0 :
                __TABFISS_tot = __TABFISS_i
                __DEBFISS_tot = __DEBFISS_i
            else :
                __TABFISS_tot = merge(__TABFISS_tot, __TABFISS_i)
                __DEBFISS_tot = merge(__DEBFISS_tot, __DEBFISS_i)


            if ( debug ):
                os.system('ls -al ' + os.path.join(os.getcwd(),tmp_ecrevisse) )

#--------------------------------------------------------------
#           CREATIONS DES CHARGEMENTS ASTER

            # Recuperation des valeurs dans la table (voir si il y a plus simple)
            _lst_c  = __TABFISS_i.COTES.values()
            _lst_f  = __TABFISS_i.FLUX.values()
            _lst_p  = __TABFISS_i.PRESSION.values()
            _lst_t  = __TABFISS_i.TEMP.values()
            _lst_cc = __TABFISS_i.COEF_CONV.values()


            try:
                a=len(_lst_c)
            except:
                _lst_c = []
                _lst_f = []
                _lst_p = []
                _lst_t = []
                _lst_cc = []
            try:
                if _lst_c[1]==0:
                    _lst_c = []
                    _lst_f = []
                    _lst_p = []
                    _lst_t = []
                    _lst_cc = []
            except:
                pass



            # ------------------------------------------------------
            # Extraction des conditions limites du calcul Ecrevisse
            #
            if len(_lst_c)>=2:
                if not chargeLin :
                    nbCotes     = len(_l_absz_m)
                    nbCotesEcre = nbCotes-1

                    # epsilon pour le decalage :
                    #   le chargement est uniforme par maille
                    if _X0[1]>=_X0[0] :
                        epsx = 1.e-8
                    else :
                        epsx = -1.e-8
                    if _Y0[1]>=_Y0[0] :
                        epsy = 1.e-8
                    else :
                        epsy = -1.e-8

                    _lst_x0   = []
                    _lst_y0   = []
                    _lst_x0_b = []
                    _lst_y0_b = []
                    ly        = []
                    ly2       = []
                    ly3       = []
                    ly4       = []

                    _lst_x0.append(_X0[0])
                    _lst_y0.append(_Y0[0])
                    _lst_x0_b.append(_X0_b[0])
                    _lst_y0_b.append(_Y0_b[0])
                    ly.append( _lst_f[0] )
                    ly2.append( _lst_p[0] )
                    ly3.append( _lst_t[0] )
                    ly4.append( _lst_cc[0] )

                    for i in range(nbCotes-2):
                        x = _X0[i+1]
                        y = _Y0[i+1]
                        toto1 =  x - epsx
                        toto2 =  x + epsx

                        _lst_x0.append( x - epsx )
                        _lst_x0.append( x + epsx )
                        _lst_y0.append( y - epsy )
                        _lst_y0.append( y + epsy )
                        x = _X0_b[i+1]
                        y = _Y0_b[i+1]
                        _lst_x0_b.append( x - epsx )
                        _lst_x0_b.append( x + epsx )
                        _lst_y0_b.append( y - epsy )
                        _lst_y0_b.append( y + epsy )
                        ly.append( _lst_f[i] )
                        ly.append( _lst_f[i+1] )
                        ly2.append( _lst_p[i] )
                        ly2.append( _lst_p[i+1] )
                        ly3.append( _lst_t[i] )
                        ly3.append( _lst_t[i+1] )
                        ly4.append( _lst_cc[i] )
                        ly4.append( _lst_cc[i+1] )

                    _lst_x0.append( _X0[nbCotes-1] )
                    _lst_y0.append( _Y0[nbCotes-1] )
                    _lst_x0_b.append( _X0_b[nbCotes-1] )
                    _lst_y0_b.append( _Y0_b[nbCotes-1] )
                    ly.append( _lst_f[nbCotesEcre-1] )
                    ly2.append( _lst_p[nbCotesEcre-1] )
                    ly3.append( _lst_t[nbCotesEcre-1] )
                    ly4.append( _lst_cc[nbCotesEcre-1] )
                    _lst_f = ly
                    _lst_p = ly2
                    _lst_t = ly3
                    _lst_cc = ly4
                else :
                    _lst_x0   = [(x1+x2)*0.5 for (x1,x2) in zip(_X0[0:len(_X0)-1],_X0[1:len(_X0)])]
                    _lst_y0   = [(x1+x2)*0.5 for (x1,x2) in zip(_Y0[0:len(_Y0)-1],_Y0[1:len(_Y0)])]
                    _lst_x0_b = [(x1+x2)*0.5 for (x1,x2) in zip(_X0_b[0:len(_X0_b)-1],_X0_b[1:len(_X0_b)])]
                    _lst_y0_b = [(x1+x2)*0.5 for (x1,x2) in zip(_Y0_b[0:len(_Y0_b)-1],_Y0_b[1:len(_Y0_b)])]
                    _lst_x0.append(_X0[-1])
                    _lst_x0_b.append(_X0_b[-1])
                    _lst_y0.append(_Y0[-1])
                    _lst_y0_b.append(_Y0_b[-1])

                # ANCIENNE VERSION (TRANSMISSION DES FLUX THERMIQUES
                if(oldVersion) :
                    alpha = round(theta)
                    if DIR_FISS == 'X' :
                        levre1pos = ((_Y0[0]-_yi)>=0.)
                        if alpha == -90. :
                            _lst_x0.reverse()
                            _lst_p.reverse()
                            _lst_f.reverse()
                        _lst_dir = _lst_x0
                    else :
                        levre1pos = ((_X0[0]-_xi)>=0.)
                        if abs(alpha) == 180. :
                            _lst_y0.reverse()
                            _lst_p.reverse()
                            _lst_f.reverse()
                        _lst_dir = _lst_y0

                    # Creation des listes pour les chargements aster :
                    #     (x1, y1, x2, y2, ...)
                    _tmp1=[]
                    _tmp2=[]
                    _tmp3=[]

                    for i in range(len(_lst_p)) :
                        _tmp1.append( _lst_dir[i] )
                        _tmp2.append( _lst_dir[i] )
                        _tmp3.append( _lst_dir[i] )

                        _tmp2.append( _lst_p[i] )
                        if levre1pos :
                            #_tmp1.append( -1*_lst_f[i] )
                            #_tmp3.append( _lst_f[i] )
                            _tmp1.append( _lst_f[i] )
                            _tmp3.append( -1*_lst_f[i] )
                        else :
                            #_tmp1.append( _lst_f[i] )
                            #_tmp3.append( -1*_lst_f[i] )
                            _tmp1.append( -1*_lst_f[i] )
                            _tmp3.append( _lst_f[i] )



                    # Flux en provenance d'Ecrevisse
                    _L_F1=DEFI_FONCTION(NOM_PARA=DIR_FISS,
                                        VALE=_tmp1,
                                        PROL_GAUCHE='LINEAIRE',
                                        PROL_DROITE='LINEAIRE');

                    _L_F2=DEFI_FONCTION(NOM_PARA=DIR_FISS,
                                        VALE=_tmp3,
                                        PROL_GAUCHE='LINEAIRE',
                                        PROL_DROITE='LINEAIRE' );

                    if DIR_FISS == 'X':
                        l_FLUX_REP_F1.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][0],
                                                                FLUX_Y=_L_F1,))
                        l_FLUX_REP_F2.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][1],
                                                                FLUX_Y=_L_F2,))
                    else :
                        l_FLUX_REP_F1.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][0],
                                                                FLUX_X=_L_F1,))
                        l_FLUX_REP_F2.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][1],
                                                                FLUX_X=_L_F2,))

                    # Pressions en provenance d'Ecrevisse
                    _L_P=DEFI_FONCTION(NOM_PARA = DIR_FISS,
                                        VALE    = _tmp2,
                                        PROL_GAUCHE = 'LINEAIRE',
                                        PROL_DROITE = 'LINEAIRE');

                    l_PRES_REP.append(_F(GROUP_MA=(dFISSURE['GROUP_MA'][0],dFISSURE['GROUP_MA'][1]),
                                                            PRES=_L_P,))



                # NOUVELLE VERSION
                else :
                    # Creation des deux listes (x1, y1, x2, y2, ...)
                    # On cree trois/six listes :
                    # Les valeurs sont constant par maille sur les levres de la fissure,
                    #  _tmp1/_tmp2 = temperature
                    #  _tmp3/_tmp4 = coefficient d echange
                    #  _tmp5/_tmp6 = pression
                    _tmp1=[]
                    _tmp2=[]
                    _tmp3=[]
                    _tmp4=[]
                    _tmp5=[]
                    _tmp6=[]

                    for i in range(len(_lst_f)) :
                        ix = copy(i)
                        iy = copy(i)
                        if _X0[1]<_X0[0] :
                            ix = len(_lst_f)-1-i
                        if _Y0[1]<_Y0[0] :
                            iy = len(_lst_f)-1-i

                        if (DIR_PREV=='X'):
                            _tmp1.append( _lst_x0[ix] )
                            _tmp1.append( _lst_t[ix] )
                            _tmp3.append( _lst_x0[ix] )
                            _tmp3.append( _lst_cc[ix] )
                            _tmp5.append( _lst_x0[ix] )
                            _tmp5.append( _lst_p[ix] )
                            _tmp2.append( _lst_x0_b[ix] )
                            _tmp2.append( _lst_t[ix] )
                            _tmp4.append( _lst_x0_b[ix] )
                            _tmp4.append( _lst_cc[ix] )
                            _tmp6.append( _lst_x0_b[ix] )
                            _tmp6.append( _lst_p[ix] )
                        else :
                            _tmp1.append( _lst_y0[iy] )
                            _tmp1.append( _lst_t[iy] )
                            _tmp3.append( _lst_y0[iy] )
                            _tmp3.append( _lst_cc[iy] )
                            _tmp5.append( _lst_y0[iy] )
                            _tmp5.append( _lst_p[iy] )
                            _tmp2.append( _lst_y0_b[iy] )
                            _tmp2.append( _lst_t[iy] )
                            _tmp4.append( _lst_y0_b[iy] )
                            _tmp4.append( _lst_cc[iy] )
                            _tmp6.append( _lst_y0_b[iy] )
                            _tmp6.append( _lst_p[iy] )





                    # Couplage thermique : Temperature et coefficients d'echange en provenance d'Ecrevisse
                    _L_T1=DEFI_FONCTION(NOM_PARA=DIR_PREV,
                                        VALE=_tmp1,
                                        PROL_GAUCHE='LINEAIRE',
                                        PROL_DROITE='LINEAIRE')

                    _L_T2=DEFI_FONCTION(NOM_PARA=DIR_PREV,
                                        VALE=_tmp2,
                                        PROL_GAUCHE='LINEAIRE',
                                        PROL_DROITE='LINEAIRE')

                    _L_CC1=DEFI_FONCTION(NOM_PARA=DIR_PREV,
                                        VALE=_tmp3,
                                        PROL_GAUCHE='LINEAIRE',
                                        PROL_DROITE='LINEAIRE')

                    _L_CC2=DEFI_FONCTION(NOM_PARA=DIR_PREV,
                                        VALE=_tmp4,
                                        PROL_GAUCHE='LINEAIRE',
                                        PROL_DROITE='LINEAIRE')

                    l_ECHANGE_F1.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][0],
                                                            TEMP_EXT=_L_T1,
                                                            COEF_H=_L_CC2))

                    l_ECHANGE_F2.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][1],
                                                            TEMP_EXT=_L_T2,
                                                            COEF_H=_L_CC2))

                    # Couplage mecanique : Pressions en provenance d'Ecrevisse
                    _L_P1=DEFI_FONCTION(NOM_PARA=DIR_PREV,
                                        VALE=_tmp5,
                                        PROL_GAUCHE='LINEAIRE',
                                        PROL_DROITE='LINEAIRE')

                    _L_P2=DEFI_FONCTION(NOM_PARA=DIR_PREV,
                                        VALE=_tmp6,
                                        PROL_GAUCHE='LINEAIRE',
                                        PROL_DROITE='LINEAIRE')

                    l_PRES_REP.append(_F(GROUP_MA=(dFISSURE['GROUP_MA'][0]),
                                                            PRES=_L_P1,))
                    l_PRES_REP.append(_F(GROUP_MA=(dFISSURE['GROUP_MA'][1]),
                                                            PRES=_L_P2,))

            # Fin extraction des conditions limites du calcul Ecrevisse
            # ----------------------------------------------------------

        if debug :
            print ('FISSURE-'+str(k+1))
            print  '_lst_c:', len(_lst_c),        _lst_c
            print  '_lst_f:', len(_lst_f),        _lst_f
            print  '_lst_p:', len(_lst_p),        _lst_p
            print  '_lst_t:',  len(_lst_t),       _lst_t
            print  '_lst_cc:', len(_lst_cc),      _lst_cc
            print  '_lst_x0:', len(_lst_x0),      _lst_x0
            print  '_lst_x0_b :', len(_lst_x0_b), _lst_x0_b
            print  '_lst_y0:', len(_lst_y0),      _lst_y0
            print  '_lst_y0_b :', len(_lst_y0_b), _lst_y0_b
            print  '_tmp1 :', len(_tmp1),     _tmp1
            print  '_tmp2 :', len(_tmp2),     _tmp2
            print  '_tmp3 :', len(_tmp3),     _tmp3
            if (not oldVersion) :
                print '_tmp4 :', len(_tmp4),  _tmp4
                print '_tmp5 :', len(_tmp5),  _tmp5
                print '_tmp6 :', len(_tmp6),  _tmp6

        #Fin du boucle sur la fissure for k


    # Assemblage des concepts sortants
    if(oldVersion) :
        __ECR_F1=AFFE_CHAR_THER_F(MODELE=MODELE_THER,
                                  FLUX_REP=l_FLUX_REP_F1);

        __ECR_F2=AFFE_CHAR_THER_F(MODELE=MODELE_THER,
                                  FLUX_REP=l_FLUX_REP_F2);
    else:
        __ECR_F1=AFFE_CHAR_THER_F(MODELE=MODELE_THER,
                                  ECHANGE=l_ECHANGE_F1);

        __ECR_F2=AFFE_CHAR_THER_F(MODELE=MODELE_THER,
                                  ECHANGE=l_ECHANGE_F2);

    __ECR_P=AFFE_CHAR_MECA_F(MODELE=MODELE_MECA,
                             PRES_REP=l_PRES_REP);

    # Table resultat
    try:
       dprod = __TABFISS_tot.dict_CREA_TABLE()
       __TAB = CREA_TABLE(**dprod)
       debprod = __DEBFISS_tot.dict_CREA_TABLE()
       __DEB = CREA_TABLE(**debprod)
    except:
       UTMESS('F','ECREVISSE0_9', valr=[Inst_Ecrevisse])
