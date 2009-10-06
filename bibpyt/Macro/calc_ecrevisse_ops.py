#@ MODIF calc_ecrevisse_ops Macro  DATE 05/10/2009   AUTEUR ASSIRE A.ASSIRE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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

# Debut de la macro, on impose en parametre les donnees placer dans T_EC, l'appel a ecrevisse

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
    from Utilitai.Utmess import UTMESS
    from Utilitai.Table import Table, merge
    from math import atan, pi, sqrt

    ier=0

    # Concepts sortant
    self.DeclareOut('__TAB',TABLE)
    self.DeclareOut('__ECR_F1',CHARGE_THER1)
    self.DeclareOut('__ECR_F2',CHARGE_THER2)
    self.DeclareOut('__ECR_P',CHARGE_MECA)
    self.DeclareOut('__DEB',DEBIT)


    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Parametres Developpeur
    tmp_ecrevisse = "tmp_ecrevisse"
    fichier_data = "data.dat"
    defaut = '00'
    # Niveaux de debug
    debug1 = (INFO>1)
    debug2 = (INFO>2)
    #
    InfoAster = 1
    #
    oldVersion = False  # Permet de retourner un chargement thermique defini selon l'ancien mode (flux thermique)

    # IMPORTATION DE COMMANDES ASTER
    DEFI_GROUP       = self.get_cmd("DEFI_GROUP")
    POST_RELEVE_T    = self.get_cmd("POST_RELEVE_T")
    MACR_ECRE_CALC   = self.get_cmd("MACR_ECRE_CALC")
    IMPR_TABLE       = self.get_cmd("IMPR_TABLE")
    DETRUIRE         = self.get_cmd("DETRUIRE")
    IMPR_CO          = self.get_cmd("IMPR_CO")
    DEFI_FONCTION    = self.get_cmd("DEFI_FONCTION")
    CREA_TABLE       = self.get_cmd("CREA_TABLE")
    CO               = self.get_cmd("CO")
    AFFE_CHAR_THER_F = self.get_cmd("AFFE_CHAR_THER_F")
    AFFE_CHAR_MECA_F = self.get_cmd("AFFE_CHAR_MECA_F")
    DEFI_FICHIER     = self.get_cmd("DEFI_FICHIER")

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

    # Instants
    _l_inst = dRESULTAT['MECANIQUE'].LIST_VARI_ACCES()
    if dRESULTAT.has_key('INST'):
        Inst_Ecrevisse = dRESULTAT['INST']
    else:
        pass

    # ancienne version
    # liste des mots cles facteurs de FLUX_REP pour le flux F1 de chaque fissure (sur GROUP_MA[0])
    l_FLUX_REP_F1 = []
    # liste des mots cles facteurs de PRES_REP pour le flux F2 de  chaque fissure (sur GROUP_MA[1])
    l_FLUX_REP_F2 = []
    
    # nouvelle version
    # liste des mots cles facteurs de ECHANGE pour le flux F1 de chaque fissure (sur GROUP_MA[0])
    l_ECHANGE_F1 = []
    # liste des mots cles facteurs de ECHANGE pour le flux F2 de chaque fissure (sur GROUP_MA[1])
    l_ECHANGE_F2 = []
    
    # Toutes versions
    # liste des mots cles facteurs de PRES_REP pour chaque fissure
    l_PRES_REP = []


    # Liste des noms des groupes de noeuds du maillage
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
        if len(dFISSURE['GROUP_MA'][0]) >7:
            sys.exit(1)

        if len(dFISSURE['GROUP_MA'])>1 and len(dFISSURE['GROUP_MA'][1]) >7:
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
                                        PRECISION=0.001,
                                        CRITERE='ABSOLU',),
                       INFO=InfoAster,);


        if len(dFISSURE['GROUP_MA'])>1:
            _nom_gno_2 = '_' + dFISSURE['GROUP_MA'][1]
            if not _nom_gno_2 in _lgno:
                DEFI_GROUP(reuse = MODELE_MECA['MAILLAGE'],
                           MAILLAGE=MODELE_MECA['MAILLAGE'],
                           CREA_GROUP_NO=_F(OPTION='SEGM_DROI_ORDO',
                                            NOM=_nom_gno_2,
                                            GROUP_NO=dFISSURE['GROUP_MA'][1],
                                            GROUP_NO_ORIG=dFISSURE['GROUP_NO_ORIG'][1],
                                            GROUP_NO_EXTR=dFISSURE['GROUP_NO_EXTR'][1],
                                            PRECISION=0.001,
                                            CRITERE='ABSOLU',),
                           INFO=InfoAster,);

        _T_DY=POST_RELEVE_T(ACTION=_F(INTITULE='DEPL_FIS',
                                      GROUP_NO=_nom_gno_1,
                                      RESULTAT=dRESULTAT['MECANIQUE'],
                                      NOM_CHAM='DEPL',
                                      NOM_CMP=('DX','DY',),
                                      INST = Inst_Ecrevisse,
                                      OPERATION='EXTRACTION',),
                                      );

        if len(dFISSURE['GROUP_MA'])>1:
            # signifie que l'on prend en compte la deuxieme levre de la fissure, different de l'essai sur tranche ou une fissure est
            # represente par une seule levre et l'autre est fixe
            _T_DY_B=POST_RELEVE_T(ACTION=_F(INTITULE='DX_FIS',
                                            GROUP_NO=_nom_gno_2,
                                            RESULTAT=dRESULTAT['MECANIQUE'],
                                            NOM_CHAM='DEPL',
                                            NOM_CMP=('DX','DY',),
                                            INST = Inst_Ecrevisse,
                                            OPERATION='EXTRACTION',),);

        _T_TEMP=POST_RELEVE_T(ACTION=_F(INTITULE='TEMP',
                                        GROUP_NO=_nom_gno_1,
                                        RESULTAT=dRESULTAT['THERMIQUE'],
                                        NOM_CHAM='TEMP',
                                        TOUT_CMP='OUI',
                                        INST = Inst_Ecrevisse,
                                        OPERATION='EXTRACTION',),);

        if len(dFISSURE['GROUP_MA'])>1:
            _T_TEMPB=POST_RELEVE_T(ACTION=_F(INTITULE='TEMP',
                                             GROUP_NO=_nom_gno_2,
                                             RESULTAT=dRESULTAT['THERMIQUE'],
                                             NOM_CHAM='TEMP',
                                             TOUT_CMP='OUI',
                                             INST = Inst_Ecrevisse,
                                             OPERATION='EXTRACTION',),);

        if ( debug2 ) :
            print '_T_DY ==================================================='
            print  _T_DY.EXTR_TABLE()
            print '_T_TEMP ================================================='
            print  _T_TEMP.EXTR_TABLE()


        # Extraction des tables Temperatures + deplacement levres fissure
        _tbl_temp = _T_TEMP.EXTR_TABLE()
        _tbl_dy   = _T_DY.EXTR_TABLE()
        DETRUIRE(CONCEPT=_F(NOM=(_T_DY,_T_TEMP,)),INFO=1,ALARME='NON')
        
        # Determination de la direction de la fissure : limitee aux cas horizontal ou vertical a cause
        # de la creation des chargement pour aster
        
        _xih = _tbl_dy.values()['COOR_X'][0] 
        _yih = _tbl_dy.values()['COOR_Y'][0] 
        _xeh = _tbl_dy.values()['COOR_X'][-1] 
        _yeh = _tbl_dy.values()['COOR_Y'][-1]    

        if abs(_xeh -_xih)<1.E-6 : # fissure verticale

           if (_yeh-_yih)<0 : # fluide du haut vers le bas
              alpha = 180.  
           else :
              alpha =0.   # fluide du bas vers le haut
           DIR_FISS='Y'
           NORM_FISS='X'
        elif abs(_yih - _yeh)<1.E-6 : # fissure horizontale
           alpha = 90.
           DIR_FISS ='X'
           NORM_FISS = 'Y'
        else:
           UTMESS('F','ECREVISSE0_23')
        
#        Determination de l'ouverture de la fissure

        _l_dy1 = _tbl_dy.values()['ABSC_CURV']
        _l_dy2 = [ x + y for (x,y) in zip(_tbl_dy.values()['D%s' % NORM_FISS], _tbl_dy.values()['COOR_%s' % NORM_FISS]) ]

        # Listes des cotes et ouvertures (deuxieme groupe)
        if len(dFISSURE['GROUP_MA'])>1:
            # signifie que l'on prend en compte la deuxieme levre de la fissure,
            _tbl_dy_b  = _T_DY_B.EXTR_TABLE()
            _tbl_temp_b = _T_TEMPB.EXTR_TABLE()
            DETRUIRE(CONCEPT=_F(NOM=(_T_DY_B,_T_TEMPB,)),INFO=1,ALARME='NON')
            
            _l_dy1_b = _tbl_dy_b.values()['ABSC_CURV']
            _l_dy2_b = [ x + y for (x,y) in zip(_tbl_dy_b.values()['D%s' % NORM_FISS], _tbl_dy_b.values()['COOR_%s' % NORM_FISS]) ]

        # Listes des cotes et temperatures (premier groupe)
        _l_t1  = _tbl_temp.values()['ABSC_CURV']
        _l_t2  = _tbl_temp.values()['TEMP']
        
        # Listes des cotes et temperatures (deuxieme groupe)
        if len(dFISSURE['GROUP_MA'])>1:
            # signifie que l'on prend en compte la deuxieme levre de la fissure, different de l'essai sur tranche ou une fissure est
            # represente par une seule levre et l'autre est fixe

            _l_t1_b  = _tbl_temp_b.values()['ABSC_CURV']
            _l_t2_b  = _tbl_temp_b.values()['TEMP']


        if ( debug2 ):
            print "Informations de debug:\n"
            print 'Inst_Ecrevisse=', Inst_Ecrevisse
            print '_l_t1=', len(_l_t1),_l_t1
            print '_l_t2=', len(_l_t2),_l_t2
            print '_l_dy1=', len(_l_dy1),_l_dy1
            print '_l_dy2=', len(_l_dy2),_l_dy2

            if len(dFISSURE['GROUP_MA'])>1:
                print '_l_t1_b=', len(_l_t1_b),_l_t1_b
                print '_l_t2_b=', len(_l_t2_b),_l_t2_b
                print '_l_dy1_b=', len(_l_dy1_b),_l_dy1_b
                print '_l_dy2_b=', len(_l_dy2_b),_l_dy2_b


        # Affichage d'informations que l'on peut voir sur le fichier .mess ou .resu

        if (debug1):
           UTMESS('I', 'ECREVISSE0_1', valk=["Premiere levre"],
                valr=[Inst_Ecrevisse, min(_l_t1), max(_l_t1), min(_l_t2), max(_l_t2), min(_l_dy1), max(_l_dy1), min(_l_dy2), max(_l_dy2)])

           if len(dFISSURE['GROUP_MA'])>1:
               UTMESS('I', 'ECREVISSE0_1',valk=["Deuxieme levre"],
                    valr=[Inst_Ecrevisse,min(_l_t1_b), max(_l_t1_b), min(_l_t2_b), max(_l_t2_b), min(_l_dy1_b), max(_l_dy1_b), min(_l_dy2_b), max(_l_dy2_b)])


        # ----------------------------------------------------------------------------------
        # Partie concernant la prise en compte des deux levres
        if len(dFISSURE['GROUP_MA'])>1:

            # Calcul de l'ouverture entre les deux levres
            _l_dy2 = map(lambda x,y: abs(y-x), _l_dy2, _l_dy2_b)

            # Pour la temperature, on fait la moyenne des temperatures des deux levres (points a ameliorer peut etre par un autre modele)
            _l_t2  = map(lambda x,y: 0.5*(x+y), _l_t2_b,_l_t2)

        # ----------------------------------------------------------------------------------
        
        if dFISSURE['OUVERT_REMANENTE']:
            # une fissure refermee a une ouverture egale a l'ouverture remanente
            _l_dy2 = map(lambda x: max(dFISSURE['OUVERT_REMANENTE'],x), _l_dy2)
        
        UTMESS('I', 'ECREVISSE0_2',
                valr=[Inst_Ecrevisse, min(_l_t1), max(_l_t1), min(_l_t2), max(_l_t2), min(_l_dy1), max(_l_dy1), min(_l_dy2), max(_l_dy2)])


        if ( debug2 ):
            print "Informations de debug:\n"
            print 'Inst_Ecrevisse=', Inst_Ecrevisse
            print '_l_t1=', len(_l_t1),_l_t1
            print '_l_t2=', len(_l_t2),_l_t2
            print '_l_dy1=', len(_l_dy1),_l_dy1
            print '_l_dy2=', len(_l_dy2),_l_dy2

        # Test de non divergence, les messages sont assez explicites

        # Si toutes les listes sont bien definies
        if len(_l_dy1)*len(_l_dy2)*len(_l_t1)*len(_l_t2) == 0:
            UTMESS('F','ECREVISSE0_3', valr=[Inst_Ecrevisse])
            __TAB = None
            break

        # Si les ouvertures ne sont pas trop faibles
        elif min(_l_dy2) < 1e-20:
            UTMESS('F','ECREVISSE0_4', valr=[Inst_Ecrevisse, min(_l_dy2)])
            __TAB = None
            break

        elif max(_l_t2) > 700:
            UTMESS('F','ECREVISSE0_5', valr=[Inst_Ecrevisse, max(_l_t2 )])
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

        # On lance Ecrevisse
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

            if dFISSURE.has_key('LISTE_COTES_BL'):
                __LISTE_COTES_BL = dFISSURE['LISTE_COTES_BL']
            else:
                __LISTE_COTES_BL = (0., max(_l_dy1))

            # Mot-cle ECOULEMENT
            txt={}
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

            MACR_ECRE_CALC(TABLE = __TAB_i,
                           DEBIT = __DEB_i,
                           ENTETE=ENTETE,
                           COURBES=COURBES,
                           IMPRESSION=IMPRESSION,
                           INFO=INFO,
                           LOGICIEL=LOGICIEL,
                           VERSION=VERSION,
                           FISSURE=_F(LONGUEUR           = max(_l_dy1)/dFISSURE['TORTUOSITE'],
                                   ANGLE              = alpha,
                                   RUGOSITE           = dFISSURE['RUGOSITE'],
                                   ZETA               = dFISSURE['ZETA'],
                                   SECTION            = dFISSURE['SECTION'],
                                   LISTE_COTES_AH     = _l_dy1,
                                   LISTE_VAL_AH       = _l_dy2,
                                   LISTE_COTES_BL     = __LISTE_COTES_BL,
                                   LISTE_VAL_BL       = dFISSURE['LISTE_VAL_BL'],
                                   ),
                           TEMPERATURE=_F(GRADIENT       = 'FOURNI',
                                       LISTE_COTES_TEMP = _l_t1,
                                       LISTE_VAL_TEMP   = _l_t2,
                                       ),
                           CONVERGENCE=_F(KGTEST         = dCONVERGENCE['KGTEST'],
                                       ITER_GLOB_MAXI = dCONVERGENCE['ITER_GLOB_MAXI'],
                                       CRIT_CONV_DEBI = dCONVERGENCE['CRIT_CONV_DEBI'],
                                       ),
                           **motscle2
                           );
            
            # Creation de la table resultat sur toutes les fissures:

            # Nom de la fissure
            nom_fiss = dFISSURE['GROUP_MA'][0] + "-" + dFISSURE['GROUP_MA'][1]
            # Creation de la table
            __TABFISS_i = __TAB_i.EXTR_TABLE()
            __DEBFISS_i = __DEB_i.EXTR_TABLE()
            # Taille de la table
            nb_lignes_table = len(__TABFISS_i["COTES"])
            # Ajout d'une colonne supplementaire
            __TABFISS_i["FISSURE"] = [nom_fiss] * nb_lignes_table
            __DEBFISS_i["FISSURE"] = [nom_fiss]
            if k==0:
                __TABFISS_tot = __TABFISS_i
                __DEBFISS_tot = __DEBFISS_i
            else:
                __TABFISS_tot = merge(__TABFISS_tot, __TABFISS_i)
                __DEBFISS_tot = merge(__DEBFISS_tot, __DEBFISS_i)
            
            
            DETRUIRE(OBJET=_F(CHAINE = '__TAB_i' ),INFO=1,ALARME='NON')
            DETRUIRE(OBJET=_F(CHAINE = '__DEB_i' ),INFO=1,ALARME='NON')

            if ( debug2 ):
               os.system('ls -al ' + os.path.join(os.getcwd(),tmp_ecrevisse) )

            pref_fic = dFISSURE['PREFIXE_FICHIER']

            # Ecriture du fichier debits en fonction du temps:
            try:
                # on lit le fichier debit produit par ecrevisse
                f_ast = open(os.path.join(tmp_ecrevisse, 'debits'),'r')
                _txt = f_ast.read()
                f_ast.close()
                nomfic = str(pref_fic) + '_debits'
                # on concatene
                fw = open( os.getcwd() + os.sep + 'REPE_OUT' + os.sep + nomfic, 'a')
                fw.write( str(Inst_Ecrevisse) + ' ' + _txt )
                fw.close()
                # On recopie le fichier debits pour reprise ulterieure
                nomfic2 = 'debits_dernier'
                fw = open( os.getcwd() + os.sep + 'REPE_OUT' + os.sep + nomfic2, 'w')
                fw.write( _txt )
                fw.close()
            except Exception, e:
                print e


            if COURBES != "AUCUNE":
              # Pour masquer certaines alarmes
              from Utilitai.Utmess import MasquerAlarme, RetablirAlarme
              MasquerAlarme('TABLE0_6')
 
              nomfic = os.path.join( os.getcwd(), 'REPE_OUT', str(pref_fic) + '_flux')
              if not os.path.isfile(nomfic): acces='NEW'
              else: acces='APPEND'

              DEFI_FICHIER(ACTION='ASSOCIER', UNITE=55, TYPE='ASCII', ACCES=acces, FICHIER=nomfic)
              IMPR_TABLE(FORMAT='XMGRACE',
                         TABLE=__TAB_i,
                         UNITE=55,
                         PILOTE=COURBES,
                         NOM_PARA=('COTES','FLUX',),
                         TITRE='Flux de chaleur a l\'instant %s' % Inst_Ecrevisse,
                         LEGENDE_X='Abscisse curviligne (m)',
                         LEGENDE_Y='Flux (W/m2)',
                        );
              DEFI_FICHIER(ACTION='LIBERER', UNITE=55)

              nomfic = os.path.join( os.getcwd(), 'REPE_OUT', str(pref_fic) + '_temperature')
              if not os.path.isfile(nomfic): acces='NEW'
              else: acces='APPEND'

              DEFI_FICHIER(ACTION='ASSOCIER', UNITE=55, TYPE='ASCII', ACCES=acces, FICHIER=nomfic)
              IMPR_TABLE(FORMAT='XMGRACE',
                         TABLE=__TAB_i,
                         UNITE=55,
                         PILOTE=COURBES,
                         NOM_PARA=('COTES','TEMP',),
                         TITRE='Temperature a l\'instant %s' % Inst_Ecrevisse,
                         LEGENDE_X='Abscisse curviligne (m)',
                         LEGENDE_Y='Temperature (degres C)',
                        );
              DEFI_FICHIER(ACTION='LIBERER', UNITE=55)

              nomfic = os.path.join( os.getcwd(), 'REPE_OUT', str(pref_fic) + '_coeffconv')
              if not os.path.isfile(nomfic): acces='NEW'
              else: acces='APPEND'

              DEFI_FICHIER(ACTION='ASSOCIER', UNITE=55, TYPE='ASCII', ACCES=acces, FICHIER=nomfic)
              IMPR_TABLE(FORMAT='XMGRACE',
                         TABLE=__TAB_i,
                         UNITE=55,
                         PILOTE=COURBES,
                         NOM_PARA=('COTES','COEF_CONV',),
                         TITRE='Coefficient de convection a l\'instant %s' % Inst_Ecrevisse,
                         LEGENDE_X='Abscisse curviligne (m)',
                         LEGENDE_Y='Coefficient de convection (W/m2/K)',
                        );
              DEFI_FICHIER(ACTION='LIBERER', UNITE=55)
              
              nomfic = os.path.join( os.getcwd(), 'REPE_OUT', str(pref_fic) + '_pression')
              if not os.path.isfile(nomfic): acces='NEW'
              else: acces='APPEND'

              DEFI_FICHIER(ACTION='ASSOCIER', UNITE=55, TYPE='ASCII', ACCES=acces, FICHIER=nomfic)
              IMPR_TABLE(FORMAT='XMGRACE',
                         TABLE=__TAB_i,
                         UNITE=55,
                         PILOTE=COURBES,
                         NOM_PARA=('COTES','PRESSION',),
                         TITRE='Pression a l\'instant %s' % Inst_Ecrevisse,
                         LEGENDE_X='Abscisse curviligne (m)',
                         LEGENDE_Y='Pression (Pa)',
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

            if ( debug2 ):
                os.system('ls -al ' + os.path.join(os.getcwd(),tmp_ecrevisse) )


            # Creation des chargements Aster
            # ******************************

            # Recuperation des valeurs dans la table (voir si il y a plus simple)
            _lst_c = __TABFISS_i.COTES.values()
            _lst_f = __TABFISS_i.FLUX.values()
            _lst_p = __TABFISS_i.PRESSION.values()
            _lst_t = __TABFISS_i.TEMP.values()
            _lst_cc = __TABFISS_i.COEF_CONV.values()

            _lis_dtot = __TABFISS_i.DEBTOT.values()
            _lis_dair = __TABFISS_i.DEBAIR.values()
            _lis_dvap = __TABFISS_i.DEBVAP.values()
            _lis_dliq = __TABFISS_i.DEBLIQ.values()
            _lis_ecou = __TABFISS_i.ECOULEMENT.values()
            
            if ( debug2 ):
                print '_lst_c2=',_lst_c
                print '_lst_f2=',_lst_f
                print '_lst_p2=',_lst_p
                print '_lst_t2=',_lst_t
                print '_lst_cc2=',_lst_cc

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
                if ( debug2 ):
                  print '_lst_c=',_lst_c
                  print '_lst_f=',_lst_f
                  print '_lst_p=',_lst_p
                  print '_lst_t=',_lst_t
                  print '_lst_cc=',_lst_cc

                _lst_f=list(_lst_f)
                _lst_p=list(_lst_p)
                _lst_t=list(_lst_t)
                _lst_cc=list(_lst_cc)

                # decalage par rapport a l'abscisse curviligne (a regler)
                _x0 = 0.001

                # ANCIENNE VERSION (TRANSMISSION DES FLUX THERMIQUES
                if(oldVersion) :
                    # Creation des deux listes (x1, y1, x2, y2, ...)
                    _tmp1=[]
                    _tmp2=[]
                    _tmp3=[]

                    for i in range(len(_lst_c)):
                        _tmp1.append( _x0 + _lst_c[i] )
                        _tmp1.append( _lst_f[i] )
                        _tmp3.append( _x0 + _lst_c[i] )
                        _tmp3.append( -1*_lst_f[i] )

                        _tmp2.append( _x0 + _lst_c[i] )
                        _tmp2.append( _lst_p[i] )

                    # Flux en provenance d'Ecrevisse

                    _L_F1=DEFI_FONCTION(NOM_PARA=DIR_FISS,
                                        VALE=_tmp1,
                                        PROL_GAUCHE='CONSTANT',
                                        PROL_DROITE='CONSTANT');

                    _L_F2=DEFI_FONCTION(NOM_PARA=DIR_FISS,
                                        VALE=_tmp3,
                                        PROL_GAUCHE='CONSTANT',
                                        PROL_DROITE='CONSTANT' );

                    l_FLUX_REP_F1.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][0],
                                                            FLUX_X=_L_F1,))

                    l_FLUX_REP_F2.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][1],
                                                            FLUX_X=_L_F2,))

                    # Pressions en provenance d'Ecrevisse
                    _L_P=DEFI_FONCTION(NOM_PARA=DIR_FISS,
                                        VALE=_tmp2,
                                        PROL_GAUCHE='CONSTANT',
                                        PROL_DROITE='CONSTANT');

                    l_PRES_REP.append(_F(GROUP_MA=(dFISSURE['GROUP_MA'][0],dFISSURE['GROUP_MA'][1]),
                                                            PRES=_L_P,))

                # NOUVELLE VERSION
                else :

                    # Creation des deux listes (x1, y1, x2, y2, ...)
                    _tmp1=[]
                    _tmp2=[]
                    _tmp3=[]

                    for i in range(len(_lst_c)):
                        # On cree trois listes :
                        #  _tmp1=temperature en chaque point du maillage,
                        #  _tmp3=coefficient d echange en chaque point du maillage
                        #  _tmp2=pression en chaque point du maillage
                        _tmp1.append( _x0 + _lst_c[i] )
                        _tmp1.append( _lst_t[i] )
                        _tmp3.append( _x0 + _lst_c[i] )
                        _tmp3.append( _lst_cc[i] )

                        _tmp2.append( _x0 + _lst_c[i] )
                        _tmp2.append( _lst_p[i] )

                    _L_F1=DEFI_FONCTION(NOM_PARA=DIR_FISS,
                                        VALE=_tmp1,
                                        PROL_GAUCHE='CONSTANT',
                                        PROL_DROITE='CONSTANT');

                    _L_F2=DEFI_FONCTION(NOM_PARA=DIR_FISS,
                                        VALE=_tmp3,
                                        PROL_GAUCHE='CONSTANT',
                                        PROL_DROITE='CONSTANT');

                    l_ECHANGE_F1.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][0],
                                                            TEMP_EXT=_L_F1,
                                                            COEF_H=_L_F2))

                    l_ECHANGE_F2.append(_F(GROUP_MA=dFISSURE['GROUP_MA'][1],
                                                            TEMP_EXT=_L_F1,
                                                            COEF_H=_L_F2))

                    # Pressions en provenance d'Ecrevisse
                    _L_P=DEFI_FONCTION(NOM_PARA=DIR_FISS,
                                        VALE=_tmp2,
                                        PROL_GAUCHE='CONSTANT',
                                        PROL_DROITE='CONSTANT');

                    l_PRES_REP.append(_F(GROUP_MA=(dFISSURE['GROUP_MA'][0],dFISSURE['GROUP_MA'][1]),
                                                            PRES=_L_P,))

            #
            # Fin extraction des conditions limites du calcul Ecrevisse
            # ----------------------------------------------------------

        # Fin de la boucle sur les fissures



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
