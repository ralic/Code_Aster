# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

def macr_ecrevisse_ops(self, reuse,
   CONV_CRITERE,
   TABLE,
   TEMPER,
   DEBIT,
   MODELE_MECA,
   MODELE_THER,
   FISSURE,
   ECOULEMENT,
   LIST_INST,
   MODELE_ECRE,
   CONVERGENCE_ECREVISSE,
   COURBES,
   LOGICIEL,
   VERSION,
   ENTETE,
   IMPRESSION,
   CHAM_MATER,
   CARA_ELEM,
   CONTACT,
   EXCIT_MECA,
   EXCIT_THER,
   COMPORTEMENT,
   NEWTON,
   CONVERGENCE,
   ETAT_INIT,
   ENERGIE,
   INFO,
   **args):
    """
    Procédure de couplage Code_Aster-Ecrevisse.
    Exécution pour tous les pas de temps des calculs thermiques, mécaniques puis hydrauliques.
    Découpage/Génération par Aster du fichier de données d'Ecrevisse et lancement d'Ecrevisse.
    """
    from Utilitai.Utmess import UTMESS, MasquerAlarme, RetablirAlarme
    from Utilitai.Table import Table, merge
    from Accas import _F
    import aster_core
    import os, aster, copy

    ier=0
    #
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Parametres debug
    debug = False

    # Info
    InfoAster = 1
    info2 = (INFO==2)
    if debug :
        info2=True

    # IMPORTATION DE COMMANDES ASTER
    DEFI_LIST_REEL = self.get_cmd("DEFI_LIST_REEL")
    THER_LINEAIRE  = self.get_cmd("THER_LINEAIRE")
    PROJ_CHAMP     = self.get_cmd("PROJ_CHAMP")
    DETRUIRE       = self.get_cmd("DETRUIRE")
    AFFE_MATERIAU  = self.get_cmd("AFFE_MATERIAU")
    STAT_NON_LINE  = self.get_cmd("STAT_NON_LINE")
    CALC_ECREVISSE = self.get_cmd("CALC_ECREVISSE")
    CO             = self.get_cmd("CO")
    CREA_TABLE     = self.get_cmd("CREA_TABLE")

    # Concepts sortants
    # TABLE creees par concatenation des tables sorties par CALC_ECREVISSE a chaque iteration
    self.DeclareOut('TABL_RES',TABLE)
    self.DeclareOut('DEB_RES',DEBIT)
    # Concepts sortant: les resultats de STAT_NON_LINE et de THER_LINEAIRE valides
    # vis-a-vis du calcul ecrevisse
    self.DeclareOut('RTHERM',TEMPER)
    self.DeclareOut('MECANIC', self.sd)

    # alarme de STAT_NON_LINE si les mot-cles de COMPORTEMENT sont renseignes a tort
    MasquerAlarme('COMPOR4_70')

    IsPoursuite = False
    IsInit = True
    # Traitement de l'etat initial en cas de poursuite
    if ETAT_INIT:
        dEtatInit=ETAT_INIT[0].cree_dict_toutes_valeurs()
        EVINIT = dEtatInit['EVOL_NOLI']
        _THINIT = dEtatInit['EVOL_THER']
        nume_ordre = dEtatInit['NUME_ORDRE']
        IsPoursuite= True
    else :
        dEtatInit = None

    ## Valeur par defaut du mot cle LOGICIEL
    #if not LOGICIEL: LOGICIEL = os.path.join(aster_core.get_option('repout'), 'ecrevisse')

    # RECUPERATION DES MOTS-CLES FACTEURS

    l_dFISSURE=[]
    for fissure in FISSURE:
        dFISSURE=fissure.cree_dict_toutes_valeurs()
        l_dFISSURE.append(dFISSURE)

    dECOULEMENT = ECOULEMENT[0].cree_dict_toutes_valeurs()
    # on ne supprime pas les valeurs None
    dMODELE_ECRE = MODELE_ECRE[0].cree_dict_valeurs(MODELE_ECRE[0].mc_liste)
    dCONVERGENCE_ECREVISSE = CONVERGENCE_ECREVISSE[0].cree_dict_toutes_valeurs()
    dCOMPORTEMENT = COMPORTEMENT[0].cree_dict_toutes_valeurs()
    dNEWTON = NEWTON[0].cree_dict_toutes_valeurs()
    dCONVERGENCE = CONVERGENCE[0].cree_dict_toutes_valeurs()

    # Recuperation des infos pour la convergence de la macro
    dMacr_Conv = CONV_CRITERE[0].cree_dict_toutes_valeurs()
    motclefsCALC_ECREVISSE = {}
    motclefsCALC_ECREVISSE['COURBES'] = COURBES,

    # --------------------------------------------------------------------------
    # Debut de la macro

    # Si LIST_INST est un DEFI_LIST_REEL :
    liste_inst = LIST_INST.Valeurs()
    if (debug):
        print 'liste des instants liste_inst = ', liste_inst

    # Drapeaux pour les 1ers calculs et les 1eres definitions
    # si l'execution d'Ecrevisse n'a pas plantee ou a ete realisee
    EcrevisseExe       = False

    # Table python devant contenir toutes les tables Ecrevisse
    T_TABL_RES = None
    T_DEB_RES  = None
    # Precision demandee pour converger sur le critere de la macro
    # Nombre de decoupages succesifs d'un pas de temps
    # Pas de temps en dessous duquel on ne decoupe plus
    if dMacr_Conv.has_key('SUBD_NIVEAU'):
        MacrNbDecoupage   = dMacr_Conv['SUBD_NIVEAU']
    if dMacr_Conv.has_key('SUBD_PAS_MINI'):
        MacrPasMini       = dMacr_Conv['SUBD_PAS_MINI']
    MacrTempRef       = dMacr_Conv['TEMP_REF']
    MacrPresRef       = dMacr_Conv['PRES_REF']
    MacrCritere       = dMacr_Conv['CRITERE']
    if dMacr_Conv.has_key('PREC_CRIT'):
        MacrPrecisCritere = dMacr_Conv['PREC_CRIT']
    else:
        MacrPrecisCritere = None
    if dMacr_Conv.has_key('NUME_ORDRE_MIN'):
        MacrNumeOrdre     = dMacr_Conv['NUME_ORDRE_MIN']

    #
    # il faut 2 pas au minimum dans la liste
    if (len(liste_inst) < 2 ):
        UTMESS('F','ECREVISSE0_20', vali=[2])

    if (not IsPoursuite) :
        nume_ordre = 0
    else :
        # Dans le cas d'une poursuite :
        # On reconstruit une nouvelle liste d'instant composee de l'ancienne liste
        # jusqu'a l'instant recherche, puis de la nouvelle a partir de cet instant
        # ainsi le nume_ordre de la nouvelle liste correspond au nume_ordre de l'ancienne
        __dico1 = _THINIT.LIST_VARI_ACCES()
        _list_precedente    = __dico1['INST']
        _list_numordre_prec = __dico1['NUME_ORDRE']
        try :
          idx_last = _list_numordre_prec.index(nume_ordre)
        except :
          UTMESS('F','ECREVISSE0_25', vali=nume_ordre)

        _inst_init = _list_precedente[idx_last]
        new_list   = _list_precedente[0:idx_last+1]

        try:
            # si l'instant est dans la liste, on recupere l'index
            _idx = liste_inst.index(_inst_init)
            _idx += 1
        except:
            # on cherche le plus proche
            _idx = 0
            found = False
            if _inst_init >= liste_inst[-1]:
              UTMESS('F','ECREVISSE0_26', valr = [liste_inst[-1], _inst_init] )

            for t in liste_inst:
                if t > _inst_init:
                    found = True
                    break
                _idx += 1

        # liste precedent jusqu'a l'instant a recalculer (inclus, ca permet de gerer
        # le cas ou l'instant a recalculer n'est pas dans la nouvelle liste : il sera ajoute)
        # on lui ajoute la nouvelle liste a partir du l'instant a recalculer
        new_list.extend( liste_inst[_idx:] )
        liste_inst = copy.copy(new_list)


    ########################################################################################
    # Debut boucle sur la liste d'instant
    ########################################################################################
    FinBoucle = False
    while ( not FinBoucle ):
        inst      = liste_inst[nume_ordre]
        if ( debug ):
            print 'Instant debut boucle', inst
        # On boucle jusqu'a convergence
        NbIter = 0
        while True:
            if ( (not IsPoursuite) or EcrevisseExe) :
            # Le temps que l'on traite
                inst_p_un = liste_inst[nume_ordre+1]
                IsInitEcre= False
                # Construction de la liste des pas
                __pas = DEFI_LIST_REEL( VALE=liste_inst, )
                if (debug):
                    print '=====> ===== ===== ===== <===='
                    print 'Iteration numero : ', NbIter
                    print 'Instant          : ', inst
                    print 'Instant+1        : ', inst_p_un
                    print 'nume_ordre       : ', nume_ordre+1
                    print 'Donnee Ecrevisse : ', EcrevisseExe

                # ---------------------
                #        THERMIQUE
                # ---------------------
                # Recuperation des chargements thermiques
                _dEXCIT_THER = []
                if EXCIT_THER:
                    for excit_i in EXCIT_THER:
                        dEXCIT_THER_i = excit_i.cree_dict_toutes_valeurs()
                        _dEXCIT_THER.append(dEXCIT_THER_i)

                # Definition des chargements thermiques venant d Ecrevisse
                if ( EcrevisseExe ):
                    _dEXCIT_THER.append( _F(CHARGE=FLU1ECR0) )
                    _dEXCIT_THER.append( _F(CHARGE=FLU2ECR0) )

                # Definition de l'etat initial
                motclefs = {}
                if (nume_ordre == 0) :
                    # On verifie que temp_ref est bien renseigne dans AFFE_MATERIAU
                    try:
                        tref = CHAM_MATER.get_vale_ref('TEMP')
                    except:
                        UTMESS('F','ECREVISSE0_22',)

                    motclefs['ETAT_INIT']=[_F(VALE=tref, NUME_ORDRE=nume_ordre)]
                    if ( debug ):
                        print 'thermique initialise avec tref'
                else:
                    if (IsInit) :
                    #if (IsPoursuite) :
                        motclefs['reuse']=_THINIT
                        motclefs['ETAT_INIT']=[_F(EVOL_THER=_THINIT, NUME_ORDRE=nume_ordre)]
                        if (debug):
                            print 'thermique initialise avec etat_initial'
                    else :
                        motclefs['reuse']=RTHERM
                        motclefs['ETAT_INIT']=[_F(EVOL_THER=RTHERM, NUME_ORDRE=nume_ordre)]
                        if (debug):
                            print 'thermique initialise avec instant precedent'

                if (debug):
                    print '====> THER_LINEAIRE <===='
                    print '   Les charges thermiques'
                    print EXCIT_THER

                if IsPoursuite :
                    _THINIT = THER_LINEAIRE(
                      MODELE     = MODELE_THER,
                      CHAM_MATER = CHAM_MATER,
                      EXCIT      = _dEXCIT_THER,
                      INCREMENT  = _F(LIST_INST=__pas,
                                      NUME_INST_INIT=nume_ordre,
                                      NUME_INST_FIN=nume_ordre+1,),
                      INFO       = InfoAster,
                      **motclefs )

                    _RTHMPJ=PROJ_CHAMP(RESULTAT=_THINIT,
                                        MODELE_1=MODELE_THER,
                                        MODELE_2=MODELE_MECA,
                                        METHODE='COLLOCATION',
                                        VIS_A_VIS=_F(TOUT_1='OUI',
                                                     TOUT_2='OUI',),
                                        INFO=2,)
                    RTHERM=_THINIT
                else :
                    RTHERM=THER_LINEAIRE(
                         MODELE     = MODELE_THER,
                         CHAM_MATER = CHAM_MATER,
                         EXCIT      = _dEXCIT_THER,
                         INCREMENT  = _F(LIST_INST=__pas,
                                         NUME_INST_INIT=nume_ordre,
                                         NUME_INST_FIN=nume_ordre+1,),
                         INFO       = InfoAster,
                         **motclefs)

                    # Projection du champ thermique, a tous les instants
                    # sinon pas de deformations thermiques
                    _RTHMPJ=PROJ_CHAMP(RESULTAT=RTHERM,
                                        MODELE_1=MODELE_THER,
                                        MODELE_2=MODELE_MECA,
                                        METHODE='COLLOCATION',
                                        VIS_A_VIS=_F(TOUT_1='OUI',
                                                     TOUT_2='OUI',),
                                        INFO=2,)

                # Definition du materiau pour la mecanique
                # note : on doit le faire a chaque fois car le nom de concept _RTHMPJ
                #        est different a chaque passage
                motclefmater = {}
                motclefmater['AFFE'] = []
                motclefmater['AFFE_VARC'] = []

                for j in CHAM_MATER['AFFE_VARC'] :
                    dvarc = j.cree_dict_toutes_valeurs()
                    motclefmater['AFFE_VARC'].append(dvarc)

                for j in CHAM_MATER['AFFE'] :
                    motclefmater['AFFE'].append(j.cree_dict_toutes_valeurs())

                #XXX on croise les doigts que AFFE_VARC est de longueur non nulle ?!!
                dvarc['EVOL'] = _RTHMPJ
                motclefmater['MAILLAGE'] = CHAM_MATER['MAILLAGE']
                __MATMEC=AFFE_MATERIAU(
                   **motclefmater
                )

                # ---------------------
                #        MECANIQUE
                # ---------------------
                _dEXCIT_MECA = []
                # Recuperation des chargements mecaniques
                if EXCIT_MECA:
                    for excit_i in EXCIT_MECA:
                        dEXCIT_MECA_i = excit_i.cree_dict_toutes_valeurs()
                        _dEXCIT_MECA.append(dEXCIT_MECA_i)


                # Definition des chargements venant d'Ecrevisse
                if ( EcrevisseExe ):
                    _dEXCIT_MECA.append( _F(CHARGE=MECAECR0) )

                motclefs = {}
                if (not IsPoursuite) :
                    if (nume_ordre != 0):
                        motclefs['reuse'] = MECANIC
                        motclefs['ETAT_INIT']=[_F(EVOL_NOLI=MECANIC, NUME_ORDRE=nume_ordre)]
                        if (debug):
                            print 'etat meca initial = pas precedent'
                    else:
                        if (debug):
                            print 'etat meca initial : vierge'
                else:
                    motclefs['reuse']=EVINIT
                    motclefs['ETAT_INIT']=[_F(EVOL_NOLI=EVINIT, NUME_ORDRE=nume_ordre)]
                    if (debug):
                        print 'etat meca initial dReuseM', motclefs

                if ENERGIE:
                    motclefs['ENERGIE']=ENERGIE[0].cree_dict_valeurs(ENERGIE[0].mc_liste)

                if (debug):
                    print '====> STAT_NON_LINE <===='
                if (debug):
                    print '   Les charges mecaniques'
                    print _dEXCIT_MECA

                MECANIC=STAT_NON_LINE(
                   MODELE      = MODELE_MECA,
                   CHAM_MATER  = __MATMEC,
                   CARA_ELEM   = CARA_ELEM,
                   CONTACT     = CONTACT,
                   EXCIT       = _dEXCIT_MECA,
                   COMPORTEMENT   = _F(**dCOMPORTEMENT),
                   INCREMENT   = _F(LIST_INST=__pas,
                                    NUME_INST_INIT=nume_ordre,
                                    NUME_INST_FIN=nume_ordre+1,),
                   NEWTON      = _F(**dNEWTON),
                   CONVERGENCE = _F(**dCONVERGENCE),
                   SOLVEUR     = _F(SYME='OUI'),
                   INFO        = InfoAster,
                   **motclefs
                )
                # Destruction des concepts
                #  Thermique projete
                #  Liste des pas
                DETRUIRE(CONCEPT=(_F(NOM=_RTHMPJ),
                                  _F(NOM=__pas),),
                         INFO=1)

            else :
                #      CAS OU LA MACRO EST REENTRANTE : ON RELANCE ECREVISSE POUR CONNAITRE
                #     LES CHARGEMENT A UTILISER POUR LES PROBLEMES THERMIQUES ET MECANIQUES
                inst_p_un  = inst
                IsInitEcre = True

            # -----------------------------------------------------------------------
            #        ECREVISSE : ATTENTION SI REPRISE CALCUL, ON RECALCULE LE DERNIER INSTANT
            # -------------------------------------------------------------------------
            # Si Ecrevisse a deja ete fait une fois.
            #   ==> Efface les concepts qui sont en sortie
            if ( EcrevisseExe ):
                DETRUIRE(
                   CONCEPT=(
                      _F(NOM=MECAECR1),
                      _F(NOM=FLU1ECR1),
                      _F(NOM=FLU2ECR1),
                      _F(NOM=TABLECR1),
                      _F(NOM=DEBIECR1),
                   ),INFO=1)

            # On remplace FONC_XXX par la valeur XXX correspondante a l'instant inst_p_un
            dECOULEMENT_ecrevisse = copy.copy(dECOULEMENT)
            for fonc_name in ["PRES_ENTREE_FO", "PRES_SORTIE_FO", "PRES_PART_FO",
                              "TITR_MASS_FO", "TEMP_ENTREE_FO"]:
                if dECOULEMENT.has_key(fonc_name):
                    fonc = dECOULEMENT_ecrevisse.pop(fonc_name)
                    vale_name = fonc_name.replace('_FO', '')
                    dECOULEMENT_ecrevisse[vale_name] = fonc(inst_p_un)


            # Initialisation des concepts en sortie
            MECAECR1=CO('MECAECR1')
            FLU1ECR1=CO('FLU1ECR1')
            FLU2ECR1=CO('FLU2ECR1')
            TABLECR1=CO('TABLECR1')
            DEBIECR1=CO('DEBIECR1')

            if (debug):
                print '====> ECREVISSE entree dans CALC_ECREVISSE <===='

            if (not IsPoursuite) :
                CALC_ECREVISSE(
                   CHARGE_MECA      = MECAECR1,
                   CHARGE_THER1     = FLU1ECR1,
                   CHARGE_THER2     = FLU2ECR1,
                   TABLE            = TABLECR1,
                   DEBIT            = DEBIECR1,
                   MODELE_MECA      = MODELE_MECA,
                   MODELE_THER      = MODELE_THER,
                   ENTETE           = ENTETE,
                   IMPRESSION       = IMPRESSION,
                   INFO             = INFO,
                   RESULTAT=_F(THERMIQUE  = RTHERM,
                               MECANIQUE  = MECANIC,
                               INST       = inst_p_un, ),
                   # chemin d acces a Ecrevisse
                   LOGICIEL         = LOGICIEL,
                   VERSION          = VERSION,
                   # donnees necessaire pour ecrevisse
                   # assurer la coherence des donnees en fonction
                   # de FLUIDE_ENTREE = iflow (voir doc Ecrevisse)
                   # activation eventuelle de TITR_VA et P_AIR
                   FISSURE=l_dFISSURE,
                   ECOULEMENT=_F( **dECOULEMENT_ecrevisse ),
                   MODELE_ECRE=_F( **dMODELE_ECRE),

                   CONVERGENCE=_F( **dCONVERGENCE_ECREVISSE ),
                   **motclefsCALC_ECREVISSE
                   );
            else :
                CALC_ECREVISSE(
                   CHARGE_MECA      = MECAECR1,
                   CHARGE_THER1     = FLU1ECR1,
                   CHARGE_THER2     = FLU2ECR1,
                   TABLE            = TABLECR1,
                   DEBIT            = DEBIECR1,
                   MODELE_MECA      = MODELE_MECA,
                   MODELE_THER      = MODELE_THER,
                   ENTETE           = ENTETE,
                   IMPRESSION       = IMPRESSION,
                   INFO             = INFO,
                   RESULTAT=_F(THERMIQUE  = _THINIT,
                               MECANIQUE  = EVINIT,
                               INST       = inst_p_un, ),
                   # chemin d acces a Ecrevisse
                   LOGICIEL         = LOGICIEL,
                   VERSION          = VERSION,
                   # donnees necessaire pour ecrevisse
                   # assurer la coherence des donnees en fonction
                   # de FLUIDE_ENTREE = iflow (voir doc Ecrevisse)
                   # activation eventuelle de TITR_VA et P_AIR
                   FISSURE=l_dFISSURE,
                   ECOULEMENT=_F( **dECOULEMENT_ecrevisse ),
                   MODELE_ECRE=_F( **dMODELE_ECRE),

                   CONVERGENCE=_F( **dCONVERGENCE_ECREVISSE ),
                   **motclefsCALC_ECREVISSE
                   );

            if (debug):
                print '====> ECREVISSE sortie de CALC_ECREVISSE <===='


            # Recuperation des infos de la table resultat Ecrevisse
            T_TABL_TMP1 = TABLECR1.EXTR_TABLE()
            T_DEB_TMP1  = DEBIECR1.EXTR_TABLE()
#         # On ajoute deux colonnes supplementaires
#         _nb_ligne = len(T_DEB_TMP1["DEBTOT"])
#         T_DEB_TMP1["NUME_ORDRE"] = [nume_ordre+1]*_nb_ligne
#         T_DEB_TMP1["INST"]       = [inst_p_un]*_nb_ligne

            # Le calcul Ecrevisse c'est bien passe ?
            EcrevisseExe = ( T_TABL_TMP1.values()['COTES'][0] != -1 )
            #
            if ( not EcrevisseExe ):
                # Destruction des concepts de sortie, et on arrete tout
                DETRUIRE(
                   CONCEPT=( _F(NOM=MECAECR1),
                             _F(NOM=FLU1ECR1),
                             _F(NOM=FLU2ECR1),
                             _F(NOM=TABLECR1),
                             _F(NOM=DEBIECR1),),
                   INFO=1)
                if ( not IsInit ):
                    DETRUIRE(
                       CONCEPT=( _F(NOM=MECAECR0),
                                 _F(NOM=FLU1ECR0),
                                 _F(NOM=FLU2ECR0),
                                 _F(NOM=TABLECR0),
                                 _F(NOM=DEBIECR0),),
                       INFO=1)
                FinBoucle = True
                break
            #
            # A t'on atteint la convergence
            #  TABLECR0 table Ecrevisse a inst
            #  TABLECR1 table Ecrevisse a inst_p_un
            # --------------------

            if ( not IsInit ):
                # On recupere la liste des temperatures a t et t+1
                lst_T_0 = T_TABL_TMP0.values()['TEMP']
                lst_T_1 = T_TABL_TMP1.values()['TEMP']
                # Le maximum des ecarts
                lst_T_diff_01 = []
                for v1, v2 in zip(lst_T_0,lst_T_1):
                    lst_T_diff_01.append(abs(v1-v2))
                max_T_diff_01 = max(lst_T_diff_01)

                # On recupere la liste des pressions a t et t+1
                lst_P_0 = T_TABL_TMP0.values()['PRESSION']
                lst_P_1 = T_TABL_TMP1.values()['PRESSION']
                # Le maximum des ecarts
                lst_P_diff_01 = []
                for v1, v2 in zip(lst_P_0,lst_P_1):
                    lst_P_diff_01.append(abs(v1-v2))
                max_P_diff_01 = max(lst_P_diff_01)
                #
                # "TEMP_PRESS","EXPLICITE","TEMP","PRESS"
                ErreurT = (max_T_diff_01/MacrTempRef)
                ErreurP = (max_P_diff_01/MacrPresRef)
                ErreurG = (ErreurT**2+ErreurP**2)**0.5
                if   ( MacrCritere == 'TEMP' ):
                    Erreur = ErreurT
                elif ( MacrCritere == 'PRESS' ):
                    Erreur = ErreurP
                else:
                    Erreur = ErreurG

                if ( MacrCritere != 'EXPLICITE' ):
                    Convergence = ( Erreur <= MacrPrecisCritere )
                #
                if info2 :
                        # Info Critere
                    UTMESS('I', 'ECREVISSE0_35', valr=inst_p_un,
                           valk=[MacrCritere, MacrPrecisCritere, Convergence])
                    # Info Convergence
                    UTMESS('I', 'ECREVISSE0_34',
                           valr=[inst_p_un, ErreurT, max_T_diff_01, ErreurP, max_P_diff_01, ErreurG])

            else:
                Convergence = True
                if info2 :
                    UTMESS('I', 'ECREVISSE0_36', valr=[inst_p_un])
            # --------------------
            #

            if ( MacrCritere == 'EXPLICITE' ):
                Convergence = True
            else:
                if ( (nume_ordre != 0) and (nume_ordre+1 <= MacrNumeOrdre) ):
                    UTMESS('A','ECREVISSE0_33',
                           vali=[nume_ordre+1, MacrNumeOrdre], valr=inst_p_un)
                    Convergence = True

            if ( Convergence ):
                nb_lignes_t1 = len(T_TABL_TMP1["COTES"])
                # Ajout de deux colonnes supplementaires
                # POUR LA TABLE ECREVISSE
                T_TABL_TMP1["NUME_ORDRE"] = [nume_ordre+1]*nb_lignes_t1
                T_TABL_TMP1["INST"]       = [inst_p_un]*nb_lignes_t1

                # POUR LA TABLE DES DEBITS
                nb_ligne_t2 = len(T_DEB_TMP1["DEBTOT"])
                T_DEB_TMP1["NUME_ORDRE"] = [nume_ordre+1]*nb_ligne_t2
                T_DEB_TMP1["INST"]       = [inst_p_un]*nb_ligne_t2

                # Ajout des infos dans la table finale
                if ( IsInit ):
                    T_TABL_RES = T_TABL_TMP1
                    T_DEB_RES  = T_DEB_TMP1
                else:
                    T_TABL_RES = merge(T_TABL_RES, T_TABL_TMP1)
                    T_DEB_RES = merge(T_DEB_RES, T_DEB_TMP1)
                    T_TABL_RES.titr = 'TABLE_SDASTER CHARGEMENT ECREVISSE'
                    T_DEB_RES.titr = 'TABLE_SDASTER DEBIT ECREVISSE'
                #
                # RAZ des compteurs de division
                NbIter = 0
                # On memorise les concepts valides
                MECAECR0 = MECAECR1
                FLU1ECR0 = FLU1ECR1
                FLU2ECR0 = FLU2ECR1
                TABLECR0 = TABLECR1
                DEBIECR0 = DEBIECR1
                #
                T_TABL_TMP0 = T_TABL_TMP1
                if (not IsInitEcre) :
                    IsInit = False
                if (info2):
                    UTMESS('I','ECREVISSE0_37', valr=[inst_p_un])
                break

            else:
                NbIter += 1
                # A t'on le droit de decouper, par rapport au nombre de division
                if ( NbIter > MacrNbDecoupage ):
                    FinBoucle = True
                    UTMESS('A','ECREVISSE0_30', valr=[inst,inst_p_un],
                           vali=[MacrNbDecoupage])
                    break
                #
                # on divise le pas de temps par 2
                tmp = (inst + inst_p_un )*0.5
                # A t'on le droit de continuer, par rapport au pas de temps minimum
                if ( (tmp - inst) <= MacrPasMini ):
                    FinBoucle = True
                    UTMESS('A','ECREVISSE0_31', valr=[inst, inst_p_un, tmp, MacrPasMini])
                    break
                #
                if ( info2 ):
                    UTMESS('A','ECREVISSE0_32', valr=[inst, inst_p_un, tmp], vali=[NbIter])
                # on insere le nouveau temps dans la liste des instants avant "inst_p_un"
                liste_inst.insert(nume_ordre+1, tmp)


        # Convergence atteinte, on passe au pas de temps suivant, s'il en reste
        if IsInitEcre :
            continue
        elif (nume_ordre + 2 < len(liste_inst)):
            nume_ordre += 1
        else:
            # On a fait tous les pas de temps
            FinBoucle = True
    ############################################################################
    #     Fin boucle sur les pas de temps
    ############################################################################

    # Creation du concept de la table en sortie
    if ( T_TABL_RES != None ):
        dprod = T_TABL_RES.dict_CREA_TABLE()
        TABL_RES = CREA_TABLE(**dprod)
    if ( T_DEB_RES != None ):
        debprod = T_DEB_RES.dict_CREA_TABLE()
        DEB_RES = CREA_TABLE(**debprod)

    # Destruction des concepts temporaires
    DETRUIRE(
             CONCEPT=( _F(NOM=MECAECR1),
                       _F(NOM=FLU1ECR1),
                       _F(NOM=FLU2ECR1),
                       _F(NOM=TABLECR1),
                       _F(NOM=DEBIECR1),),
             INFO=1,)

    if (nume_ordre != 0 ):
        DETRUIRE(
                 CONCEPT=( _F(NOM=MECAECR0),
                           _F(NOM=FLU1ECR0),
                           _F(NOM=FLU2ECR0),
                           _F(NOM=TABLECR0),
                           _F(NOM=DEBIECR0),),
                 INFO=1,)

    RetablirAlarme('COMPOR4_70')
    return ier
