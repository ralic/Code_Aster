#@ MODIF macro_expans_ops Macro  DATE 17/07/2007   AUTEUR REZETTE C.REZETTE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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



def macro_expans_ops( self,
                      MODELE_CALCUL,
                      MODELE_MESURE,
                      NUME_DDL,
                      RESU_NX=None,
                      RESU_EX=None,
                      RESU_ET=None,
                      RESU_RD=None,
                      MODES_NUM=None,
                      MODES_EXP=None,
                      RESOLUTION=None,
                      *args
                     ):
    """!macro MACRO_EXPANS """
    from Accas import _F
    from Cata.cata import CO, mode_meca, dyna_harmo
    from Utilitai.Utmess import U2MESS as UTMESS
    from types import ListType, TupleType
    ier = 0

    EXTR_MODE = self.get_cmd('EXTR_MODE')
    PROJ_MESU_MODAL = self.get_cmd('PROJ_MESU_MODAL')
    REST_BASE_PHYS = self.get_cmd('REST_BASE_PHYS')
    PROJ_CHAMP = self.get_cmd('PROJ_CHAMP')
    NORM_MODE = self.get_cmd('NORM_MODE')
    
    
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    NUME_MODE_NUM = MODELE_CALCUL['NUME_MODE']
    NUME_MODE_EXP = MODELE_MESURE['NUME_MODE']
    
    RESU_NUM = MODELE_CALCUL['BASE']
    RESU_EXP = MODELE_MESURE['MESURE']

    MOD_CALCUL = MODELE_CALCUL['MODELE']
    MOD_MESURE = MODELE_MESURE['MODELE']

    NOM_CHAM = MODELE_MESURE['NOM_CHAM']

    # Extraction des modes numériques
    # -------------------------------
    self.DeclareOut( "RESU_NX", RESU_NX )
    if not NUME_MODE_NUM:
        RESU_NX = RESU_NUM
    else:
        RESU_NX = EXTR_MODE( FILTRE_MODE=_F( MODE=RESU_NUM,
                                             NUME_MODE=NUME_MODE_NUM ) )

    
    # Extraction des modes expérimentaux
    # ----------------------------------
    self.DeclareOut( "RESU_EX", RESU_EX )
    if not NUME_MODE_EXP:
        RESU_EX = RESU_EXP
    else:
        RESU_EX = EXTR_MODE( FILTRE_MODE=_F( MODE=RESU_EXP,
                                             NUME_MODE=NUME_MODE_EXP ) )


    
    # Projection des modes experimentaux - on passe le mot-clef
    # RESOLUTION directement à PROJ_MESU_MODAL

    # Mot-clé facteur de résolution
    mfact = []
    for m in RESOLUTION:
        if m['METHODE'] == 'SVD':
            mfact.append({'METHODE':'SVD','EPS':m['EPS']})
        if m['METHODE'] == 'LU':
            mfact.append({'METHODE':'LU'})
        if m['REGUL'] != 'NON':
            if m['COEF_PONDER']:
                mfact.append({'COEF_PONDER':m['COEF_PONDER']})
            if m['COEF_PONDER_F']:
                mfact.append({'COEF_PONDER_F':m['COEF_PONDER_F']})

    # Paramètres à garder : si on étend des mode_meca, on conserve les
    # freq propres, amortissements réduits, et masses généralisées. Pour
    # les dyna_harmo, on conserve les fréquences uniquement
    if isinstance(RESU_EXP, mode_meca):
        paras = ('FREQ','AMOR_REDUIT','MASS_GENE',)
    elif isinstance(RESU_EXP, dyna_harmo):
        paras = ('FREQ')
    else:
        paras = None
#             "LE MODELE MEDURE DOIT ETRE UN CONCEPT DE TYPE DYNA_HARMO OU MODE_MECA")
        UTMESS('A','MEIDEE0_1')
            
    
    try:
        __PROJ = PROJ_MESU_MODAL(MODELE_CALCUL = _F( BASE=RESU_NX,
                                                     MODELE=MOD_CALCUL,
                                                   ),
                                 MODELE_MESURE = _F( MESURE=RESU_EX,
                                                     MODELE=MOD_MESURE,
                                                     NOM_CHAM=NOM_CHAM,
                                                   ),
                                 RESOLUTION=mfact,
                                 NOM_PARA=paras,
                                 );
    except Exception, err:
        raise Exception, err
    
    # Phase de reconstruction des donnees mesurees sur le maillage
    # numerique
    self.DeclareOut( "RESU_ET", RESU_ET )
    RESU_ET = REST_BASE_PHYS( RESU_GENE  = __PROJ,
                              MODE_MECA   = RESU_NX,
                              TOUT_ORDRE  = 'OUI',
                              NOM_CHAM    = NOM_CHAM);



    
    # Restriction des modes mesures etendus sur le maillage capteur
    self.DeclareOut( "RESU_RD", RESU_RD )
    RESU_RD = PROJ_CHAMP( METHODE    = 'ELEM',
                          RESULTAT   = RESU_ET,
                          MODELE_1   = MOD_CALCUL,
                          MODELE_2   = MOD_MESURE,
                          NOM_CHAM   = NOM_CHAM,
                          TOUT_ORDRE = 'OUI',
                          NUME_DDL   = NUME_DDL,
                          VIS_A_VIS  =_F( TOUT_1='OUI',
                                          TOUT_2='OUI',),
                          NOM_PARA   = paras,
                        );

   
    return ier
