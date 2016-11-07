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

from Accas import _F
from Cata.cata import nappe_sdaster,fonction_sdaster,fonction_c
from Utilitai.Utmess import  UTMESS
import aster_fonctions
import numpy as NP
import math

try:
   import aster
   from Utilitai.Utmess import  UTMESS
except:
   pass
 
def calc_spectre_ipm_ops(self,EQUIPEMENT,CALCUL,RESU,MAILLAGE=None,RESULTAT=None,GROUP_NO=None,
                        AMOR_SPEC=None,LIST_INST=None,FREQ=None, LIST_FREQ=None, NORME=None, TOLE_INIT=None, CORR_INIT=None, **args): 
    import numpy as NP
    import math
    import string
    from types import ListType, TupleType, StringType
    ier=0;
    EnumType=(ListType, TupleType) 
   
    ### Comptage commandes + déclaration concept sortant
    self.set_icmd(1);
    self.DeclareOut('tab',self.sd,);
    macro='calc_spectre_ipm';
   
    ### On importe les definitions des commandes a utiliser dans la macro
    RECU_FONCTION = self.get_cmd("RECU_FONCTION")
    CALC_FONCTION = self.get_cmd('CALC_FONCTION')   
    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    CREA_TABLE       = self.get_cmd('CREA_TABLE')
   
    if AMOR_SPEC!=None and type(AMOR_SPEC) not in EnumType: AMOR_SPEC=(AMOR_SPEC,)
    
    ### construction de la liste des noeuds à traiter
    planch_nodes={}
    planch_param={}
    if ( MAILLAGE ):
        dic_gpno=aster.getcolljev(MAILLAGE.nom.ljust(8)+".GROUPENO")
        l_nodes =aster.getvectjev(MAILLAGE.nom.ljust(8)+".NOMNOE")
    l_plancher=[]
    #
    dplancher = []
    for j in EQUIPEMENT :
        dplancher.append(j.cree_dict_valeurs(j.mc_liste))
    #
    for plancher in dplancher :
        liste_no=[]
        clefs = plancher.keys()
        if ( 'NOEUD' in clefs ):
            if plancher['NOEUD'] != None :
                if type(plancher['NOEUD'])==StringType :
                    liste_no.append(plancher['NOEUD'])
                else :
                    for noeud in plancher['NOEUD'] : liste_no.append(noeud)
        if ( 'GROUP_NO' in clefs ):
            if plancher['GROUP_NO'] != None :
                assert ( MAILLAGE != None )
                if type(plancher['GROUP_NO'])==StringType :
                    noms_no =[string.strip(l_nodes[n-1]) for n in dic_gpno[plancher['GROUP_NO'].ljust(24)]]
                    liste_no=liste_no+noms_no
                else :
                    for group_no in plancher['GROUP_NO'] :
                        noms_no =[string.strip(l_nodes[n-1]) for n in dic_gpno[group_no.ljust(24)]]
                        liste_no=liste_no+noms_no
        planch_nodes[plancher['NOM']]=liste_no
        l_plancher.append(plancher['NOM'])
        
        if plancher['AMOR_EQUIP']!=None and type(plancher['AMOR_EQUIP']) not in EnumType: AMOR_EQUI=(plancher['AMOR_EQUIP'],)
        else: AMOR_EQUI=plancher['AMOR_EQUIP']
        if plancher['AMOR_SUPPORT']!=None and type(plancher['AMOR_SUPPORT']) not in EnumType: AMOR_SUPP=(plancher['AMOR_SUPPORT'],)
        else: AMOR_SUPP=plancher['AMOR_SUPPORT']
        if plancher['RAPPORT_MASSE_TOTALE']!=None and type(plancher['RAPPORT_MASSE_TOTALE']) not in EnumType: RAP_MAS=(plancher['RAPPORT_MASSE_TOTALE'],)
        else: RAP_MAS=plancher['RAPPORT_MASSE_TOTALE']
        if plancher['FREQ_SUPPORT']!=None and type(plancher['FREQ_SUPPORT']) not in EnumType: FREQ_SUPP=(plancher['FREQ_SUPPORT'],)
        else: FREQ_SUPP=plancher['FREQ_SUPPORT']
        FREQ_EQUI    = plancher['FREQ_EQUIP']
        RAP_MAS_COEF = plancher['COEF_MASS_EQUIP']
        ## verification de la longueur des listes
        if (len(AMOR_EQUI)!=len(FREQ_EQUI) or len(AMOR_EQUI)!=len(RAP_MAS_COEF)):
            UTMESS('F','SPECTRAL0_14')
        
        ## verification somme des rapport de masses
        pi = math.pi
        sum = NP.sum(RAP_MAS_COEF)
        if abs(sum-1)>1e-4:
            UTMESS('F','SPECTRAL0_15')
        planch_param[plancher['NOM']]={'AMOR_EQUI':AMOR_EQUI,'AMOR_SUPP':AMOR_SUPP[0],'FREQ_EQUI':FREQ_EQUI,'FREQ_SUPP':FREQ_SUPP[0],'RAP_MAS_COEF':RAP_MAS_COEF,'RAP_MAS':RAP_MAS[0]}    
        
    dico_global = {}    
    # ---------------------------------------------------------------------------------------------
    # boucle 1 sur les planchers
    for plancher in l_plancher :
        AMOR_EQUI = planch_param[plancher]['AMOR_EQUI']
        AMOR_SUPP = planch_param[plancher]['AMOR_SUPP']
        FREQ_EQUI = planch_param[plancher]['FREQ_EQUI']
        FREQ_SUPP = planch_param[plancher]['FREQ_SUPP']
        RAP_MAS_COEF = planch_param[plancher]['RAP_MAS_COEF']
        RAP_MAS = planch_param[plancher]['RAP_MAS']
        # -----------------------------------------------------------------------------------------
        # boucle 2 sur les noeuds du plancher
        indexn=0
        for node in planch_nodes[plancher] :
            # ---------------------------------------------------------------------------------
            # boucle 3 sur les résultats
            l_fonc=[]
            for resu in RESU :
                # Etape 1: Récupération des fonctions
                # if resu['RESU_GENE'] != None :
                    # __ACCE_E=RECU_FONCTION(NOM_CHAM     = 'ACCE',
                            # TOUT_ORDRE   = 'OUI',
                            # NOM_CMP      = 'DZ',
                            # INTERPOL     = 'LIN',
                            # PROL_GAUCHE  = 'CONSTANT',
                            # PROL_DROITE  = 'CONSTANT',
                            # NOEUD        = node ,
                            # RESU_GENE    = resu['RESU_GENE']
                        # )
                # if resu['RESULTAT'] !=None :
                    # __ACCE_E=RECU_FONCTION(
                            # NOM_CHAM     = 'ACCE',
                            # TOUT_ORDRE   = 'OUI',
                            # NOM_CMP      = 'DZ',
                            # INTERPOL     = 'LIN',
                            # PROL_GAUCHE  = 'CONSTANT',
                            # PROL_DROITE  = 'CONSTANT',
                            # NOEUD        = node ,
                            # RESULTAT     = resu['RESULTAT']
                        # )
                if resu['TABLE'] !=None :
                    # 2 formats de table possible. Avec les colonnes :
                    #   INST NOEUD NOM_CHAM NOM_CMP VALE
                    #   INST NOEUD NOM_CHAM DX DY DZ
                    # récupération du nom des colonnes de la table
                    nomcol = resu['TABLE'].get_nom_para()
                    #
                    lst1 = ('INST','NOEUD','NOM_CHAM','NOM_CMP','VALE')
                    ok1  = True
                    for para in lst1: ok1 = ok1 and (para in nomcol)
                    #
                    lst2 = ('INST','NOEUD','NOM_CHAM','DZ',)
                    ok2  = True
                    for para in lst2: ok2 = ok2 and (para in nomcol)
                    #
                    if ( not ok1 ^ ok2 ):
                        print nomcol
                        assert (ok1 ^ ok2)
                    #
                    if ( ok1 ):
                        __ACCE_E=RECU_FONCTION(
                            TABLE=resu['TABLE'],
                            PARA_X  = 'INST', PARA_Y  = 'VALE', INTERPOL= 'LIN',
                            FILTRE  = ( _F(NOM_PARA='NOEUD',    VALE_K=node),
                                        _F(NOM_PARA='NOM_CHAM', VALE_K='ACCE' ),
                                        _F(NOM_PARA='NOM_CMP',  VALE_K='DZ'   ),),
                        )
                    #
                    if ( ok2 ):
                        __ACCE_E=RECU_FONCTION(
                            TABLE=resu['TABLE'], PARA_X  = 'INST', PARA_Y  = 'DZ', INTERPOL= 'LIN',
                                FILTRE  = ( _F(NOM_PARA='NOEUD',    VALE_K=node),
                                            _F(NOM_PARA='NOM_CHAM', VALE_K='ACCE' ),),
                        )
                # Etape 2: Combinaisons        
                if CALCUL=='RELATIF' :
                    # Combinaison avec fonction d'accélération
                    motscles={}
                    if LIST_INST!=None : motscles['LIST_PARA']=LIST_INST
                    __ACCE_E=CALC_FONCTION(
                        COMB=(  _F(FONCTION=__ACCE_E, COEF= 1.0  ),
                                _F(FONCTION=resu['ACCE_Z'], COEF= 1.0),),
                        **motscles
                    )
                val_Acc = NP.array( __ACCE_E.Ordo() )  
                init = abs(val_Acc[0])/max(abs(val_Acc))
                if init>TOLE_INIT:
                    if CORR_INIT=="OUI":
                        UTMESS('A','SPECTRAL0_16', valr=(init, TOLE_INIT))
                        val_Acc[0]=0
                        __ACCE_E  = DEFI_FONCTION(ABSCISSE =__ACCE_E.Absc(),ORDONNEE =val_Acc, NOM_PARA='INST' )
                    else:
                        UTMESS('F','SPECTRAL0_16', valr=(init, TOLE_INIT))
                          
                freq1 = FREQ_SUPP
                # frequence modèle A
                omega1=freq1*2.*pi;
                # frequence modèle B
                omega1_=omega1*(1+RAP_MAS)**0.5
                # calcul de la fft de l'accelero d entree
                # methode 2: CALC_FONCTION pour FFT
                _FFTE = CALC_FONCTION(FFT = _F(FONCTION = __ACCE_E, METHODE='COMPLET'))     
                FFT   = NP.array(_FFTE.Ordo()) +complex(0.,1.)*NP.array(_FFTE.OrdoImg())
                X     = NP.array(_FFTE.Valeurs()[0])
                #
                RES=[];
                cNum = [0]*len(FFT)
                cDenom = [0]*len(FFT)
                # boucle 4 sur les equipements
                for i in range(len(FREQ_EQUI)):
                    omega = FREQ_EQUI[i]*2.*pi;
                    Delta =  -(2.*pi*X[0:len(FFT)])**2 + 2.*complex(0.,1.)*2.*pi*X[0:len(FFT)]*omega*AMOR_EQUI[i] + omega**2
                    cNum = cNum + RAP_MAS*RAP_MAS_COEF[i]*(Delta+(2.*pi*X[0:len(FFT)])**2)/Delta
                    cDenom = cDenom + RAP_MAS*RAP_MAS_COEF[i]*(Delta+(2.*pi*X[0:len(FFT)])**2) - RAP_MAS*RAP_MAS_COEF[i]*(Delta+(2.*pi*X[0:len(FFT)])**2)**2/Delta    
                # Modele B
                Delta1_ = -(2.*pi*X[0:len(FFT)])**2 + 2.*complex(0.,1.)*2.*pi*X[0:len(FFT)]*omega1_*AMOR_SUPP*(1+RAP_MAS)**0.5 + omega1_**2;
                # Modele A
                Delta1 = -(2.*pi*X[0:len(FFT)])**2 + 2.*complex(0.,1.)*2.*pi*X[0:len(FFT)]*omega1*AMOR_SUPP + omega1**2;
                # Calcul de la fonction de transfert
                c = (1.+(2.*pi*X[0:len(FFT)])**2*( 1 + cNum)/(Delta1_ + cDenom))/(1+(2.*pi*X[0:len(FFT)])**2/Delta1)
                RES = FFT*c;
                #
                # methode 2: CALC_FONCTION pour FFT
                val_c=[]
                for i in range(len(X)/2):
                    val_c+=[X[i],RES[i].real,RES[i].imag]
                __FRESULT  = DEFI_FONCTION(VALE_C =val_c, NOM_PARA='FREQ' )
                __ACCEAcor = CALC_FONCTION(FFT = _F(FONCTION = __FRESULT, METHODE='COMPLET', SYME='NON'));     
                #
                # calcul de spectres corriges
                motscles={}
                if FREQ     !=None : motscles['FREQ']     =FREQ
                if LIST_FREQ!=None : motscles['LIST_FREQ']=LIST_FREQ
                __Spec=[None]*len(AMOR_SPEC);
                for amor in range(len(AMOR_SPEC)): # eviter la boucle ??
                    __Spec[amor] = CALC_FONCTION(
                                SPEC_OSCI = _F(  FONCTION = __ACCEAcor, 
                                                 AMOR_REDUIT = AMOR_SPEC[amor], 
                                                 NORME = NORME ,
                                                  **motscles) ,
                                                 NOM_PARA='FREQ', ) ;     
       #****************************************************
                dico_global['FREQ ' + plancher] = __Spec[amor].Valeurs()[1][0][0];
                for amor in range(len(AMOR_SPEC)):
                    if len(planch_nodes[plancher])>1:
                        nom = 'IPM '+ plancher + ' ' +node + ' ' + str(int(AMOR_SPEC[amor]*100)) + '%'
                    else:
                        nom = 'IPM '+ plancher + ' ' + str(int(AMOR_SPEC[amor]*100)) + '%'
                    dico_global[nom] = __Spec[amor].Valeurs()[1][0][1];    
    lListe=[]
    lkeys = dico_global.keys()
    lkeys.sort()
    for key in lkeys:
        lListe.append(_F(LISTE_R=dico_global[key],PARA=key))
    tab=CREA_TABLE(LISTE=lListe, TITRE='Calcul des spectres avec IPM'  + '\n #')    
    
    return ier    
