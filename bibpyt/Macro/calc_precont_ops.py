#@ MODIF calc_precont_ops Macro  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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


# RESPONSABLE ASSIRE A.ASSIRE

def calc_precont_ops(self,reuse,MODELE,CHAM_MATER,CARA_ELEM,EXCIT,
                                CABLE_BP,CABLE_BP_INACTIF,
                                COMP_INCR,ETAT_INIT,NEWTON,RECH_LINEAIRE,
                                CONVERGENCE,INCREMENT,SOLVEUR,SOLV_NON_LOCAL,
                                LAGR_NON_LOCAL,PARM_THETA,INFO,TITRE,**args):


  """
     Ecriture de la macro CALC_PRECONT
  """
  import copy
  import aster
  import string
  import types
  from Accas import _F
  from Noyau.N_utils import AsType
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  AFFE_MODELE      = self.get_cmd('AFFE_MODELE')
  CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
  AFFE_CHAR_MECA   = self.get_cmd('AFFE_CHAR_MECA')
  DEFI_LIST_REEL   = self.get_cmd('DEFI_LIST_REEL')
  STAT_NON_LINE    = self.get_cmd('STAT_NON_LINE')
  CALC_NO          = self.get_cmd('CALC_NO')
  CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
  DEFI_FONCTION    = self.get_cmd('DEFI_FONCTION')
  RECU_TABLE       = self.get_cmd('RECU_TABLE')
  DEFI_MATERIAU    = self.get_cmd('DEFI_MATERIAU')
  AFFE_MATERIAU    = self.get_cmd('AFFE_MATERIAU')

  # La macro compte pour 1 dans la numerotation des commandes
  self.icmd=1

  # Le concept sortant (de type evol_noli) est nomme RES dans 
  # le contexte de la macro

  self.DeclareOut('RES',self.sd)

  # -------------------------------------------------------------
  # 1. CREATION DES MOTS-CLES ET CONCEPTS POUR LES STAT_NON_LINE 
  # ------------------------------------------------------------


  # 1.1 Recuperation de la liste d'instants, de l'instant initial et final
  #     Creation de la nouvelle liste d'instants
  # ----------------------------------------------------------   

  dIncrement=INCREMENT[0].cree_dict_valeurs(INCREMENT[0].mc_liste)
  
  __prec = dIncrement['PRECISION']
  __L0   = dIncrement['LIST_INST']
  __L1   = __L0.Valeurs()
     
  # Traitement de l'etat initial
  if ETAT_INIT:
      dEtatInit=ETAT_INIT[0].cree_dict_valeurs(ETAT_INIT[0].mc_liste)
      for i in dEtatInit.keys():
          if dEtatInit[i]==None : del dEtatInit[i]

      __EVINIT = dEtatInit['EVOL_NOLI']
  else :
      dEtatInit=None
      
  # Test de la presence de reuse=
  if self.reuse == None:
      dReuse=None
  else :
      dReuse='RES'

  # Teste si INST_INIT est donné ou bien recalcule __TMIN
  if dIncrement['INST_INIT'] == None:
    if self.reuse == None:
      __TMIN = __L1[0]
    else:
      __dico = __EVINIT.LIST_VARI_ACCES()
      __TMIN = __dico['INST'][-1]
  else:
    __TMIN = dIncrement['INST_INIT']

  # Teste si INST_FIN est donné ou bien recalcule __TMAX
  if dIncrement['INST_FIN'] == None:
    __TMAX = __L1[-1]
  else:
    __TMAX = dIncrement['INST_FIN']

  # Teste si INST_INIT est bien plus petit que INST_FIN
  if __TMAX <= __TMIN:
    ier=ier+1
    self.cr.fatal("""<F> <CALC_PRECONT> ERREUR : INST_FIN PLUS PETIT QUE INST_INIT""")
    return ier

  # Cree la liste d'instant __L2 allant de __TMIN a __TMAX et contenant 
  # un instant supplementaire __TINT
  __L2=[]
  for m in __L1:
    if m>=__TMIN and m<=__TMAX:
      __L2.append(m)
  
  __TINT = (9.*__L2[-1] + __L2[-2])/10.
  __L2[-1:-1] = [__TINT]

  # __LST0 est la liste d'instants utilisée pour l'etape 1 
  __LST0=DEFI_LIST_REEL( DEBUT = __TMIN,
                        INTERVALLE = _F(JUSQU_A = __TMAX, NOMBRE = 1),)

  # __LST et __FCT sont utilisés pour les etapes 2 et 3
  __LST=DEFI_LIST_REEL(VALE=__L2,);
  __FCT=DEFI_FONCTION(INTERPOL=('LIN','LIN'),
                         NOM_PARA='INST',
                         VALE=(__TMIN,0.0,__TINT,1.0,__TMAX,1.0),);

  for i in dIncrement.keys():
      if dIncrement[i]==None : del dIncrement[i] 
  dIncrement['LIST_INST']= __LST
  dIncrement['INST_FIN'] = __TINT



  # 1.2 Recuperation des parametres pour STAT_NON_LINE
  # -------------------------------------------------------

  dNewton=NEWTON[0].cree_dict_valeurs(NEWTON[0].mc_liste)
  for i in dNewton.keys():
      if dNewton[i]==None : del dNewton[i]

  dConvergence=CONVERGENCE[0].cree_dict_valeurs(CONVERGENCE[0].mc_liste)
  for i in dConvergence.keys():
      if dConvergence[i]==None : del dConvergence[i]

  dSolveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
  for i in dSolveur.keys():
      if dSolveur[i]==None : del dSolveur[i]

  if RECH_LINEAIRE:
    dRech_lin=RECH_LINEAIRE[0].cree_dict_valeurs(RECH_LINEAIRE[0].mc_liste)
    for i in dRech_lin.keys():
        if dRech_lin[i]==None : del dRech_lin[i]
  else :
    dRech_lin=None
    
  if SOLV_NON_LOCAL:
    dSolv_nonloc=SOLV_NON_LOCAL[0].cree_dict_valeurs(SOLV_NON_LOCAL[0].mc_liste)
    for i in dSolv_nonloc.keys():
        if dSolv_nonloc[i]==None : del dSolv_nonloc[i]
  else :
    dSolv_nonloc=None
        
  if LAGR_NON_LOCAL:
    dLagr_nonloc=LAGR_NON_LOCAL[0].cree_dict_valeurs(LAGR_NON_LOCAL[0].mc_liste)
    for i in dLagr_nonloc.keys():
        if dLagr_nonloc[i]==None : del dLagr_nonloc[i]
  else :
    dLagr_nonloc=None   
    


  # 1.3 Creation des mots-cles pour les 3 AFFE_CHAR_MECA
  #     Recuperation des cables dans les concepts CABLE_BP
  #     et CABLE_BP_INACTIF
  # ------------------------------------------------------
  if type(CABLE_BP) is not types.NoneType:
    if type(CABLE_BP) is not types.TupleType:
      CABLE_BP0 = CABLE_BP
      CABLE_BP = []
      CABLE_BP.append ( CABLE_BP0 )

  if type(CABLE_BP_INACTIF) is not types.NoneType:
    if type(CABLE_BP_INACTIF) is not types.TupleType:
      CABLE_BP_INACTIF0 = CABLE_BP_INACTIF
      CABLE_BP_INACTIF = []
      CABLE_BP_INACTIF.append ( CABLE_BP_INACTIF0 )

  motscles={}
  motscles['RELA_CINE_BP']=[]
  motscle2={}
  motscle2['RELA_CINE_BP']=[]
  motscle3={}
  motscle3['RELA_CINE_BP']=[]
  __GROUP_MA_A=[]
  Result = [[None]*1]
  for mcabl in CABLE_BP:
    # Creation de mots-cles pour les AFFE_CHAR_MECA
    motscles['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                       SIGM_BPEL = 'OUI',
                                       RELA_CINE = 'NON',) )
    motscle2['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                       SIGM_BPEL = 'NON',
                                       RELA_CINE = 'OUI',) )
    motscle3['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                       SIGM_BPEL = 'OUI',
                                       RELA_CINE = 'OUI',) )

    # Creation de __GROUP_MA_A : liste des noms des cables contenus 
    # dans chaque concept CABLE_BP = cables  a activer 
    __TCAB = RECU_TABLE(CO=mcabl,NOM_TABLE='CABLE_BP');
    __nb = 0
    while 1: 
      try:
          Result[__nb][0] = __TCAB['NOM_CABLE',__nb+1]
          __CAB = __TCAB['NOM_CABLE',__nb+1]
          if __nb == 0:
            __GROUP_MA_A.append(__CAB)
          else:
            i = 0
            # enlève les doublons
            for m in __GROUP_MA_A:
              i=i+1
              if __CAB == m:
                break
              if i == len(__GROUP_MA_A):
                __GROUP_MA_A.append(__CAB)
    
          __nb = __nb + 1  
          Result.append([None]*1)
    #   Si on a lu toutes les valeurs alors on sort de la boucle
      except KeyError:
        break

  # Creation de __GROUP_MA_I : liste des noms des cables contenus 
  # dans chaque CABLE_BP_INACTIF
  # __GROUP_MA_CABLE = liste des cables actifs et inactifs
  Result = [[None]*1]
  __GROUP_MA_I=[]

  if CABLE_BP_INACTIF:
    for mcabl in CABLE_BP_INACTIF:
      __TCA0 = RECU_TABLE(CO=mcabl,NOM_TABLE='CABLE_BP');
      __nb = 0
      while 1: 
        try:
            Result[__nb][0] = __TCA0['NOM_CABLE',__nb+1]
            __CA0 = __TCA0['NOM_CABLE',__nb+1]
            if __nb == 0:
              __GROUP_MA_I.append(__CA0)
            else:
              i = 0
              # enlève les doublons
              for m in __GROUP_MA_I:
                i=i+1
                if __CA0 == m:
                  break
                if i == len(__GROUP_MA_I):
                  __GROUP_MA_I.append(__CA0)
      
            __nb = __nb + 1  
            Result.append([None]*1)
      #   Si on a lu toutes les valeurs alors on sort de la boucle
        except KeyError:
          break
    motscle6={}
    motscle6['RELA_CINE_BP']=[]
    for mcabl in CABLE_BP_INACTIF:
      # Creation de mots-cles pour les AFFE_CHAR_MECA
      motscle6['RELA_CINE_BP'].append(_F(CABLE_BP=mcabl,
                                         SIGM_BPEL = 'NON',
                                         RELA_CINE = 'OUI',) )  
                                               
  __GROUP_MA_CABLES = __GROUP_MA_A + __GROUP_MA_I


  # 1.4 Creation des mots-clés facteurs COMP_INCR 
  # pour étape 2 (dComp_incr0) et étape 3 (dComp_incr1)
  # ------------------------------------------------------

  dComp_incr=[]
  for j in COMP_INCR :
      dComp_incr.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dComp_incr[-1].keys():
          if dComp_incr[-1][i]==None : del dComp_incr[-1][i]

  dComp_incr0=copy.copy(dComp_incr)
  dComp_incr1=copy.copy(dComp_incr)
   
  dComp_incr0.append(_F(RELATION='SANS',GROUP_MA=__GROUP_MA_CABLES) )
  if __GROUP_MA_I:
    dComp_incr1.append(_F(RELATION='SANS',GROUP_MA=__GROUP_MA_I) )


  # 1.5 Modele contenant uniquement les cables de precontrainte
  # ---------------------------------------------------------

  __MOD = string.ljust(MODELE.nom,8)
  __MOD =__MOD+'.MODELE    .NOMA        '
  __LMAIL = aster.getvectjev(__MOD)
  __MAIL  = string.strip(__LMAIL[0])

  objma=self.get_sd_avant_etape(__MAIL,self)

  __M_CA=AFFE_MODELE( MAILLAGE=objma,
                       AFFE    =_F( GROUP_MA     = __GROUP_MA_A,
                                    PHENOMENE    = 'MECANIQUE',
                                    MODELISATION = 'BARRE') )
  

  # 1.6 Blocage de tous les noeuds des cables actifs
  # --------------------------------------------------
  
  __B_CA=AFFE_CHAR_MECA(MODELE=__M_CA,
                        DDL_IMPO= _F( GROUP_MA = __GROUP_MA_A,
                                      DX = 0.,
                                      DY = 0.,
                                      DZ = 0.),)                                                 


  # 1.7 Chargements concernant les cables
  # -------------------------------------
  __C_CN=AFFE_CHAR_MECA(MODELE=__M_CA,**motscles)
  __C_CA=AFFE_CHAR_MECA(MODELE=MODELE,**motscle2)
  __C_CT=AFFE_CHAR_MECA(MODELE=MODELE,**motscle3)
  if CABLE_BP_INACTIF:
    __C_CI=AFFE_CHAR_MECA(MODELE=MODELE,**motscle6)



  # -------------------------------------------------------------
  # 2. CALCULS 
  # ------------------------------------------------------------


  #------------------------------------------------------------------- 
  # 2.1 Premiere etape : calcul sur le(s) cable(s) et 
  #     recuperation des __F_CAs aux noeuds 
  #     on travaile entre tmin et tmax
  #-------------------------------------------------------------------

  __EV1=STAT_NON_LINE(
                         MODELE     = __M_CA,
                         CHAM_MATER = CHAM_MATER,
                         CARA_ELEM  = CARA_ELEM,
                         EXCIT      =(_F(CHARGE = __B_CA),
                                      _F(CHARGE = __C_CN),),
                         COMP_INCR  =_F( RELATION = 'ELAS',
                                         DEFORMATION = 'PETIT',
                                         TOUT = 'OUI'),
                         INCREMENT  =_F(LIST_INST = __LST0,
                                        PRECISION = __prec),
                         SOLVEUR = dSolveur,
                         PARM_THETA = PARM_THETA,
                         INFO     =INFO,
                         TITRE = TITRE,  )                      
                   
  __EV1 = CALC_NO( reuse    = __EV1,
                   RESULTAT = __EV1,
                   MODELE   = __M_CA,
                   CARA_ELEM= CARA_ELEM,
                   EXCIT=(_F(CHARGE = __B_CA),
                          _F(CHARGE = __C_CN), ),
                   GROUP_MA = __GROUP_MA_A,
                   OPTION = 'FORC_NODA' )
                           
  __REA = CREA_CHAMP (
                     TYPE_CHAM = 'NOEU_DEPL_R',
                     OPERATION = 'EXTR',
                     RESULTAT  =  __EV1,
                     NOM_CHAM  = 'FORC_NODA',
                     INST      = __TMAX);

  __REAC = CREA_CHAMP (TYPE_CHAM='NOEU_DEPL_R',
                     OPERATION = 'ASSE',
                     MODELE    = MODELE,
                     ASSE= _F(GROUP_MA=__GROUP_MA_A,
                              CHAM_GD=__REA,
                              COEF_R = -1.), )
                  
  __F_CA=AFFE_CHAR_MECA(MODELE=__M_CA,
                          VECT_ASSE = __REAC )      
  
  
  
  #-----------------------------------------------------------------------
  # 2.2 Deuxieme etape : application de la precontrainte sur le beton
  #     en desactivant les cables                         
  #-----------------------------------------------------------------------

  # Regeneration des mots-cles EXCIT passés en argument de la macro
  dExcit=[]
  for j in EXCIT :
      dExcit.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dExcit[-1].keys():
          if dExcit[-1][i]==None : del dExcit[-1][i]

  if CABLE_BP_INACTIF:
    dExcit.append(_F(CHARGE=__C_CI),)

  # Creation du mots-cle EXCIT pour le STAT_NON_LINE
  dExcit1=copy.copy(dExcit)
  dExcit1.append(_F(CHARGE=__C_CA),)
  dExcit1.append(_F(CHARGE = __F_CA,
                    FONC_MULT=__FCT ),)

  RES=STAT_NON_LINE( 
                     MODELE      =MODELE,
                     CARA_ELEM   =CARA_ELEM,
                     CHAM_MATER  = CHAM_MATER,
                     COMP_INCR=dComp_incr0,
                     INCREMENT=dIncrement,
                     ETAT_INIT = dEtatInit,
                     NEWTON =dNewton,
                     CONVERGENCE=dConvergence,
                     RECH_LINEAIRE = dRech_lin,
                     SOLVEUR = dSolveur,
                     SOLV_NON_LOCAL = dSolv_nonloc,
                     LAGR_NON_LOCAL = dLagr_nonloc,
                     ARCHIVAGE = _F(INST = __TINT),
                     PARM_THETA = PARM_THETA,
                     INFO     =INFO,
                     TITRE = TITRE,
                     EXCIT = dExcit1,
                     )

  # Recuperation du dernier numero d'ordre pour pouvoir  l'écraser dans RES
  __dico2 = RES.LIST_VARI_ACCES()
  __no = __dico2['NUME_ORDRE'][-1]


  #-----------------------------------------------------------------------
  # 2.2 Troisieme etape : on remet la tension dans les cables
  #-----------------------------------------------------------------------

  # Creation du mots-cles EXCIT pour le STAT_NON_LINE
  dExcit2=copy.copy(dExcit)
  dExcit2.append(_F(CHARGE=__C_CT,) )
   
  # Calcul sur un seul pas (de __TINT a __TMAX)
  RES=STAT_NON_LINE( reuse      = RES,
                     ETAT_INIT  = _F(EVOL_NOLI =RES),
                     MODELE     = MODELE,
                     CHAM_MATER = CHAM_MATER,
                     CARA_ELEM  = CARA_ELEM,
                     COMP_INCR=dComp_incr1, 
                     INCREMENT=_F(LIST_INST = __LST, 
                                  PRECISION = __prec),
                     NEWTON =dNewton,
                     RECH_LINEAIRE = dRech_lin,
                     CONVERGENCE=dConvergence,
                     SOLVEUR = dSolveur,
                     SOLV_NON_LOCAL = dSolv_nonloc,
                     LAGR_NON_LOCAL = dLagr_nonloc,
                     ARCHIVAGE = _F(NUME_INIT = __no,
                                    DETR_NUME_SUIV = 'OUI' ),
                     PARM_THETA = PARM_THETA,
                     INFO  =INFO,
                     TITRE = TITRE,    
                     EXCIT =dExcit2,                 
                     )  

  return ier

