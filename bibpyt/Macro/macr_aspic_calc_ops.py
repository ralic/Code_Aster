#@ MODIF macr_aspic_calc_ops Macro  DATE 08/02/2005   AUTEUR CIBHHLV L.VIVAN 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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


def macr_aspic_calc_ops(self,TYPE_MAILLAGE,TUBULURE,MAILLAGE,MODELE,CHAM_MATER,CARA_ELEM,
                             FOND_FISS_1,FOND_FISS_2,CHARGE,RESU_THER,AFFE_MATERIAU,EQUILIBRE,
                             PRES_REP,ECHANGE,TORS_CORP,TORS_TUBU,COMP_INCR,COMP_ELAS,
                             THETA_3D,OPTION,SOLVEUR,CONVERGENCE,NEWTON,RECH_LINEAIRE,
                             INCREMENT,PAS_AZIMUT,IMPRESSION,INFO,TITRE ,**args):          
  """
     Ecriture de la macro MACR_ASPIC_CALC
  """
  from Accas import _F
  import types
  ier=0
#------------------------------------------------------------------
  # On recopie le mot cle affe_materiau pour le proteger
  mc_AFFE_MATERIAU=AFFE_MATERIAU
#------------------------------------------------------------------
  # On importe les definitions des commandes a utiliser dans la macro
  AFFE_MODELE      =self.get_cmd('AFFE_MODELE'     )
  AFFE_MATERIAU    =self.get_cmd('AFFE_MATERIAU'   )
  AFFE_CARA_ELEM   =self.get_cmd('AFFE_CARA_ELEM'  )
  AFFE_CHAR_THER_F =self.get_cmd('AFFE_CHAR_THER_F')
  THER_LINEAIRE    =self.get_cmd('THER_LINEAIRE'   )
  AFFE_CHAR_MECA   =self.get_cmd('AFFE_CHAR_MECA'  )
  STAT_NON_LINE    =self.get_cmd('STAT_NON_LINE'   )
  CALC_ELEM        =self.get_cmd('CALC_ELEM'       )
  POST_RELEVE_T    =self.get_cmd('POST_RELEVE_T'   )
  IMPR_TABLE       =self.get_cmd('IMPR_TABLE'      )
  POST_RCCM        =self.get_cmd('POST_RCCM'       )
  DEFI_FOND_FISS   =self.get_cmd('DEFI_FOND_FISS'  )
  CALC_THETA       =self.get_cmd('CALC_THETA'      )
  CALC_G_THETA_T   =self.get_cmd('CALC_G_THETA_T'  )
  CALC_G_LOCAL_T   =self.get_cmd('CALC_G_LOCAL_T'  )
  IMPR_RESU        =self.get_cmd('IMPR_RESU'       )

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
 
#------------------------------------------------------------------
# data
  GRMAIL= ('EQUERRE','PEAUINT','EXCORP1','EXCORP2','EXTUBU','LEVRTUBU','LEVRCORP')
  NOMNOE= ('P1_CORP','P2_CORP','P_TUBU ')
  IMPRT1= ('NUME_ORDRE','INTITULE','RESU'  ,'NOM_CHAM',
           'ABSC_CURV' ,'COOR_X'  ,'COOR_Y','COOR_Z'  ,
           'SIXX'      ,'SIXY'    ,'SIXZ'   )
  IMPRT2= ('NUME_ORDRE','INTITULE','RESU'  ,'NOM_CHAM',
           'ABSC_CURV' ,'COOR_X'  ,'COOR_Y','COOR_Z'  ,
           'SIYY'      ,'SIXY'    ,'SIYZ'   )
  IMPRT3= ('NUME_ORDRE','INTITULE','RESU'  ,'NOM_CHAM',
           'ABSC_CURV' ,'COOR_X'  ,'COOR_Y','COOR_Z'  ,
           'TEMP'   )
  APPRES= ('PEAUINT ','LEVRTUBU','LEVRCORP')
#------------------------------------------------------------------
#
  i=0
  for mate in mc_AFFE_MATERIAU:
     if mate['RCCM']=='OUI' :
        i=i+1
        MRCCM=mate['MATER']
  if i>1 :
     ier=ier+1
     self.cr.fatal("""<E> <MACR_ASPIC_CALC> vous affectez plus d un materiau contenant l option rccm""")
     return ier
#
  if (TYPE_MAILLAGE[:4]=='SAIN') and (TUBULURE==None) :
     ier=ier+1
     self.cr.fatal("""<E> <MACR_ASPIC_CALC> pour les piquages sains, TUBULURE doit etre renseigne""")
     return ier
#
  if EQUILIBRE['NOEUD'] not in ('P1_CORP','P2_CORP') :
     ier=ier+1
     self.cr.fatal("""<E> <MACR_ASPIC_CALC> EQUILIBRE[NOEUD] : on attend 'P1_CORP ' ou 'P2_CORP'""")
     return ier
#
  if PRES_REP['EFFE_FOND']=='OUI' :
     if PRES_REP['NOEUD']==None :
       ier=ier+1
       self.cr.fatal("""<E> <MACR_ASPIC_CALC> il faut preciser un noeud pour EFFE_FOND""")
       return ier
     if PRES_REP['NOEUD'] not in ('P1_CORP','P2_CORP') :
       ier=ier+1
       self.cr.fatal("""<E> <MACR_ASPIC_CALC> PRES_REP[NOEUD] : on attend 'P1_CORP' ou 'P2_CORP'""")
       return ier
     if PRES_REP['NOEUD']==EQUILIBRE['NOEUD'] :
       ier=ier+1
       self.cr.fatal("""<E> <MACR_ASPIC_CALC> on ne peut appliquer un EFFE_FOND sur PRES_REP[NOEUD] car ce noeud est bloque""")
       return ier
#
  if TORS_CORP!=None :
     for tors in TORS_CORP :
         if tors['NOEUD'] not in ('P1_CORP','P2_CORP') :
            ier=ier+1
            self.cr.fatal("""<E> <MACR_ASPIC_CALC> TORS_CORP[NOEUD] : on attend 'P1_CORP' ou 'P2_CORP'""")
            return ier
         if tors['NOEUD']==EQUILIBRE['NOEUD'] :
           ier=ier+1
           self.cr.fatal("""<E> <MACR_ASPIC_CALC> on ne peut appliquer un torseur sur TORS_CORP[NOEUD] car ce noeud est bloque""")
           return ier
#
  if (TYPE_MAILLAGE[:4]=='SAIN') and (THETA_3D!=None) :
     ier=ier+1
     self.cr.fatal("""<E> <MACR_ASPIC_CALC> si TYPE_MAILLAGE SAIN : mecanique de la rupture impossible""")
     return ier
#
  if OPTION in ('CALC_G_MAX','CALC_G_MAX_LOCAL') :
    if BORNES==None :
       ier=ier+1
       self.cr.fatal("""<E> <MACR_ASPIC_CALC> mot-clef <BORNES> obligatoire avec cette option""")
       return ier
#
  if IMPRESSION!=None :
    if IMPRESSION['FORMAT'] in ('IDEAS','CASTEM') :
      if IMPRESSION['NOM_CHAM']==None :
        ier=ier+1
        self.cr.fatal("""<E> <MACR_ASPIC_CALC> impression de resultats demandée sans preciser le nom des champs
                                               cf. la documentation utilisateur : U4.PC.20.""")
        return ier
#
#------------------------------------------------------------------
#
#     --- commande AFFE_MODELE ---
#
  if MODELE!=None : self.DeclareOut('modele',MODELE)
  mcfact=[]
  if (PRES_REP['PRES_LEVRE']=='OUI') and (TYPE_MAILLAGE[-4:]=='_DEB') :
     mcfact.append(_F(GROUP_MA=GRMAIL,     PHENOMENE='MECANIQUE',MODELISATION='3D'    ))
  else:
     mcfact.append(_F(GROUP_MA=GRMAIL[:-2],PHENOMENE='MECANIQUE',MODELISATION='3D'    ))
  mcfact.append(   _F(GROUP_MA='P1_CORP'  ,PHENOMENE='MECANIQUE',MODELISATION='DIS_TR'))
  mcfact.append(   _F(GROUP_MA='P2_CORP'  ,PHENOMENE='MECANIQUE',MODELISATION='DIS_TR'))
  mcfact.append(   _F(GROUP_MA='P_TUBU'   ,PHENOMENE='MECANIQUE',MODELISATION='DIS_TR'))
  modele = AFFE_MODELE( MAILLAGE = MAILLAGE ,
                        AFFE     = mcfact    )
  if ECHANGE!=None :                                # modele thermique
     __modthe = AFFE_MODELE( MAILLAGE = MAILLAGE ,
                             AFFE     = _F(GROUP_MA    =GRMAIL[:-2],
                                           PHENOMENE   ='THERMIQUE',
                                           MODELISATION='3D' )       )
#
#     --- commande AFFE_MATERIAU ---
#
  if CHAM_MATER!=None : self.DeclareOut('affmat',CHAM_MATER)
  mcfact=[]
  for mater in mc_AFFE_MATERIAU :
     if mater['TOUT']!=None : mcfact.append(_F(TOUT    =mater['TOUT'    ],MATER=mater['MATER'],TEMP_REF=mater['TEMP_REF']))
     else                   : mcfact.append(_F(GROUP_MA=mater['GROUP_MA'],MATER=mater['MATER'],TEMP_REF=mater['TEMP_REF']))
  affmat = AFFE_MATERIAU( MAILLAGE = MAILLAGE ,
                          MODELE   = modele ,
                          AFFE     = mcfact    )
#
#     --- commande AFFE_CARA_ELEM ---
#
  if CARA_ELEM!=None : self.DeclareOut('carael',CARA_ELEM)
  carael = AFFE_CARA_ELEM( MODELE   = modele ,
                           DISCRET  = ( _F( GROUP_MA='P1_CORP' ,
                                            CARA    ='K_TR_D_N',
                                            VALE    = ( 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 ) ),
                                        _F( GROUP_MA='P2_CORP' ,
                                            CARA    ='K_TR_D_N',
                                            VALE    = ( 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 ) ),
                                        _F( GROUP_MA='P_TUBU' ,
                                            CARA    ='K_TR_D_N',
                                            VALE    = ( 0.0 , 0.0 , 0.0 , 0.0 , 0.0 , 0.0 ) ), ) )
#
#     --- commande AFFE_CHAR_THER_F ---
#         condition aux limites
#
  if ECHANGE!=None :
     __chther = AFFE_CHAR_THER_F( MODELE = __modthe ,
                                  ECHANGE=( _F(GROUP_MA='PEAUTUBU',
                                               COEF_H  =ECHANGE['COEF_H_TUBU'],
                                               TEMP_EXT=ECHANGE['TEMP_EXT'],),
                                            _F(GROUP_MA='PEAUCORP',
                                               COEF_H  =ECHANGE['COEF_H_CORP'],
                                               TEMP_EXT=ECHANGE['TEMP_EXT'],),))
#
#     --- calcul thermique ---
#
     if RESU_THER!=None : self.DeclareOut('resuth',RESU_THER)
     mcsimp={}
     if INCREMENT['NUME_INST_INIT']!=None : mcsimp['NUME_INIT']=INCREMENT['NUME_INST_INIT']
     if INCREMENT['NUME_INST_FIN' ]!=None : mcsimp['NUME_FIN' ]=INCREMENT['NUME_INST_FIN' ]
     mcfact=_F(LIST_INST=INCREMENT['LIST_INST'],**mcsimp)
     resuth = THER_LINEAIRE( MODELE     = __modthe ,
                             CHAM_MATER = affmat ,
                             TEMP_INIT  = _F(STATIONNAIRE='OUI',),
                             EXCIT      = _F(CHARGE=__chther,),
                             INCREMENT  = mcfact, )
#
     if CHARGE!=None : self.DeclareOut('chmeth',CHARGE)
     chmeth = AFFE_CHAR_MECA( MODELE        = modele ,
                              TEMP_CALCULEE = resuth )
#
#     --- commande AFFE_CHAR_MECA ---
#         condition aux limites
#
  if     EQUILIBRE['NOEUD']=='P1_CORP' :
         NENCAS = EQUILIBRE['NOEUD']
         AEFOCO = 'EXCORP2'
         ATORCO = 'P2_CORP'
         LINTC  = 'L_INT_C2'
  elif   EQUILIBRE['NOEUD']=='P2_CORP' :
         NENCAS = EQUILIBRE['NOEUD']
         AEFOCO = 'EXCORP1'
         ATORCO = 'P1_CORP'
         LINTC  = 'L_INT_C1'
  __conlim = AFFE_CHAR_MECA( MODELE   = modele ,
                             LIAISON_ELEM  = ( _F( OPTION    ='3D_POU'  ,
                                                   GROUP_MA_1='EXCORP1',
                                                   GROUP_NO_2='P1_CORP'),
                                               _F( OPTION    ='3D_POU'  ,
                                                   GROUP_MA_1='EXCORP2' ,
                                                   GROUP_NO_2='P2_CORP'),
                                               _F( OPTION    ='3D_POU'  ,
                                                   GROUP_MA_1='EXTUBU',
                                                   GROUP_NO_2='P_TUBU'), ),
                             DDL_IMPO      =   _F( GROUP_NO  = NENCAS  ,
                                                   DX        = 0.0 ,
                                                   DY        = 0.0 ,
                                                   DZ        = 0.0 ,
                                                   DRX       = 0.0 ,
                                                   DRY       = 0.0 ,
                                                   DRZ       = 0.0 , ) )
#
#     --- commande AFFE_CHAR_MECA ---
#         chargement mecanique : pres_rep, effet de fond 
#
  motscles={}
  if (PRES_REP['PRES_LEVRE']=='OUI') and (TYPE_MAILLAGE[-4:]=='_DEB') :
      motscles['PRES_REP'  ]= _F(GROUP_MA=APPRES,   PRES=PRES_REP['PRES'])
  else :
      motscles['PRES_REP'  ]= _F(GROUP_MA=APPRES[0],PRES=PRES_REP['PRES'])
  if  PRES_REP['EFFE_FOND' ]!=None :
      motscles['EFFE_FOND' ]=(_F(GROUP_MA    ='EXTUBU  ',
                                 GROUP_MA_INT='L_INT_TU',
                                 PRES        =PRES_REP['PRES']),
                              _F(GROUP_MA    =AEFOCO,
                                 GROUP_MA_INT=LINTC,
                                 PRES        =PRES_REP['PRES']))
  __chpres = AFFE_CHAR_MECA( MODELE   = modele ,**motscles)
#
#     --- commande AFFE_CHAR_MECA ---
#         chargement mecanique : torseur sur le corps
#
  if TORS_CORP!=None:
     __chtrc = [None]*6
     i=0
     for tors in TORS_CORP :
       mcsimp={}
       if tors['FX']!=None : mcsimp['FX']=tors['FX']
       if tors['FY']!=None : mcsimp['FY']=tors['FY']
       if tors['FZ']!=None : mcsimp['FZ']=tors['FZ']
       if tors['MX']!=None : mcsimp['MX']=tors['MX']
       if tors['MY']!=None : mcsimp['MY']=tors['MY']
       if tors['MZ']!=None : mcsimp['MZ']=tors['MZ']
       mcfact=_F(GROUP_NO=ATORCO,**mcsimp)
       __chtrc[i] = AFFE_CHAR_MECA( MODELE       = modele ,
                                    FORCE_NODALE = mcfact , )
       i=i+1
#
#     --- commande AFFE_CHAR_MECA ---
#         chargement mecanique : torseur sur la tubulure
#
  if TORS_TUBU!=None:
     __chtrt = [None]*6
     i=0
     for tors in TORS_TUBU :
       mcsimp={}
       if tors['FX']!=None : mcsimp['FX']=tors['FX']
       if tors['FY']!=None : mcsimp['FY']=tors['FY']
       if tors['FZ']!=None : mcsimp['FZ']=tors['FZ']
       if tors['MX']!=None : mcsimp['MX']=tors['MX']
       if tors['MY']!=None : mcsimp['MY']=tors['MY']
       if tors['MZ']!=None : mcsimp['MZ']=tors['MZ']
       mcfact=_F(GROUP_NO='P_TUBU  ',**mcsimp)
       __chtrt[i] = AFFE_CHAR_MECA( MODELE       = modele ,
                                    FORCE_NODALE = mcfact , )
       i=i+1
#
#     --- commande STAT_NON_LINE ---
#
  motscles={}
#
  mcfex=[]  # mot clé facteur EXCIT
  mcfex.append(_F(CHARGE=__conlim,))
  if ECHANGE!=None :
     mcfex.append(_F(CHARGE=chmeth,))
  if PRES_REP['FONC_MULT']!=None :
     mcfex.append(_F(CHARGE=__chpres,FONC_MULT=PRES_REP['FONC_MULT']))
  else :
     mcfex.append(_F(CHARGE=__chpres,))
  if TORS_CORP!=None:
     i=0
     for tors in TORS_CORP :
       if tors['FONC_MULT']!=None :
          mcfex.append(_F(CHARGE=__chtrc[i],FONC_MULT=tors['FONC_MULT']))
       else :
          mcfex.append(_F(CHARGE=__chtrc[i],))
       i=i+1
  if TORS_TUBU!=None:
     i=0
     for tors in TORS_TUBU :
       if tors['FONC_MULT']!=None :
          mcfex.append(_F(CHARGE=__chtrt[i],FONC_MULT=tors['FONC_MULT']))
       else :
          mcfex.append(_F(CHARGE=__chtrt[i],))
       i=i+1
  motscles['EXCIT'] =mcfex
#
  mcfci=[]  # mot clé facteur COMP_INCR :obligatoire pour les noeuds discrets
  if COMP_INCR!=None :
    mcfci.append(_F(TOUT    ='OUI' ,RELATION=COMP_INCR['RELATION']))
  mcfci.append(  _F(GROUP_MA=NOMNOE,RELATION='ELAS'))
  motscles['COMP_INCR'] =mcfci
#
  if COMP_ELAS!=None :
    motscles['COMP_ELAS'] =_F(GROUP_MA=GRMAIL[:-2] ,RELATION=COMP_ELAS['RELATION'])
#
  dSolveur=SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
  for i in dSolveur.keys():
      if dSolveur[i]==None : del dSolveur[i]
#
  dConverg=CONVERGENCE[0].cree_dict_valeurs(CONVERGENCE[0].mc_liste)
  for i in dConverg.keys():
      if dConverg[i]==None : del dConverg[i]
#
  dNewton=NEWTON[0].cree_dict_valeurs(NEWTON[0].mc_liste)
  for i in dNewton.keys():
      if dNewton[i]==None : del dNewton[i]
#
  dRechlin=RECH_LINEAIRE[0].cree_dict_valeurs(RECH_LINEAIRE[0].mc_liste)
  for i in dRechlin.keys():
      if dRechlin[i]==None : del dRechlin[i]
#
  dIncrem=INCREMENT[0].cree_dict_valeurs(INCREMENT[0].mc_liste)
  for i in dIncrem.keys():
      if dIncrem[i]==None : del dIncrem[i]
#
  if TITRE!=None :
    motscles['TITRE'        ] =TITRE
  motscles  ['SOLVEUR'      ] =dSolveur
  motscles  ['CONVERGENCE'  ] =dConverg
  motscles  ['NEWTON'       ] =dNewton
  motscles  ['RECH_LINEAIRE'] =dRechlin
  motscles  ['INCREMENT'    ] =dIncrem
  self.DeclareOut('nomres',self.sd)
  nomres = STAT_NON_LINE( MODELE     = modele ,
                          CHAM_MATER = affmat ,
                          CARA_ELEM  = carael ,
                          INFO       = INFO   , **motscles)
#
  nomres = CALC_ELEM( reuse      = nomres,
                      RESULTAT   = nomres ,
                      TOUT_ORDRE = 'OUI'  ,
                      OPTION     = ('SIEF_ELNO_ELGA','VARI_ELNO_ELGA','EQUI_ELNO_SIGM') ,
                      INFO       = INFO ,)
#
#-----------------------------------------------------------------------
  if TYPE_MAILLAGE[:4]=='SAIN' :
#-----------------------------------------------------------------------
#
#     --- post traitement :  POST_RELEVE_T  --- azimuts droits
#
#     ----  champs de contrainte SI, SII ET SIII  ----
#
    if TYPE_MAILLAGE=='SAIN_GROS' : NBAZIM = 40
    else                          : NBAZIM = 48
    mcfact=[]
    TYPSOU=None
    if TUBULURE!=None : TYPSOU = TUBULURE['TYPE']
    for i in range(1,NBAZIM+1,PAS_AZIMUT):
      if i<10 : NUME = '0'+str(i)
      else    : NUME =     str(i)
      mcsimp={}
      if TYPSOU=='TYPE_1':
          mcsimp['REPERE' ]='CYLINDRIQUE',
          mcsimp['ORIGINE']=( 0.0 , 0.0 , 0.0 )
          mcsimp['AXE_Z'  ]=( 0.0 , 0.0 , 1.0 )
          INTITD = 'AZI_'+NUME+'_D-REP_CYL'
      else:
          mcsimp['REPERE' ]='LOCAL'
          mcsimp['VECT_Y' ]=( 0.0 , 0.0 , 1.0 )
          INTITD = 'AZI_'+NUME+'_D-REP_LOC'
      mcsimp['INTITULE' ]=INTITD
      mcsimp['GROUP_NO' ]='LD'+str(i)
      mcfact.append( _F( RESULTAT   = nomres,
                         TOUT_ORDRE ='OUI',
                         NOM_CHAM   ='SIEF_ELNO_ELGA',
                         PRECISION  =55.E-1,
                         TOUT_CMP   ='OUI',
                         OPERATION  ='EXTRACTION',**mcsimp))
    __noposd=POST_RELEVE_T(ACTION = mcfact,
                           TITRE  = '-- TRAITEMENT DES AZIMUTS DROITS --')
#
#     --- IMPR_TABLE dans un repere cylindrique ou local
#         des champs de contrainte SI, SII ET SIII
#
    if TYPSOU=='TYPE_1' : nompara=IMPRT1
    else                : nompara=IMPRT2
    IMPR_TABLE(TABLE    = __noposd,
               NOM_PARA = nompara   )
#
#     ----  Pm, Pm+Pb sur les lignes de depouillement  ----
#
    if mc_AFFE_MATERIAU[0]['RCCM']=='OUI':
      mcfact=[]
      for i in range(1,NBAZIM+1,PAS_AZIMUT):
        if i<10 : NUME = '0'+str(i)
        else    : NUME =     str(i)
        mcsimp={}
        mcsimp['INTITULE'   ]='LD'+str(i)
        mcsimp['GROUP_NO'   ]='LD'+str(i)
        mcsimp['RESULTAT'   ]=nomres
        mcsimp['TOUT_ORDRE' ]='OUI'
        mcsimp['NOM_CHAM'   ]='SIEF_ELNO_ELGA'
        mcsimp['PRECISION'  ]=55.E-1
        mcsimp['TOUT_CMP'   ]='OUI'
        mcsimp['OPERATION'  ]='EXTRACTION'
        mcfact.append( _F(**mcsimp) )
      __prelsd=POST_RELEVE_T(ACTION=mcfact)
      __pmpbsd=POST_RCCM(OPTION         = 'PM_PB',
                         TYPE_RESU_MECA = 'EVOLUTION',
                         TYPE_RESU      = 'VALE_MAX',
                         MATER          = MRCCM,
                         TRANSITOIRE = _F(TABL_RESU_MECA = __prelsd,),
                         TITRE       = '-- TRAITEMENT DES AZIMUTS DROITS --',)
      IMPR_TABLE(TABLE = __pmpbsd, )
#
#     ----  champ de temperature, si il a ete calcule, sur les lignes de depouillement  ----
#
    if ECHANGE!=None :
      mcfact=[]
      for i in range(1,NBAZIM+1,PAS_AZIMUT):
        if i<10 : NUME = '0'+str(i)
        else    : NUME =     str(i)
        mcsimp={}
        mcsimp['GROUP_NO'   ]='LD'+str(i)
        mcsimp['RESULTAT'   ]=resuth
        mcsimp['TOUT_ORDRE' ]='OUI'
        mcsimp['NOM_CHAM'   ]='TEMP'
        mcsimp['PRECISION'  ]=55.E-1
        mcsimp['TOUT_CMP'   ]='OUI'
        mcsimp['INTITULE'   ]='AZI_'+NUME+'_D'
        mcsimp['OPERATION'  ]='EXTRACTION'
        mcfact.append( _F(**mcsimp) )
      __rthazd=POST_RELEVE_T(ACTION=mcfact)
      IMPR_TABLE(TABLE = __rthazd, )
#
#     ----  parametres caracterisant la distribution de temperature,
#           si elle a ete calculee, dans l epaisseur du ligament  ----
#
      mcfact=[]
      for i in range(1,NBAZIM+1,PAS_AZIMUT):
        if i<10 : NUME = '0'+str(i)
        else    : NUME =     str(i)
        mcsimp={}
        mcsimp['GROUP_NO'   ]='LD'+str(i)
        mcsimp['RESULTAT'   ]=resuth
        mcsimp['TOUT_ORDRE' ]='OUI'
        mcsimp['NOM_CHAM'   ]='TEMP'
        mcsimp['PRECISION'  ]=55.E-1
        mcsimp['TOUT_CMP'   ]='OUI'
        mcsimp['INTITULE'   ]='AZI_'+NUME+'_D'
        mcsimp['OPERATION'  ]='MOYENNE'
        mcfact.append( _F(**mcsimp) )
      __rmothd=POST_RELEVE_T(ACTION=mcfact)
      IMPR_TABLE(TABLE = __rmothd, )
#
#-----------------------------------------------------------------------
#
#     --- post traitement :  POST_RELEVE_T  --- azimuts inclines
#     --- champs de contrainte SI, SII ET SIII  ----
#
    mcfact=[]
    for i in range(1,NBAZIM+1,PAS_AZIMUT):
      if i<10 : NUME = '0'+str(i)
      else    : NUME =     str(i)
      mcsimp={}
      mcsimp['GROUP_NO'   ]='LI'+str(i)
      mcsimp['RESULTAT'   ]=nomres
      mcsimp['TOUT_ORDRE' ]='OUI'
      mcsimp['NOM_CHAM'   ]='SIEF_ELNO_ELGA'
      mcsimp['PRECISION'  ]=55.E-1
      mcsimp['TOUT_CMP'   ]='OUI'
      mcsimp['REPERE'     ]='LOCAL'
      mcsimp['VECT_Y'     ]=( 0.0 , 0.0 , 1.0 )
      mcsimp['INTITULE'   ]='AZI_'+NUME+'_I-REP_LOC'
      mcsimp['OPERATION'  ]='EXTRACTION'
      mcfact.append( _F(**mcsimp) )
    __noposi=POST_RELEVE_T(ACTION=mcfact)
    IMPR_TABLE(TABLE = __noposi, )
#
#     ----  Pm, Pm+Pb sur les lignes de depouillement  ----
#
    if mc_AFFE_MATERIAU[0]['RCCM']=='OUI':
      mcfact=[]
      for i in range(1,NBAZIM+1,PAS_AZIMUT):
        if i<10 : NUME = '0'+str(i)
        else    : NUME =     str(i)
        mcsimp={}
        mcsimp['INTITULE'   ]='LI'+str(i)
        mcsimp['GROUP_NO'   ]='LI'+str(i)
        mcsimp['RESULTAT'   ]=nomres
        mcsimp['TOUT_ORDRE' ]='OUI'
        mcsimp['NOM_CHAM'   ]='SIEF_ELNO_ELGA'
        mcsimp['PRECISION'  ]=55.E-1
        mcsimp['TOUT_CMP'   ]='OUI'
        mcsimp['OPERATION'  ]='EXTRACTION'
        mcfact.append( _F(**mcsimp) )
      __prelsi=POST_RELEVE_T(ACTION=mcfact)
      __pmpbsi=POST_RCCM(OPTION         = 'PM_PB',
                         TYPE_RESU_MECA = 'EVOLUTION',
                         TYPE_RESU      = 'VALE_MAX',
                         MATER          = MRCCM,
                         TRANSITOIRE = _F(TABL_RESU_MECA = __prelsi,),
                         TITRE       = '-- TRAITEMENT DES AZIMUTS INCLINES --',)
      IMPR_TABLE(TABLE = __pmpbsi, )
#
#     ----  champs de temperature,si il a ete calcule, sur les lignes de depouillement  ----
#
    if ECHANGE!=None :
      mcfact=[]
      for i in range(1,NBAZIM+1,PAS_AZIMUT):
        if i<10 : NUME = '0'+str(i)
        else    : NUME =     str(i)
        mcsimp={}
        mcsimp['GROUP_NO'   ]='LI'+str(i)
        mcsimp['RESULTAT'   ]=resuth
        mcsimp['TOUT_ORDRE' ]='OUI'
        mcsimp['NOM_CHAM'   ]='TEMP'
        mcsimp['PRECISION'  ]=55.E-1
        mcsimp['TOUT_CMP'   ]='OUI'
        mcsimp['INTITULE'   ]='AZI_'+NUME+'_I'
        mcsimp['OPERATION'  ]='EXTRACTION'
        mcfact.append( _F(**mcsimp) )
      __rthazi=POST_RELEVE_T(ACTION=mcfact)
      IMPR_TABLE(TABLE    = __rthazi,
                 NOM_PARA = IMPRT3 )
#
#     ----  parametres caracterisant la distribution de temperature,
#           si elle a ete calculee, dans l epaisseur du ligament  ----
#
      mcfact=[]
      for i in range(1,NBAZIM+1,PAS_AZIMUT):
        if i<10 : NUME = '0'+str(i)
        else    : NUME =     str(i)
        mcsimp={}
        mcsimp['GROUP_NO'   ]='LI'+str(i)
        mcsimp['RESULTAT'   ]=resuth
        mcsimp['TOUT_ORDRE' ]='OUI'
        mcsimp['NOM_CHAM'   ]='TEMP'
        mcsimp['PRECISION'  ]=55.E-1
        mcsimp['TOUT_CMP'   ]='OUI'
        mcsimp['INTITULE'   ]='AZI_'+NUME+'_I'
        mcsimp['OPERATION'  ]='MOYENNE'
        mcfact.append( _F(**mcsimp) )
      __rmothi=POST_RELEVE_T(ACTION=mcfact)
      IMPR_TABLE(TABLE = __rmothi, )
#
#-----------------------------------------------------------------------
  elif TYPE_MAILLAGE[:4]=='FISS' :
#-----------------------------------------------------------------------
#
    NOMGRO=[]
    NOMGRE=[]
    TABMA8=[]
    NOMMA =[]
    if TYPE_MAILLAGE in ('FISS_COUR_DEB','FISS_LONG_DEB','FISS_AXIS_DEB','FISS_COUR_NONDEB') :
       NBFIS = 1
       NOMGRO.append(('P_FON1' ,'P_FIS1' ),)
       NOMGRE.append(('P_FON2' ,'P_FIS2' ),)
       TABMA8.append('FONDFISS')
       if TYPE_MAILLAGE in ('FISS_COUR_DEB','FISS_LONG_DEB') : FERME=0
       else :
          FERME=1
          NOMMA.append('MAIL_ORI')
    elif TYPE_MAILLAGE in ('FISS_LONG_NONDEB','FISS_AXIS_NONDEB') :
       NBFIS = 2
       NOMGRO.append(('P_FON1' ,'P_FIS1' ),)
       NOMGRE.append(('P_FON2' ,'P_FIS2' ),)
       NOMGRO.append(('PI_FON1','PI_FIS1'),)
       NOMGRE.append(('PI_FON2','PI_FIS2'),)
       TABMA8.append('FOND_SUP')
       TABMA8.append('FOND_INF')
       if TYPE_MAILLAGE=='FISS_LONG_NONDEB' : FERME=0
       else :
          FERME=1
          NOMMA.append('MA_ORI_S')
          NOMMA.append('MA_ORI_I')
#
    if ECHANGE!=None:
#
#     ----  champs de temperature en fond de fissure
#           si il a ete calcule, cas 1 fond de fissure  ----
#
      if NBFIS==1:
        __rthfis=POST_RELEVE_T(ACTION=_F(GROUP_NO   ='FONDFISS',
                                         RESULTAT   =resuth,
                                         TOUT_ORDRE ='OUI',
                                         NOM_CHAM   ='TEMP',
                                         PRECISION  =55.E-1,
                                         TOUT_CMP   ='OUI',
                                         INTITULE   ='FONDFISS',
                                         OPERATION  ='EXTRACTION',))
        IMPR_TABLE(TABLE = __rthfis, )
#
#     ----  champs de temperature en fond de fissure
#           si il a ete calcule, cas 2 fonds de fissure  ----
#
      elif NBFIS==2:
#
        __rthfis1=POST_RELEVE_T(ACTION=_F(GROUP_NO   ='FOND_SUP',
                                          RESULTAT   =resuth,
                                          TOUT_ORDRE ='OUI',
                                          NOM_CHAM   ='TEMP',
                                          PRECISION  =55.E-1,
                                          TOUT_CMP   ='OUI',
                                          INTITULE   ='FOND_SUP',
                                          OPERATION  ='EXTRACTION',))
        IMPR_TABLE(TABLE = __rthfis1, )
        __rthfis2=POST_RELEVE_T(ACTION=_F(GROUP_NO   ='FOND_INF',
                                          RESULTAT   =resuth,
                                          TOUT_ORDRE ='OUI',
                                          NOM_CHAM   ='TEMP',
                                          PRECISION  =55.E-1,
                                          TOUT_CMP   ='OUI',
                                          INTITULE   ='FOND_INF',
                                          OPERATION  ='EXTRACTION',))
        IMPR_TABLE(TABLE = __rthfis2, )
#
#        boucle sur le nombre de fond de fissure
#
    fond3d = [None]*2
    for j in range(NBFIS):
      if FOND_FISS_1!=None : self.DeclareOut('fond3d_0',FOND_FISS_1)
      if FOND_FISS_2!=None : self.DeclareOut('fond3d_1',FOND_FISS_2)
#
#          --- commande DEFI_FOND_FISS ---
#
      motscles={}
      if not FERME:
         motscles['FOND_FISS']=_F(GROUP_MA     =TABMA8[j],
                                  GROUP_NO_ORIG=NOMGRO[j][0],
                                  GROUP_NO_EXTR=NOMGRE[j][0],)
         motscles['VECT_GRNO_ORIG']=NOMGRO[j]
         motscles['VECT_GRNO_EXTR']=NOMGRE[j]
      else:
         if TYPE_MAILLAGE.find('AXIS')!=-1  : grnoorig=NOMGRE[j][0]
#                  si AXIS, P_FON1 est remplace par P_FON2 pour
#                  fermer le fond de fissure
         else                               : grnoorig=NOMGRO[j][0]
         motscles['FOND_FERME']=_F(GROUP_MA     =TABMA8[j],
                                   GROUP_NO_ORIG=grnoorig,
                                   GROUP_MA_ORIG=NOMMA[j],)
      fond3d[j]=DEFI_FOND_FISS( MAILLAGE  = MAILLAGE,
                                LEVRE_SUP = _F(GROUP_MA='LEVRCORP',),
                                LEVRE_INF = _F(GROUP_MA='LEVRTUBU',),**motscles)
      if THETA_3D!=None:
        for tht3d in THETA_3D : 
#
#          --- commande CALC_THETA ---
#
          __theta = CALC_THETA( MODELE    = modele,
                                FOND_FISS = fond3d[j],
                                THETA_3D  = _F( TOUT    = 'OUI',
                                                MODULE  =  1.0 ,
                                                R_INF   = tht3d['R_INF'],
                                                R_SUP   = tht3d['R_SUP'], ) )
#
#          --- commande CALC_G_THETA_T ---
#
          montit = 'G_THETA AVEC R_INF = '+str(tht3d['R_INF'])+' ET R_SUP = '+str(tht3d['R_SUP'])
          motscles={}
          if COMP_ELAS!=None:  motscles['COMP_ELAS']=  _F(TOUT     = 'OUI',
                                                          RELATION = COMP_ELAS['RELATION'],)
          if COMP_INCR!=None:  motscles['COMP_INCR']=  _F(RELATION = COMP_INCR['RELATION'],)
          print motscles
          __gtheta = CALC_G_THETA_T( MODELE     = modele,
                                     CHAM_MATER = affmat,
                                     THETA      = __theta,
                                     RESULTAT   = nomres,
                                     TOUT_ORDRE = 'OUI',
                                     TITRE      = montit,**motscles)
          IMPR_TABLE(TABLE = __gtheta, )
#
#           recherche du g max
#
          if OPTION=='CALC_G_MAX' :
            if BORNES!=None:
              mcfact=[]
              for born in BORNES :
                mcfact.append(_F( NUME_ORDRE = born['NUME_ORDRE'] ,
                                  VALE_MIN   = born['VALE_MIN'  ] ,
                                  VALE_MAX   = born['VALE_MAX'  ]   ) )
              __gbil = CALC_G_THETA_T( MODELE     = modele,
                                       CHAM_MATER = affmat,
                                       THETA      = __theta,
                                       RESULTAT   = nomres,
                                       TOUT_ORDRE = 'OUI',
                                       COMP_ELAS  =  _F(TOUT     = 'OUI',
                                                        RELATION = COMP_ELAS['RELATION'],),
                                       TITRE    = montit,
                                       OPTION   = OPTION,
                                       BORNES   = mcfact,)
              IMPR_TABLE(TABLE = __gbil, )
#
#          --- commande CALC_G_LOCAL_T ---
#
          montit = 'G_LOCAL AVEC R_INF = '+str(tht3d['R_INF'])+' ET R_SUP = '+str(tht3d['R_SUP'])
          motscles={}
          if COMP_ELAS!=None:  motscles['COMP_ELAS'    ]=  _F(TOUT     = 'OUI',
                                                              RELATION = COMP_ELAS['RELATION'],)
          if FERME:
                               motscles['LISSAGE_THETA']= 'LAGRANGE'
                               motscles['LISSAGE_G'    ]= 'LAGRANGE'
          __glocal = CALC_G_LOCAL_T( MODELE     = modele,
                                     CHAM_MATER = affmat,
                                     FOND_FISS  = fond3d[j],
                                     RESULTAT   = nomres,
                                     TOUT_ORDRE = 'OUI',
                                     R_INF      = tht3d['R_INF'],
                                     R_SUP      = tht3d['R_SUP'],
                                     TITRE      = montit,**motscles)
#
#          recherche du g max local
#
          if OPTION=='CALC_G_MAX_LOCAL' :
            if BORNES!=None:
              motscles={}
              mcfact=[]
              if FERME:
                        motscles['LISSAGE_THETA']= 'LAGRANGE'
                        motscles['LISSAGE_G'    ]= 'LAGRANGE'
              for born in BORNES :
                mcfact.append(_F( NUME_ORDRE = born['NUME_ORDRE'] ,
                                  VALE_MIN   = born['VALE_MIN'  ] ,
                                  VALE_MAX   = born['VALE_MAX'  ]   ) )
              motscles['BORNES']=mcfact
              __glbil = CALC_G_LOCAL_T( MODELE     = modele,
                                        CHAM_MATER = affmat,
                                        FOND_FISS  = fond3d[j],
                                        RESULTAT   = nomres,
                                        TOUT_ORDRE = 'OUI',
                                        COMP_ELAS  =  _F(TOUT     = 'OUI',
                                                         RELATION = COMP_ELAS['RELATION'],),
                                        TITRE      = montit,
                                        OPTION     = 'CALC_G_MAX',
                                        R_INF      = tht3d['R_INF'],
                                        R_SUP      = tht3d['R_SUP'],**motscles)
              IMPR_TABLE(TABLE = __glbil, )
#
#     --- commande IMPR_RESU  ---
#
  if IMPRESSION!=None:
    mcfresu =[]
    motscles={}
    motsclei={}
    if IMPRESSION['FORMAT'] in ('IDEAS','CASTEM') :
      ncham   =[]
      if IMPRESSION['NOM_CHAM']!=None :
         if type(IMPRESSION['NOM_CHAM']) in (types.TupleType,types.ListType) : ncham= IMPRESSION['NOM_CHAM']
         else                                                                : ncham=[IMPRESSION['NOM_CHAM'],]
      if    len(ncham)==3       : motscles['NOM_CHAM'  ]=('DEPL','EQUI_ELNO_SIGM')
      elif (len(ncham)==1) and (ncham[0][:4]!='TEMP')  :
                                  motscles['NOM_CHAM'  ]= ncham[0]['NOM_CHAM']
      elif (len(ncham)==2) and (ncham[0][:4]!='TEMP') and (ncham[1][:4]!='TEMP')  :
                                  motscles['NOM_CHAM'  ]=(ncham[0]['NOM_CHAM'],ncham[1]['NOM_CHAM'])
      elif (len(ncham)==2) and (ncham[0][:4]=='TEMP')  :
                                  motscles['NOM_CHAM'  ]= ncham[1]['NOM_CHAM']
      elif (len(ncham)==2) and (ncham[1][:4]=='TEMP') :
                                  motscles['NOM_CHAM'  ]= ncham[0]['NOM_CHAM']
      if   IMPRESSION['TOUT_ORDRE']!=None :
                                  motscles['TOUT_ORDRE']= IMPRESSION['TOUT_ORDRE']
      elif IMPRESSION['NUME_ORDRE']!=None :
                                  motscles['NUME_ORDRE']= IMPRESSION['NUME_ORDRE']
      elif IMPRESSION['INST']!=None :
                                  motscles['INST'      ]= IMPRESSION['INST']
    if IMPRESSION['FORMAT']=='IDEAS' :
                                  motsclei['VERSION'   ]= IMPRESSION['VERSION']
    if IMPRESSION['FORMAT']=='CASTEM' :
                                  motsclei['NIVE_GIBI' ]= IMPRESSION['NIVE_GIBI']
    mcfresu.append(_F(MAILLAGE=MAILLAGE,RESULTAT=nomres,**motscles))
    if ECHANGE!=None:
      motscles={}
      if IMPRESSION['FORMAT'] in ('IDEAS','CASTEM') :
        if    len(ncham)==3       : motscles['NOM_CHAM'  ]=('TEMP',)
        elif (len(ncham)==1) and (ncham[0][:4]=='TEMP') :
                                    motscles['NOM_CHAM'  ]= ncham[0]['NOM_CHAM']
        elif (len(ncham)==2) and (ncham[0][:4]=='TEMP') :
                                    motscles['NOM_CHAM'  ]= ncham[0]['NOM_CHAM']
        elif (len(ncham)==2) and (ncham[1][:4]=='TEMP') :
                                    motscles['NOM_CHAM'  ]= ncham[1]['NOM_CHAM']
        if   IMPRESSION['TOUT_ORDRE']!=None :
                                    motscles['TOUT_ORDRE']= IMPRESSION['TOUT_ORDRE']
        elif IMPRESSION['NUME_ORDRE']!=None :
                                    motscles['NUME_ORDRE']= IMPRESSION['NUME_ORDRE']
        elif IMPRESSION['INST']!=None :
                                    motscles['INST'      ]= IMPRESSION['INST']
      if IMPRESSION['FORMAT']=='IDEAS' :
                                    motsclei['VERSION'   ]= IMPRESSION['VERSION']
      if IMPRESSION['FORMAT']=='CASTEM' :
                                    motsclei['NIVE_GIBI' ]= IMPRESSION['NIVE_GIBI']
      mcfresu.append(_F(RESULTAT=nomres,**motscles))
    IMPR_RESU( MODELE = modele,
               RESU   = mcfresu,
               FORMAT=IMPRESSION['FORMAT'],**motsclei)
#
  return ier
