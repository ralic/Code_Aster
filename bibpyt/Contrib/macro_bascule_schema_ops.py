#@ MODIF macro_bascule_schema_ops Contrib  DATE 18/02/2013   AUTEUR GREFFET N.GREFFET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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

def macro_bascule_schema_ops (self,MODE,MATE,CARA,
                        INCR_I,INCR_E,
                        SCH_TEMPS_I,SCH_TEMPS_E,SCH_TEMPS_EQ,
                        C_INCR_I,C_INCR_E,
                        EXC_T,NEWT,INIT,TP_BAS,SCH_INI,EQUILIBRAGE,**args):
  ier=0
  import copy
  import aster
  import string
  import types
  from Accas import _F
  from Noyau.N_utils import AsType
  from Utilitai.Utmess     import  UTMESS,MasquerAlarme, RetablirAlarme
  # On importe les definitions des commandes a utiliser dans la macro
  DYNA_NON_LINE  =self.get_cmd('DYNA_NON_LINE')
  CREA_CHAMP     =self.get_cmd('CREA_CHAMP')
  DEFI_LIST_REEL =self.get_cmd('DEFI_LIST_REEL')
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
  # Le concept sortant (de type evol_noli) est nommé
  # 'nomres' dans le contexte de la macro
  self.DeclareOut('nomres',self.sd)
  #
  #
  dexct=[]
  for j in EXC_T :
      dexct.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dexct[-1].keys():
          if dexct[-1][i]==None : del dexct[-1][i]
  #
  dComp_incri=[]
  for j in C_INCR_I :
      dComp_incri.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dComp_incri[-1].keys():
          if dComp_incri[-1][i]==None : del dComp_incri[-1][i]
  #
  dComp_incre=[]
  for j in C_INCR_E :
      dComp_incre.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dComp_incre[-1].keys():
          if dComp_incre[-1][i]==None : del dComp_incre[-1][i]
  #
  dincri=[]
  for j in INCR_I :
      dincri.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dincri[-1].keys():
          if dincri[-1][i]==None : del dincri[-1][i]
  #
  dincre=[]
  for j in INCR_E :
      dincre.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dincre[-1].keys():
          if dincre[-1][i]==None : del dincre[-1][i]
  #
  dschi=[]
  for j in SCH_TEMPS_I :
      dschi.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dschi[-1].keys():
          if dschi[-1][i]==None : del dschi[-1][i]
  #
  dsche=[]
  for j in SCH_TEMPS_E :
      dsche.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dsche[-1].keys():
          if dsche[-1][i]==None : del dsche[-1][i]
  #
  dscheq=[]
  for j in SCH_TEMPS_EQ :
      dscheq.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dscheq[-1].keys():
          if dscheq[-1][i]==None : del dscheq[-1][i]
  #
  dnew=[]
  for j in NEWT :
      dnew.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dnew[-1].keys():
          if dnew[-1][i]==None : del dnew[-1][i]
  #
  dini=[]
  for j in INIT :
      dini.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dini[-1].keys():
          if dini[-1][i]==None : del dini[-1][i]
  #
  dequi=[]
  for j in EQUILIBRAGE :
      dequi.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dequi[-1].keys():
          if dequi[-1][i]==None : del dequi[-1][i]
  #
  __L0   = TP_BAS['VALE']
  dincri1=copy.copy(dincri)
  dincri1[-1]['INST_FIN']= __L0[0]
  #
  __dtimp=dequi[-1]['DT_IMP']
  __dtexp=dequi[-1]['DT_EXP']
  #
  __dim=(-1)*len(dComp_incri)
  __lis=range(0,__dim,-1)
  __non_lin='NON'
  for i in __lis :
      if (dComp_incri[i]['RELATION']!='DIS_CHOC' and dComp_incri[i]['RELATION']!='ELAS'):
          __non_lin='OUI'
          break
  #
  #

  # alarme de DYNA_NON_LINE si les mot-cles de COMP_INCR sont renseignes a tort
  MasquerAlarme('COMPOR1_70')

  if SCH_INI=='IMPLICITE':
       dincri1=copy.copy(dincri)
       dincri1[-1]['INST_FIN']= __L0[0]
       nomres=DYNA_NON_LINE(MODELE      =MODE,
                            CHAM_MATER  =MATE,
                            CARA_ELEM   =CARA,
                            EXCIT       =dexct,
                            COMP_INCR   =dComp_incri,
                            INCREMENT   =dincri1,
                            SCHEMA_TEMPS=dschi,
                            NEWTON=dnew,
                            ETAT_INIT=dini,           )
       __prc = 'IMPLICITE'
  #
  if SCH_INI=='EXPLICITE':
       dincre1=copy.copy(dincre)
       dincre1[-1]['INST_FIN']= __L0[0]
       nomres=DYNA_NON_LINE(MODELE      =MODE,
                            CHAM_MATER  =MATE,
                            MASS_DIAG   ='OUI',
                            CARA_ELEM   =CARA,
                            EXCIT       =dexct,
                            COMP_INCR   =dComp_incre,
                            INCREMENT   =dincre1,
                            SCHEMA_TEMPS=dsche,
                            NEWTON=dnew,
                            ETAT_INIT=dini,           )

       __prc = 'EXPLICITE'

 #
  __nb=len(__L0)
  j = 1
  while 1:
     #
     if __prc=='IMPLICITE' :
        __Ue=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                        TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='DEPL', INST=__L0[j-1],)
        #
        __Ve=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                        TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='VITE', INST=__L0[j-1],)
        #
        __Ae=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                        TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='ACCE', INST=__L0[j-1],)
        #
        __Ce=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                        TYPE_CHAM='ELGA_SIEF_R', NOM_CHAM='SIEF_ELGA', INST=__L0[j-1],)
        #
        __Vae=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                         TYPE_CHAM='ELGA_VARI_R', NOM_CHAM='VARI_ELGA', INST=__L0[j-1],)
        dincre1=copy.copy(dincre)
        dincre1[-1]['INST_INIT']= __L0[j-1]
        if ( j < __nb ) :
           dincre1[-1]['INST_FIN'] = __L0[j]
        else :
           del dincre1[-1]['INST_FIN']
        nomres=DYNA_NON_LINE(reuse=nomres,
                             MODELE=MODE,
                             CHAM_MATER=MATE,
                             CARA_ELEM=CARA,
                             EXCIT=dexct,
                             ETAT_INIT=_F(DEPL=__Ue, VITE=__Ve, ACCE=__Ae,
                                          SIGM=__Ce, VARI=__Vae,),
                             COMP_INCR=dComp_incre,
                             INCREMENT=dincre1,
                             SCHEMA_TEMPS=dsche,
                             NEWTON=dnew,)
        #
        __prc='EXPLICITE'
        bool = (j!=(__nb))
        if (not bool): break
        j = j + 1
        #
     if __prc=='EXPLICITE' :
            # calcul sur la zone de recouvrement
            print('calcul d''une solution explicite stabilisée')
            __U1=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                            TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='DEPL', INST=__L0[j-1],)
            #
            __V1=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                            TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='VITE', INST=__L0[j-1],)
            #
            __A1=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                            TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='ACCE', INST=__L0[j-1],)
            #
            __C1=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                            TYPE_CHAM='ELGA_SIEF_R', NOM_CHAM='SIEF_ELGA', INST=__L0[j-1],)
            #
            __Va1=CREA_CHAMP(OPERATION='EXTR', PRECISION=1.E-7, RESULTAT=nomres,
                             TYPE_CHAM='ELGA_VARI_R', NOM_CHAM='VARI_ELGA', INST=__L0[j-1],)
            #
            __lrec=DEFI_LIST_REEL(DEBUT=__L0[j-1],
                                  INTERVALLE=_F(JUSQU_A=(__L0[j-1])+(10*(__dtexp)),
                                                PAS=__dtexp),)
            schema_equi = dscheq[-1]['SCHEMA']
            if ( schema_equi == 'TCHAMWA') or (schema_equi == 'DIFF_CENT') :
              masse_diago = 'OUI'
            else :
              masse_diago = 'NON'            
            __u_rec=DYNA_NON_LINE(MODELE=MODE,
                                  CHAM_MATER=MATE,
                                  MASS_DIAG=masse_diago,
                                  CARA_ELEM=CARA,
                                  EXCIT=dexct,
                                  ETAT_INIT=_F(DEPL=__U1, VITE=__V1, ACCE=__A1,
                                               SIGM=__C1, VARI=__Va1,),
                                  COMP_INCR=dComp_incre,
                                  INCREMENT=_F(LIST_INST=__lrec,
                                               INST_INIT=__L0[j-1],
                                               INST_FIN=(__L0[j-1])+(10*(__dtexp))),
                                  SCHEMA_TEMPS=dscheq,
                                  NEWTON=dnew,)
            #
            __Ui =CREA_CHAMP(OPERATION='EXTR',        PRECISION=1.E-7,      RESULTAT=__u_rec,
                             TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='DEPL',      INST=(__L0[j-1])+(10*(__dtexp)),)
            #
            __Vi =CREA_CHAMP(OPERATION='EXTR',        PRECISION=1.E-7,      RESULTAT=__u_rec,
                             TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='VITE',      INST=(__L0[j-1])+(10*(__dtexp)),)
            #
            __Ai =CREA_CHAMP(OPERATION='EXTR',        PRECISION=1.E-7,      RESULTAT=__u_rec,
                             TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='ACCE',      INST=(__L0[j-1])+(10*(__dtexp)),)
            #
            # equilibrage du premier pas implicite
            print('equilibrage du pas explicite stabilisée')
            dincri1=copy.copy(dincri)
            dincri1[-1]['INST_FIN'] = ((__L0[j-1])+(10*(__dtexp)))
            dincri1[-1]['INST_INIT']=  (__L0[j-1])
            nomres=DYNA_NON_LINE(reuse=nomres,
                                 MODELE=MODE,
                                 CHAM_MATER=MATE,
                                 CARA_ELEM=CARA,
                                 EXCIT=dexct,
                                 ETAT_INIT=_F(DEPL=__Ui, VITE=__Vi, ACCE=__Ai,
                                              SIGM=__C1, VARI=__Va1,),
                                 COMP_INCR=dComp_incri,
                                 INCREMENT=dincri1,
                                 SCHEMA_TEMPS=dschi,
                                 NEWTON=dnew,)
            #
            __Ui =CREA_CHAMP(OPERATION='EXTR',        PRECISION=1.E-7,      RESULTAT=nomres,
                             TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='DEPL',      INST=(__L0[j-1])+(10*(__dtexp)),)
            #
            __Vi =CREA_CHAMP(OPERATION='EXTR',        PRECISION=1.E-7,      RESULTAT=nomres,
                             TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='VITE',      INST=(__L0[j-1])+(10*(__dtexp)),)
            #
            __Ai =CREA_CHAMP(OPERATION='EXTR',        PRECISION=1.E-7,      RESULTAT=nomres,
                             TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM='ACCE',      INST=(__L0[j-1])+(10*(__dtexp)),)
            #
            __Ci =CREA_CHAMP(OPERATION='EXTR',        PRECISION=1.E-7,      RESULTAT=nomres,
                             TYPE_CHAM='ELGA_SIEF_R', NOM_CHAM='SIEF_ELGA', INST=(__L0[j-1])+(10*(__dtexp)),)
            #
            __Vai=CREA_CHAMP(OPERATION='EXTR',        PRECISION=1.E-7,      RESULTAT=nomres,
                             TYPE_CHAM='ELGA_VARI_R', NOM_CHAM='VARI_ELGA', INST=(__L0[j-1])+(10*(__dtexp)),)
            #
            print('calcul implicite après équilibrage')
            dincri1=copy.copy(dincri)
            dincri1[-1]['INST_INIT']= ((__L0[j-1])+(10*(__dtexp)))
            if ( j < __nb ) :
               dincri1[-1]['INST_FIN'] = __L0[j]
            else :
               del dincri1[-1]['INST_FIN']
            nomres=DYNA_NON_LINE(reuse=nomres,
                                 MODELE=MODE,
                                 CHAM_MATER=MATE,
                                 CARA_ELEM=CARA,
                                 EXCIT=dexct,
                                 ETAT_INIT=_F(DEPL=__Ui, VITE=__Vi, ACCE=__Ai,
                                              SIGM=__Ci, VARI=__Vai,
                                             ),
                                 COMP_INCR=dComp_incri,
                                 INCREMENT=dincri1,
                                 SCHEMA_TEMPS=dschi,
                                 NEWTON=dnew,)
            #
            __prc='IMPLICITE'
            bool = (j!=(__nb))
            if (not bool): break
            j = j + 1
  #
  RetablirAlarme('COMPOR1_70')
  return ier
