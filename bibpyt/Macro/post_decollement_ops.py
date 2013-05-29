# coding=utf-8
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

########################

# fonction qui renvoie une liste de 0/1 en fonction du signe des éléments de la liste listIN:
def fctZeroUn(listIN):
   listOUT=[]
   for n in listIN:
      if n>0: listOUT.append(1);
      else: listOUT.append(0);
   return listOUT;

def post_decollement_ops(self,RESULTAT,NOM_CHAM,NOM_CMP,GROUP_MA,INFO,**args):
  """
     Corps de la macro POST_DECOLLEMENT
  """
  ier=0
  import aster
  import os,string,types
  from Accas import _F
  from Utilitai.Utmess import  UTMESS, MasquerAlarme, RetablirAlarme


  ### On importe les definitions des commandes a utiliser dans la macro
  CREA_CHAMP     = self.get_cmd('CREA_CHAMP')
  POST_ELEM      = self.get_cmd('POST_ELEM')
  CREA_TABLE     = self.get_cmd('CREA_TABLE')
  POST_RELEVE_T  = self.get_cmd('POST_RELEVE_T')
  DEFI_GROUP     = self.get_cmd('DEFI_GROUP')
  DEFI_MATERIAU  = self.get_cmd('DEFI_MATERIAU')
  CREA_RESU      = self.get_cmd('CREA_RESU')
  AFFE_MODELE    = self.get_cmd('AFFE_MODELE')
  AFFE_MATERIAU  = self.get_cmd('AFFE_MATERIAU')
  IMPR_TABLE     = self.get_cmd('IMPR_TABLE')

  ### Comptage commandes + déclaration concept sortant
  self.set_icmd(1)
  self.DeclareOut('C_out',self.sd)
  
  ### on recupere le concept maillage 
  iret,ibid,nom_mo = aster.dismoi('F','MODELE',RESULTAT.nom,'RESULTAT')
  iret,ibid,nom_ma = aster.dismoi('F','NOM_MAILLA',nom_mo.strip(),'MODELE')
  MAILLAGE = self.get_concept(nom_ma.strip())
 
  ### Creation du groupe de noeuds 'PDECOL'
  DEFI_GROUP(reuse=MAILLAGE,MAILLAGE=MAILLAGE,
             DETR_GROUP_NO= _F(NOM='PDECOL',),
             CREA_GROUP_NO=_F(GROUP_MA=GROUP_MA,NOM='PDECOL'),
             ALARME='NON',)

  
  ### le modele 3D ne va contenir que des mailles de peau : on masque les alarmes
  MasquerAlarme('CALCULEL2_63')
  MasquerAlarme('CALCULEL2_64')
  
  ### model restreint au GROUP_MA
  __model=AFFE_MODELE(MAILLAGE=MAILLAGE,
                  AFFE=_F(  GROUP_MA = GROUP_MA,PHENOMENE = 'MECANIQUE',
                            MODELISATION = '3D'),)
                            
  ### le modele 3D ne va contenir que des mailles de peau : on retablit les alarmes
  RetablirAlarme('CALCULEL2_63')
  RetablirAlarme('CALCULEL2_64')
  
  ### Calcul de la surface du GROUP_MA : __surf
  __unit = CREA_CHAMP(OPERATION='AFFE',TYPE_CHAM='NOEU_NEUT_R',MODELE=__model,
                      AFFE=_F(GROUP_NO='PDECOL',NOM_CMP='X1',VALE=1.0),)

  __chpg0 = CREA_CHAMP(PROL_ZERO='OUI',MODELE=__model,OPERATION='DISC',TYPE_CHAM='ELGA_NEUT_R',
                      CHAM_GD=__unit)

  __mater0 = DEFI_MATERIAU(ELAS=_F(E=210000000.0,NU=0.3),)

  __chmat0 = AFFE_MATERIAU(MODELE=__model,MAILLAGE=MAILLAGE,AFFE=_F(TOUT='OUI',MATER=__mater0),)

  __resu0 = CREA_RESU(OPERATION='AFFE',TYPE_RESU='EVOL_ELAS',NOM_CHAM='VARI_ELGA',
                      AFFE=_F(CHAM_MATER=__chmat0,MODELE=__model,CHAM_GD=__chpg0,INST=0.0),)

  __tbSurf0 = POST_ELEM(RESULTAT=__resu0,INST=0.0,MODELE=__model,
                       INTEGRALE=_F(NOM_CHAM='VARI_ELGA',NOM_CMP='X1',GROUP_MA=GROUP_MA),)

  __surf=__tbSurf0.EXTR_TABLE().values()['INTE_X1'][0]

  __linst=RESULTAT.LIST_VARI_ACCES()['INST']

  ### Calcul de la surface des noeuds décollés
  __pct=[]
  
  for  inst in __linst:

     __dep = CREA_CHAMP(OPERATION='EXTR',RESULTAT=RESULTAT,TYPE_CHAM='NOEU_'+NOM_CHAM[:4]+'_R',
                         INST=inst,NOM_CHAM=NOM_CHAM)

     __tb1  = POST_RELEVE_T(ACTION=_F(OPERATION='EXTRACTION',GROUP_NO='PDECOL', INTITULE=GROUP_MA,
                                      CHAM_GD=__dep,NOM_CMP=NOM_CMP),) 

     __col = fctZeroUn(__tb1.EXTR_TABLE().values()[NOM_CMP])

     __tb2 = CREA_TABLE(LISTE=(_F(LISTE_K=__tb1.EXTR_TABLE().values()['NOEUD'],PARA='NOEUD'),
                               _F(LISTE_R=__col,PARA='X1'),),)

     __ch = CREA_CHAMP(OPERATION='EXTR',TYPE_CHAM='NOEU_NEUT_R',TABLE=__tb2,MAILLAGE=MAILLAGE)
 
     __chg = CREA_CHAMP(MODELE=__model,OPERATION='DISC',TYPE_CHAM='ELGA_NEUT_R',
                         PROL_ZERO='OUI',CHAM_GD=__ch)
  
     __resu = CREA_RESU(OPERATION='AFFE',TYPE_RESU='EVOL_ELAS',NOM_CHAM='VARI_ELGA',
                      AFFE=_F(CHAM_MATER=__chmat0,MODELE=__model,CHAM_GD=__chg,INST=0.0),)

     __tb3 = POST_ELEM(RESULTAT=__resu,INST=0.0,MODELE=__model,
                          INTEGRALE=_F(NOM_CHAM='VARI_ELGA',NOM_CMP='X1',GROUP_MA=GROUP_MA),)

     __su2=__tb3.EXTR_TABLE().values()['INTE_X1'][0]

     __pct.append(100.0*__su2/__surf);


  C_out=CREA_TABLE(LISTE=(_F(LISTE_R=__linst,PARA='INST'),
                          _F(LISTE_R=__pct,PARA='%DECOL'),),)
  if INFO > 1:
     IMPR_TABLE(UNITE=6,
                TABLE=C_out)
  return ier
