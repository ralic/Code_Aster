# coding=utf-8
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

import aster
from Accas import _F

def CHAINAGE_INIT(self,args,motscles):

   MODELE_MECA = args['MODELE_MECA']
   MODELE_HYDR = args['MODELE_HYDR']

   # On importe les definitions des commandes a utiliser dans la macro
   CREA_MAILLAGE   = self.get_cmd('CREA_MAILLAGE')
   PROJ_CHAMP      = self.get_cmd('PROJ_CHAMP')

   MATR_MH=PROJ_CHAMP(METHODE='COLLOCATION',MODELE_1=MODELE_MECA,MODELE_2=MODELE_HYDR,PROJECTION='NON',**motscles)

   iret,ibid,nom_mail = aster.dismoi('F','NOM_MAILLA',MODELE_HYDR.nom,'MODELE')
   nom_mail=nom_mail.strip()
   __maillage_h = self.get_concept(nom_mail)

   _maillin=CREA_MAILLAGE(MAILLAGE=__maillage_h,
                            QUAD_LINE=_F(TOUT='OUI',),**motscles);

   MATR_HM1=PROJ_CHAMP(METHODE='COLLOCATION',  MODELE_1=MODELE_HYDR, MAILLAGE_2=_maillin, PROJECTION='NON',**motscles)
   MATR_HM2=PROJ_CHAMP(METHODE='COLLOCATION',  MAILLAGE_1=_maillin, MODELE_2=MODELE_MECA, PROJECTION='NON',**motscles)

   return MATR_MH, MATR_HM1, MATR_HM2
