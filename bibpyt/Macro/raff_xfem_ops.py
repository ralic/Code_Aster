#@ MODIF raff_xfem_ops Macro  DATE 16/11/2009   AUTEUR COURTOIS M.COURTOIS 
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


     
def raff_xfem_ops(self,FISSURE,INFO,**args):
   """
   Macro RAFF_XFEM
   Calcul de la distance au fond de fissure le plus proche
   """
   import aster
   import string
   import copy
   import math
   import Numeric
   from types import ListType, TupleType
   from Accas import _F
   from SD.sd_xfem import sd_fiss_xfem
   from Execution.E_JDC import JDC
   EnumTypes = (ListType, TupleType)

   macro = 'RAFF_XFEM'
   from Accas               import _F
   from Utilitai.Utmess     import  UTMESS

   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type cham_no_sdaster ou derive)
   self.DeclareOut('chamout', self.sd)
   
   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   FORMULE       = self.get_cmd('FORMULE')
   CREA_CHAMP    = self.get_cmd('CREA_CHAMP')
   DETRUIRE      = self.get_cmd('DETRUIRE')

#  recuperation de la liste des fissures
   if type(FISSURE) == tuple :
      nbfiss = len(FISSURE)
   else :
      nbfiss = 1

#  formule distance pour une fissure: 1/exp(r)
   __MDISTF=FORMULE(NOM_PARA=('X1','X2'),VALE= '1./exp(sqrt(X1**2+X2**2))');
#  formule distance pour une interface: 1/exp(lsn)
   __MDISTI=FORMULE(NOM_PARA=('X1'),VALE= '1./exp(sqrt(X1**2))');

   __CERR= [None]*nbfiss
   list_err=[]
   list_nom_cmp=[]
   for_max = 'max('

   for i in range(0,nbfiss) :

      if nbfiss == 1 :
         fiss = FISSURE
      else :
         fiss = FISSURE[i]

      # recuperation du type de discontinuite :'FISSURE' ou 'INTERFACE'
      # si FISSURE   : l'erreur est la distance au fond de fissure
      # si INTERFACE : l'erreur est la distance a l'interface 
      iret,ibid,typ_ds = aster.dismoi('F','TYPE_DISCONTINUITE',fiss.nom,'FISS_XFEM')
      typ_ds=typ_ds.rstrip()

      # extraction des champs level sets
      __CHLN=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_R',
                      OPERATION='EXTR',
                      NOM_CHAM='LNNO',
                      FISSURE=fiss);

      # on recupere le concept maillage associe a la level set normale
      iret,ibid,nom_ma = aster.dismoi('F','NOM_MAILLA',__CHLN.nom,'CHAM_NO')
      nom_ma=nom_ma.strip()
      MA = self.get_concept(nom_ma)

      if typ_ds == 'FISSURE' : 
         __CHLTB=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_R',
                          OPERATION='EXTR',
                          NOM_CHAM='LTNO',
                          FISSURE=fiss);

         # on renomme le composante X1 en X2
         __CHLT=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_R',
                         OPERATION='ASSE',
                         MAILLAGE=MA,
                         ASSE=_F(TOUT='OUI',
                                 CHAM_GD = __CHLTB,
                                 NOM_CMP='X1',
                                 NOM_CMP_RESU='X2',),
                           );

         DETRUIRE(CONCEPT=_F(NOM=__CHLTB),INFO=1) 

      # On affecte à chaque noeud du maillage MA la formule __MDISTF ou __MDISTI
      if typ_ds == 'FISSURE' : 
         __CHFOR=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_F',
                           OPERATION='AFFE',
                           MAILLAGE=MA,
                           AFFE=_F(TOUT='OUI',
                                   NOM_CMP='X1',
                                   VALE_F=__MDISTF,),
                          );
      elif typ_ds == 'INTERFACE' : 
         __CHFOR=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_F',
                           OPERATION='AFFE',
                           MAILLAGE=MA,
                           AFFE=_F(TOUT='OUI',
                                   NOM_CMP='X1',
                                   VALE_F=__MDISTI,),
                          );  
      
      # on evalue en tout noeud le champ de formules
      if typ_ds == 'FISSURE' : 
         __CERRB=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_R',
                       OPERATION='EVAL',
                       CHAM_F=__CHFOR,
                       CHAM_PARA=(__CHLN,__CHLT,));

         DETRUIRE(CONCEPT=_F(NOM=__CHLT),INFO=1) 

      elif typ_ds == 'INTERFACE' : 
         __CERRB=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_R',
                       OPERATION='EVAL',
                       CHAM_F=__CHFOR,
                       CHAM_PARA=(__CHLN,));

      DETRUIRE(CONCEPT=_F(NOM=__CHLN),INFO=1) 
      DETRUIRE(CONCEPT=_F(NOM=__CHFOR),INFO=1) 

      # champ d'Erreur de la fissure i
      __CERR[i]=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_R',
                          OPERATION='ASSE',
                          MAILLAGE=MA,
                          ASSE=_F(TOUT='OUI',
                                  CHAM_GD = __CERRB,
                                  NOM_CMP='X1',
                                  NOM_CMP_RESU='X'+str(i+1),
                                ),
                        ); 

      list_err.append(__CERR[i])
      list_nom_cmp.append('X'+str(i+1))
      for_max = for_max+'X'+str(i+1)+','

      DETRUIRE(CONCEPT=_F(NOM=__CERRB),INFO=1) 

   # si nbfiss = 1, c'est directement X1
   # si nbfiss > 1 : on prend le max des erreurs de chaque fissure 
   for_max = for_max+')'

   if nbfiss == 1 :
      __Erreur=FORMULE(NOM_PARA=(list_nom_cmp),VALE= 'X1');
   else :
      __Erreur=FORMULE(NOM_PARA=(list_nom_cmp),VALE= for_max);

   # Définition de l'erreur en chaque noeud du maillage           
   __CHFORM=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_F',
                     OPERATION='AFFE',
                     MAILLAGE=MA,
                     AFFE=_F(TOUT='OUI',
                             NOM_CMP='X1',
                             VALE_F=__Erreur,),
                    );

   # champ de sortie
   chamout=CREA_CHAMP(TYPE_CHAM='NOEU_NEUT_R',
                     OPERATION='EVAL',
                     CHAM_F=__CHFORM,
                     CHAM_PARA=(list_err));

   for i in range(0,nbfiss) :
      DETRUIRE(CONCEPT=_F(NOM=__CERR[i]),INFO=1) 

   DETRUIRE(CONCEPT=_F(NOM=__MDISTF),INFO=1) 
   DETRUIRE(CONCEPT=_F(NOM=__MDISTI),INFO=1) 
   DETRUIRE(CONCEPT=_F(NOM=__Erreur),INFO=1) 
   DETRUIRE(CONCEPT=_F(NOM=__CHFORM),INFO=1) 

   return
 
