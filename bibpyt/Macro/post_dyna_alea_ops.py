#@ MODIF post_dyna_alea_ops Macro  DATE 17/07/2007   AUTEUR REZETTE C.REZETTE 
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

def post_dyna_alea_ops(self,INTE_SPEC,NUME_VITE_FLUI,TOUT_ORDRE,NUME_ORDRE_I,
                       NOEUD_I,OPTION,MOMENT,TITRE,INFO,**args):
   import aster
   from types import ListType, TupleType
   EnumTypes = (ListType, TupleType)
   from Accas               import _F
   from Utilitai.Utmess     import U2MESS as UTMESS
   from Utilitai.t_fonction import t_fonction
   from Utilitai.Table      import Table
   import Numeric
   import math
   from math import pi,sqrt
   
   commande='POST_DYNA_ALEA'

   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_sdaster ou dérivé) est tab
   self.DeclareOut('tabout', self.sd)
   
   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   CALC_TABLE    = self.get_cmd('CALC_TABLE')
   IMPR_TABLE    = self.get_cmd('IMPR_TABLE')
   RECU_FONCTION = self.get_cmd('RECU_FONCTION')
   IMPR_FONCTION = self.get_cmd('IMPR_FONCTION')

   intespec=INTE_SPEC.EXTR_TABLE()

#  ------------------------------------------------------------------
#  Liste des moments spectraux
#  repérer le type de l'interspectre et son nom
#                1- concept interspectre
#                2- table de table d interspectre

   if 'NUME_VITE_FLUI' in intespec.para :
      if TOUT_ORDRE!=None :
         jnuor=intespec['NUME_VITE_FLUI'].values()['NUME_VITE_FLUI']
         jvite=dict([(i,0) for i in jnuor]).keys()
      else :
        jvite=[NUME_VITE_FLUI,]
   else :
      jvite  =[None]

#  ------------------------------------------------------------------
#  Repérer les couples d'indices selectionnés
#  vérification de l'égalité du nombre d indices en i et j

   if NUME_ORDRE_I!=None :
     l_ind_i=NUME_ORDRE_I
     l_ind_j=args['NUME_ORDRE_J']
     if type(l_ind_i) not in EnumTypes : l_ind_i=[l_ind_i]
     if type(l_ind_j) not in EnumTypes : l_ind_j=[l_ind_j]
     if len(l_ind_i)!=len(l_ind_j) :
        UTMESS('F','PROBA0_8')
     listpara=['NUME_ORDRE_I','NUME_ORDRE_J']
     listtype=['I','I']
     dicotabl={'NUME_ORDRE_I'  : l_ind_i  ,\
               'NUME_ORDRE_J'  : l_ind_j  , }
   elif NOEUD_I!=None :
     l_ind_i=NOEUD_I
     l_ind_j=args['NOEUD_J']
     l_cmp_i=args['NOM_CMP_I']
     l_cmp_j=args['NOM_CMP_J']
     if type(l_cmp_i) not in EnumTypes : l_cmp_i=[l_cmp_i]
     if type(l_cmp_j) not in EnumTypes : l_cmp_j=[l_cmp_j]
     if type(l_ind_i) not in EnumTypes : l_ind_i=[l_ind_i]
     if type(l_ind_j) not in EnumTypes : l_ind_j=[l_ind_j]
     if len(l_ind_i)!=len(l_ind_j) :
        UTMESS('F','PROBA0_8')
     if len(l_cmp_i)!=len(l_cmp_j) :
        UTMESS('F','PROBA0_9')
     if len(l_ind_i)!=len(l_cmp_i) :
        UTMESS('F','PROBA0_10')
     listpara=['NOEUD_I','NOEUD_J','NOM_CMP_I','NOM_CMP_J']
     listtype=['K8','K8','K8','K8',]
     dicotabl={'NOEUD_I'  : l_ind_i,\
               'NOEUD_J'  : l_ind_j,\
               'NOM_CMP_I': l_cmp_i,\
               'NOM_CMP_J': l_cmp_j }
#  ------------------------------------------------------------------
#  Cas de tous les indices centraux

   elif OPTION!=None :
      if 'NUME_ORDRE_I' in intespec.para :
         inuor=intespec['NUME_ORDRE_I'].values()['NUME_ORDRE_I']
         imode=dict([(i,0) for i in inuor]).keys()
         l_ind_i=imode
         l_ind_j=imode
         listpara=['NUME_ORDRE_I','NUME_ORDRE_J']
         listtype=['I','I']
         dicotabl={'NUME_ORDRE_I'  : l_ind_i  ,\
                   'NUME_ORDRE_J'  : l_ind_j  , }
      else :
         if 'NUME_VITE_FLUI' in intespec.para :
            intespec=intespec.NUME_VITE_FLUI==jvite[0]
         l_ind_i=intespec['NOEUD_I'].values()['NOEUD_I']
         l_ind_j=intespec['NOEUD_J'].values()['NOEUD_J']
         if len(l_ind_i)!=len(l_ind_j) :
            UTMESS('F','PROBA0_8')
         l_cmp_i=intespec['NOM_CMP_I'].values()['NOM_CMP_I']
         l_cmp_j=intespec['NOM_CMP_J'].values()['NOM_CMP_J']
         if (len(l_ind_i)!=len(l_cmp_i) or len(l_ind_j)!=len(l_cmp_j)) :
            UTMESS('F','PROBA0_10')
         l_l=zip(zip(l_ind_i,l_cmp_i),zip(l_ind_j,l_cmp_j))
         l_ind_i=[]
         l_ind_j=[]
         l_cmp_i=[]
         l_cmp_j=[]
         for ai,aj in l_l :
             if ai==aj :
                l_ind_i.append(ai[0])
                l_ind_j.append(aj[0])
                l_cmp_i.append(ai[1])
                l_cmp_j.append(aj[1])
         listpara=['NOEUD_I','NOEUD_J','NOM_CMP_I','NOM_CMP_J']
         listtype=['K8','K8','K8','K8',]
         dicotabl={'NOEUD_I'  : l_ind_i*len(jvite)  ,\
                   'NOEUD_J'  : l_ind_j*len(jvite)  ,\
                   'NOM_CMP_I': l_cmp_i*len(jvite)  ,\
                   'NOM_CMP_J': l_cmp_j*len(jvite) }

   if jvite[0]!=None :
      listpara.append('NUME_VITE_FLUI')
      listtype.append('I')
      dicotabl['NUME_VITE_FLUI']=[]
#  ------------------------------------------------------------------
#  Liste des moments spectraux

   l_moments=[0,1,2,3,4]
   if MOMENT!=None :
      l_moments=l_moments+list(MOMENT)
      l_moments=dict([(i,0) for i in l_moments]).keys()

#  ------------------------------------------------------------------
#  Boucle sur les tables

   l_ind=zip(l_ind_i,l_ind_j)
   for vite in jvite :
     if INFO==2 :
        texte='POUR LA MATRICE INTERSPECTRALE '+INTE_SPEC.nom+'\n'
        aster.affiche('MESSAGE',texte)
     for ind in l_ind :
        mcfact=[]
        if vite!=None : 
          dicotabl['NUME_VITE_FLUI'].append(vite)
          mcfact.append(_F(NOM_PARA='NUME_VITE_FLUI',VALE_I=vite))
        if 'NOEUD_I' in listpara :
          mcfact.append(_F(NOM_PARA='NOEUD_I',VALE_K=ind[0]))
          mcfact.append(_F(NOM_PARA='NOEUD_I',VALE_K=ind[1]))
          if INFO==2 :
             aster.affiche('MESSAGE','INDICES :'+ind[0]+' - '+ind[1]+'\n')
        else :
          mcfact.append(_F(NOM_PARA='NUME_ORDRE_I',VALE_I=ind[0]))
          mcfact.append(_F(NOM_PARA='NUME_ORDRE_J',VALE_I=ind[1]))
        if INFO==2 :
             aster.affiche('MESSAGE','INDICES :'+str(ind[0])+' - '\
                                                +str(ind[1])+'\n')
        __fon1=RECU_FONCTION(TABLE        = INTE_SPEC,
                             NOM_PARA_TABL= 'FONCTION_C',
                             FILTRE       = mcfact, )
        val  = __fon1.Valeurs()
        fvalx= Numeric.array(val[0])
        fvaly= Numeric.array(val[1])
        frez = fvalx[0]

#--- moments spectraux

        val_mom={}
        for i_mom in l_moments :
            trapz     = Numeric.zeros(len(fvaly),Numeric.Float)
            trapz[0]  = 0.
            valy      = fvaly*(2*pi*fvalx)**i_mom
            trapz[1:] = (valy[1:]+valy[:-1])/2*(fvalx[1:]-fvalx[:-1])
            prim_y    = Numeric.cumsum(trapz)
            val_mom[i_mom] = prim_y[-1]
        for i_mom in l_moments :
          chmo='LAMBDA_'+str(i_mom).zfill(2)
          if dicotabl.has_key(chmo) : dicotabl[chmo].append(val_mom[i_mom])
          else :
                 dicotabl[chmo]=[val_mom[i_mom],]
                 listpara.append(chmo)
                 listtype.append('R')

#--- fonctions statistiques

        pstat  = {'ECART'           :0.,\
                  'NB_PASS_ZERO_P_S':0.,\
                  'NB_EXTREMA_P_S'  :0.,\
                  'FACT_IRRE'       :0.,\
                  'FREQ_APPAR'      :0.,}
        if (NUME_VITE_FLUI or frez>=0.) :
#--- cas NUME_VITE_FLUI, seule la partie positive du spectre est utilisée
#--- Il faut donc doubler lambda  pour calculer le bon écart type
            pstat['ECART'] = sqrt(val_mom[0]*2.)
        else :
            pstat['ECART'] = sqrt(val_mom[0])
        if abs(val_mom[2])>=1e-20 :
              pstat['NB_EXTREMA_P_S'] = 1./pi*sqrt(val_mom[4]/val_mom[2])
        if abs(val_mom[0])>=1e-20 :
           pstat['NB_PASS_ZERO_P_S'] = 1./pi*sqrt(val_mom[2]/val_mom[0])
           pstat['FREQ_APPAR'] = 0.5*pstat['NB_PASS_ZERO_P_S']
           if abs(val_mom[4])>=1e-20 :
              pstat['FACT_IRRE'] = sqrt( val_mom[2]*val_mom[2]/val_mom[0]/val_mom[4])

        for key in pstat.keys(): 
          if dicotabl.has_key(key) : dicotabl[key].append(pstat[key])
          else :
                 dicotabl[key]=[pstat[key],]
                 listpara.append(key)
                 listtype.append('R')

#--- construction de la table produite

   mcfact=[]
   for i in range(len(listpara)) :
      if listtype[i]=='R':
         mcfact.append(_F(PARA=listpara[i] ,LISTE_R=dicotabl[listpara[i]] ))
      if listtype[i]=='K8':
         mcfact.append(_F(PARA=listpara[i] ,LISTE_K=dicotabl[listpara[i]] ))
      if listtype[i]=='I':
         mcfact.append(_F(PARA=listpara[i] ,LISTE_I=dicotabl[listpara[i]] ))
   tabout = CREA_TABLE(LISTE=mcfact,TITRE = 'POST_DYNA_ALEA concept : '+self.sd.nom)

   return ier
