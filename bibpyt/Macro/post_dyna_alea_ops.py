# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

import random
import string
from types import ListType, TupleType
from math  import pi,sqrt,log,exp

EnumTypes = (ListType, TupleType)


def post_dyna_alea_ops(self,INTERSPECTRE,FRAGILITE,TITRE,INFO,**args):
   import numpy as NP
   import aster
   from Accas                 import _F
   from Utilitai.Utmess       import UTMESS
   from Cata_Utils.t_fonction import t_fonction
   from Utilitai.Table        import Table

   commande='POST_DYNA_ALEA'

   ier = 0
   # La macro compte pour 1 dans la numérotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_sdaster ou dérivé) est tab
   self.DeclareOut('tabout', self.sd)

   # On importe les définitions des commandes a utiliser dans la macro
   # Le nom de la variable doit être obligatoirement le nom de la commande
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   CALC_TABLE    = self.get_cmd('CALC_TABLE')
   IMPR_TABLE    = self.get_cmd('IMPR_TABLE')
   RECU_FONCTION = self.get_cmd('RECU_FONCTION')
   IMPR_FONCTION = self.get_cmd('IMPR_FONCTION')
   DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
   DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
   CALC_FONCTION  = self.get_cmd('CALC_FONCTION')


#  ------------------------------------------------------------------
#---------algorithme d'optimisation pour le  maximum de vraisemblance
   def vrais(x):
      am=x[0]
      beta=x[1]
#       assert am >0.000, 'optimize.py: beta negatif'
#       assert am >0.000, 'optimize.py: am negatif'
      if am <=0.000:
          am=0.01
      if beta <=0.000:
          beta=0.001
      res=1.0
      for k in range(Nbval):
         ai=liste_indic[k]
         xi=float(liste_def[k])
         val=log(ai/am)
         pfa=normcdf(val/beta)
         f0=pfa**xi*(1.-pfa)**(1-xi)
         res=res*f0
      return -res

   def boot_vrais(x):
      am=x[0]
      beta=x[1]
      res=1.0
      for k in range(Nbval):
         ai=liste_indic[list_rand[k]]
         xi=float(liste_def[list_rand[k]])
         val=log(ai/am)
         pfa=normcdf(val/beta)
         f0=pfa**xi*(1.-pfa)**(1-xi)
         res=res*f0
      return -res

#  ------------------------------------------------------------------
#  OPTION FRAGILITE
# ------------------------------------------------------------------
   if FRAGILITE !=None :
      from Utilitai.optimize   import fmin
      from Utilitai.stats   import normcdf

      if FRAGILITE['LIST_PARA'] != None :
         liste_a = FRAGILITE['LIST_PARA'].sdj.VALE.get()
      elif FRAGILITE['VALE'] != None :
         liste_a =FRAGILITE['VALE']


      Nba=len(liste_a)
      lpfa=[]
      tab2 = FRAGILITE['TABL_RESU'].EXTR_TABLE()
      dicta = tab2.values()

      if dicta.has_key('DEFA') :
         liste_def = dicta['DEFA']
      else:
         UTMESS('F','TABLE0_1',valk=('DEFA'))
      if dicta.has_key('PARA_NOCI') :
        liste_indic = dicta['PARA_NOCI']
      else:
        UTMESS('F','TABLE0_1',valk=('PARA_NOCI'))

      Nbval=len(liste_indic)

      test1 = NP.equal(None,liste_indic)
      test2 = NP.equal(None,liste_def)
      if test1.any() or test2.any():
         UTMESS('F', 'TABLE0_14', valk=('DEFA', 'PARA_NOCI'))

      # estimation paramètres
      x0 = [FRAGILITE['AM_INI'],FRAGILITE['BETA_INI']]
      xopt = fmin(vrais,x0)

      texte='PARAMETRES Am, beta ESTIMES : '+str(xopt)+'\n'
      aster.affiche('MESSAGE',texte)      #print 'parametres Am, beta estimes: ', xopt

      #courbe de fragilité
      vec_a=NP.array(liste_a)
      vecval=(NP.log(vec_a/xopt[0]))/xopt[1]
      for m in range(Nba):
         lpfa.append(normcdf(vecval[m]))

      # table sortie

      mcfact=[]
      if  TITRE !=None :
           mcfact.append(_F(PARA= 'TITRE' , LISTE_K= TITRE  ))

      mcfact.append(_F(PARA= 'AM' ,LISTE_R=xopt[0] ))
      mcfact.append(_F(PARA= 'BETA' ,LISTE_R=xopt[1] ))
      mcfact.append(_F(PARA= 'PARA_NOCI' ,LISTE_R =liste_a  ))
      mcfact.append(_F(PARA= 'PFA' ,LISTE_R = lpfa ))


      # si calcul de fractiles (intervalles de confiance) par bootstrap

      x0 = xopt
      if FRAGILITE['FRACTILE']!= None :
         if INFO==2 :
            texte='FRACTILES A CALCULER PAR BOOTSTRAP '+ str(FRAGILITE['FRACTILE']) +'\n'
            aster.affiche('MESSAGE',texte)
         if FRAGILITE['NB_TIRAGE']!= None :
            Nboot = FRAGILITE['NB_TIRAGE']
            if Nboot > Nbval :
               UTMESS('F','PROBA0_11')     #assert Nboot <= Nbval , 'ERREUR: nombre de tirages demandes trop grand'
         else:
            Nboot = Nbval

         list_fonc = []
         lfract =FRAGILITE['FRACTILE']
         __F1=[None]*Nbval
         __ABS=[None]*Nbval
         __ORDO=[None]*Nbval

         for kb in range(Nboot) : #in range(Nbval)

            lpfa = []
            list_rand = []

            for kb2 in range(Nbval) :
               list_rand.append(random.randint(0,Nbval-1))

            xopt = fmin(boot_vrais,x0)
            if INFO==2 :
               texte1='BOOTSTRAP TIRAGE '+ str(kb+1)
               texte2='  PARAMETRES Am, beta ESTIMES : '+str(xopt)+'\n'
               aster.affiche('MESSAGE',texte1)
               aster.affiche('MESSAGE',texte2)
            vecval=(NP.log(vec_a/xopt[0]))/xopt[1]
            for m in range(Nba):
               lpfa.append(normcdf(vecval[m]))

            __ABS[kb]=DEFI_LIST_REEL( VALE = liste_a  );
            __ORDO[kb]=DEFI_LIST_REEL( VALE = lpfa );

            __F1[kb]=DEFI_FONCTION(  NOM_PARA='PGAZ',
                                     NOM_RESU = 'PFA',
                                     VALE_PARA = __ABS[kb],
                                     VALE_FONC = __ORDO[kb],);
            list_fonc.append(__F1[kb],)


         #__FRACTILE = [None]*len(lfract)
         liste = [None]*len(lfract)
         for kb in range(len(lfract)):
            __FRACTILE=CALC_FONCTION(FRACTILE=_F(FONCTION=(list_fonc),
                             FRACT=lfract[kb]), );
            liste[kb]= __FRACTILE.Ordo()
            mcfact.append(_F(PARA= str(lfract[kb]) ,LISTE_R =liste[kb]  ))


      #   fin FRAGILITE
      tabout = CREA_TABLE(LISTE=mcfact,TITRE = 'POST_DYNA_ALEA concept : '+self.sd.nom)

#  ------------------------------------------------------------------


#  ------------------------------------------------------------------
#  OPTION INTESPEC
# ------------------------------------------------------------------
   if INTERSPECTRE !=None :

      INTE_SPEC=INTERSPECTRE['INTE_SPEC']

      NUME_ORDRE_I=INTERSPECTRE['NUME_ORDRE_I']
      NOEUD_I=INTERSPECTRE['NOEUD_I']
      OPTION=INTERSPECTRE['OPTION']
      MOMENT=INTERSPECTRE['MOMENT']
      DUREE=INTERSPECTRE['DUREE']


      # table résultat
      tabres = Table(titr='POST_DYNA_ALEA concept : %s' % self.sd.nom)

#     ------------------------------------------------------------------
#     Liste des moments spectraux
#     repérer le type de l'interspectre et son nom
#                   1- concept interspectre
#                   2- table de table d interspectre

      intespec = INTE_SPEC.nom.ljust(8)

#     ------------------------------------------------------------------
#     Repérer les couples d'indices selectionnés
#     vérification de l'égalité du nombre d indices en i et j

      if NUME_ORDRE_I!=None :
        l_ind_i=NUME_ORDRE_I
        if type(l_ind_i) not in EnumTypes : l_ind_i=[l_ind_i]
        l_ind_j=INTERSPECTRE['NUME_ORDRE_J']
        if l_ind_j:
          if type(l_ind_j) not in EnumTypes : l_ind_j=[l_ind_j]
          if len(l_ind_i)!=len(l_ind_j) :
             UTMESS('F','PROBA0_8')
        else:
          l_ind_j=NUME_ORDRE_I
          if type(l_ind_j) not in EnumTypes : l_ind_j=[l_ind_j]
          
        # paramètres fixes de la table
        tabres.add_para(['NUME_ORDRE_I','NUME_ORDRE_J'], 'I')
      elif NOEUD_I!=None :
        l_ind_i=NOEUD_I
        l_cmp_i=INTERSPECTRE['NOM_CMP_I']
        if type(l_ind_i) not in EnumTypes : l_ind_i=[l_ind_i]
        if type(l_cmp_i) not in EnumTypes : l_cmp_i=[l_cmp_i]
        l_ind_j=INTERSPECTRE['NOEUD_J']
        if l_ind_j:
          l_cmp_j=INTERSPECTRE['NOM_CMP_J']
          if type(l_ind_j) not in EnumTypes : l_ind_j=[l_ind_j]
          if type(l_cmp_j) not in EnumTypes : l_cmp_j=[l_cmp_j]
          if len(l_ind_i)!=len(l_ind_j) :
             UTMESS('F','PROBA0_8')
          if len(l_cmp_i)!=len(l_cmp_j) :
             UTMESS('F','PROBA0_9')
          if len(l_ind_i)!=len(l_cmp_i) :
             UTMESS('F','PROBA0_10')
        else:
          l_ind_j=NOEUD_I
          l_cmp_j=INTERSPECTRE['NOM_CMP_I']
          if type(l_ind_j) not in EnumTypes : l_ind_j=[l_ind_j]
          if type(l_cmp_j) not in EnumTypes : l_cmp_j=[l_cmp_j]
        # paramètres fixes de la table
        tabres.add_para(['NOEUD_I','NOEUD_J','NOM_CMP_I','NOM_CMP_J'], 'K8')

#     ------------------------------------------------------------------
#     Cas de tous les indices

      elif OPTION=='TOUT' :
           if aster.getvectjev(intespec+'.NUMI'):
             l_ind_i = aster.getvectjev(intespec+'.NUMI')
             l_ind_j = aster.getvectjev(intespec+'.NUMJ')
             tabres.add_para(['NUME_ORDRE_I','NUME_ORDRE_J'], 'I')
           if aster.getvectjev(intespec+'.NOEI'):
             l_ind_i = aster.getvectjev(intespec+'.NOEI')
             l_ind_j = aster.getvectjev(intespec+'.NOEJ')
             l_cmp_i = aster.getvectjev(intespec+'.CMPI')
             l_cmp_j = aster.getvectjev(intespec+'.CMPJ')
             tabres.add_para(['NOEUD_I','NOEUD_J','NOM_CMP_I','NOM_CMP_J'], 'K8')

#     ------------------------------------------------------------------
#     Cas de tous les indices centraux

      elif OPTION=='DIAG' :
           if aster.getvectjev(intespec+'.NUMI'):
             l_ind_i_all = aster.getvectjev(intespec+'.NUMI')
             l_ind_j_all = aster.getvectjev(intespec+'.NUMJ')
             l_ind_i = [ind for i,ind in enumerate(l_ind_i_all) if l_ind_j_all[i]==ind]
             l_ind_j = l_ind_i
             tabres.add_para(['NUME_ORDRE_I','NUME_ORDRE_J'], 'I')
           if aster.getvectjev(intespec+'.NOEI'):
             l_ind_i_all = aster.getvectjev(intespec+'.NOEI')
             l_ind_j_all = aster.getvectjev(intespec+'.NOEJ')
             l_ind_i = [ind for i,ind in enumerate(l_ind_i_all) if l_ind_j_all[i]==ind]
             l_ind_j = l_ind_i
             l_cmp_i_all = aster.getvectjev(intespec+'.CMPI')
             l_cmp_j_all = aster.getvectjev(intespec+'.CMPJ')
             l_cmp_i = [cmpi for i,cmpi in enumerate(l_cmp_i_all) if l_cmp_j_all[i]==cmpi]
             l_cmp_j = l_cmp_i
             tabres.add_para(['NOEUD_I','NOEUD_J','NOM_CMP_I','NOM_CMP_J'], 'K8')

#     ------------------------------------------------------------------
#     Liste des moments spectraux

      l_moments=[0,1,2,3,4]
      if MOMENT!=None :
         l_moments.extend(list(MOMENT))
         l_moments=list(set(l_moments))

#     ------------------------------------------------------------------
#     Boucle sur les fonctions

      if aster.getvectjev(intespec+'.NOEI') :
         l_ind=zip(l_ind_i,l_ind_j, l_cmp_i,l_cmp_j)
      else :
         l_ind=zip(l_ind_i, l_ind_j )


      # pour la présentation de la table finale, on stocke le nbre de paramètres "initiaux"
      nbpara0 = len(tabres.para)

      if INFO==2 :
           texte='POUR LA MATRICE INTERSPECTRALE '+INTE_SPEC.nom+'\n'
           aster.affiche('MESSAGE',texte)
      for ind in l_ind :
           dlign = {}
           dlrecu = {}
           if NOEUD_I :
             i_foncstat = ind[0] == ind[1] and  ind[2] == ind[3]
             dlign['NOEUD_I'], dlign['NOEUD_J'], dlign['NOM_CMP_I'], dlign['NOM_CMP_J'] = \
                  ind[0], ind[1], ind[2], ind[3]
             if ind[0] == ind[1] and  ind[2] == ind[3]:
                 dlrecu['NOEUD_I'], dlrecu['NOM_CMP_I'] = ind[0], ind[2]
             else:
                 dlrecu['NOEUD_I'], dlrecu['NOEUD_J'], dlrecu['NOM_CMP_I'], \
                  dlrecu['NOM_CMP_J'] = ind[0], ind[1], ind[2], ind[3]
             if INFO==2 :
                aster.affiche('MESSAGE','INDICES :'+ind[0]+' - '+ind[1])
                aster.affiche('MESSAGE','INDICES :'+ind[2]+' - '+ind[3]+'\n')
           elif NUME_ORDRE_I:
             i_foncstat = ind[0] == ind[1]
             dlign['NUME_ORDRE_I'], dlign['NUME_ORDRE_J'] = ind[0], ind[1]
             if ind[0] == ind[1]:
                 dlrecu['NUME_ORDRE_I'] = ind[0]
             else:
                 dlrecu['NUME_ORDRE_I'], dlrecu['NUME_ORDRE_J'] = ind[0], ind[1]
             if INFO==2 :
                aster.affiche('MESSAGE','INDICES :'+str(ind[0])+' - '\
                                                   +str(ind[1])+'\n')
           else:  #TOUT_ORDRE
             if aster.getvectjev(intespec+'.NUMI'):
               i_foncstat = ind[0] == ind[1]
               dlign['NUME_ORDRE_I'], dlign['NUME_ORDRE_J'] = ind[0], ind[1]
               if ind[0] == ind[1]:
                 dlrecu['NUME_ORDRE_I'] = ind[0]
               else:
                 dlrecu['NUME_ORDRE_I'], dlrecu['NUME_ORDRE_J'] = ind[0], ind[1]
             if aster.getvectjev(intespec+'.NOEI'):
               i_foncstat = ind[0] == ind[1] and  ind[2] == ind[3]
               dlign['NOEUD_I'], dlign['NOEUD_J'], dlign['NOM_CMP_I'], dlign['NOM_CMP_J'] = \
                  ind[0], ind[1], ind[2], ind[3]
               if ind[0] == ind[1] and  ind[2] == ind[3]:
                 dlrecu['NOEUD_I'], dlrecu['NOM_CMP_I'] = ind[0], ind[2]
               else:
                 dlrecu['NOEUD_I'], dlrecu['NOEUD_J'], dlrecu['NOM_CMP_I'], \
                  dlrecu['NOM_CMP_J'] = ind[0], ind[1], ind[2], ind[3]

           __fon1=RECU_FONCTION(INTE_SPEC    = INTE_SPEC,
                                 **dlrecu )

           val  = __fon1.Valeurs()
           fvalx= NP.array(val[0])
           fvaly= NP.array(val[1])
           frez = fvalx[0]

           # -- moments spectraux

           val_mom={}
           for i_mom in l_moments :
               n         = len(fvaly)
               trapz     = NP.zeros(n)
               trapz[0]  = 0.
               valy      = fvaly*(2*pi*fvalx)**i_mom
               trapz[1:n] = (valy[1:n]+valy[:-1])/2.*(fvalx[1:n]-fvalx[:-1])
               prim_y    = NP.cumsum(trapz)
               val_mom[i_mom] = prim_y[-1]
                  # -- cas si, seule la partie positive du spectre est utilisée
                  # -- Il faut donc doubler lambda  pour calculer le bon écart type
               if frez >= 0. :
                 val_mom[i_mom]=val_mom[i_mom]*2.
           for i_mom in l_moments :
             chmo='LAMBDA_'+str(i_mom).zfill(2)
             dlign[chmo] = val_mom[i_mom]

        #--- si auto-spectre:
           if i_foncstat:
              # test si le spectre est bien à valeurs positives
              if min(fvaly) < 0.0 :
                 aster.affiche('MESSAGE', str(ind)+'\n')
                 UTMESS('F','MODELISA9_95')
              # -- fonctions statistiques

              dlign['ECART'] = sqrt(val_mom[0])

              if DUREE != None :
                 Ts=DUREE
                 vop=sqrt(val_mom[2] /val_mom[0])/(2.*pi)
                 Nu=Ts*vop/(-log(0.5))
                 deltau=sqrt(1.- val_mom[1] **2/(val_mom[2]*val_mom[0]) )
                 valNd=2.*Nu*(1-exp(-(deltau)**1.2*sqrt(pi*log(2.*Nu))));
                 val_peak=sqrt(2.*log(valNd))
                 dlign['FACT_PIC'] = val_peak   # -- facteur de peak (oour max moyen)
                 dlign['MAX_MOY'] = val_peak*sqrt(val_mom[0])    # -- max moyen

              if abs(val_mom[2])>=1e-20 :
                    dlign['NB_EXTREMA_P_S'] = 1./pi*sqrt(val_mom[4]/val_mom[2])
              if abs(val_mom[0])>=1e-20 :
                 dlign['NB_PASS_ZERO_P_S'] = 1./pi*sqrt(val_mom[2]/val_mom[0])
                 dlign['FREQ_APPAR'] = 0.5*dlign['NB_PASS_ZERO_P_S']
                 if abs(val_mom[4])>=1e-20 :
                    dlign['FACT_IRRE'] = sqrt( val_mom[2]*val_mom[2]/val_mom[0]/val_mom[4])

           # ajoute la ligne à la Table
           tabres.append(dlign)

#--- construction de la table produite

      # tri des paramètres
      ord_para = tabres.para[nbpara0:]
      ord_para.sort()
      ord_para = tabres.para[:nbpara0] + ord_para
      dprod = tabres[ord_para].dict_CREA_TABLE()

      tabout = CREA_TABLE(**dprod)

   return ier
