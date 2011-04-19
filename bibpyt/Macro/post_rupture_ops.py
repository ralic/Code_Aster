#@ MODIF post_rupture_ops Macro  DATE 19/04/2011   AUTEUR GENIAUT S.GENIAUT 
# -*- coding: iso-8859-1 -*-
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

def verif_reuse(OPERATION,obj_reuse) :
   # verification que reuse est correctement employe

   from Utilitai.Utmess     import  UTMESS

   if OPERATION in ('ABSC_CURV_NORM','ANGLE_BIFURCATION','LOI_PROPA',) :
      if not obj_reuse :
         UTMESS('F','RUPTURE1_62',valk=(OPERATION))

   if OPERATION in ('COMPTAGE_CYCLES','CUMUL_CYCLES') :
      if obj_reuse :
         UTMESS('F','RUPTURE1_63',valk=(OPERATION))


def verif_un_fond(tabin,TABLE,OPERATION):
   """ verification qu'il n'y a qu'un fond de fissure """

   from Utilitai.Utmess     import  UTMESS

   # si NUME_FOND est present dans la table
   if 'NUME_FOND' in tabin.para:
      # récupération de la liste des NUME_FOND dans un ensemble (set)
      snum = set(tabin.NUME_FOND.values())
      nfond = len(snum)
      if nfond > 1:
         UTMESS('F','RUPTURE1_5',valk=(OPERATION,TABLE.nom),vali=nfond)


def verif_un_instant(tabin,TABLE,OPERATION,COMPTAGE):
   """ verification qu'il n'y a qu'un instant ou numero d'ordre """

   from Utilitai.Utmess     import  UTMESS

   # si NUME_ORDRE est present dans la table
   if 'NUME_ORDRE' in tabin.para:
      # récupération de la liste des NUME_ORDRE dans un ensemble (set)
      s = set(tabin.NUME_ORDRE.values())
      n = len(s)
      if n > 1:
         UTMESS('F','RUPTURE1_64',valk=(COMPTAGE,TABLE.nom),vali=n)

   # si INST est present dans la table
   if 'INST' in tabin.para:
      # récupération de la liste des INST dans un ensemble (set)
      s = set(tabin.INST.values())
      n = len(s)
      if n > 1:
         UTMESS('F','RUPTURE1_64',valk=(COMPTAGE,TABLE.nom),vali=n)



def verif_exi(tabin,TABLE,col):
   """ verification que la colonne col existe """
   from Utilitai.Utmess     import  UTMESS
   if not col in tabin.para:
      UTMESS('F','RUPTURE1_59',valk=(TABLE.nom,col))


def sittmax(k1,k2):
   """ critere maximum hoop stress"""
   import numpy as NP
   if k2 == 0.:
      return 0.
   else:
      return 2.*NP.arctan(((k1/k2)-NP.sign(k2)*NP.sqrt((k1/k2)**2+8))/4.)


def amestoy(k1,k2,crit_ang):
   """ criteres de Amestoy, Bui, Dang Van : K1 max, K2 nul et G max (uniquement en 2D)"""
   import numpy as NP
   from Utilitai.Utmess     import  UTMESS

   # attention, K2_nul ne marche pas pour un angle > 60
   # et pour les grands angles K1_MAX est tres sensible au signe de K1
   # donc si k1 est negatif ou nul, on force k1 = +0
   if k1 <= 0 : k1=1e-15

   K11 = [1., 0.9886, 0.9552, 0.9018, 0.8314, 0.7479, 0.6559, 0.5598, 0.4640, 0.3722]
   K21 = [0., 0.0864, 0.1680, 0.2403, 0.2995, 0.3431, 0.3696, 0.3788, 0.3718, 0.3507]
   K12 = [0.,  -0.2597, -0.5068, -0.7298, -0.9189, -1.0665, -1.1681, -1.2220, -1.2293, -1.1936]
   K22 = [1., 0.9764, 0.9071, 0.7972, 0.6540, 0.4872, 0.3077, 0.1266, -0.0453, -0.1988]
   # kdevp : pour phi de 0 à 90
   # kdevm : pour phi de -0 à -90
   k1devp = NP.array(K11) * k1 + NP.array(K12) * k2
   k1devm = NP.array(K11) * k1 - NP.array(K12) * k2
   k2devp =  NP.array(K21) * k1 + NP.array(K22) * k2
   k2devm = -NP.array(K21) * k1 + NP.array(K22) * k2
   if crit_ang == 'K1_MAX':
      l = abs(k1devp).tolist()+abs(k1devm).tolist()
   elif crit_ang == 'K2_NUL':
      l = abs(k2devp).tolist()+abs(k2devm).tolist()
   elif crit_ang == 'G_MAX':
      assert(0==1)
      # le critere en G est chiant car il faut connaitre le type de modelisation (CP/DP)
      # et les parametres materiaux, evalués au bon instant, a la bonne temperature....
      # on l'interdit pour le moment

   # suppression de l'angle phi = -0
   l.pop(10)
   if crit_ang == 'K1_MAX':
      i = l.index(max(l))
   elif crit_ang == 'K2_NUL':
      i = l.index(min(l))
   # calcul de phi par rapport à l'indice trouve
   if i<10 : 
      phi = i*10.
   else :
      phi = -(i-9)*10.
      
   if  crit_ang == 'K2_NUL' and abs(phi)>60 :
      UTMESS('A','RUPTURE1_60')
   

   return phi

     
def post_rupture_ops(self,TABLE,OPERATION,**args):
   """
   Macro POST_RUPTURE
   """
   import aster

   macro = 'POST_RUPTURE'
   from Accas import _F
   from Utilitai.Utmess     import  UTMESS
   from numpy import sign
   import numpy as NP

   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table) est tabout
   self.DeclareOut('tabout', self.sd)
   
   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   FORMULE       = self.get_cmd('FORMULE')
   CALC_TABLE    = self.get_cmd('CALC_TABLE')
   DETRUIRE      = self.get_cmd('DETRUIRE')
   RECU_FONCTION = self.get_cmd('RECU_FONCTION')
   IMPR_FONCTION = self.get_cmd('IMPR_FONCTION')
   POST_FATIGUE  = self.get_cmd('POST_FATIGUE')
   IMPR_TABLE    = self.get_cmd('IMPR_TABLE')
   CREA_TABLE    = self.get_cmd('CREA_TABLE')

   # extraction de la table en entree dans l'espace python -> table python
   tabin = TABLE.EXTR_TABLE()

   # verification que reuse est correctement employe
   verif_reuse(OPERATION,self.reuse)

   #-----------------------------------------------------------------------
   if OPERATION == 'ABSC_CURV_NORM' :

      # verification que la table contient une colonne 'ABSC_CURV'
      verif_exi(tabin,TABLE,'ABSC_CURV')

      # verification qu'il n'y a qu'un fond de fissure 
      verif_un_fond(tabin,TABLE,OPERATION)

      # récupération de l'abscisse maximale et ajout dans l'environnement
      smax = max(tabin.ABSC_CURV.values())
      self.update_const_context({'smax' : smax})

      __NORM_ABS=FORMULE(NOM_PARA=('ABSC_CURV'),VALE='ABSC_CURV/smax');

      tabout=CALC_TABLE(TABLE=TABLE,
                        reuse=TABLE,
                        ACTION=_F(OPERATION='OPER',
                                  FORMULE=__NORM_ABS,
                                  NOM_PARA=args['NOM_PARA']))


   #-----------------------------------------------------------------------
   if OPERATION == 'ANGLE_BIFURCATION' :

      # verification qu'il n'y a qu'un fond de fissure 
      verif_un_fond(tabin,TABLE,OPERATION)
      
      crit_ang = args['CRITERE']
      self.update_const_context({'crit_ang' : crit_ang})

      # verification que la table contient les colonnes necessaires
      if crit_ang in ('SITT_MAX','K1_MAX','K2_NUL','G_MAX') :
         verif_exi(tabin,TABLE,'K1')
         verif_exi(tabin,TABLE,'K2')
         
      if crit_ang == 'SITT_MAX' :

         __angle_deg=FORMULE(NOM_PARA=('K1','K2'),VALE='sittmax(K1,K2)*180./pi')

         # on l'ajoute dans l'environnement pour pouvoir s'en servir dans les commandes
         self.update_const_context({'sittmax' : sittmax})

      elif crit_ang in ('K1_MAX','K2_NUL','G_MAX'):

         # criteres uniquement valables en 2D
         # mais comment savoir ?
         
         __angle_deg=FORMULE(NOM_PARA=('K1','K2'),VALE='amestoy(K1,K2,crit_ang)')

         # on l'ajoute dans l'environnement pour pouvoir s'en servir dans les commandes
         self.update_const_context({'amestoy' : amestoy})

      tabout=CALC_TABLE(TABLE=TABLE,
                        reuse=TABLE,
                        ACTION=_F(OPERATION='OPER',
                                  FORMULE=__angle_deg,
                                  NOM_PARA=args['NOM_PARA']))


   #-----------------------------------------------------------------------
   if OPERATION == 'COMPTAGE_CYCLES' :

      COMPTAGE=args['COMPTAGE']

      # verification qu'il n'y a qu'un fond de fissure 
      verif_un_fond(tabin,TABLE,OPERATION)

      # quantites sur lesquelles s'effectue le comptage
      list_q = args['NOM_PARA']
      
      # convertion en une liste si on n'a une quantité pour pouvoir itérer dessus
      if  type(list_q) == str : list_q = [list_q]

      for q in list_q:
         verif_exi(tabin,TABLE,q)

      nq = len(list_q)

      #  comptage unitaire
      if COMPTAGE == 'UNITAIRE' :

         # recuperation des mot-clés
         COEF_MULT_MAXI=args['COEF_MULT_MAXI']
         COEF_MULT_MINI=args['COEF_MULT_MINI']
      
         # verification qu'il n'y a qu'un instant (ou instant absent)
         verif_un_instant(tabin,TABLE,OPERATION,COMPTAGE)

         # definition d'une fonction constante entiere (DEFI_CONSTANTE ne gere que les reels)
         __cycle_unit = FORMULE(NOM_PARA=('NUM_PT',),VALE='1')
      
         for i,q in enumerate(list_q):

            # definition de la formule
            __delta_unit = FORMULE(NOM_PARA=q,VALE=q+'*(COEF_MULT_MAXI-COEF_MULT_MINI)')
            self.update_const_context({'q' : q})
            self.update_const_context({'COEF_MULT_MAXI' : COEF_MULT_MAXI})
            self.update_const_context({'COEF_MULT_MINI' : COEF_MULT_MINI})

            mostcles={}
            if i==0 :
               mostcles['TABLE']=TABLE
            else :
               mostcles['TABLE']=tabout
               mostcles['reuse']=tabout
               
            tabout=CALC_TABLE(ACTION=(_F(OPERATION='OPER',FORMULE=__delta_unit,NOM_PARA='DELTA_'+q),
                                      _F(OPERATION='SUPPRIME',NOM_PARA=q)),
                              **mostcles)

            DETRUIRE(CONCEPT=_F(NOM=__delta_unit),INFO=1)

         # suppression des colonnes INST et NUME_ORDRE si elles existent
         tabout=CALC_TABLE(TABLE=tabout,reuse=tabout,
                           ACTION=_F(OPERATION='SUPPRIME',NOM_PARA=('INST','NUME_ORDRE'))
                           )

         # on ajoute la colonne CYCLE
         # voir comment faire avec CALC_TABLE/AJOUT_COLONNE
         tabout=CALC_TABLE(reuse=tabout,TABLE=tabout,
                           ACTION=_F(OPERATION='OPER',FORMULE=__cycle_unit,NOM_PARA='CYCLE'))
      
      # vrai comptage des cycles avec POST_FATIGUE
      else : 

         verif_exi(tabin,TABLE,'INST')

         __delta = FORMULE(NOM_PARA=('VALE_MIN','VALE_MAX'),VALE='VALE_MAX-VALE_MIN')

         # si on effectue le comptage sur plusieurs quantités,
         # il faut qu'elles aient le meme nombre de cycles
         # pour cela, on stocke pour chaque quantite le nombres de cycles dans 'nb_cycles'
         nb_cycles=[]

         # creation d'une table vide (en fait qui contient juste une ligne qui sera supprimee en fin)
         tabout=CREA_TABLE(LISTE=_F(LISTE_I=(0,),PARA='BIDON'))

         # boucle sur les points du fond de fissure
         nbpt=max(TABLE.EXTR_TABLE().NUM_PT.values())

         for ipt in range(nbpt):
            numpt=ipt+1

            __numpt = FORMULE(NOM_PARA=('CYCLE',),VALE='numpt')
            self.update_const_context({'numpt' : numpt})

            # boucle sur les quantites à compter
            __TABC=[None]*nq
            for i,q in enumerate(list_q):

               __EVOLQ=RECU_FONCTION(TABLE=TABLE,
                                   PARA_X='INST',
                                   PARA_Y=q,
                                   FILTRE=_F(NOM_PARA='NUM_PT',
                                             CRIT_COMP='EQ',
                                             VALE_I=numpt),
                                   )

               __TABC[i]=POST_FATIGUE(CHARGEMENT='UNIAXIAL',
                                    HISTOIRE=_F(SIGM=__EVOLQ),
                                    COMPTAGE=args['COMPTAGE'],
                                    )

               __TABC[i]=CALC_TABLE(TABLE=__TABC[i],
                                    reuse=__TABC[i],
                                    ACTION=(_F(OPERATION='OPER',FORMULE=__delta,NOM_PARA='DELTA_'+q),
                                            _F(OPERATION='SUPPRIME',NOM_PARA=('VALE_MIN','VALE_MAX')))
                                    )

               if i==0 :
                  nb_cycles_ref = __TABC[i].EXTR_TABLE().CYCLE.values()[-1]
               else :
                  # verif que l'on a bien le meme nb de cycles que la 1ere quantité
                  nb_cycles_i = __TABC[i].EXTR_TABLE().CYCLE.values()[-1]
                  if nb_cycles_ref != nb_cycles_i :
                     UTMESS('F','RUPTURE1_61')

                  # combinaison de la table courante avec la 1ere table
                  __TABC[0]=CALC_TABLE(reuse=__TABC[0],
                                       TABLE=__TABC[0],
                                       ACTION=_F(TABLE=__TABC[i],OPERATION='COMB',NOM_PARA='CYCLE'))


            # on complete la table avec le point du fond
            # voir comment faire avec CALC_TABLE/AJOUT_COLONNE
            __TABC[0]=CALC_TABLE(reuse=__TABC[0],
                                 TABLE=__TABC[0],
                                 ACTION=_F(OPERATION='OPER',
                                           FORMULE=__numpt,
                                           NOM_PARA='NUM_PT') )

            # on complete la table avec les parametres initiaux
            # voir comment faire avec CALC_TABLE/AJOUT_COLONNE

            # on concatene la table avec la table sortie
            tabout=CALC_TABLE(reuse=tabout,
                               TABLE=tabout,
                               ACTION=_F(TABLE=__TABC[0],OPERATION='COMB'))

         # on supprime la colonne bidon
         tabout=CALC_TABLE(TABLE=tabout,
                            reuse=tabout,
                            TITRE=tabout.nom,
                            ACTION=_F(OPERATION='SUPPRIME',NOM_PARA='BIDON'))

   #-----------------------------------------------------------------------
   if OPERATION == 'LOI_PROPA' :

      # verification qu'il n'y a qu'un fond de fissure 
      verif_un_fond(tabin,TABLE,OPERATION)

      # nom de la colonne correspondant au delta_K_eq
      dkeq = args['NOM_DELTA_K_EQ']
      verif_exi(tabin,TABLE,dkeq)
      
      loi = args['LOI']
      C = args['C']
      M = args['M']

      if loi == 'PARIS' :

         __da=FORMULE(NOM_PARA=(str(dkeq)),VALE='C * ' + dkeq + ' **M ')

         # on l'ajoute dans l'environnement pour pouvoir s'en servir dans les commandes
         self.update_const_context({'C' : C})
         self.update_const_context({'M' : M})

      tabout=CALC_TABLE(TABLE=TABLE,
                        reuse=TABLE,
                        ACTION=_F(OPERATION='OPER',
                                  FORMULE=__da,
                                  NOM_PARA=args['NOM_PARA']))

   #-----------------------------------------------------------------------
   if OPERATION == 'CUMUL_CYCLES' :

      # verification qu'il n'y a qu'un fond de fissure 
      verif_un_fond(tabin,TABLE,OPERATION)

      # quantité sur laquelle s'effectue le cumul
      q = args['NOM_PARA']
      verif_exi(tabin,TABLE,q)

      verif_exi(tabin,TABLE,'CYCLE')

      # boucle sur les points du fond de fissure
      nbpt=max(tabin.NUM_PT.values())

      for ipt in range(nbpt):
         numpt=ipt+1
         tab = tabin.NUM_PT==numpt
         da_cycle = NP.array(tab.DELTA_A.values())
         #sum(da_cycle)

#          tabout=CALC_TABLE(TABLE=tabin,
#                            ACTION=_F(OPERATION='FILTRE',NOM_PARA='CYCLE',CRIT_COMP='EQ',VALE_I=1),
#                         )
# 
#          IMPR_TABLE(TABLE=tabout)
         
         
   return
 
