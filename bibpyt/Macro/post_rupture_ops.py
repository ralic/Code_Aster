#@ MODIF post_rupture_ops Macro  DATE 07/01/2013   AUTEUR LADIER A.LADIER 
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

import numpy as NP
from Utilitai.Utmess import UTMESS
from SD.sd_mater     import sd_compor1

def verif_nb_table(OPERATION, TABLE) :
   """ verification que le nombre de tables est correct et retourne le nombre de tables"""
   # une ou plusieurs tables en entree ?
   nb_tabin = len(TABLE)

   # seule PILO_PROPA autorise plusieurs tables
   if OPERATION is not 'PILO_PROPA':
      if nb_tabin != 1 :
         UTMESS('F','RUPTURE1_66',valk=(OPERATION), vali=nb_tabin)

   return  nb_tabin


def verif_reuse(OPERATION,obj_reuse) :
   # verification que reuse est correctement employe
   if OPERATION in ('ABSC_CURV_NORM','ANGLE_BIFURCATION','LOI_PROPA','K1_NEGATIF') :
      if not obj_reuse :
         UTMESS('F','RUPTURE1_62',valk=(OPERATION))

   if OPERATION in ('COMPTAGE_CYCLES','CUMUL_CYCLES','PILO_PROPA') :
      if obj_reuse :
         UTMESS('F','RUPTURE1_63',valk=(OPERATION))


def verif_un_instant(tabin, OPERATION, COMPTAGE):
   """ verification qu'il n'y a qu'un instant ou numero d'ordre """
   # si NUME_ORDRE est present dans la table
   if 'NUME_ORDRE' in tabin.para:
      # récupération de la liste des NUME_ORDRE dans un ensemble (set)
      s = set(tabin.NUME_ORDRE.values())
      n = len(s)
      if n > 1:
         UTMESS('F','RUPTURE1_64',valk=(COMPTAGE, tabin.nom),vali=n)

   # si INST est present dans la table
   if 'INST' in tabin.para:
      # récupération de la liste des INST dans un ensemble (set)
      s = set(tabin.INST.values())
      n = len(s)
      if n > 1:
         UTMESS('F','RUPTURE1_64',valk=(COMPTAGE, tabin.nom),vali=n)


def verif_exi(tabin, col):
   """ verification que la colonne col existe """
   if not col in tabin.para:
      UTMESS('F','RUPTURE1_59',valk=(tabin.nom,col))


def verif_non_exi(tabin, col):
   """ verification que la colonne col n'existe pas"""
   if col in tabin.para:
      UTMESS('F','RUPTURE1_65',valk=(tabin.nom,col))


def sittmax(k1,k2):
   """ critere maximum hoop stress"""
   if k2 == 0.:
      return 0.
   else:
      return 2.*NP.arctan(((k1/k2)-NP.sign(k2)*NP.sqrt((k1/k2)**2+8))/4.)


def amestoy(k1,k2,crit_ang):
   """ criteres de Amestoy, Bui, Dang Van : K1 max, K2 nul et G max (uniquement en 2D)"""
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


def caract_mater(self,mater):
   """ recupere le module d'Young et le coefficient de poisson du materiau"""
   matph = mater.sdj.NOMRC.get()
   phenom=None
   for cmpt in matph :
      if cmpt[:4]=='ELAS' :
         phenom=cmpt
         break
   if phenom==None :
     UTMESS('F','RUPTURE0_5')      

   compor = sd_compor1('%-8s.%s' % (mater.nom, phenom))
   valk = [s.strip() for s in compor.VALK.get()]
   valr = compor.VALR.get()
   dicmat=dict(zip(valk,valr))

   young=dicmat['E']
   poisson = dicmat['NU']   
   
#  E et nu definis avec defi_fonction
   if young==0.0 and poisson==0.0:    
      list_oper=valk[: len(valk)/2]
      list_fonc=valk[len(valk)/2 :]    
#   valk contient les noms des operandes mis dans defi_materiau dans une premiere partie et
#   et les noms des concepts de type [fonction] (ecrits derriere les operandes) dans une 
#   une seconde partie  
      try:list_oper.remove("B_ENDOGE")
      except: ValueError
      try:list_oper.remove("RHO")     
      except: ValueError
      try:list_oper.remove("PRECISIO")
      except: ValueError
      try:list_oper.remove("K_DESSIC")
      except: ValueError      
      try:list_oper.remove("TEMP_DEF")
      except: ValueError
   
      nom_fonc_e = self.get_concept(list_fonc[list_oper.index("E")])
      nom_fonc_nu = self.get_concept(list_fonc[list_oper.index("NU")])
    
      if (nom_fonc_e.sdj.PROL.get()[0].strip()=='CONSTANT' and
          nom_fonc_nu.sdj.PROL.get()[0].strip()=='CONSTANT'):
         young  = nom_fonc_e.Ordo()[0]
         poisson = nom_fonc_nu.Ordo()[0]
      else:
         UTMESS('F','RUPTURE1_68')
   return (young, poisson)


def verif_val_neg(val_k1):
   """ recherche presence de valeurs de K1 negatives"""
   s=filter(lambda x: x < 0, val_k1)
   if len(s) != 0 : return True
   else :           return False


def mise_zero(x) :
   """ mise à zero des valeurs negatives"""
   if   x<0.  : return 0.
   elif x>=0. : return x


def fonc_norm(ABSC_CURV,NUME_FOND,nume_fond,max_absc_fond):
   for i in range(len(nume_fond)):
        if NUME_FOND == nume_fond[i] :
          return ABSC_CURV/max_absc_fond[i]

#------------------------------------------------------------------------------------------------------
def post_rupture_ops(self, TABLE, OPERATION, **args):
   """
   Macro POST_RUPTURE
   """
   import aster

   macro = 'POST_RUPTURE'
   from Accas import _F
   
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
   DEFI_CONSTANTE= self.get_cmd('DEFI_CONSTANTE')

   # verification que le nombre de tables est correct
   # et retourne le nombre de tables
   nb_tabin = verif_nb_table(OPERATION, TABLE)

   # verification que reuse est correctement employe
   verif_reuse(OPERATION,self.reuse)

   # extraction de la ou des tables en entree dans l'espace python -> table python
   l_tabin = [t.EXTR_TABLE() for t in TABLE]
   if nb_tabin == 1:
      # pour simplifier l'écriture
      tabin = l_tabin[0]
      TABIN = TABLE[0]

   #-----------------------------------------------------------------------
   if OPERATION == 'ABSC_CURV_NORM' :

      # verification que la table contient une colonne 'ABSC_CURV'
      verif_exi(tabin, 'ABSC_CURV')

      if 'NUME_FOND' in tabin.para :
        # recuperation des numeros des fonds
        contenu_nume_fond = tabin.NUME_FOND.values()
        nume_fond = list(set(contenu_nume_fond))

        indice_tmp = 0
        max_absc_fond=[]
        # boucle sur les fonds
        for i,fond_i in enumerate(nume_fond):
          # abscisse curviligne maximale pour le fond 'fond_i'      
          tab_fond_i = tabin.NUME_FOND==fond_i
          max_absc_fond.append(max(tab_fond_i.ABSC_CURV.values()))

        self.update_const_context({'max_absc_fond' : max_absc_fond})
        self.update_const_context({'nume_fond' : nume_fond})
        self.update_const_context({'fonc_norm' : fonc_norm})

        __formul = FORMULE(NOM_PARA=('ABSC_CURV','NUME_FOND'),
                             VALE = 'fonc_norm(ABSC_CURV,NUME_FOND,nume_fond,max_absc_fond)')

        tabout=CALC_TABLE(TABLE=TABIN,
                          reuse=TABIN,
                          ACTION=_F(OPERATION='OPER',
                                    FORMULE=__formul,
                                    NOM_PARA=args['NOM_PARA']))
      else :
        # récupération de l'abscisse maximale et ajout dans l'environnement
        smax = max(tabin.ABSC_CURV.values())
        self.update_const_context({'smax' : smax})

        __NORM_ABS=FORMULE(NOM_PARA=('ABSC_CURV'),VALE='ABSC_CURV/smax');

        tabout=CALC_TABLE(TABLE=TABIN,
                          reuse=TABIN,
                          ACTION=_F(OPERATION='OPER',
                                    FORMULE=__NORM_ABS,
                                    NOM_PARA=args['NOM_PARA']))

   #-----------------------------------------------------------------------
   if OPERATION == 'ANGLE_BIFURCATION' :

      crit_ang = args['CRITERE']
      self.update_const_context({'crit_ang' : crit_ang})

      # verification que la table contient les colonnes necessaires
      if crit_ang in ('SITT_MAX','K1_MAX','K2_NUL','G_MAX') :
         verif_exi(tabin, 'K1')
         verif_exi(tabin, 'K2')

      if crit_ang == 'SITT_MAX' :
         # on l'ajoute dans l'environnement pour pouvoir s'en servir dans les commandes
         self.update_const_context({'sittmax' : sittmax})
         __angle_deg=FORMULE(NOM_PARA=('K1','K2'),VALE='sittmax(K1,K2)*180./pi')

      elif crit_ang in ('K1_MAX','K2_NUL','G_MAX'):
         # criteres uniquement valables en 2D
         # mais comment savoir ?
         # on l'ajoute dans l'environnement pour pouvoir s'en servir dans les commandes
         self.update_const_context({'amestoy' : amestoy})
         __angle_deg=FORMULE(NOM_PARA=('K1','K2'),VALE='amestoy(K1,K2,crit_ang)')

      tabout=CALC_TABLE(TABLE=TABIN,
                        reuse=TABIN,
                        ACTION=_F(OPERATION='OPER',
                                  FORMULE=__angle_deg,
                                  NOM_PARA=args['NOM_PARA']))

   #-----------------------------------------------------------------------
   if OPERATION[-4:] == 'K_EQ' :

      cumul=args['CUMUL']
      
      if len(OPERATION)==4:
         Q1='K1'
         Q2='K2'
         Q3='K3'
         list_cumul=('LINEAIRE','QUADRATIQUE')
      else:
         Q1='DELTA_K1'
         Q2='DELTA_K2'
         Q3='DELTA_K3'
         list_cumul=('QUADRATIQUE')  
      
      if Q3 in tabin.para :
         ndim = 3
      else :
         ndim = 2

      # verification que la table contient les colonnes necessaires
      if cumul in list_cumul :
         verif_exi(tabin, Q1)
         verif_exi(tabin, Q2)
         if ndim==3 :
            verif_exi(tabin, Q3)      
      
      if cumul == 'CUMUL_G' :
#            recup de E et nu, attention avec la dependance a INST et X, Y, Z
         verif_exi(tabin, 'G')
         
         (young, poisson)=caract_mater(self,args['MATER'])
         self.update_const_context({'E' : young})
         self.update_const_context({'nu' : poisson})
         __cumul = FORMULE(NOM_PARA='G',VALE='sqrt(G*E/(1-nu**2))')

      elif cumul == 'QUADRATIQUE' :
#         nu=
         (young, poisson)=caract_mater(self,args['MATER']) 
         self.update_const_context({'nu' : poisson})

         if ndim==3 :
            __cumul=FORMULE(NOM_PARA=(Q1,Q2,Q3),VALE='sqrt('+Q1+'**2+'+Q2+'**2+'+Q3+'**2/(1.-nu))')
         elif ndim==2 :
            __cumul=FORMULE(NOM_PARA=(Q1,Q2),     VALE='sqrt('+Q1+'**2+'+Q2+'**2)')

      elif cumul == 'LINEAIRE' :

         if ndim==3 :
            __cumul=FORMULE(NOM_PARA=('K1','K2','K3'),VALE='max(K1,0)+abs(K2)+0.74*abs(K3))')
         elif ndim==2 :
            __cumul=FORMULE(NOM_PARA=('K1','K2'),     VALE='max(K1,0)+abs(K2)')

      elif cumul == 'MODE_I' :

         verif_exi(tabin, Q1)
         __cumul = FORMULE(NOM_PARA=Q1,VALE=Q1)


      tabout=CALC_TABLE(TABLE=TABIN,
                        reuse=TABIN,
                        ACTION=_F(OPERATION='OPER',
                                  FORMULE=__cumul,
                                  NOM_PARA=args['NOM_PARA']))



   #-----------------------------------------------------------------------
   if OPERATION == 'COMPTAGE_CYCLES' :

      COMPTAGE=args['COMPTAGE']

      # quantites sur lesquelles s'effectue le comptage
      list_q = args['NOM_PARA']

      for q in list_q:
         verif_exi(tabin, q)

      nq = len(list_q)

      # construction de la liste des parametres "auxiliaires" (a completer en fin d'operation)
      l_para_tout = set( tabin.para )
      l_para_deja = set( list_q+('NUME_ORDRE','INST','NUM_PT','NUME_FOND') )
      l_para_aux  = l_para_tout - l_para_deja

      #  comptage unitaire
      if COMPTAGE == 'UNITAIRE' :

         # recuperation des mot-clés
         COEF_MULT_MAXI=args['COEF_MULT_MAXI']
         COEF_MULT_MINI=args['COEF_MULT_MINI']

         # verification qu'il n'y a qu'un instant (ou instant absent)
         verif_un_instant(tabin, OPERATION, COMPTAGE)

         for i,q in enumerate(list_q):

            # definition de la formule
            self.update_const_context({'q' : q})
            self.update_const_context({'COEF_MULT_MAXI' : COEF_MULT_MAXI})
            self.update_const_context({'COEF_MULT_MINI' : COEF_MULT_MINI})
            __delta_unit = FORMULE(NOM_PARA=q,VALE=q+'*(COEF_MULT_MAXI-COEF_MULT_MINI)')

            mostcles={}
            if i==0 :
               mostcles['TABLE']=TABIN
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
         tabout=CALC_TABLE(reuse=tabout,TABLE=tabout,
                           ACTION=_F(OPERATION='AJOUT_COLONNE',VALE=1,NOM_PARA='CYCLE'))


      # vrai comptage des cycles avec POST_FATIGUE
      else :

         verif_exi(tabin, 'INST')
         __delta = FORMULE(NOM_PARA=('VALE_MIN','VALE_MAX'),VALE='VALE_MAX-VALE_MIN')

         # creation d'une table vide (en fait qui contient juste une ligne qui sera supprimee en fin)
         tabout=CREA_TABLE(LISTE=_F(LISTE_I=(0,),PARA='&BIDON&'))

         __COPIE_TABIN = CALC_TABLE(TABLE = TABIN,
                                 ACTION =_F(OPERATION='EXTR',
                                 NOM_PARA=tabin.para))
         
         if 'NUME_FOND' not in tabin.para :
           __COPIE_TABIN = CALC_TABLE(TABLE = __COPIE_TABIN,
                          ACTION =_F(OPERATION='AJOUT_COLONNE',
                          VALE=1,
                          NOM_PARA='NUME_FOND'))
         if 'NUM_PT' not in tabin.para :
           __COPIE_TABIN = CALC_TABLE(TABLE = __COPIE_TABIN,
                                      ACTION =_F(OPERATION='AJOUT_COLONNE',
                                      VALE=1,
                                      NOM_PARA='NUM_PT'))         

         tabin = __COPIE_TABIN.EXTR_TABLE()
         contenu_nume_fond = tabin.NUME_FOND.values()
         nume_fond = list(set(contenu_nume_fond))

         for j,fond_j in enumerate(nume_fond):

           # récupération du num_pt maximale pour le fond 'fond_j'
           tab_fond_j = tabin.NUME_FOND==fond_j
           nbpt=max(tab_fond_j.NUM_PT.values())

           # si on effectue le comptage sur plusieurs quantités,
           # il faut qu'elles aient le meme nombre de cycles
           # pour cela, on stocke pour chaque quantite le nombres de cycles dans 'nb_cycles'
           nb_cycles=[]

           # boucle sur les points du fond de fissure du fond 'fond_i'
           for ipt in range(nbpt):

             numpt=ipt+1

             # boucle sur les quantites à compter
             __TABC=[None]*nq
             for i,q in enumerate(list_q):

               __EVOLQ=RECU_FONCTION(TABLE=__COPIE_TABIN,
                                     PARA_X='INST',
                                     PARA_Y=q,
                                     FILTRE=(_F(NOM_PARA='NUM_PT',
                                               CRIT_COMP='EQ',
                                               VALE_I=numpt),
                                             _F(NOM_PARA='NUME_FOND',
                                               CRIT_COMP='EQ',
                                               VALE_I=fond_j),
                                             ),
                                     )

               __TABC[i]=POST_FATIGUE(CHARGEMENT='UNIAXIAL',
                                      HISTOIRE=_F(SIGM=__EVOLQ),
                                      COMPTAGE=args['COMPTAGE'],
                                      DELTA_OSCI=args['DELTA_OSCI'],
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
             if 'NUM_PT' in tabin.para :
               __TABC[0]=CALC_TABLE(reuse=__TABC[0],
                                    TABLE=__TABC[0],
                                    ACTION=_F(OPERATION='AJOUT_COLONNE',
                                              VALE=numpt,
                                              NOM_PARA='NUM_PT'),)

             # on complete la table avec le numero du fond
             if 'NUME_FOND' in tabin.para :
               __TABC[0]=CALC_TABLE(reuse=__TABC[0],
                                    TABLE=__TABC[0],
                                    ACTION=_F(OPERATION='AJOUT_COLONNE',
                                              VALE=fond_j,
                                              NOM_PARA='NUME_FOND'),)

             # on complete la table avec les parametres auxiliaires
             tab_tmp = tabin.NUM_PT==numpt
             tab_tmp = tab_tmp.values()
             for para in l_para_aux :
              # on prend la 1ere valeur (pour bien faire, il faudrait verifier que toutes
              # les valeurs sont bien identiques)
               vale= tab_tmp[para][0]
               __TABC[0]=CALC_TABLE(reuse=__TABC[0],
                                    TABLE=__TABC[0],
                                    ACTION=_F(OPERATION='AJOUT_COLONNE',
                                              VALE=vale,
                                              NOM_PARA=para) )

             # on concatene la table avec la table sortie
             tabout=CALC_TABLE(reuse=tabout,
                                TABLE=tabout,
                                ACTION=_F(TABLE=__TABC[0],OPERATION='COMB'))

         # on supprime la colonne bidon
         tabout=CALC_TABLE(TABLE=tabout,
                           reuse=tabout,
                           TITRE=tabout.nom,
                           ACTION=_F(OPERATION='SUPPRIME',NOM_PARA='&BIDON&'))

         DETRUIRE(CONCEPT=_F(NOM=__COPIE_TABIN),INFO=1)
   #-----------------------------------------------------------------------
   if OPERATION == 'LOI_PROPA' :

      # nom de la colonne correspondant au delta_K_eq
      dkeq = args['NOM_DELTA_K_EQ']
      verif_exi(tabin, dkeq)

      loi = args['LOI']
      C = args['C']
      M = args['M']

      if loi == 'PARIS' :
         # on l'ajoute dans l'environnement pour pouvoir s'en servir dans les commandes
         self.update_const_context({'C' : C})
         self.update_const_context({'M' : M})
         __da=FORMULE(NOM_PARA=(str(dkeq)),VALE='C * ' + dkeq + ' **M ')

      tabout=CALC_TABLE(TABLE=TABIN,
                        reuse=TABIN,
                        ACTION=_F(OPERATION='OPER',
                                  FORMULE=__da,
                                  NOM_PARA=args['NOM_PARA']))

   #-----------------------------------------------------------------------
   if OPERATION == 'CUMUL_CYCLES' :

      # quantité sur laquelle s'effectue le cumul
      q = args['NOM_PARA']
      verif_exi(tabin, q)

      verif_exi(tabin, 'CYCLE')

      # creation d'une table vide (en fait qui contient juste une ligne qui sera supprimee en fin)
      tabout=CREA_TABLE(LISTE=_F(LISTE_I=(0,),PARA='&BIDON&'))

      # liste des para de la nouvelle table : on rajoutera "q" a la fin apres
      l_para_tout = set( tabin.para )
      l_para_deja = set( ['CYCLE' , q ])
      l_para  = list(l_para_tout - l_para_deja)

      if 'NUME_FOND' not in tabin.para :
        tabin['NUME_FOND']=[1]*len(tabin)
      if 'NUM_PT' not in tabin.para :
        tabin['NUM_PT']=[1]*len(tabin)
      contenu_nume_fond = tabin.NUME_FOND.values()
      nume_fond = list(set(contenu_nume_fond))


      for i,fond_i in enumerate(nume_fond):

        tab_fond_i = tabin.NUME_FOND==fond_i

        # récupération du num_pt maximale pour le fond 'fond_i'
        nbpt = max(tab_fond_i.NUM_PT.values())

        # boucle sur les points du fond de fissure
        for ipt in range(nbpt):
          numpt=ipt+1
          tab = tab_fond_i.NUM_PT==numpt
          dic = tab.values()

          # creation de la liste des valeurs des para initiaux
          l_vale=[]
          for para in l_para :
            # on prend la 1ere valeur (pour bien faire, il faudrait verifier que toutes
            # les valeurs sont bien identiques et ne pas copier les colonnes dont les
            # valeurs ne sont pas identiques...)
            l_vale.append(dic[para][0])

          # rajout de la derniere colonne : moyenne des valeurs
          l_para.append(q)
          da_cycle = NP.array(tab.values()[q])
          moy = da_cycle.mean()
          l_vale.append(moy)

          # ajout dans la table de cette ligne
          tabout=CALC_TABLE(reuse=tabout,
                            TABLE=tabout,
                            ACTION=_F(OPERATION='AJOUT_LIGNE',
                                      NOM_PARA =l_para,
                                      VALE     =l_vale)
                           )

      # on supprime la colonne bidon
      tabout=CALC_TABLE(TABLE=tabout,
                        reuse=tabout,
                        TITRE=tabout.nom,
                        ACTION=_F(OPERATION='SUPPRIME',NOM_PARA='&BIDON&'))

   #-----------------------------------------------------------------------
   if OPERATION == 'PILO_PROPA' :

      # creation d'une table vide (en fait qui contient juste une ligne qui sera supprimee en fin)
      tabout=CREA_TABLE(LISTE=_F(LISTE_I=(0,),PARA='&BIDON&'))

      # pour toutes les tables en entrees (cad toutes les fissures)
      for i, tab_i in enumerate(l_tabin) :
         # verification que DELTA_A existe
         verif_exi(tab_i, 'DELTA_A')

         # verification que DELTA_CYCLE n'existe pas
         verif_non_exi(tab_i, 'DELTA_CYCLE')

         # verification que DELTA_A_PILO n'existe pas
         verif_non_exi(tab_i, 'DELTA_A_PILO')

      # Pilotage en increment du nombre de cycles ou en increment d'avancee max ?
      DN_pilo    = args['DELTA_N']
      DAmax_pilo = args['DELTA_A_MAX']
      assert (DN_pilo or DAmax_pilo)

      # si pilotage en increment d'avancee max : calcul du l'increment de cycles pilo
      if DAmax_pilo :

         # récupération de l'avancee max des points des fonds pour toutes les fissures
         damax=0
         for tab_i in l_tabin:
            damax = max( damax , max(tab_i.DELTA_A.values()))

         # increment de cycles pilo
         DN_pilo = DAmax_pilo/damax

      # quelque soit le type de pilotage : mise a jour du DELTA_A et ecriture du DN
      self.update_const_context({'DN_pilo' : DN_pilo})
      __DNpilo=FORMULE(NOM_PARA='DELTA_A', VALE= 'DN_pilo * DELTA_A ')

      for TABLE_i in TABLE:

         __tabtmp=CALC_TABLE(TABLE=TABLE_i,
                             ACTION=(
                                 _F(OPERATION='OPER',FORMULE=__DNpilo,NOM_PARA='DELTA_A_PILO'),
                                 _F(OPERATION='AJOUT_COLONNE',VALE=DN_pilo,NOM_PARA='DELTA_CYCLE'),
                                 _F(OPERATION='SUPPRIME',NOM_PARA='DELTA_A'),
                                 _F(OPERATION='RENOMME',NOM_PARA=('DELTA_A_PILO','DELTA_A')),
                                 ),
                            )

         # combinaison de la table courante avec la 1ere table
         tabout=CALC_TABLE(reuse=tabout,
                           TABLE=tabout,
                           ACTION=_F(TABLE=__tabtmp,OPERATION='COMB'))

      # on supprime la colonne bidon
      tabout=CALC_TABLE(TABLE=tabout,
                        reuse=tabout,
                        TITRE=tabout.nom,
                        ACTION=_F(OPERATION='SUPPRIME',NOM_PARA='&BIDON&'))

   #-----------------------------------------------------------------------
   if OPERATION == 'K1_NEGATIF' :

      # verification que K1 existe
      verif_exi(tabin, 'K1')      

      # verification de la presence de valeurs negatives de K1 
      change = verif_val_neg(tabin.K1)

      # si on a trouve au moins une valeur negative de K1 et G present
      if change == True and ('G' or 'G_IRWIN' in tabin.para) :

         # verification que K2 existe
         verif_exi(tabin, 'K2')

         # recuperation des donnees du materiau
         (young, poisson)=caract_mater(self,args['MATER'])
         self.update_const_context({'E' : young})
         self.update_const_context({'nu' : poisson})

         # formule d'Irwin servant a recalculer G
         if args['MODELISATION'] == '3D' :
           # verification que K3 existe
           verif_exi(tabin, 'K3')
           __formul_G = FORMULE(NOM_PARA=('K2','K3'),VALE='(K2**2)*(1.-nu**2)/E+(K3**2)*(1.+nu)/E')
         elif args['MODELISATION'] == 'D_PLAN' or args['MODELISATION'] == 'AXIS' :
           __formul_G = FORMULE(NOM_PARA=('K2'),VALE='(K2**2)*(1.-nu**2)/E')
         elif args['MODELISATION'] == 'C_PLAN' :
           __formul_G = FORMULE(NOM_PARA=('K2'),VALE='(K2**2)/E')


##       creation de la table de sortie avec les nouvelles valeurs de K1 et de G et/ou de G_IRWIN

         # tableau 1 avec les valeurs a ne pas modifier
         __tabtmp=CALC_TABLE(TABLE=TABIN,
                             ACTION=_F( OPERATION='FILTRE',
                                        NOM_PARA= 'K1' ,
                                        CRIT_COMP= 'GE' , VALE= 0.0,),)

         # tableau 2 avec les valeurs a modifier
         tabout=CALC_TABLE(TABLE=TABIN,reuse=TABIN,
                           ACTION=_F( OPERATION='FILTRE',
                                      NOM_PARA= 'K1' , CRIT_COMP= 'LT' , VALE= 0.0,),)

         # operation sur la colonne K1 du tableau 2
         tabout=CALC_TABLE(TABLE=tabout,reuse=tabout,
                           ACTION=(_F(OPERATION='SUPPRIME',
                                      NOM_PARA='K1',),
                                   _F(OPERATION='AJOUT_COLONNE',
                                      VALE=0.,
                                      NOM_PARA='K1'),),
                          )

         # calcul des nouvelles valeurs de G et/ou de G_IRWIN
         list_G = [x for x in ['G','G_IRWIN'] if x in tabin.para]
         for param_G in list_G :
            tabout=CALC_TABLE(TABLE=tabout,reuse=tabout,
                              ACTION=(_F(OPERATION='SUPPRIME',
                                         NOM_PARA=param_G),
                                      _F(OPERATION='OPER',
                                         FORMULE=__formul_G,
                                         NOM_PARA=param_G),),
                             )
         # association des tableaux 1 et 2
         tabout= CALC_TABLE(TABLE=tabout,reuse=tabout,
                            ACTION=_F(TABLE=__tabtmp,OPERATION='COMB'),
                           )

         # tri du tableau de sortie
         if 'NUME_FOND' or 'INST' or 'NUM_PT' in tabin.para:
            list_para = [x for x in ['NUME_FOND','INST','NUM_PT'] if x in tabin.para]
            tabout= CALC_TABLE(TABLE=tabout,reuse=tabout,
                               ACTION=_F(OPERATION='TRI',
                                         NOM_PARA=list_para ,
                                         ORDRE= 'CROISSANT'),
                              )

      # si on a trouve au moins une valeur negative de K1 et G non present
      elif change == True and ('G' and 'G_IRWIN' not in tabin.para) :

         # formule servant a mettre a zero les valeurs negatives de K1         
         self.update_const_context({'mise_zero' : mise_zero})
         __formul = FORMULE( NOM_PARA = 'K1',
                             VALE = 'mise_zero(K1)')

         # mise a zero des valeurs de K1 negatives
         tabout=CALC_TABLE(TABLE=TABIN,reuse=TABIN,
                           ACTION=(_F(OPERATION='OPER',
                                      FORMULE=__formul,
                                      NOM_PARA='K1_bis'),
                                   _F(OPERATION='SUPPRIME',
                                      NOM_PARA='K1'),
                                   _F(OPERATION='RENOMME',
                                      NOM_PARA=('K1_bis','K1')),),
                          )
      else :
         # tous les K1 sont >=0: aucune modification a effectuer
         tabout=TABIN

   return

