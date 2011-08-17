#@ MODIF post_bordet_ops Macro  DATE 17/08/2011   AUTEUR COURTOIS M.COURTOIS 

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

#definition des fonctions python pour les passer en formule plus tard
def nap_for_py(x,y,F):
   return F(x,y)

def maxi(x,y):
   if x>=y :
      return x
   elif y>x:
      return y

#corps de la macro
def post_bordet_ops(self,
TOUT,
GROUP_MA,
INST,
PRECISION,
CRITERE,
NUME_ORDRE,
PROBA_NUCL,
RESULTAT,
PARAM,
TEMP,
COEF_MULT,**args):
   import numpy as NP
   import aster
   from Accas import _F
   from Cata.cata import fonction_sdaster, nappe_sdaster
   from Utilitai.Utmess import  UTMESS

   ier=0
 #
 # La macro compte pour 1 dans la numerotation des commandes
 #
   self.set_icmd(1)
 #
 # On importe les definitions des commandes a utiliser dans la macro
 #
   CREA_CHAMP = self.get_cmd('CREA_CHAMP')
   CALC_CHAM_ELEM  = self.get_cmd('CALC_CHAM_ELEM')
   CALC_ELEM  = self.get_cmd('CALC_ELEM')
   CREA_TABLE  = self.get_cmd('CREA_TABLE')
   FORMULE     =self.get_cmd('FORMULE')
   CALC_TABLE  =self.get_cmd('CALC_TABLE')
 #
 # Definition du concept sortant dans le contexte de la macro
 #

   self.DeclareOut('tabout', self.sd)

 #
 #Recuperation du champ materiau compris dans le resultat
 #
   iret,ibid,__nom_cham_mater = aster.dismoi('F','CHAM_MATER',RESULTAT.nom,'RESULTAT')
#   if (len(__nom_cham_mater) == 0) or (len(__nom_cham_mater) > 1)  :
   if (__nom_cham_mater.strip() == "#PLUSIEURS") or (__nom_cham_mater.strip() == "#AUCUN") :
        print 'ON EST LA'
        UTMESS('F','RUPTURE1_58')
   else :
        __cham_mater = self.get_concept(__nom_cham_mater.strip())
#
#Recuperation du modele a partir du resultat
   iret,ibid,__n_modele = aster.dismoi('F','MODELE',RESULTAT.nom,'RESULTAT')
   __n_modele=__n_modele.rstrip()
   if len(__n_modele)==0 or __n_modele=="#PLUSIEURS":
      UTMESS('F','RUPTURE1_58')
   __model = self.get_concept(__n_modele)
#

#
 # Creation du dictionnaire des parametres materiau de l'utilisateur
 #
   __DPARAM=PARAM[0].cree_dict_valeurs(PARAM[0].mc_liste)

 # on l'ajoute dans l'environnement pour pouvoir s'en servir dans les commandes
   self.update_const_context({"""__DPARAM""" : __DPARAM})

 #
 #Dimension du modele
 #
   iret,ndim,rbid = aster.dismoi('F','DIM_GEOM',__model.nom,'MODELE')

   if (iret==1) or (ndim==23): UTMESS('F','RUPTURE1_57')

 #
 #Definition des formules pour le calcul de sigy plus tard
 #

   __NPY=FORMULE(NOM_PARA=('EPSI'),VALE="""nap_for_py(EPSI,__TEMPE,__DPARAM['SEUIL_CALC'])""")

   __MAXI=FORMULE(NOM_PARA=('T1'),VALE="""maxi(T1,0.)""")

 # on l'ajoute dans l'environnement pour pouvoir s'en servir dans les commandes
   self.update_const_context({'nap_for_py' : nap_for_py})
   self.update_const_context({'maxi' : maxi})
 #
 #Calcul des grandeurs dont on a besoin : contrainte principale, def plastique et volume du pt de gauss
 #

 #Volume point de gauss
   __VOL_PG=CALC_CHAM_ELEM(MODELE=__model,
                      CHAM_MATER=__cham_mater,
                      TOUT='OUI',
                      OPTION='COOR_ELGA',);
   if GROUP_MA:
      __VOL=__VOL_PG.EXTR_COMP('W',[GROUP_MA])
   elif TOUT :
      __VOL=__VOL_PG.EXTR_COMP('W',[])

#contrainte principale max
   __RESU=CALC_ELEM(
            RESULTAT=self['RESULTAT'],
            OPTION='SIEQ_ELGA');
#            NOM_CMP='PRIN3',);
#deformation plastique
   __RESU=CALC_ELEM(
            reuse=__RESU,
            RESULTAT=self['RESULTAT'],
            OPTION='EPSP_ELGA',
            );
#
#Recuperation de la liste des instants et des ordres de calcul
   __list_ordre=__RESU.LIST_VARI_ACCES()['NUME_ORDRE']
   __list_inst=__RESU.LIST_VARI_ACCES()['INST']

#
#On va travailler en ordre ; si l'utilisateur entre un instant, on va le transformer en ordre
   __entree_instant=None
   if INST :
      if CRITERE=='ABSOLU':
         __prec=PRECISION
      elif CRITERE=='RELATIF':
         __prec=PRECISION*INST
      __entree_instant=True
      __n=0
      __trouv=None
      while (__n<len(__list_inst) and not __trouv):
       if (__list_inst[__n]+__prec>=INST) and (__list_inst[__n]-__prec<=INST):
           __instant=__list_inst[__n]
           __trouv=True
       __n=__n+1
      if not __trouv:
       UTMESS('F','RUPTURE1_53',valr=INST,valk='utilise pour le calcul de Bordet')
   if __entree_instant==True:
          index_ordre=__list_inst.index(__instant)
          nume_ordre=__list_ordre[index_ordre]
   elif NUME_ORDRE:
      nume_ordre=NUME_ORDRE
      if nume_ordre not in __list_ordre :
         UTMESS('F','RUPTURE0_51',vali=int(nume_ordre),valk='utilise pour le calcul de Bordet')
#
#Pour Bordet, il nous faut les champs a tous les instants jusqu'a l'instant considere
#
   __S_TOT=[None for i in range(nume_ordre+1)]  #contrainte principale maximale
   __EPSP=[None for i in range(nume_ordre+1)]   #champ de deformation plastique
   __EP=[[None  for j in range(6)] for i in range(nume_ordre+1)]     #tenseur des deformations plastiques
   __EPEQ=[[None for j in range(0)] for i in range(nume_ordre+1)]   #deformation plastique equivalente
   __EPEQM=[[0.] for i in range(nume_ordre+1)]  #deformation plastique equivalente a l'instant precedent
   __S_BAR=[None for i in range(nume_ordre+1)]
   __PRIN=[[None]for i in range(nume_ordre+1)]
   __EQ_BAR=[[None] for i in range(nume_ordre+1)]
   __EQ_PT=[[None] for i in range(nume_ordre+1)]
   __EQ_PT2=[[None] for i in range(nume_ordre+1)]
   __PR_BAR=[[None]for i in range(nume_ordre+1)]
   __DEP=[[None] for i in range(nume_ordre+1)]
   __BORDTO=0.#valeur sans l'exposant final, que l'on va sommer sur les instants
   __BORDTI=0.#valeur sans l'exposant final, sommee sur les instants
   __BORDTT=[0. for i in range(nume_ordre+1)]#valeur avec l'exposant, que l'on stocke dans la table a chaque instant
   __PROBA=[0. for i in range(nume_ordre+1)]#Probabilite de rupture par clivage

#LISTE DES PARAMETRES
   __sig0=__DPARAM['SEUIL_REFE']
   __sigth=__DPARAM['SIG_CRIT']
   __sigref=__DPARAM['SIGM_REFE']
   __m=__DPARAM['M']
   __V0=__DPARAM['VOLU_REFE']
   if PROBA_NUCL=='OUI':
      __ep0=__DPARAM['DEF_PLAS_REFE']
   elif PROBA_NUCL=='NON':
      __ep0=0
   __c_mult=COEF_MULT
#
#On va constuire des champs a chaque instant
#
   if __list_ordre[0]==0:
      __fin_ordre=nume_ordre+1
   elif __list_ordre[0]!=0:
      __fin_ordre=nume_ordre
   for ordre in range(__list_ordre[0],__fin_ordre):
#
#Temperature a extraire : soit une fonction du temps, soit un reel
#
      if type(TEMP)==fonction_sdaster:
         __TEMPE=TEMP(__list_inst[ordre])
      elif type(TEMP)!=fonction_sdaster:
         __TEMPE=TEMP


      self.update_const_context({'__TEMPE' : __TEMPE})
#
#On met ces grandeurs dans des champs specifiques
#
      __S_TOT[ordre]=CREA_CHAMP(TYPE_CHAM='ELGA_SIEF_R',
                RESULTAT=__RESU,
                OPERATION='EXTR',
                NUME_ORDRE=ordre,
                NOM_CHAM='SIEQ_ELGA',);

      __EPSP[ordre]=CREA_CHAMP(TYPE_CHAM='ELGA_EPSI_R',
                RESULTAT=__RESU,
                OPERATION='EXTR',
                NUME_ORDRE=ordre,
                NOM_CHAM='EPSP_ELGA',);
#
#On recupere la valeur des champs au niveau des groupes qui nous interessent
#

      if GROUP_MA:
         __PRIN[ordre]=__S_TOT[ordre].EXTR_COMP('PRIN_3',[GROUP_MA],0).valeurs;

#Pour la deformation plastique, on construit de quoi calculer sa norme de VMises
         __EP[ordre][0]=__EPSP[ordre].EXTR_COMP('EPXX',[GROUP_MA],0).valeurs;
         __EP[ordre][1]=__EPSP[ordre].EXTR_COMP('EPYY',[GROUP_MA],0).valeurs;
         __EP[ordre][2]=__EPSP[ordre].EXTR_COMP('EPZZ',[GROUP_MA],0).valeurs;
         __EP[ordre][3]=__EPSP[ordre].EXTR_COMP('EPXY',[GROUP_MA],0).valeurs;
         if ndim==3:
            __EP[ordre][4]=EPSP[ordre].EXTR_COMP('EPXZ',[GROUP_MA],0).valeurs;
            __EP[ordre][5]=EPSP[ordre].EXTR_COMP('EPYZ',[GROUP_MA],0).valeurs;

      elif TOUT:
         __PRIN[ordre]=__S_TOT[ordre].EXTR_COMP('PRIN_3',[],0).valeurs;
         __EP[ordre][0]=__EPSP[ordre].EXTR_COMP('EPXX',[],0).valeurs;
         __EP[ordre][1]=__EPSP[ordre].EXTR_COMP('EPYY',[],0).valeurs;
         __EP[ordre][2]=__EPSP[ordre].EXTR_COMP('EPZZ',[],0).valeurs;
         __EP[ordre][3]=__EPSP[ordre].EXTR_COMP('EPXY',[],0).valeurs;
         if ndim==3:
            __EP[ordre][4]=__EPSP[ordre].EXTR_COMP('EPXZ',[],0).valeurs;
            __EP[ordre][5]=__EPSP[ordre].EXTR_COMP('EPYZ',[],0).valeurs;

      nval=len(__PRIN[ordre])
      nval2=len(__EP[ordre][0])
      if nval2!=nval: UTMESS('F','RUPTURE1_54')


      if ndim==3:
         __EPEQ[ordre]=NP.sqrt(2./3.*(__EP[ordre][0]**2+__EP[ordre][1]**2+__EP[ordre][2]**2+2.*__EP[ordre][3]**2+2.*__EP[ordre][3]**2+2.*__EP[ordre][4]**2+2.*__EP[ordre][5]**2))
      elif ndim==2:
         __EPEQ[ordre]=NP.sqrt(2./3.*(__EP[ordre][0]**2+__EP[ordre][1]**2+__EP[ordre][2]**2+2.*__EP[ordre][3]**2))


#
#Construction des champs barre et des champs de vitesse
#
      __EQ_PT2[__list_ordre[0]]=NP.zeros([nval])
      __EPEQ[ordre]=NP.array(__EPEQ[ordre])

      if ordre != __list_ordre[0]:
         dt=__list_inst[ordre]-__list_inst[ordre-1]
         if dt==0 : UTMESS('F','RUPTURE1_55')
         __EPEQM[ordre]=__EPEQ[ordre-1]
         __EQ_BAR[ordre]=(__EPEQ[ordre] +__EPEQ[ordre-1])/2.
         __EQ_PT2[ordre]=(__EPEQ[ordre]-__EPEQ[ordre-1])/(2*dt)
         __EQ_PT[ordre]=__EQ_PT2[ordre-1]+__EQ_PT2[ordre]
         __DEP[ordre]=__EPEQ[ordre]-__EPEQ[ordre-1]
         __PR_BAR[ordre]=(__PRIN[ordre]+__PRIN[ordre-1])/2.

         if type(__DPARAM['SEUIL_CALC'])==fonction_sdaster:
            __sigy=__DPARAM['SEUIL_CALC'](__TEMPE)
         elif type(__DPARAM['SEUIL_CALC'])==nappe_sdaster:
            __EQ_PT[ordre]=list(__EQ_PT[ordre])
            __TAB=CREA_TABLE(LISTE=(
                                   _F(PARA='EPSI',LISTE_R=__EQ_PT[ordre],),
                                   ),
                            )
            __TAB=CALC_TABLE(TABLE = __TAB,
                             reuse =__TAB,
                             ACTION=_F(OPERATION='OPER',
                                       FORMULE=__NPY,
                                       NOM_PARA='TSIGY'),)
            __sigy=__TAB.EXTR_TABLE().values()['TSIGY']
            __sigy=NP.array(__sigy)

         T1=__sigy/__sig0*(__PR_BAR[ordre]**__m-__sigth**__m)
         T1=list(T1)
         __TABT1=CREA_TABLE(LISTE=(
                                   _F(PARA='T1',LISTE_R=T1,),
                                   )
                           )
         __TABT1=CALC_TABLE(TABLE = __TABT1,
                             reuse =__TABT1,
                             ACTION=_F(OPERATION='OPER',
                                       FORMULE=__MAXI,
                                       NOM_PARA='T1BIS'),)

         __T1=__TABT1.EXTR_TABLE().values()['T1BIS']
         __T1=NP.array(__T1)
         if PROBA_NUCL=='OUI':
            __T2=NP.exp(-__sigy/__sig0*__EQ_BAR[ordre]/__ep0)
         elif PROBA_NUCL=='NON':
            __T2=1.
         __T3=__DEP[ordre]
         __T4=__VOL.valeurs/__V0
         __BORDTO=NP.cumsum(__T1*__T2*__T3*__T4)[-1]
         __BORDTI=__BORDTI+__BORDTO

      __BORDTT[ordre]=(__c_mult*__BORDTI)**(1/__m)

      if __sigref(__TEMPE)!=0.:
         __PROBA[ordre]=1-NP.exp(-(__BORDTT[ordre]/__sigref(__TEMPE))**__m)
      elif __sigref(__TEMPE)==0.:
         UTMESS('F','RUPTURE1_56',valr=__list_inst[ordre])

   tabout=CREA_TABLE(LISTE=(
                  _F(PARA='INST',LISTE_R=__list_inst[0:nume_ordre+1]),
                  _F(PARA='SIG_BORDET',LISTE_R=__BORDTT,),
                  _F(PARA='PROBA_BORDET',LISTE_R=__PROBA,),
                  ),)
   return ier
