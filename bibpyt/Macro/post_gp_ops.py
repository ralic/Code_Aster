#@ MODIF post_gp_ops Macro  DATE 10/10/2011   AUTEUR MACOCCO K.MACOCCO 
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

EnumTypes = (list, tuple)

# -----------------------------------------------------------------------------
def post_gp_ops(self, **args):
   """
      Corps de la macro POST_GP
   """
   import pdb
   macro = 'POST_GP'
   ier=0
   from Accas import _F
   from Utilitai.Utmess       import UTMESS
   from Utilitai.Table        import Table, merge, Colonne
   from Cata_Utils.t_fonction import t_fonction
   from Cata.cata             import evol_noli
   import aster
   import string
   import numpy as NP
   from sets import Set
   global DEFI_GROUP, POST_RELEVE_T, DETRUIRE
   # ----- On importe les definitions des commandes a utiliser dans la macro
   CALC_THETA    = self.get_cmd('CALC_THETA')
   CALC_G        = self.get_cmd('CALC_G')
   POST_ELEM     = self.get_cmd('POST_ELEM')
   POST_RELEVE_T = self.get_cmd('POST_RELEVE_T')
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   DEFI_LIST_ENTI= self.get_cmd('DEFI_LIST_ENTI')
   CALC_ELEM     = self.get_cmd('CALC_ELEM')
   RECU_FONCTION = self.get_cmd('RECU_FONCTION')
   DEFI_GROUP    = self.get_cmd('DEFI_GROUP')
   EXTR_RESU      = self.get_cmd('EXTR_RESU')
   DETRUIRE      = self.get_cmd('DETRUIRE')
   FIN           = self.get_cmd('FIN')

   tmp_liste_inst_postgp = []

   #----------------------------------------------------------
   # ----- Comptage, commandes + déclaration concept sortant
   #----------------------------------------------------------
   self.set_icmd(1)
   self.DeclareOut('result', self.sd)
   self.DeclareOut('tabresult', self['TABL_GPMAX'])
   if (self['TABL_GP']!= None ):
       self.DeclareOut('tabgp', self['TABL_GP'])

   liste_inst_post = self['LIST_INST']
   Resultat = EXTR_RESU(RESULTAT=self['RESULTAT'],ARCHIVAGE=_F(LIST_INST=liste_inst_post))
   liste_inst_postgp = aster.getvectjev(string.ljust(liste_inst_post.nom,19)+'.VALE')


   #----------------------------------------------------------
   # ---- Recuperation du nombre d'instants deja calculés
   #----------------------------------------------------------
   if aster.getvectjev(string.ljust(Resultat.nom,19)+'.ORDR        ').__contains__(0):UTMESS('F','POST0_37')
   num_ord = len(aster.getvectjev(string.ljust(Resultat.nom,19)+'.ORDR        '))
   liste_ord = aster.getvectjev(string.ljust(Resultat.nom,19)+'.ORDR        ')


   # Cas 2D
   if self['THETA_2D'] is not None:
      is_2D = True
   else:
      is_2D = False
      UTMESS('A','POST0_38')
      # TABLE GP à remplier pour 3D
      liste_3d_inst =  []
      liste_3d_lieu =  []
      liste_3d_icop =  []
      liste_3d_deltal =  []
      liste_3d_gp =  []

   info = self['INFO']
   type_def = self['TYPE_DEF']


   #----------------------------------------------------------
   # 0. ----- Type de calcul
   #----------------------------------------------------------
   identification = self['IDENTIFICATION'] != None
   if identification:
      # 0.1. --- identification : on boucle sur les valeurs de TEMP.
      #          Pour chaque couple (T, Kjc(T)), on évalue les Ki, Kmoy et
      #          les valeurs de Gpmax, DeltaLmax, inst.max correspondantes.
      mccalc = self['IDENTIFICATION']
      l_crit = mccalc['KJ_CRIT']
      lv_ident = []
      l_temp = mccalc['TEMP']
   else:
      # 0.2. --- prédiction : on ne fait qu'une itération.
      #          Il faut un RESU_THER (sinon on utilise la température du
      #          premier Gpcrit et cà n'a pas trop d'intéret).
      #          A chaque instant, on regarde à quelle température est le
      #          fond d'entaille et on compare Gpmax à cet instant au Gpcrit.
      mccalc = self['PREDICTION']
      l_crit = mccalc['GP_CRIT']
      lv_pred = []
      l_temp = mccalc['TEMP'][0]

   if not type(l_temp) in EnumTypes:
      l_temp = [l_temp,]
   if not type(l_crit) in EnumTypes:
      l_crit = [l_crit,]


   #----------------------------------------------------------
   # Maillage associe au modele
   #----------------------------------------------------------
   __MAIL = aster.getvectjev( self['MODELE'].nom.ljust(8) + '.MODELE    .LGRF        ' )
   nom_maillage = __MAIL[0].strip()

   maya = self.get_concept(nom_maillage)
   ltyma =aster.getvectjev("&CATA.TM.NOMTM")

   #----------------------------------------------------------
   # Excitation
   #----------------------------------------------------------
   args={}
   if self['EXCIT']:args={'EXCIT'   : self['EXCIT'].List_F()}


   #----------------------------------------------------------
   # 1. ----- calcul de G-theta
   #----------------------------------------------------------

   if is_2D:
      nbcour = len(self['THETA_2D'])
      l_tab = []
      for occ in self['THETA_2D']:
         dMC = occ.cree_dict_valeurs(occ.mc_liste)

         __theta = CALC_THETA(MODELE=self['MODELE'],
                              DIRECTION=self['DIRECTION'],
                              THETA_2D=_F(GROUP_NO=dMC['GROUP_NO'],
                                          MODULE=1.0,
                                          R_INF=dMC['R_INF'],
                                          R_SUP=dMC['R_SUP']),)

         __gtheta = CALC_G(THETA=_F(THETA=__theta),
                           RESULTAT=Resultat,
                           TOUT_ORDRE='OUI',
                           SYME_CHAR=self['SYME_CHAR'],
                           COMP_ELAS=self['COMP_ELAS'].List_F(),
                           **args)

         tab = __gtheta.EXTR_TABLE()

         # une Table par couronne
         l_tab.append(tab)

   else:
      #Cas 3D
      nbcour = len(self['THETA_3D'])
      l_tab = []
      l_noeuds_fissure, pas = getFondFissInfo(self['FOND_FISS'])
      nb_noeuds_fissure = len(l_noeuds_fissure)
      FOND_FISS =  self['FOND_FISS']
      lnormale = aster.getvectjev(FOND_FISS.nom.ljust(8)+'.NORMALE')
      if (lnormale==None):UTMESS('F','POST0_39')

      for occ in self['THETA_3D']:
         dMC = occ.cree_dict_valeurs(occ.mc_liste)

         # on met les mots-clés facultatifs dans des dictionnaires
         dpar_theta = {}

         __gtheta = CALC_G(
                           THETA=_F(R_INF=dMC['R_INF'],
                                    R_SUP=dMC['R_SUP'],
                                    MODULE=1.0,
                                    FOND_FISS=self['FOND_FISS'],
                                    **dpar_theta),
                           RESULTAT=Resultat,
                           TOUT_ORDRE='OUI',
                           SYME_CHAR=self['SYME_CHAR'],
                           COMP_ELAS=self['COMP_ELAS'].List_F(),
                           LISSAGE=self['LISSAGE'].List_F(),
                           **args)


         tab = __gtheta.EXTR_TABLE()

         # une Table par couronne
         l_tab.append(tab)

   #----------------------------------------------------------
   # 1.1.----- calcul de la mesure des mailles
   #           appartenant à l'axe de symétrie
   #           ou au plan de symétrie
   #----------------------------------------------------------
   l_copo_tot = []
   l_ep_copeaux_tot_3D = []
   for tmpocc in self['TRANCHE']:
     dMCT = tmpocc.cree_dict_valeurs(tmpocc.mc_liste)
     for grma in dMCT['GROUP_MA'] :
       l_copo_tot.append(grma)
   nb_tranches = len(self['TRANCHE'])
   #  En supposant que le nombre de copeaux est identique par tranche
   nbcop_tot = len(l_copo_tot)
   nbcop = nbcop_tot/nb_tranches


   if is_2D:
      # Récupération des noeuds appartenant à la droite de symétrie
      # le vecteur normal est construit comme étant perpendiculaire au vecteur direction
      # La commande CALC_THETA/THETA_2D suppose que la composante suivant z est nulle
       lnormale = (self['DIRECTION'][1],self['DIRECTION'][0])
       Nds_fdfiss = dMC['GROUP_NO']
       Recup_Noeuds_Surf(is_2D,maya,Nds_fdfiss,lnormale)
       mesure = Calcul_mesure_2D(is_2D,maya,nbcop,num_ord,l_copo_tot,ltyma,Resultat,type_def)

   else:
      # Récupération des noeuds appartenant à la surface de symétrie
      # le vecteur normal est récuperé dans FOND_FISS
       FOND_FISS =  self['FOND_FISS']
       Recup_Noeuds_Surf(is_2D,maya,l_noeuds_fissure[0],lnormale,l_noeuds_fissure)
       mesure, l_ep_copeaux_tot_3D = Calcul_mesure_3D(is_2D,maya,nbcop,num_ord,l_copo_tot,ltyma,Resultat,type_def)
       l_ep_copeaux_tot_3D = l_ep_copeaux_tot_3D*num_ord

   #----------------------------------------------------------
   # 2. ----- Calcul de l'energie élastique en exploitant les groupes de
   #          mailles fournis par la procedure de maillage
   #----------------------------------------------------------
   l_ep_copeaux_tot = []
   for it in range(len(liste_ord)):
     for Copeau_k in l_copo_tot :
       l_ep_copeaux_tot.append(mesure[Copeau_k][it])
   l_t_enel = []

   if self['TRAC_COMP']=='OUI':
      # prise en compte de la traction-compression dans le calcul de l'energie
      resu2=CALC_ELEM(OPTION=('SIEQ_ELNO'),
                     RESULTAT=Resultat,
                     )

      # indices des mailles du dernier group_ma
      # (pour avoir le nombre de mailles par tranche)
      l_mailles_last_gm = maya.sdj.GROUPEMA.get()[l_copo_tot[-1].ljust(8)]

      # initialisation des concepts reutilises dans la boucle
      # on suppose que chaque tranche a le meme nombre de mailles

      kk = 0

      E_el = [None]*len(l_mailles_last_gm)*nb_tranches

      T_el = [None]*len(l_mailles_last_gm)*nb_tranches

      # on recupere les sd en dehors de la boucle
      maya_GROUPEMA = maya.sdj.GROUPEMA.get()
      maya_NOMMAI = maya.sdj.NOMMAI.get()
      maya_CONNEX = maya.sdj.CONNEX.get()
      maya_NOMNOE = maya.sdj.NOMNOE.get()

   # liste des tables tb_Gpmax repartie aux noeuds
   l_tb_Gpmax_noeuds = []

   # Charges
   args={}
   if self['EXCIT']:args={'CHARGE': [charg['CHARGE'] for charg in self['EXCIT']]}
   for i in range(0,nb_tranches):
      l_copo = l_copo_tot[i*nbcop:(i+1)*nbcop]

      if info >= 2 and not is_2D:
         print "<I> Calcul de la tranche %i"%(i+1)

      if self['TRAC_COMP']=='OUI':

         # l_copo est une liste commulative de mailles
         # il faut lancer POST_ELEM sur chaque maille de chaque copeau
         # puis regarder la trace de SIEF_ELGA sur ce copeau

         # on fera attention a ne pas lancer POST_ELEM sur une maille qui
         # a deja ete calculee en stockant son resultat pour la maille en question
         d_groupma={}
         d_nomma={}

         # indices des mailles des copeaux
         for group_ma in l_copo:
            d_groupma[group_ma] = maya_GROUPEMA[group_ma.ljust(8)]

         # le dernier copeau contient tous les elements
         # on calcule l energie de chaque element de ce copeau
         last_copo = l_copo[-1]

         d_ener = {}

         d_nomma = {}

         for k, id_elem in enumerate(d_groupma[last_copo]):


            # les id des elements dans Aster commencent a 1
            # la liste python commence a 0
            elem = maya_NOMMAI[id_elem-1]
            d_nomma[id_elem]=elem

            E_el[kk] = POST_ELEM(MODELE=self['MODELE'],
                                 RESULTAT=Resultat,
                                 TOUT_ORDRE='OUI',
                                 ENER_ELAS=_F(MAILLE=elem),
                                 TITRE='Energie élastique',
                                 **args)

            T_el[kk] = E_el[kk].EXTR_TABLE()

            l_enel = T_el[kk].TOTALE.values()

            # signe de la trace <=> signe de la composante VMIS_SG du tenseur SIEQ_ELNO,
            # mais E_enel est par element => on fait une moyenne sur les noeuds de l'element

            list_no = []
            for ind_no in maya_CONNEX[id_elem] :
               nomnoe = maya_NOMNOE[ind_no-1]
               if nomnoe not in list_no :
                  list_no.append(nomnoe)

            l_inst = T_el[kk].INST.values()
            nb_inst = len(l_inst)

            T_noeuds = Table()
            T_noeuds['INST']=l_inst

            # pour chaque noeud de l'element on recupere sa trace
            for noeud in list_no:

               __VM=RECU_FONCTION(RESULTAT=resu2,
                                    TOUT_INST='OUI',
                                    NOM_CHAM='SIEQ_ELNO',
                                    NOM_CMP='VMIS_SG',
                                    MAILLE=elem,
                                    NOEUD=noeud);

               T_noeuds[noeud]=__VM.Ordo()

            T_noeuds.fromfunction('VM_MAIL', moyenne, list_no)

            l_VM_MAIL = T_noeuds.VM_MAIL.values()

            for j, vm in enumerate(l_VM_MAIL):
               if vm < 0:
                  l_enel[j]=-l_enel[j]

            del T_el[kk]['TOTALE']
            T_el[kk][elem]=l_enel

            if k==0:

               # Table de l'energie elastique sur le GROUP_MA
               T_el_gm = Table()
               T_el_gm['NUME_ORDRE'] = T_el[kk].NUME_ORDRE.values()
               T_el_gm['INST'] = T_el[kk].INST.values()
               T_el_gm['LIEU'] = [last_copo]*nb_inst
               T_el_gm['ENTITE'] = ['GROUP_MA']*nb_inst

            T_el_gm[elem]=l_enel

            kk+=1

         # sommation sur les mailles du group_ma:
         l_nomma = d_nomma.values()
         T_el_gm.fromfunction('TOTALE', mysum, l_nomma)

         # Table totale
         t_enel=Table(titr="Energie élastique")
         t_enel['NUME_ORDRE']=T_el_gm.NUME_ORDRE.values()
         t_enel['INST']=T_el_gm.INST.values()
         t_enel['LIEU']=T_el_gm.LIEU.values()
         t_enel['ENTITE']=T_el_gm.ENTITE.values()
         t_enel['TOTALE']=T_el_gm.TOTALE.values()

         # t_enel ne contient jusqu'ici que l'energie elastique du dernier copeau

         # calcul de l'energie elastique pour les autres copeaux
         T_el_sub = T_el_gm.copy()

         for k in range(len(l_copo)-2,-1,-1):
            group_ma = l_copo[k]
            T_el_sub = T_el_sub.copy()
            del T_el_sub['LIEU']
            del T_el_sub['TOTALE']
            T_el_sub['LIEU']=[group_ma]*nb_inst
            l_id_elem = d_groupma[group_ma]
            l_nom_elem = []

            for id_elem, nom_elem in d_nomma.items():
               if not id_elem in l_id_elem:
                  # colonne a supprimer
                  del T_el_sub[nom_elem]
                  del d_nomma[id_elem]
               else:
                  l_nom_elem.append(nom_elem)

            T_el_sub.fromfunction('TOTALE', sum_and_check, l_nom_elem)

            # Table de l'energie elastique sur le GROUP_MA
            T_el_gm_k = Table()
            T_el_gm_k['NUME_ORDRE'] =T_el_sub.NUME_ORDRE.values()
            T_el_gm_k['INST'] = T_el_sub.INST.values()
            T_el_gm_k['LIEU'] = [group_ma]*nb_inst
            T_el_gm_k['ENTITE'] = ['GROUP_MA']*nb_inst
            T_el_gm_k['TOTALE'] = T_el_sub.TOTALE.values()

            # contribution du group_ma a la table totale:
            t_enel = merge(t_enel, T_el_gm_k)

         t_enel.sort('NUME_ORDRE')

      else:
         # si self['TRAC_COMP']!='OUI'
         # calcul classique de l'energie elastique

         __ener = POST_ELEM(MODELE=self['MODELE'],
                                 RESULTAT=Resultat,
                                 TOUT_ORDRE='OUI',
                                 ENER_ELAS=_F(GROUP_MA=l_copo),
                                 TITRE='Energie élastique',
                                 **args)

         t_enel = __ener.EXTR_TABLE()

      # 2.1. ----- Indice de chaque copeau et deltaL
      d_icop = dict(zip(l_copo, range(1, nbcop + 1)))

      l_lieu = [grma.strip() for grma in t_enel.LIEU.values()]

      l_icop = [d_icop[grma] for grma in l_lieu]

      t_enel['ICOP'] = l_icop

      l_numord = list(set(t_enel.NUME_ORDRE.values()))
      l_numord.sort()

      l_ep_copeaux_tranche=[]
      for it in range(num_ord):
        l_ep_copeaux_tranche = l_ep_copeaux_tranche + l_ep_copeaux_tot[i*nbcop+it*nbcop*nb_tranches:(i+1)*nbcop+it*nbcop*nb_tranches]
      l_ep_copeaux_tranche_3D = l_ep_copeaux_tot_3D[i*nbcop:(i+1)*nbcop]

      # 2.2. ----- Calcul de Gp fonction de Ener.Totale et de deltaL
      if is_2D:
         t_enel['DELTAL'] = l_ep_copeaux_tranche
         t_enel.fromfunction('GP', fGp_Etot, ('TOTALE', 'DELTAL'),
            {'syme'   : self['SYME_CHAR'] != 'SANS',
             'R'      : self['RAYON_AXIS'],})
      else:
         ep_tranche=1
         t_enel['MESURE'] = l_ep_copeaux_tranche
         t_enel['DELTAL'] = l_ep_copeaux_tranche_3D*len(l_numord)
         t_enel.fromfunction('GP', fGp_Etot, ('TOTALE', 'MESURE'),
               {'syme'   : self['SYME_CHAR'] != 'SANS',
                'R'      : ep_tranche })

      #if info >= 2:
      #   print "Table de l'énergie élastique: ", t_enel

      l_t_enel.append(t_enel)

      # 2.3. ----- Tableau de Gp = f(icop) pour chaque instant

      if info >= 2:
         tGp_t_icop = t_enel['INST', 'DELTAL', 'GP']
         tGp_t_icop.titr = "Gp à chaque instant en fonction de la distance au " \
                           "fond d'entaille"
         tGp_t_icop.ImprTabCroise()

      # 2.4. ----- Table Gpmax
      ttmp = t_enel['NUME_ORDRE', 'INST', 'ICOP', 'DELTAL', 'GP']

      for j in l_numord:
         tj = ttmp.NUME_ORDRE == j

         ## pour tester le comportement de Gpmax quand GP est identiquement nul
         #del tj['GP']
         #tj['GP']=[0]*len(tj.GP.values())

         if self['CRIT_MAXI_GP'] == 'ABSOLU':
            t = tj.GP.MAXI()
         else:
            t = MaxRelatif(tj, 'GP')


         # cas GP identiquement nul: plusieurs max de GP
         # on prend le DELTAL minimum
         if len(t.GP.values())>1:
            t = t.DELTAL.MINI()

         if l_numord.index(j) == 0:
            tb_Gpmax_i = t
         else:
            tb_Gpmax_i = tb_Gpmax_i | t

      tb_Gpmax_i.Renomme('GP', 'GPMAX')
      tb_Gpmax_i.Renomme('ICOP', 'ICOPMAX')
      tb_Gpmax_i.Renomme('DELTAL', 'DELTALMAX')
      tb_Gpmax_i.titr = 'Gpmax à chaque instant'

      # On transfert Gpmax au noeud sommet à gauche et au milieu (si cas quadratique)
      # sauf pour la dernière tranche où on transfère également au noeud sommet de droite.
      # Tout cela pour pouvoir avoir une table complète avec les G et les Gpmax.
      if not is_2D:
         tb_Gpmax_i['NUME_TRANCHE']=[i+1]*len(tb_Gpmax_i['GPMAX'])
         if i==0:
            l_inst = tb_Gpmax_i.INST.values()
            nb_inst = len(l_inst)
         if pas==1:
            tb_Gpmax_i_noeuds = tb_Gpmax_i.copy()
            tb_Gpmax_i_noeuds['NOEUD']=[l_noeuds_fissure[i]]*nb_inst
            l_tb_Gpmax_noeuds.append(tb_Gpmax_i_noeuds)
         else:
            tb_Gpmax_i_noeuds_1 = tb_Gpmax_i.copy()
            tb_Gpmax_i_noeuds_1['NOEUD'] = [l_noeuds_fissure[pas*i]]*nb_inst
            l_tb_Gpmax_noeuds.append(tb_Gpmax_i_noeuds_1)
            tb_Gpmax_i_noeuds_2 = tb_Gpmax_i.copy()
            tb_Gpmax_i_noeuds_2['NOEUD'] = [l_noeuds_fissure[pas*i+1]]*nb_inst
            l_tb_Gpmax_noeuds.append(tb_Gpmax_i_noeuds_2)
         if i==nb_tranches-1:
            tb_Gpmax_i_noeuds_3 = tb_Gpmax_i.copy()
            tb_Gpmax_i_noeuds_3['NOEUD'] = [l_noeuds_fissure[-1]]*nb_inst
            l_tb_Gpmax_noeuds.append(tb_Gpmax_i_noeuds_3)



      if i == 0:
         tb_Gpmax = tb_Gpmax_i
      else:
         tb_Gpmax = merge(tb_Gpmax, tb_Gpmax_i)

      # 2.5. ----- POUR LE CAS DE 3D
      # RECUPERE LES INFOS NECESSAIRE POUR LA CREATION DU TABLEAU GP (ie TABL_GP)Tableau de Gp = f(icop) pour chaque instant
      if not is_2D:

        for kocc in range(len(t_enel['INST'].values()['INST'])) :
          liste_3d_inst.append(t_enel['INST'].values()['INST'][kocc])
          liste_3d_lieu.append(t_enel['LIEU'].values()['LIEU'][kocc])
          liste_3d_icop.append(t_enel['ICOP'].values()['ICOP'][kocc])
          liste_3d_deltal.append(t_enel['DELTAL'].values()['DELTAL'][kocc])
          liste_3d_gp.append(t_enel['GP'].values()['GP'][kocc])

   # FIN BOUCLE SUR LES TRANCHES

   if not is_2D:
      tb_Gpmax_noeuds = Table(para=tb_Gpmax.para+['NOEUD'])
      for j, tb in enumerate(l_tb_Gpmax_noeuds):
         if j==0:
            tb_Gpmax_noeuds = tb
         else:
            tb_Gpmax_noeuds = merge(tb_Gpmax_noeuds, tb)


   # 2.5. ----- extraction de la température en fond d'entaille
   #voir le cas 3D => THETA_3D, mais qu'en est-il des tranches?
   if self['RESU_THER']:
      #sur un seul noeud ou sur tous les noeuds du fond d'entaille?

      if is_2D:
         grno_fond = self['THETA_2D'][0]['GROUP_NO']
      else:
         grma_fond = self['THETA_3D'][0]['GROUP_MA']
         grno_fond = "GRNOFOND"
         DEFI_GROUP(reuse =maya,
                    MAILLAGE=maya,
                    DETR_GROUP_NO= _F(NOM=grno_fond,),
                    CREA_GROUP_NO=_F(GROUP_MA=grma_fond,
                                     NOM=grno_fond,),);

      l_ordres = DEFI_LIST_ENTI(VALE=l_numord)
      __relev = POST_RELEVE_T(ACTION=_F(RESULTAT=self['RESU_THER'],
                                        OPERATION='EXTRACTION',
                                        INTITULE='Temperature',
                                        NOM_CHAM='TEMP',
                                        LIST_ORDRE=l_ordres,
                                        NOM_CMP='TEMP',
                                        GROUP_NO=grno_fond,),)

      t_relev = __relev.EXTR_TABLE()['NUME_ORDRE', 'NOEUD', 'TEMP']

   # 3. ----- boucle sur les mots-clés facteurs
   #          opérations dépendant de la température
   MATER = self['MATER']
   flag_mat = True

   for iocc, TEMP in enumerate(l_temp):
      # 3.0. ----- Temperature fonction du temps : si on n'a pas de RESU_THER,
      #            on prend la température d'identification.
      if not self['RESU_THER']:
         l_rows = [{'NUME_ORDRE' : j, 'TEMP' : TEMP} for j in l_numord]
         t_relev = Table(rows=l_rows, para=('NUME_ORDRE', 'TEMP'), typ=('R', 'R'))
         flag_mat = True

      # 3.1. ----- extrait du matériau E(TEMP) et NU(TEMP) (si nécessaire)
      if flag_mat:
         t_relev.fromfunction('YOUNG', CallRCVALE, 'TEMP',
               { 'para' : 'E', 'MATER' : MATER })
         t_relev.fromfunction('NU', CallRCVALE, 'TEMP',
               { 'para' : 'NU', 'MATER' : MATER })
         flag_mat = False

      # 3.2. ----- paramètres
      dict_constantes = {
         'YOUNG' : CallRCVALE(TEMP, 'E', MATER),
         'NU'    : CallRCVALE(TEMP, 'NU', MATER),
      }

      # 3.3. ----- calcul de Kj(G)
      l_tabi = []
      for k, tab in enumerate(l_tab):
         #tab: table de la couronne k

         # calcul de Kj(G) = K_i
         new_para = 'K_%d' % (k + 1)
         if is_2D:
            # fusion avec TEMP, E et nu
            tab = merge(tab, t_relev, 'NUME_ORDRE')
            tab.fromfunction(new_para, fKj, ('G', 'YOUNG', 'NU'))
            # renomme G en G_i
            tab.Renomme('G', 'G_%d' % (k + 1))
         else:
            if self['RESU_THER']:
               tab=merge(tab, t_relev, ['NUME_ORDRE', 'NOEUD'])
            else:
               tab=mergeLineInTable(tab, t_relev, nb_noeuds_fissure)

            # en 3D, le paramètre R n'intervient pas
            tab.fromfunction(new_para, fKj, ('G', 'YOUNG', 'NU'))
            tab.Renomme('G', 'G_%d' % (k + 1))

         l_tabi.append(tab)

      # 3.4 ----- Table des Gi, Ki sur les differentes couronnes + Kmoyen
      if is_2D:
         tabK_G = l_tabi[0]['NUME_ORDRE']
         for tab in l_tabi:
            tabK_G = merge(tabK_G, tab, 'NUME_ORDRE')
      else:
         tabK_G=l_tabi[0]
         for i in range(1,len(l_tabi)):
            tabK_G = merge(tabK_G, l_tabi[i], ['NUME_ORDRE', 'NOEUD'])
      tabK_G.titr = 'G et K sur les differentes couronnes + moyennes'
      tabK_G.fromfunction('GMOY', moyenne_positive, ['G_%d' % (k + 1) for k in range(nbcour)])
      tabK_G.fromfunction('KMOY', moyenne, ['K_%d' % (k + 1) for k in range(nbcour)])

      # 3.5. ----- Contribution à la table globale

      if is_2D:
         tabres = merge(tabK_G, tb_Gpmax, 'NUME_ORDRE')
         tabres['OCCURRENCE'] = [iocc + 1] * len(l_numord)
      else:
         # tb_Gpmax est une table sur les tranches,
         # on l'ajoute dans la table aux noeuds tabres avec la convention:
         # au 1er noeud et noeud milieu de la tranche on affecte la valeur de la tranche
         # sauf pour la derniere tranche où on affecte la valeur sur les 3 noeuds de la tranche
         tabres = tabK_G
         tabres = merge(tabK_G, tb_Gpmax_noeuds, ['NUME_ORDRE', 'NOEUD'])
         tabres['OCCURRENCE'] = [iocc + 1] * len(l_numord) * nb_noeuds_fissure
      #if info >= 2:
      #   print tabres

      # 3.5.1. --- Table globale
      if iocc == 0:
         tabl_glob = tabres
      else:
         tabl_glob = merge(tabl_glob, tabres)
      tabl_glob.titr = 'G, K sur les differentes couronnes, Gmoy, Kmoy et ' \
                       'Gpmax fonctions du temps'

      # 3.6. ----- traitement selon identification / prédiction
      d_para = {
         'INTERPOL' : ['LIN', 'LIN'],
         'NOM_PARA' : 'INST',
         'PROL_DROITE' : 'CONSTANT',
         'PROL_GAUCHE' : 'CONSTANT',
      }

      # 3.6.1. --- identification
      if identification:
         KJ_CRIT = l_crit[iocc]
         # on verifie que KJ_CRIT soit compris dans l'intervalle [KMOY_min, KMOY_max]
         valkmoy = tabres.KMOY.values()
         message_kjcritique_non_atteint = 0
         if not (min(valkmoy) <= KJ_CRIT <= max(valkmoy)):
            UTMESS('A','RUPTURE0_1')
            message_kjcritique_non_atteint = 1
         if is_2D:
            # définition des fonctions pour faire les interpolations
            d_para.update({ 'NOM_RESU' : 'DELTALMAX' })
            # DeltaMax en fonction du temps
            fdL = t_fonction(tb_Gpmax.INST.values(), tb_Gpmax.DELTALMAX.values(), d_para)
            # Gpmax fonction du temps
            d_para.update({ 'NOM_RESU' : 'GPMAX' })
            fGp = t_fonction(tb_Gpmax.INST.values(), tb_Gpmax.GPMAX.values(), d_para)

            d_para.update({ 'NOM_PARA' : 'KMOY',
                           'NOM_RESU' : 'INST', })
            valkmoy = tabres.KMOY.values()
            if len(liste_inst_postgp) == 1 :
                # si un seul instant alors
                ti = liste_inst_postgp[0]
            else:

                # temps en fonction de Kmoy
                finv = t_fonction(valkmoy, tabres.INST.values(), d_para)

                # valeurs à mettre dans la table
                # temps correspondant à KJ_CRIT
                ti   = finv(KJ_CRIT)
                ind_instant_plus = [x > ti for x in liste_inst_postgp].index(True)
                if (abs(liste_inst_postgp[ind_instant_plus-1]-ti)/ti) <= 1e-4:
                   ti = liste_inst_postgp[ind_instant_plus-1]
                elif (abs(liste_inst_postgp[ind_instant_plus]-ti)/ti) <= 1e-4:
                   ti = liste_inst_postgp[ind_instant_plus]
                else:
                   UTMESS('A','POST0_36',valr =tuple(liste_inst_postgp[ind_instant_plus-1:ind_instant_plus+1]))

            # GP correspondant au temps critique
            if (message_kjcritique_non_atteint == 1) :
              Gpi     = '-'
              kgpcrit = '-'
              dLcrit  = '-'
              message = 'KJ CRITIQUE NON ATTEINT'
              type_tab_ident=('R', 'R', 'K8', 'K8', 'K8', 'K24' )

            else :
              Gpi  = fGp(ti)
              kgpcrit = fKj(Gpi, **dict_constantes)
              dLcrit  = fdL(ti)
              message = 'KJ CRITIQUE ATTEINT'
              type_tab_ident=('R', 'R', 'R', 'R', 'R', 'K24' )


            d_ident = {
               'KJ_CRIT'     : KJ_CRIT,
               'INST'        : ti,
               'GP_CRIT'     : Gpi,
               'KGP_CRIT'    : kgpcrit,
               'DELTALCRIT'  : dLcrit,
               'MESSAGE'     : message
             }
            lv_ident.append(d_ident)


         else:

            row_KMOY_max = tabres.KMOY.MAXI()
            noeud_KMOY_max = row_KMOY_max.NOEUD.values()[0]
            Gpi = 0.0
            nume_tranche_kjcrit = 0
            # On determine par interpolation le temps critique ou KJ_CRIT est atteint
            tabres_NoeudCrit = tabres.NOEUD==noeud_KMOY_max
            d_para.update({ 'NOM_PARA' : 'KMOY',
                           'NOM_RESU' : 'INST', })
            valkmoy = tabres_NoeudCrit.KMOY.values()
            if len(liste_inst_postgp) == 1 :
                # si un seul instant alors
                ti = liste_inst_postgp[0]
            else:

                # temps en fonction de Kmoy
                finv = t_fonction(valkmoy, tabres_NoeudCrit.INST.values(), d_para)

                # valeurs à mettre dans la table
                # temps correspondant a KJ_CRIT
                ti   = finv(KJ_CRIT)
                ind_instant_plus = [x > ti for x in liste_inst_postgp].index(True)
                if abs(liste_inst_postgp[ind_instant_plus-1]-ti)/ti < 1e-4:
                   ti = liste_inst_postgp[ind_instant_plus-1]
                elif abs(liste_inst_postgp[ind_instant_plus]-ti)/ti < 1e-4:
                   ti = liste_inst_postgp[ind_instant_plus]
                else:
                   UTMESS('A','POST0_36',valr =tuple(liste_inst_postgp[ind_instant_plus-1:ind_instant_plus+1]))

            # Gpmax fonction du temps
            d_para.update({  'NOM_PARA' : 'INST','NOM_RESU' : 'GPMAX' })
            for id_noeud_KMOY_max in range(len( tb_Gpmax.NUME_TRANCHE)):
                if id_noeud_KMOY_max==0:
                  # "Gpmax sur la 1ere tranche"
                  nume_tranche_Gpmax = 1
                elif id_noeud_KMOY_max==(len(l_noeuds_fissure)-1):
                  # "Gpmax sur la derniere tranche"
                  nume_tranche_Gpmax = nb_tranches
                else:
                  # "Gpmax sur une tranche intermediaire"
                  Gpi_tot = Table(para=tb_Gpmax.para)
                  nume_tranche_Gpmax = tb_Gpmax.NUME_TRANCHE[id_noeud_KMOY_max]

                tb_Gpmax_TrancheCrit = tb_Gpmax.NUME_TRANCHE==nume_tranche_Gpmax
                fGp = t_fonction(tb_Gpmax_TrancheCrit.INST.values(),
                                 tb_Gpmax_TrancheCrit.GPMAX.values(),
                                 d_para)
                if (Gpi<fGp(ti)):
                  Gpi = max(Gpi,fGp(ti))
                  nume_tranche_kjcrit = nume_tranche_Gpmax

            # définition des fonctions pour faire les interpolations
            d_para.update({ 'NOM_RESU' : 'DELTALMAX' })
            # DeltaMax en fonction du temps
            fdL = t_fonction(tb_Gpmax_TrancheCrit.INST.values(),
                              tb_Gpmax_TrancheCrit.DELTALMAX.values(),
                              d_para)

            if (message_kjcritique_non_atteint == 1) :
               Gpi     = '-'
               kgpcrit = '-'
               dLcrit  = '-'
               message = 'KJ CRITIQUE NON ATTEINT'
               type_tab_ident=('R', 'R', 'I', 'K8', 'K8', 'K8', 'K24' )
            else :
            # GP correspondant au temps critique
               kgpcrit = fKj(Gpi, **dict_constantes)
               dLcrit  = fdL(ti)
               message = 'KJ CRITIQUE ATTEINT'
               type_tab_ident=('R', 'R', 'I', 'R', 'R', 'R', 'K24' )

             # par rapport a 2D, on ajoute 'NUME_TRANCHE'
            d_ident = {
               'KJ_CRIT'     : KJ_CRIT,
               'INST'        : ti,
               'NUME_TRANCHE': nume_tranche_kjcrit,
               'GP_CRIT'     : Gpi,
               'KGP_CRIT'    : kgpcrit,
               'DELTALCRIT'  : dLcrit,
               'MESSAGE'     : message
              }
            lv_ident.append(d_ident)

      # 3.6.2. --- prédiction
      else:
         pass

   # 4. ----- Construction de la table résultat si demandée
   # 4.1. --- identification
   if identification:
      if is_2D:
          para_tab_ident=('KJ_CRIT', 'INST', 'GP_CRIT', 'KGP_CRIT', 'DELTALCRIT', 'MESSAGE' )
      else:
         para_tab_ident=('KJ_CRIT', 'INST', 'NUME_TRANCHE', 'GP_CRIT', 'KGP_CRIT', 'DELTALCRIT','MESSAGE')
      tab_ident = Table(rows=lv_ident,
                        para=para_tab_ident,
                        typ = type_tab_ident,
                        titr='Identification aux valeurs de tenacités critiques')
      dprod_result = tab_ident.dict_CREA_TABLE()
      if info >= 2:
         print tab_ident

   # 4.2. --- prédiction
   else:
      # définition de la fonction GPcrit = f(TEMP)
      d_para.update({ 'NOM_PARA' : 'TEMP',
                      'NOM_RESU' : 'GP_CRIT', })
      fGpc = t_fonction(mccalc['TEMP'], mccalc['GP_CRIT'], d_para)

      # en 3D, GPMAX et DELTALMAX ne sont pas dans tab_glob: les recuperer de tb_Gpmax

      if is_2D:
         tab_pred = tabl_glob['NUME_ORDRE', 'INST', 'TEMP', 'DELTALMAX', 'GPMAX']
      else:
         tab_pred = tb_Gpmax['NUME_ORDRE', 'INST', 'NUME_TRANCHE', 'DELTALMAX', 'GPMAX']
         # on recupere TEMP de tabl_glob
         tab_temp_tranche=Table(para=['NUME_ORDRE', 'NUME_TRANCHE', 'TEMP'])
         # on fait une moyenne de la temperature sur les noeuds d'une tranche
         for ordre in l_numord:
            tabl_glob_ORDRE_i = tabl_glob.NUME_ORDRE==ordre
            temp_ORDRE_i = tabl_glob_ORDRE_i.TEMP.values()
            for i_tranche in range(nb_tranches):
               l_temp_noeuds_tranche_i = temp_ORDRE_i[i_tranche : i_tranche+pas+1]
               temp_tranche_i = moyenne(*l_temp_noeuds_tranche_i)
               d = {'NUME_ORDRE': ordre, 'NUME_TRANCHE': i_tranche+1, 'TEMP': temp_tranche_i}
               tab_temp_tranche.append(d)
         tab_pred = merge(tab_pred, tab_temp_tranche, ['NUME_ORDRE', 'NUME_TRANCHE'])

      tab_pred.fromfunction('GP_CRIT', fGpc, 'TEMP')
      tab_pred.fromfunction('PREDICTION', crit, ('GP_CRIT', 'GPMAX'))
      tab_pred.titr = 'Comparaison Gpmax à Gpcrit(T)'
      dprod_result = tab_pred.dict_CREA_TABLE()

   # 9. ----- création de la table_sdaster résultat
   dprod = tabl_glob.dict_CREA_TABLE()
   result = CREA_TABLE(**dprod)
   tabresult = CREA_TABLE(**dprod_result)

   # 9. ----- création de la table_sdaster tabgp
   if (self['TABL_GP']!= None ):

     temps_agarde_gp = []
     lv_tabgp = []

     # CAS 2D
     if is_2D :

         for i in range(num_ord) :

           d_tabgp = {
               'INST'         : t_enel['INST'].values()['INST'][i],
               'GROUP_MA'     : t_enel['LIEU'].values()['LIEU'][i],
               'NUMERO_COP'   : t_enel['ICOP'].values()['ICOP'][i],
               'DELTAL'       : t_enel['DELTAL'].values()['DELTAL'][i],
               'GP'           : t_enel['GP'].values()['GP'][i],
                }
           lv_tabgp.append(d_tabgp)

           texte = 'GP 2D pour chaque instant'

     # CAS 3D
     else :

         for i in range(len(liste_3d_inst)) :

           d_tabgp = {
               'INST'         : liste_3d_inst[i],
               'GROUP_MA'     : liste_3d_lieu[i],
               'NUMERO_COP'   : liste_3d_icop[i],
               'DELTAL'       : liste_3d_deltal[i],
               'GP'           : liste_3d_gp[i],
              }
           lv_tabgp.append(d_tabgp)
         texte = 'GP 3D pour chaque instant'


#    Creation du tableau Gp 2D ou 3D
     para_tab_tabgp=('INST', 'GROUP_MA', 'NUMERO_COP', 'DELTAL', 'GP')
     liste_tab_tabgp = ('R','K8','I','R','R')

     tab_tabgp = Table(rows=lv_tabgp,
                       para=para_tab_tabgp,
                       typ = liste_tab_tabgp,
                       titr= texte)


     dprod_tabgp = tab_tabgp.dict_CREA_TABLE()
     tabgp = CREA_TABLE(**dprod_tabgp)
   DETRUIRE(CONCEPT=_F(NOM=Resultat),INFO=1);

# -----------------------------------------------------------------------------
def CallRCVALE(TEMP, para, MATER):
   """Fonction appelant RCVALE et retourne la valeur d'un paramètre.
   """
   valres, flag_ok = MATER.RCVALE('ELAS', 'TEMP', TEMP, para)
   assert len(flag_ok) == 1 and  flag_ok[0] == 0, \
         'Erreur lors de la récupération des valeurs du matériau.'
   return valres[0]

# -----------------------------------------------------------------------------
def fKj(G, YOUNG, NU):
   """Calcul de Kj à partir de G (formule d'Irwin)
   """
   Kj=(abs(G * YOUNG / (1.0 - NU**2)))**0.5
   return Kj

# -----------------------------------------------------------------------------
def fDL(ICOP, pascop):
   """DeltaL = numéro copeau * pas d'entaille
   """
   return ICOP * pascop

# -----------------------------------------------------------------------------
def fGp_Etot(TOTALE, DELTAL, R, syme=False):
   """Gp(Etotale, K), deltal pris dans le context global.
      ICOP      : numéro du copeau,
      DELTAL    : liste des epaisseurs des copeaux
      R         : rayon en axisymetrique,
                  longueur de l'élément 1D situé sur le front d'entaille si modèle 3D.
      syme      : True s'il y a symétrie.
   """
   import types
   fact_syme = 1.
   if syme:
      fact_syme = 2.
   Gp_Etot = fact_syme * TOTALE / (DELTAL * R )
   return Gp_Etot

# -----------------------------------------------------------------------------
def MaxRelatif(table, nom_para):
   """Extrait le dernier maxi du champ `nom_para` de la table.
   """
   l_val = getattr(table, nom_para).values()
   l_val.reverse()
   Vlast = l_val[0]
   for val in l_val:
      if val < Vlast:
         break
      Vlast = val
   return getattr(table, nom_para) == Vlast

# -----------------------------------------------------------------------------
def crit(GP_CRIT, GPMAX):
   """Retourne 1 quand GP_CRIT > GPMAX
   """
   if GPMAX > GP_CRIT:
      return 1
   else:
      return 0

# -----------------------------------------------------------------------------
def moyenne(*args):
   """Fonction moyenne
   """
   return sum(args)/len(args)

def moyenne_positive(*args):
   """Fonction moyenne
   """
   return sum([abs(a) for a in args])/len(args)

def mysum(*args):
   """Fonction sum.
      La fonction sum ne peut pas etre appelee sur une liste de parametre
      d'une table via fromfunction
   """
   return sum(args)

def sum_and_check(*args):
   """Fonction sum.
      Verifie si la somme est positive.
      Si la somme est negative, on la met egale a zero
   """
   somme = sum(args)
   if somme<0:
      somme=0
   return somme

# On recupere des infos sur le fond de fissure
def getFondFissInfo(fondfiss):
   # >FONFISS .FOND      .NOEU        <
   # >FONFISS .FOND      .TYPE        <
   import aster
   l_noeuds_fissure = aster.getvectjev(fondfiss.nom.ljust(8)+'.FOND.NOEU')
   type_mailles = aster.getvectjev(fondfiss.nom.ljust(8)+'.FOND.TYPE')
   if (type_mailles[0].strip() == 'SEG3' ):
      pas = 2
   else:
      pas = 1
   return l_noeuds_fissure, pas

########################################################################
# determination de la distance min entre 2 points consécutifs de la ligne de coupe

def largeur_tranche(nom_maillage, l_noms_noeuds_fissure, pas, i_tranche):
   # >MA      .COORDO    .VALE        <
   from math import sqrt
   import aster

   # tuple des noms des noeuds du maillage
   t_noms_noeuds_maillage = aster.getvectjev(nom_maillage.ljust(8)+'.NOMNOE')
   # on convertit en liste pour utiliser la methode index
   # qui est plus optimal qu'une boucle sur les indices du tuple
   l_noms_noeuds_maillage = list(t_noms_noeuds_maillage)

   l_numeros_noeuds_fissure = []
   for i in range(0,len(l_noms_noeuds_fissure),pas):
      nom = l_noms_noeuds_fissure[i]
      index = l_noms_noeuds_maillage.index(nom)
      l_numeros_noeuds_fissure.append(index)

   coor1=aster.getvectjev(nom_maillage.ljust(8)+'.COORDO    .VALE        ',
                        l_numeros_noeuds_fissure[i_tranche]*3,3)
   coor2=aster.getvectjev(nom_maillage.ljust(8)+'.COORDO    .VALE        ',
                        l_numeros_noeuds_fissure[i_tranche+1]*3,3)

   d=sqrt( (coor1[0]-coor2[0])**2+(coor1[1]-coor2[1])**2+(coor1[2]-coor2[2])**2)
   return d

def mergeLineInTable(multiTable, lineTable, nb_noeuds):
   # on ajoute a la table multiTable les colonnes de lineTable
   # pour chaque nume_ordre autant de fois qu'il y a de nb_noeuds
   from Utilitai.Table      import Table, merge

   l_ordre = lineTable.NUME_ORDRE
   l_para = lineTable.copy().para
   l_para.remove('NUME_ORDRE')
   for i, ordre in enumerate(l_ordre):
      multiTable_i = multiTable.NUME_ORDRE==ordre
      row_i = lineTable.rows[i]
      for para in l_para:
         valeur_i = row_i[para]
         multiTable_i[para] = [valeur_i] * nb_noeuds
      if i==0:
         newTable=multiTable_i
      else:
         newTable = merge(newTable, multiTable_i)

   return newTable

def CalDist(coor_nd,deform,coor_nds,deforms):
   # on calcule la distance du noeud 1 aux autres noeuds

   import numpy as NP

   nbr_noeuds = len(coor_nds)
   if nbr_noeuds == 1:
      dist_min = NP.sqrt((coor_nds[0][0]+deforms[0][0]-coor_nd[0]-deform[0])**2 +
                      (coor_nds[0][1]+deforms[0][1]-coor_nd[1]-deform[1])**2 +
                      (coor_nds[0][2]+deforms[0][2]-coor_nd[2]-deform[2])**2)
   else:
     dist_min = NP.sqrt((coor_nds[0][0]+deforms[0][0]-coor_nd[0]-deform[0])**2 +
                        (coor_nds[0][1]+deforms[0][1]-coor_nd[1]-deform[1])**2 +
                        (coor_nds[0][2]+deforms[0][2]-coor_nd[2]-deform[2])**2)
     for inds in range(1,nbr_noeuds):
       dist = NP.sqrt((coor_nds[inds][0]+deforms[inds][0]-coor_nd[0]-deform[0])**2 +
                      (coor_nds[inds][1]+deforms[inds][1]-coor_nd[1]-deform[1])**2 +
                      (coor_nds[inds][2]+deforms[inds][2]-coor_nd[2]-deform[2])**2)
       if dist < dist_min : dist_min = dist
   return dist_min

def CalSurf(coord_noeuds,deformation):
   # on calcule la surface deformee

   dAB = CalDist(coord_noeuds[1],deformation[1],[coord_noeuds[0]],[deformation[0]])
   dBC = CalDist(coord_noeuds[2],deformation[2],[coord_noeuds[1]],[deformation[1]])
   dCD = CalDist(coord_noeuds[3],deformation[3],[coord_noeuds[2]],[deformation[2]])
   dDA = CalDist(coord_noeuds[0],deformation[0],[coord_noeuds[3]],[deformation[3]])

   mesure = (dAB+dCD)/2.*(dBC+dDA)/2.

   return mesure

#-------------------------------------------------------------
def Recup_Noeuds_Surf(is_2D,maya,var1,var2,var3=None):

      # Récupération des noeuds appartenant à la surface de symétrie
      from Accas import _F
      dicma=[]
      dicma.append({'NOM' : 'Nds_Plan'})
      DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_NO=dicma);
      dicma=[]
      dicma.append({'NOM' : 'Nds_Plan', 'OPTION' : 'PLAN', 'VECT_NORMALE' : var2, 'PRECISION' : 1e-6})
      if is_2D:
        dicma[0].__setitem__('GROUP_NO_CENTRE' , var1)
        DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_NO=dicma);
      else:
        dicma[0].__setitem__('NOEUD_CENTRE' , var1)
        DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_NO=dicma);
        dicma=[]
        dicma.append({'NOM' : 'Nds_Fond', 'NOEUD' : var3 })
        DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_NO=dicma);

def Recup_Noeuds_Copeaux(is_2D,maya,Copeau_k):

      # Récupération des noeuds appartenant à la surface de symétrie
      # et aux copeaux
      from Accas import _F
      dicma=[];
      dicma.append(_F(NOM = Copeau_k))
      dicma.append(_F(NOM = 'Cop_Pl'))
      DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_NO=dicma);

      dicma=[];
      dicma.append(_F(NOM = Copeau_k , GROUP_MA = Copeau_k));
      dicma.append(_F(NOM = 'Cop_Pl' , INTERSEC = (Copeau_k,'Nds_Plan',)));
      DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_NO=dicma);


def Recup_2D(maya,C_k,mon_nom):

   from Accas import _F
   if C_k==0:
       dicma=[]
       dicma.append({'NOM' : mon_nom })
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : mon_nom, 'OPTION' : 'APPUI', 'GROUP_NO' : 'Cop_Pl', 'TYPE_APPUI' : 'TOUT'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma)
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla2'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma)
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla2', 'GROUP_MA' : ('Mai_Plan')})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma)
   else:
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla1' })
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla1', 'OPTION' : 'APPUI', 'GROUP_NO' : 'Cop_Pl', 'TYPE_APPUI' : 'TOUT'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : mon_nom})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : mon_nom, 'DIFFE' : ('Mai_Pla1','Mai_Pla2')})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla2'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla2', 'UNION' : ('Mai_Pla1','Mai_Plan')})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma)

def Recup_3D(maya,C_k,mon_nom):

   from Accas import _F
   if C_k==0:
       dicma=[]
       dicma.append({'NOM' : mon_nom })
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : mon_nom, 'OPTION' : 'APPUI', 'GROUP_NO' : 'Cop_Pl', 'TYPE_APPUI' : 'TOUT', 'TYPE_MAILLE' : '2D'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla2'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla2', 'GROUP_MA' : (mon_nom)})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),


       dicma=[]
       dicma.append(_F(NOM = 'Nds_Delt'))
       dicma.append(_F(NOM = 'Nds_Floc'))
       dicma.append(_F(NOM = mon_nom))
       DEFI_GROUP(reuse =maya,MAILLAGE=maya,DETR_GROUP_NO=dicma,);

       dicma=[]
       dicma.append(_F(NOM = mon_nom,GROUP_MA=mon_nom))
       dicma.append(_F(NOM = 'Nds_Delt',INTERSEC=('Nds_Fond',mon_nom)))
       dicma.append(_F(NOM = 'Nds_Floc',GROUP_NO='Nds_Delt'))
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_NO=dicma);

   else :
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla1' })
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla1', 'OPTION' : 'APPUI', 'GROUP_NO' : 'Cop_Pl', 'TYPE_APPUI' : 'TOUT'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : mon_nom})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : mon_nom, 'DIFFE' : ('Mai_Pla1','Mai_Pla2'), 'TYPE_MAILLE' : '2D'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma),
       dicma=[]
       dicma.append(_F(NOM = 'Nds_Delt'))
       dicma.append(_F(NOM = mon_nom))
       dicma.append(_F(NOM = 'Mai_Pla2'))
       DEFI_GROUP(reuse =maya,MAILLAGE=maya,DETR_GROUP_NO=dicma,);
       dicma=[]
       dicma.append(_F(NOM = mon_nom, GROUP_MA =mon_nom))
       dicma.append(_F(NOM = 'Mai_Pla2', GROUP_MA ='Mai_Pla2'))
       dicma.append(_F(NOM = 'Nds_Delt', INTERSEC = ('Mai_Pla2',mon_nom)))
       DEFI_GROUP(reuse =maya,MAILLAGE=maya,CREA_GROUP_NO=dicma);
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla2'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_MA=dicma),
       dicma=[]
       dicma.append({'NOM' : 'Mai_Pla2', 'GROUP_MA' : 'Mai_Pla1'})
       DEFI_GROUP(reuse =maya, MAILLAGE=maya, CREA_GROUP_MA=dicma);


def Calcul_mesure_2D(is_2D,maya,nbcop,num_ord,l_copo_tot,ltyma,resu,type_def):
   # Calcul de la mesure des mailles déformées ou non
   # appartenant à l'axe de symétrie

   import numpy as NP

   mesure = {}
   mon_nom = 'Mai_Plan'
   for C_k in range(nbcop) :
        Copeau_k = l_copo_tot[C_k]
        Recup_Noeuds_Copeaux(True,maya,Copeau_k)
        Recup_2D(maya,C_k,mon_nom)
        if C_k==0:
          mesure[Copeau_k] = NP.zeros(num_ord)
        else:
          mesure[Copeau_k] = mesure[l_copo_tot[C_k-1]]

        # Calcul de la surface des mailles du copeau courant pour chaque instant
        tmp_mesure = NP.zeros(num_ord)
        tab_noeud=[]
        tab_maille=[]
        for maille_courante in maya.sdj.GROUPEMA.get()[mon_nom.ljust(8)]:
          if ltyma[maya.sdj.TYPMAIL.get()[maille_courante]][0:3]=='SEG':
            connexe = maya.sdj.CONNEX.get()[maille_courante]
            tab_noeud = tab_noeud + [maya.sdj.NOMNOE.get()[connexe[i]-1] for i in range(2)]
            tab_maille.append(maya.sdj.NOMMAI.get()[maille_courante])
        tab_DEP_el = Coord_Recup(tab_noeud,tab_maille,resu)
        num_ord_init  = tab_DEP_el.NUME_ORDRE[0]
        # Calcul de la surface des mailles du copeau courant pour chaque instant
        Coord_int = []
        tab_DEP_el_ord = tab_DEP_el.NUME_ORDRE.values()
        indices = [tab_DEP_el_ord.index(num_ord_init,x) for x in range(tab_DEP_el_ord.index(num_ord_init),tab_DEP_el_ord.index(num_ord_init)+tab_DEP_el_ord.count(num_ord_init))]
        tab_noeud = [tab_DEP_el.NOEUD[x] for x in indices]
        tab_coordX = [tab_DEP_el.COOR_X[x] for x in indices]
        tab_coordY = [tab_DEP_el.COOR_Y[x] for x in indices]
        tab_coordZ = [tab_DEP_el.COOR_Z[x] for x in indices]
        for i_maille_courante in range(len(tab_maille)):
           indic = [tab_noeud.index(tab_noeud[i_maille_courante*2+x]) for x in range(2)]
           Coord_int = Coord_int + [(tab_coordX[indic[x]],tab_coordY[indic[x]],tab_coordZ[indic[x]]) for x in indic]

        # Si le type est grandes déformations alors il faut tenir compte de la déformée lors du calcul de la surface
        if type_def=='GRAND':
          l_ord = list(set(tab_DEP_el_ord))
          for it in range(num_ord):
               Def_int   = []
               indices = [tab_DEP_el_ord.index(l_ord[it],x) for x in range(tab_DEP_el_ord.index(l_ord[it]),tab_DEP_el_ord.index(l_ord[it])+tab_DEP_el_ord.count(l_ord[it]))]
               tab_defoX = [tab_DEP_el.DX[x] for x in indices]
               tab_defoY = [tab_DEP_el.DY[x] for x in indices]
               tab_defoZ = [tab_DEP_el.DZ[x] for x in indices]
               for i_maille_courante in range(len(tab_maille)):
                   indic = [tab_noeud.index(tab_noeud[i_maille_courante*2+x]) for x in range(2)]
                   Def_int   = Def_int   + [(tab_defoX[indic[x]], tab_defoY[indic[x]], tab_defoZ[indic[x]])  for x in indic]
                   tmp_mesure[it] = mesure[Copeau_k][it] + NP.sqrt((Coord_int[0][0]+Def_int[0][0]-Coord_int[1][0]-Def_int[1][0])**2 +
                                 (Coord_int[0][1]+Def_int[0][1]-Coord_int[1][1]-Def_int[1][1])**2 + (Coord_int[0][2]+Def_int[0][2]-Coord_int[1][2]-Def_int[1][2])**2)
          mesure[Copeau_k] = tmp_mesure
        else:
          mesure[Copeau_k] = mesure[Copeau_k] + NP.sqrt((Coord_int[0][0]-Coord_int[1][0])**2 +
                                 (Coord_int[0][1]-Coord_int[1][1])**2 + (Coord_int[0][2]-Coord_int[1][2])**2)

   Supr_mano(maya,mon_nom)

   return mesure

def Calcul_mesure_3D(is_2D,maya,nbcop,num_ord,l_copo_tot,ltyma,resu,type_def):
   # Calcul de la mesure des mailles déformées ou non
   # appartenant à l'axe de symétrie

   import numpy as NP
   import aster
   from Accas import _F
   from Utilitai.Table        import Table

   # Initialisation
   coord_fond = []
   mesure={}
   l_ep_copeaux_tot_3D = []

   mon_nom = 'Mai_Plan'
   COOR = maya.sdj.COORDO.VALE.get()

   # Recupération des noeuds de fond de fissure appartenant à la tranche courante
   Recup_Noeuds_Copeaux(False,maya,l_copo_tot[0])
   Recup_3D(maya,0,mon_nom)
   l_noeud_fond = [noeud_courant for noeud_courant in maya.sdj.GROUPENO.get()['Nds_Floc'.ljust(8)]]
   tab_DEP_el   = Coord_Recup([maya.sdj.NOMNOE.get()[x-1] for x in maya.sdj.GROUPENO.get()['Nds_Floc'.ljust(8)]] ,None,resu)
   coord_fond   = [(tab_DEP_el.COOR_X[x],tab_DEP_el.COOR_Y[x], tab_DEP_el.COOR_Z[x]) for x in range(len(l_noeud_fond))]

   for C_k in range(len(l_copo_tot)) :

       Copeau_k = l_copo_tot[C_k]

       # Recupération des groupes de noeuds appartenant au copeau courant
       Recup_Noeuds_Copeaux(False,maya,Copeau_k)
       Recup_3D(maya,C_k%nbcop,mon_nom)

       # La mesure de la surface est cumulée
       if C_k%nbcop==0:
          mesure[Copeau_k] = NP.zeros(num_ord)
       else :
          mesure[Copeau_k] = mesure[l_copo_tot[C_k-1]]


       # Recupération des coordonnées des noeuds appartenant au copeau courant
       tmp_mesure = NP.zeros(num_ord)
       tab_noeud  = []
       tab_maille = []
       for maille_courante in maya.sdj.GROUPEMA.get()[mon_nom.ljust(8)]:
           if ltyma[maya.sdj.TYPMAIL.get()[maille_courante]][0:4]=='QUAD':
               connexe   = maya.sdj.CONNEX.get()[maille_courante]
               tab_noeud = tab_noeud + [maya.sdj.NOMNOE.get()[connexe[i]-1] for i in range(4)]
               tab_maille.append(maya.sdj.NOMMAI.get()[maille_courante])
       tab_DEP_el    = Coord_Recup(tab_noeud,tab_maille,resu)
       num_ord_init  = tab_DEP_el.NUME_ORDRE[0]

       # Calcul de la surface des mailles du copeau courant pour chaque instant
       Coord_int      = []
       tab_DEP_el_ord = tab_DEP_el.NUME_ORDRE.values()
       indices        = [tab_DEP_el_ord.index(num_ord_init,x) for x in range(tab_DEP_el_ord.index(num_ord_init),tab_DEP_el_ord.index(num_ord_init)+tab_DEP_el_ord.count(num_ord_init))]
       tab_noeud      = [tab_DEP_el.NOEUD[x] for x in indices]
       tab_coordX     = [tab_DEP_el.COOR_X[x] for x in indices]
       tab_coordY     = [tab_DEP_el.COOR_Y[x] for x in indices]
       tab_coordZ     = [tab_DEP_el.COOR_Z[x] for x in indices]
       for i_maille_courante in range(len(tab_maille)):
           indic = [tab_noeud.index(tab_noeud[i_maille_courante*4+x]) for x in range(4)]
           Coord_int = Coord_int + [(tab_coordX[indic[x]],tab_coordY[indic[x]],tab_coordZ[indic[x]]) for x in indic]

       # Si le type est grandes déformations alors il faut tenir compte de la déformée lors du calcul de la surface
       if type_def=='GRAND':
         l_ord = list(set(tab_DEP_el_ord))
         for it in range(num_ord):
               Def_int   = []
               indices = [tab_DEP_el_ord.index(l_ord[it],x) for x in range(tab_DEP_el_ord.index(l_ord[it]),tab_DEP_el_ord.index(l_ord[it])+tab_DEP_el_ord.count(l_ord[it]))]
               tab_defoX = [tab_DEP_el.DX[x] for x in indices]
               tab_defoY = [tab_DEP_el.DY[x] for x in indices]
               tab_defoZ = [tab_DEP_el.DZ[x] for x in indices]
               surface_maille = 0.0
               for i_maille_courante in range(len(tab_maille)):
                   indic = [tab_noeud.index(tab_noeud[i_maille_courante*4+x]) for x in range(4)]
                   Def_int   = Def_int   + [(tab_defoX[indic[x]], tab_defoY[indic[x]], tab_defoZ[indic[x]])  for x in indic]
                   surface_maille = surface_maille + CalSurf(Coord_int,Def_int)
               tmp_mesure[it] = mesure[Copeau_k][it] + surface_maille
         mesure[Copeau_k] = tmp_mesure
       else:
         mesure[Copeau_k] = mesure[Copeau_k] + CalSurf(Coord_int,[tuple(NP.zeros(3))]*len(tab_noeud))

       # Calcul de la distance du copeau au fond d'entaille
       coord_nds = []
       dist_moy=0.
       for noeud_courant in maya.sdj.GROUPENO.get()['Nds_Delt'.ljust(8)]:
            coord_nds = (COOR[(noeud_courant-1)*3],COOR[(noeud_courant-1)*3+1],COOR[(noeud_courant-1)*3+2])
            dist_moy  = dist_moy + CalDist(coord_nds,NP.zeros(3),coord_fond,[tuple(NP.zeros(3))]*len(coord_fond))

       l_ep_copeaux_tot_3D.append(dist_moy/len(maya.sdj.GROUPENO.get()['Nds_Delt'.ljust(8)]))

   Supr_mano(maya,mon_nom)

   return mesure, l_ep_copeaux_tot_3D

def Coord_Recup(noeud_courant,maille_courante,resu):
   # Utilisation de POST_RELEVE_T pour extraire des nouvelles coordonnées des
   # noeuds des copeaux appartenant à la surface de symétrie

   from Accas import _F
   from Utilitai.Table        import Table
   dicarg =[]
   if maille_courante :
       dicarg.append(_F(OPERATION='EXTRACTION',RESULTAT=resu, INTITULE='DEP_el',
                    TOUT_CMP='OUI',NOM_CHAM='DEPL', TOUT_ORDRE='OUI',
                    NOEUD=noeud_courant,MAILLE=maille_courante))
   else:
       dicarg.append(_F(OPERATION='EXTRACTION',RESULTAT=resu, INTITULE='DEP_el',
                    TOUT_CMP='OUI',NOM_CHAM='DEPL',  TOUT_ORDRE='OUI',
                    NOEUD=noeud_courant))

   DEP_el_i = POST_RELEVE_T(ACTION=dicarg)
   tab_DEP_el = DEP_el_i.EXTR_TABLE()
   DETRUIRE(CONCEPT=_F(NOM=DEP_el_i),INFO=1);
   return tab_DEP_el

def Supr_mano(maya,mon_nom):

      from Accas import _F
      dicno=[];
      dicno.append(_F(NOM = 'Cop_Pl'));
      dicno.append(_F(NOM = mon_nom));
      dicno.append(_F(NOM = 'Mai_Pla1'));
      dicno.append(_F(NOM = 'Mai_Pla2'));
      dicno.append(_F(NOM = 'Nds_Plan'));
      dicno.append(_F(NOM = 'Nds_Fond'));
      dicno.append(_F(NOM = 'Nds_Floc'));
      dicno.append(_F(NOM = 'Nds_Delt'));
      dicma=[];
      dicma.append(_F(NOM = mon_nom));
      dicma.append(_F(NOM = 'Mai_Pla1'));
      dicma.append(_F(NOM = 'Mai_Pla2'));
      DEFI_GROUP(reuse =maya, MAILLAGE=maya, DETR_GROUP_NO=dicno, DETR_GROUP_MA=dicma);
