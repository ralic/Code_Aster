#@ MODIF post_gp_ops Macro  DATE 06/07/2009   AUTEUR COURTOIS M.COURTOIS 
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

from types import ListType, TupleType
EnumTypes = (ListType, TupleType)
from sets import Set

# -----------------------------------------------------------------------------
def post_gp_ops(self, **args):
   """
      Corps de la macro POST_GP
   """
   import pdb
   macro = 'POST_GP'
   ier=0
   from Accas import _F
   from Utilitai.Utmess     import UTMESS
   from Utilitai.Table      import Table, merge, Colonne
   from Utilitai.t_fonction import t_fonction
   from Cata.cata import evol_noli
   import aster
   
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
   FIN           = self.get_cmd('FIN')
   
   # ----- Comptage, commandes + déclaration concept sortant
   self.set_icmd(1)
   self.DeclareOut('result', self.sd)
   self.DeclareOut('tabresult', self['TABL_RESU'])
   info = self['INFO']
   
   # 0. ----- Type de calcul
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
   

   # Maillage associe au modele
   __MAIL = aster.getvectjev( self['MODELE'].nom.ljust(8) + '.MODELE    .LGRF        ' )
   nom_maillage = __MAIL[0].strip()
   
   jdc = CONTEXT.get_current_step().jdc
   maya = jdc.sds_dict[nom_maillage]
   
   # 1. ----- calcul de G-theta
   
   # Cas 2D
   if self['THETA_2D'] is not None:
      is_2D = True
   else:
      is_2D = False
   
   if is_2D:
      nbcour = len(self['THETA_2D'])
      nb_tranches = 1
      ep_z = 1
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
                           EXCIT=self['EXCIT'].List_F(),
                           RESULTAT=self['RESULTAT'],
                           TOUT_ORDRE='OUI',
                           SYME_CHAR=self['SYME_CHAR'],
                           COMP_ELAS=self['COMP_ELAS'].List_F(),)
   
         tab = __gtheta.EXTR_TABLE()
         
         # une Table par couronne
         l_tab.append(tab)
         
   else:
      #Cas 3D
      nbcour = len(self['THETA_3D'])
      nb_tranches = self['NB_TRANCHES']
      l_tab = []
      l_noeuds_fissure, pas = getFondFissInfo(self['FOND_FISS'])
      nb_noeuds_fissure = len(l_noeuds_fissure)
      
      for occ in self['THETA_3D']:
         dMC = occ.cree_dict_valeurs(occ.mc_liste)
         
         # on met les mots-clés facultatifs dans des dictionnaires
         dpar_theta = {}
         if self['DIRECTION'] is not None:
            dpar_theta['DIRECTION'] = self['DIRECTION']
         
         __gtheta = CALC_G(
                           THETA=_F(R_INF=dMC['R_INF'],
                                    R_SUP=dMC['R_SUP'],
                                    MODULE=1.0,
                                    FOND_FISS=self['FOND_FISS'],
                                    **dpar_theta),
                           EXCIT=self['EXCIT'].List_F(),
                           RESULTAT=self['RESULTAT'],
                           TOUT_ORDRE='OUI',
                           SYME_CHAR=self['SYME_CHAR'],
                           COMP_ELAS=self['COMP_ELAS'].List_F(),
                           LISSAGE=self['LISSAGE'].List_F()
                           )

 
         tab = __gtheta.EXTR_TABLE()
 
         
         # une Table par couronne
         l_tab.append(tab)

   

   # 2. ----- Calcul de l'energie élastique en exploitant les groupes de
   #          mailles fournis par la procedure de maillage

   l_copo_tot = [grma.strip() for grma in self['GROUP_MA']]
   nbcop_tot = len(l_copo_tot)
   nbcop = nbcop_tot/nb_tranches
   
   if self['LIST_EP_COPEAUX'] is not None:
      l_ep_copeaux_tot = self['LIST_EP_COPEAUX']
   
   l_t_enel = []
   
   if self['TRAC_COMP']=='OUI':
      # prise en compte de la traction-compression dans le calcul de l'energie
      resu2=CALC_ELEM(OPTION=('EQUI_ELNO_SIGM'),
                     RESULTAT=self['RESULTAT'],
                     )
                     
      # indices des mailles du dernier group_ma
      # (pour avoir le nombre de mailles par tranche)
      l_mailles_last_gm = maya.GROUPEMA.get()[l_copo_tot[-1].ljust(8)]
      
      # initialisation des concepts reutilises dans la boucle
      # on suppose que chaque tranche a le meme nombre de mailles
      
      kk = 0
      
      E_el = [None]*len(l_mailles_last_gm)*nb_tranches

      T_el = [None]*len(l_mailles_last_gm)*nb_tranches
      
      # on recupere les sd en dehors de la boucle
      maya_GROUPEMA = maya.GROUPEMA.get()
      maya_NOMMAI = maya.NOMMAI.get()
      maya_CONNEX = maya.CONNEX.get()
      maya_NOMNOE = maya.NOMNOE.get()

   # liste des tables tb_Gpmax repartie aux noeuds
   l_tb_Gpmax_noeuds = []
      
   for i in range(0,nb_tranches):
      l_copo = l_copo_tot[i*nbcop:(i+1)*nbcop]   
      l_charg = [charg['CHARGE'] for charg in self['EXCIT']]
      
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
                                 RESULTAT=self['RESULTAT'],
                                 CHARGE=l_charg,
                                 TOUT_ORDRE='OUI',
                                 ENER_ELAS=_F(MAILLE=elem),
                                 TITRE='Energie élastique',)
            
            T_el[kk] = E_el[kk].EXTR_TABLE()
            
            l_enel = T_el[kk].TOTALE.values()
            
            # signe de la trace <=> signe de la composante VMIS_SG du tenseur EQUI_ELNO_SIGM,
            # mais E_enel est par element => on fait une moyenne sur les noeuds de l'element
            
            list_no = []
            for ind_no in maya_CONNEX[id_elem] :
               nomnoe = maya_NOMNOE[ind_no-1]
               if nomnoe not in list_no :
                  list_no.append(nomnoe)
            
            # print "liste des noeuds de la maille ", id_elem, ": ", list_no
            
            l_inst = T_el[kk].INST.values()
            nb_inst = len(l_inst)
            
            T_noeuds = Table()
            T_noeuds['INST']=l_inst
               
            # pour chaque noeud de l'element on recupere sa trace
            for noeud in list_no:
               
               __VM=RECU_FONCTION(RESULTAT=resu2,
                                    TOUT_INST='OUI',
                                    NOM_CHAM='EQUI_ELNO_SIGM',
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
                                 RESULTAT=self['RESULTAT'],
                                 CHARGE=l_charg,
                                 TOUT_ORDRE='OUI',
                                 ENER_ELAS=_F(GROUP_MA=l_copo),
                                 TITRE='Energie élastique',)
      
         t_enel = __ener.EXTR_TABLE()
   
      # 2.1. ----- Indice de chaque copeau et deltaL
      d_icop = dict(zip(l_copo, range(1, nbcop + 1)))
      
      l_lieu = [grma.strip() for grma in t_enel.LIEU.values()]
      
      l_icop = [d_icop[grma] for grma in l_lieu]
      
      t_enel['ICOP'] = l_icop
   
      l_numord = list(Set(t_enel.NUME_ORDRE.values()))
      l_numord.sort()

      if self['PAS_ENTAILLE'] is not None:
         t_enel.fromfunction('DELTAL', fDL, 'ICOP', { 'pascop' : self['PAS_ENTAILLE'] })
      else:
         l_ep_copeaux_tranche = l_ep_copeaux_tot[i*nbcop:(i+1)*nbcop]
         t_enel['DELTAL'] = l_ep_copeaux_tranche*len(l_numord)

      # 2.2. ----- Calcul de Gp fonction de Ener.Totale et de deltaL
      if is_2D:
         t_enel.fromfunction('GP', fGp_Etot, ('TOTALE', 'DELTAL'),
            {'syme'   : self['SYME_CHAR'] != 'SANS',
             'R'      : self['RAYON_AXIS'],})
      else:
         ep_tranche = largeur_tranche(nom_maillage, l_noeuds_fissure, pas, i)
         #print "ep_tranche %i: "%i, ep_tranche
         t_enel.fromfunction('GP', fGp_Etot, ('TOTALE', 'DELTAL'),
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
         
         if j == 1:
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
      if is_2D:
         dict_constantes['R'] = self['RAYON_AXIS']
      else:
         dict_constantes['R'] = ep_tranche
         
      
      # 3.3. ----- calcul de Kj(G)
      l_tabi = []
      for k, tab in enumerate(l_tab):
         #tab: table de la couronne k
         
         # calcul de Kj(G) = K_i
         new_para = 'K_%d' % (k + 1)
         if is_2D:
            # fusion avec TEMP, E et nu
            tab = merge(tab, t_relev, 'NUME_ORDRE')
            tab.fromfunction(new_para, fKj, ('G', 'YOUNG', 'NU'),
                           { 'R' : self['RAYON_AXIS'] })
            # renomme G en G_i
            tab.Renomme('G', 'G_%d' % (k + 1))
         else:
            if self['RESU_THER']:
               tab=merge(tab, t_relev, ['NUME_ORDRE', 'NOEUD'])
            else:
               tab=mergeLineInTable(tab, t_relev, nb_noeuds_fissure)

            # en 3D, le paramètre R n'intervient pas
            tab.fromfunction(new_para, fKj, ('G_LOCAL', 'YOUNG', 'NU'))
            tab.Renomme('G_LOCAL', 'G_%d' % (k + 1))

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
         if not (min(valkmoy) <= KJ_CRIT <= max(valkmoy)):
#                               'constant utilisé).')
            UTMESS('A','RUPTURE0_1')
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
            # temps en fonction de Kmoy
            finv = t_fonction(valkmoy, tabres.INST.values(), d_para)

            # valeurs à mettre dans la table
            # temps correspondant à KJ_CRIT
            ti   = finv(KJ_CRIT)
            # GP correspondant au temps critique
            Gpi  = fGp(ti)
            d_ident = {
               'KJ_CRIT'   : KJ_CRIT,
               'INST'      : ti,
               'GPMAX'     : Gpi,
               'KGPMAX'    : fKj(Gpi, **dict_constantes),
               'DELTALMAX' : fdL(ti),
            }
            lv_ident.append(d_ident)
         else:
            
            l_i_noeuds_sommets = range(0,len(l_noeuds_fissure),pas)
            t_noeud_Kcrit = Table(para=tabres.para)
            
            # On determine quel noeud sommet maximise KMOY au cours du temps:
            
            row_KMOY_max = tabres.KMOY.MAXI()
            noeud_KMOY_max = row_KMOY_max.NOEUD.values()[0]
            
            # avec le noeud ou KJ_CRIT est atteint, on regarde GP a gauche et a droite. 
            # le GP le plus grand correspond au GPmax
            # on garde la tranche ou GP est le plus grand            
            
            id_noeud_KMOY_max = list(l_noeuds_fissure).index(noeud_KMOY_max)
            if id_noeud_KMOY_max==0:
               # "Gpmax sur la 1ere tranche"
               nume_tranche_Gpmax = 1
            elif id_noeud_KMOY_max==(len(l_noeuds_fissure)-1):
               # "Gpmax sur la derniere tranche"
               nume_tranche_Gpmax = nb_tranches
            else:
               # "Gpmax sur une tranche intermediaire"
               Gpi_tot = Table(para=tb_Gpmax.para)
               Gpi_gauche = tb_Gpmax.NUME_TRANCHE==(id_noeud_KMOY_max/pas)
               Gpi_tot.append(Gpi_gauche.rows[0])
               Gpi_droite = tb_Gpmax.NUME_TRANCHE==(id_noeud_KMOY_max/pas+1)
               Gpi_tot.append(Gpi_droite.rows[0])
               Gpi_tab = Gpi_tot.GPMAX.MAXI()
               nume_tranche_Gpmax = Gpi_tab.NUME_TRANCHE.values()[0]
            
            # tb_Gpmax_TrancheCrit est une table de la meme nature que la table 2D tb_Gpmax
            # i.e. valeurs sur la seule tranche qui nous interesse (ou on sait
            # que KJ_CRIT sera atteint)
            
            tb_Gpmax_TrancheCrit = tb_Gpmax.NUME_TRANCHE==nume_tranche_Gpmax
            
            # avec le noeud ou KJ_CRIT est atteint, on determine le temps
            # critique par interpolation            
            tabres_NoeudCrit = tabres.NOEUD==noeud_KMOY_max
            
            # la suite est idem 2D, seuls les noms des tables changent
    
            # définition des fonctions pour faire les interpolations
            d_para.update({ 'NOM_RESU' : 'DELTALMAX' })
            # DeltaMax en fonction du temps
            fdL = t_fonction(tb_Gpmax_TrancheCrit.INST.values(),
                              tb_Gpmax_TrancheCrit.DELTALMAX.values(),
                              d_para)
            
            # Gpmax fonction du temps
            d_para.update({ 'NOM_RESU' : 'GPMAX' })
            fGp = t_fonction(tb_Gpmax_TrancheCrit.INST.values(),
                              tb_Gpmax_TrancheCrit.GPMAX.values(),
                              d_para)
            
    
            d_para.update({ 'NOM_PARA' : 'KMOY',
                           'NOM_RESU' : 'INST', })
            valkmoy = tabres_NoeudCrit.KMOY.values()
            # temps en fonction de Kmoy
            finv = t_fonction(valkmoy, tabres_NoeudCrit.INST.values(), d_para)
   
            # valeurs à mettre dans la table
            # temps correspondant a KJ_CRIT
            ti   = finv(KJ_CRIT)
            # GP correspondant au temps critique
            Gpi  = fGp(ti)
            # par rapport a 2D, on ajoute 'NUME_TRANCHE'
            d_ident = {
               'KJ_CRIT'      : KJ_CRIT,
               'INST'         : ti,
               'NUME_TRANCHE' : int(nume_tranche_Gpmax),
               'GPMAX'        : Gpi,
               'KGPMAX'       : fKj(Gpi, **dict_constantes),
               'DELTALMAX'    : fdL(ti),
            }
            lv_ident.append(d_ident)
            
      # 3.6.2. --- prédiction
      else:
         pass
   
   # 4. ----- Construction de la table résultat si demandée
   # 4.1. --- identification
   if identification:
      if is_2D:
         para_tab_ident=('KJ_CRIT', 'INST', 'GPMAX', 'KGPMAX', 'DELTALMAX')
      else:
         para_tab_ident=('KJ_CRIT', 'INST', 'NUME_TRANCHE', 'GPMAX', 'KGPMAX', 'DELTALMAX')
      tab_ident = Table(rows=lv_ident,
                        para=para_tab_ident,
                        typ = ('R')*len(para_tab_ident),
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



# -----------------------------------------------------------------------------
def CallRCVALE(TEMP, para, MATER):
   """Fonction appelant RCVALE et retourne la valeur d'un paramètre.
   """
   valres, flag_ok = MATER.RCVALE('ELAS', 'TEMP', TEMP, para)
   assert list(flag_ok).count('OK') != 1, \
         'Erreur lors de la récupération des valeurs du matériau.'
   return valres

# -----------------------------------------------------------------------------
def fKj(G, YOUNG, NU, R=1):
   """Calcul de Kj à partir de G (formule d'Irwin)
      R n'intervient pas en 3D
   """
   Kj=(abs(G / R * YOUNG / (1.0 - NU**2)))**0.5
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
   l_noeuds_fissure = aster.getvectjev(fondfiss.nom.ljust(8)+'.FOND      .NOEU        ')
   type_mailles = aster.getvectjev(fondfiss.nom.ljust(8)+'.FOND      .TYPE        ')
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
