# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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

# person_in_charge: charles.bodel at edf.fr

from numpy import array, zeros, conjugate, identity
from numpy import transpose, ones, arctan, pi, log

from Tkinter import Frame, Menubutton, Checkbutton, Menu, StringVar, IntVar
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Listbox

import tkFont

from Accas import _F
from Cata.cata import OBSERVATION, DETRUIRE, CO, IMPR_RESU
from Calc_essai.cata_ce import Resultat, ModeMeca, InterSpectre, CreaTable
from Calc_essai.cata_ce import nume_ddl_phy, nume_ddl_gene, CreaTable
from Calc_essai.ce_calcul_identification import CalcEssaiIdentification
from Calc_essai.outils_ihm import Compteur, MyMenu, MultiList, VisuSpectre
from Calc_essai.outils_ihm import ChgtRepereDialogue,ObservationWindow
import aster_core


########################
#                      #
#  CLASSES GRAPHIQUES  #
#                      #
########################




class InterfaceIdentification(Frame):

    """
    Classe qui fabrique l'interface graphique permettant de réaliser une identification
    d'efforts turbulents, et de controler les calculs. On y trouve les méthodes suivantes :
     - choix_donnees, choix_projection, frame_meth, _choix_methode, visu_resu : définition
       frame d'interface graphique,
     - get_list, plot_curve : résultats à visualiser, affichage des courbes,
     - calculate_force : dirige les calculs (effectués dans la classe CalculInverse)
     - crea_champ, proj_champ : utilitaires utilisant les op aster du meme nom
    """

    
    def __init__(self, root, ce_objects, mess, out, param_visu):
        Frame.__init__(self, root, borderwidth=4)

        # Classe de calculs appelable ici, ou dans ce_test.py pour validation
        self.calcturb = CalcEssaiIdentification(ce_objects, mess)

        # Objets aster en memoire, et initialisation des noms generiques donnes
        self.objects = ce_objects                                                  # concepts aster dans le dictionnaire
        self.mess = mess                                                           # fichier de message en bs de la fenetre
        self.out = out                                                             # concepts sortants pre-declares a l'appel de la macro
        self.param_visu = param_visu                                               # parametre pour l'affichage des courbes

        self.opt_noms = []                                                         # ['Depl Phy', 'Eff Phy', Eff Mod'...]
        self.opt_data = {}                                                         # {'Depl Phy':{'function':self.calcul_depl_phy,'resultat_attr':'Syy','nume':'nume_phy'},'Eff Phy':{...},...}                            
        self._create_opt_data()

        self.font1 = tkFont.Font( family="Helvetica", size=16 )
        self.font2 = tkFont.Font( family="Helvetica", size=14 )

        # Initialisation des inter-spectres calcules (taille nulle a priori)
        nb_freq = nb_mod = nb_mes = nb_act = 0
        # nb_freq = nombre de frequences de discretisations
        # nb_mes = nombre de mesures
        # nb_act = nombre d'actionneurs
        # nb_mod : nombre de modes pour le modele modal
        self.Syy = zeros((nb_freq, nb_mes, nb_mes))
        self.Syy_R = zeros((nb_freq, nb_mes, nb_mes))
        self.Sqq = zeros((nb_freq, nb_mod, nb_mod))
        self.SQQ = zeros((nb_freq, nb_mod, nb_mod))
        self.SQQ_R = zeros((nb_freq, nb_mod, nb_mod))
        self.Sff = zeros((nb_freq, nb_act, nb_act))
        self.Syy_S = zeros((nb_freq, nb_mes, nb_mes))
       
        self.alpha = StringVar()                                                   # parametre de regularisation de Thikonov
        self.epsilon = StringVar()                                                 # regularisation par svd tronquee
        self.mcoeff = StringVar()                                                  # modulation de alpha selon la frequence

        self.chgt_rep = ChgtRepereDialogue(mess)
        self.obs_co = None                                                         # observabilite (base de modes projetee sur les capteurs
        self.com_co = None                                                         # commandabilite (bdm projetee sur les actionneurs)

        self.inter_spec = None

        self.base = None                                                           # base de mode dont on extrait les caracteristiques modales

        lab = Label(self,text="Identification de chargement",
                  pady=5, font=self.font1 )
        lab.grid(row=0, columnspan = 8)

        colonne_1 = self._construit_colonne_1()
        colonne_2 = self._construit_colonne_2()
        
        colonne_1.grid(row=1, column=0, rowspan=1, sticky='ew')
        colonne_2.grid(row=1, column=1, rowspan=1, sticky='new')
        self.columnconfigure(0, weight=1)
        self.columnconfigure(1, weight=2)

    def setup(self):
        "Utilisé par outils_ihm.TabbedWindow"
        mdo = self.objects
        self.menu_resu_fonc.update(mdo.get_inter_spec_name(),
                                   self.var_resu_fonc,self._get_inter_spec)
        self.menu_resu_mod.update(mdo.get_mode_meca_name(),
                                  self.var_resu_mod,self._get_base)
        

    def _create_opt_data(self):
        opt_res_definitions = [
            ("Depl phy", self.calcul_depl_phy, 'Syy', 'nume_phy'),
            ("Eff mod", self.calcul_eff_mod, 'SQQ', 'nume_gene'),
            ("Depl phy r", self.calcul_depl_phy, 'Syy_R', 'nume_phy'),
            ("Eff mod r", self.calcul_eff_mod, 'SQQ_R', 'nume_gene'),
            ("Eff phy", self.calcul_eff_phy, 'Sff', 'nume_phy'),
        
            ("Depl synt", self.calcul_depl_phy, 'Syy_S', 'nume_phy'),
            ("Valeurs sing", self.calcul_valeurs, 'val_sing', 'nume_gene'),
            ("regul", self.calcul_valeurs, 'regul', None),
            ]
        for nom, function, res_attr, num in opt_res_definitions:
            self.opt_data[nom] = {"function" : function,
                                  "resultat_attr" : res_attr,
                                  "nume" : num}
            self.opt_noms.append(nom)
        
    def _construit_colonne_1(self):
        col = Frame(self, relief = 'sunken',borderwidth=1)
        # Menu de choix des donnes de calcul en entree
        Label(col, text=u"Choix des données de calcul",
              font=self.font2).grid(row=0, padx=50, pady=2)

        box_cd = self._choix_base_modale(col,relief='flat') 
        box_obs = self._definit_observabilite(col,self,relief='flat')
        box_cmd = self._definit_commandabilite(col,self,relief='flat')
        box_int = self._choix_interspectre(col,relief='flat')
        box_cm = self._choix_methode(col,relief='flat')
        
        for idx, box in enumerate([box_cd, box_obs, box_cmd, box_int, box_cm]):
            box.grid(row=idx + 1, sticky='ew', padx=4, pady=2) 
        Button(col, text="Calculer",
               command=self.calculs).grid(row=6, sticky='s'+'e',
                                          padx=4, pady=2)

        return col

    def _choix_base_modale(self,parent,**args):
        """Choix des données d'entrée"""
        fra = Frame(parent,args)

        # Menu choix de la base modale
        Label(fra, text="Base modale").grid(row=1, column=0, sticky='w')
        
        options = self.objects.get_mode_meca_name()
        self.var_resu_mod = StringVar()
        self.menu_resu_mod = MyMenu(fra, options, self.var_resu_mod,
                                    self._get_base)
        self.menu_resu_mod.grid(row=1, column=1)

        return fra
   
    def _definit_observabilite(self,parent,root,**args):
        """Définition du concept d'observabilité."""

        self.observabilite = ObservationWindow(parent,root,self.mess,self.objects,
                                               None,u"'observabilité",0,**args)
        return self.observabilite


    def _definit_commandabilite(self,parent,root,**args):
        """Définition du concept de commandabilité."""
        self.commandabilite = ObservationWindow(parent,root,self.mess,self.objects,
                                                None,u"e commandabilité",0,**args)
        return self.commandabilite 


    def _choix_interspectre(self, root,**args):
        """ Choix de l'interspectre"""

        self.var_resu_fonc = StringVar() # le nom de l'interspectre
        self.typ_resu_fonc = StringVar() # le type de 'interspectre
        fra = Frame(root, args)
        desc = "Interspectre en fonctionnement"
        Label(fra, text=desc).grid(row=1, column=0, sticky='w')
       
        options = self.objects.get_inter_spec_name()
        self.menu_resu_fonc = MyMenu(fra, options, self.var_resu_fonc,
                                     self._get_inter_spec)
        self.menu_resu_fonc.grid(row=1, column=1)
        Label(fra, text = "Type champ",).grid(row=1,column=2)
        opt_cham = ['DEPL','VITE','ACCE'] 
        typ_cham = MyMenu(fra,opt_cham,self.typ_resu_fonc)
        self.typ_resu_fonc.set('DEPL')
        typ_cham.grid(row=1, column=3, sticky='e')
        
        return fra

    def _choix_methode(self, root,**args):
        """Choix de la méthode de résolution (techniques de régularisation)"""
        fra = Frame(root,args )

        # Menu de choix de la methode de calcul des efforts
        Label(fra, text="Définition du calcul",
              font=self.font2).grid(row=0, column=0, columnspan=3)
               
        Label(fra, text="Tikhonov")
        Label(fra, text="Alpha =").grid(row=1,column=1)
        entree1 = Entry(fra, textvariable=self.alpha )
        entree1.grid(row=1,column=2)
        self.alpha.set(0.0)
                
        Label(fra, text="SVD tronquée").grid(row=2, column=0)
        Label(fra, text="Eps =").grid(row=2, column=1)
        entree2 = Entry(fra, textvariable=self.epsilon)
        entree2.grid(row=2, column=2)
        self.epsilon.set(0.0)

        Label(fra, text="puissance").grid(row=3, column=0)
        Label(fra, text="m =").grid(row=3, column=1)
        entree2 = Entry(fra, textvariable=self.mcoeff)
        entree2.grid(row=3, column=2)
        self.mcoeff.set(2.0)
        
        return fra

    def _construit_colonne_2(self):
        """ Affichage dans une fenetre des voies à afficher, possibilité
        de visualiser les autospectres et les interspectres"""
        # variables pour la visu
        self.nb_col_visu = 2
        self.var_visu_resu = [StringVar() for kk in range(self.nb_col_visu)]#,StringVar()]#*self.nb_col_visu
        self.var_export = [StringVar()]*self.nb_col_visu
        self.label_visu = [StringVar()]*self.nb_col_visu                    # variable pour le label de la colonne de visu
        self.curve_list = [None]*self.nb_col_visu
        self.radio_donnees = IntVar()                                       # visu reel, abs, imag, phase
        self.xlinlog = IntVar()                                             # axe x lin ou log
        self.ylinlog = IntVar()                                             # axe y lin ou log
        
        
        fra = VisuSpectre(self,self.nb_col_visu,choix=self.opt_noms,export='oui',
                          label_visu=self.label_visu,
                          relief='sunken',borderwidth=1)

        for label in self.label_visu:
            label.set(u"Noeuds/numéros d'ordre")
        
        return fra



    def _get_base(self):
        """ Va chercher l'instance de la classe ModeMeca correspondant
        au nom de base choisi
        """
        nom_base = self.var_resu_mod.get()
        self.base = self.objects.resultats[nom_base] 


    def _get_inter_spec(self):
        """ Va chercher l'instance de la classe InteSpectre correspondant
        au nom de l'inter-spectre choisi
        """
        nom_intsp = self.var_resu_fonc.get()
        self.inter_spec = self.objects.inter_spec[nom_intsp]
        
    def check_data(self):
        """verification des donnees d'entree"""
        # TODO : on pourrait ajouter ici la verification sur les
        # dimensions des matrices
        self.objects.recup_objects()
        if self.var_resu_fonc.get() == 'Choisir':
            self.mess.disp_mess("Il faut choisir la base modale")
            return 0
        if not self.observabilite.obs_co:
            self.mess.disp_mess(u"Il faut définir le concept d'observabilité")
            return 0
        self.obs_co = self.observabilite.obs_co
        if not self.commandabilite.obs_co:
            self.mess.disp_mess(u"Il faut définir le concept de commandabilité")
            return 0
        self.com_co = self.commandabilite.obs_co
        if self.var_resu_fonc.get() == 'Choisir':
            self.mess.disp_mess("Il faut choisir l'inter-spectre des mesures")
            return 0

        return 1
        

    def calculs(self):
        """!Lance la classe CalculAster, qui dirige toutes les routines
            Aster et python
        """

        iret = self.check_data()
        if iret == 0:
            return
      
        self.calcturb.set_base(self.base)
        self.calcturb.set_observabilite(self.observabilite.obs_co)
        self.calcturb.set_commandabilite(self.commandabilite.obs_co)
        self.calcturb.set_interspectre(self.inter_spec)
        self.calcturb.set_type_intsp(self.typ_resu_fonc.get())
        self.calcturb.set_alpha(self.alpha.get())
        self.calcturb.set_epsilon(self.epsilon.get())
        self.calcturb.set_mcoeff(self.mcoeff.get())
        
        # Lancement des calculs
        self.calcturb.calculate_force()

        
        # Rafraichissement des donnees dans la classe InterfaceTurbulent
        self.Syy = self.Syy_R = self.Sqq = self.SQQ = None
        self.SQQ_R = self.Sff = self.Syy_S = None
        # self.calcturb.is_XX = 1 quand les calculs se sont bien passes
        if self.calcturb.is_Syy == 1:
            self.Syy   = self.calcturb.Syy
        if self.calcturb.is_SQQ == 1:
            self.SQQ   = self.calcturb.SQQ
            self.val_sing = self.calcturb.val_sing
            self.regul = self.calcturb.regul
        if self.calcturb.is_SQQ_R == 1:
            self.SQQ_R = self.calcturb.SQQ_R
        if self.calcturb.is_Sff == 1:
            self.Sff   = self.calcturb.Sff
        if self.calcturb.is_Syy_S == 1:
            self.Syy_S = self.calcturb.Syy_S
        if self.calcturb.is_Syy_R ==  1:
            self.Syy_R = self.calcturb.Syy_R

    def get_list(self):
        """Routine qui crée les liste de courbes à afficher.
        Choix de la donnée à représenter:
            Depl phy <=> déplacements physiques,
            Eff mod <=> efforts modaux,
            Depl phy r <=>
            Eff mod r <=> efforts modaux reconstitués,
            Eff phy r <=> efforts physiques
            Depl synt <=> déplacements physiques resynthétisés
            Valeurs  synt <=> valeurs singulieres de la matrice C.phi.Zm1
            regul <=> parametres de regulation"""
        self.var_list = [[],[]]
        mess_err = "!! Impossible de créer la liste des courbes à afficher !!"
        self.mess.disp_mess(" ")
        for ind_tabl in range(2):
            opt_key = self.var_visu_resu[ind_tabl].get()
            if opt_key.split()[0] == "Choisir" :
                continue
            optdict = self.opt_data[opt_key]
            # Récupération de la fonction à appeler
            calc_func = optdict["function"]
            calc_func(optdict["resultat_attr"], ind_tabl)

    def calcul_depl_phy(self, resultat_attr, ind_tabl):
        """Calcul les paramètres: 'Depl phy', 'Depl phy r','Depl synt'."""
        liste = nume_ddl_phy(self.obs_co)
        
        for ind in range(len(liste)):
            lst = crea_list_no(ind, liste)
            for ind in lst:
                self.var_list[ind_tabl].append(ind)
        self.curve_list[ind_tabl].set_values(self.var_list[ind_tabl])

    def calcul_eff_phy(self, resultat_attr, ind_tabl):
        """Calcul les paramètres: 'Eff Phy'."""
        liste = nume_ddl_phy(self.com_co)
        
        for ind in range(len(liste)):
            lst = crea_list_no(ind, liste)
            for ind in lst:
                self.var_list[ind_tabl].append(ind)
        self.curve_list[ind_tabl].set_values(self.var_list[ind_tabl])


    def calcul_eff_mod(self, resultat_attr, ind_tabl):
        """Calcul les paramètres: 'Eff mod', 'Eff mod r'."""
        liste = nume_ddl_gene(self.calcturb.res_base)
        
        for mode in range(len(liste)):
            lst = crea_list_mo(mode, liste)
            for ind in lst:
                self.var_list[ind_tabl].append(ind)
        self.curve_list[ind_tabl].set_values(self.var_list[ind_tabl])
    
    def calcul_valeurs(self, resultat_attr, ind_tabl):
        """Calcul les paramètres: 'Valeurs sing', 'regul'."""
        if not self.calcturb.inter_spec.nume_gene:
            liste = nume_ddl_gene(self.calcturb.res_base)
        for ind in liste:
            self.var_list[ind_tabl].append(ind)
        self.curve_list[ind_tabl].set_values(self.var_list[ind_tabl])

    def _get_selected_variables(self, ind_tab):
        """Retourne les variables selectionées dans une colonne
        affichant les résultats."""
        var_list = []
        liste = self.curve_list[ind_tab].get_selection()
        for idx in liste:
            var_list.append(idx[1])
        return var_list

    def _get_graph_data(self):
        """Retourne les valeurs et les légendes pour les courbes de résultats.
           Remarque importante : ici les fonctions sont extraites de la matrice
           inter-spectrale python alors que dans ce_calc_spec, on fait un RECU_FONCTION
           sur le concept inter-spectre aster.
           TODO : homogénéiser les deux procédures dans une méthiode commune à mettre
           dans la classe VisuSpectre (dans outils_ihm.py)"""
        # values = liste dont chaque elmt est une courbe
        values = []
        captions = []
        
        freq = self.inter_spec.f
        
        for ind_tab in range(2):
            opt_key = self.var_visu_resu[ind_tab].get()
            if opt_key.split()[0] == "Choisir" :
                continue
            
            optdict = self.opt_data[opt_key]
            res = getattr(self, optdict["resultat_attr"])
            num_option = self.opt_noms.index(opt_key)

            for var in self._get_selected_variables(ind_tab):
                vect = None
                # Type de resultat a representer
                if num_option <= 5:
                    num = getattr(res, optdict["nume"])
                    ikey, jkey = var.split(',')
                    iidx = num.index(ikey)
                    jidx = num.index(jkey)
                    vect = getattr(res, 'matr_inte_spec')[:, iidx, jidx]
                elif num_option == 6 or num_option == 7:
                    idx = int(var[2:])
                    vect = res[idx - 1, 1:]

                # Options de visualisation : reel, abs, imag, phase
                if self.radio_donnees.get() == 0:
                    vect = vect.real
                elif self.radio_donnees.get() == 1:
                    vect = abs(vect)
                elif self.radio_donnees.get() == 2:
                    vect = vect.imag
                elif self.radio_donnees.get() == 3:      
                    vect = arctan(vect.imag / vect.real)

                # axes de visu : lin ou log
                # en log y, on ne visualise que la valeur absolue
                if self.ylinlog.get() == 1:
                    self.radio_donnees.set(1)
                    vect = log(abs(vect))/log(10)
                if self.xlinlog.get() == 1:
                    # on supprime le pas de frequence nulle si on visualise en log x
                    vect = vect[1:]
                
                values.append(vect)
                captions.append("%s %s" % (opt_key, var))

            if self.xlinlog.get() == 1 and self.radio_donnees.get() == 'Phase':
                self.mess.disp_mess("Impossible de représenter la phase dans un\n"\
                                    "diagramme logarithmique !")
                return ([],[],[])

        if self.xlinlog.get() == 1:
            freq = log(freq[1:])/log(10.0)
        
        return freq, values, captions

    def display_curve(self):
        """Selection dans les interspectres disponibles des resultats
        sélectionnées dans les listes curve_list, et plot
        des courbes correspondantes"""       
        freq, values, caption = self._get_graph_data()
        if freq == []:
            return
        
        self.param_visu.visu_courbe(freq, values,
                                    couleur=None,
                                    titre='Inter-spectres',
                                    l_legende=caption,
                                    legende_x='Frequence',
                                    legende_y='Amplitude',
                                    unite_x='Hz',
                                    unite_y='u^2/Hz')
   
    def export_inte_spec1(self):
        option = self.var_visu_resu[0].get()
        titre = self.var_export[0].get()
        self.export_inte_spec(option, titre)

    def export_inte_spec2(self):
        option = self.var_visu_resu[1].get()
        titre = self.var_export[1].get()
        self.export_inte_spec(option, titre)
        
    def export_inte_spec(self, option, titre):
        out = self.out
        if option == self.opt_noms[0]:
            self.Syy.make_inte_spec(titre, out)
        elif option == self.opt_noms[1]:
            self.SQQ.make_inte_spec(titre, out)
        elif option == self.opt_noms[2]:
            self.Syy_R.make_inte_spec(titre, out)
        elif option == self.opt_noms[3]:
            self.SQQ_R.make_inte_spec(titre, out)
        elif option == self.opt_noms[4]:
            self.Sff.make_inte_spec(titre, out)
        elif option == self.opt_noms[5]:
            self.Syy_S.make_inte_spec(titre, out)
        else:
            self.mess.disp_mess("!! Il n'est pas possible " \
                                "d'exporter ces données !!")
            self.mess.disp_mess("  ")
    

    def teardown(self):
        "Utilisé par outils_ihm.TabbedWindow"
        pass


#------------------------------------------------------------------------------

########################
#                      #
#  PETITS UTILITAIRES  #
#                      #
########################

def crea_list_mo(x, list_in):
    liste = []
    for ind in range(x+1):
        liste.append(list_in[x]+','+list_in[ind])
    return liste

def crea_list_no(x,list_in):
    """
    Creation d'une liste de resultats visualisables sous la forme :
     < No #noeud ddl, No #noeud ddl>
    Liste est le resultat de crea_champ
    """
    list_out = []
    for ind in range(x+1):
        list_out.append(list_in[x]+','+list_in[ind])
    return list_out


def get_filtres(grp_no, grp_ma):
    filtres = []
    for grp in grp_no:
        filtres.append(_F(GROUP_NO=grp["NOM"],
                          DDL_ACTIF=grp["NOM_CMP"],
                          NOM_CHAM='DEPL'))
    for grp in grp_ma:
        filtres.append(_F(GROUP_MA=grp["NOM"],
                          DDL_ACTIF=grp["NOM_CMP"],
                          NOM_CHAM='DEPL'))
    return filtres

def get_chgt_repere(grp_no, grp_ma):
    chgt_reps = []
    for grp in grp_no:
        if grp["CHGT_REP"]:
            chgt_reps.append(_F(GROUP_NO=grp["NOM"],
                                **grp["CHGT_REP"]))
    for grp in grp_ma:
        if grp["CHGT_REP"]:
            chgt_reps.append(_F(GROUP_MA=grp["NOM"],
                                **grp["CHGT_REP"]))
    return chgt_reps
