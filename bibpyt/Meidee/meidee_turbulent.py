#@ MODIF meidee_turbulent Meidee  DATE 14/05/2008   AUTEUR BODEL C.BODEL 
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

from Numeric import array, zeros, conjugate, identity
from Numeric import transpose, ones, arctan, pi, log

from Tkinter import Frame, Menubutton, Checkbutton, Menu, StringVar, IntVar
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Listbox


from Accas import _F
from Cata.cata import OBSERVATION, DETRUIRE, CO, IMPR_RESU
from Meidee.meidee_cata import Resultat, InterSpectre, CreaTable
from Meidee.meidee_cata import nume_ddl_phy, nume_ddl_gene, CreaTable
from Meidee.meidee_iface import Compteur, MyMenu
from Meidee.meidee_iface import MultiList, XmgrManager
from Meidee.meidee_calcul_turbulent import CalculTurbulent
from Meidee.modes import SelectionNoeuds, SelectionMailles, sort_compo_key
from Meidee.modes import ChgtRepereDialogue


########################
#                      #
#  CLASSES GRAPHIQUES  #
#                      #
########################




class InterfaceTurbulent(Frame):

    """
    Classe qui fabrique l'interface graphique permettant de réaliser une identification
    d'efforts turbulents, et de controler les calculs. On y trouve les méthodes suivantes :
     - choix_donnees, choix_projection, frame_meth, _choix_methode, visu_resu : définition
       frame d'interface graphique,
     - get_list, plot_curve : résultats à visualiser, affichage des courbes,
     - calculate_force : dirige les calculs (effectués dans la classe CalculInverse)
     - crea_champ, proj_champ : utilitaires utilisant les op aster du meme nom
    """

    
    def __init__(self, root, aster_objects, mess, out):
        Frame.__init__(self, root, relief='raised', borderwidth=4)

        # Classe de calculs
        self.calcturb = CalculTurbulent(aster_objects, mess)

        # Objets aster en memoire, et initialisation des noms generiques donnes
        self.objects = aster_objects
        self.mess = mess
        self.out = out

        self.opt_data = {}
        self.opt_noms = []
        self._create_opt_data()
        
        # Parametres de calcul et de visualisation
        nb_freq = nb_mod = nb_mes = nb_act = 0
        self.Syy = zeros((nb_freq, nb_mes, nb_mes))
        self.Syy_R = zeros((nb_freq, nb_mes, nb_mes))
        self.Sqq = zeros((nb_freq, nb_mod, nb_mod))
        self.SQQ = zeros((nb_freq, nb_mod, nb_mod))
        self.SQQ_R = zeros((nb_freq, nb_mod, nb_mod))
        self.Sff = zeros((nb_freq, nb_act, nb_act))
        
        self.alpha = StringVar()
        self.epsilon = StringVar()
        self.mcoeff = StringVar()

        self.chgt_rep = ChgtRepereDialogue(mess)
        self.obs_co = Resultat(self.objects,"__OBS",CO("__OBS"))
        self.obs_extraction_ddls = []
        self.com_co = Resultat(self.objects,"__COM",CO("__COM"))
        self.com_extraction_ddls = []

        self.inter_spec = None

        self.base = None

        lab = Label(self,text="Identification d'un chargement aléatoire",
                  pady=5, font=("Helvetica", "16", "bold") )
        lab.grid(row=0, columnspan = 8)

        colonne_1 = self._construit_colonne_1()
        colonne_2 = Frame(self)
        box_vr = self.visu_resu(colonne_2)
        box_vr.grid(row=1, sticky='w'+'e'+'n'+'s')
        
        colonne_1.grid(row=1, column=0, rowspan=3)
        colonne_2.grid(row=1, column=1, rowspan=1, sticky='n'+'s')
        self.columnconfigure(0, weight=1)
        self.columnconfigure(1, weight=1)

        self.xmgr_manager = XmgrManager()

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
                                  "numero" : num}
            self.opt_noms.append(nom)
        
    def _construit_colonne_1(self):
        col = Frame(self, relief="groove", borderwidth=4)
        # Menu de choix des donnes de calcul en entree
        Label(col, text="Choix des données de calcul",
              font=("Arial", "16")).grid(row=0, padx=4, pady=2)
        
        box_cd = self._choix_base_modale(col) 
        
        box_obs = self._definit_observabilite(col)
        
        box_cmd = self._definit_commandabilite(col)

        box_int = self._choix_interspectre(col)
    
        box_cm = self._choix_methode(col)
        
        for idx, box in enumerate([box_cd, box_obs, box_cmd, box_int, box_cm]):
            box.grid(row=idx + 1, sticky='w'+'e'+'n', padx=4, pady=2) 

        Button(col, text="Calculer",
               command=self.calculs).grid(row=6, sticky='s'+'e',
                                          padx=4, pady=2)

        return col

    def _choix_base_modale(self, root):
        """Choix des données d'entrée"""
        fra = Frame(root, relief='ridge', borderwidth=4)

        # Menu choix de la base modale
        Label(fra, text="Base modale").grid(row=1, column=0, sticky='w')
        
        options = self.objects.get_resultats_num()
        self.var_resu_mod = StringVar()
        menu_resu_mod = MyMenu(fra, options, self.var_resu_mod,
                               self._get_base)
        menu_resu_mod.grid(row=1, column=1)

        return fra
   
    def _definit_observabilite(self, root):
        """Définition du concept d'observabilité."""
        fra = Frame(root, relief='ridge', borderwidth=4)

        Label(fra,
              text="Définition du concept d'observabilité",
              font=("Arial","16")).grid(row=0, column=0, columnspan=3)   
        
        # Menu choix du modele experimental associé
        # aux donnees en fonctionnement
        Label(fra, text="Modèle expérimental").grid(row=1,column=0, sticky='ew')
        self.nom_obs_modele = StringVar()
        menu_obs_modele = MyMenu(fra,
                                 self.objects.get_model_name(),
                                 self.nom_obs_modele,
                                 self._observabilite_changed)
        menu_obs_modele.grid(row=2, column=0, sticky='we')

        Label(fra, text="Base de déformées").grid(row=1,column=1, sticky='ew')
        self.nom_obs_resu = StringVar()
        menu_obs_resu = MyMenu(fra,
                               self.objects.get_resultats(),
                               self.nom_obs_resu,
                               self._observabilite_changed)
        menu_obs_resu.grid(row=2, column=1, sticky='we')
        
        no_title = "Groupe de noeuds et DDL des capteurs"
        self.obs_noeuds = SelectionNoeuds(fra, no_title, bg='#90a090',
                                          chgt_rep=self.chgt_rep)
        self.obs_noeuds.grid(row=3, column=0, sticky='w'+'e',
                             columnspan=3, pady=2, padx=2)
        
        ma_title = "Groupe de mailles et DDL des capteurs"
        self.obs_mailles = SelectionMailles(fra, ma_title, bg='#9090a0',
                                            chgt_rep=self.chgt_rep)
        self.obs_mailles.grid(row=4, column=0, sticky='w'+'e',
                              columnspan=3, pady=2, padx=2)
        
        but = Button(fra, text="Valider",
                     command=self._calculate_observabilite)
        but.grid(row=5, column=3, sticky='e', padx=2, pady=2)
        
        fra.columnconfigure(0, weight=1)
        fra.columnconfigure(1, weight=1)

        return fra

    def _definit_commandabilite(self, root):
        """Définition du concept de commandabilité."""
        fra = Frame(root, relief='ridge', borderwidth=4)
        
        Label(fra, text="Définition du concept de commandabilité",
              font=("Arial","16")).grid(row=0, column=0, columnspan=3)
        
        Label(fra, text="Modèle de controlabilite"
              ).grid(row=1, column=0, sticky='ew')
        self.nom_com_modele = StringVar()
        menu_com_modele = MyMenu(fra,
                                 self.objects.get_model_name(),
                                 self.nom_com_modele,
                                 self._commandabilite_changed)
        menu_com_modele.grid(row=2, column=0, sticky='ew')

        Label(fra, text="Base de déformées").grid(row=1,column=1, sticky='ew')
        self.nom_com_resu = StringVar()
        menu_com_resu = MyMenu(fra,
                               self.objects.get_resultats(),
                               self.nom_com_resu,
                               self._commandabilite_changed)
        menu_com_resu.grid(row=2, column=1, sticky='we')
        
        no_title = "Groupe de noeuds et DDL des capteurs"
        self.com_noeuds = SelectionNoeuds(fra, no_title, bg='#90a090',
                                          chgt_rep=self.chgt_rep)
        self.com_noeuds.grid(row=3, column=0, sticky='w'+'e',
                             columnspan=2, pady=2, padx=2)
        
        ma_title = "Groupe de mailles et DDL des capteurs"
        self.com_mailles = SelectionMailles(fra, ma_title, bg='#9090a0',
                                            chgt_rep=self.chgt_rep)
        self.com_mailles.grid(row=4, column=0, sticky='w'+'e',
                              columnspan=2, pady=2, padx=2)
        
        but = Button(fra, text="Valider",
                     command=self._calculate_commandabilite)
        but.grid(row=5, column=3, sticky='e', padx=2, pady=2)
        
        fra.columnconfigure(0, weight=1)
        fra.columnconfigure(1, weight=1)

        return fra

    def _choix_interspectre(self, root):
        fra = Frame(root, relief='ridge', borderwidth=4)
        desc = "Interspectre en fonctionnement"
        Label(fra, text=desc).grid(row=1, column=0, sticky='w')
       
        options = self.objects.get_inter_spec_name()
        self.var_resu_fonc = StringVar()
        menu_resu_fonc = MyMenu(fra, options, self.var_resu_fonc,
                                self._get_inter_spec)
        menu_resu_fonc.grid(row=1, column=1)
        
        return fra

    def _choix_methode(self, root):
        """Choix de la méthode de résolution (techniques de régularisation)"""
        fra = Frame(root, relief='ridge', borderwidth=4 )

        # Menu de choix de la methode de calcul des efforts
        Label(fra, text="Définition du calcul",
              font=("Times","16")).grid(row=0, column=0, columnspan=3)
               
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

    def visu_resu(self, root):
        """ Affichage dans une fenetre des voies à afficher, possibilité
        de visualiser les autospectres et les interspectres"""
        fra = Frame(root, relief='ridge', borderwidth=4)
        self.curve_color = StringVar()
        self.var_abs = StringVar()
        self.var_ord = StringVar()
        self.var_amp = StringVar()
        self.born_freq = StringVar()

        Label(fra, text="Visualisation des résultats",
              font=("Arial","16")).grid(row=0, column=0,columnspan=3)
 
        Label(fra, text="Choix des données à visualiser").grid(row=1,
                                                               column=0,
                                                               columnspan=3)

        self.var_visu_resu = [StringVar(), StringVar()]
        self.var_export = [StringVar(), StringVar()]
        self.menu_visu_resu = [None, None]
        self.menu_list = [None, None]

        for ind_tab in range(2):
            self.menu_visu_resu[ind_tab] = MyMenu(fra, self.opt_noms,
                                                  self.var_visu_resu[ind_tab])
            self.menu_visu_resu[ind_tab].grid(row=2, column=ind_tab,
                                              columnspan=5, sticky='w')
            self.menu_list[ind_tab] = MultiList(fra, ["indice mode/position"])
            self.menu_list[ind_tab].grid(row=3,
                                         column=ind_tab, sticky='w'+'e'+'n'+'s')
            Entry(fra, textvariable=self.var_export[ind_tab]).grid(row=4,
                                                                 column=ind_tab,
                                                                 sticky='w'+'e')
            fonc = "export_inte_spec" + str(ind_tab + 1)
            Button(fra, text="Exporter Inter-spectre",
                        command=getattr(self, fonc)).grid(row=5, column=ind_tab)
        Button(fra, text="Valider", command=self.get_list).grid(row=2, column=2)

        # Options de visualisation (échelles)
        opt_box1 = Frame(fra)
        Label(opt_box1,
             text="Echelle Absisses").grid(row=0, column=0, sticky='w')
        Label(opt_box1,
              text="Echelle Ordonnées").grid(row=1, column=0, sticky='w')
        Label(opt_box1, text="Amp/Phase").grid(row=2, column=0, sticky='w')
        self.amp_phas = ["Amplitude","Phase"]
        opt_trac = ["LIN","LOG"]
        self.menu_abs = MyMenu(opt_box1, opt_trac, self.var_abs)
        self.menu_abs.grid(row=0, column=1, columnspan=5, sticky='e')
        self.menu_ord = MyMenu(opt_box1, opt_trac, self.var_ord)
        self.menu_ord.grid(row=1, column=1, columnspan=5, sticky='e')
        self.menu_amp = MyMenu(opt_box1, self.amp_phas, self.var_amp)
        self.menu_amp.grid(row=2, column=1, columnspan=5, sticky='e')
        
        self.var_amp.set("Amplitude")
        self.var_abs.set("LIN")
        self.var_ord.set("LIN")
        opt_box1.grid(row=6, column=0, columnspan=2, sticky = 'e''w')
        
        opt_box2 = Frame(fra)
        Button(opt_box2, text="Afficher courbe",
               command=self.plot_curve).grid(row=0, column=0)
        Button(opt_box2, text="Critère d'erreur",
               command=self.plot_curve).grid(row=1, column=0)
        Entry(opt_box2, textvariable=self.born_freq).grid(row=2, column=0)
        opt_box2.grid(row=3, column=2, sticky = 'e''w')

        return fra
    
    def _observabilite_changed(self):
        nom_modele = self.nom_obs_modele.get()
        modele = self.objects.get_model(nom_modele)
        
        self.obs_noeuds.set_resultat(modele)
        self.obs_mailles.set_resultat(modele)

    def _commandabilite_changed(self):
        nom_modele = self.nom_com_modele.get()
        modele = self.objects.get_model(nom_modele)
        
        self.com_noeuds.set_resultat(modele)
        self.com_mailles.set_resultat(modele)


    def _get_base(self):
        """ Va chercher l'instance de la classe Resultat correspondant
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
        
    
    def _calculate_observabilite(self):
        if self.obs_co:
            DETRUIRE(CONCEPT=_F(NOM=self.obs_co.obj), ALARME='NON',INFO=1)
        
        nom_resu = self.nom_obs_resu.get()
        if nom_resu == 'Choisir':
            self.mess.disp_mess("Il faut choisir une base de deformees " \
                                "pour définir l'observabilité.")
            return
        resu = self.objects.get_resu(nom_resu)

        nom_modele = self.nom_obs_modele.get()
        if nom_modele == 'Choisir':
            self.mess.disp_mess("Il faut choisir un concept " \
                                "pour définir l'observabilité.")
            return
        modele = self.objects.get_model(nom_modele)

        grp_no = self.obs_noeuds.get_selected()
        grp_ma = self.obs_mailles.get_selected()
        if not (grp_no or grp_ma):
            self.mess.disp_mess("Il faut sélectioner un groupe de noeuds " \
                                "ou de mailles parmis ceux proposés " \
                                "pour le concept d'observabilité.")
            return

        proj = 'OUI'
        if resu.modele_name == modele.nom:
            proj = 'NON'

        try:
            __OBS = OBSERVATION( RESULTAT = resu.obj,
                                 MODELE_1 = resu.modele.obj,
                                 MODELE_2  = modele.obj,
                                 PROJECTION  = proj,
                                 TOUT_ORDRE  = 'OUI',
                                 NUME_DDL = modele.nume_ddl,
                                 NOM_CHAM = 'DEPL',
                                 FILTRE   = get_filtres(grp_no, grp_ma),
                                 MODI_REPERE = get_chgt_repere(grp_no, grp_ma)
                               );
        except:
            self.mess.disp_mess("Le concept d'observabilité " \
                                "n'a pas pu être calculé.")
            self.mess.disp_mess("L'erreur est affichée en console.")
            raise

        self.mess.disp_mess("Le concept d'observabilité " \
                            "a été calculé.")
        self.obs_co = Resultat(self.objects,__OBS.nom,__OBS,self.mess)
        self.obs_extraction_ddls = get_filtres(grp_no, grp_ma)

    def _calculate_commandabilite(self):
        if self.com_co:
            DETRUIRE(CONCEPT=_F(NOM=self.com_co.obj), ALARME='NON',INFO=1)
        
        nom_resu = self.nom_com_resu.get()
        if nom_resu == 'Choisir':
            self.mess.disp_mess("Il faut choisir une base de deformees " \
                                "pour définir l'observabilité.")
            return
        resu = self.objects.get_resu(nom_resu)

        nom_modele = self.nom_com_modele.get()        
        if nom_modele == 'Choisir':
            self.mess.disp_mess("Il faut choisir un concept " \
                                "pour définir la commandabilité.")
            return
        modele = self.objects.get_model(nom_modele)

        grp_no = self.com_noeuds.get_selected()
        grp_ma = self.com_mailles.get_selected()
        if not (grp_no or grp_ma):
            self.mess.disp_mess("Il faut sélectioner un groupe de noeuds " \
                                "ou de mailles parmis ceux proposés " \
                                "pour le concept de commandabilité.")
            return

        proj = 'OUI'
        if resu.modele_name == modele.nom:
            proj = 'NON'

        try:
            __COM = OBSERVATION(RESULTAT = resu.obj,
                                MODELE_1 = resu.modele.obj,
                                MODELE_2 = modele.obj,
                                PROJECTION = proj,
                                TOUT_ORDRE = 'OUI',
                                NUME_DDL = modele.nume_ddl,
                                NOM_CHAM = 'DEPL',
                                FILTRE   = get_filtres(grp_no, grp_ma),
                                MODI_REPERE = get_chgt_repere(grp_no, grp_ma)
                               )
        except:
            self.mess.disp_mess("Le concept de commandabilité " \
                                "n'a pas pu être calculé.")
            self.mess.disp_mess("L'erreur est affichée en console.")
            raise

        self.mess.disp_mess("Le concept de commandabilité " \
                            "a été calculé.")
        self.com_co = Resultat(self.objects,__COM.nom,__COM,self.mess)
        self.com_extraction_ddls = get_filtres(grp_no , grp_ma)
        

    def calculs(self):
        """!Lance la classe CalculAster, qui dirige toutes les routines
            Aster et python
        """
        if self.var_resu_fonc.get() == 'Choisir':
            self.mess.disp_mess("Il faut choisir la base modale")
            return
        self.calcturb.set_base(self.base)
        
        # Attention à rafraîchir la mémoire de MeideeObjects
        self.objects.recup_objects()
        self.calcturb.set_observabilite(self.obs_co)
        self.calcturb.set_commandabilite(self.com_co)

        if self.var_resu_fonc.get() == 'Choisir':
            self.mess.disp_mess("Il faut choisir l'inter-spectre des mesures")
            return
        self.calcturb.set_interspectre(self.inter_spec)

        self.calcturb.set_alpha(self.alpha.get())
        self.calcturb.set_epsilon(self.epsilon.get())
        self.calcturb.set_mcoeff(self.mcoeff.get())
        
        self.calcturb.set_extraction_ddl_obs(self.obs_extraction_ddls)
        self.calcturb.set_extraction_ddl_com(self.com_extraction_ddls)
        
        # Lancement des calculs
##        try:
        self.calcturb.calculate_force()
##        except ValueError:
##            # Problème de produit matriciel
##            self.mess.disp_mess("!! Problèmes dans les produits de matrices              !!")
##            self.mess.disp_mess("!! Vérifier la cohérence entre les bases et les données !!")
##            self.mess.disp_mess("!! sur le nombre de DDL de mesure ou le nombre de modes !!")
##            self.mess.disp_mess(" ")
##            UTMESS('A','MACRO_VISU_MEIDEE','PROBLEME DE COHERENCE ENTRE DONNEES')

        
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
        AJOUT LE 28/08/2007 :
            Valeurs  synt <=> valeurs singulieres de la matrice C.phi.Zm1
            regul <=> parametres de regulation
        MODIFICATION LE 07/01/2008: utilisation de reférences
        vers les fonctions à appeler."""
        self.var_list = [[],[]]
        mess_err = "!! Impossible de créer la liste des courbes à afficher !!"
        self.mess.disp_mess(" ")
        for ind_tabl in range(2):
            opt_key = self.var_visu_resu[ind_tabl].get()
            if opt_key == "Choisir" :
                continue
            optdict = self.opt_data[opt_key]
            # Récupération de la fonction à appeler
            calc_func = optdict["function"]
            calc_func(optdict["resultat_attr"], ind_tabl)

    def calcul_depl_phy(self, resultat_attr, ind_tabl):
        """Calcul les paramètres: 'Depl phy', 'Depl phy r','Depl synt'."""
##        inter_spec = getattr(self, resultat_attr)
        liste, bid1, bid2 = nume_ddl_phy(self.obs_co,self.obs_extraction_ddls)
        
        for ind in range(len(liste)):
            lst = crea_list_no(ind, liste)
            for ind in lst:
                self.var_list[ind_tabl].append([ind])
        self.menu_list[ind_tabl].set_values(self.var_list[ind_tabl])

    def calcul_eff_phy(self, resultat_attr, ind_tabl):
        """Calcul les paramètres: 'Eff Phy'."""
##        inter_spec = getattr(self, resultat_attr)
        liste, bid, bid = nume_ddl_phy(self.com_co,self.com_extraction_ddls)
        
        for ind in range(len(liste)):
            lst = crea_list_no(ind, liste)
            for ind in lst:
                self.var_list[ind_tabl].append([ind])
        self.menu_list[ind_tabl].set_values(self.var_list[ind_tabl])


    def calcul_eff_mod(self, resultat_attr, ind_tabl):
        """Calcul les paramètres: 'Eff mod', 'Eff mod r'."""
        liste = nume_ddl_gene(self.calcturb.res_base)
        
        for mode in range(len(liste)):
            lst = crea_list_mo(mode, liste)
            for ind in lst:
                self.var_list[ind_tabl].append([ind])
        self.menu_list[ind_tabl].set_values(self.var_list[ind_tabl])
    
    def calcul_valeurs(self, resultat_attr, ind_tabl):
        """Calcul les paramètres: 'Valeurs sing', 'regul'."""
        if not self.calcturb.inter_spec.nume_gene:
            liste = nume_ddl_gene(self.calcturb.res_base)
        for ind in liste:
            self.var_list[ind_tabl].append([ind])
        self.menu_list[ind_tabl].set_values(self.var_list[ind_tabl])

    def _get_selected_variables(self, ind_tab):
        """Retourne les variables selectionées dans une colonne
        affichant les résultats."""
        var_list = []
        liste = self.menu_list[ind_tab].get_selected()
        for idx in liste:
            var = self.var_list[ind_tab][int(idx)]
            var_list.append(var[0])
        return var_list

    def _get_graph_data(self):
        """Retourne les valeurs et les légendes pour
        les courbes de résultats."""
        # values = liste dont chaque elmt est une courbe
        values = []
        captions = []
        
        for ind_tab in range(2):
            opt_key = self.var_visu_resu[ind_tab].get()
            if opt_key == "Choisir" :
                continue
            
            optdict = self.opt_data[opt_key]
            res = getattr(self, optdict["resultat_attr"])
            num_option = self.opt_noms.index(opt_key)

            for var in self._get_selected_variables(ind_tab):
                vect = None
                # Type de resultat a representer
                if num_option <= 5:
                    num = getattr(res, optdict["numero"])
                    ikey, jkey = var.split(',')
                    iidx = num.index(ikey)
                    jidx = num.index(jkey)
                    vect = getattr(res, 'matr_inte_spec')[1:, iidx, jidx]
                elif num_option == 6 or num_option == 7:
                    idx = int(var[2:])
                    vect = res[idx - 1, 1:]

                # Options de visualisation  
                if self.var_amp.get() == 'Amplitude':
                    vect = abs(vect)
                elif self.var_amp.get() == 'Phase':
                    vect = arctan(vect.imag / vect.real)
                if self.var_ord.get() != "Lin" and  \
                                            self.var_amp.get() == 'Phase':
                    self.mess.disp_mess("!! Impossible de représenter " \
                                        "la phase dans un !!")
                    self.mess.disp_mess("!!           diagramme " \
                                        "logarithmique          !!")

                values.append(vect)          
                captions.append("%s %s" % (opt_key, var))
        
        return values, captions

    def plot_curve(self):
        """Selection dans les interspectres disponibles des resultats
        sélectionnées dans les listes menu_list, et plot
        des courbes correspondantes"""
        try:
            freq = self.Syy.f[1:]
        except:
            self.mess.disp_mess("Impossible de récupérer la fréquence " \
                                "sur l'interspectre 'Syy'")
            return
        
        values, caption = self._get_graph_data()
        
        # XXX color n'est plus uilisé mais est-ce important? 
        # Xmgrace applique automatiquement une nouvelle couleur
        # à chaque courbe.
        color = range(1, 15)
        if len(color) > len(values):
            color = color[0 : len(values)]
        elif len(color) < len(values):
            for k in range(len(values) - len(color)):
                color.append(',1')

        self.xmgr_manager.affiche(freq, values, color, caption,
                                  self.var_abs.get(), self.var_ord.get())
   
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
    
    def setup(self):
        "Utilisé par meidee_iface.TabbedWindow"
        pass

    def teardown(self):
        "Utilisé par meidee_iface.TabbedWindow"
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
        filtres.append(_F(GROUP_NO=grp["NOM"], DDL_ACTIF=grp["NOM_CMP"]))
    for grp in grp_ma:
        filtres.append(_F(GROUP_MA=grp["NOM"], DDL_ACTIF=grp["NOM_CMP"]))
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

def update_extraction_ddls(grp_no, grp_ma):
    """Trouve les DDL selectionés pour les groupes de noeuds
    et de mailles."""
    extraction_ddls = set()
    
    for grp_lst in [grp_no, grp_ma]:
        for grp in grp_lst:
            for ddl in grp["NOM_CMP"]:
                extraction_ddls.add(ddl)
    
    extraction_ddls = list(extraction_ddls)
    extraction_ddls.sort(key=sort_compo_key)
    return extraction_ddls


