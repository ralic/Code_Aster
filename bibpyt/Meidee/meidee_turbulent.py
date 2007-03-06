#@ MODIF meidee_turbulent Meidee  DATE 06/03/2007   AUTEUR BODEL C.BODEL 
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

from Utilitai.Utmess import UTMESS
try:
    from Tkinter import Frame, Menubutton, Menu, StringVar, IntVar, Listbox
    from Tkinter import Toplevel, Scrollbar, Label, Radiobutton, Button, Entry
    from Tkinter import Checkbutton
    from Numeric import array, zeros, conjugate, transpose, identity
    from Numeric import transpose, ones, arctan, pi, log
    import MLab
    from Meidee.meidee_iface import Compteur, CreaTable, MyMenu, MultiList, PlotXMGrace
    from Meidee.meidee_cata import Resultat, InterSpectre
    import string
    from Matrix import Matrix
    from Accas import _F
    from Cata.cata import modele_sdaster , mode_meca, matr_asse_depl_r, maillage_sdaster
    from Cata.cata import cara_elem, cham_mater, table_sdaster, table_fonction
    from Cata.cata import CREA_CHAMP, PROJ_CHAMP, DETRUIRE
    import aster
    
except ImportError:    
    UTMESS('A',  'MACRO_VISU_MEIDEE',
           "ERREUR PENDANT L'IMPORTATION DES MODULES MEIDEE")


########################
#                      #
#  CLASSES GRAPHIQUES  #
#                      #
########################



DEFAULT_FRAME_ARGS = {
    "relief" : 'ridge',
    "borderwidth" : 4,
    }




class InterfaceTurbulent(Frame):

    """
    Classe qui fabrique l'interface graphique permettant de réaliser une identification
    d'efforts turbulents, et de controler les calculs. On y trouve les méthodes suivantes :
     - choix_donnees, choix_projection, frame_meth, choix_methode, visu_resu : définition
       frame d'interface graphique,
     - get_list, plot_curve : résultats à visualiser, affichage des courbes,
     - calculate_force : dirige les calculs (effectués dans la classe CalculInverse)
     - crea_champ, proj_champ : utilitaires utilisant les op aster du meme nom
    """
    
    def __init__(self,root,aster_objects, mess, out):

        Frame.__init__(self, root, relief='raised', borderwidth=4)
        self.root = root

        # Classe de calculs
        self.calcturb = CalculTurbulent(mess)

        # Objets aster en memoire, et initialisation des noms generiques donnes
        self.objects = aster_objects
##        self.tables = InterSpectre
        self.resu_mod = None
        self.resu_fonc = None
        self.modele_fonc = None
        self.mess = mess
        self.out = out

        
        # Parametres de calcul et de visualisation
        nb_freq = nb_mod = nb_mes = nb_act = 0
        self.f = zeros((nb_freq))
        self.Syy = zeros((nb_freq,nb_mes, nb_mes))
        self.Syy_R = zeros((nb_freq,nb_mes, nb_mes))
        self.Sqq = zeros((nb_freq,nb_mod, nb_mod))
        self.SQQ = zeros((nb_freq,nb_mod, nb_mod))
        self.SQQ_R = zeros((nb_freq,nb_mod, nb_mod))
        self.Sff = zeros((nb_freq,nb_act, nb_act))
        self.alpha = StringVar() 
        self.epsilon = StringVar()
        
        l = Label(self,text="Identification d'un chargement aléatoire",
                  pady=5, font=("Helvetica", "16", "bold") )
        l.grid(row=0, columnspan = 8 )

        # Les 3 etapes de l'interface
        colonne_1 = Frame(self)
        box_cd = self.choix_donnees(colonne_1) 
        box_cd.grid(row=0, sticky='w'+'e'+'n'+'s')
        box_cp = self.choix_projection(colonne_1)
        box_cp.grid(row=1, sticky='w'+'e'+'n'+'s')
        box_cm = self.choix_methode(colonne_1)
        box_cm.grid(row=2, sticky='w'+'e'+'n'+'s')
        colonne_2 = Frame(self)
        box_vr = self.visu_resu(colonne_2)
        box_vr.grid(row=1, sticky='w'+'e'+'n'+'s')
        colonne_1.grid(row=0, column=0, rowspan=1)
        colonne_2.grid(row=0, column=1, rowspan=1)
        self.columnconfigure(0,weight=1)
        self.columnconfigure(1,weight=1)
        


    def choix_donnees(self,root):
        """Choix des données d'entrée"""
        f = Frame(root, relief='ridge', borderwidth=4 )

        # Menu de choix des donnes de calcul en entree
        Label(f, text="Choix des donnees de calcul", font=("Times", "16")
              ).grid(row=0, column=0,columnspan=6)

        # Menu choix de la base modale
        Label(f, text="Base modale").grid(row=1,column=0, columnspan=2)
        options = self.objects.get_resultats_num()
        self.var_resu_mod = StringVar()
        self.menu_resu_mod = MyMenu( f, options, self.var_resu_mod )
        self.menu_resu_mod.grid(row=2, column=0, columnspan=2)


        # Menu choix du resultat en fonctionnement
        Label(f, text="Résultats en fonctionnement").grid(row=1, column=2, columnspan=2)
        options = self.objects.get_inter_spec_name()
        self.var_resu_fonc = StringVar()
        self.menu_resu_fonc = MyMenu( f, options, self.var_resu_fonc)
        self.menu_resu_fonc.grid(row=2, column=2)
        


        # Menu choix du modele experimental associe aux donnees en fonctionnement
        Label(f, text="Modèle  fonctionnement").grid(row=1, column=4, columnspan=2)
        options = self.objects.get_resultats_num()
        self.var_modele_fonc = StringVar()
        self.menu_modele_fonc = MyMenu( f, options, self.var_modele_fonc)
        self.menu_modele_fonc.grid(row=2, column=4, columnspan=2)

        return f
    

    def choix_projection(self, root):
        """ Choix de la méthode de projection des efforts sur base physique"""
        
        self.fr = Frame(root, relief='ridge', borderwidth=4 )
        Label(self.fr, text="Type de représentation des efforts sur base physique",
              font=("Times","16")).grid(row=0, column=0, columnspan=6, stick='e'+'w')   
        self.options_meth = ["Efforts discrets localises",
                             "Efforts et moments discrets"]
        
        self.var_opt = StringVar()
        MyMenu( self.fr, self.options_meth, self.var_opt, cmd=self.switch_tab
                ).grid(row=1, sticky='w',column=0)
        self.var_opt.set(self.options_meth[0])
        
        self.frame_meth()
        
        Button(self.fr, text="Valider", command=self.validate_models
               ).grid(row=3,column=6,sticky='e'+'w')

        return self.fr


    def switch_tab(self):
        self.sub_fr.grid_remove()
        self.frame_meth()



    def frame_meth(self):
        """ Fenetre à afficher pour le choix des options relatives à la méthode de projection"""
        self.sub_fr = Frame(self.fr)
        self.sel_ddl = StringVar()
        if self.var_opt.get() == self.options_meth[0]:
            # Option "efforts discrets localisés"
            Label(self.sub_fr, text="Modèle définissant les points de localisation"
                  ).grid(row=1, column=0, columnspan=4)
            options = self.objects.get_resultats_num()
            self.var_choix_act = StringVar()
            self.menu_choix_act = MyMenu( self.sub_fr, options, self.var_choix_act)
            self.menu_choix_act.grid(row=2, column=0, sticky='w', columnspan=2, rowspan=2)
            # TODO : ajouter un moyen de sélectionner une partie des ddl
        elif self.var_opt.get() == self.options_meth[1]:
            # Option "Efforts et moments discrets"
            Label(self.sub_fr, text="Modèle définissant les points de localisation"
                  ).grid(row=1, column=0, columnspan=4)
            options = self.objects.get_resultats_num()
            self.var_choix_act = StringVar()
            self.menu_choix_act = MyMenu( self.sub_fr, options, self.var_choix_act)
            self.menu_choix_act.grid(row=2, column=0, sticky='w', columnspan=2, rowspan=2)                        
        self.sub_fr.grid(row=2,column=0,sticky='e'+'w')


    

    def choix_methode(self, root):
        """Choix de la méthode de résolution (techniques de régularisation"""
        f = Frame(root, relief='ridge', borderwidth=4 )

        # Menu de choix de la methode de calcul des efforts
        Label(f, text="Choix de la methode de calcul",
              font=("Times","16")).grid(row=0, column=0,columnspan=3)

        Label(f, text="Tikhonov")
        Label(f, text="Alpha =").grid(row=1,column=1)
        entree1 = Entry(f, textvariable=self.alpha )
        entree1.grid(row=1,column=2)
        self.alpha.set(0.0)
                
        Label(f, text="SVD tronquée").grid(row=2, column=0)
        Label(f, text="Eps =").grid(row=2,column=1)
        entree2 = Entry(f, textvariable=self.epsilon )
        entree2.grid(row=2,column=2)
        self.epsilon.set(0.0)

        Button(f, text="Calculer", command=self.calculs).grid(row=4,column=6,sticky='s'+'w')

        return f



    def visu_resu(self, root):
        """ Affichage dans une fenetre des voies à afficher, possibilité
        de visualiser les autospectres et les interspectres"""
        f = Frame(root, relief='ridge', borderwidth=4 )
        self.curve_color = StringVar()
        self.var_abs = StringVar()
        self.var_ord = StringVar()
        self.var_amp = StringVar()

        Label(f, text="Visualisation des résultats",
              font=("Times","16")).grid(row=0, column=0,columnspan=3)
        Label(f, text="Choix des données à visualiser").grid(row=1, column=0,columnspan=3)

        self.options = ["Depl phy",
                        "Depl phy r",
                        "Depl mod",
                        "Eff mod",
                        "Eff mod r",
                        "Eff phy",
                        "Depl synt"]
        self.var_visu_resu = [StringVar(), StringVar()]
        self.var_export = [StringVar(), StringVar()]
        self.menu_visu_resu = [None, None]
        self.menu_list = [None, None]

        for ind_tab in range(2):
            self.menu_visu_resu[ind_tab] = MyMenu( f,self.options,
                                                   self.var_visu_resu[ind_tab])
            self.menu_visu_resu[ind_tab].grid(row=2, column=ind_tab,
                                              columnspan=5, sticky='w')
            self.menu_list[ind_tab] = MultiList(f,["indice mode/position"])
            self.menu_list[ind_tab].grid(row=3, column=ind_tab,sticky='w'+'e'+'n'+'s')
            Entry(f, textvariable=self.var_export[ind_tab]).grid(row=4, column=ind_tab, sticky='w'+'e')
            fonc = "export_inte_spec" + str(ind_tab + 1)
            Button(f, text="Exporter Inter-spectre", command=getattr(self, fonc)).grid(row=5, column=ind_tab)
        Button(f, text="Valider", command=self.get_list).grid(row=2, column=2)

        # Options de visualisation (échelles)
        opt_box = Frame(f)
        Label(opt_box, text="Echelle Absisses").grid(row=0, column=0, sticky='w')
        Label(opt_box, text="Echelle Ordonnées").grid(row=1, column=0, sticky='w')
        Label(opt_box, text="Amp/Phase").grid(row=2, column=0, sticky='w')
        self.amp_phas = ["Amplitude","Phase"]
        self.opt_trac = ["LIN","LOG","dB"]
        self.menu_abs = MyMenu( opt_box,self.opt_trac,self.var_abs)
        self.menu_abs.grid(row=0, column=1, columnspan=5, sticky='e')
        self.menu_ord = MyMenu( opt_box,self.opt_trac,self.var_ord)
        self.menu_ord.grid(row=1, column=1,columnspan=5, sticky='e')
        self.menu_amp = MyMenu( opt_box,self.amp_phas,self.var_amp)
        self.menu_amp.grid(row=2, column=1,columnspan=5, sticky='e')
        
        self.var_amp.set("Amplitude")
        self.var_abs.set("LIN")
        self.var_ord.set("LIN")
        opt_box.grid(row=6, column=0, columnspan=2, sticky = 'e''w')

        Button(f, text="Afficher courbe", command=self.plot_curve).grid(row=5, column=2)

        return f


    def plot_curve(self):
        """
        Selection dans les interspectres disponibles des resultats sélectionnées
        dans les listes menu_list, et plot des courbes correspondantes
        """
        caption = []
        values = []
        var_list = []
        echelles = [self.var_abs.get(), self.var_ord.get()]
        freq = self.Syy.f[1:]
        if echelles[0] == 'LOG':
            freq = log(freq).tolist()
        for ind_tab in range(2):
            liste = []
            liste = self.menu_list[ind_tab].get_selected()
            var_list = []
            for l in liste:
                var_list.append(self.var_list[ind_tab][int(l)])
            for l in var_list:
                vect = None
                a = string.split(l[0],',')
                # Type de resultat a representer
                option = self.var_visu_resu[ind_tab].get()
                if option == self.options[0]:
                    i = self.Syy.nume.index(a[0])
                    j = self.Syy.nume.index(a[1])
                    vect = self.Syy.matr_inte_spec[1:,i,j]
                elif option == self.options[1]:
                    i = self.Syy.nume.index(a[0])
                    j = self.Syy.nume.index(a[1])
                    vect = self.Syy_R.matr_inte_spec[1:,i,j]
                elif option == self.options[2]:
                    i = self.Syy.modes.index(a[0])
                    j = self.Syy.modes.index(a[1])
                    vect = self.Sqq.matr_inte_spec[1:,i,j]
                elif option == self.options[3]:
                    i = self.Syy.modes.index(a[0])
                    j = self.Syy.modes.index(a[1])
                    vect = self.SQQ.matr_inte_spec[1:,i,j]
                elif option == self.options[4]:
                    i = self.Syy.modes.index(a[0])
                    j = self.Syy.modes.index(a[1])
                    vect = self.SQQ_R.matr_inte_spec[1:,i,j]
                elif option == self.options[5]:
                    i = self.Sff.nume.index(a[0])
                    j = self.Sff.nume.index(a[1])
                    vect = self.Sff.matr_inte_spec[1:,i,j]
                if option == self.options[6]:
                    i = self.Syy.nume.index(a[0])
                    j = self.Syy.nume.index(a[1])
                    vect = self.Syy_S.matr_inte_spec[1:,i,j]
                    
                # Options de visualisation  
                if self.var_amp.get() == 'Amplitude':
                    vect = abs(vect)
                elif self.var_amp.get() == 'Phase':
                    vect = arctan(vect.imag/vect.real)
                if echelles[1] != "Lin" and  self.var_amp.get() == 'Phase' :
                    self.mess.disp_mess("!! Impossible de représenter la phase dans un !!")
                    self.mess.disp_mess("!!           diagramme logarithmique          !!")
                if echelles[1] == 'LOG':
                    for ind in range(vect.shape[0]):
                        vect[ind] = log(vect[ind])/log(10)
                if self.var_ord.get() == 'dB':
                    vect = 20*log(vect)/log(10)
                values.append(vect)           # values = liste dont chaque elmt est une courbe
                caption.append(option+' '+l[0])
                
        color = [1,2,3,4,5,6,7,8,9,10,11,12,13,14]

        if len(color) > len(values):
            color = color[0:len(values)]
        elif len(color) < len(values):
            for k in range(len(values) - len(color)):
                color.append(',1')
        
        PlotXMGrace(freq, values, color, caption, echelles)
        self.values = values
        self.caption = caption
                             

                    
    def get_list(self):
        """
        Routine qui crée les liste de courbes à afficher.
        self.options = choix de la donnée à représenter : 0 <=> déplacements physiques,
        1 <=> déplacements physiques reconstitués, 2 <=> déplacements modaux,
        3 <=> efforts modaux, 4 <=> efforts modaux reconstitués, 5 <=> efforts physiques
        """
        self.var_list = [[],[]]
        self.mess.disp_mess( ( " get_list : creation des courbes a afficher " ) ) 
        for ind_tabl in range(2):
            var = self.var_visu_resu[ind_tabl].get()
            if var==self.options[2] or var==self.options[3] or var==self.options[4]:
                liste = self.resu_fonc.nume_ddl_gene(self.resu_mod)  # Liste des indices de modes du modèle modal
                for mode in range(len(liste)):
                    l = crea_list_mo(mode, liste)
                    for ind in l:
                        self.var_list[ind_tabl].append([ind])
                self.menu_list[ind_tabl].set_values(self.var_list[ind_tabl])
            elif var==self.options[0] or var==self.options[1] or var==self.options[5] or var==self.options[6]:
                if var==self.options[0] or var==self.options[1] or var==self.options[6]:
                    intsp = self.Syy
                    liste = intsp.nume_ddl_phy(self.modele_fonc)
                if var==self.options[5]:
                    intsp = self.Sff
                    liste = intsp.nume_ddl_phy(self.modele_act)
                    intsp.var_opt(self.var_opt.get())
                for ind in range(len(liste)):
                    l = crea_list_no(ind, liste)
                    for ind in l:
                        self.var_list[ind_tabl].append([ind])
                self.menu_list[ind_tabl].set_values(self.var_list[ind_tabl])
            else:
                pass


    def calculs(self):
        """!Lance la classe CalculAster, qui dirige toutes les routines
            Aster et python
        """

        self.para = [self.alpha.get(), self.epsilon.get(),
                     self.var_opt.get()]

                     
        # Lancement des calculs
        self.calcturb.calculate_force(self.resu_fonc, self.modele_fonc,
                                      self.resu_mod, self.modele_act,
                                      self.para)
        
        # Rafraichissement des donnees dans la classe InterfaceTurbulent
        self.Syy   = self.calcturb.Syy
        self.Syy_R = self.calcturb.Syy_R
        self.Sqq   = self.calcturb.Sqq
        self.SQQ   = self.calcturb.SQQ
        self.SQQ_R = self.calcturb.SQQ_R
        self.Sff   = self.calcturb.Sff
        self.Syy_S = self.calcturb.Syy_S

   
    def refresh(self):
        options = self.objects.get_resultats_num()
        self.menu_resu_mod.update( options, self.var_resu_mod )
        options = self.objects.get_inter_spec_name()
        self.menu_resu_fonc.update( options, self.var_resu_fonc )
        options = self.objects.get_resultats_num()
        self.menu_modele_fonc.update( options, self.var_modele_fonc )

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
        if option == self.options[0]:
            self.Syy.make_inte_spec(titre, out)
        if option == self.options[1]:
            self.Syy_R.make_inte_spec(titre, out)
        if option == self.options[2]:
            self.Sqq.make_inte_spec(titre, out)
        if option == self.options[3]:
            self.SQQ.make_inte_spec(titre, out)
        if option == self.options[4]:
            self.SQQ_R.make_inte_spec(titre, out)
        if option == self.options[5]:
            self.Sff.make_inte_spec(titre, out)
                          
        
    
    def setup(self):
        self.refresh()


    def validate_models(self):
        """
        On va chercher les resultats associes aux noms choisis dans les menus
        dans les concepts aster en memoire
        """  
        # mesures en fonctionnement :
        self.resu_fonc = self.objects.get_inter_spec(self.var_resu_fonc.get())
        # modele EF :
        self.modele_fonc = self.objects.get_resu(self.var_modele_fonc.get())
        # modele modal :
        self.resu_mod = self.objects.get_resu(self.var_resu_mod.get())
        # modele EF sur lequel on projette les efforts modaux
        self.modele_act = self.objects.get_resu(self.var_choix_act.get())

        # Les inter-spectres mesurés sont reliés au Resultat self.modele_fonc
        self.resu_fonc.link_model(self.resu_mod)
##        self.resu_fonc.link_model(self.modele_fonc)

        self.mess.disp_mess( ( "Base de modes utilisée : " +
                               self.var_resu_mod.get() ) ) 
        self.mess.disp_mess( ( "Données mesurées en fonctionnement : " +
                               self.var_resu_fonc.get() ) )
        self.mess.disp_mess( ( "Maillage des points de mesure en fonctionnement : " +
                               self.var_modele_fonc.get() ) ) 
        self.mess.disp_mess( ( "Maillage de projection des efforts sur base physique : " +
                               self.var_choix_act.get() ) )
        self.mess.disp_mess( ( " " ) )

    def teardown(self):
        pass

#------------------------------------------------------------------------------

#######################
#                     #
#  CLASSES DE CALCUL  #
#                     #
#######################


class CalculTurbulent:
    """!Classe de methodes dirigeant des operateurs Code_Aster,
        ou faisant le lien entre sd aster et instances de classes python"""

    def __init__(self, mess):
        self.mess   = mess

        
    def calculate_force(self, resu_fonc, modele_fonc, resu_mod, modele_act, para):
        """ !Rappels :
          - self.resu_mod est un modele numerique dont on utilise la base modale (instance de Resultat)
          - self.modele_fonc est un modele dans lequel on décrit le dispositif experimental. Instance de Resultat
            (mais on a juste besoin dans l'instance du modele et de la numerotation, voir si on peut pas creer une
             classe juste pour ca).
          - self.resu_fonc : inter-spectre de mesures en fonctionnement
          - self.modele_act : de la classe Resultat, le modele décrit les ddl où l'on souhaite projeter
            les efforts modaux
        """
        
        self.resu_fonc    = resu_fonc
        self.modele_fonc  = modele_fonc
        self.resu_mod     = resu_mod
        self.modele_act   = modele_act
        self.alpha        = para[0]
        self.epsilon      = para[1]
        self.var_opt      = para[2]

        # Fabrication de la matrice C_Phi au sens Aster
        self.res_red = self.proj_cham(self.modele_fonc, 'res_red')
        # Idem pour PhiT_B, dont la fabrication dépend de la methode de projection
        self.res_act = self.proj_cham(self.modele_act, 'res_act')

        # On passe ces matrice au format python
        self.f = self.resu_fonc.f                               # Frequences de discretisation
        self.modes_red = self.extr_mat(self.res_red, opt = 1)   # Matrice C_Phi. Les mesures sont faites sur les ddl de translation uniquement, donc pour l'instant, on n'extrait la base modale que sur ces ddl
        self.modes_act = transpose(self.extr_mat(self.res_act)) # Matrice Phi^T_B, pour l'instant on projette les efforts sur les points de mesure
        self.Syy = self.resu_fonc                               # Matrice inter-spectrale
        # On range tous les résultats intermédiaires et définitifs dans une classe CalculInverse
        # Les resultats sont des instances de la classe InterSpectre
        resultat = CalculInverse(self, self.mess)
        self.Sqq = resultat.calc_Sqq(self.Syy.matr_inte_spec, self.modes_red)
        self.Syy_R = resultat.verif_Syy()
        self.Syy_R.link_model(self.modele_fonc)
        self.SQQ = resultat.calc_SQQ(self.resu_mod)
        self.Sff = resultat.calc_Sff(self.modes_act)
        self.Sff.link_model(self.modele_act)
        self.SQQ_R = resultat.verif_SQQ()
        self.Syy_S = resultat.synthes_Syy(self.resu_mod)
        self.Syy_S.link_model(self.modele_fonc)

        self.Sff.link_model(self.modele_act) # on lie les interspectres de résulat au modele
##        self.Sff.var_opt(self.var_opt)
 


    
    def crea_champ(self, resu, ind_mod, opt = 0, select_ddl = None): 
        """!Extrait un champ de deplacement d'une sd_resultat aster
            et extrait les DDL de mesure, qui sont pour l'instant les DY
            et les rotations DRZ """
        # TODO : utiliser la macro d'observabilite pour choisir les DDL de mesure
        # sur les noeuds du modele exprimental
        __CHANO = CREA_CHAMP( TYPE_CHAM = 'NOEU_DEPL_R',
                              OPERATION  = 'EXTR',
                              RESULTAT   = resu,
                              NOM_CHAM   = 'DEPL',
                              NUME_ORDRE = ind_mod,
                             );

        if self.var_opt == "Efforts discrets localises" or opt == 1:
            chano_x  = __CHANO.EXTR_COMP('DX',[],1)
            DETRUIRE(CONCEPT=_F(NOM=(__CHANO,),),INFO=1,)
            return chano_x.noeud, 'DX', chano_x.valeurs
        elif self.var_opt == "Efforts et moments discrets":
            chano_x  = __CHANO.EXTR_COMP('DX',[],1)
            chano_rz = __CHANO.EXTR_COMP('DRZ',[],1)
            DETRUIRE(CONCEPT=_F(NOM=(__CHANO,),),INFO=1,)
            return chano_x.noeud, 'DX', chano_x.valeurs, 'DRZ', chano_rz.valeurs


    def proj_cham(self, modele, name):
        """ Projette le résultat "base modale" [Phi] sur le modèle
            décrivant les capteurs (self.modele_fonc) ou les points de
            localisation des forces (self.modele_act) pour fabriquer les
            matrices [C_Phi] et [PhiT_B]
        """
        
        
        __red=PROJ_CHAMP(METHODE='ELEM',
                         RESULTAT=self.resu_mod.obj,
                         MODELE_1=self.resu_mod.modele,
                         MODELE_2=modele.modele,
                         NOM_CHAM='DEPL',
                         TOUT_ORDRE='OUI',
                         NUME_DDL=modele.nume,
                         VIS_A_VIS=_F(TOUT_1='OUI',
                                      TOUT_2='OUI',
                                     ),
                        );
        
        modes_red = Resultat(name, __red, self.mess)
        modes_red.cara_mod = self.resu_mod.get_cara_mod()
        return modes_red



    def extr_mat(self, modele_red,
                 opt = 0
                 ):
        """
        Extraction d'une matrice python de modes type sdt à partir d'une matrice Aster
        Pour l'instant, on ne récupère que les coordonnées des modes selon DY
        et/ou DRZ.
        
        """
        self.nb_mod = modele_red.cara_mod.shape[0] # nb de modes

        # boucles sur les modes
        matrice = []
        for ind_mod in range(1,self.nb_mod+1):
            defo = []
            args = self.crea_champ(modele_red.obj, ind_mod, opt)
            for ind_no in range(len(args[0])):
                for ind_ddl in range(2,len(args),2):
                    defo.append(args[ind_ddl][ind_no])
            matrice.append(defo)
            
        matrice = transpose(array(matrice))
        
        return matrice

#-------------------------------------------------------------------------------

class CalculInverse:
    """ classe rassemblant tous les résultats intermédiaires et définitifs
    calculés par methode inverse en python uniquement"""

    def __init__(self,
                 calcturb,
                 mess):
        
        # dimensions du problème
        self.calcturb = calcturb
        self.nb_freq  = len(calcturb.f)               # nombre de pas de fréquence
        self.nb_mod   = calcturb.modes_red.shape[1]   # nb modes
        self.nb_mes   = calcturb.modes_red.shape[0]   # nb de points de mesure
        self.nb_act   = calcturb.modes_act.shape[1]   # nb de points de discrétisation
        self.f        = calcturb.f
        self.var_opt  = calcturb.var_opt

        self.mess = mess
        
        # matrices inter-spectrales de données et de résultats

        # paramètres de calcul
        self.alpha   = float(calcturb.alpha)
        self.epsilon = float(calcturb.epsilon)


    def calc_Sqq(self, Syy, CPhi):
        """ Calcul de l'inter-spectre des déplacements modaux à partir des
        déplacements physiques : [Sqq(om)] = ([CPhi]+)[Syy(om)]([CPhi]+)^H"""

        self.mess.disp_mess("Calcul de Sqq : deplacements modaux")
        self.mess.disp_mess(" ")
        self.Sqq = zeros((self.nb_freq,self.nb_mod,self.nb_mod),'D')
        self.Syy = Syy
        self.CPhi = CPhi
        aster.matfpe(-1) # on desactive temporairement les FPE generees parfois, a tort, par blas
        U,S,VH = MLab.svd(self.CPhi)
        aster.matfpe(1) # FPE reactivees
        Smax = max(S)
        U = Matrix(U)
        V = Matrix(transpose(VH))
        l = len(S)
        S = array(S)
        inv_S = S[:]
        self.mess.disp_mess("Valeurs singulières de la matrice d'observabilité")
        self.mess.disp_mess(str(S))
        for ind in range(l):
            if S[ind] > self.epsilon*Smax:
                inv_S[ind] = S[ind]/(S[ind]**2+self.alpha)
            else:
                inv_S[ind] = 0
        inv_S = Matrix(array(inv_S)*identity(l))
        inv_CPhi = V*inv_S*transpose(U)
        inv_CPhi_H = conjugate(transpose(inv_CPhi))
        for ind_freq in range(self.nb_freq):
            Syy_f = Matrix(self.Syy[ind_freq,:,:])
            Sqq_f = inv_CPhi*self.Syy[ind_freq,:,:]*inv_CPhi_H
            self.Sqq[ind_freq,:,:] = Sqq_f

        Sqq = InterSpectre(nom        = 'Sqq',
                           mat        = self.Sqq,
                           frequences = self.f,
                           var_opt    = self.var_opt,
                           mess       = self.mess)
        return Sqq


    def verif_Syy(self):
        """ Recalcule l'inter-spectre Syy à partir de Sqq, calcule un coefficient d'erreur"""

        self.mess.disp_mess("Calcul de Syy_R : deplacements physiques reconstitues")
        self.mess.disp_mess(" ")
        self.Syy_R = zeros((self.nb_freq,self.nb_mes,self.nb_mes),'D')
        CPhi = Matrix(self.CPhi)
        CPhi_H = conjugate(transpose(Matrix(CPhi)))
        for ind_freq in range(self.nb_freq):
            Sqq_f = Matrix(self.Sqq[ind_freq,:,:])
            Syy_R_f = CPhi*Sqq_f*CPhi_H
            self.Syy_R[ind_freq,:,:] = Syy_R_f

        Syy_R = InterSpectre(nom        = 'Syy_R',
                             mat        = self.Syy_R,
                             frequences = self.f,
                             var_opt    = self.var_opt,
                             mess       = self.mess)
        
        return Syy_R # TODO : le coefficient d'erreur


    def calc_SQQ(self,modele_mod):
        """ Calcul de la matrice d'impédance, affichage des cara modales
        et  calcul des excitations modales : [SQQ(om)] = [Z].[Sqq].[Z]^H"""

        self.mess.disp_mess("Calcul de SQQ : efforts modaux")
        self.mess.disp_mess(" ")
        self.SQQ = zeros((self.nb_freq,self.nb_mod,self.nb_mod),'D')
        freq_i, xsi_i, mass_i, modes_i, amor_i, rigi_i = modele_mod.get_modes()
        l_freq_i = array([i for i in freq_i[:,1]])    # frequences propres
        l_omega_i = 2*pi*l_freq_i                     # pulsations propres equivalentes
        l_xsi_i  = array([j for j in  xsi_i[:,1]])    # amorstissements reduits propres
        l_mass_i  = array([j for j in  mass_i[:,1]])  # amorstissements reduits propres
        for ind_freq in range(self.nb_freq):
            omega = 2*pi*self.f[ind_freq]*ones(l_freq_i.shape[0])
            Z = Matrix(l_mass_i*(-omega**2 + l_omega_i**2 + j*2*omega*l_omega_i
                               *l_xsi_i)*identity(l_freq_i.shape[0]))
            Sqq_f = Matrix(self.Sqq[ind_freq,:,:])
            SQQ_f = Z*Sqq_f*conjugate(transpose(Z))
            self.SQQ[ind_freq,:,:] = SQQ_f
        SQQ = InterSpectre(nom        = 'SQQ',
                           mat        = self.SQQ,
                           frequences = self.f,
                           var_opt    = self.var_opt,
                           mess       = self.mess)
        return SQQ
            

    def calc_Sff(self, PhiT_B):
        """ Calcul de l'inter-spectre des efforts physiques à partir des
        déplacements physiques : [Sqq(om)] = ([CPhi]+)[Syy(om)]([CPhi]+)^H"""

        self.mess.disp_mess("Calcul de Sff : efforts physiques ")
        self.mess.disp_mess(" ")
        self.Sff = zeros((self.nb_freq,self.nb_act,self.nb_act),'D')
        self.PhiT_B = PhiT_B
        aster.matfpe(-1) # on desactive temporairement les FPE generees parfois, a tort, par blas        
        U,S,VH = MLab.svd(self.PhiT_B)
        aster.matfpe(-1) #FPE reactivees
        Smax = max(S)
        U = Matrix(U)
        V = Matrix(transpose(VH))
        l = len(S)
        inv_S = S[:]
        self.mess.disp_mess("Valeurs singulières de la matrice de commande")
        self.mess.disp_mess(str(S))
        for ind in range(l):
            if S[ind] > self.epsilon*Smax:
                inv_S[ind] = S[ind]/(S[ind]**2+self.alpha)
            else:
                inv_S[ind] = 0
        inv_S = Matrix(array(inv_S)*identity(l))
        inv_PhiT_B = V*inv_S*transpose(U)
        inv_PhiT_B_H = conjugate(transpose(inv_PhiT_B))
        for ind_freq in range(self.nb_freq):
            SQQ_f = Matrix(self.SQQ[ind_freq,:,:])
            Sff_f = inv_PhiT_B*SQQ_f*inv_PhiT_B_H
            self.Sff[ind_freq,:,:] = Sff_f
        
        Sff = InterSpectre(nom        = 'Sff',
                           mat        = self.Sff,
                           frequences = self.f,
                           var_opt    = self.var_opt,
                           mess       = self.mess)
        return Sff


    def verif_SQQ(self):
        """ Recalcule l'inter-spectre SQQ à partir de Sff, calcule un coefficient d'erreur"""

        self.mess.disp_mess("Calcul de SQQ_R : efforts modaux reconstitues")
        self.mess.disp_mess(" ")
        self.SQQ_R = zeros((self.nb_freq,self.nb_mod,self.nb_mod),'D')
        PhiT_B = Matrix(self.PhiT_B)
        PhiT_B_H = conjugate(transpose(Matrix(PhiT_B)))
        for ind_freq in range(self.nb_freq):
            Sff_f = Matrix(self.Sff[ind_freq,:,:])
            SQQ_R_f = PhiT_B*Sff_f*PhiT_B_H
            self.SQQ_R[ind_freq,:,:] = SQQ_R_f
            
        SQQ_R = InterSpectre(nom        = 'SQQ_R',
                             mat        = self.SQQ_R,
                             frequences = self.f,
                             var_opt    = self.var_opt,
                             mess       = self.mess)
        return SQQ_R # TODO : le critère d'erreur

    def synthes_Syy(self,modele_mod):
        """! Synthèse de l'inter-spectre à partir des efforts physiques identifiés"""

        self.mess.disp_mess("Calcul des Syy_S : Synthèse modale des déplacements")
        self.mess.disp_mess(" ")
        self.Syy_S = zeros((self.nb_freq,self.nb_mes,self.nb_mes),'D')
        
        freq_i, xsi_i, mass_i, modes_i, amor_i, rigi_i = modele_mod.get_modes()
        l_freq_i = array([i for i in freq_i[:,1]])    # frequences propres
        l_omega_i = 2*pi*l_freq_i                     # pulsations propres equivalentes
        l_xsi_i  = array([j for j in  xsi_i[:,1]])    # amorstissements reduits propres
        l_mass_i  = array([j for j in  mass_i[:,1]])  # amorstissements reduits propres

        CPhi = Matrix(self.CPhi)
        CPhi_H = conjugate(transpose(CPhi))
        for ind_freq in range(self.nb_freq):
            omega = 2*pi*self.f[ind_freq]*ones(l_freq_i.shape[0])
            inv_Z = Matrix(1/(l_mass_i*(-omega**2 + l_omega_i**2 + j*2*omega*l_omega_i
                               *l_xsi_i))*identity(l_freq_i.shape[0]))
            SQQ_R_f = Matrix(self.SQQ_R[ind_freq,:,:])
            Syy_S_f = CPhi*inv_Z*SQQ_R_f*conjugate(transpose(inv_Z))*CPhi_H
            self.Syy_S[ind_freq,:,:] = Syy_S_f
            
        Syy_S = InterSpectre(nom        = 'Syy_S',
                             mat        = self.Syy_S,
                             frequences = self.f,
                             var_opt    = self.var_opt,
                             mess       = self.mess)

        return Syy_S # TODO : le coefficient d'erreur

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
    Création d'une liste de résultats visualisables sous la forme :
     < No #noeud ddl, No #noeud ddl>
    Liste est le résultat de crea_champ
    """
    list_out = []
    for ind in range(x+1):
        list_out.append(list_in[x]+','+list_in[ind])
    return list_out



