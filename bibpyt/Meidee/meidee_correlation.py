#@ MODIF meidee_correlation Meidee  DATE 21/10/2008   AUTEUR NISTOR I.NISTOR 
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


## \package meidee_mac Module de gestion des calculs %Meidee avec Aster
#
# La classe Meidee permet de centraliser les calculs de projections
# de modes experimentaux sur des modes numériques
# 
# La classe InterfaceCorrelation dirige les objets graphiques

from Utilitai.Utmess import UTMESS

import Numeric
import sys
import aster
from Meidee.meidee_cata import Resultat, CaraElem, InterSpectre, MeideeObjects

from Accas import _F
import weakref
import os

from Tkinter import Frame, Menubutton, Menu, StringVar, IntVar, Listbox
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Canvas, Toplevel
from Meidee.meidee_iface import MacWindow, MacMode, MyMenu, ModeList
from Meidee.modes import ModeFreqList, DispFRFDialogue, MacWindowFrame
from Meidee.meidee_calcul_correlation import MeideeCorrelation



########################
#                      #
#  CLASSES GRAPHIQUES  #
#                      #
#######################


#------------------------------------------------------------------------------------------------------
        
class InterfaceCorrelation(Frame):
    """!Interface principale de l'outil de projection des modes
    expérimentaux sur les modes numériques
    
    permet la sélection et de calcul des modes en air et écoulement"""
    def __init__(self,
                 root,
                 meidee_objects,
                 macro,
                 mess,
                 param_visu):
        """!Constructeur

        :IVariable:
         - `root`: fenetre parente
         - `meidee_objects`: objet Meidee, permettant d'accéder aux résultats aster
         - `mode_exp`: valeur associée au bouton de sélection des modes expérimentaux
         - `mode_etendu`: valeur associée au bouton de sélection des modes étendus
         - `mode_nume`: valeur associée au bouton de sélection des modes numériques
         - `mode_nume_red`: valeur associée au bouton de sélection des modes numériques réduits
         - `use_nume_mass`: indicateur d'utilisation de la matrice de masse numérique
         - `proj_champ_meth`: méthode à utiliser pour PROJ_CHAMP (SVD ou LU)
         - `proj_svd_param`: si méthode SVD, alors paramètre de sélection
         - `mac_windows`: liste des fenetres d'affichage de MAC modes
        """
        
        Frame.__init__(self, root, relief='flat', borderwidth=4) # Première frame
        self.mess = mess
        self.root = root
        self.macro = macro
        self.meidee_objects = meidee_objects #     objets Aster en mémoire
        self.param_visu = param_visu
        self.is_resu1 = IntVar()
        self.is_resu2 = IntVar()
        self.use_nume_mass = IntVar()
        self.proj_champ_meth = StringVar()
        self.proj_svd_param = StringVar()
        self.export_name = StringVar()
        self.meidee = MeideeCorrelation( macro, self.mess, self.meidee_objects )
        self.type_resu_exp = StringVar()
        self.term = []
        self.mac_windows = []
        self.afreq = None
        self.anum = None
        self.interface_meidee()
        self.resu_num = None   # base d'expansion (instance de Resultat)
        self.resu_exp = None   # donnees exp (instance de Resultat ou de DynaHarmo)
        self.norme_num = None  # matrice assemblee sur modele num (non obligatoire pour calcul)
        self.norme_exp = None  # idem sur modele exp



    def setup(self):
        """!Appelée par le gestionnaire de tab lors de l'affichage
            Permet de prendre en compte des nouveaux concepts
            et de les ajouter dans les MyMenu et cie..."""
        mdo = self.meidee_objects
        # commande update de MyMenu :
        #les arguments sont, dans l'ordre : options, var, command
        self.menu_resu_num.update( mdo.get_resultats_num() , self.var_resu_num , self.num_changed )
        self.menu_resu_exp.update( mdo.get_all_resus_name() , self.var_resu_exp , self.exp_changed )
        self.menu_norme_num.update( mdo.get_matr_norme() , self.var_norme_num , self.num_changed )     
        self.menu_norme_exp.update( mdo.get_matr_norme() , self.var_norme_exp, self.exp_changed )
        self.menu_resu1.update( mdo.get_all_resus_name(), self.var_resu1, self.visu1_changed )
        self.menu_resu2.update( mdo.get_all_resus_name(), self.var_resu2, self.visu2_changed )

    def teardown(self):
        """!Appelée par le gestionnaire de tab lors du masquage (passage à un autre tab)"""
        return



    def interface_meidee(self):
        """!Fonction principale de création de l'interface

        """
        self.columnconfigure(0, weight=1)
        self.rowconfigure(2, weight=1)
        l = Label(self,text="Expansion de donnees", pady=5, font=("Helvetica", "16") )
        l.grid(row=0)

        select_box = self.interface_selection(self)
        select_box.grid(row=1, sticky='w'+'e'+'s'+'n')
        
        main_param = self.interface_parametres(self)
        main_param.grid(row=2, sticky='w'+'e'+'s'+'n')
        
        visu_param = self.interface_visu(self)
        visu_param.grid(row=3, sticky='w'+'e'+'s'+'n')
        self.main = self


    def interface_selection(self, root):
        """!Creation de l'interface de selection des objets resultats"""
        f = Frame(root, relief='ridge', borderwidth=4 )
        Label(f, text="   ").grid(row=0, column=0, columnspan=3, sticky='w'+'e' )
        
        # menu de selection du resultat numerique
        Label(f,text="Base numérique d'expansion").grid(row=1,column=0,sticky='e')
        self.var_resu_num = StringVar()
        self.menu_resu_num = MyMenu( f, options = self.meidee_objects.get_resultats_num(),
                                     var = self.var_resu_num, cmd = self.num_changed )
        self.menu_resu_num.grid(row=1, column=1, sticky='ew')

        # menu de selection du resultat experimental
        Label(f,text="Résultat expérimental").grid(row=1,column=2,sticky='e')
        self.var_resu_exp = StringVar()
        self.menu_resu_exp = MyMenu( f, options = self.meidee_objects.get_all_resus_name(),
                                     var = self.var_resu_exp, cmd = self.exp_changed )
        self.menu_resu_exp.grid(row=1, column=3, sticky='ew')

        # menu de selection de la norme numerique
        Label(f,text="Norme numérique").grid(row=2,column=0,sticky='e')
        self.var_norme_num = StringVar()
        self.menu_norme_num = MyMenu( f, options = self.meidee_objects.get_matr_norme(),
                                      var = self.var_norme_num, cmd = self.num_changed )
        self.menu_norme_num.grid(row=2, column=1, sticky='ew')
        
        # menu de selection de la norme experimentale
        Label(f,text="Norme experimentale").grid(row=2,column=2,sticky='e')
        self.var_norme_exp = StringVar()
        self.menu_norme_exp = MyMenu( f, options = self.meidee_objects.get_matr_norme(),
                                      var = self.var_norme_exp, cmd = self.exp_changed )
        self.menu_norme_exp.grid(row=2, column=3, sticky='ew')

        # Type de resu experimental (dyna_harmo ou modes)
        Label(f,text="Type de résultat expérimental").grid(row=1,column=4,columnspan=2)
        Radiobutton(f,text='resultat harmonique',value='harmo',
                    variable=self.type_resu_exp).grid(row=2,column=4)
        Radiobutton(f,text='modes',value='mode',
                    variable=self.type_resu_exp).grid(row=2,column=5)

        Label(f, text="   ").grid(row=3, column=0, columnspan=3, sticky='w'+'e' )
        f.columnconfigure(0,weight=1)
        f.columnconfigure(1,weight=4)
        f.columnconfigure(3,weight=4)
        
        return f


    """ Deux routines pour mettre a jour les donnees, sd_resus et normes"""
    def num_changed(self):
        mdo = self.meidee_objects
        resu_num = norme_num = None
        if self.var_resu_num.get() != 'Choisir':
            self.resu_num = mdo.get_resu(self.var_resu_num.get())
            self.liste_num.set_resu(self.resu_num.obj) # remplissage de la liste des frequences
        if self.var_norme_exp.get() != 'Choisir':
            self.norme_num = mdo.get_matr(self.var_norme_num.get())
            
##        self.meidee.setup_num( self.resu_num, self.norme_num)

    def exp_changed(self):
        mdo = self.meidee_objects
        self.resu_exp = self.norme_exp = None
        if self.var_resu_exp.get() != 'Choisir':
            self.resu_exp = mdo.get_all_resus(self.var_resu_exp.get()) # dyna_harmo
            if self.meidee_objects.dyna_harmo.has_key(self.resu_exp.nom):
                self.type_resu_exp.set('harmo')
            else: # mode_meca ou base_modale
                self.type_resu_exp.set('mode')
            self.liste_exp.set_resu(self.resu_exp.obj) # remplissage de la liste des frequences
        if self.var_norme_exp.get() != 'Choisir':
            self.norme_exp = mdo.get_matr(self.var_norme_exp.get())

##        self.meidee.setup_exp( self.resu_exp, self.norme_exp)
    """fin"""


    def interface_parametres(self, root):
        """!Création de l'interface de choix des paramètres de calcul

        On permet à l'utilisateur de choisir les modes numériques et expérimentaux
        ainsi que la méthode de projection
        """
        f = Frame(root, relief='ridge', borderwidth=4)
        Label(f, text="   ").grid(row=0, column=0, columnspan=4, sticky='w'+'e' )
        proj_mesu_frame = Frame(f,relief='flat',borderwidth = 10)

        l1 = Label(proj_mesu_frame, text="options de \n PROJ_MESU_MODAL")
        l1.grid(row=0, column=0, columnspan=3, rowspan=2, sticky='w'+'e' )
        b1 = Radiobutton(proj_mesu_frame, text="SVD", value="SVD",variable=self.proj_champ_meth )
        b1.grid(row=2, column=0)
        Label(proj_mesu_frame, text="Eps=").grid(row=2,column=1)
        Entry(proj_mesu_frame, textvariable=self.proj_svd_param ).grid(row=2,column=2)
        b2 = Radiobutton(proj_mesu_frame, text="LU", value="LU", variable=self.proj_champ_meth )
        b2.grid(row=3, column=0)
        self.proj_champ_meth.set("SVD")
        self.proj_svd_param.set("1.E-2")
        #l = Checkbutton( f, text='Utiliser la matrice de masse numérique', variable=self.use_nume_mass )
        #l.grid( row=1, column=0, columnspan=2 )
        proj_mesu_frame.grid(row=1,column=0,columnspan=2)
        
        # liste des modes numeriques
        b3 = Button(f,text='Calculer',command=self.prepare_calcul)
        b3.grid( row = 2, column = 0, columnspan=2)
        Label(f,text='Exporter').grid(row=3, column=0)
        Entry(f, textvariable=self.export_name ).grid(row=3,column=1)
        
        self.liste_num = ModeFreqList( f, "Modes Numériques" )
        self.liste_num.grid(row=1, column=3, rowspan=2, sticky='w'+'e'+'s'+'n' )
        self.liste_exp = ModeFreqList( f, "Modes Expérimentaux" )
        self.liste_exp.grid(row=1, column=4, rowspan=2, sticky='w'+'e'+'s'+'n' )
        Label(f, text="   ").grid(row=4, column=0, columnspan=3, sticky='w'+'e' )
        
        f.grid(row=1, sticky='w'+'e'+'s'+'n')
        return f




    def interface_visu(self, root):
        """!Création de l'interface de visualisation

        permet de choisir d'afficher les matrices MAC ou les modes avec gmsh
        gère la compatibilité des choix de l'utilisateur (les calculs de MAC
        ne sont pas tous possibles)
        """
        mdo = self.meidee_objects
        f = Frame(root, relief='ridge', borderwidth=4 )
        Label(f, text="   ").grid(row=0, column=1,columnspan = 3,sticky='w'+'e' )
        Label(f, text="   ").grid(row=2, column=1,columnspan = 3,sticky='w'+'e' )
        f.columnconfigure(0,weight=3)
        f.columnconfigure(1,weight=3)
        

        f1 = Frame(f)
        f1.grid(row=1,column=0,sticky='ew' )
        f1.columnconfigure(1,weight=4)
        f1.columnconfigure(2,weight=4)
        
        Checkbutton(f1,variable=self.is_resu1,
                    command=self.cb_changed,
##                    command = None
                    ).grid(row=0, column = 0,sticky='e',padx=20)
        Checkbutton(f1,variable=self.is_resu2,
                    command=self.cb_changed,
##                    command = None
                    ).grid(row=1, column = 0,sticky='e',padx=20)

        Label(f1,text="Resultat 1").grid(row=0,column=1,sticky='w')
        self.var_resu1 = StringVar()
        self.menu_resu1 = MyMenu( f1, options = mdo.get_all_resus_name(),
                                     var = self.var_resu1, cmd = self.visu1_changed )
        self.menu_resu1.grid(row=0, column=2, sticky='ew',padx=20)

        Label(f1,text="Resultat 2").grid(row=1,column=1,sticky='w')
        self.var_resu2 = StringVar()
        self.menu_resu2 = MyMenu( f1, options = mdo.get_all_resus_name(),
                                      var = self.var_resu2, cmd = self.visu2_changed )
        self.menu_resu2.grid(row=1, column=2, sticky='ew',padx=20)

        f2 = Frame(f)        
        f2.grid(row=1,column=1)
        self.mac_button = Button(f2,text='    MAC    ',command=self.view_macs,state='disabled')
        self.mac_button.grid(row=1,column=0, sticky='ew' )
        self.phi_button = Button(f2,text='Déformées',command=self.view_modes,state='disabled')
        self.phi_button.grid(row=2,column=0, sticky='ew')
        self.frf_button = Button(f2,text='    FRF    ',command=self.view_frf)
        self.frf_button.grid(row=3,column=0, sticky='ew' )

        return f

    def set_proj_svd(self):
        self.proj_champ_meth.set("SVD")
        
    def set_proj_crout(self):
        self.proj_champ_meth.set("LU")

    def visu1_changed(self):
        """ desactivation du bouton concernant le visu1"""
        self.is_resu1.set(1)
        self.check_state()        

    def visu2_changed(self):
        """ desactivation du bouton concernant le visu1"""
        self.is_resu2.set(1)
        self.check_state()

    def cb_changed(self):
        self.check_state()

##    def check_state(self):
##        """Verifie la compatibilite des bases pour le MAC et l'existence
##           des donnees necessaires pour la visu des deformees et des FRF"""
##        mdo = self.meidee_objects
##   
##        # Y a-t-il un MAC a calculer ?
##        if self.is_resu1.get() and not self.is_resu2.get():
##            if self.var_resu1.get().strip() != "Choisir":
##                self.resu1 = mdo.get_all_resus(self.var_resu1.get())
##                if mdo.resultats.has_key(self.resu1.nom):
##                    self.mac_button.configure(state='normal')
##        elif self.is_resu2.get() and not self.is_resu1.get():
##            if self.var_resu2.get().strip() != "Choisir":
##                self.resu2 = mdo.get_all_resus(self.var_resu2.get())
##                if mdo.resultats.has_key(self.resu2.nom):
##                    self.mac_button.configure(state='normal')
##        elif self.is_resu1.get() and self.is_resu2.get():
##            if self.var_resu1.get().strip() != "Choisir" and self.var_resu2.get().strip() != "Choisir":
##                self.resu1 = mdo.get_all_resus(self.var_resu1.get())
##                self.resu2 = mdo.get_all_resus(self.var_resu2.get())
##                if mdo.resultats.has_key(self.resu1.nom) and mdo.resultats.has_key(self.resu2.nom):
##                    if self.resu1.modele_name == self.resu2.modele_name:
##                        self.mac_button.configure(state='normal')
##                    else:
##                        self.mac_button.configure(state='disabled')
##        # Y a-t-il des deformees a representer ?
##        if self.is_resu1.get() or self.is_resu2.get():
##            self.phi_button.configure(state='normal')
##        else:
##            self.phi_button.configure(state='disabled')


    def check_state(self):
        """Verifie la compatibilite des bases pour le MAC et l'existence
           des donnees necessaires pour la visu des deformees et des FRF"""
        mdo = self.meidee_objects
   
        # Y a-t-il un MAC a calculer ?
        if self.is_resu1.get() and not self.is_resu2.get():
            if mdo.resultats.has_key(self.var_resu1.get()):
                self.mac_button.configure(state='normal')
        elif self.is_resu2.get() and not self.is_resu1.get():
            if mdo.resultats.has_key(self.var_resu2.get()):
                self.mac_button.configure(state='normal')
        elif self.is_resu1.get() and self.is_resu2.get():
            if mdo.resultats.has_key(self.var_resu1.get()) and mdo.resultats.has_key(self.var_resu2.get()):
                resu1 = mdo.get_all_resus(self.var_resu1.get())
                resu2 = mdo.get_all_resus(self.var_resu2.get())
                if resu1.modele_name == resu2.modele_name:
                    self.mac_button.configure(state='normal')
                else:
                    self.mac_button.configure(state='disabled')
        else:
            self.mac_button.configure(state='disabled')
        # Y a-t-il des deformees a representer ?
        if self.is_resu1.get() or self.is_resu2.get():
            self.phi_button.configure(state='normal')
        else:
            self.phi_button.configure(state='disabled')

            
    def view_frf(self):
        """lancement d'une fenetre de visualisation des frf"""
        mdo = self.meidee_objects
        resu1 = None
        resu2 = None
        if self.is_resu1.get():
            resu1 = mdo.get_all_resus(self.var_resu1.get())
        if self.is_resu2.get():
            resu2 = mdo.get_all_resus(self.var_resu2.get())
        fenetre = DispFRFDialogue(self.mess, self.meidee_objects, self.param_visu, resu1, resu2)
            

    def view_modes(self, *args):
        """!Visualisation des modes par GMSH ou Salome
        """
        mdo = self.meidee_objects
        l_resultat = []
        l_modele = []
        if self.is_resu1.get():
            resu1 = mdo.get_all_resus(self.var_resu1.get())
            l_resultat.append(resu1.obj)
            l_modele.append(resu1.modele.obj)
        if self.is_resu2.get():
            resu2 = mdo.get_all_resus(self.var_resu2.get())
            l_resultat.append(resu2.obj)
            l_modele.append(resu2.modele.obj)
        term = self.param_visu.visu_resu(modele=l_modele,
                                         resultat=l_resultat)
        
        self.term.append(term)


    def view_macs(self):
        """!Creation d'une nouvelle fenetre de visu MAC"""
        mdo = self.meidee_objects
        resu1 = None
        resu2 = None
        if self.is_resu1.get() and self.is_resu2.get():
            resu1 = mdo.get_all_resus(self.var_resu1.get())
            resu2 = mdo.get_all_resus(self.var_resu2.get())
        elif self.is_resu1.get():
            resu1 = mdo.get_all_resus(self.var_resu1.get())
            resu2 = mdo.get_all_resus(self.var_resu1.get())
        elif self.is_resu2.get():
            resu1 = mdo.get_all_resus(self.var_resu2.get())
            resu2 = mdo.get_all_resus(self.var_resu2.get())

        mac = self.meidee.calc_mac_mode( resu1, resu2, norme = None )

        titre = "matrice de MAC pour " + resu1.nom + " et " + resu2.nom

        f = Toplevel()
        size = (200,200)
        mac_win = MacWindowFrame( f, titre, resu1.nom, resu2.nom, size)
        mac_win.grid(row=0,column=0,sticky='nsew')
        afreq1, axsi, amass, amodes, amor, arigi = resu1.get_modes()
        afreq2, axsi, amass, amodes, amor, arigi = resu2.get_modes()
        mac_win.set_modes(afreq1, afreq2, mac)

        self.mac_windows.append( mac_win )

            
    def prepare_calcul(self, *args):
        """! Demande le lancement de la macro MACRO_VISU_MEIDEE_PROJ
        """
        # Validité des donnees :
        if self.resu_num == None or self.resu_exp == None:
            self.mess.disp_mess("Il manque des données pour le calcul")
            return
        if self.resu_num.modele == None:
            self.mess.disp_mess("Il manque le modele associe au résultat numérique")
            return
        if self.resu_exp.modele == None:
            self.mess.disp_mess("Il manque le modele associe au résultat expérimental")
            return

        self.modes_num_list = self.liste_num.selection()
        self.modes_exp_list = self.liste_exp.selection()
        

        self.meidee.setup( self.resu_num, self.modes_num_list,
                           self.resu_exp, self.modes_exp_list)

        self.meidee.calc_proj_resu(basename = self.export_name.get())
        self.setup()

##    def prepare_macs(self):
##        """!Préparations des objets utilisés pour le calcul et
##        l'affichage de MAC_MODEs
##
##        \return un tuple (type modele1,type modele2) et les modes 1, modes 2, norme 1, norme 2
##        """
##        modeles = self.check_state()
##        num_modes = self.liste_num.selection()
##
##        exp_modes = self.liste_exp.selection()
##
##        norme_num = self.meidee_objects.get_matr(self.var_norme_num.get())
##        norme_exp = self.meidee_objects.get_matr(self.var_norme_exp.get())
##        proj_meth = self.proj_champ_meth.get()
##        if proj_meth == "SVD":
##            try:
##                proj_param = ( float(self.proj_svd_param.get()), )
##            except :
##                self.proj_svd_param.set("1.E-2")
##                proj_param = ( 1.E-2, )
##        else:
##            proj_param = ()
##        self.meidee.proj_meth = proj_meth
##        self.meidee.proj_param = proj_param
##        return modeles, num_modes, exp_modes, norme_num, norme_exp
##
##    def export_var(self):
##        """!Exporte le calculs de mode avec le nom basename"""
##        basename = self.export_name.get()
##        if len(basename)>5:
##            self.mess.disp_mess( "Le nom doit etre de 5 caractères maximum" )
##        modeles, num_modes, exp_modes, norme_num, norme_exp = self.prepare_macs()
##        resu_num = self.meidee.resu_num
##        resu_exp = self.meidee.resu_exp   
##        self.meidee.calc_proj_resu( num_modes, exp_modes, resu_num, resu_exp, basename )
####        self.meidee_objects.recup_objects()
##        self.mess.disp_mess(("Création des concepts : " + basename + "_ET, " +
##                            basename+"_NX, " + basename + "_EX, " + basename + "_RD, "))

##    def view_macs(self, *args):
##        """!Visualisation des modes MAC
##
##        On vérifie la validité du ou des deux jeux de modes sélectionnés, puis on
##        récupère la matrice de MAC par l'objet self.meidee et enfin on affiche le
##        résultat dans une fenetre MacWindow
##        """
##        if not self.meidee.is_valid():
##            return
##        modeles, num_modes, exp_modes, norme_num, norme_exp = self.prepare_macs()
##        titre = "matrice de MAC pour " + " et ".join(modeles)
##      titre+="\n"
##      if "phi_et" in modeles or "phi_num" in modeles:
##            if self.var_norme_num.get() != "Choisir" and self.var_norme_num.get() != "Aucune":
##                titre+=" Normalisation par " +self.var_norme_num.get()+ " pour les modes definis sur base numerique \n"
##        else:
##            if self.var_norme_exp.get() != "Choisir" and self.var_norme_exp.get() != "Aucune":
##                titre+=" Normailsation par " +self.var_norme_exp.get()+ " pour les modes definis sur base experimentale \n"
##
##        resu1, resu2 = self.meidee.get_res_mac( modeles,num_modes, exp_modes,norme_num,norme_exp )      
##        mac, m1, m2 = self.meidee.get_mac( modeles,num_modes, exp_modes,norme_num,norme_exp )
##
##        self.create_mac_window( titre, m1, m2, mac, resu1, resu2 )


    def quit(self):
        for term in self.term:
            if term is not None:
                term.Fermer()

    def create_mac_window(self, title, modes1, modes2, mat , resu1, resu2):
        """!Creation d'une nouvelle fenetre de visu MAC"""
        mac_win = MacWindow( self, title, modes1, modes2, mat, resu1, resu2 )
        self.mac_windows.append( mac_win )

    def destroy_mac_window(self, mac_win ):
        """!Une fenetre MAC a été détruite"""
        try:
            self.mac_windows.remove( mac_win )
        except ValueError:
            pass


