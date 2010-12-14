#@ MODIF ce_ihm_expansion Calc_essai  DATE 14/12/2010   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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

# RESPONSABLE BODEL C.BODEL


# La classe InterfaceCorrelation dirige les objets graphiques

import sys
import weakref
import os

from Utilitai.Utmess import UTMESS

import aster
from Calc_essai.cata_ce import CaraElem, InterSpectre, CalcEssaiObjects
from Calc_essai.cata_ce import Resultat, ModeMeca, DynaHarmo
from Calc_essai.modes import ParamProjMesuModal

from Accas import _F
import tkFont

from Tkinter import Frame, Menubutton, Menu, StringVar, IntVar, Listbox
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Canvas, Toplevel
from Calc_essai.outils_ihm import MyMenu, ModeFreqList
from Calc_essai.outils_ihm import DispFRFDialogue, MacWindowFrame
from Calc_essai.ce_calcul_expansion import CalcEssaiExpansion



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
                 objects,
                 macro,
                 mess,
                 param_visu):
        """!Constructeur

        :IVariable:
         - `root`: fenetre parente
         - `objects`: objet permettant d'accéder aux résultats aster existants dans le JDC
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
        self.objects = objects #     objets Aster en mémoire
        self.param_visu = param_visu
        self.is_resu1 = IntVar()
        self.is_resu2 = IntVar()
        self.use_nume_mass = IntVar()
        self.proj_champ_meth = StringVar()
        self.proj_svd_param = StringVar()
        self.calculs = CalcEssaiExpansion( macro, mess, objects)
        self.type_resu_exp = StringVar()
        self.term = []
        self.mac_windows = []
        self.afreq = None
        self.anum = None
        self.interface_main()
        self.resu_num = None   # base d'expansion (instance de ModeMeca)
        self.resu_exp = None   # donnees exp (instance de ModeMeca ou de DynaHarmo)
        self.norme_num = None  # matrice assemblee sur modele num (non obligatoire pour calcul)
        self.norme_exp = None  # idem sur modele exp



    def setup(self):
        """!Appelée par le gestionnaire de tab lors de l'affichage
            Permet de prendre en compte des nouveaux concepts
            et de les ajouter dans les MyMenu et cie..."""
        mdo = self.objects
        mdo.recup_objects
        # mise a jour des options des boutons : commande update de MyMenu :
        #les arguments sont, dans l'ordre : options, var, command
        self.menu_resu_num.update( mdo.get_mode_meca_name() , self.var_resu_num , self.num_changed )
        self.menu_resu_exp.update( mdo.get_resultats_name() , self.var_resu_exp , self.exp_changed )
        self.menu_resu1.update( mdo.get_resultats_name(), self.var_resu1, self.visu1_changed )
        self.menu_resu2.update( mdo.get_resultats_name(), self.var_resu2, self.visu2_changed )

    def teardown(self):
        """!Appelée par le gestionnaire de tab lors du masquage (passage à un autre tab)"""
        return



    def interface_main(self):
        """!Fonction principale de création de l'interface

        """
        self.columnconfigure(0, weight=1)
        self.rowconfigure(2, weight=1)
        l = Label(self,text="Expansion de donnees", pady=5, font=("Helvetica", "16") )
        l.grid(row=0)

        select_box = self.interface_selection(self)
        select_box.grid(row=1, sticky='nsew')
        
        main_param = self.interface_parametres(self)
        main_param.grid(row=2, sticky='nsew')
        
        visu_param = self.interface_visu(self)
        visu_param.grid(row=3, sticky='nsew')
        self.main = self


    def interface_selection(self, root):
        """!Creation de l'interface de selection des objets resultats"""
        f = Frame(root,relief='sunken',borderwidth=1 )
        Label(f, text="   ").grid(row=0, column=0, columnspan=3, sticky='w'+'e' )
        
        # menu de selection du resultat numerique
        Label(f,text="Base numérique d'expansion").grid(row=1,column=0,sticky='e')
        self.var_resu_num = StringVar()
        self.menu_resu_num = MyMenu( f, options = self.objects.get_mode_meca_name(),
                                     var = self.var_resu_num, cmd = self.num_changed )
        self.menu_resu_num.grid(row=1, column=1, sticky='ew')

        # menu de selection du resultat experimental
        Label(f,text="Résultat expérimental").grid(row=1,column=2,sticky='e')
        self.var_resu_exp = StringVar()
        self.menu_resu_exp = MyMenu( f, options = self.objects.get_resultats_name(),
                                     var = self.var_resu_exp, cmd = self.exp_changed )
        self.menu_resu_exp.grid(row=1, column=3, sticky='ew')

## La norme pourrait etre utilisee pour le MAC, mais elle ne l'est pas actuellement : on commente ces lignes
##        # menu de selection de la norme numerique
##        Label(f,text="Norme numérique").grid(row=2,column=0,sticky='e')
##        self.var_norme_num = StringVar()
##        self.menu_norme_num = MyMenu( f, options = self.objects.get_matr_norme(),
##                                      var = self.var_norme_num, cmd = self.num_changed )
##        self.menu_norme_num.grid(row=2, column=1, sticky='ew')
##        
##        # menu de selection de la norme experimentale
##        Label(f,text="Norme experimentale").grid(row=2,column=2,sticky='e')
##        self.var_norme_exp = StringVar()
##        self.menu_norme_exp = MyMenu( f, options = self.objects.get_matr_norme(),
##                                      var = self.var_norme_exp, cmd = self.exp_changed )
##        self.menu_norme_exp.grid(row=2, column=3, sticky='ew')

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
        mdo = self.objects
        resu_num = norme_num = None
        if self.var_resu_num.get() != 'Choisir':
            self.resu_num = mdo.get_mode_meca(self.var_resu_num.get())
            self.liste_num.set_resu(self.resu_num) # remplissage de la liste des frequences
##        if self.var_norme_num.get() != 'Choisir':
##            self.norme_num = mdo.get_matr(self.var_norme_num.get())
            

    def exp_changed(self):
        mdo = self.objects
        self.resu_exp = self.norme_exp = None
        if self.var_resu_exp.get() != 'Choisir':
            self.resu_exp = mdo.get_resultats(self.var_resu_exp.get())
            if isinstance(self.resu_exp,DynaHarmo):
                self.type_resu_exp.set('harmo')
            elif isinstance(self.resu_exp,ModeMeca):
                self.type_resu_exp.set('mode')
            self.liste_exp.set_resu(self.resu_exp) # remplissage de la liste des frequences
##        if self.var_norme_exp.get() != 'Choisir':
##            self.norme_exp = mdo.get_matr(self.var_norme_exp.get())



    def interface_parametres(self, root):
        """!Création de l'interface de choix des paramètres de calcul

        On permet à l'utilisateur de choisir les modes numériques et expérimentaux
        ainsi que la méthode de projection
        """
        self.var_expans_param_frame_visible=IntVar()
        self.export_name=StringVar()
        self.param = None
        self.suffix = ['_NX','_EX','_ET','_RD']
        listres = ['résultat numérique   extrait  NX',
                   'résultat expérimental extrait EX',
                   'résultat étendu ET',
                   'résultat réduit RD']

        f = Frame(root,relief='sunken',borderwidth=1)

        self.param_proj_mesu=ParamProjMesuModal(f, "Paramètres de PROJ_MESU_MODAL")
        self.param = self.param_proj_mesu.get_option()

        # parametres de proj_mesu_modal
        paraf = Frame(f,borderwidth = 1)    
        Label(paraf, text="Paramètres de PROJ_MESU_MODAL").grid(row=0,column=0,rowspan=2,sticky='nswe' )
        Checkbutton(paraf,text="Réglages",
                    command=self.display_expans_param_frame,
                    variable=self.var_expans_param_frame_visible,
                    indicatoron=0).grid(row=2,column=0,pady=5,sticky='e')
        paraf.grid(row=0,column=0,sticky='ew',pady=10,padx=10 )

        #lancer le calcul
        launchf =  Frame(f,borderwidth = 1)
        Button(launchf,text='Calculer',command=self.prepare_calcul).grid(row=2,column=0,sticky='w')
        Entry(launchf,textvariable=self.export_name, bg='white' ).grid(row=2,column=1,pady=2)
        launchf.grid(row=1,column=0,sticky='ew',pady=10,padx=10 )
        
        
        self.liste_num = ModeFreqList( f, "Modes Numériques" )
        self.liste_num.grid(row=0, column=3, rowspan=3, sticky='nsew',pady=10,padx=10  )
        self.liste_exp = ModeFreqList( f, "Modes Expérimentaux" )
        self.liste_exp.grid(row=0, column=4, rowspan=3, sticky='nsew',pady=10,padx=10  )
        
        f.grid(row=1, sticky='ew')
        return f

    def display_expans_param_frame(self):
        """Affichage d'une fenetre pour reglage de PROJ_MESU_MODAL"""
        self.expans_param_frame = frm1 = Toplevel()
        frm1.rowconfigure(0,weight=1)
        frm1.columnconfigure(0,weight=1)
        self.param_proj_mesu = ParamProjMesuModal(frm1, "Paramètres de PROJ_MESU_MODAL")
        self.param_proj_mesu.grid(row=0,column=0,sticky='nsew')
        if self.param:
            self.param_proj_mesu.set_option(self.param)
            self.param_proj_mesu.select_option()
        state = self.var_expans_param_frame_visible.set(1)
        frm1.protocol("WM_DELETE_WINDOW", self.hide_expans_param_frame)
        Button(frm1,text="OK",command=self.hide_expans_param_frame).grid(row=1,column=0)


    def hide_expans_param_frame(self):
        """Fermer la fenetre de reglage"""
        self.var_expans_param_frame_visible.set(0)
        self.expans_param_frame.withdraw()
        self.param = self.param_proj_mesu.get_option()

    def interface_visu(self, root):
        """!Création de l'interface de visualisation

        permet de choisir d'afficher les matrices MAC ou les modes avec gmsh
        gère la compatibilité des choix de l'utilisateur (les calculs de MAC
        ne sont pas tous possibles)
        """
        mdo = self.objects
        f = Frame(root,relief='sunken', borderwidth=1 )
        Label(f, text="   ").grid(row=0, column=1,columnspan = 3,sticky='w'+'e' )
        Label(f, text="   ").grid(row=2, column=1,columnspan = 3,sticky='w'+'e' )
        f.columnconfigure(0,weight=3)
        f.columnconfigure(1,weight=3)
        

        f1 = Frame(f)
        f1.grid(row=1,column=0,sticky='ew' )
        f1.columnconfigure(1,weight=4)
        f1.columnconfigure(2,weight=4)
        
        bir1 = Checkbutton(f1,variable=self.is_resu1, command=self.cb_changed)
        bir1.grid(row=0, column = 0,sticky='e',padx=20)
        bir2 = Checkbutton(f1,variable=self.is_resu2,command=self.cb_changed)
        bir2.grid(row=1, column = 0,sticky='e',padx=20)

        Label(f1,text="Resultat 1").grid(row=0,column=1,sticky='w')
        self.var_resu1 = StringVar()
        self.menu_resu1 = MyMenu( f1, options = mdo.get_resultats_name(),
                                  var = self.var_resu1, cmd = self.visu1_changed )
        self.menu_resu1.grid(row=0, column=2, sticky='ew',padx=20)

        Label(f1,text="Resultat 2").grid(row=1,column=1,sticky='w')
        self.var_resu2 = StringVar()
        self.menu_resu2 = MyMenu( f1, options = mdo.get_resultats_name(),
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


    def check_state(self):
        """Verifie la compatibilite des bases pour le MAC et l'existence
           des donnees necessaires pour la visu des deformees et des FRF"""
        mdo = self.objects
   
        # Y a-t-il un MAC a calculer ?
        if (self.is_resu1.get() and not self.is_resu2.get()) or (self.is_resu2.get() and not self.is_resu1.get()):
            self.mac_button.configure(state='normal')
        elif self.is_resu1.get() and self.is_resu2.get():
            resu1 = mdo.get_resultats(self.var_resu1.get())
            resu2 = mdo.get_resultats(self.var_resu2.get())
            if resu1.modele_name.strip() and resu1.modele_name == resu2.modele_name:
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
        mdo = self.objects
        resu1 = None
        resu2 = None
        if self.is_resu1.get():
            resu1 = mdo.get_resultats(self.var_resu1.get())
        if self.is_resu2.get():
            resu2 = mdo.get_resultats(self.var_resu2.get())
        fenetre = DispFRFDialogue(self.mess, self.objects, self.param_visu, resu1, resu2)
            

    def view_modes(self, *args):
        """!Visualisation des modes par GMSH ou Salome
        """
        mdo = self.objects
        l_resultat = []
        l_modele = []
        if self.is_resu1.get():
            resu1 = mdo.get_resultats(self.var_resu1.get())
            
            l_resultat.append(resu1.obj)
        if self.is_resu2.get():
            resu2 = mdo.get_resultats(self.var_resu2.get())
            l_resultat.append(resu2.obj)
        term = self.param_visu.visu_resu(resultat=l_resultat)
        
        self.term.append(term)


    def view_macs(self):
        """!Creation d'une nouvelle fenetre de visu MAC"""
        mdo = self.objects
        resu1 = None
        resu2 = None
        if self.is_resu1.get() and self.is_resu2.get():
            resu1 = mdo.get_resultats(self.var_resu1.get())
            resu2 = mdo.get_resultats(self.var_resu2.get())
        elif self.is_resu1.get():
            resu1 = mdo.get_resultats(self.var_resu1.get())
            resu2 = mdo.get_resultats(self.var_resu1.get())
        elif self.is_resu2.get():
            resu1 = mdo.get_resultats(self.var_resu2.get())
            resu2 = mdo.get_resultats(self.var_resu2.get())

        mac = self.calculs.calc_mac_mode( resu1, resu2, norme = None )

        titre = "matrice de MAC pour " + resu1.nom + " et " + resu2.nom

        f = Toplevel()
        size = (20,300)
        f.columnconfigure(0,weight=1)
        f.rowconfigure(0,weight=1)
        mac_win = MacWindowFrame( f, titre, resu1.nom, resu2.nom, size)
        mac_win.grid(row=0,column=0,sticky='nsew')
        afreq1 = resu1.get_modes_data()['FREQ']
        neud_cmp1 = resu1.get_modes_data()['NOEUD_CMP']
        afreq2 = resu2.get_modes_data()['FREQ']
        neud_cmp2 = resu2.get_modes_data()['NOEUD_CMP']
        # si mode statique, on donne le champ NOEUD_CMP a la place de la frequence
        for ind_ordr in range(len(afreq1)):
            if afreq1[ind_ordr]==None:afreq1[ind_ordr]=neud_cmp1[ind_ordr]
        for ind_ordr in range(len(afreq2)):
            if afreq2[ind_ordr]==None:afreq2[ind_ordr]=neud_cmp2[ind_ordr]
        mac_win.set_modes(afreq1, afreq2, mac)

        self.mac_windows.append( mac_win )

            
    def prepare_calcul(self, *args):
        """! Demande le lancement de la macro MACRO_EXPANS dans calc_proj_resu
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
        param =  self.param_proj_mesu.get_resolution()

        self.calculs.setup( self.resu_num, self.modes_num_list,
                           self.resu_exp, self.modes_exp_list,
                           param)

        self.calculs.calc_proj_resu(self.suffix,self.export_name.get())
        self.setup()



    def quit(self):
        for term in self.term:
            if term is not None:
                term.Fermer()


