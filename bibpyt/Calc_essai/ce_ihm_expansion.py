# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: albert.alarcon at edf.fr


# La classe InterfaceCorrelation dirige les objets graphiques

import sys
import weakref
import os

from Utilitai.Utmess import UTMESS

import aster
from Calc_essai.cata_ce import CaraElem, InterSpectre, CalcEssaiObjects
from Calc_essai.cata_ce import Resultat, ModeMeca, DynaHarmo
from Calc_essai.outils_ihm import ParamProjMesuModal

from Accas import _F
import tkFont

from Tkinter import Frame, Menubutton, Menu, StringVar, IntVar, Listbox
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Canvas, Toplevel
from Calc_essai.outils_ihm import MyMenu, ModeFreqList
from Calc_essai.outils_ihm import DispFRFDialogue, MacWindowFrame, DispObs
from Calc_essai.ce_calcul_expansion import CalcEssaiExpansion
from Calc_essai.ce_ihm_parametres import CalcEssaiSalome


#
#
# CLASSES GRAPHIQUES  #
#
#


#-------------------------------------------------------------------------


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

        Frame.__init__(
            self, root, relief='flat', borderwidth=4)  # Première frame
        self.mess = mess
        self.root = root
        self.macro = macro
        self.objects = objects  # objets Aster en mémoire
        self.param_visu = param_visu
        self.is_resu1 = IntVar()
        self.is_resu2 = IntVar()
        self.use_nume_mass = IntVar()
        self.proj_champ_meth = StringVar()
        self.proj_svd_param = StringVar()
        self.calculs = CalcEssaiExpansion(macro, mess, objects)
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
        mdo.recup_objects()
        # mise a jour des options des boutons : commande update de MyMenu :
        # les arguments sont, dans l'ordre : options, var, command
        self.menu_resu_num.update(
            mdo.get_mode_meca_name(), self.var_resu_num, self.num_changed)
        self.menu_resu_exp.update(
            mdo.get_resultats_name(), self.var_resu_exp, self.exp_changed)
        self.menu_resu1.update(
            mdo.get_resultats_name(), self.var_resu1, self.visu1_changed)
        self.menu_resu2.update(
            mdo.get_resultats_name(), self.var_resu2, self.visu2_changed)

    def teardown(self):
        """!Appelée par le gestionnaire de tab lors du masquage (passage à un autre tab)"""
        return

    def interface_main(self):
        """!Fonction principale de création de l'interface

        """
        self.columnconfigure(0, weight=1)
        self.rowconfigure(2, weight=1)
        l = Label(self, text=u"Expansion de données ",
                  pady=5, font=("Helvetica", "16"))
        l.grid(row=0)

        select_box = self.interface_selection(self)
        select_box.grid(row=1, sticky='nsew')

        main_param = self.interface_parametres(self)
        main_param.grid(row=2, sticky='nsew')

    def interface_selection(self, root):
        """!Creation de l'interface de selection des objets resultats"""
        f = Frame(root, relief='sunken', borderwidth=1)
        Label(f, text="   ").grid(
            row=0, column=0, columnspan=3, sticky='w' + 'e')

        # menu de selection du resultat numerique
        Label(f, text=u"Base numérique d'expansion").grid(
            row=1, column=0, sticky='e')
        self.var_resu_num = StringVar()
        self.menu_resu_num = MyMenu(
            f, options=self.objects.get_mode_meca_name(),
            var=self.var_resu_num, cmd=self.num_changed)
        self.menu_resu_num.grid(row=1, column=1, sticky='ew')

        # menu de selection du resultat experimental
        Label(f, text=u"Résultat expérimental").grid(
            row=1, column=2, sticky='e')
        self.var_resu_exp = StringVar()
        self.menu_resu_exp = MyMenu(
            f, options=self.objects.get_resultats_name(),
            var=self.var_resu_exp, cmd=self.exp_changed)
        self.menu_resu_exp.grid(row=1, column=3, sticky='ew')

        self.var_resu1 = StringVar()
        self.menu_resu1 = MyMenu(
            f, options=self.objects.get_resultats_name(),
            var=self.var_resu1, cmd=self.visu1_changed)

        self.var_resu2 = StringVar()
        self.menu_resu2 = MyMenu(
            f, options=self.objects.get_resultats_name(),
            var=self.var_resu2, cmd=self.visu2_changed)

# La norme pourrait etre utilisee pour le MAC, mais elle ne l'est pas actuellement : on commente ces lignes
# menu de selection de la norme numerique
# Label(f,text="Norme numérique").grid(row=2,column=0,sticky='e')
# self.var_norme_num = StringVar()
# self.menu_norme_num = MyMenu( f, options = self.objects.get_matr_norme(),
# var = self.var_norme_num, cmd = self.num_changed )
# self.menu_norme_num.grid(row=2, column=1, sticky='ew')
#
# menu de selection de la norme experimentale
# Label(f,text="Norme experimentale").grid(row=2,column=2,sticky='e')
# self.var_norme_exp = StringVar()
# self.menu_norme_exp = MyMenu( f, options = self.objects.get_matr_norme(),
# var = self.var_norme_exp, cmd = self.exp_changed )
# self.menu_norme_exp.grid(row=2, column=3, sticky='ew')
        # Type de resu experimental (dyna_harmo ou modes)
        Label(f, text=u"Type de résultat expérimental").grid(
            row=1, column=4, columnspan=2)
        Radiobutton(f, text=u"résultat harmonique", value='harmo',
                    variable=self.type_resu_exp).grid(row=2, column=4)
        Radiobutton(f, text=u"modes", value='mode',
                    variable=self.type_resu_exp).grid(row=2, column=5)

        Label(f, text="   ").grid(
            row=3, column=0, columnspan=3, sticky='w' + 'e')
        f.columnconfigure(0, weight=1)
        f.columnconfigure(1, weight=4)
        f.columnconfigure(3, weight=4)

        return f

    """ Deux routines pour mettre a jour les donnees, sd_resus et normes"""

    def num_changed(self):
        mdo = self.objects
        resu_num = norme_num = None
        if self.var_resu_num.get() != "Choisir":
            self.resu_num = mdo.get_mode_meca(self.var_resu_num.get())
            self.liste_num.set_resu(
                self.resu_num)  # remplissage de la liste des frequences
# if self.var_norme_num.get() != 'Choisir':
# self.norme_num = mdo.get_matr(self.var_norme_num.get())

    def exp_changed(self):
        mdo = self.objects
        self.resu_exp = self.norme_exp = None
        if self.var_resu_exp.get() != "Choisir":
            self.resu_exp = mdo.get_resultats(self.var_resu_exp.get())
            if isinstance(self.resu_exp, DynaHarmo):
                self.type_resu_exp.set('harmo')
            elif isinstance(self.resu_exp, ModeMeca):
                self.type_resu_exp.set('mode')
            self.liste_exp.set_resu(
                self.resu_exp)  # remplissage de la liste des frequences
# if self.var_norme_exp.get() != 'Choisir':
# self.norme_exp = mdo.get_matr(self.var_norme_exp.get())

    def interface_parametres(self, root):
        """!Création de l'interface de choix des paramètres de calcul

        On permet à l'utilisateur de choisir les modes numériques et expérimentaux
        ainsi que la méthode de projection
        """
        self.var_expans_param_frame_visible = IntVar()
        self.export_name = StringVar()
        self.param = None
        self.suffix = ['_NX', '_EX', '_ET', '_RD']
        listres = ["résultat numérique   extrait  NX",
                   "résultat expérimental extrait EX",
                   "résultat étendu ET",
                   "résultat réduit RD"]

        f = Frame(root, relief='sunken', borderwidth=1)

        self.param_proj_mesu = ParamProjMesuModal(
            f, u"Paramètres de PROJ_MESU_MODAL")
        self.param = self.param_proj_mesu.get_option()

        # parametres de proj_mesu_modal
        paraf = Frame(f, borderwidth=1)
        Label(paraf, text=u"Paramètres de PROJ_MESU_MODAL").grid(
            row=0, column=0, rowspan=2, sticky='nswe')
        Checkbutton(paraf, text=u"Réglages",
                    command=self.display_expans_param_frame,
                    variable=self.var_expans_param_frame_visible,
                    indicatoron=0).grid(row=2, column=0, pady=5, sticky='e')
        paraf.grid(row=0, column=0, sticky='ew', pady=10, padx=10)

        # lancer le calcul
        launchf = Frame(f, borderwidth=1)
        Button(launchf, text="Calculer", command=self.prepare_calcul).grid(
            row=2, column=0, sticky='w')
        Entry(launchf, textvariable=self.export_name,
              bg='white').grid(row=2, column=1, pady=2)
        launchf.grid(row=1, column=0, sticky='ew', pady=10, padx=10)

        self.liste_num = ModeFreqList(f, u"Modes Numériques")
        self.liste_num.grid(
            row=0, column=3, rowspan=3, sticky='nsew', pady=10, padx=10)
        self.liste_exp = ModeFreqList(f, u"Modes Expérimentaux")
        self.liste_exp.grid(
            row=0, column=4, rowspan=3, sticky='nsew', pady=10, padx=10)

        f.grid(row=1, sticky='ew')
        return f

    def display_expans_param_frame(self):
        """Affichage d'une fenetre pour reglage de PROJ_MESU_MODAL"""
        self.expans_param_frame = frm1 = Toplevel()
        frm1.rowconfigure(0, weight=1)
        frm1.columnconfigure(0, weight=1)
        self.param_proj_mesu = ParamProjMesuModal(
            frm1, u"Paramètres de PROJ_MESU_MODAL")
        self.param_proj_mesu.grid(row=0, column=0, sticky='nsew')
        if self.param:
            self.param_proj_mesu.set_option(self.param)
            self.param_proj_mesu.select_option()
        state = self.var_expans_param_frame_visible.set(1)
        frm1.protocol("WM_DELETE_WINDOW", self.hide_expans_param_frame)
        Button(frm1, text="OK", command=self.hide_expans_param_frame).grid(
            row=1, column=0)

    def hide_expans_param_frame(self):
        """Fermer la fenetre de reglage"""
        self.var_expans_param_frame_visible.set(0)
        self.expans_param_frame.withdraw()
        self.param = self.param_proj_mesu.get_option()

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

    def prepare_calcul(self, *args):
        """! Demande le lancement de la macro MACRO_EXPANS dans calc_proj_resu
        """
        # Validité des donnees :
        if self.resu_num == None or self.resu_exp == None:
            self.mess.disp_mess(u"Il manque des données pour le calcul")
            return
        if self.resu_num.modele == None:
            self.mess.disp_mess(
                u"Il manque le modele associe au résultat numérique")
            return
        if self.resu_exp.modele == None:
            self.mess.disp_mess(
                u"Il manque le modele associe au résultat expérimental")
            return

        self.modes_num_list = self.liste_num.get_selection()
        self.modes_exp_list = self.liste_exp.get_selection()
        param = self.param_proj_mesu.get_resolution()

        self.calculs.setup(self.resu_num, self.modes_num_list,
                           self.resu_exp, self.modes_exp_list,
                           param)

        self.calculs.calc_proj_resu(self.suffix, self.export_name.get())
        self.setup()

    def quit(self):
        for term in self.term:
            if term is not None:
                term.Fermer()
