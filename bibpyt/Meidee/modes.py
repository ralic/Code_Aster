#@ MODIF modes Meidee  DATE 03/11/2008   AUTEUR BODEL C.BODEL 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
# La classe ModifStruct permet de gerer les calculs de modification structurale
# 

from Tkinter import *

import aster
import string
import Numeric
from Meidee.meidee_iface import MyMenu, MacMode, VecteurEntry
from Meidee.meidee_cata import DynaHarmo
from Accas import _F, ASSD

from Cata.cata import IMPR_RESU, MAC_MODES, DETRUIRE, DEFI_GROUP
from Cata.cata import MODE_STATIQUE,DEFI_BASE_MODALE,PROJ_MESU_MODAL,MACR_ELEM_STAT
from Cata.cata import NUME_DDL_GENE,PROJ_MATR_BASE,MODE_ITER_SIMULT
from Cata.cata import EXTR_MODE, DEFI_MAILLAGE, ASSE_MAILLAGE, AFFE_MODELE
from Cata.cata import INFO_EXEC_ASTER, DEFI_FICHIER, CO, MACRO_EXPANS
from Cata.cata import AFFE_CARA_ELEM, AFFE_MATERIAU, AFFE_CHAR_MECA
from Cata.cata import CALC_MATR_ELEM, NUME_DDL, ASSE_MATRICE, DEPL_INTERNE

try:
    set
except NameError:
    from sets import Set as set

def txtfield( root, title, var, row ):
    Label(root,text=title).grid(row=row,column=0)
    e=Entry(root)
    e.grid(row=row,column=1,textvariable=var)
    return e

class ModeList(Frame): # pure Tk -> testable hors aster
    """Interface permettant de selectionner les modes
    d'unes structure sd_resultat_dyn
    """
    def __init__(self, root, title):
        Frame.__init__(self, root)
        self.title = title
        self.resu = None # la sd resultat sélectionnée
        self.modes = []  # la liste de modes
        self.rowconfigure(1,weight=1)
        Label(self, text=title, bg="#f0f0f0").grid(column=0,row=0,columnspan=2,sticky="ew")
        scroll = Scrollbar ( self, orient='vertical' )
        scroll.grid ( row=1, column=1, sticky='ns' )
        lst = Listbox(self,
                      selectmode='extended',
                      yscrollcommand=scroll.set,
                      exportselection=False,
                      font=("Courier","12"),
#                      width=10, height=20,
                      background='white'
                      )
        lst.grid(column=0, row=1, sticky='sn')
        self.columnconfigure(0,weight=0)
        scroll["command"] = lst.yview
        self.modes_list = lst
        buttonbar = Frame(self,bg='blue')
        buttonbar.grid(row=2,column=0,columnspan=2)
        Button(buttonbar, text="Tout", command=self.select_all).grid(row=0,column=0)
        Button(buttonbar, text="Rien", command=self.deselect_all).grid(row=0,column=1)

    def select_all(self):
        self.modes_list.selection_set(0,END)

    def deselect_all(self):
        self.modes_list.selection_clear(0,END)

    def fill_modes(self, modes ):
        """! Remplit une liste de modes avec les numéros/item passés
             en paramètres modes une liste de (numero, item)
        """
        self.modes_list.delete(0,END)
        self.modes = modes
        self.values = []
        for val, label in modes:
            self.modes_list.insert( END, val +" "+ label )
            self.values.append( val )

    def selection(self):
        """!Renvoie les valeurs associées à la sélection des modes"""
        return [int(self.modes[int(v)][0]) for v in self.modes_list.curselection() ]


class ModeFreqList(ModeList):
    """!Permet de créer une liste de modes contenues dans un sd_resultat.
        Valable pour mode_meca, base_modale (a verifier ??) et dyna_harmo"""
    def __init__(self, root, title=None):
        if title is None:
            title="Choisissez les modes"
        ModeList.__init__(self, root, title)

    def set_resu(self, resu):
        para = resu.LIST_PARA()
        modes = para['NUME_ORDRE']
        freq = para['FREQ']
        self.fill_modes( [ ('%3i' % n, '%8.2f Hz' % f) for n,f in zip(modes,freq) ] )


class ModeHarmoList(ModeList):
    """!liste de numeros d'ordre plutot pour les dyna_harmo"""
    pass


class GroupNoList(ModeList):
    """!Crée une liste de groupe de noeuds à partir d'un maillage"""
    def __init__(self, root):
        if title is None:
            title="Choisissez les groupes de noeuds"
        ModeList.__init__(self, root, title)

    def set_mesh(self, mail):
        groupno = mail.GROUPENO.get()
        self.fill_modes( zip( groupno.keys(), groupno.keys() ) )

class GroupMaList(ModeList):
    """!Crée une liste de groupe de noeuds à partir d'un maillage"""
    def __init__(self, root):
        ModeList.__init__(self, root, "Choisissez les groupes de mailles")

    def set_mesh(self, mail):
        groupno = mail.GROUPEMA.get()
        self.fill_modes( zip( groupno.keys(), groupno.keys() ) )



class OptionFrame(Frame):
    def __init__(self, root, title, desc):
        Frame.__init__(self, root, relief=GROOVE,bd=2)
        r=0
        self.widgets = []
        if title:
            Label(self, text=title).grid(row=r,column=0,columnspan=2)
            r+=1
        for label, typ, extra in desc:
            Label(self, text=label).grid(row=r, column=0)
            w=typ(self, **extra)
            w.grid(row=r, column=1)
            self.widgets.append(w)
            r+=1
            

class ParamModeIterSimult(Frame):
    """Un panneau pour spécifier les paramètres de résolution
    de MODE_ITER_SIMULT
    """
    # Note: réutiliser la partie générique d'Eficas est plutot
    # compliqué et donne des interfaces peu 'intuitives'...

    # On ne traite que SORENSEN pour l'instant
    # (JACOBI et TRI_DIAG ont peu d'avantages d'après U4.52.03-H
    def __init__(self, root, title, **kwargs):
        Frame.__init__(self, root, **kwargs)
        Label(self, text=title).grid(row=0, column=0, columnspan=1)


        self.precision = DoubleVar()
        self.iter = IntVar()
        self.para_ortho = DoubleVar()
        self.precision.set(0.0)
        self.iter.set(20)
        self.para_ortho.set(0.717)
        self.sorensen = OptionFrame(self, "Méthode SORENSEN",
                                    [ ("Précision (SORENSEN)",Entry,
                                       { 'textvariable':self.precision } ),
                                      ("Nbmax itérations",Entry,
                                       { 'textvariable':self.iter } ),
                                      ("Coef d'orthogonalisation",Entry,
                                       { 'textvariable':self.para_ortho } ),
                                      ])
        

        self.sorensen.grid(row=1,column=0,sticky="WE",columnspan=2)
        Label(self,text="Option").grid(row=2,column=0)
        self.opt_option = StringVar()
        # variables for the different panels
        #self.opt_centre_amor_reduit = DoubleVar() # utile?
        self.opt_nmax_freq = IntVar() # PLUS_PETITE, BANDE
        self.opt_nmax_freq.set( 10 )
        self.opt_freq1 = DoubleVar() # CENTRE, BANDE
        self.opt_freq2 = DoubleVar() # BANDE

        self.option = MyMenu(self, ("CENTRE","BANDE","PLUS_PETITE"),
                             self.opt_option, self.select_option )
        self.option.grid(row=2,column=1)
        self.opt_option.set("PLUS_PETITE")
        self.opt_panel = None
        self.select_option()
        # Post traitement
        self.opt_seuil_freq = DoubleVar()
        self.opt_seuil_freq.set( 1e-4 )
        self.post = OptionFrame(self, "Post traitement",
                                [("Seuil freq.",Entry,
                                  {'textvariable':self.opt_seuil_freq}),
                                 ])

    def select_option(self, *args):
        opt = self.opt_option.get()
        if self.opt_panel:
            self.opt_panel.destroy()
        if opt == "CENTRE":
            panel = OptionFrame(self, None, [
                ("Fréquence",Entry,{'textvariable':self.opt_freq1}),
                ("NMax (-1 pour tout)",Entry,{'textvariable':self.opt_nmax_freq}),
                ])
        elif opt == "BANDE":
            panel = OptionFrame(self, None, [
                ("Fréquence min",Entry,{'textvariable':self.opt_freq1}),
                ("Fréquence max",Entry,{'textvariable':self.opt_freq2}),
                ])
        elif opt == "PLUS_PETITE":
            panel = OptionFrame(self, None, [
                ("Nmax",Entry,{'textvariable':self.opt_nmax_freq}),
                ])
        else:
            raise RuntimeError("Unknown option '%s'" % opt)
        self.opt_panel = panel
        panel.grid(row=3,column=0,columnspan=2,sticky="WE")
        

    def get_calc_freq(self):
        opt = self.opt_option.get()
        mc = _F(OPTION=opt)
        mc['SEUIL_FREQ']=self.opt_seuil_freq.get()
        if opt=="PLUS_PETITE":
            mc['NMAX_FREQ']=self.opt_nmax_freq.get()
        elif opt=="BANDE":
            mc['FREQ']=(self.opt_freq1.get(),self.opt_freq2.get())
        elif opt=="CENTRE":
            mc['FREQ']=self.opt_freq1.get()
            mc['NMAX_FREQ']=self.opt_nmax_freq.get()


        return mc



class ParamModeIterInv(Frame):
    """Un panneau pour spécifier les paramètres de résolution
    de MODE_ITER_INV (type_resu='dynamique')
    """
    def __init__(self, root, title, **kwargs):
        Frame.__init__(self, root, **kwargs)
        Label(self, text=title).grid(row=0, column=0, columnspan=1)

        self.frequences = []
        self.opt_option = StringVar()
        self.nmax_freq = IntVar()
        self.nmax_freq.set( 0 )
        self.freq = StringVar()
        self.nmax_iter_separe = IntVar()    # SEPARE ou AJUSTE
        self.nmax_iter_separe.set( 30 )
        self.prec_separe = DoubleVar()
        self.prec_separe.set( 1e-4 )
        self.nmax_iter_ajuste = IntVar()    # AJUSTE
        self.nmax_iter_ajuste.set( 15 )
        self.prec_ajuste = DoubleVar()
        self.prec_ajuste.set( 1e-4 )
        self.opt_freq1 = DoubleVar() # SEPARE, AJUSTE
        self.opt_freq2 = DoubleVar()


        Label(self,text="Option").grid(row=2,column=0)
        self.option = MyMenu(self, ("PROCHE","SEPARE","AJUSTE"),
                             self.opt_option, self.select_option )
        self.opt_option.set("AJUSTE")
        self.option.grid(row=2,column=1)
        Label(self,text="NMAX_FREQ").grid(row=3,column=0)
        Entry(self,textvariable=self.nmax_freq).grid(row=3,column=1)

        self.opt_panel = None
        self.select_option()


    def select_option(self, *args):
        opt = self.opt_option.get()
        if self.opt_panel:
            self.opt_panel.destroy()
        if opt == "PROCHE":
            panel = ModeList(self, "Fréquences proches")
            panel.fill_modes( [ (f, "% 6.2f Hz"%f) for f in self.frequences ] )
        elif opt == "SEPARE":
            panel = OptionFrame(self, None, [
                ("Fréquence min",Entry,{'textvariable':self.opt_freq1}),
                ("Fréquence max",Entry,{'textvariable':self.opt_freq2}),
                ("NMAX iter separe",Entry,{'textvariable':self.nmax_iter_separe}),
                ("Precision separe",Entry,{'textvariable':self.prec_separe}),
                ])
        elif opt == "AJUSTE":
            panel = OptionFrame(self, None, [
                ("Fréquence min",Entry,{'textvariable':self.opt_freq1}),
                ("Fréquence max",Entry,{'textvariable':self.opt_freq2}),
                ("NMAX iter separe",Entry,{'textvariable':self.nmax_iter_separe}),
                ("Precision separe",Entry,{'textvariable':self.prec_separe}),
                ("NMAX iter ajuste",Entry,{'textvariable':self.nmax_iter_ajuste}),
                ("Precision ajuste",Entry,{'textvariable':self.prec_ajuste}),
                ])
        else:
            raise RuntimeError("Unknown option '%s'" % opt)
        self.opt_panel = panel
        panel.grid(row=4,column=0,columnspan=2,sticky="WE")
        

    def get_calc_freq(self):
        opt = self.opt_option.get()
        mc = _F(OPTION=opt)
        mc['NMAX_FREQ']=self.nmax_freq.get()
        if opt=="PROCHE":
            mc['FREQ']=self.panel.selection()
        elif opt=="SEPARE":
            mc['FREQ']=(self.opt_freq1.get(),self.opt_freq2.get())
            mc['NMAX_ITER_SEPARE'] = self.nmax_iter_separe.get()
            mc['PREC_SEPARE'] = self.prec_separe.get()
        elif opt=="AJUSTE":
            mc['FREQ']=(self.opt_freq1.get(),self.opt_freq2.get())
            mc['NMAX_ITER_SEPARE'] = self.nmax_iter_separe.get()
            mc['PREC_SEPARE'] = self.prec_separe.get()
            mc['NMAX_ITER_AJUSTE'] = self.nmax_iter_ajuste.get()
            mc['PREC_AJUSTE'] = self.prec_ajuste.get()
        return mc

class ParamProjMesuModal(Frame):
    """Un panneau pour spécifier les paramètres de résolution
    de PROJ_MESU_MODAL
    """
    
    def __init__(self, root, title, **kwargs):
        Frame.__init__(self, root, **kwargs)
        Label(self, text=title).grid(row=0, column=0, columnspan=1)


        self.methode = StringVar()
        self.eps = DoubleVar()
        self.regul = DoubleVar()
        self.regul_meth = StringVar()
        self.regul_meth.set("NON")
        self.regul.set(1.0e-5)
        self.regul.set(0.0)        

        Label(self,text="Option").grid(row=2,column=0)
        self.opt_option = StringVar()

        self.option = MyMenu(self, ("SVD","LU"),
                             self.opt_option, self.select_option )
        self.option.grid(row=2,column=1)
        self.opt_option.set("SVD")
        self.opt_panel = None
        self.select_option()

    def select_option(self, *args):
        opt = self.opt_option.get()
        if self.opt_panel:
            self.opt_panel.destroy()
        if opt == "SVD":
            regul_opt = ['NON','NORM_MIN','TIK_RELA']
            panel = OptionFrame(self, None, [
                ("eps",Entry,{'textvariable':self.eps}),
                ("regularisation",MyMenu,{'var':self.regul_meth,
                                          'options':regul_opt}),
                ("ponderation",Entry,{'textvariable':self.regul}),
                ])
            self.regul_meth.set('NON')
        elif opt == "LU":
            panel = Label(self,text="Pas de paramétrage")            
        else:
            raise RuntimeError("Unknown option '%s'" % opt)
        self.opt_panel = panel
        panel.grid(row=3,column=0,columnspan=2,sticky="WE")

    def get_resolution(self):
        mc = {}
        opt = self.opt_option.get()
        if opt == 'LU':
            mc['METHODE'] = 'LU'
        elif opt == 'SVD':
            mc['METHODE'] = 'SVD'
            mc['REGUL'] = self.regul_meth.get()
            if mc['REGUL'] != 'NON':
                mc['COEF_PONDER'] = self.regul.get()

        return mc

class InitParamRecalS0(Frame):
    """Un panneau pour specifier les valeurs initiales des parametres 
    A, PULSc, BETA et MU pour l'interpolation de la fonction S0
    par la forme analytique attendue
    """
    def __init__(self, root, title, **kwargs):
        Frame.__init__(self, root, **kwargs)
        Label(self, text=title).grid(row=0, column=0, columnspan=1)


        self.A = DoubleVar()
        self.Amin = DoubleVar()
        self.Amax = DoubleVar()
        self.PULSC = DoubleVar()
        self.PULSCmin = DoubleVar()
        self.PULSCmax = DoubleVar()
        self.BETA = DoubleVar()
        self.BETAmin = DoubleVar()
        self.BETAmax = DoubleVar()
        self.MU = DoubleVar()
        self.MUmin = DoubleVar()
        self.MUmax = DoubleVar()
        self.p1 = DoubleVar()
        self.p2 = DoubleVar()

        self.A.set(1.0e+11)
        self.Amin.set(1.0e+8)
        self.Amax.set(1.0e+14)
        self.PULSC.set(100.0)
        self.PULSCmin.set(50.0)
        self.PULSCmax.set(150.0)
        self.BETA.set(10.0)
        self.BETAmin.set(5.0)
        self.BETAmax.set(15.0)
        self.MU.set(0.025)
        self.MUmin.set(0.01)
        self.MUmax.set(0.05)        
        self.p1.set(1.0)
        self.p2.set(0.1)

        self.opt_panel = None
        self.opt_poids = None

        panel = OptionFrame(self, None, [
                ("A",Entry,{'textvariable':self.A}),
                ("Valeur minimale de A - ",Entry,{'textvariable':self.Amin}),
                ("Valeur maximale de A",Entry,{'textvariable':self.Amax}),
                ("OMEGAc",Entry,{'textvariable':self.PULSC}),
                ("Valeur minimale de OMEGAc",Entry,{'textvariable':self.PULSCmin}),
                ("Valeur maximale de OMEGAc",Entry,{'textvariable':self.PULSCmax}),
                ("BETA",Entry,{'textvariable':self.BETA}),
                ("Valeur minimale de BETA",Entry,{'textvariable':self.BETAmin}),
                ("Valeur maximale de BETA",Entry,{'textvariable':self.BETAmax}),
                ("MU",Entry,{'textvariable':self.MU}),
                ("Valeur minimale de MU",Entry,{'textvariable':self.MUmin}),
                ("Valeur maximale de MU",Entry,{'textvariable':self.MUmax}),
                ])

        self.opt_panel = panel
        panel.grid(row=1,column=0,columnspan=2,sticky="WE")

        Label(self,text="POIDS A AFFECTER").grid(row=2,column=0)

        poids = OptionFrame(self, None, [
                ("Poids à affecter au mode fondamental",Entry,{'textvariable':self.p1}),
                ("Poids à affecter aux modes supplémentaires (EML)",Entry,{'textvariable':self.p2}),
                ])
        self.opt_poids = poids
        poids.grid(row=3,column=0,columnspan=2,sticky="WE")

    def get_resolution(self):
        mc = {}
        mc['A'] = self.A.get()
        mc['Amin'] = self.Amin.get()
        mc['Amax'] = self.Amax.get()
        mc['PULSC'] = self.PULSC.get()
        mc['PULSCmin'] = self.PULSCmin.get()
        mc['PULSCmax'] = self.PULSCmax.get()
        mc['BETA'] = self.BETA.get()
        mc['BETAmin'] = self.BETAmin.get()
        mc['BETAmax'] = self.BETAmax.get()
        mc['MU'] = self.MU.get()
        mc['MUmin'] = self.MUmin.get()
        mc['MUmax'] = self.MUmax.get()
        mc['poids1'] = self.p1.get()
        mc['poids2'] = self.p2.get()
        return mc


class RowDict(dict):
    """Un dictionaire utilisé pour décrire
    chaque group de DDL.
    """

    def __init__(self, *args, **kargs):
        dict.__init__(self, *args, **kargs)
        self.chgt_repere_choix = StringVar()
        self.chgt_repere_choix.set("AUCUN")
    
    def set_chgt_rep(self, chgt_rep):
        """Garde en référence une instance de `ChgtRepereDialogue'."""
        self.chgt_rep = chgt_rep

    def affiche_chgt_rep(self):
        """Affiche l'interface de changement de repère."""
        self.chgt_rep.affiche(self)


class ChgtRepereDialogue(Toplevel):
    """L'interface offrant un changement de repère.
    """

    def __init__(self, mess, type_sel=None):
        Toplevel.__init__(self)
        self.withdraw()
        self.protocol("WM_DELETE_WINDOW", self.hide)
        self.dialog_titre = StringVar()
        self._build_interface()

        self.mess = mess
        self.type_sel = type_sel

        self.is_active  = False
        self.row_dict = {}
        self.tmp_widgets = {"labels" : [], "values" : []}
        self._param_key = {
            "UTILISATEUR" :  ["ANGL_NAUT"],
            "CYLINDRIQUE" : ["ORIGINE", "AXE_Z"],
            # Seulement VECT_X ou VECT_Y est utilisé pour NORMALE
            "NORMALE" : ["VECT_X", "VECT_Y"]
            }
        self.normale_vector_choix = None
        self.normale_vector_choix_lst = ["Vecteur x", "Vecteur y"]
        self._update_interface = {
            "AUCUN" : self._update_aucun,
            "UTILISATEUR" : self._update_utilisateur,
            "CYLINDRIQUE" : self._update_cylindrique,
            "NORMALE" : self._update_normale
            }

    def set_type_sel(self, type_sel):
        """Place le titre décrivant le changement de repère."""
        self.type_sel = type_sel

    def _build_interface(self):
        """Construit l'interface de dialogue."""
        fra = Frame(self, relief="groove", borderwidth=4)
        fra.grid(row=0)
        self._build_selection(fra)

        but = Button(fra, text="OK", width=10,
                     command=self.validate_data, default=ACTIVE)
        but.grid(row=1, column=0, padx=4, pady=5)
        but = Button(fra, text="Cancel",
                     width=10, command=self.hide)
        but.grid(row=1, column=1, padx=4, pady=5)

        self.bind("&lt;Return>", self.validate_data)
        self.bind("&lt;Escape>", self.hide)

    def _build_selection(self, root):
        """Construit la séléction du type de changement de repère."""
        fra = Frame(root, relief="ridge", borderwidth=4) 
        fra.grid(row=0, column=0, columnspan=2, padx=4, pady=5)
        self.fra = fra

        Label(fra, textvariable=self.dialog_titre,
              font=("Arial", "16")).grid(row=0, padx=4,
                                         pady=2, sticky='w' + 'e')

        lab = Label(fra, text="Type de changement de repère: ")
        lab.grid(row=1, column=0, padx=4, pady=2)

        self.chgt_repere_choix = StringVar()
        menu = MyMenu(fra,
                      ["AUCUN", "UTILISATEUR", "CYLINDRIQUE", "NORMALE"],
                      self.chgt_repere_choix,
                      self.chgt_repere_changed)
        menu.grid(row=1, column=1, padx=4, pady=2)
    
    def _set_interface(self):
        """Construit le dialogue à son ouverture. Les choix courants
        de l'utilsateur sont affichés."""
        self.dialog_titre.set(self.type_sel % self.row_dict["NOM"])
        
        ancien_row_choix = self.row_dict.chgt_repere_choix.get()
        self.chgt_repere_choix.set(ancien_row_choix)
        self.chgt_repere_changed()
    
    def chgt_repere_changed(self):
        """Construit l'interface pour le type de changement de repère
        selectionné."""
        for wid in self.tmp_widgets["labels"] + self.tmp_widgets["values"]:
            wid.destroy()
        self.tmp_widgets = {"labels" : [], "values" : []}
        choix = self.chgt_repere_choix.get()
        self._update_interface[choix]()
        
    def _insert_vector(self, param_key, row_offset=0):
        """Place un vecteur dans l'interface."""
        if param_key in self.row_dict["CHGT_REP"]:
            default_values = self.row_dict["CHGT_REP"][param_key]
        else:
            default_values = (0.0, 0.0, 0.0)
        vec = VecteurEntry(self.fra, default_values, self.mess)
        vec.grid(init_col=1, row=2 + row_offset)
        self.tmp_widgets["values"].append(vec)
    
    def _update_utilisateur(self): 
        """Reconstruit l'interface pour le changement
        de repère UTILISATEUR"""
        lab = Label(self.fra, text="Angle nautique :")
        lab.grid(row=2, column=0,
                 padx=4, pady=2, sticky='w'+'e')
        self.tmp_widgets["labels"].append(lab)
           
        self._insert_vector("ANGL_NAUT")

    def _update_cylindrique(self):
        """Reconstruit l'interface pour le changement
        de repère CYLINDRIQUE"""
        data = zip(self._param_key["CYLINDRIQUE"],
                   ["Origine", "Axe z"])
        for row_offset, (param_key, text) in enumerate(data):
            lab = Label(self.fra, text=text + " :")
            lab.grid(row=2 + row_offset, column=0,
                     padx=4, pady=2, sticky='w'+'e')
            self.tmp_widgets["labels"].append(lab)

            self._insert_vector(param_key, row_offset)

    def _update_aucun(self):
        """Aucune interface n'est à constuire si AUCUN changement
        de repère n'est appliqué."""
        pass

    def _update_normale(self):
        """Reconstruit l'interface pour le changement
        de repère NORMALE"""
        self.normale_vector_choix = StringVar()
        menu = MyMenu(self.fra,
                      self.normale_vector_choix_lst,
                      self.normale_vector_choix,
                      default_var=self.normale_vector_choix_lst[0])
        menu.grid(row=2, column=0,
                  padx=4, pady=2, sticky='w' + 'e')
        self.tmp_widgets["labels"].append(menu)
        
        has_old_value = False
        for idx, veckey in enumerate(["VECT_X", "VECT_Y"]):
            if veckey in self.row_dict["CHGT_REP"]:
                vectext = self.normale_vector_choix_lst[idx]
                self.normale_vector_choix.set(vectext)
                self._insert_vector(veckey)
                has_old_value = True
        if not has_old_value:
            vectext = self.normale_vector_choix_lst[0]
            self.normale_vector_choix.set(vectext)
            self._insert_vector("VECT_X")
        
    def validate_data(self):
        """Valide les données entrées et les place sur le dictionaire
        de résultat."""
        choix = self.chgt_repere_choix.get()
        
        if  choix == "AUCUN":
            param_res = {}
        else:
            param_res = {"REPERE" : choix}
            if choix == "NORMALE":
                res = self.normale_vector_choix.get()
                idx = self.normale_vector_choix_lst.index(res)
                param_key = [self._param_key["NORMALE"][idx]]
            else:
                param_key = self._param_key[choix]

            for key, wid in zip(param_key, self.tmp_widgets["values"]):
                values = wid.get()
                if values:
                    param_res[key] = values
                else:
                    return

        self.row_dict["CHGT_REP"] = param_res
        self.row_dict.chgt_repere_choix.set(choix)
        
        self.hide()

    def hide(self):
        """Annulation du dialogue"""
        self.withdraw()

    def affiche(self, row_dict):
        """Lance l'interface de dialogue pour entrer un changement
        de repère."""
        if not self.is_active:
            self.row_dict = row_dict
            self.is_active = True
            self._set_interface()
            self.deiconify()
            self.is_active = False
        else:
            self.mess.disp_mess(
                "Un dialogue pour le groupe '%s' " \
                "est déjà ouvert. " % self.row_dict["NOM"])
            self.mess.disp_mess(
                "Il doit d'abord être fermé pour pouvoir lancer " \
                "un autre changement de repère." 
                )


class GroupNoWidget(Frame):
    """La grille permettant la séléction des DDL.
    Un label identifiant le groupe de DDL et un boutton
    pour ajouter un changement de repère peuvent y être
    ajoutes.
    """
    
    def __init__(self, root, nlines, **kwargs):
        Frame.__init__(self, root, **kwargs)
        self.root = root
        self.nlines = nlines
        self.kwargs = kwargs
        
        self.data = []
        self.widgets = []
        self.ypos = 0
        self.scroll = None
        self.chgt_rep = None
        self.BTNSTYLE = {'indicatoron':0,
                         'borderwidth':1,
                         'relief':GROOVE}

    def set_chgt_rep(self, chgt_rep):
        self.chgt_rep = chgt_rep

    def yview(self, *args):
        IDX = { 'pages' : self.nlines-1, 'units' :1 }
        if args[0]=='moveto':
            _,pos = args
            self.ypos=int(len(self.data)*float(pos))
        elif args[0]=='scroll':
            _,pos,unit = args
            self.ypos += int(pos) * IDX[unit]
        if self.ypos<0:
            self.ypos = 0
        if self.ypos>len(self.data)-self.nlines:
            self.ypos=len(self.data)-self.nlines
        self.redraw()

    def set_data(self, user_data):
        """Place les DDL à séléctioner dans la grille.
        Chaque ligne est representée par un dictionaire
        ayant la structure::
            {"NOM" : le nom du groupe DDL (optionel),
             "NOM_CMP" : la liste des DDL à afficher}

        :param user_data: la structure de la grille.
        :type user_data: une liste de dictionaires.
        """
        for wlist in self.widgets:
            for wid in wlist:
                wid.destroy()
        
        self.data = []
        self.widgets = []
        STYLE = self.BTNSTYLE.copy()
        STYLE.update(self.kwargs)
        
        for row_user_dict in user_data:
            row_dict = RowDict(row_user_dict.items())
            row_widgets = []
            row_ddl_vars = []

            if "NOM" in row_dict:
                lab =  Label(self, text=row_dict["NOM"], **self.kwargs)
                row_widgets.append(lab)
            else:
                row_dict["NOM"] = None

            for ddl_key in row_dict["NOM_CMP"]:
                var = IntVar()
                but = Checkbutton(self, text=ddl_key,
                                        command=self.toggled,
                                        variable=var, **STYLE)
                row_ddl_vars.append(var)
                row_widgets.append(but)

            row_dict["DDL_VARS"] = row_ddl_vars
            
            if self.chgt_rep:
                row_dict["CHGT_REP"] = {}
                row_dict.set_chgt_rep(self.chgt_rep)
                but = Button(self,
                             textvariable=row_dict.chgt_repere_choix,
                             command=row_dict.affiche_chgt_rep)
                row_widgets.append(but)
            
            self.data.append(row_dict)
            self.widgets.append(row_widgets)
        
        self.redraw()

    def redraw(self):
        for i,r in enumerate(self.widgets):
            for j,o in enumerate(r):
                if i<self.ypos or i>=self.ypos+self.nlines:
                    o.grid_forget()
                else:
                    o.grid(row=i-self.ypos,column=j,sticky='w')
        if self.scroll:
            N = float(len(self.data))
            p0 = self.ypos/N
            p1 = (self.ypos+self.nlines)/N
            self.scroll.set(p0,p1)

    def get_selected(self):
        """Retourne la liste des groupes de DDL
        selectionés.
        
        :return: une liste de dictionaires."""
        resu_lst = []
        for row_dict in self.data:
            res = []
            for ddl_key, int_widget in zip(row_dict["NOM_CMP"],
                                           row_dict["DDL_VARS"]):
                if int_widget.get():
                    res.append(ddl_key.strip())
            if res:
                resdict = {"NOM" : row_dict["NOM"],
                           "NOM_CMP" : res}
                if self.chgt_rep:
                    resdict["CHGT_REP"] = row_dict["CHGT_REP"]
                resu_lst.append(resdict)
        return resu_lst

    def toggled(self):
        self.root.notify()




class DispFRFDialogue(Toplevel):
    """L'interface offrant un changement de repère.
    """

    def __init__(self, mess, meidee_objects, param_visu, resu1=None, resu2=None, sumail=None):
        Toplevel.__init__(self)

        self.mess = mess
        self.meidee_objects = meidee_objects
        self.param_visu = param_visu
        self.resu1 = resu1
        self.resu2 = resu2
        self.sumail = sumail
        self.dyna1 = None
        self.dyna2 = None
        self.dyna3 = None
        self.errmess1 = "Donnees incompletes pour le calcul"

        self.protocol("WM_DELETE_WINDOW", self.hide)
        self.dialog_titre = StringVar()
        self._build_interface()

        self.is_active  = False
        self.row_dict = {}
        self.tmp_widgets = {"labels" : [], "values" : []}
        self.bind("&lt;Escape>", self.hide)



    def _build_interface(self):
        """Construit l'interface de dialogue."""
        f = Frame(self)
        f.grid()
        mdo = self.meidee_objects

        # boite 1 : choix de deux concepts resultats
        f1 = Frame(f, relief='ridge', borderwidth=4)
        f1.columnconfigure(0,weight=2)
        f1.columnconfigure(1,weight=3)

        Label(f1, text="Choix des concepts a visualiser").grid(row=0,column=0,columnspan = 2,pady=5)        
        self.var_resu1 = StringVar()
        Label(f1, text="Resultat 1").grid(row=1,column=0)
        MyMenu( f1, options = mdo.get_all_resus_name(),
                var = self.var_resu1, cmd = self.update).grid(row=1,column=1,sticky='ew')
        if self.resu1 != None: self.var_resu1.set(self.resu1.nom)
        
        self.var_resu2 = StringVar()
        Label(f1, text="Resultat 2").grid(row=2,column=0)
        MyMenu( f1, options = mdo.get_all_resus_name(),
                var = self.var_resu2, cmd = self.update).grid(row=2,column=1,sticky='ew')
        if self.resu2 != None: self.var_resu2.set(self.resu2.nom)
        f1.grid(row=0,column=0, sticky='ew')

        # boite 2 : si le ou les concepts sont des mode_meca, calcul d'une FRF
        f2 = Frame(f, relief='ridge', borderwidth=4)
        if self.resu1:
            if mdo.resultats.has_key(self.var_resu1.get()):
                #le concept 1 est un mode_meca
                self.exci_no1 = StringVar()
                self.exci_ddl1 = StringVar()
                self.exci_param(f2,self.var_resu1.get(),self.exci_no1,
                                self.exci_ddl1).grid(row=1,column=0,columnspan = 2)
                # PROVISOIRE
                self.exci_no1.set('N1')
                self.exci_ddl1.set('FX')
            else:
                # le concept est un dyna_harmo
                self.dyna1 = mdo.dyna_harmo[self.var_resu1.get()]
        if self.resu2:
            if mdo.resultats.has_key(self.var_resu2.get()):
                self.exci_no2 = StringVar()
                self.exci_ddl2 = StringVar()
                self.exci_param(f2,self.var_resu2.get(),self.exci_no2,
                                self.exci_ddl2).grid(row=2,column=0,columnspan = 2)
                # PROVISOIRE
                self.exci_no2.set('N1')
                self.exci_ddl2.set('FX')
            else:
                # le concept est un dyna_harmo
                self.dyna2 = mdo.dyna_harmo[self.var_resu2.get()]

        if mdo.resultats.has_key(self.var_resu1.get()) or mdo.resultats.has_key(self.var_resu2.get()):
            self.freq_max = StringVar()
            self.df = StringVar()
            Label(f2,text='Parametres de calcul').grid(row=3,column=0,columnspan=2,pady=5)
            Label(f2,text='Frequence max').grid(row=4,column=0, sticky='e')
            Entry(f2,textvariable = self.freq_max).grid(row=4,column=1, sticky='we')
            Label(f2,text='Resolution frequentielle').grid(row=5,column=0, sticky='e')
            Entry(f2,textvariable = self.df).grid(row=5,column=1, sticky='we')
            # PROVISOIRE
            self.freq_max.set("100")
            self.df.set('1')

            Button(f2,text='Calculer',command=self.prep_calc).grid(row=6,column=1,sticky='e')            
            
        f2.grid(row=1,column=0,sticky='ew')      

        # boite 3 : choix du noeud et DDL de visualisation
        f3 = Frame(f, relief='ridge', borderwidth=4)
        Label(f3, text=" visualisation des FRF").grid(row=0,column=0)
        Label(f3, text="Noeud de visualisation").grid(row=1, column=0, sticky='e')
        self.visu_no = StringVar()
        Entry(f3, textvariable = self.visu_no).grid(row=1,column=1, sticky='ew')
        
        Label(f3, text="Direction de visualisation").grid(row=2, column=0, sticky='e')
        ddls = ('DX','DY','DZ','DRX','DRY','DRZ')
        self.visu_ddl = StringVar()
        MyMenu(f3, var = self.visu_ddl,
               options = ddls).grid(row=2, column=1, sticky='ew')
        
        Label(f3, text="Nom champ").grid(row=3, column=0, sticky='e')
        nom_chams = ('DEPL','VITE','ACCE')
        self.nom_cham = StringVar()
        MyMenu(f3, var = self.nom_cham,
               options = nom_chams).grid(row=3, column=1, sticky='ew')
        
        Label(f3, text="   ").grid(row=4, column=0)
        Button(f3,text='Afficher',command=self.affich_FRF).grid(row=4,column=1,sticky='e')
        f3.grid(row=2,column=0,sticky='ew')
        # PROVISOIRE
        self.visu_no.set('N1')
        self.visu_ddl.set('DX')
        self.nom_cham.set('DEPL')
        
        
    def exci_param(self, parent, nom_resu, exci_no, exci_ddl):
        """Frame pour definir les parametres d'excitation"""
        f = Frame(parent)
        texte = "Simulation du resultat harmonique pour " + nom_resu
        Label(f, text=texte).grid(row=0,column=0, columnspan = 2,pady=5)
        Label(f, text="Noeud d'excitation").grid(row=1, column=0, sticky='e')
        Entry(f, textvariable = exci_no).grid(row=1,column=1, sticky='ew')
        Label(f, text="Direction d'excitation").grid(row=2, column=0, sticky='e')
        ddls = ('FX','FY','FZ')
        MyMenu(f, var = exci_ddl,
               options = ddls).grid(row=2, column=1, sticky='ew')
##        Label(f, text="   ").grid(row=3, column=0)
        return f


    def prep_calc(self):
        mdo = self.meidee_objects
        
        if mdo.resultats.has_key(self.var_resu1.get()):
            # le concept 1 est un mode_meca
            if self.exci_no1.get() and self.exci_ddl1.get():
                self.resu1 = mdo.resultats[self.var_resu1.get()]
                kass = self.resu1.kass
                mass = self.resu1.mass
                cass = self.resu1.cass
                self.dyna1 = self.calc_dyna_line_harm(self.resu1, mass, kass, cass,
                                                      self.exci_no1.get(),self.exci_ddl1.get())
            else:
                self.mess.disp_mess(self.errmess1)
                
        if mdo.resultats.has_key(self.var_resu2.get()):
            if self.exci_no2.get() and self.exci_ddl2.get():
                self.resu2 = mdo.resultats[self.var_resu2.get()]
                kass = self.resu2.kass
                mass = self.resu2.mass
                cass = self.resu2.cass
                self.dyna2 = self.calc_dyna_line_harm(self.resu2, mass, kass, cass,
                                                      self.exci_no2.get(),self.exci_ddl2.get())
                if self.sumail:
                    # etape supplementaire pour les calculs de modif struct
                    __DYNAM=DEPL_INTERNE(DEPL_GLOBAL=self.dyna2.obj,SUPER_MAILLE=self.sumail)
                    self.dyna3 = DynaHarmo( self.meidee_objects, __DYNAM.nom, __DYNAM )
                    self.meidee_objects.update(__DYNAM.nom, __DYNAM )

            else:
                self.mess.disp_mess(self.errmess1)



    def calc_dyna_line_harm(self, resu, mass, kass, cass, exci_no, exci_ddl):
        from Cata.cata import DYNA_LINE_HARM, AFFE_CHAR_MECA, DEFI_FONCTION

        freq_max = string.atof(self.freq_max.get())
        df = string.atof(self.df.get())
        nbfreq = int(freq_max/df)
        freq = tuple(1./df*Numeric.array(range(nbfreq)))

        modele = resu.modele.obj
        
        mcfact = { 'NOEUD' : exci_no, exci_ddl : 1 }
        __char = AFFE_CHAR_MECA( MODELE = modele,
                                 FORCE_NODALE = mcfact )
                                                          
        __dyna = DYNA_LINE_HARM( MATR_MASS = mass,
                                 MATR_RIGI = kass,
                                 MATR_AMOR = cass,
                                 FREQ = freq,
                                 MODELE = modele,
                                 EXCIT = _F( CHARGE = __char,
                                             COEF_MULT_C=('RI',1.,0.) ))

        dyna = DynaHarmo( self.meidee_objects, __dyna.nom, __dyna )
        self.meidee_objects.update(__dyna.nom, __dyna )

        return dyna


    def affich_FRF(self):
        from Cata.cata import RECU_FONCTION
        if self.sumail:
            dynas = [self.dyna1,self.dyna3]
        else:
            dynas = [self.dyna1,self.dyna2]
        module = []
        legende = []
        couleur = []
        for ind in range(2):
            if dynas[ind]:
                nomcmp = self.visu_ddl.get()
                if ind == 0 and self.sumail:
                    # les frf mesurees sont issues de LIRE_RESU dataset 58
                    # les noms des composantes sont D1, D2, D3 au lieu de DX, DY, DZ
                    if self.visu_ddl.get() == 'DX':
                        nomcmp = ['D1']
                    if self.visu_ddl.get() == 'DY':
                        nomcmp = ['D2']
                    if self.visu_ddl.get() == 'DZ':
                        nomcmp = ['D3']
                try:
                    __fonc = RECU_FONCTION( RESULTAT = dynas[ind].obj,
                                        NOM_CHAM = self.nom_cham.get(),
                                        NOEUD = self.visu_no.get(),
                                        NOM_CMP = nomcmp )
                except :
                    self.mess.disp_mess("Erreur dans RECU_FONCTION : " \
                                "Vérifier le nom et la direction du noeud")
                    return

                fonc_py = __fonc.convert('complex')
                ordo = fonc_py.vale_y
                freq = fonc_py.vale_x
                module.append([abs(kk) for kk in ordo])
                legende.append("%s %s_%s" % (dynas[ind].nom,self.visu_no.get(),self.visu_ddl.get()))
                couleur.append(ind+1)

        self.param_visu.visu_courbe(freq, module, couleur, legende, 'LIN', 'LOG')

    def update(self):
        """ refabrication de l'interface si les concepts
            a afficher ont change"""
        self.withdraw()
        self._build_interface()

    def hide(self):
        """Annulation du dialogue"""
        self.withdraw()



# Récupération du concept donnant la composante 
# du déplacement (DX, DY...DRZ) à partir de l'indice
# du degré de liberté
NOMCMP = aster.getcolljev('&CATA.GD.NOMCMP'.ljust(24))
DEPL_R = NOMCMP['DEPL_R  ']
ORDERED_DDL_PATTERN = ['DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ']

def sort_compo_key(compo_key):
    """Retourne l'indice du DDL par rapport au modèle."""
    return ORDERED_DDL_PATTERN.index(compo_key.strip())

def find_composantes(grp_noeuds, noeuds_def, mess = None):
    """Retourne les composantes du déplacement (DX, DY...DRZ) 
    pour chaque groupe de noeuds. Les noeuds sont définis par
    leur indice provenant du maillage."""
    composantes = {}

    if not grp_noeuds:
        return None
    
    for grp, noeud_idxs in grp_noeuds.items():
        grp_comp = set()
        for noeud_idx in noeud_idxs:
            k = (noeud_idx - 1) * 4
            for i in range(4):
                comp_bits = noeuds_def[k + i]
                for j in range(1, 31):
                    if comp_bits & 1<<j:
                        idx = 30*i + j - 1
                        grp_comp.add(DEPL_R[idx])
        composantes[grp] = grp_comp
    
    return composantes


class _SelectionBase(Frame):


    def __init__(self, root, title, **kwargs):
        self.cmd = None
        self.chgt_rep = None
        kwargs = self._parse_kwargs(kwargs)

        Frame.__init__(self, root, **kwargs)
        Label(self, text=title, **kwargs).grid(row=0, column=0, columnspan=2)
        
        self.grp = GroupNoWidget(self, 2, **kwargs)
        self.grp.grid(row=1,column=0)
        if self.chgt_rep:
            self.grp.set_chgt_rep(self.chgt_rep)

        scroll = Scrollbar(self, orient='vertical',
                           command=self.grp.yview, **kwargs)
        scroll.grid(row=1, column=1, sticky='ns')
        self.grp.scroll = scroll

    def _parse_kwargs(self, kwargs):
        if 'command' in kwargs:
            self.cmd = kwargs['command']
            del kwargs['command']
        
        if 'chgt_rep' in kwargs:
            self.chgt_rep = kwargs['chgt_rep']
            del kwargs['chgt_rep']
        
        return kwargs

    def set_modele_maillage(self, modl, maillage):
        pass
    
    def set_modele(self, modl, concepts):
        noma = modl.MODELE.LGRF.get()[0].strip()
        maillage = concepts[noma]
        self.set_modele_maillage(modl, maillage) 
    
    def set_resultat(self, modele):
        """Place un concept de type `Resultat' pour
        la représentation des groupe de noeuds ou de mailles
        dans l'interface Tk. Cette méthode exploite le travail
        accomplit par `Resultat' qui recherche automatiquement
        le modèle et le maillage lors de sa création."""
        self.set_modele_maillage(modele.obj, modele.maya)

    def display_comp(self, composantes):
        data = []
        groupes = composantes.keys()
        groupes.sort()

        for grp in groupes:
            ddl_keys = list(composantes[grp])
            if len(ddl_keys) != 0:
                try:
                    ddl_keys.sort(key=sort_compo_key)
                except TypeError: # version de python < 2.4
                    ddl_keys.sort()
                data.append({"NOM": grp, "NOM_CMP" : ddl_keys})
        
        self.grp.set_data(data)
   
    def get_selected(self):
        return self.grp.get_selected()

    def notify(self):
        if self.cmd:
            self.cmd()


class SelectionNoeuds(_SelectionBase):

    def __init__(self, root, title, **kwargs):
        _SelectionBase.__init__(self, root, title, **kwargs)
        if self.chgt_rep:
            self.chgt_rep.set_type_sel("Groupe de noeuds: %s.")
        
    def set_modele_maillage(self, modl, maillage):
        noeuds_def = modl.MODELE.PRNM.get()
        groupno = maillage.GROUPENO.get()
        composantes = find_composantes(groupno, noeuds_def)
        self.display_comp(composantes)


class SelectionMailles(_SelectionBase):
    
    def __init__(self, root, title, **kwargs):
        _SelectionBase.__init__(self, root, title, **kwargs)
        if self.chgt_rep:
            self.chgt_rep.set_type_sel("Groupe de mailles: %s.")
        
    def set_modele_maillage(self, modl, maillage):
        noeuds_def = modl.MODELE.PRNM.get()
        groupma = maillage.GROUPEMA.get()
        mailles_def = maillage.CONNEX.get()
        
        groupno = {} 
        for grp, mailles in groupma.items():
            noeuds = []
            groupno[grp] = noeuds
            for maille in mailles:
                for nod in mailles_def[maille]:
                    if nod not in noeuds:
                        noeuds.append(nod)
        composantes = find_composantes(groupno, noeuds_def)
        self.display_comp(composantes)





class MacWindowFrame(Frame):
    """!Interface de la fenetre d'affichage des modes MAC

    contient:

     - un titre
     - la matrice de MAC
     - la liste des modes
     - les labels des lignes et colonnes de la matrice
     - un bouton (log) permettant de commuter l'affichage linéaire et logarithmique

    """
    def __init__(self, root, label,  name1=None, name2=None, size=None ):
        """!Constructeur

        :IVariables:
         - `root`: la fenetre parente
         - `mat`: la matrice des valeurs à afficher
         - `modes1`: liste des modes en Y
         - `modes2`: liste des modes en X
         - `logvar`: variable Tk liée au bouton radio indiquant la méthode (log ou pas) d'affichage
         - `mac`: l'objet MacMode
         - `diplayvar1`: variable liée à un label pour permettre l'affichage des coordonnées de la valeur sous le curseur
         - `diplayvar2`: variable liée à un label pour permettre l'affichage de la valeur sous le curseur
        """
        Frame.__init__(self, root)
        self.rowconfigure(1, weight=1)
        self.columnconfigure(0, weight=1)

        # Titre
        titre = Label( self, text=label )
        titre.grid(row=0, column=0, columnspan=4, sticky='n' )

        # Graphique
        if size == None:
            size = (100,100)
        self.mac = MacMode( self, height=size[0], width=size[1] )
        self.mac.grid( row=2, column=0, sticky="nsew" )
        self.modes1 = None
        self.modes2 = None

        # Label abcisse/ordonnée
        if not name1:
            name1 = "1"
        if not name2:
            name2 = "2"
        Label(self,text=name1).grid(row=1, column=1, sticky='e')
        Label(self,text=name2).grid(row=1, column=2)

        # Tableau de modes
        self.text_modes1 = Text(self, width=8, height=10)
        self.text_modes1.grid( row=2, column=1 )
        self.text_modes2 = Text(self, width=8, height=10)
        self.text_modes2.grid( row=2, column=2 )

        # Switch log/lin
        self.logvar = IntVar()
        self.mac.resize_ok()
        self.displayvar1 = StringVar()
        self.displayvar1.set("none")
        self.displayvar2 = StringVar()
        self.displayvar2.set("none")
        Label(self,textvariable=self.displayvar1).grid(row=5,column=0,columnspan=4)
        Label(self,textvariable=self.displayvar2).grid(row=6,column=0,columnspan=4)
        self.mac.bind("<Motion>", self.mode_info )


    def mode_info(self, event):
        """!Récupère la valeur MAC de la case sous le curseur"""
        oid = self.mac.find_closest( event.x, event.y )
        if oid:
            i,j = self.mac.items.get(oid[0], (None,None) )
        else:
            return
        if i is None or i<0 or i>=len(self.modes1[0]):
            return
        if j is None or j<0 or j>=len(self.modes2[0]):
            return
        txt1 = self.modes1[1][i]
        txt2 = self.modes2[1][j]
        v = self.mac.mat[i,j]
        self.displayvar1.set("%s / %s" % (txt1,txt2) )
        self.displayvar2.set("%.5g" % (v) )


    def build_modes(self, text, modes):
        """!Construit la liste des modes dans une boite texte"""
        text.configure( width=max( [len(m) for m in modes[1] ] )+1 )
        text.delete( '0.0', 'end' )
        text.insert('end', "\n".join( modes[1] ) )

    def set_modes(self, modes1, modes2, mat ):
        modes1 = (modes1[:,1], ["%8.2f Hz" % f for f in modes1[:,1] ])
        modes2 = (modes2[:,1], ["%8.2f Hz" % f for f in modes2[:,1] ])
        self.modes1 = modes1
        self.modes2 = modes2
        self.build_modes(self.text_modes1, modes1 )
        self.build_modes(self.text_modes2, modes2 )
        self.mat = mat
        self.mac.show_mat( self.mat )

    def clear_modes(self):
        self.modes1 = ( [], [] )
        self.modes2 = ( [], [] )
        self.text_modes1.delete( '0.0', 'end' )
        self.text_modes2.delete( '0.0', 'end' )
        self.mac.clear()
