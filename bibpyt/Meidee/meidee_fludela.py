#@ MODIF meidee_fludela Meidee  DATE 14/05/2008   AUTEUR BODEL C.BODEL 
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


__docformat__ = "restructuredtext fr"


import sys
import string
from Utilitai.Utmess import UTMESS

from Cata.cata import MAC_MODES
import aster
import Numeric
from Numeric import zeros, size, Float, pi, sqrt, array
from Meidee.meidee_calcul_correlation import extract_mac_array
from Tkinter import Frame, Menubutton, Menu, StringVar, IntVar, Listbox
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, DoubleVar
from Meidee.meidee_iface import MyMenu, LabelArray, MenuArray, EntryArray, ModeList
from Meidee.meidee_iface import VLabelledItem, HLabelledItem, MacWindow
from Meidee.meidee_calcul_fludela import MeideeFludela


######################
#                    #
# CLASSES GRAPHIQUES #
#                    #
######################

## paramètres par défaut pour la création des Frame qui servent de cadre
DEFAULT_FRAME_ARGS = {
    "relief" : 'ridge',
    "borderwidth" : 4,
    }


#-----------------------------------------------------------------------------

class InterfaceLongeq(Frame):
    """!Interface de paramétrage du calcul de la longueur équivalente

    """
    def __init__(self, parent, objects, fludela, mess ):
        """!Constructeur

        Construit l'interface de selection et calcul des longueurs équivalente

        \param parent La Frame parente
        \param objects Une instance de MeideeObjects
        \param fludela Une instance de MeideeFludela
        
        """
        Frame.__init__(self, parent, **DEFAULT_FRAME_ARGS)
        self.parent = parent
        self.objects = objects
        assert isinstance( parent, InterfaceFludela)
        # Menu choix du resultat numerique
        Label(self, text="Choisissez un resultat pour le calcul" \
              " de la longeur equivalente ").grid(row=0, column=0, columnspan=3)

        Label(self, text="Resultat").grid(row=1,column=0)
        self.modes_num = StringVar()
        self.menu_modes = MyMenu( self,
                                  objects.get_resultats_num(),
                                  self.modes_num,
                                  self.change_mode )
        self.menu_modes.grid(row=2, column=0)


        Label(self, text="Cara Elem").grid(row=1, column=1)
        self.cara_num = StringVar()
        self.menu_cara = MyMenu( self,
                                 objects.get_cara_elem(),
                                 self.cara_num,
                                 self.compute_leq )

        self.menu_cara.grid(row=2, column=1)

        Label(self, text="Champ Materiau").grid(row=1, column=2)
        self.cham_num = StringVar()
        self.menu_cham = MyMenu( self,
                                 objects.get_cham_mater(),
                                 self.cham_num,
                                 self.compute_leq )

        self.menu_cham.grid(row=2, column=2)


        self.long_eq_array = ModeList( self, 'Longeur equivalente' )
        self.long_eq_array.grid(row=3,column=0)
        self.long_eq_values = zeros( (1,1), )
        self.fludela = fludela
        self.fludela.register_longeq( self.notify )
        self.mess = mess

    def refresh(self):
##        self.objects.recup_objects()

        results_num = self.objects.get_resultats_num()
        old_result = self.modes_num.get()
        self.menu_modes.update( results_num, self.modes_num, self.change_mode )
        self.modes_num.set(old_result)

        self.change_mode()


    def change_mode(self):
        mode = self.modes_num.get()
        if mode=="Choisir":
            mode = None
        # Selection des cara_elem
        cara_elems = self.objects.get_cara_elem(mode)
        old_cara = self.cara_num.get()
        #self.menu_cara.update( cara_elems, self.cara_num, self.compute_leq )
        if old_cara in cara_elems:
            self.cara_num.set(old_cara)
        elif len(cara_elems)==1:
            self.cara_num.set(cara_elems[0])
        else:
            self.cara_num.set("Choisir")


        # Selection des cham_mater
        cham_maters = self.objects.get_cham_mater(mode)
        old_cara = self.cham_num.get()
        self.menu_cham.update( cham_maters, self.cham_num, self.compute_leq )
        if old_cara in cham_maters:
            self.cham_num.set(old_cara)
        elif len(cham_maters)==1:
            self.cham_num.set(cham_maters[0])
        else:
            self.cham_num.set("Choisir")

        self.compute_leq()

    def compute_leq(self):
        """!Si possible calcule la longueur equivalente"""
        try:
            self.resu = self.objects.get_resu( self.modes_num.get() )
        except KeyError:
            return
        self.mess.disp_mess( (  " Calcul de la longueur équivalente" ) )
        self.mess.disp_mess( (  self.resu.modele_name ) )
        self.mess.disp_mess( (  self.resu.nom ) )
        self.mess.disp_mess( ( "  " ) ) 

        self.fludela.set_res_longeq( self.resu )
        self.fludela.compute()

    def notify(self):
        leq = self.fludela.long_eq
        nume_modes = self.resu.get_modes()[3]
        self.long_eq_array.fill_modes( leq, nume_modes, format = '%13.5E' )


#-----------------------------------------------------------------------------


class InterfaceDisplayModes(Frame):
    """!Interface affichant le tableau de sélection des modes pour Massamor

    On présente un tableau contenant un nombre de lignes correspondant aux
    modes sur lesquels effectuer les calculs. Les trois premières colonnes
    sont éditables et permettent de choisir les numéros des modes qui sont
    en correspondance pour les études en air, en eau et en écoulement.

    Les colonnes suivantes permettent d'afficher certains des résultats de
    calcul et l'entete de chaque colonne permet de préciser quel est le
    résultat à afficher
    """
    def __init__(self, parent, objects, fludela, mess, rows = 8, cols = 5 ):
        """!Constructeur

        \param parent La frame Tkinter.Frame parente
        \param objects Une instance de MeideeObjects permettant d'accéder
               à la liste des objets Aster disponibles
        \param fludela Une instance de la classe MeideeFludela

        L'attribut fields fournit une correspondance entre la liste
        des choix possibles comme entete de colonne (la clef sert de
        label) et la source de donnée.

        La source de données est décrite comme un tuple (nom de champ, index)
        où l'index désigne si le numéro du mode est relatif ou absolu (si le
        mode désigne un mode extrait ou un mode d'origine)
        """
        Frame.__init__(self, parent)
        self.mess = mess
        self.objects=objects
        self.rows = rows
        self.cols = cols
        self.fields =  { "      F 1      " : ('freq_eq_a',0),
                         "      F 2      " : ('freq_eq_e',0),
                         "      F 3      " : ('freq_eq_v',0),
                         "      M 1      " : ('mass_a', 1),
                         "      M 2      " : ('mass_e', 2),
                         "      M 3      " : ('mass_v', 3),
                         "     Xsi 1     ":  ("xsi_a",1),
                         "     Xsi 2     " : ("xsi_e",2),
                         "     Xsi 3     " : ("xsi_v",3),
                         "Ma rep dim  mod" : ('mass_ajou_dim_mod',0),
                         "Ma rep dim  lin" : ('mass_ajou_dim_lin',0),
                         "Ma rep adim lin" : ('mass_ajou_adim_lin',0),
                         "Ca rep dim  mod"  : ('amor_ajou_rep_dim_mod',0),
                         "Ca rep dim  lin"  : ('amor_ajou_rep_dim_lin',0),
                         "Ca vit dim  mod"  : ('amor_ajou_vit_dim_mod',0),
                         "Ca vit dim  lin"  : ('amor_ajou_vit_dim_lin',0),
                         "Ca vit adim lin" : ('amor_ajou_vit_adim_lin',0),
                         "   Xsia rep    " : ('xsi_rep_ajou',0),
                         "   Xsia vit    " : ('xsi_vit_ajou',0),
                         "Ka vit dim  mod"  : ('raid_ajou_dim_mod',0),
                         "Ka vit dim  lin"  : ('raid_ajou_dim_lin',0),
                         "Ka vit adim lin"  : ('raid_ajou_adim_lin',0),
                         "      K 1      " : ("raid_a",1),
                         "      K 2      " : ("raid_e",2),
                         "      K 3      " : ("raid_v",3),
                         " Mint dim mod  " : ("xmi",1),
                         }

        
        frame = Frame(self)
        frame.grid( row=1, column=0, columnspan=4 )
        self.modes_array = EntryArray(self, 2, 0, self.rows, 3, cb=self.update, width=[6,8,8] )
        self.value_array = LabelArray(self, 2, 3, self.rows, self.cols )
        options = self.fields.keys()
        options.sort()
        self.menu_array = MenuArray(self, 1, 3, 1, self.cols, options, cb=self.notify )
        Label(self,text="Air (1)").grid(row=1,column=0)
        Label(self,text="Eau (2)").grid(row=1,column=1)
        Label(self,text="Ecoul (3)").grid(row=1,column=2)
        for n in range(self.rows):
            self.modes_array.set( n, 0, 0)
            self.modes_array.set( n, 1, 0)
            self.modes_array.set( n, 2, 0)
        k = self.fields.keys()
        k.sort()
        for col in range(min(self.cols,len(k))):
            self.menu_array.set(0,col,k[col])
        self.fludela = fludela
        self.fludela.register_massam( self.notify )
        self.update()
        

    def update_ind_mod(self, res_air, res_eau, res_vit):
        self.ind_air = self.ind_eau = self.ind_vit = [0]*self.rows
        try:
            self.ind_air = res_air.get_modes()[3][:,1]
            self.ind_eau = res_eau.get_modes()[3][:,1]
            self.ind_vit = res_vit.get_modes()[3][:,1]
        except AttributeError:
            pass
        for n in range(self.rows):
            try:
                self.modes_array.set( n, 0, int(self.ind_air[n]))
                self.modes_array.set( n, 1, int(self.ind_eau[n]))
                self.modes_array.set( n, 2, int(self.ind_vit[n]))
            except IndexError:
                #cas ou il y a plus de lignes que de modes
                pass

    def add_line(self):
        """!Ajoute une ligne au tableau des résultats"""
        state = self.save_state()
        self.rows += 1
        self.reinit( state )

    def rem_line(self):
        """!Supprime une ligne du tableau des résultats"""
        state = self.save_state()
        self.rows -= 1
        self.reinit( state )

    def save_state(self):
        """!Sauvegarde l'état du tableau"""
        pass

    def reinit(self, state):
        """Réinitialise le tableau en essayant de restaurer l'état"""
        self.update()

    def update(self, event=None):
        """!Notifie l'objet massamor des changements de modes"""
        modes = []
        for i, (m1, m2, m3) in enumerate(self.modes_array.rows_value()):
            # on recupere les numero de mode (1 ou plusieurs)
            err = False
            try:
                m1 = [ int(m) for m in m1.split(",") ]
            except ValueError:
                self.show_err( i,0 )
                err = True
            try:
                m2 = [ int(m) for m in m2.split(",") ]
            except ValueError:
                self.show_err( i,1 )
                err = True
            try:
                m3 = [ int(m) for m in m3.split(",") ]
            except ValueError:
                self.show_err( i,2 )
                err = True
            if err:
                continue
            self.show_ok( i )
            # pour l'instant un seul mode
            modes.append( (m1, m2, m3) )

        return modes

    def notify( self, event=None ):
        """!Callback de notification

        est appelé par CalcFludela lorsque le calcul a été effectué
        """
        self.mess.disp_mess( (  "NOTIFY" ) ) 
        modes = self.fludela.pairs
        self.mess.disp_mess( (  'modes = ' + str(modes) ) )
        data = self.fludela.calc
        self.mess.disp_mess( ( 'data = ' + str(data) ) ) 
        for i,(m1,m2,m3) in enumerate(modes):
            P = (i,m1,m2,m3)
            # TODO probablement un recalc de massamor pour obtenir les
            # resu pour la frequence equivalente
            for c in range(self.cols):
                choice = self.menu_array.get(0,c)
                field, p = self.fields[choice]
                data_field = data[field]
                if p == 0:
                    value1 = data_field[P[p]]
                else:
                    modes = P[p]
                    res = []
                    for m in modes:
                        res.append( str(data_field[m]) )
                    value1 = ",".join( res )
                self.value_array.set( P[0], c, value1 )

    def show_err( self, i, k ):
        """!Indique une erreur de saisie dans la cellule"""
        t1, w1 = self.modes_array.getw( i, k )
        w1['background']='red'


    def show_ok( self, i ):
        """!Indique que les choix d'une ligne sont valides"""
        for k in range(3):
            t1, w1 = self.modes_array.getw( i, k )
            w1['background']='green'


#-----------------------------------------------------------------------------

class InterfaceChoixModes(Frame):
    """!Panneau de sélection des modes et des écoulements"""
    def __init__(self, parent, objects, paramphy, longeq, fimens, mess, fludela ):
        """!Constructeur

          - \param parent: La Frame parente
          - \param objects: Le repository d'objets aster
          - \param paramphy: Interface des paramètres physiques
          - \param longeq: Interface pour le calcul de la longueur équivalente
          - \param fimens: Fichiers fimen à utiliser
          - \param fludela: instance de MeideeFludela

        :type objects: MeideeObjects
        :type paramphy: InterfaceParamPhys
        :type longeq: InterfaceLongeq
        :type fludela: MeideeFludela
        :type fimens: ( fd, nom de fichier )
        """
        Frame.__init__(self, parent, **DEFAULT_FRAME_ARGS)
        self.mess = mess
        Label(self, text="Choix des écoulements").grid(columnspan=3, sticky='n')
        self.objects = objects
        self.longeq_obj = longeq
        self.paramphy_obj = paramphy
        self.fludela = fludela
        self.parent = parent
        self.mess = mess

        # Choix des écoulements

        self.modes_air = StringVar()
        itm = HLabelledItem( self, "Air                            :",
                             MyMenu,
                             objects.get_resultats_num(),
                             self.modes_air,
                             cmd = self.update
                             )
        self.menu_air = itm.itm
        itm.grid(row=1,column=1,sticky='w')


        self.modes_eau = StringVar()
        itm = HLabelledItem( self, "Eau                           :",
                             MyMenu,
                             objects.get_resultats_num(),
                             self.modes_eau,
                             self.update
                             )
        self.menu_eau = itm.itm
        itm.grid(row=2,column=1,sticky='w')



        self.modes_vit = StringVar()
        itm = HLabelledItem( self, "Ecoulement               :",
                             MyMenu,
                             objects.get_resultats_num(),
                             self.modes_vit,
                             cmd = self.update2
                             )
        self.menu_vit = itm.itm
        itm.grid(row=3,column=1,sticky='w')

        self.vitesse = StringVar()
        itm = HLabelledItem( self, "Vitesse                :",
                             Entry,
                             textvariable=self.vitesse )
        self.vitesse_entry = itm.itm
        itm.itm.bind("<Return>", self.change_vitesse )
        itm.grid(row=3, column=2,sticky='w' )

        # Choix des fichiers FIMEN
        self.fimens = {}
        for u,m in fimens:
            self.fimens[m] = u
        self.fimen_name = StringVar()
        self.menu_fimen = MyMenu( self, self.fimens.keys(),
                                  self.fimen_name )
        self.fimen_name.set("Fichier FIMEN")
        self.menu_fimen.grid(row=2, column=2,sticky='w')

        self.methode = StringVar()
        itm = HLabelledItem(self, "Methode multimodale :",
                      MyMenu,
                      fludela.methodes_globales, self.methode, self.update )
        itm.grid(row=4,column=1,sticky='w')
        Button(self, text="Calculer !", command=self.calcul ).grid(row=5, column=1,sticky='w')

        # Tableau de données
        plusmoins1 = Frame(self)
        Button(plusmoins1, text="+", command=self.add_row ).grid(row=0, column=0)
        Button(plusmoins1, text="-", command=self.rem_row ).grid(row=1, column=0)
        plusmoins1.grid(row=8,column=0)
        plusmoins2 = Frame(self)
        Button(plusmoins2, text="+", command=self.add_col ).grid(row=0, column=0)
        Button(plusmoins2, text="-", command=self.rem_col ).grid(row=0, column=1)
        plusmoins2.grid(row=7,column=1, columnspan = 3)
        
        self.multi = InterfaceDisplayModes( self, self.objects, fludela, mess )
        self.multi.grid(row=8,column=1,columnspan=3)
        self.rowconfigure(8,weight=1)
        
        mac_frame = Frame(self)
        mac_frame.grid( row=6, column=0, columnspan=3)
        
        Button(mac_frame, text="Mac Air-Eau", command=self.show_mac_air_eau ).grid(row=0, column=1)
        Button(mac_frame, text="Mac Eau-Ecoulement", command=self.show_mac_eau_ecoul ).grid(row=0, column=2)
        Button(mac_frame, text="Mac Air-Ecoulement", command=self.show_mac_air_ecoul ).grid(row=0, column=3)
        Button(self, text="Sauver", command=self.save_calc ).grid(row=9, column=0)
        Button(self, text="Exporter ", command=self.impr_resu ).grid(row=9, column=1)
        self.export_name = StringVar()
        Entry(self, textvariable=self.export_name).grid(row=9, column=2)


    def add_row(self):
        self.change_interface_display_modes(1,0)
        
    def add_col(self):
        self.change_interface_display_modes(0,1)

    def rem_row(self):
        self.change_interface_display_modes(-1,0)

    def rem_col(self):
        self.change_interface_display_modes(0,-1)

    def change_interface_display_modes(self,r,c):
        nb_rows = self.multi.rows + r
        nb_cols =self.multi.cols + c
        self.multi.grid_remove()
        self.multi = InterfaceDisplayModes( self, self.objects, self.fludela,
                                            self.mess, rows=nb_rows, cols=nb_cols )
        self.multi.grid(row=8,column=1,columnspan=3)
        self.update()

        
        pass

    def refresh(self):
##        self.objects.recup_objects()
        results_num = self.objects.get_resultats_num()
        self.menu_air.update( results_num, self.modes_air, self.update )
        self.menu_eau.update( results_num, self.modes_eau,  self.update)
        self.menu_vit.update( results_num, self.modes_vit, self.update2 )
        

    def mode_valid( self, mode_ ):
        """!Détermine si un objet résultat sélectionné est valide."""
        try:
            obj = self.objects.get_resu( mode_ )
        except KeyError:
            return False
        return True
    

    def update(self):
        """! Affiche les nume_ordr des mode dans le tableau à trois
             colonnes de l'interface InterfaceDisplayModes dans
             laquelle on affiche les résultats (en envoyant update_ind_mod)
        """
        res_air = res_eau = res_vit = None
        try:
            res_air = self.objects.get_resu( self.modes_air.get() )
            res_eau = self.objects.get_resu( self.modes_eau.get() )
            res_vit = self.objects.get_resu( self.modes_vit.get() )
        except KeyError:
            pass
        self.multi.update_ind_mod(res_air, res_eau, res_vit)
        self.multi.update()


    def update2(self):
        self.update()
        self.update_vit()

    def calcul(self):
        """!Relance les calculs en cas de changement de paramètrage"""
        res_vit = res_air = res_eau = None
        if self.mode_valid( self.modes_air.get() ):
            res_air = self.objects.get_resu( self.modes_air.get() )
        if self.mode_valid( self.modes_eau.get() ):
            res_eau = self.objects.get_resu( self.modes_eau.get() )
        if self.mode_valid( self.modes_vit.get() ):
            res_vit = self.objects.get_resu( self.modes_vit.get() )
            
        liste_para = [list.get() for list in self.paramphy_obj.param_phys]
        self.fludela.set_param_phy( rho_int = liste_para[0],
                                    rho_ext = liste_para[1],
                                    diam_int = liste_para[2],
                                    diam_ext = liste_para[3])
        
        self.fludela.set_results( res_air, res_eau, res_vit )
        modes = self.multi.update()              # Update des "accouplements de modes"
        self.fludela.set_modes_pairs( modes )
        if self.fimen_name.get() != 'Fichier FIMEN':
            self.fludela.set_fimen(self.fimen_name.get())
        self.fludela.prep_massamor()
        self.fludela.methode_globale = self.methode.get()
        self.fludela.compute()

        
    def update_vit(self):
        """!Change la couleur de la vitesse pour que l'utilisateur
        pense à la re-préciser si nécessaire"""
        self.refresh()
        self.vitesse_entry["background"] = "red"


    def show_mac_air_eau(self):
        """!Affichage de la matrice MAC air - eau"""
        mode1 = self.modes_air.get()
        mode2 = self.modes_eau.get()
        titre = "MAC AIR - EAU"
        self.show_mac( titre, mode1, mode2 )

    def show_mac_eau_ecoul(self):
        """!Affichage de la matrice MAC eau - écoulement"""
        mode1 = self.modes_eau.get()
        mode2 = self.modes_vit.get()
        titre = "MAC EAU - ECOULEMENT"
        self.show_mac( titre, mode1, mode2 )

    def show_mac_air_ecoul(self):
        """!Affichage de la matrice MAC air - écoulement"""
        mode1 = self.modes_air.get()
        mode2 = self.modes_vit.get()
        titre = "MAC AIR - ECOULEMENT"
        self.show_mac( titre, mode1, mode2 )
    
    def show_mac(self, titre, mode1, mode2 ):
        """!Affichage d'une matrice de MAC

        utilisée par show_mac_air_eau, show_mac_eau_ecoul, show_mac_air_ecoul
        pour afficher la fenetre d'affichage de la matrice de MAC

        \param titre le titre de la fenetre
        \param mode1 le nom de l'objet mode_meca pour la base 1
        \param mode2 le nom de l'objet mode_meca pour la base 2
        """
        if ( not self.mode_valid( mode1 ) or
             not self.mode_valid( mode2 ) ):
            return
        res_1 = self.objects.get_resu( mode1 )
        res_2 = self.objects.get_resu( mode2 )
        try:
            __MAC = MAC_MODES( BASE_1=res_1.obj,
                               BASE_2=res_2.obj )
        except aster.FatalError:
            self.mess.disp_mess("!! Calcul de MAC impossible : bases incompatibles !!")
            UTMESS('A','MEIDEE0_3')
            return
        mat = extract_mac_array( __MAC )
        freq1, _, _, modes1, _, _ = res_1.get_modes()
        freq2, _, _, modes2, _, _ = res_2.get_modes()
        freq1 = [ "%.2f" % f[1] for f in freq1 ]
        freq2 = [ "%.2f" % f[1] for f in freq2 ]
        modes1 = [ int(i[1]) for i in modes1]
        modes2 = [ int(i[1]) for i in modes2]
        mac_win = MacWindow( self, titre, (modes1,freq1), (modes2,freq2), mat, name1=mode1, name2=mode2 )

    def save_calc(self):
        """!Sauvegarde un résultat de calcul dans un fichier
            Important : on ne sauve que les paramètres affichés dans l'IHM.
        """        
        name = self.vitesse.get()
        self.mess.disp_mess('name = ' + str(name))
        data_tmp = self.fludela.save_data()
        data = {}
        for col in range(self.multi.cols):
            key = self.multi.menu_array.get(0,col)
            obj = self.multi.fields[key][0]
            data[key] = data_tmp[obj]
        self.mess.disp_mess("Ecriture dans le fichier resultat")
        self.mess.disp_mess("des données affichées dans l'IHM")
        self.mess.disp_mess(" ")
        self.fludela.saved_data[name] = data
        self.fludela.save_to_file(name, data)

    def impr_resu(self):
        """!Impression dans une table aster des tous les résultats
            obtenus avec les différents calculs"""
        data_fin = self.fludela.saved_data
        nom = self.export_name.get()
        if len(nom) > 8:
            self.mess.disp_mess("!! Nom de concept défini trop long !!")
            self.mess.disp_mess("  ")
        self.fludela.save_to_table(data_fin, nom)
        

    def change_vitesse(self, event):
        """!Callback appelé lors d'un changement du paramètre vitesse"""
        try:
            vit = float(self.vitesse.get())
        except ValueError:
            self.mess.disp_mess( (  "Vitesse invalide" ) )
            self.mess.disp_mess( ( " " ) ) 
            self.vitesse_entry['background'] = "red"
            return
        except TypeError:
            self.mess.disp_mess( (  "Rentrer une vitesse d'écoulement" ) )
            self.mess.disp_mess( ( " " ) )
            return
        self.vitesse_entry['background'] = "green"
        self.fludela.set_speed( vit )
    
    def destroy_mac_window( self, win ):
        # inutile ?
        pass


#------------------------------------------------------------------------------

class InterfaceParamPhys(Frame):
    """!Interface de lecture des paramètres physiques"""
    
    def __init__(self, parent, fludela, mess):
        """!Constructeur
        \param parent l'objet parent dérivant de Tkinter.Frame
        \param fludela une instance de MeideeFludela
        """
        Frame.__init__(self, parent, **DEFAULT_FRAME_ARGS)
        Label(self, text="Parametre physiques").grid(columnspan=2)
        self.mess = mess
        self.diam_ext = DoubleVar()
        self.diam_int = DoubleVar()
        self.diam_ext.set(fludela.diam_ext)
        self.diam_int.set(fludela.diam_int)        
        self.rho_flu_int = DoubleVar()
        self.rho_flu_ext = DoubleVar()
        self.rho_flu_int.set(fludela.rho_flu_int)
        self.rho_flu_ext.set(fludela.rho_flu_ext)
        e0 = VLabelledItem( self, "Diametre ext. du tube",
                            Entry,
                            textvariable=self.diam_ext )
        e0.grid( row=1, column=0 )
        e1 = VLabelledItem( self, "Diametre int. du tube",
                            Entry,
                            textvariable=self.diam_int )
        e1.grid( row=1, column=1 )
        e2 = VLabelledItem( self, "Masse vol fluide interne",
                            Entry,
                            textvariable=self.rho_flu_int )
        e2.grid( row=1, column=2 )

        e3 = VLabelledItem( self, "Masse vol fluide externe",
                            Entry,
                            textvariable=self.rho_flu_ext )
        e3.grid( row=1, column=3 )
        self.param_phys = (self.rho_flu_int,self.rho_flu_ext,self.diam_int,self.diam_ext)
        
        self.fludela = fludela


    def update(self, event):
        """!Callback appelé lors du changement d'un des paramètres physique

        on notifie l'objet fludela du changement et on relance le calcul
        """
        self.mess.disp_mess( ( "Update, relancement du calcul " ) ) 
        rho_int = self.rho_flu_int.get()
        rho_ext = self.rho_flu_ext.get()
        if rho_ext == 0:
            self.mess.disp_mess("!! Calcul impossible : la masse volumique !!")
            self.mess.disp_mess("!!     doit etre différente de zero       !!")
        diam = self.diam_ext.get()
        if diam == 0:
            self.mess.disp_mess("!! Calcul impossible : la le diamètre ext !!")
            self.mess.disp_mess("!!     doit etre différent de zero        !!")
        liste_para = [list.get() for list in self.param_phys]
        self.fludela.set_param_phy( rho_int = liste_para[0],
                                    rho_ext = liste_para[1],
                                    diam_int = liste_para[2],
                                    diam_ext = liste_para[3])
        self.fludela.compute()


#-------------------------------------------------------------------------------

class InterfaceFludela(Frame):
    """!Classe qui crée les différents panneaux d'interface du tab fludela"""
    
    def __init__(self, parent, aster_objects, fimens, mess, out ):
        """!Constructeur

        :IVariable:
         - `parent`: fenetre parente
         - `aster_objects`: concepts aster dans jeveux
         - `fimens`: UL des fichiers fimen2x
         - `mess` : fenetre message
         - `obj_out`: concepts aster "table_sdaster" dans lesquelles on va ranger
            les résultats que l'utilisateur voudra sauver
         - `declout`: la fonction self.DeclareOut
        """

        Frame.__init__(self, parent, **DEFAULT_FRAME_ARGS )
        self.mess = mess
        self.objects = aster_objects
##        self.objects.recup_objects()
        self.columnconfigure(0, weight=1)
        self.rowconfigure(3, weight=1)
        self.mess.disp_mess( (  "FIC fimens:" + str(fimens) ) ) 
        self.fimens = fimens

        self.fludela = MeideeFludela(mess, out)

        Label(self, text="MeideeFludela - Fludela").grid(row=0, column=0, sticky='n')
        self.longeq = InterfaceLongeq(self, aster_objects, self.fludela, mess)
        self.param_phys = InterfaceParamPhys(self, self.fludela, mess )
        self.choix_modes = InterfaceChoixModes(self, aster_objects, self.param_phys,
                                               self.longeq, fimens, mess, self.fludela )

        self.longeq.grid( row=1, column=0, sticky='n'+'e'+'w'+'s' )
        self.param_phys.grid( row=2, column=0, sticky='n'+'e'+'w'+'s' )
        self.choix_modes.grid( row=4, column=0, sticky='n'+'e'+'w'+'s' )

        ##self.saved_data = {}


    def setup(self):
##        self.objects.recup_objects()
        self.longeq.refresh()
        self.choix_modes.refresh()
        
    def teardown(self):
        # XXX : sauvegarde des donnees
        pass


#-------------------------------------------------------------------------------

class InterfaceDisplay(Frame):
    """!Classe (Tab) d'affichage des résultats de calcul
    sous forme de tracé de courbes
    """

    def __init__(self, parent, fludela, mess):
        """!Constructeur

        \param parent Frame parente
        \param fludela interface fludela (InterfaceFludela)

        """
        Frame.__init__(self, parent,  **DEFAULT_FRAME_ARGS)
        self.mess = mess
        self.parent = parent
        self.fludela = fludela
        ##self.resultat = fludela.saved_data

        Label(self, text="Visualisation").grid(row=0, column=0, columnspan=3, sticky='n')

        self.plot = PlotCanvas( self, "300", "300", relief=SUNKEN, border=2,
                                zoom = 1, select = self.display)
        self.plot.grid(row=1, column=0)
        self.curves = {}
        curveframe = Frame(self)
        curveframe.grid(row=1, column=1)
        
        self.scroll = Scrollbar( curveframe, orient=VERTICAL )
        Label(curveframe, text="Tracés").grid(row=0, column=0, columnspan=2)
        self.curvelist = Listbox( curveframe,
                                  yscrollcommand=self.scroll.set,
                                  exportselection=False )
        self.curvelist.grid(row=1, column=0)
        self.scroll.grid(row=1, column=1)


        # Ajout d'un tracé
        editframe = Frame(self)
        editframe.grid(row=2, column=0)
        options = []
        self.field = StringVar()
        self.fieldchoice = MyMenu( editframe,
                                   options,
                                   self.field,
                                  )
        self.fieldchoice.grid(row=0, column=0)
        self.curvename = StringVar()
        self.modenum = StringVar()
        HLabelledItem( editframe, "Nom :", Entry, textvariable=self.curvename ).grid(row=0,column=1)
        HLabelledItem( editframe, "Mode :", Entry, textvariable=self.modenum ).grid(row=1,column=1)
        Button(editframe, text="Ajouter", command=self.add_curve ).grid(row=2, column=1)

    def refresh(self):
        """!Raffraichit l'interface lorsqu'on affiche le panneau"""
        self.mess.disp_mess( (  "InterfaceDisplay:refresh" +
                                str(self.fludela.saved_data) ) ) 
        self.resultat = self.fludela.saved_data
        k = self.resultat.keys()
        if k:
            data = self.resultat[k[0]]
            self.fieldchoice.update( data.keys(), self.field )
        self.curvelist.delete(0,END)
        for c in self.curves:
            self.curvelist.insert(END, c )

    def setup(self):
        self.refresh()

    def teardown(self):
        pass

    def read_results(self):
        """!Lecture du fichier contenant les résultats"""
        pass

    def draw(self):
        return
        self.mess.disp_mess( ( "Drawing..." ) ) 
        try:
            self.plot.clear()
            for curve in self.curves:
                self.plot.draw( curve.get_curve(), 'automatic', 'automatic' )

        except Exception, e:
            import traceback
            import sys
            traceback.print_exc(file=sys.stdout)
            self.mess.disp_mess( (  "Erreur dans draw (methode de InterfaceDisplay)" ) ) 

    def display(self, value):
        self.plot.select( value )

    def add_curve( self ):
        name = self.curvename.get()
        mode = int(self.modenum.get())
        field = self.field.get()
        if not name:
            name = "%s - %d" % (field,mode)
        self.curves[name] = Curve( name, self.resultat, field, mode )
        self.curvename.set("")
        self.refresh()


#-------------------------------------------------------------------------------

class Curve:
    """!Encapsule une courbe pour TkPlotCanvas"""
    def __init__(self, name, data, field, mode, mess):
        self.mess = mess
        self.name = name
        self.data = data
        self.field = field
        self.mode = mode

    def get_curve(self):
        speed = [ float(v) for v in self.data.keys() ]
        speed.sort()
        return PolyMarker( values, color='blue', fillcolor='blue', marker='circle' )



#######################################################
#                                                     #
# CLASSES ET FONCTIONS UTILITAIRES (FIMEN, LONGEQ...) #
#                                                     #
#######################################################

class Fimen:
    """!Fimen

    classe permettant la manipulation de fichier au format Fimen
    On va chercher dans un fichier fimen, décodé en ascii avant le calcul Aster
    les données modales à manipuler
    """

    def __init__(self, mess, name):
        self.mess = mess
        self.taue = 0.0
        self.nm = 0
        self.nbz = 0
        self.fzoom = 0.0   # fzoom
        self.nrf = 0       # nombre de references 
        self.n0rf = zeros( (self.nrf,), Int )
        self.freq = None
        self.xsi = 1.0*zeros( (self.nm,) )
        self.a = None
        self.b = None
        self.u = None
        self.v = None
        self.mess.disp_mess( "on ouvre le fichier fimen suivant : ")
        self.mess.disp_mess( name )
        self.mess.disp_mess( "  " )
        self.fimen = open(name, 'r')

    def lec_fimen(self):
        """ !Lecture d'un fichier fimen donne par l'unite fortran
            Le fichier est ascii, décodé en amont du code"""
        fimen = self.fimen
        liste = []
        fimen.readline()
        for ind_l in range(5):
            line = fimen.readline()
            liste.append(string.split(line)[-1:][0])
        (self.taue, self.nm, self.nbz, self.fzoom, self.nrf) = (string.atof(liste[0]),string.atoi(liste[1]),
                                                                string.atoi(liste[2]),string.atof(liste[3]),
                                                                string.atoi(liste[4]))
                                                                     
        
        # Caras modales
        for ind_l in range(3):
            line = string.split(fimen.readline())
        self.freq = [float(k) for k in line]
        self.freq = array(self.freq)
        for ind_l in range(2):
            line = string.split(fimen.readline())
        self.xsi = [float(k) for k in line]
        
        # Coefficients de participation
        # Matrice A :
        fimen.readline()
        self.a = 1.0*zeros( (self.nm,self.nrf))
        for i in range(self.nm):
            line = [float(j) for j in string.split(fimen.readline())]
            for j in range(self.nrf):
                self.a[i,j] = line[j]        
        fimen.readline()
        
        # Matrice B :
        fimen.readline()
        self.b = 1.0*zeros( (self.nm,self.nrf))
        for i in range(self.nm):
            line = [float(j) for j in string.split(fimen.readline())]
            for j in range(self.nrf):
                self.b[i,j] = line[j]
        fimen.readline()

        # Déformées modales complexes
        # Matrice U
        fimen.readline()
        self.u = 1.0*zeros( (self.nm,self.nbz))
        for i in range(self.nm):
            line = [float(j) for j in string.split(fimen.readline())]
            for j in range(self.nbz):
                self.u[i,j] = line[j]
        fimen.readline()        

        # Matrice V
        fimen.readline()
        self.v = 1.0*zeros( (self.nm,self.nbz))
        for i in range(self.nm):
            line = [float(j) for j in string.split(fimen.readline())]
            for j in range(self.nbz):
                self.v[i,j] = line[j]
        fimen.readline()         
                

    def global_granger( self, ind_ref=0, modes=None):
        """!Calcul de la frequence et de l'amortissement globaux pour un groupe de modes

        \return la fréquence et l'amortissement globaux
        
        Nécessite la lecture préalable du fichier binaire Fimen par la méthode
        lec_fimen.

        On utilise la formulation réelle approchee (<> formulation complexe)
        
        Si il y a plusieurs voies de référence et/ou que la voie de référence
        n'est pas la voie 1, on le renseigne en entrée de la fonction
        """
        nref = self.nrf
        freq, xsi = self.freq, self.xsi
        A,B,U,V = self.a, self.b, self.u, self.v

        if modes is None:
            modes = range(self.nm)
        freq = take( freq, modes )
        xsi = take( xsi, modes )
        f0 = 0.0
        xsi0 = 0.0
        for iref in range(nref):
            # Freq globale moyennee sur les voies de reference
            ponderation = A[:,iref]*U[:,iref] - B[:,iref]*V[:,iref]
            ponderation = take( ponderation, modes )
            f2 = sum( ponderation * freq**2 )/ sum( ponderation )
            if f2 < 0:
                self.mess.disp_mess("!!  Calcul global impossible par méthode Granger  !!")
                self.mess.disp_mess("!!          Essayer par une autre méthode         !!")
                self.mess.disp_mess("                                     ")
                return -1.0, -1.0
            f0 = f0 + sqrt( f2 )
            xsi0 = xsi0 + sum( ponderation * xsi) / sum( ponderation )

        f0 = f0/nref
        xsi0 = xsi0/nref

        return f0, xsi0        

