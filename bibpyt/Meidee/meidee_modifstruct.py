#@ MODIF meidee_modifstruct Meidee  DATE 28/01/2010   AUTEUR BODEL C.BODEL 
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

# RESPONSABLE BODEL C.BODEL

## \package meidee_mac Module de gestion des calculs %Meidee avec Aster
#
# La classe InterfaceModifStruct dirige les objets graphiques
#

import tkFont
from Tkinter import Frame, Toplevel
from Tkinter import Label, Button, Entry, Checkbutton
from Tkinter import StringVar, IntVar, DoubleVar


import aster
from Accas import _F

from Cata.cata import char_meca, IMPR_RESU
from Cata.cata import MODE_ITER_INV, MODE_ITER_SIMULT
from Cata.cata import INFO_EXEC_ASTER, DEFI_FICHIER, DETRUIRE

from Meidee.meidee_cata import Resultat, DynaHarmo, ModeMeca
from Meidee.meidee_iface import MyMenu

from Meidee.modes import ModeFreqList, SelectionNoeuds, DispFRFDialogue
from Meidee.modes import ParamModeIterSimult, ParamModeIterInv, ParamProjMesuModal
from Meidee.modes import OptionFrame, MacWindowFrame

from Meidee.meidee_calcul_modifstruct import ModifStruct


class InterfaceModifStruct(Frame):
    """!Interface principale de l'outil de calcul de modification structurale
    
    """
    def __init__(self,
                 root,
                 meidee_objects,
                 macro,
                 mess,
                 outputs,
                 param_visu):
        """!Constructeur

        :IVariable:
         - `root`: fenetre parente
         - `meidee_objects`: objet Meidee, permettant d'accéder aux résultats aster
         - `macro`: self de la macro qui utilise cet objet
         - `mess`: fenetre de messages
         - `outputs`: concepts Aster de l'utilisateur a afficher en sortie

        """
        Frame.__init__(self, root, relief='flat',borderwidth=4) # Premiere frame
        self.mess = mess
        self.root = root
        self.macro = macro
        self.objects = meidee_objects
        self.param_visu = param_visu
        self.modifstruct = ModifStruct(macro, meidee_objects, self.mess, outputs)
        self.main = self
        self.font1 = tkFont.Font( family="Helvetica", size=16 )
        self.font2 = tkFont.Font( family="Helvetica", size=14 )
        self.interface_modifstruct()


    def setup(self):
        """!Appelee par le gestionnaire de tab lors de l'affichage

        """
        mdo = self.objects
        mdo.recup_objects()
        self.expansion.setup(mdo)
        self.condensation.setup(mdo)
        self.visu.setup(mdo)
        

    def teardown(self):
        """!Appelee par le gestionnaire de tab lors du masquage (passage a un autre tab)

        """
        pass

    def interface_modifstruct(self):
        """!Fonction principale de creation de l'interface modifstruct

        """
        self.columnconfigure(0, weight=1)
        self.rowconfigure(1, weight=1)
        #self.rowconfigure(2, weight=0)

        # titre
        # -----
        l = Label(self,text="Modification Structurale", pady=5, font=self.font1 )
        l.grid(row=0,sticky='new')

        # panneau expansion
        # -----------------
        self.expansion = InterfaceExpansion(self, self.modifstruct, self.param_visu, self.mess)
        self.expansion.grid(row=1, column=0, sticky='wesn')

        # panneau condensation
        # --------------------
        self.condensation = InterfaceCondensation(self, self.modifstruct, self.param_visu, self.mess)
        self.condensation.grid(row=2, column=0, sticky='wesn')

        # panneau visualisation
        # --------------------
        self.visu = InterfaceVisu(self, self.modifstruct, self.param_visu, self.mess)
        self.visu.grid(row=3, column=0, sticky='wesn')

    def expansion_completed(self):
        self.condensation.notify_expans_ok()

#-------------------------------------------------------------------------------
class InterfaceExpansion(Frame):


    def __init__(self, root, modifstruct, param_visu, mess):
        """!Creation de l'interface pour le calcul de condensation de la mesure

        """
        Frame.__init__(self, root, relief='sunken', borderwidth=1)
        self.root = root
        self.modif_struct = modifstruct
        self.param_visu = param_visu
        self.mess = mess
        self.columnconfigure(0, weight=1)
        self.columnconfigure(1, weight=1)
        self.columnconfigure(2, weight=1)
        self.rowconfigure(1, weight=1)
        self.base_expansion = None # la base d'expansion calculee
        self.term = []
        
        objects = self.root.objects
        # Déclaration des variables Tk
        self.var_resu_exp = StringVar()
        self.condens_meth = StringVar()
        self.var_modl_sup = StringVar()
        self.var_grno_capt= StringVar()
        self.var_grno_intf=StringVar()
        self.var_raid_name = StringVar()
        self.var_modlx = StringVar()
        self.sumail_name = StringVar()

        # -----------------------------------------------------------------
        # Titre
        #
        Label(self, text="Choix de la base d'expansion", font=self.root.font2
              ).grid(row=0, column=0, columnspan=3, sticky="ew")

        # -----------------------------------------------------------------
        # Definition du modele support
        f = Frame(self)
        f.grid(row=1,column=0,sticky='nsew',padx = 60)
        f.columnconfigure(0,weight=4)
        f.columnconfigure(0,weight=3)

        Label(f,text="Modes expérimentaux").grid(row=0,column=0,sticky='w')
        self.menu_resu_exp = MyMenu( f, objects.get_mode_meca_name(),
                                     self.var_resu_exp,self.refresh_list_exp )
        self.menu_resu_exp.grid(row=0, column=1, sticky = 'ew')


        
        # menu de selection du modele support
        Label(f,text="Modele support").grid(row=1,column=0,sticky='w' )
        self.menu_modl_sup = MyMenu( f, objects.get_model_name(),
                                     self.var_modl_sup,self.modele_support_changed )
        self.menu_modl_sup.grid(row=1, column=1, sticky='ew')

        # menu de selection de la matrice de raideur assemblee du modele support
        Label(f,text="Matrice raideur",justify='left').grid(row=2,column=0,sticky='w')
        self.menu_raid_name = MyMenu( f, objects.get_matr_name(),
                                      self.var_raid_name,
                                      self.mat_raideur_changed )
        self.menu_raid_name.grid(row=2, column=1, sticky = 'ew')


        Label(f, text="Méthode").grid(row=3,column=0,sticky='w')
        
        self.dic_condens_meth = {
            "ES" : "Expansion statique",
            "LMME" : "Expansion statique projetee",
            }

        self.menu_condens_meth = MyMenu( f, self.dic_condens_meth.keys(),
                                         self.condens_meth,
                                         self.condens_changed )
        self.menu_condens_meth.grid(row=3,column=1,sticky='ew')
        self.condens_meth.set("ES")


        # menu selection du modele modification
        Label(f, text="Modele modification").grid(row=4,column=0,sticky='w')
        self.menu_modlx = MyMenu( f, objects.get_model_name(),
                                  self.var_modlx, self.modele_modif_changed )
        self.menu_modlx.grid(row=4,column=1,sticky = 'ew')


        # menu de selection du groupe de noeuds capteur
        self.capteur = SelectionNoeuds(f, "Noeuds et DDL capteur",
                                       bg='#90a090',command=self.capteur_changed )
        self.capteur.grid(row=5,column=0,columnspan=2,pady=3,sticky='ew')
        
        # menu de selection du groupe de noeuds interface
        self.iface = SelectionNoeuds(f, "Noeuds et DDL interface",
                                     bg='#9090a0',command=self.iface_changed)
        self.iface.grid(row=6,column=0,columnspan=2,pady=3,sticky='ew')

        Label(f, text="Nom de la super maille : ").grid(row=7,column=0,sticky='w')
        Entry( f, textvariable=self.sumail_name).grid(row=7, column=1 )
        self.sumail_name.set("SUMAIL")

        Button(f,text="Valider", command=self.anything_changed).grid(row=8,
                                                                     column=1,
                                                                     sticky = 'e')        


        # -----------------------------------------------------------------
        # menu de selection des modes identifies experimentalement
        f = Frame(self)
        f.grid(row=1,column=1,sticky='nsew')
        f.rowconfigure(0, weight=1)
        self.liste_exp = ModeFreqList( f, "Modes du modèle experimental" )
        self.liste_exp.grid(row=0, column=0, columnspan=2, sticky='nsew')
        Button(f,text="Voir", command=self.view_model_exp).grid(row=1,
                                                                column=0,
                                                                columnspan=2)




        # -----------------------------------------------------------------
        # menu de selection de la methode pour
        # le calcul de la base d'expansion
        f = Frame(self)
        f.grid(row=1,column=2,sticky='nsew')
        f.rowconfigure(0, weight=1)
        self.liste_sup = ModeFreqList( f, "Base d'expansion" )
        self.liste_sup.grid(row=0, column=0, sticky='snew' )

        Button(f, text="Voir", command=self.view_expansion).grid(row=1,column=0)

        self.reglage_lmme_visible = IntVar()
        self.button_reglage_lmme = Checkbutton(f, text="Réglages LMME",
                                               variable=self.reglage_lmme_visible,
                                               command=self.show_param_dialog,
                                               state='disabled', indicatoron=0)

        self.reglage_lmme_visible.set(0)
        self.button_reglage_lmme.grid(row=1,column=1)
        self.configure_param_lmme_dialog()

    def setup(self,mdo):
        """ Actualisation des concepts dans les listes lorsqu'on a change de tab"""
        self.menu_resu_exp.update( mdo.get_mode_meca_name() , self.var_resu_exp , self.refresh_list_exp )
        self.menu_modl_sup.update( mdo.get_model_name() , self.var_modl_sup , self.modele_support_changed )
        self.menu_modlx.update(    mdo.get_model_name() , self.var_modlx    , self.modele_modif_changed )   
        
    def configure_param_lmme_dialog(self):
        w = Toplevel()
        w.protocol('WM_DELETE_WINDOW', self.hide_param_dialog)
        self.param_dlg = w
        w.withdraw() # cache la fenetre
        w.rowconfigure(0,weight=1)
        w.columnconfigure(0,weight=1)
        prm = ParamModeIterSimult(w, "Parametre de MODE_ITER_SIMULT pour la methode LMME",
                                  relief='sunken', borderwidth=2)
        prm.grid(row=0,column=0)
        self.param_mode_iter_simult_lmme = prm
        Button(w,text="Appliquer",command=self.anything_changed).grid(row=1,column=0)
        Button(w,text="OK",command=self.hide_param_dialog).grid(row=2,column=0)

    def show_param_dialog(self):
        state = self.reglage_lmme_visible.get()
        if state:
            self.param_dlg.deiconify()
        else:
            self.param_dlg.withdraw()

    def hide_param_dialog(self):
        self.anything_changed()
        self.reglage_lmme_visible.set(0)
        self.param_dlg.withdraw()

    def modele_support_changed(self):
        """Selectionne les matrices de raideur compatibles avec le modèle
        support sélectionné
        """
        obj_dict = CONTEXT.get_current_step().get_contexte_courant()
        modsup = self.var_modl_sup.get()
        # choix des matrices de raideur assemblees de ce modele support
        mat_asse = self.root.objects.matrices.items()
        mat_rigi = []
        for name, obj in mat_asse:
            LIME = obj.LIME.get()
            if not LIME:
                continue
            for k in LIME:
                obj = obj_dict[k.strip()]
                refe = obj.RERR.get()
                modl = refe[0].strip()  # Modele
                typ = refe[1].strip()   # Type matrice (Masse ou raideur)
                if typ=="RIGI_MECA" and modl==modsup:
                    mat_rigi.append( name )
        self.menu_raid_name.update( mat_rigi, self.var_raid_name, self.mat_raideur_changed )
        old_rigi = self.var_raid_name.get()
        if old_rigi not in mat_rigi:
            self.var_raid_name.set(mat_rigi[0])
        # choix des groupno capteur
        self.capteur.set_modele( obj_dict[modsup], obj_dict )
        self.iface.set_modele( obj_dict[modsup], obj_dict )

    def refresh_list_exp(self):
        resu_exp = self.var_resu_exp.get()
        resu = self.root.objects.get_mode_meca(resu_exp)
        self.liste_exp.set_resu( resu )

    def anything_changed(self):
        # if self.valid():
        self.refresh_list_sup()

    def condens_changed(self):
        meth = self.condens_meth.get()
        if meth=="ES":
            self.button_reglage_lmme['state'] = 'disabled'
        else:
            self.button_reglage_lmme['state'] = 'normal'
        self.anything_changed()
    
    def group_no_capteur_changed(self):
        """modif : on ne fait les calculs qu'une fois qu'on a appuye sur "Valider" """
        pass
##        self.anything_changed()
    
    def group_no_iface_changed(self):
        """modif : on ne fait les calculs qu'une fois qu'on a appuye sur "Valider" """
        pass
##        self.anything_changed()
    
    def mat_raideur_changed(self):
        self.anything_changed()
    
    def modele_modif_changed(self):
        self.anything_changed()
    
    def iface_changed(self):
        """modif : on ne fait les calculs qu'une fois qu'on a appuye sur "Valider" """
        pass
##        self.anything_changed()
    
    def capteur_changed(self):
        """modif : on ne fait les calculs qu'une fois qu'on a appuye sur "Valider" """
        pass
##        self.anything_changed())

    def _can_set_modif_struct_para(self):
        """Renvoit  True si tous les paramêtres pour calculer les modes
        de la structure modifiée ont pu être obtenu."""
        disp_mess = self.root.mess.disp_mess
        
        modlsup = self.var_modl_sup.get()
        self.modif_struct.find_support_modele_from(modlsup)

        objects = self.modif_struct.objects
        matr_rig = objects.get_matr(self.var_raid_name.get())
        if matr_rig == None :
            disp_mess( ("Il faut selectionner une matrice raideur " \
                        "parmi celles proposées!") )
            return False
        else:
            self.modif_struct.set_stiffness_matrix(matr_rig)

        self.modif_struct.set_method_name(self.condens_meth.get())
        self.modif_struct.set_sumail_name(self.sumail_name.get())

        grno_capt = self.capteur.get_selected()
        if not grno_capt :
            disp_mess( ("Il faut selectionner un GROUP_NO capteur " \
                         "parmi ceux proposes!") )
            return False
        else:
            self.modif_struct.set_captor_groups(grno_capt)
        
        grno_iface= self.iface.get_selected()
        self.modif_struct.set_ms_externe_groups(grno_iface)
        
        return True

    def refresh_list_sup(self):
        """!Rafraichit la liste des vecteurs de base d'expansion

        depend: var_raid_name, var_grno_capt

        """
        if not self._can_set_modif_struct_para():
            return

        self.modif_struct.get_mode_meca_modele_support()
        self.modif_struct.find_maillage_modif_from(self.var_modlx.get())
        self.modif_struct.find_maillage_support_from(self.var_modl_sup.get())
        self.modif_struct.find_modele_couple_from(self.var_modlx.get())
     
        # Getting the frequency from the interface for the LMME method
        calc_freq = None
        if self.modif_struct.method_name == "LMME": 
            calc_freq=self.param_mode_iter_simult_lmme.get_calc_freq()
        
        x_bsmo = self.modif_struct.calc_base_proj(calc_freq)
        self.liste_sup.set_resu(x_bsmo)
        
        # on sauve le nom du modele sup utilisé
        # pour calculer la base d'expansion (pour affichage par GMSH)
        #self.base_expansion_modl = modlsup
        #self.base_expansion = self.modif_struct.base_expansion

        self.root.expansion_completed()

    def view_expansion(self, *args):
        """!Visualisation de la base d'expansion par GMSH ou Salome
        """
        if not self.modif_struct.base_expansion:
            return
        be = self.modif_struct.base_expansion
        #print be.dump(present=True,size=150)
        #modl = self.root.objects.get_model(self.modif_struct.support_modele.nom)
        #base_mod = Resultat(self.root.objects, be.nom,
        #                    be, self.root.mess)

        # Ordres a afficher
        modes_expansion = self.liste_sup.selection()

        term = self.param_visu.visu_resu(resultat=be,
                                         nume_mode=modes_expansion)
        
        self.term.append( term )

    def view_model_exp(self, *args):
        """!Visualisation de la base d'expansion par GMSH ou Salome.
        """
        resu_exp = self.var_resu_exp.get()
        resu = self.root.objects.get_mode_meca(resu_exp)

        # Modes a afficher
        modes_ide = self.liste_exp.selection()
        
        term = self.param_visu.visu_resu(resultat=resu.obj,
                                         nume_mode=modes_ide)
        self.term.append( term )

#-------------------------------------------------------------------------------
class InterfaceCondensation(Frame):

    def __init__(self, root, modif_struct, param_visu, mess):
        """!Creation de l'interface pour le couplage des deux modeles : mesure condense / modification

        """
        Frame.__init__(self, root, relief='sunken', borderwidth=1 )
        self.root = root
        self.modif_struct = modif_struct
        self.param_visu = param_visu
        self.mess = mess
        
        mdo = self.modif_struct.objects
        mstruct = self.modif_struct

        # Titre du panneau
        # ----------------
        Label(self, text="Couplage modification / modele condense",
              bg='#f0f0f0', font=self.root.font2,
              ).grid(row=0, column=0, columnspan=3, sticky='new')

        self.columnconfigure(0,weight=1)
        self.columnconfigure(1,weight=1)
        self.columnconfigure(2,weight=1)
        self.rowconfigure(1,weight=1)
        

        # Parametres de PROJ_MES_MODAL pour le couplage
        # ----------------------------------------------
        f1 = Frame(self,relief='sunken',borderwidth=1)
        f1.rowconfigure(0,weight=1)
        f1.grid(row=1,column=0,sticky='nsew')

        Label(f1, text=" Paramètres de PROJ_MESU_MODAL",
              bg='#f0f0f0').grid(row=0,column=0,columnspan=2,sticky='new')
        
        self.var_expans_param_frame_visible = IntVar()
        Checkbutton(f1,text="Réglages",
                    command=self.display_expans_param_frame,
                    variable=self.var_expans_param_frame_visible,
                    indicatoron=0).grid(row=1,column=1,pady=5,sticky='e')

        self.expans_param_frame = frm1 = Toplevel()
        frm1.rowconfigure(0,weight=1)
        frm1.columnconfigure(0,weight=1)

        self.param_proj_mesu = ParamProjMesuModal(frm1, "Paramètres de PROJ_MESU_MODAL")
        self.param_proj_mesu.grid(row=0,column=0,sticky='nsew')

        frm1.protocol("WM_DELETE_WINDOW", self.hide_expans_param_frame)
        Button(frm1,text="OK",command=self.hide_expans_param_frame).grid(row=1,column=0)
        frm1.withdraw()


        # Frame de parametrage du couplage : calcul modal
        # -----------------------------------------------
        f2 = Frame(self,relief='sunken',borderwidth=1)
        f2.rowconfigure(0,weight=1)
        f2.grid(row=2,column=0,sticky='nsew')

        Label(f2, text="Calcul modal sur le modèle couplé",
              bg='#f0f0f0').grid(row=0,column=0,columnspan=3,sticky='new')
        
        Label(f2, text="Mode de calcul ").grid(row=1,column=0,sticky='w')
        self.var_meth_modes_couple = StringVar()
        self.menu_meth_modes_couple = MyMenu( f2, ['MODE_ITER_SIMULT','MODE_ITER_INV'],
                                              self.var_meth_modes_couple,
                                              self.choix_methode_modes_couple)
        self.menu_meth_modes_couple.grid(row=1,column=2,sticky='ew')
        self.var_meth_modes_couple.set("MODE_ITER_SIMULT")
        self.var_couplage_param_frame_visible = IntVar()
        Checkbutton(f2,text="Réglages",
                    command=self.display_couplage_param_frame,
                    variable=self.var_couplage_param_frame_visible,
                    indicatoron=0).grid(row=2,column=2,pady=5,sticky='e')

        self.couplage_param_frame = frm2 = Toplevel()
        frm2.rowconfigure(0,weight=1)
        frm2.columnconfigure(0,weight=1)
        self.param_inv_modes_couple = ParamModeIterInv(frm2, "Modele couple")
        self.param_inv_modes_couple.grid(row=0,column=0,sticky='nsew')
        self.param_inv_modes_couple.grid_remove()

        self.param_simult_modes_couple = ParamModeIterSimult(frm2, "Modele couple")
        self.param_simult_modes_couple.grid(row=0,column=0,sticky='nsew')

        frm2.protocol("WM_DELETE_WINDOW", self.hide_couplage_param_frame)
        Button(frm2,text="OK",command=self.hide_couplage_param_frame).grid(row=1,column=0)
        frm2.withdraw()
        

        # Frame pour calcul de critere de qualite de la base d'expansion
        # -----------------------------------------------
        f3 = Frame(self,relief='sunken',borderwidth=1)
        f3.rowconfigure(0,weight=1)
        f3.grid(row=3,column=0,sticky='nsew')

        Label(f3, text="Qualite de la base expansion",
              bg='#f0f0f0').grid(row=0,column=0,columnspan=2,sticky='new')
        
        self.dic_mac_meth = {
            "MAC" : "Calcul de MAC classic",
            "IERI" : "Critere IERI",
            }

        Label(f3, text="Critere").grid(row=1,column=0,sticky='w')
        self.mac_meth = StringVar()
        self.menu_mac_meth = MyMenu( f3, self.dic_mac_meth.keys(),
                                         self.mac_meth,
                                         self.mac_changed )
        self.menu_mac_meth.grid(row=1,column=1,sticky='ew')

        Label(f3, text="Ponderation").grid(row=2,column=0,sticky='w')
        self.mac_ponder = StringVar()
        self.menu_ponder = MyMenu( f3, ['SANS','RIGIDITE','MASSE'],
                                              self.mac_ponder,
                                              self.choix_ponder)
        self.menu_ponder.grid(row=2,column=1,sticky='ew')


        f4 = Frame(self,relief='sunken',borderwidth=1)
        f4.rowconfigure(0,weight=1)
        f4.grid(row=4,column=0,sticky='nsew')

        self.button_condensation = Button(f4, text='Calculer',
                                          command=self.calc_condensation)
        self.button_condensation.grid(row=0,column=0, sticky="ew")

        self.button_display = Button(f4, text='Voir',
                                     command=self.view_modes_couples, state='disabled' )
        self.button_display.grid(row=0, column=1, sticky='ew')


        
        # Affichage de MAC_MODE
        # ---------------------
        f = Frame(self,relief='sunken',borderwidth=1)
        f.grid(row=1,column=1,rowspan=5,sticky='nsew')
        #Label(f, text="Indicateur", bg='#f0f0f0').grid(row=0,column=0, sticky='new')
        f.rowconfigure(0, weight=1)
        f.columnconfigure(0, weight=1)

        self.mw = MacWindowFrame( f, "Critere de qualite de la base", "Frequences propres", "(structure modifiee)" )
        self.mw.grid(row=0,column=0,sticky='nsew')
        #self.mac_canvas = Canvas(f)
        #self.mw = MacMode( self.mac_canvas )
        #self.mac_canvas.grid(row=1,column=0,sticky='nsew')

        return

    def setup(self,mdo):
        pass

    def display_couplage_param_frame(self):
        state = self.var_couplage_param_frame_visible.get()
        if state:
            self.couplage_param_frame.deiconify()
        else:
            self.couplage_param_frame.withdraw()

    def display_expans_param_frame(self):
        state = self.var_expans_param_frame_visible.get()
        if state:
            self.expans_param_frame.deiconify()
        else:
            self.expans_param_frame.withdraw()

    def hide_couplage_param_frame(self):
        self.var_couplage_param_frame_visible.set(0)
        self.couplage_param_frame.withdraw()

    def hide_expans_param_frame(self):
        self.var_expans_param_frame_visible.set(0)
        self.expans_param_frame.withdraw()

    def choix_methode_modes_couple(self):
        choix = self.var_meth_modes_couple.get()
        if choix=='MODE_ITER_SIMULT':
            self.param_inv_modes_couple.grid_remove()
            self.param_simult_modes_couple.grid()
        else:
            self.param_simult_modes_couple.grid_remove()
            self.param_inv_modes_couple.grid()

    def notify_expans_ok(self):
        mdo = self.root.objects
        mstruct = self.root.modifstruct
    
    def _can_set_modif_struct_para(self):
        """Renvoit True si tous les paramêtres pour lancer le calcul
        sur la stucture modifiée ont pu être obtenu."""
        expans = self.root.expansion
        disp_mess = self.root.mess.disp_mess
        
        # resultat experimentaux
        resu_exp_name = expans.var_resu_exp.get()  
        self.modif_struct.find_experimental_result_from(resu_exp_name)
        # caracteristiques du modele support
        # Sould not it be done just one time? 
        modlsup_name = expans.var_modl_sup.get()
        self.modif_struct.find_support_modele_from(modlsup_name)
        
        # XXX Finally kassup and matr_rig are the same?
        #kassup = mdo.get_matr(expans.var_raid_name.get())
        
        grno_capt = expans.capteur.get_selected()
        self.modif_struct.set_captor_groups(grno_capt)
        grno_iface = expans.iface.get_selected()
        self.modif_struct.set_ms_externe_groups(grno_iface)
        
        modes_ide = expans.liste_exp.selection()
        if not modes_ide:
            disp_mess("Il faut sélectionner des modes " \
                      "du modèle experimental pour la condensation")
            return False
        else:
            self.modif_struct.set_modes_ide(modes_ide)

        modes_expansion = [int(m) for m in expans.liste_sup.selection()]
        if not modes_expansion:
            disp_mess("Il faut sélectionner des modes " \
                      "de la base d'expansion pour la condensation")
            return False
        else:
            self.modif_struct.set_modes_expansion(modes_expansion)
        
        choix = self.var_meth_modes_couple.get()
        self.modif_struct.set_coupling_method_name(choix)

        if not self.modif_struct._can_get_nume_support_model():
            return False
            
        return True

    def calc_condensation(self):
        """Lance le calcul de condensation sur la structure modifiée.""" 
        self.button_display.configure(state='disabled')
        self.mw.clear_modes()
        
        if not self._can_set_modif_struct_para():
            return

        reso = self.param_proj_mesu.get_resolution()
        self.modif_struct.set_param_condens(reso)
        self.modif_struct.calcul_condensation()

        if self.modif_struct.coupling_method_name=='MODE_ITER_SIMULT':
            mode_simult = 1
            calc_freq = self.param_simult_modes_couple.get_calc_freq()
            calc_freq['SEUIL_FREQ'] = 1e-4
        else:
            calc_freq = self.param_inv_modes_couple.get_calc_freq()
            mode_simult = 0
        self.modif_struct.calcul_coupling_model_modes(mode_simult, calc_freq)
    
        self._show_modes()

    def _show_modes(self):
        """Montre dans l'interface Tk les modes trouvés
        sur la structure modifiée."""
        modes1 = self.modif_struct.x_deplint
        modes2 = self.modif_struct.x_deplxint

        freq1 = modes1.get_modes_data()['FREQ']
        freq2 = modes2.get_modes_data()['FREQ']

        if self.modif_struct.mac_val :
            self.mw.set_modes(freq1,freq2,self.modif_struct.mac_val)
        self.button_display.configure(state='normal')

    def mac_changed(self):
        self.button_display['state'] = 'disabled'
        self.modif_struct.set_mac_method(self.mac_meth.get())
    
    def choix_ponder(self):
        self.button_display['state'] = 'disabled'
        self.modif_struct.set_mac_ponder(self.mac_ponder.get())

    def view_modes_couples(self, *args):
        """!Visualisation de la base d'expansion par GMSH ou Salome.
        """
        mstruct = self.root.modifstruct
        term = self.param_visu.visu_resu( resultat=[mstruct.x_deplint, mstruct.x_deplxint])
        self.term.append( term )






#-------------------------------------------------------------------------------
class InterfaceVisu(Frame):

    def __init__(self, root, modif_struct, param_visu, mess):
        """!Creation de l'interface pour visualisation 
            et compariaison modele initial / modele modifie

        permet de choisir d'afficher les modes avec gmsh
        gère la compatibilité des choix de l'utilisateur (les calculs de MAC
        ne sont pas tous possibles)

        """
        Frame.__init__(self, root, relief='sunken', borderwidth=1 )
        self.root = root
        self.modif_struct = modif_struct
        self.param_visu = param_visu
        self.mess = mess
        self.term = []

        self.var_rep_freq = StringVar()
        self.dyh_mesu = None

        self.columnconfigure(0, weight=1)
        self.columnconfigure(1, weight=1)
        self.columnconfigure(2, weight=1)
        self.rowconfigure(1, weight=1)

        mdo = self.modif_struct.objects

        # Titre du panneau
        # ----------------
        Label(self, text="Comparaison structure initiale / structure modifiee",
              bg='#f0f0f0', font=self.root.font2,
              ).grid(row=0, column=0, columnspan=3, sticky='new')

        f1 = Frame(self)
        f1.grid(row=1,column=1)

        Label(f1,text="Réponse mesurée").grid(row=1,column=0,sticky='w')
        self.menu_rep_freq = MyMenu( f1, mdo.get_dyna_harmo_name(),
                                     self.var_rep_freq,
                                     self.refresh_list_rep )
        self.menu_rep_freq.grid(row=1, column=1, sticky = 'ew')

        f2 = Frame(self)
        f2.grid(row=1,column=2)

        self.phi_button = Button(f2,text='Déformées',command=self.view_modes)
        self.phi_button.grid(row=0,column=2, sticky='ew')
        self.frf_button = Button(f2,text='Réponses fréquentielles',command=self.view_frf)
        self.frf_button.grid(row=1,column=2, sticky='ew' )

    def setup(self,mdo):
        pass

    def refresh_list_rep(self):
        if not self.var_rep_freq:
            return
        rep_freq = self.var_rep_freq.get()
        resu = self.root.objects.get_dyna_harmo(rep_freq)
        self.dyh_mesu = resu
        self.var_rep_freq.set(resu.nom),

    def view_modes(self, *args):
        """!Visualisation des modes par GMSH ou Salome.
        """
        disp_mess = self.root.mess.disp_mess
        if not self.modif_struct.is_valid():
            disp_mess("Resultats sur la structure modifiee non disponibles!")
            return
        resu1 = self.modif_struct.x_mide
        resu2 = self.modif_struct.modes_retr

        modlexp = self.modif_struct.resu_exp.modele

        term = self.param_visu.visu_resu( resultat=[resu1, resu2])
        
        self.term.append( term )


    def view_frf(self):
        """lancement d'une fenetre de visualisation des frf"""
        disp_mess = self.root.mess.disp_mess
        if not self.modif_struct.is_valid():
            disp_mess("Resultats sur la structure modifiee non disponibles!")
            return
        mdo = self.modif_struct.objects

        # resu1 : reponse frequentielle mesuree
        resu1 = self.dyh_mesu
        # il faut donner a resu2 un mode meca pour que DispFRF calcule la rep freq
        resu2 = self.modif_struct.modes_couple
        sumail = self.modif_struct.sumail
        
        fenetre = DispFRFDialogue(self.root.mess, mdo, self.param_visu, resu1, resu2, sumail)

