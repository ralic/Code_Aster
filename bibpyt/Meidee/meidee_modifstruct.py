#@ MODIF meidee_modifstruct Meidee  DATE 14/05/2008   AUTEUR BODEL C.BODEL 
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

from Meidee.meidee_correlation import GMSH_PATH, GMSH
from Meidee.meidee_cata import Resultat
from Meidee.meidee_iface import MyMenu

from Meidee.modes import ModeFreqList, SelectionNoeuds
from Meidee.modes import ParamModeIterSimult, ParamModeIterInv, ParamProjMesuModal
from Meidee.modes import OptionFrame, MacWindowFrame

from Meidee.meidee_calcul_modifstruct import ModifStruct


# Recuperation de deux UL disponibles pour les operations du superviseur GMSH
# TODO: proprifier ca si possible
_TUL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
_ULGMSH=_TUL['UNITE_LIBRE',1]
DEFI_FICHIER(FICHIER='TMP',UNITE=_ULGMSH)
_TUL2=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
_ULMAIL=_TUL2['UNITE_LIBRE',1]
DEFI_FICHIER(ACTION='LIBERER',UNITE=_ULGMSH)
DETRUIRE(CONCEPT = _F(NOM = (_TUL,_TUL2)), INFO=1)

        
class InterfaceModifStruct(Frame):
    """!Interface principale de l'outil de calcul de modification structurale
    
    """
    def __init__(self,
                 root,
                 meidee_objects,
                 macro,
                 mess,
                 outputs):
        """!Constructeur

        :IVariable:
         - `root`: fenetre parente
         - `meidee_objects`: objet Meidee, permettant d'accéder aux résultats aster
         - `mode_exp`: valeur associee au bouton de selection des modes experimentaux
         - `mode_etendu`: valeur associee au bouton de selection des modes etendus
         - `mode_nume`: valeur associee au bouton de selection des modes numeriques
         - `mode_nume_red`: valeur associee au bouton de selection des modes numeriques reduits
         - `use_nume_mass`: indicateur d'utilisation de la matrice de masse numerique
         - `proj_champ_meth`: methode a utiliser pour PROJ_CHAMP (SVD ou LU)
         - `proj_svd_param`: si methode SVD, alors parametre de selection
         - `mac_windows`: liste des fenetres d'affichage de MAC modes

        """
        Frame.__init__(self, root, relief='raised', borderwidth=4) # Premiere frame
        self.mess = mess
        self.root = root
        self.macro = macro
        self.meidee_objects = meidee_objects
        self.mode_exp = IntVar()
        self.mode_etendu = IntVar()
        self.mode_nume = IntVar()
        self.mode_nume_red = IntVar()
        self.use_nume_mass = IntVar()
        self.condens_meth = StringVar()
        self.modifstruct = ModifStruct(macro, meidee_objects, self.mess, outputs)
        self.grno_capt_old = None
        self.matr_rig_old = None
        self.macres_obj = None
        self.main = self
        self.font1 = tkFont.Font( family="Helvetica", size=16, weight="bold" )
        self.font2 = tkFont.Font( family="Helvetica", size=14, weight="bold" )
        self.interface_modifstruct()


    def setup(self):
        """!Appelee par le gestionnaire de tab lors de l'affichage

        """
        pass

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
        l = Label(self,text="Modification Structurale", pady=5, font=self.font1,bg="white" )
        l.grid(row=0,sticky='new')

        # panneau expansion
        # -----------------
        self.expansion = InterfaceExpansion(self, self.modifstruct)
        self.expansion.grid(row=1, column=0, sticky='wesn')

        # panneau condensation
        # --------------------
        self.condensation = InterfaceCondensation(self, self.modifstruct)
        self.condensation.grid(row=2, column=0, sticky='wesn')

    def expansion_completed(self):
        self.condensation.notify_expans_ok()

#-------------------------------------------------------------------------------
class InterfaceExpansion(Frame):
    def __init__(self, root, modifstruct):
        """!Creation de l'interface pour le calcul de condensation de la mesure

        """
        Frame.__init__(self, root, relief='ridge', borderwidth=4 )
        self.root = root
        self.modif_struct = modifstruct
        self.columnconfigure(0, weight=1)
        self.columnconfigure(1, weight=1)
        self.columnconfigure(2, weight=1)
        self.rowconfigure(1, weight=1)
        self.base_expansion = None # la base d'expansion calculee
        self.term_gmsh = []
        
        meidee_objects = self.root.meidee_objects
        # Déclaration des variables Tk
        self.var_resu_exp = StringVar()
        self.condens_meth = StringVar()
        self.var_modl_sup = StringVar()
        self.var_grno_capt= StringVar()
        self.var_grno_intf=StringVar()
        self.var_raid_name = StringVar()

        # -----------------------------------------------------------------
        # Titre
        #
        Label(self, text="Choix de la base d'expansion",bg="white", font=self.root.font2
              ).grid(row=0, column=0, columnspan=3, sticky="ew")

        # -----------------------------------------------------------------
        # Definition du modele support
        f = Frame(self)
        f.grid(row=1,column=0,sticky='nsew')

        Label(f,text="Modes expérimentaux").grid(row=0,column=0,sticky='w')
        self.menu_resu_exp = MyMenu( f, meidee_objects.get_resultats_num(),
                                     self.var_resu_exp,
                                     self.refresh_list_exp )
        self.menu_resu_exp.grid(row=0, column=1)


        
        # menu de selection du modele support
        Label(f,text="Modele support").grid(row=1,column=0,sticky='w' )
        self.menu_modl_sup = MyMenu( f, meidee_objects.get_model_name(),
                                     self.var_modl_sup,self.modele_support_changed )
        self.menu_modl_sup.grid(row=1, column=1, sticky='ew')

        # menu de selection de la matrice de raideur assemblee du modele support
        Label(f,text="Matrice raideur",justify='left').grid(row=2,column=0,sticky='w')
        self.menu_raid_name = MyMenu( f, meidee_objects.get_matr_name(),
                                      self.var_raid_name,
                                      self.mat_raideur_changed )
        self.menu_raid_name.grid(row=2, column=1,sticky='ew')


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




        # menu de selection du groupe de noeuds capteur
        self.capteur = SelectionNoeuds(f, "Noeuds et DDL capteur",
                                       bg='#90a090',command=self.capteur_changed )
        self.capteur.grid(row=4,column=0,columnspan=2,pady=3)
        
        # menu de selection du groupe de noeuds interface
        self.iface = SelectionNoeuds(f, "Noeuds et DDL interface",
                                     bg='#9090a0',command=self.iface_changed)
        self.iface.grid(row=5,column=0,columnspan=2,pady=3)

        Button(f,text="Valider", command=self.anything_changed).grid(row=6,
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
        self.button_reglage_lmme.grid(row=2,column=0)
        self.configure_param_lmme_dialog()


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
        obj_dict = CONTEXT.get_current_step().jdc.sds_dict
        modsup = self.var_modl_sup.get()
        # choix des matrices de raideur assemblees de ce modele support
        mat_asse = self.root.meidee_objects.masses.items()
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
        resu = self.root.meidee_objects.get_resu(resu_exp)
        self.liste_exp.set_resu( resu.obj )

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

        meidee_objects = self.modif_struct.meidee_objects
        matr_rig = meidee_objects.get_matr(self.var_raid_name.get())
        if matr_rig == None :
            disp_mess( ("Il faut selectionner une matrice raideur " \
                        "parmi celles proposées!") )
            return False
        else:
            self.modif_struct.set_stiffness_matrix(matr_rig)

        self.modif_struct.set_method_name(self.condens_meth.get())
        
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
     
        # Getting the frequency from the interface for the LMME method
        calc_freq = None
        if self.modif_struct.method_name == "LMME": 
            calc_freq=self.param_mode_iter_simult_lmme.get_calc_freq()
        
        self.modif_struct.calc_base_proj(calc_freq)
        self.liste_sup.fill_modes(self.modif_struct.calculated_modes)
        
        # on sauve le nom du modele sup utilisé
        # pour calculer la base d'expansion (pour affichage par GMSH)
        #self.base_expansion_modl = modlsup
        #self.base_expansion = self.modif_struct.base_expansion

        self.root.expansion_completed()

    def view_expansion(self, *args):
        """!Visualisation de la base d'expansion par GMSH.
        On utilise la classe GMSH de Stanley
        """
        if not self.modif_struct.base_expansion:
            return
        be = self.modif_struct.base_expansion
        #print be.dump(present=True,size=150)
        #modl = self.root.meidee_objects.get_model(self.modif_struct.support_modele.nom)
        #base_mod = Resultat(self.root.meidee_objects, be.nom,
        #                    be, self.root.mess, owned=False)


        IMPR_RESU( UNITE   = _ULGMSH, 
                   FORMAT  = 'GMSH',
                   MODELE  = self.modif_struct.support_modele.obj,
                   RESU    = _F( RESULTAT=be,
                                 TYPE_CHAM = 'VECT_3D',
                                 NOM_CMP = ('DX','DY','DZ')
                                ),
                   )
        DEFI_FICHIER(ACTION='LIBERER', UNITE=_ULGMSH)
        
        param = { 'mode' : 'LOCAL',
                  'SKIN' : 'NON',
                  'gmsh' : GMSH_PATH}
        filename = "fort.%d" % _ULGMSH
        term = GMSH('POST', filename, param, options={} )
        self.term_gmsh.append( term )

    def view_model_exp(self, *args):
        """!Visualisation de la base d'expansion par GMSH.
        On utilise la classe GMSH de Stanley
        """
        resu_exp = self.var_resu_exp.get()
        resu = self.root.meidee_objects.get_resu(resu_exp)
        
        IMPR_RESU( UNITE   = _ULGMSH, 
                   FORMAT  = 'GMSH',
                   MODELE  = resu.modele,
                   RESU    = _F(RESULTAT=resu.obj, TYPE_CHAM = 'VECT_3D',
                                NOM_CMP = ('DX','DY','DZ')),
                   )
        DEFI_FICHIER(ACTION='LIBERER', UNITE=_ULGMSH)
        
        param = { 'mode' : 'LOCAL',
                  'SKIN' : 'NON',
                  'gmsh' : GMSH_PATH}
        filename = "fort.%d" % _ULGMSH
        term = GMSH('POST', filename, param, options={} )
        self.term_gmsh.append( term )

#-------------------------------------------------------------------------------
class InterfaceCondensation(Frame):

    def __init__(self, root, modif_struct):
        """!Creation de l'interface pour le couplage des deux modeles : mesure condense / modification

        """
        Frame.__init__(self, root, relief='ridge', borderwidth=4 )
        self.root = root
        self.modif_struct = modif_struct
        
        mdo = self.modif_struct.meidee_objects
        mstruct = self.modif_struct

        # Déclaration des variables Tk
        # ----------------------------
        self.var_modl_modif = StringVar()
        self.var_mailx = StringVar()
        self.var_modlx = StringVar()

        # Titre du panneau
        # ----------------
        Label(self, text="Evaluation modele modification / modele condense",
              bg='#f0f0f0', font=self.root.font2,
              ).grid(row=0, column=0, columnspan=3, sticky='new')

##         main_param = Frame(root)
##         main_param.rowconfigure(1,weight=1)
##         main_param.columnconfigure(0,weight=1)
##         # les parametres vont dans 'f'
## 
##         # menu de selection du modele modification
##         f = Frame(main_param, relief='ridge', borderwidth=4 )
##         Label(f,text="Modele modification").grid(row=1,column=0,sticky='w'+'s'+'n' )
##         self.menu_modl_modif = MyMenu( f, self.meidee_objects.get_model_name(), self.var_modl_modif,self.choix_modif)
##         self.menu_modl_modif.grid(row=1, column=1)
## 
        self.columnconfigure(0,weight=1)
        self.columnconfigure(1,weight=1)
        self.columnconfigure(2,weight=1)
        self.rowconfigure(1,weight=1)
        
        # Frame contenant les infos pour la condensation
        # ----------------------------------------------
        f=Frame(self,relief='sunken', borderwidth=1)
        f.grid(row=1,column=0,sticky='new')

        Label(f, text="Condensation", bg="#f0f0f0").grid(row=0,column=0,
                                                         columnspan=2,
                                                         sticky="ew")
        
        Label(f, text="Modification").grid(row=1,column=0,columnspan=2)
        Label(f, text="Maillage").grid(row=2,column=0)
        self.menu_mailx = MyMenu( f, mdo.maillages.keys(),
                                  self.var_mailx, self.mailx_changed )
        self.menu_mailx.grid(row=2,column=1)

        Label(f, text="Modele").grid(row=3,column=0)
        self.menu_modlx = MyMenu( f, mdo.get_model_name(),
                                  self.var_modlx, self.mailx_changed )
        self.menu_modlx.grid(row=3,column=1)

        self.button_condensation = Button(f, text='Calculer',
                                          command=self.calc_condensation)
        self.button_condensation.grid(row=4,column=0, sticky="ew")

        self.button_display = Button(f, text='Voir',
                                     command=self.view_modes_couples, state='disabled' )
        self.button_display.grid(row=4, column=1, sticky='ew')

        # arametres de PROJ_MES_MODAL pour la condensation

        Label(f, text=" Paramètres de PROJ_MESU_MODAL",
              bg='#f0f0f0').grid(row=5,column=0,columnspan=2,sticky='new')
        
        self.var_expans_param_frame_visible = IntVar()
        Checkbutton(f,text="Réglages",
                    command=self.display_expans_param_frame,
                    variable=self.var_expans_param_frame_visible,
                    indicatoron=0).grid(row=6,column=1)

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
              bg='#f0f0f0').grid(row=0,column=0,columnspan=2,sticky='new')
        
        Label(f2, text="Mode de calcul").grid(row=1,column=0,sticky='w')
        self.var_meth_modes_couple = StringVar()
        self.menu_meth_modes_couple = MyMenu( f2, ['MODE_ITER_SIMULT','MODE_ITER_INV'],
                                              self.var_meth_modes_couple,
                                              self.choix_methode_modes_couple)
        self.menu_meth_modes_couple.grid(row=1,column=1,sticky='ew')
        self.var_meth_modes_couple.set("MODE_ITER_SIMULT")
        self.var_couplage_param_frame_visible = IntVar()
        Checkbutton(f2,text="Réglages",
                    command=self.display_couplage_param_frame,
                    variable=self.var_couplage_param_frame_visible,
                    indicatoron=0).grid(row=2,column=1)

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
        

        #calc_freq = self.param_retroprojection.get_calc_freq()

        
        # Affichage de MAC_MODE
        # ---------------------
        f = Frame(self,relief='sunken',borderwidth=1)
        f.grid(row=1,column=1,rowspan=2,sticky='nsew')
        #Label(f, text="Indicateur", bg='#f0f0f0').grid(row=0,column=0, sticky='new')
        f.rowconfigure(0, weight=1)
        f.columnconfigure(0, weight=1)

        self.mw = MacWindowFrame( f, "MAC MODES", "depl int", "depl xint" )
        self.mw.grid(row=0,column=0,sticky='nsew')
        #self.mac_canvas = Canvas(f)
        #self.mw = MacMode( self.mac_canvas )
        #self.mac_canvas.grid(row=1,column=0,sticky='nsew')

        return
        # Frame pour le calcul de la retroprojection
        # ------------------------------------------
        """cette partie permet de recalcumer une FRF en donnant
           une excitation. On s'en servira plus tard """
        f = Frame(self,relief='sunken',borderwidth=1)
        f.grid(row=1,column=2,sticky='nsew')

        Label(f, text="Retroprojection").grid(row=0,column=0)
        self.freq_debut = DoubleVar()
        self.freq_debut.set(5.)
        self.freq_fin = DoubleVar()
        self.freq_fin.set(20.)
        self.freq_nombre = IntVar()
        self.freq_nombre.set( 4 )
        opt = OptionFrame( f, "Frequences à calculer",
                           [ ( "Debut", Entry, { 'textvariable':self.freq_debut } ),
                             ( "Fin", Entry, { 'textvariable':self.freq_fin } ),
                             ( "Nombre", Entry, { 'textvariable':self.freq_nombre } ),
                             ]
                           )

        opt.grid(row=2,column=0)
        f2=Frame(f,relief='flat',borderwidth=0)
        Label(f2,text="Excitation").grid(row=0,column=0)
        f2.grid(row=3,column=0)
        self.var_chargement_retro = StringVar()
        self.menu_chargement = MyMenu(f2, [], self.var_chargement_retro, None )
        self.menu_chargement.grid(row=0,column=1)
        Button(f, text="Calculer", command=self.calc_retroprojection ).grid(row=4)
        
        #return main_param

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

    def calc_retroprojection(self):
        mstruct = self.root.modifstruct
        choix = self.var_meth_modes_couple.get()
        if choix=='MODE_ITER_SIMULT':
            mode_simult = 1
            calc_freq = self.param_simult_modes_couple.get_calc_freq()
            calc_freq['SEUIL_FREQ'] = 1e-4
        else:
            calc_freq = self.param_inv_modes_couple.get_calc_freq()
            mode_simult = 0

#        interv = ( self.freq_debut.get(), self.freq_fin.get(), self.freq_nombre.get() )
#        charg_name = self.var_chargement_retro.get()
#        charg = mstruct.cpl.concepts[charg_name] # on recupere le chargement recalcule sur le modele couple
#
        mstruct.modes_modele_couple(mode_simult, calc_freq)

    def notify_expans_ok(self):
        mdo = self.root.meidee_objects
        mstruct = self.root.modifstruct
    
    def update_chargement(self):
        charg = []
        for name, co in mstruct.cpl.concepts.items():
            if isinstance(co, char_meca):
                charg.append(name)
        self.menu_chargement.update(charg, self.var_chargement_retro, None)
        
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
        
        self.modif_struct.find_maillage_couple_from(self.var_mailx.get())
        self.modif_struct.find_modele_couple_from(self.var_modlx.get())
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
        def liste_modes( modes ):
            resu = Resultat(self.root.meidee_objects,modes.nom,
                            modes, self.root.mess, owned=False)
            afreq, axsi, amass, amodes, amor, arigi = resu.get_modes()
            return afreq
        self.mw.set_modes(liste_modes(modes1), liste_modes(modes2),
                          self.modif_struct.mac_val)
        self.button_display.configure(state='normal')

    def mailx_changed(self):
        # Do checks:
        # 1. mailx ok
        # 2. model ok
        # 3. modes identifies ok
        # 4. modes expansion ok
        pass

    def view_modes_couples(self, *args):
        """!Visualisation de la base d'expansion par GMSH.
        On utilise la classe GMSH de Stanley
        """
        mstruct = self.root.modifstruct
        mdo = self.root.meidee_objects
        IMPR_RESU( UNITE   = _ULGMSH, 
                   FORMAT  = 'GMSH',
                   MODELE  = mstruct.x_modlint,
                   RESU    = ( _F(RESULTAT=mstruct.x_deplint,
                                  TYPE_CHAM = 'VECT_3D', NOM_CMP = ('DX','DY','DZ')),
                               _F(RESULTAT=mstruct.x_deplxint,
                                  TYPE_CHAM = 'VECT_3D', NOM_CMP = ('DX','DY','DZ')),
                               )
                   )
        DEFI_FICHIER(ACTION='LIBERER', UNITE=_ULGMSH)
        
        param = { 'mode' : 'LOCAL',
                  'SKIN' : 'NON',
                  'gmsh' : GMSH_PATH}
        filename = "fort.%d" % _ULGMSH
        term = GMSH('POST', filename, param, options={} )
