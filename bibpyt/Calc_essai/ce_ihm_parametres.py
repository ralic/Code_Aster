# coding=utf-8
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
# person_in_charge: albert.alarcon at edf.fr

# La classe InterfaceParametres gere les options et les logiciels de Visualisation

from Utilitai.Utmess import UTMESS

import sys
import aster


from Calc_essai.cata_ce import  CaraElem, InterSpectre, CalcEssaiObjects, Resultat, ModeMeca, DynaHarmo


from Accas import _F
import weakref
import os
import time
import tkFont


from Tkinter import Tk, Frame, Menubutton, Menu, StringVar, IntVar, BooleanVar, Listbox
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Canvas, Toplevel
from Tkinter import NORMAL, DISABLED
from Calc_essai.outils_ihm import XmgrManager, MyMenu, MacWindowFrame, StudyList,DispFRFDialogue, DispObs
from Utilitai.Table import Table
from Cata.cata import EXEC_LOGICIEL
from Calc_essai.ce_calcul_expansion import make_mac_salome, make_mesh_mac,CalcEssaiExpansion

try:
    from Stanley.gmsh import GMSH
except ImportError:
    # Mode standalone
    from gmsh import GMSH

########################
#                      #
#  CLASSES GRAPHIQUES  #
#                      #
#######################


#------------------------------------------------------------------------------------------------------
def StateActivate(l_widget):
    """! Active tous les widgets de la liste l_widget"""
    for widget in l_widget:
        widget.config(state=NORMAL)

def StateDesactivate(l_widget):
    """! Desactive tous les widgets de la liste l_widget"""
    for widget in l_widget:
        widget.config(state=DISABLED)



class InterfaceParametres(Frame):

    """!Interface principale de l'outil de projection des modes
    expérimentaux sur les modes numériques

    permet la sélection et de calcul des modes en air et écoulement"""

    def __init__(self,
                 root,
                 objects,
                 macro,
                 mess,
                 ):
        """!Constructeur
        """


        Frame.__init__(self, root, relief='sunken', borderwidth=1) # Première frame

        self.mess = mess
        self.root = root
        self.logiciel = StringVar()
        self.salome_port = IntVar()
        self.machine_locale_name = self.get_machine_name()
        self.type_visu = StringVar()
        self.user = StringVar()
        self.protocole = StringVar()
        self.machine_name = self.machine_locale_name
        self.salome_widgets  = []
        self.distant_widgets = []
        self.protocole_ok = None
        self.logiciel_courbes = None

        self.macro = macro
        self.objects = objects
        self.param_visu = self
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
        self.resu_num = None   # base d'expansion (instance de ModeMeca)
        self.resu_exp = None   # donnees exp (instance de ModeMeca ou de DynaHarmo)
        self.interface_param()


    def setup(self):
        """!Appelée par le gestionnaire de tab lors de l'affichage"""

        mdo = self.objects
        mdo.recup_objects()

        self.menu_resu1.update( mdo.get_resultats_name(), self.var_resu1, self.visu1_changed )
        self.menu_resu2.update( mdo.get_resultats_name(), self.var_resu2, self.visu2_changed )
        pass

    def teardown(self):
        """!Appelée par le gestionnaire de tab lors
           du masquage (passage à un autre tab)"""
        return

    def _observabilite_changed(self):
        nom_resu = self.nom_obs_resu.get()
        if nom_resu.strip() !='Choisir':
            resu = self.objects.get_resultats(nom_resu)

        nom_modele = self.nom_obs_modele.get()
        if nom_modele.strip() != 'Choisir':
            modele = self.objects.get_model(nom_modele)
            self.obs_noeuds.set_resultat(resu.modele)
            self.obs_mailles.set_resultat(resu.modele)


    def interface_param(self):
        """!Fonction principale de création de l'interface"""

        self.columnconfigure(0, weight=1)
        self.rowconfigure(2, weight=1)
        l = Label(self,text=u" CALC_ESSAI : Paramètres de visualisation",
                  padx = 270, pady=5, font=("Helvetica", "16") )
        l.grid(row=0, sticky='nsew')

        select_box = self.interface_parametres(self)
        select_box.grid(row=1, sticky='nsew')

        visu_param = self.interface_visu(self)
        visu_param.grid(row=3, sticky='nsew')

        self.main = self


    def interface_selection(self, root):

        self.var_resu_num = StringVar()
        self.menu_resu_num = MyMenu( f, options = self.objects.get_mode_meca_name(),
                                     var = self.var_resu_num, cmd = self.num_changed )


        self.var_resu_exp = StringVar()
        self.menu_resu_exp = MyMenu( f, options = self.objects.get_resultats_name(),
                                     var = self.var_resu_exp, cmd = self.exp_changed )

        return f



    def interface_visu(self,root):
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

        Label(f1,text=u"Résultat 1").grid(row=0,column=1,sticky='w')
        self.var_resu1 = StringVar()
        self.menu_resu1 = MyMenu( f1, options = mdo.get_resultats_name(),
                                  var = self.var_resu1, cmd = self.visu1_changed )
        self.menu_resu1.grid(row=0, column=2, sticky='ew',padx=20)

        Label(f1,text=u"Résultat 2").grid(row=1,column=1,sticky='w')
        self.var_resu2 = StringVar()
        self.menu_resu2 = MyMenu( f1, options = mdo.get_resultats_name(),
                                  var = self.var_resu2, cmd = self.visu2_changed )
        self.menu_resu2.grid(row=1, column=2, sticky='ew',padx=20)

        f2 = Frame(f)
        f2.grid(row=1,column=1)


        self.mac_button = Button(f2,text="    MAC    ",command=self.view_macs,state='disabled')
        self.mac_button.grid(row=1,column=2, sticky='ew' )
        self.phi_button = Button(f2,text=u"Déformées",command=self.view_modes,state='disabled')
        self.phi_button.grid(row=2,column=2, sticky='ew')
        self.frf_button = Button(f2,text="    FRF    ",command=self.view_frf)
        self.frf_button.grid(row=3,column=2, sticky='ew' )
        self.frf_button = Button(f1,text=" Observation ",command=self.view_obs_1)
        self.frf_button.grid(row=0,column=3, sticky='ew' )
        self.frf_button = Button(f1,text=" Observation ",command=self.view_obs_2)
        self.frf_button.grid(row=1,column=3, sticky='ew' )

        return f

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

    def  view_obs_1(self):
        if self.is_resu1.get():
            self.view_obs(self.var_resu1)
        else :
            self.mess.disp_mess(u"Choisir un résultat")
            return
        self.setup()


    def  view_obs_2(self):
        if self.is_resu2.get():
            self.view_obs(self.var_resu2)
        else :
            self.mess.disp_mess(u"Choisir un résultat")
            return
        self.setup()


    def view_obs(self,var_resu):
        """lancement d'une fenetre d'observation"""
        mdo = self.objects
        resu = mdo.get_resultats(var_resu.get())
        fenetre = DispObs(self,self.mess,self.objects,resu)
        fenetre.set_resu(resu.nom)


    def view_modes(self, *args):
        """!Visualisation des modes par GMSH ou Salome"""

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
        return


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

        self.param_visu.visu_mac(mac,resu1,resu2)


    def activate_salome_widgets(self):
        StateActivate(self.salome_widgets)

    def desactivate_salome_widgets(self):
        StateDesactivate(self.salome_widgets)

    def interface_parametres(self, root):
        """!Création de l'interface de choix des logiciels de visualisation
        On permet à l'utilisateur de choisir Gmsh/Xmgrace ou Salome
        """

        main_param = Frame(root)
        main_param.rowconfigure(1,weight=1)
        main_param.columnconfigure(0,weight=1)

        f = Frame(main_param, relief='sunken', borderwidth=1 )
        # les parametres vont dans 'f'
        logiciels_frame = Frame(f, borderwidth=4)

        label_parametres_salome = Label(logiciels_frame, text=u"Paramètres Salome")
        label_parametres_salome.grid(row=2, column=1, columnspan=2)
        self.salome_widgets.append(label_parametres_salome)

        label_port = Label(logiciels_frame, text=u"Port")
        label_port.grid(row=3, column=1, sticky='w')
        self.salome_widgets.append(label_port)

        entry_salome_port = Entry(logiciels_frame, textvariable=self.salome_port)
        entry_salome_port.grid(row=3, column=2)
        self.salome_widgets.append(entry_salome_port)
        self.salome_port.set(self.get_runnig_salome_port())
        self.ce_salome = None

        liste_etudes = StudyList(logiciels_frame, self, u"choix de l'étude Salomé")
        liste_etudes.grid(row=4, column=2, sticky='w')
        self.salome_widgets.append(liste_etudes.liste)
        self.salome_widgets.append(liste_etudes.titre)
        liste_etudes.actualiser()


        label_choix_logiciel = Label(logiciels_frame, text=u"Choix du logiciel")
        label_choix_logiciel.grid(row=0, column=0, columnspan=3 )
        button_gmsh = Radiobutton(logiciels_frame, text=u"Gmsh/Xmgrace", value="Gmsh/Xmgrace", variable=self.logiciel,
                                  command = self.desactivate_salome_widgets  )
        button_gmsh.grid(row=1, column=0, sticky='w')

        button_salome = Radiobutton(logiciels_frame, text=u"Salomé", value="Salome", variable=self.logiciel,
                                    command = self.activate_salome_widgets )
        button_salome.grid(row=2, column=0, rowspan=3, sticky='w')


        self.logiciel.set("Salome")

        logiciels_frame.grid(row=1)

        f.grid(row=1, sticky='w'+'e'+'s'+'n')
        return main_param


    def get_user(self):
        import getpass
        user = getpass.getuser()
        return user


    def get_machine_name(self):
        """! Recupere le nom de la machine distance pour les parametres corba"""
        # on retourne le nom de la machine locale
        import socket
        machine_name = socket.gethostname()
        return machine_name

    def is_salome_launched(self):
        """! Determine si Salome est lance"""
        ok = False
        ret = os.system("ps auxw | grep -v grep | grep omniNames > /dev/null")
        if ret != 256:
            # Salome est lance
            ok = True
        return ok

    def get_runnig_salome_port(self):
        """! Recupere le port CORBA sur lequel est lance Salome pour les parametres corba"""
        salome_port = 2810
        if self.is_salome_launched():
            try:
                cmd = "ps auxw | grep -v grep | grep omniNames"
                p = os.popen(cmd)
                data = p.read()
                # On recupere la derniere ligne de ps pour avoir le dernier numero de port
                l_data = data.split("\n")
                last_line = l_data[-2]
                omniNames_params = last_line.split(" ")
                idx = omniNames_params.index("-start") + 1
                salome_port = int(omniNames_params[idx])
            except:
                msg =  u"Problème lors de la détermination du port Salome.\n"
                msg += u"Veuillez déterminer manuellement le port de Salome, en tapant ceci dans l'interpréteur python embarqué de Salome:\n"
                msg += u"import NSparam\n"
                msg += u"NSparam.getNSparams()"
                self.mess.disp_mess(msg)
        return salome_port


    def save_parameters(self, do_check_protocole=True):
        """! Sauvegarde les parametres dans une classe parente pour qu'ils soient communs a tous les onglets """
        self.machine_name = self.machine_locale_name


    def get_logiciel(self):
        self.save_parameters()
        if self.logiciel.get() == "Gmsh/Xmgrace":
            return CalcEssaiGmsh(self.mess)
        else:
            if self.ce_salome:return self.ce_salome
            else: return CalcEssaiSalome(self.mess, self.machine_name, self.salome_port.get(),
                                         self.user.get(), self.protocole.get(),
                                         self.protocole_ok, self)
        pass

    def get_logiciel_courbes(self):
        # Les courbes sont transferees par CORBA
        # => Pas besoin de verifier le protocole rcp/scp
        self.save_parameters(do_check_protocole=False)
        if self.logiciel.get() == "Gmsh/Xmgrace":
            return CalcEssaiXmgrace()
        else:
            if self.ce_salome_courbes:return self.ce_salome_courbes
            else:return CalcEssaiSalomeCourbes(self.mess, self.machine_name, self.salome_port.get(),self)
        pass

    def visu_studylist(self):
        self.ce_salome = CalcEssaiSalome(self.mess, self.machine_name, self.salome_port.get(),
                                         self.user.get(), self.protocole.get(),
                                         self.protocole_ok, self)

        self.ce_salome_courbes = CalcEssaiSalomeCourbes(self.mess, self.machine_name, self.salome_port.get(),self)

        studylist = self.ce_salome.studylist()
        return studylist

    def set_study(self, study):
        self.ce_salome.study_name = study
        self.ce_salome_courbes.study_name = study
        self.mess.disp_mess(u"Les courbes et vues seront affichées dans l'étude Salomé "+study)

    # fonction proxy vers le bon logiciel
    def visu_resu(self,resultat, nume_mode=None):
        logiciel = self.get_logiciel()
        self.param_visu.type_visu.set('deformee')
        term = logiciel.visu_resu(resultat, nume_mode)
        return term

    def visu_mac(self,mac,resu1,resu2):
        logiciel = self.get_logiciel()
        term = logiciel.visu_mac(mac,resu1,resu2)
        return term

    def visu_courbe(self, l_x, ll_y, couleur=None, titre='Courbe', l_legende=None,
                    legende_x="Abscisses",legende_y="Ordonnées",
                    unite_x="ua",unite_y="ua"):
        self.logiciel_courbes = self.get_logiciel_courbes()
        self.logiciel_courbes.affiche(l_x, ll_y,
                                      couleur, titre,
                                      l_legende,
                                      legende_x, legende_y,
                                      unite_x,unite_y)
        pass


    def quit(self):
        for term in self.term:
            if term is not None:
                term.Fermer()




##############################################################
# Classes specifiques pour chaque logiciel de post-traitement
##############################################################

from Cata.cata import INFO_EXEC_ASTER, DEFI_FICHIER, DETRUIRE, IMPR_RESU

# Classe abstraite
class CalcEssaiLogiciel(object):

    def __init__(self, mess):
        self.mess = mess
        self.impr_resu_format = None
        self.impr_resu_params = None
        self.unite_logique = None

    def get_impr_resu_format(self):
        return self.impr_resu_format

    def get_impr_resu_params(self):
        return self.impr_resu_params

    def get_unite_logique(self):
        _TUL = INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
        self.unite_logique = _TUL['UNITE_LIBRE',1]
        DETRUIRE(CONCEPT = _F(NOM = (_TUL)), INFO=1)
        pass

    def defi_fichier(self):
        self.get_unite_logique()

        # On efface le fichier si il existe deja
        if os.path.exists('fort.'+str(self.unite_logique)):
            try:
                os.remove('fort.'+str(self.unite_logique))
            except:
                pass

        DEFI_FICHIER(UNITE = self.unite_logique,
                     TYPE  = 'LIBRE',)
        pass

    def libere_fichier(self):
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.unite_logique)
        pass

    def get_nom_fichier(self):
        filename = "fort.%d" % self.unite_logique
        return filename

    # @param resultat (multitype): on peut imprimer un ou plusieurs resultats dans le meme fichier:
    # resultat est soit une sd resultat soit une liste ou un tuple de sd resultat
    def impr_resu(self,resultat, nume_mode = None):

        d_resu = self.get_impr_resu_params()

        #if nume_mode is not None:
        #    d_resu['NUME_MODE'] = nume_mode

        if nume_mode is not None:
            if isinstance(nume_mode[0],str):
                d_resu = {}
                d_resu['NOEUD_CMP'] = nume_mode
                d_resu['NOM_CHAM'] = 'DEPL'
            elif isinstance(nume_mode[0],int):
                d_resu['NUME_MODE'] = nume_mode

        l_resultat = []

        if isinstance(resultat, tuple) or isinstance(resultat, list):
            for resultat_i in resultat:
                l_resultat.append(_F(RESULTAT=resultat_i,
                                     **d_resu))
        else:
            l_resultat.append(_F(RESULTAT=resultat,
                                     **d_resu))

        IMPR_RESU( UNITE   = self.unite_logique,
                   FORMAT  = self.impr_resu_format,
                   RESU    = l_resultat
                   )
        pass

    # @param resultat (multitype): on peut imprimer un ou plusieurs resultats dans le meme fichier:
    # resultat est soit une sd resultat soit une liste ou un tuple de sd resultat.
    #
    # @warning Si une liste de modeles est fournie, il faut une liste de resultats.
    # On peut avoir plusieurs resultats sur la meme modele

    def visu_resu(self,
                  resultat, nume_mode = None):
        ok = None
        self.defi_fichier()
        if isinstance(resultat, tuple) or isinstance(resultat, list):
            for i in range(len(resultat)):
                    self.impr_resu(resultat[i], nume_mode)
        else:
            self.impr_resu(resultat, nume_mode)
        term = self.affiche()

        return term


# Classe specifique pour la gestion de la visu GSMH
class CalcEssaiGmsh(CalcEssaiLogiciel):
    def __init__(self, mess):
        CalcEssaiLogiciel.__init__(self, mess)
        self.impr_resu_format = "GMSH"
        self.impr_resu_params = {"TYPE_CHAM": 'VECT_3D',
                                 "NOM_CMP"  : ['DX','DY','DZ']}
        self.term  = None
        self.param = None
        self.get_gmsh_params()


    def get_gmsh_params(self):
        GMSH_CANDIDATES = [ os.path.join( pth, "gmsh" ) for pth in os.environ['PATH'].split(":") ]
        GMSH_CANDIDATES += ["./gmsh"] # custom paths
        GMSH_CANDIDATES += ["/logiciels/Aster/outils/gmsh"]
        GMSH_CANDIDATES += ["/aster/outils/gmsh"]

        for pth in GMSH_CANDIDATES:
            if os.access( pth, os.X_OK ):
                GMSH_PATH = pth
                break
        else:
            GMSH_PATH = 'gmsh'

        self.param = { 'mode' : 'LOCAL',
            'SKIN' : 'NON',
            'gmsh' : GMSH_PATH}

        pass

    def affiche(self, fichier=None):
        #param:
        #gmsh: /aster/outils/gmsh
        #mode: LOCAL
        #SKIN: NON
        if fichier is None:
            fichier = self.get_nom_fichier()
        term = GMSH('POST', fichier, self.param, options={} )
        return term


    def visu_mac(self,mac,resu1,resu2):

        titre = "matrice de MAC pour " + resu1.nom + " et " + resu2.nom

        f = Toplevel()
        size = (20,300)
        f.columnconfigure(0,weight=1)
        f.rowconfigure(0,weight=1)
        mac_win = MacWindowFrame( f, titre, resu1.nom, resu2.nom, size)
        mac_win.grid(row=0,column=0,sticky='nsew')
        afreq1 = resu1.get_modes_data()['FREQ']
        noeud_cmp1 = resu1.get_modes_data()['NOEUD_CMP']
        afreq2 = resu2.get_modes_data()['FREQ']
        noeud_cmp2 = resu2.get_modes_data()['NOEUD_CMP']
        # si mode statique, on donne le champ NOEUD_CMP a la place de la frequence
        for ind_ordr in range(len(afreq1)):
            if afreq1[ind_ordr]==None:afreq1[ind_ordr]=noeud_cmp1[ind_ordr]
        for ind_ordr in range(len(afreq2)):
            if afreq2[ind_ordr]==None:afreq2[ind_ordr]=noeud_cmp2[ind_ordr]
        mac_win.set_modes(afreq1, afreq2, mac)




# Classe specifique pour la gestion de la visu Salome
class CalcEssaiSalome(CalcEssaiLogiciel):
    def __init__(self, mess, machine_name, salome_port,
                 user, protocole, protocole_ok, param_visu):
        CalcEssaiLogiciel.__init__(self, mess)
        self.impr_resu_format = "MED"
        self.impr_resu_params = {}
        self.machine_name = machine_name
        self.salome_port = salome_port
        self.user = user
        self.protocole = protocole
        self.protocole_ok = protocole_ok
        self.dataPresentation = None
        self.theMedFile = None
        self.param_visu = param_visu
        self.study_name = None

    def affiche(self, fichier=None):
        if fichier is None:
            fichier = self.get_nom_fichier()
        ok = None
        working_dir     = os.getcwd()
        self.theMedFile = os.path.join(working_dir, fichier)

        self.Show( './fort.%s'%self.unite_logique)
        self.libere_fichier()


    def studylist(self):
        """
        Retourne la liste des études
        """
        result = []

        _UL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
        unite=_UL['UNITE_LIBRE',1]

        dSALOME = { 'CHEMIN_SCRIPT'    : './Python/Templates/salomeGetStudies.py',
                    'SALOME_HOST'      : self.machine_name,
                    'SALOME_PORT'      : self.salome_port,
                    'FICHIERS_SORTIE'  : [ './fort.%s' % unite ],
                  }

        EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                      INFO=2,
                      SALOME=dSALOME
                      );

        f=open('./fort.%s' % unite, 'r')
        result=[ study.strip() for study in f.readlines() ]
        f.close()

        DEFI_FICHIER(ACTION='LIBERER', UNITE=unite)

        return result

    def visu_mac(self,mac,resu1,resu2):

        param_visu=self.param_visu
        param_visu.type_visu.set('mac')


        self.defi_fichier()
        make_mac_salome(mac,resu1,resu2,self.unite_logique)
        self.Show( './fort.%s'%self.unite_logique)
        self.libere_fichier()


    def Show(self, medFilePath) :
        """
        Lecture d'un fichier MED par le composant VISU de SALOME.

        @type     medFilePath : string
        @param  medFilePath:  chemin du fichier MED

        @type     visuType :     integer
        @param  visuType:      type de visualisation
        """
        if self.param_visu.type_visu.get()=='mac' :
            script = './Python/Templates/salomeScriptMac.py'
        elif self.param_visu.type_visu.get()=='deformee' :
            script = './Python/Templates/salomeScript.py'
        else:
            print "Le type de deformee a visualiser n'a pas ete defini"
        if not self.study_name:
            self.mess.disp_mess(u"Choisir l'étude Salomé dans laquelle afficher les résultats")
            return

        dSALOME = { 'CHEMIN_SCRIPT'    : script,
                    'SALOME_PORT'      : self.salome_port,
                    'FICHIERS_ENTREE'  : [ medFilePath ],
                    'NOM_PARA'         : [ 'STUDY' ],
                    'VALE'             : [ self.study_name ],
                  }

        EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                      INFO=2,
                      SALOME=dSALOME
                      );

        UTMESS('I','STANLEY_20')



class CalcEssaiSalomeCourbes(CalcEssaiSalome):

    def __init__(self, mess, machine_name, salome_port, param_visu):
        self.mess = mess
        self.machine_name = machine_name
        self.salome_port = salome_port
        self.param_visu = param_visu
        self.study_name = None

    # l_x: liste des abscisses
    # ll_y: liste de liste des ordonnees (une liste par courbe)
    # l_legende: liste de legendes (une legende par courbe)
    def affiche(self, l_x, ll_y, couleur=None, titre="Courbes", l_legende=None,
                legende_x="Abscisses",legende_y="Ordonnées",
                unite_x="ua",unite_y="ua"):

        if l_legende == None:
            l_legende = [' toto ']*len(ll_y)


        # Creation d'une table pour ranger les fonctions
        titre1 = 'TITLE:'+titre
        legende_x=legende_x.strip()
        legende_x=legende_x.replace(' ','')
        legende_y=legende_y.strip()
        legende_y=legende_y.replace(' ','')

        # Liste des noms des courbes, et titre des axes
        l_colonnes = [legende_x]
        titre2 = 'COLUMN_TITLES:'+legende_x+' | '
        for legende in l_legende:
            legende=legende.strip()
            legende=legende.replace(' ','')
            l_colonnes.append(legende)
            titre2 = titre2 + legende+' | '

        # unite des axes
        unite_x=unite_x.strip()
        unite_x=unite_x.replace(' ','')
        unite_y=unite_y.strip()
        unite_y=unite_y.replace(' ','')
        titre3 = 'COLUMN_UNITS:' + unite_x +' '+ unite_y
        TITRE = titre1+'\n'+titre2+'\n'+titre3

        # Liste des noms des courbes
        dico = []
        for ind_absc in range(len(l_x)):
            dico.append({ legende_x:l_x[ind_absc]})
            for ind_courb in range(len(ll_y)):
                dico[ind_absc].update({l_colonnes[ind_courb+1]:ll_y[ind_courb][ind_absc]})

        table = Table(titr=TITRE)
        table.extend(dico)
        table = table[l_colonnes]

        self.defi_fichier()
        fw=open('fort.%s' %self.unite_logique, 'w')
        fw.write( str(table) )
        fw.close()

        # recuperation des noms des etudes Salome ouvertes
        if not self.study_name:
            self.mess.disp_mess(u"Sélectonner l'étude Salomé dans laquelle afficher les courbes")
            return

        self.Show( './fort.%s'%self.unite_logique)
        self.libere_fichier()


    def Show(self,medFilePath):
        """
        Lecture d'un fichier MED par le composant VISU de SALOME.

        @type     medFilePath : string
        @param  medFilePath:  chemin du fichier MED

        @type     visuType :     integer
        @param  visuType:      type de visualisation
        """

        dSALOME = { 'CHEMIN_SCRIPT'    : './Python/Templates/salomeScript.py',
                    'SALOME_PORT'      : self.salome_port,
                    'FICHIERS_ENTREE'  : [ medFilePath ],
                    #'SALOME_RUNAPPLI'  : self.salome_runscript,
                    'NOM_PARA'         : [ 'CHOIX', 'STUDY' ],
                    'VALE'             : [ 'COURBE', self.study_name ],
                  }


        EXEC_LOGICIEL(CODE_RETOUR_MAXI=-1,
                      INFO=2,
                      SALOME=dSALOME
                      );

        UTMESS('I','STANLEY_20')

    def fermer(self):
        pass



##class CalcEssaiXmgrace(CalcEssaiLogicielCourbes):
class CalcEssaiXmgrace():

    def __init__(self):
        self.xmgr_manager = None
        pass

    # l_x: liste des abscisses
    # ll_y: liste de liste des ordonnees (une liste par courbe)
    def affiche(self, l_x, ll_y, couleur=None, titre='Courbes', l_legende=None,
                legende_x=' ', legende_y=' ',
                unite_x=' ',unite_y=' '):
        if couleur is None:
            # XXX color n'est plus uilisé mais est-ce important?
            # Xmgrace applique automatiquement une nouvelle couleur
            # à chaque courbe.
            couleur = range(1, 15)
            if len(couleur) > len(l_x):
                couleur = couleur[0 : len(l_x)]
            elif len(couleur) < len(l_x):
                for k in range(len(l_x) - len(couleur)):
                    couleur.append(',1')

        self.xmgr_manager = XmgrManager()
        self.xmgr_manager.affiche(l_x, ll_y, couleur, l_legende, legende_x, legende_y)
        pass

    def fermer(self):
        pass
