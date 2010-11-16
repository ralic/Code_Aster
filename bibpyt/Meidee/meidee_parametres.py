#@ MODIF meidee_parametres Meidee  DATE 16/11/2010   AUTEUR BODEL C.BODEL 
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

## \package meidee_parametres Module de gestion de la visualisation %Meidee
# 
# La classe InterfaceParametres gere les options et les logiciels de Visualisation

from Utilitai.Utmess import UTMESS

import sys
import aster

from Accas import _F
import weakref
import os
import time

from Tkinter import Frame, Menubutton, Menu, StringVar, IntVar, BooleanVar, Listbox
from Tkinter import Scrollbar, Label, Radiobutton, Button, Entry
from Tkinter import Checkbutton, Canvas, Toplevel
from Tkinter import NORMAL, DISABLED
from Meidee.meidee_iface import XmgrManager, MyMenu

# teste si pylotage est present pour pouvoir utiliser Salome
try:
    import pylotage
    __salome__ = True
    # la ligne suivante fait appel a une surcharge provisoire de pylotage
    from Meidee.meidee_salome_visu import Visu, NODE

    from pylotage.TOOLS.Study import StudyManager
except ImportError:
    __salome__ = False
    Visu = object

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

def StateDeactivate(l_widget):
    """! Desactive tous les widgets de la liste l_widget"""
    for widget in l_widget:
        widget.config(state=DISABLED)

class InterfaceParametres(Frame):
    """!Interface principale de l'outil de projection des modes
    expérimentaux sur les modes numériques
    
    permet la sélection et de calcul des modes en air et écoulement"""
    def __init__(self,
                 root,
                 mess):
        """!Constructeur

        :IVariable:
         - `root`: fenetre parente
         - `meidee_objects`: objet Meidee, permettant d'accéder aux résultats aster
         - `logiciel`: valeur associée au bouton de sélection du logiciel
         - `mode_distant`: valeur associée au bouton de sélection du mode d'exécution de Salome
         - `distant_name`: valeur associée à l'entrée du nom de machine distante
         - `salome_port`: valeur associée à l'entrée du numéro de port Corba de Salome
         - `use_nume_mass`: indicateur d'utilisation de la matrice de masse numérique
         - `proj_champ_meth`: méthode à utiliser pour PROJ_CHAMP (SVD ou LU)
         - `proj_svd_param`: si méthode SVD, alors paramètre de sélection
         - `mac_windows`: liste des fenetres d'affichage de MAC modes
        """
        
        Frame.__init__(self, root, relief='sunken', borderwidth=1) # Première frame
        
        self.mess = mess
        self.root = root
        self.logiciel = StringVar()
        self.salome_port = IntVar()
        self.mode_distant = BooleanVar()
        self.machine_distante_name = StringVar()
        self.machine_locale_name = self.get_machine_name()
        self.user = StringVar()
        self.protocole = StringVar()
        self.machine_name = self.machine_locale_name
        self.salome_widgets  = []
        self.distant_widgets = []
        self.protocole_ok = None
        self.logiciel_courbes = None
        self.interface_meidee()


    def setup(self):
        """!Appelée par le gestionnaire de tab lors de l'affichage"""
        pass

    def teardown(self):
        """!Appelée par le gestionnaire de tab lors
           du masquage (passage à un autre tab)"""
        return


    def interface_meidee(self):
        """!Fonction principale de création de l'interface

        """
        self.columnconfigure(0, weight=1)
        #self.rowconfigure(1, weight=1)
        self.rowconfigure(2, weight=1)
        #self.rowconfigure(3, weight=1)
        l = Label(self,text="Paramètres de visualisation", pady=5, font=("Helvetica", "16") )
        l.grid(row=0)

        select_box = self.interface_parametres(self)
        select_box.grid(row=1, sticky='w'+'e'+'s'+'n')

        self.main = self
    
    def activate_distant_widgets(self):
        StateActivate(self.distant_widgets)
    
    def deactivate_distant_widgets(self):
        StateDeactivate(self.distant_widgets)
    
    def activate_or_deactivate_distant_widgets(self):
        if self.mode_distant.get():
            self.activate_distant_widgets()
        else:
            self.deactivate_distant_widgets()
    
    def activate_salome_widgets(self):
        self.activate_or_deactivate_distant_widgets()
        StateActivate(self.salome_widgets)
    
    def deactivate_salome_widgets(self):
        self.deactivate_distant_widgets()
        StateDeactivate(self.salome_widgets)
    
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
        
        label_parametres_salome = Label(logiciels_frame, text="Paramètres Salome")
        label_parametres_salome.grid(row=2, column=1, columnspan=2)
        self.salome_widgets.append(label_parametres_salome)
        
        label_port = Label(logiciels_frame, text="Port")
        label_port.grid(row=3, column=1, sticky='w')
        self.salome_widgets.append(label_port)
        
        entry_salome_port = Entry(logiciels_frame, textvariable=self.salome_port)
        entry_salome_port.grid(row=3, column=2)
        self.salome_widgets.append(entry_salome_port)
        self.salome_port.set(self.get_runnig_salome_port())
        
        label_parametres_distant = Label(logiciels_frame, text="Paramètres mode distant")
        label_parametres_distant.grid(row=5, column=2, columnspan=3)
        self.distant_widgets.append(label_parametres_distant)
        
        label_machine_name = Label(logiciels_frame, text="Nom machine")
        label_machine_name.grid(row=6, column=2, sticky='w')
        self.distant_widgets.append(label_machine_name)
        
        entry_machine_name = Entry(logiciels_frame, textvariable=self.machine_distante_name)
        entry_machine_name.grid(row=6,column=3, columnspan=2)
        self.distant_widgets.append(entry_machine_name)
        self.machine_distante_name.set(self.get_machine_name())
        
        label_user_name = Label(logiciels_frame, text="Nom utilisateur")
        label_user_name.grid(row=7, column=2, sticky='w')
        self.distant_widgets.append(label_user_name)
        
        entry_user = Entry(logiciels_frame, textvariable=self.user)
        entry_user.grid(row=7,column=3, columnspan=2)
        self.distant_widgets.append(entry_user)
        self.user.set(self.get_user())
        
        label_protocole = Label(logiciels_frame, text="Protocole de copie")
        label_protocole.grid(row=8, column=2, sticky='w')
        self.distant_widgets.append(label_protocole)
        
        button_rcp = Radiobutton(logiciels_frame, text="rcp", value="rcp", variable=self.protocole)
        button_rcp.grid(row=8, column=3, sticky='w')
        self.distant_widgets.append(button_rcp)
        self.protocole.set("rcp")
        
        button_scp = Radiobutton(logiciels_frame, text="scp", value="scp", variable=self.protocole)
        button_scp.grid(row=8, column=4, sticky='w')
        self.distant_widgets.append(button_scp)
        
        button_verifier_protocole = Button(logiciels_frame, text='Vérifier protocole', command=self.check_protocole)
        button_verifier_protocole.grid(row=9, column=3, columnspan=2)
        self.distant_widgets.append(button_verifier_protocole)
        
        button_local = Radiobutton(logiciels_frame, text="Local", value=False, variable=self.mode_distant,
                            command = self.deactivate_distant_widgets )
        button_local.grid(row=4, column=1, sticky='w')
        self.salome_widgets.append(button_local)
        
        button_distant = Radiobutton(logiciels_frame, text="Distant", value=True, variable=self.mode_distant,
                            command = self.activate_distant_widgets )
        button_distant.grid(row=5, column=1, sticky='w')
        self.salome_widgets.append(button_distant)

        # Par defaut, on coche mode local
        self.mode_distant.set(False)
        
        # => On desactive les widgets du mode distant
        self.activate_or_deactivate_distant_widgets()
        
        label_choix_logiciel = Label(logiciels_frame, text="Choix du logiciel")
        label_choix_logiciel.grid(row=0, column=0, columnspan=3 )
        button_gmsh = Radiobutton(logiciels_frame, text="Gmsh/Xmgrace", value="Gmsh/Xmgrace", variable=self.logiciel,
                                  command = self.deactivate_salome_widgets  )
        button_gmsh.grid(row=1, column=0, sticky='w')
        
        button_salome = Radiobutton(logiciels_frame, text="Salome", value="Salome", variable=self.logiciel,
                                    command = self.activate_salome_widgets )
        button_salome.grid(row=2, column=0, rowspan=3, sticky='w')
        
        if __salome__ is not True:
            self.logiciel.set("Gmsh/Xmgrace")
            self.mess.disp_mess( "Le module python 'pylotage' semble être absent, ou mal configuré.")
            self.mess.disp_mess( "On désactive le choix Salome.")
            StateDeactivate(self.salome_widgets+[button_salome])
        elif not self.is_salome_launched():
            self.logiciel.set("Gmsh/Xmgrace")
            self.deactivate_salome_widgets()
            self.mess.disp_mess( "Salome n'a pas été trouvé en local. On utilise Gmsh/Xmgrace.")
        else:
            self.logiciel.set("Salome")
        
        logiciels_frame.grid(row=1)
        
        # Pas besoin d'utiliser un bouton valider si on ne sauvegarde pas les parametres dans un fichier
        #button_valider = Button(f,text='Valider',command=self.save_parameters)
        #button_valider.grid( row = 2)

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
                msg = "Problème lors de la détermination du port Salome.\n"
                msg += "Veuillez déterminer manuellement le port de Salome, en tapant ceci dans l'interpréteur python embarqué de Salome:\n"
                msg += "import NSparam\n"
                msg += "NSparam.getNSparams()"
                self.mess.disp_mess(msg)
        return salome_port
    
    # Verifie que le protocole de copie fonctionne
    def check_protocole(self):
        import signal
        
        txt = 'toto'
        
        executable = "%ssh"%(self.protocole.get()[0])
        
        args = [self.machine_distante_name.get(), "-l", self.user.get(), "echo '%s' > /dev/null"%txt]
        
        pid = os.spawnlp(os.P_NOWAIT, executable, executable, *args)
        
        returncode = -1
        wpid = 0
        t = 0
        # on attend que la commande se termine ou on quitte apres 5s
        while (wpid == 0 and t<5):
            wpid, returncode = os.waitpid(pid,1)
            time.sleep(1)
            t = t+1
        
        # si la commande n'est pas terminee apres 5s, on la tue
        if wpid == pid and returncode == 0:
            self.mess.disp_mess("Le protocole %s est correctement configuré."%self.protocole.get())
            self.protocole_ok = True
        else:
            os.kill(pid, signal.SIGTERM)
            self.mess.disp_mess("Le protocole %s est mal configuré. Vérifiez vos paramètres."%self.protocole.get())
            self.protocole_ok = False
        pass

    def save_parameters(self, do_check_protocole=True):
        """! Sauvegarde les parametres dans une classe parente pour qu'ils soient communs a tous les onglets """
        if self.logiciel.get() == "Salome":
            if self.mode_distant.get():
                self.machine_name = self.machine_distante_name.get()
                if do_check_protocole and self.protocole_ok is None:
                    self.check_protocole()
            else:
                self.machine_name = self.machine_locale_name
            pass
        pass
    
    def get_logiciel(self):
        self.save_parameters()
        if self.logiciel.get() == "Gmsh/Xmgrace":
            return MeideeGmsh(self.mess)
        else:
            return MeideeSalome(self.mess, self.machine_name, self.salome_port.get(),
                                self.mode_distant.get(), self.user.get(), self.protocole.get(), 
                                self.protocole_ok, self)
        pass
    
    def get_logiciel_courbes(self):
        # Les courbes sont transferees par CORBA
        # => Pas besoin de verifier le protocole rcp/scp
        self.save_parameters(do_check_protocole=False)
        if self.logiciel.get() == "Gmsh/Xmgrace":
            return MeideeXmgrace()
        else:
            return MeideeSalomeCourbes(self.mess, self.machine_name, self.salome_port.get(),self)
        pass
    
    # fonction proxy vers le bon logiciel
    def visu_resu(self,resultat, nume_mode=None):
        logiciel = self.get_logiciel()
        term = logiciel.visu_resu(resultat, nume_mode)
        return term
    
    def visu_courbe(self, l_x, ll_y, couleur=None, l_legende=None, titre_x="Abscisses", titre_y="Ordonnées"):
        self.logiciel_courbes = self.get_logiciel_courbes()
        self.logiciel_courbes.affiche(l_x, ll_y, couleur, l_legende, titre_x, titre_y)
        pass




##############################################################
# Classes specifiques pour chaque logiciel de post-traitement
##############################################################

from Cata.cata import INFO_EXEC_ASTER, DEFI_FICHIER, DETRUIRE, IMPR_RESU

# Classe abstraite
class MeideeLogiciel(object):
    
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
        self.libere_fichier()
        term = self.affiche()
        
        return term

    
# Classe specifique pour la gestion de la visu GSMH
class MeideeGmsh(MeideeLogiciel):
    def __init__(self, mess):
        MeideeLogiciel.__init__(self, mess)
        self.impr_resu_format = "GMSH"
        self.impr_resu_params = {"TYPE_CHAM": 'VECT_3D', 
                                 "NOM_CMP"  : ['DX','DY','DZ']}
        self.term  = None
        self.param = None
        self.get_gmsh_params()
    
    def get_gmsh_params(self):
        # TODO: supprimer ces lignes de recherche de gmsh de meidee_correlation.py (et autres) pour ne le garder qu'ici.
        
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

# Classe specifique pour la gestion de la visu Salome
class MeideeSalome(MeideeLogiciel):
    def __init__(self, mess, machine_name, salome_port, mode_distant,
                 user, protocole, protocole_ok, ihm_param):
        MeideeLogiciel.__init__(self, mess)
        self.impr_resu_format = "MED"
        self.impr_resu_params = {}
        self.machine_name = machine_name
        self.salome_port = salome_port
        self.mode_distant = mode_distant
        self.user = user
        self.protocole = protocole
        self.protocole_ok = protocole_ok
        self.dataPresentation = None
        self.theMedFile = None
        self.ihm_param = ihm_param

    def affiche(self, fichier=None):
        if fichier is None:
            fichier = self.get_nom_fichier()
        ok = None
        theDelay        = 0
        working_dir     = os.getcwd()
        self.theMedFile = os.path.join(working_dir, fichier)
        
        # liste des etudes ouvertes sous le name service donne
        st_mng = StudyManager(ORBInitRef='NameService=corbaname::%s:%i'%(self.machine_name, self.salome_port),
                              machineName = self.machine_name)
        studies = st_mng.getOpenStudies()
        
        self.study_name = None
        if not studies :
            # pas d'etudes Salome ouverte : il faut en ouvrir une
            study = st_mng.getOrCreateStudy('CALC_ESSAI')
            self.study_name = 'CALC_ESSAI'
            self.suite()
        # cas ou une ou plusieurs etudes salome sont ouvertes.
        if len(studies) == 1:
            # une seule etude ouverte
            self.study_name = studies[0]
            self.suite()
        else:
            # plusieurs etudes ouvertes : on ouvre une fenetre pour choisir l'etude a selectionner
            choix_st = ChoixStudy(self,studies,self.ihm_param)
            
    def suite(self):

        entity = NODE
        theDelay = 0
        try:
            salomeVisu = Visu( ORBInitRef='NameService=corbaname::%s:%i'%(self.machine_name, self.salome_port),
                               studyName = self.study_name,
                               machineName = self.machine_name  )
        except:
            self.mess.disp_mess("Impossible de trouver le module VISU de la machine %s sur le port %i" \
                              %(self.machine_name, self.salome_port))
            self.mess.disp_mess("Conseil : si i existe une cession de Salomé, pensez à activer le modue Visu")
            return
        
        # Transfert vers machine distante
        if self.mode_distant:
            if not self.protocole_ok:
                self.mess.disp_mess("Le protocole %s est mal configuré. Vérifiez vos paramètres."%self.protocole)
                return
            ok = self.transfert_med_file()
            if not ok:
                return
        
        # On procede au chargement des données du fichier med
        ok = salomeVisu.importMED(self.theMedFile)
        if not ok:
            self.mess.disp_mess("Problème à l'import du fichier med %s"%theMedFile)
            return
        
        # Pour recuperer les infos sur le fichier med
        self.dataPresentation = salomeVisu.dataPresentation
        self.study = salomeVisu.study
        
        l_mesh_names = salomeVisu.dataPresentation.GetMeshNames()
        
        for mesh_name in l_mesh_names:
            ok = salomeVisu.setEntity(entity, mesh_name)
            if not ok:
                self.mess.disp_mess("Impossible de creer le maillage %s"%mesh_name)
                return
            
            # BUG (ou spec???) Renvoit une liste de chaines de caracteres vides!
            # => cf REX 12607
            ##l_field_names = salomeVisu.dataPresentation.GetFields(theMeshName, NODE)
            # => on developpe notre propre methode getFieldNames:
            l_field_names = self.getFieldNames(mesh_name)
            
            for field_name in l_field_names:
                l_time_stamps = salomeVisu.dataPresentation.GetTimeStampNumbers(mesh_name, NODE, field_name)
                nb_time_stamps = len(l_time_stamps)
                for iteration in range(1, nb_time_stamps+1):
                    ok = salomeVisu.DeformedShape( field_name , iteration, "DEPL",  theDelay)
                    if not ok:
                        self.mess.disp_mess("Impossible d'afficher %s, iteration %i"%(field_name, iteration))
                        return
                    pass
                pass
            pass
        pass
    
    # On transfert le fichier med via rcp ou scp
    #
    # L'ideal serait de pouvoir transferer le fichier via CORBA, comme dans ExportToMED de Smesh dans pylotage
    # pour qu'on n'ait pas a entrer le nom d'utilisateur et le protocole dans l'interface,
    # mais le transfert dans ce sens n'est pas implemente dans le KERNEL de SALOME.
    # Pour pouvoir transférer un fichier de la machine locale vers la machine contenant les services CORBA,
    # dans SALOME_FileTransfer_i.cxx, il faut :
    # - implémenter le mode 'w' dans la méthode open pour ouvrir un fichier sur la machine CORBA
    # - une méthode putBlock qui écrit un bloc d'octets dans ce fichier.
    # => TODO: demande a NEPAL
    # => cf REX Code_Aster 12606
    def transfert_med_file(self):
        
        current_time =  time.strftime("%Y_%m_%d-%H_%M_%S")
        med_distant = "/tmp/CALC_ESSAI-%s-%s.med"%(self.user, current_time)
        cmd = "%s %s %s@%s:%s"%(self.protocole, self.theMedFile, self.user, self.machine_name, med_distant)
        ret = os.system(cmd)
        if ret != 0:
            self.mess.disp_mess("Problème lors de la copie du fichier med vers %s avec le protocole %s"%(self.machine_name, self.protocole))
            return False
        self.theMedFile = med_distant
        return True
    
    # Remplacement de VISU.Result.GetFields par un parcours de l'arbre d'etude
    # - nom du fichier med
    #     - nom du maillage 1
    #     + Families
    #     + Groups
    #     - Fields
    #         + nom du champ 1
    #         + nom du champ 2
    #     - nom du maillage 2
    # ...
    def getFieldNames(self, mesh_name):
        l_field_names = []
        med_result_id = self.dataPresentation.GetID()
        med_result_SObj = self.study.FindObjectIOR( med_result_id )
        #self.mess.disp_mess(med_result_SObj.GetName())
        i = 1
        mesh_SObj = None
        while med_result_SObj.FindSubObject(i)[0]:
            try:
                mesh_SObj = med_result_SObj.FindSubObject(i)[1]
                #self.mess.disp_mess(mesh_SObj.GetName())
                #self.mess.disp_mess("i=%i"%i)
                if mesh_SObj.GetName() == mesh_name:
                    #self.mess.disp_mess("=> trouvé")
                    break
            except:
                self.mess.disp_mess("=> pb dans mesh_name i=%i"%i)
                pass
            i = i+1
        
        if mesh_SObj is None:
            self.mess.disp_mess("maillage %s non trouvé"%mesh_name)
            return
        
        # 1: Groups
        # 2: Families
        # 3: Fields
        # => FindSubObject(3)
        fields_SObj = mesh_SObj.FindSubObject(3)[1]
        j = 1
        while fields_SObj.FindSubObject(j)[0]:
            try:
                field = fields_SObj.FindSubObject(j)[1]
                field_name_tmp = field.GetName()
                field_name = field_name_tmp.split(",")[0]
                #self.mess.disp_mess("j=%i"%j)
                #self.mess.disp_mess(field_name)
                l_field_names.append(field_name)
            except:
                self.mess.disp_mess("=> pb dans field_name j=%i"%j)
                pass
            j = j+1
        
        return l_field_names

class MeideeLogicielCourbes(MeideeLogiciel):
    
    def __init__(self):
        pass

    def fermer(self):
        pass

class MeideeSalomeCourbes(MeideeLogicielCourbes):
    
    def __init__(self, mess, machine_name, salome_port, ihm_param):
        self.mess = mess
        self.machine_name = machine_name
        self.salome_port = salome_port
        self.ihm_param = ihm_param
    
    # l_x: liste des abscisses
    # ll_y: liste de liste des ordonnees (une liste par courbe)
    # l_legende: liste de legendes (une legende par courbe)
    def affiche(self, l_x, ll_y, couleur=None, l_legende=None, titre_x="Abscisses", titre_y="Ordonnées"):
        # les donnees d'entree sont passees en attribut de la classe pour etre utilisees dans la methode "suite"
        self.l_x = l_x
        self.ll_y = ll_y
        self.couleur = couleur
        self.l_legende = l_legende
        self.titre_x = titre_x
        self.titre_y = titre_y

        # recuperation des noms des etudes Salome ouvertes :
        st_mng = StudyManager(ORBInitRef='NameService=corbaname::%s:%i'%(self.machine_name, self.salome_port),
                              machineName = self.machine_name)
        studies = st_mng.getOpenStudies()
        
        self.study_name = None
        if not studies :
            # pas d'etudes Salome ouverte : il faut en ouvrir une
            study = st_mng.getOrCreateStudy('CALC_ESSAI')
            self.study_name = 'CALC_ESSAI'
            self.suite()
        # cas ou une ou plusieurs etudes salome sont ouvertes.
        if len(studies) == 1:
            # une seule etude ouverte
            self.study_name = studies[0]
            self.suite()
        else:
            # plusieurs etudes ouvertes : on ouvre une fenetre pour choisir l'etude a selectionner
            choix_st = ChoixStudy(self,studies,self.ihm_param)
        

    def suite(self):
        """ suite de la methode affiche"""
        # On cree la meme liste de courbes que celle sortie dans Stanley donnee a pylotage
        from Stanley import as_courbes
        
        l_courbes = []
        l_x = self.l_x
        # boucle sur les listes d'ordonnees
        for i, l_y in enumerate(self.ll_y):
            courbe = as_courbes.Courbe(l_x, l_y)
            if self.l_legende is None:
                nom_courbe = "Courbe %i"%i
            else:
                nom_courbe = self.l_legende[i]
            l_courbes.append((courbe, nom_courbe))

        # on recupere les services du module VISU
        salomeVisu = Visu( ORBInitRef='NameService=corbaname::%s:%i'%(self.machine_name, self.salome_port),
                           studyName = self.study_name, machineName = self.machine_name  )
        
        if self.l_legende is None:
            titre='Courbes CALC_ESSAI'
        else:
            titre=self.l_legende[0]
        
        salomeVisu.XYPlot2(l_courbes, titre, self.titre_x, self.titre_y)
        pass

class MeideeXmgrace(MeideeLogicielCourbes):
    
    def __init__(self):
        self.xmgr_manager = None
        pass
    
    # l_x: liste des abscisses
    # ll_y: liste de liste des ordonnees (une liste par courbe)
    def affiche(self, l_x, ll_y, couleur=None, legende=None, titre_x=None, titre_y=None):
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
        self.xmgr_manager.affiche(l_x, ll_y, couleur, legende, titre_x, titre_y)
        pass
    
    def fermer(self):
        self.xmgr_manager.fermer()
        pass
    

class ChoixStudy:

    def __init__(self,parent,results,ihm_parent): # attention : rajouter ihm_parametre a l'appel
        self.parent = parent
        
        self.fenetre = Toplevel(ihm_parent)
        self.var_resu = StringVar()
        self.fenetre.title("Choix de l'etude Salome")
        f = Frame(self.fenetre)
        MyMenu( f, options = results,var = self.var_resu).grid(row=0, column=0, sticky='ew')
        Button(f,text='OK',command=self.fermer,width = 30).grid(row=1,column=0,sticky='ew')
        f.grid(row=0,column=0)

        self.fenetre.mainloop()

    def fermer(self):
        self.parent.study_name = self.var_resu.get()
        self.parent.suite()
        self.fenetre.destroy()
