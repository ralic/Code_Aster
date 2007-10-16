#@ MODIF meidee_correlation Meidee  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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
try:
    import Numeric
    import sys
    import aster
    from Meidee.meidee_cata import Resultat, CaraElem, InterSpectre, MeideeObjects
    from Meidee.meidee_iface import MacWindow, MacMode, MyMenu, ModeList
##    from Meidee.TkPlotCanvas import PlotCanvas
    from Accas import _F
    import weakref
    import os
    from Cata.cata import modele_sdaster , mode_meca, matr_asse_depl_r, maillage_sdaster
    from Cata.cata import cara_elem, cham_mater, table_sdaster, table_fonction
    from Cata.cata import IMPR_RESU, RECU_TABLE, MAC_MODES, DETRUIRE
    from Cata.cata import INFO_EXEC_ASTER, DEFI_FICHIER, CO, MACRO_EXPANS
    from Tkinter import Frame, Menubutton, Menu, StringVar, IntVar, Listbox
    from Tkinter import Toplevel, Scrollbar, Label, Radiobutton, Button, Entry
    from Tkinter import Checkbutton, Canvas

    # Recuperation de deux UL disponibles pour les operations du superviseur GMSH
    # TODO: proprifier ca si possible
    _TUL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
    _ULGMSH=_TUL['UNITE_LIBRE',1]
    DEFI_FICHIER(FICHIER='TMP',UNITE=_ULGMSH)
    _TUL2=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
    _ULMAIL=_TUL2['UNITE_LIBRE',1]
    DEFI_FICHIER(ACTION='LIBERER',UNITE=_ULGMSH)
    DETRUIRE(CONCEPT = _F(NOM = (_TUL,_TUL2)), INFO=1)


    
except ImportError:
    UTMESS('F','MEIDEE0_2')

try:
    from Stanley.gmsh import GMSH
except ImportError:
    # Mode standalone
    from gmsh import GMSH
   

palette = [ "#%02x%02x%02x" % (i, 255-i, 0) for i in range(256) ]

GMSH_CANDIDATES = [ os.path.join( pth, "gmsh" ) for pth in os.environ['PATH'].split(":") ]
GMSH_CANDIDATES += ["./gmsh"] # custom paths
GMSH_CANDIDATES += ["/logiciels/aster/outils/gmsh"]
GMSH_CANDIDATES += ["/aster/outils/gmsh"]


for pth in GMSH_CANDIDATES:
    if os.access( pth, os.X_OK ):
        GMSH_PATH = pth
        print "gmsh found here:" , GMSH_PATH
        break
else:
    print "Couldn't find gmsh executable"   
    GMSH_PATH = 'gmsh'




#######################
#                     #
#  CLASSES DE CALCUL  #
#                     #
#######################

def extract_mac_array( mac_mode ):
    """!Reconstruit un tableau numeric de modes MAC

    /param mac_mode concept Table aster
    """
    data1 = mac_mode.EXTR_TABLE().Array('NUME_MODE_1','MAC')
    data2 = mac_mode.EXTR_TABLE().Array('NUME_MODE_2','MAC')
    N = int(Numeric.maximum.reduce(data1[:,0]))
    M = int(Numeric.maximum.reduce(data2[:,0]))
    mac = Numeric.zeros( (N,M), Numeric.Float )
    for i in range(data1.shape[0]):
        i1 = int(data1[i,0])-1
        i2 = int(data2[i,0])-1
        mac[ i1, i2 ] = data1[i,1]
    return mac

class MeideeCorrelation:
    """!Classe qui s'occupe des calculs de l'interface correlation

    Cette classe un peu fourre-tout a pour but de séparer les calculs
    spécifique aster de l'interface graphique. L'objectif étant de pouvoir
    tester plus rapidement l'interface graphique en cours de développement.
    
    """
    def __init__(self, macro, mess):
        """!Constructeur

        \param macro le self de l'objet macro provenant de macro_visu_meidee_ops
        """
        self.resu_num = None
        self.resu_exp = None
        self.norme_num = None
        self.norme_exp = None
        self.Calc_Modes = {
            ("phi_et", ) : self.mac_et,
            ("phi_et", "phi_num" ) : self.mac_et_num,
            ("phi_exp", ) : self.mac_exp,
            ("phi_exp", "phi_num_red") : self.mac_exp_red,
            ("phi_num",) : self.mac_num,
            ("phi_num_red",) : self.mac_numred,
            }
        self.resu_et = None
        self.resu_num_extr = None
        self.resu_red = None
        self.resu_exp_extr = None
        self.proj_meth = "SVD"
        self.proj_param = (1.e-2,)
        self.concepts = {}      # mapping number -> (name,owned)
        self.concepts_names = {} #mapping name -> number
        self.macro = macro
        self.mess = mess

    def __setitem__(self, n, val):
        """!Emulation d'un dictionnaire

        On implemente __setitem__
        pour faire croire au superviseur qu'on cree
        un concept dans une liste"""
        self.concepts[n] = val

    def __getitem__(self, n):
        """!Emulation d'un dictionnaire

        On implémente __getitem__ pour les meme raison, et pour
        faire de l'allocation dynamique de numéro de concept
        on se comporte comme une liste"""
        return self.concepts[n]

    def is_valid( self ):
        """!Indique si on a spécifié un résultat numérique et un résultat expérimental

        Valide si on a précisé un resultat numérique et un résultat
        experimental"""
        bool = (self.resu_num is not None and self.resu_exp is not None)
        return bool

    def Setup( self, resu_num, resu_exp,
               norme_num, norme_exp ):
        """!Paramétrage de l'objet
        
        On fournit les objets aster nécessaires au calcul. Par défaut, la liste des modes
        sélectionnés dans self.modes_num_liste est la liste de tous les numéros d'ordre.
        """
        self.resu_num = resu_num
        self.resu_exp = resu_exp
        self.norme_num = norme_num
        self.norme_exp = norme_exp
        self.resu_et = None
        self.resu_red = None
        self.resu_num_extr = None
        self.resu_exp_extr = None

    def cleanup(self):
        """!Détruit les objets temporaires créés par calc_proj_resu"""
        self.resu_et = None
        self.resu_num_extr = None
        self.resu_exp_extr = None
        self.resu_red = None
        items = self.concepts_names.items()
        for name, (n,owners) in items:
            for c in owners[:]:
                if c() is None:
                    owners.remove(c)
            if not owners:
                self.mess.disp_mess( ("Destruction de "+name ) )
                DETRUIRE(CONCEPT=_F(NOM=(name,)),
                         ALARME='OUI', INFO=1)
                del self.concepts_names[name]
                del self.concepts[n]
            else:
                self.mess.disp_mess( ("Concept"+name+"toujours utile"+ owners))
                for c in owners:
                    self.mess.disp_mess( ("Owned by"+c() ) )
        self.mess.disp_mess( ( " " ) )
        


    def __del__(self):
        cleanup()

    def register_result(self, result):
        """!Enregistre un Resultat pour destruction ultérieure

        Appelé par un objet Resultat pour s'enregistrer dans la liste des
        objets Resultat qui utilisent ce concept et permet de ne pas détruire
        ce concept si le Resultat est toujours utilisé.

        On utilise une référence faible (weakref) qui permet de savoir si un
        objet à été détruit sans compter comme une vrai référence :
        c = weakref.ref(obj) # le refcount de obj est inchangé
        x = c() # x == obj si obj n'a pas été détruit, None sinon
        """
        self.concepts_names[result.nom][1].append( weakref.ref(result) )

    def unregister_result(self, result ):
        """!Supprime un objet résultat de la liste des Resultat référencés"""
        owners = []
        if result.nom in self.concepts_names:
            (n,owners) = self.concepts_names[ result.nom ]
            for c in owners[:]:
                if result == c():
                    owners.remove(c)
            if not owners:
                del self.concepts_names[result.nom]
        if not owners:
            self.mess.disp_mess( ("Destruction de" +  result.nom) ) 
            DETRUIRE(CONCEPT=_F(NOM=(result.nom,)), ALARME='OUI', INFO=1)



    def get_modes_num(self):
        return self.resu_num.get_modes()

    def get_modes_exp(self):
        return self.resu_exp.get_modes()

    def calc_proj_resu(self, num_modes, exp_modes,
                       resu_num, resu_exp,
                       basename=None,
                       reso=None):
        """!Projection des modes experimentaux sur la base modale numérique
            reso est different de None en mode non interactif. En mode
            interactif, on va chercher les parametres de resolution dans
            l'IHM                                                          """
        self.mess.disp_mess( "Début de MACRO_EXPANS")
        if not basename:
            basename = "_tmp"
            DETRUIRE(CONCEPT=_F(NOM=('_tmp_NX','_tmp_EX',
                                     '_tmp_ET','_tmp_RD')),
                     ALARME='NON', INFO=2 )
        name_nx = basename+"_NX"
        name_ex = basename+"_EX"
        name_et = basename+"_ET"
        name_rd = basename+"_RD"
        res_nx = CO(name_nx)
        res_ex = CO(name_ex)
        res_et = CO(name_et)
        res_rd = CO(name_rd)

        if reso == None:
            reso = []
            if self.proj_meth == "SVD":
                reso.append({"METHODE":"SVD", "EPS":self.proj_param[0] })
            else:
                reso.append({"METHODE":"LU"})
        self.mess.disp_mess( ('Donnees num             : '+ resu_num.nom))
        self.mess.disp_mess( ('nume_ordre selectionnes : '+ str(tuple(num_modes[0]))))        
        self.mess.disp_mess( ('Donnees exp             : '+ resu_exp.nom))
        self.mess.disp_mess( ('nume_ordre a etendre    : '+ str(tuple(exp_modes[0]))))
        self.mess.disp_mess( ('type résolution         : ' + reso[0]['METHODE']))
        self.mess.disp_mess("  ")

##        try:
        MACRO_EXPANS( MODELE_CALCUL = _F(MODELE = resu_num.modele,
                                         BASE   = resu_num.obj,
                                         NUME_MODE  = tuple(num_modes[0])),
                      MODELE_MESURE = _F(MODELE = resu_exp.modele,
                                         MESURE = resu_exp.obj,
                                         NUME_MODE  = tuple(exp_modes[0])),
                      RESU_NX = res_nx,
                      RESU_EX = res_ex,
                      RESU_ET = res_et,
                      RESU_RD = res_rd,                           
                      RESOLUTION = reso
                     )
##        except Exception, err:
##            print "err = ", err
##            self.mess.disp_mess("!! Erreur dans MACRO_EXPANS !!")
##            self.mess.disp_mess( "Fin de MACRO_EXPANS")
##            return
                
        self.resu_num_extr = Resultat(name_nx, res_nx, self.mess, owned=False)
        self.resu_exp_extr = Resultat(name_ex, res_ex, self.mess, owned=False)
        self.resu_et =Resultat(name_et, res_et, self.mess, owned=False)
        self.resu_red=Resultat(name_rd, res_rd, self.mess, owned=False)

        self.resu_et.cara_mod = self.resu_exp.get_cara_mod()
        self.resu_red.cara_mod = Numeric.take(self.resu_exp.get_cara_mod(),
                                              Numeric.array(exp_modes[0])-1, axis=0)

        self.mess.disp_mess( "Fin de MACRO_EXPANS")
        self.mess.disp_mess( " ")


    def mac_et_num(self, num_modes, exp_modes, num_norme, exp_norme):
        return self.resu_num_extr, self.resu_et, num_modes, exp_modes, num_norme

    def mac_exp_red(self, num_modes, exp_modes, num_norme, exp_norme):
        return self.resu_exp_extr, self.resu_red, num_modes, exp_modes, exp_norme

    def mac_numred(self, num_modes, exp_modes, num_norme, exp_norme):
        return self.resu_red, self.resu_red, num_modes, exp_modes, exp_norme

    def mac_et(self, num_modes, exp_modes, num_norme, exp_norme):
        return self.resu_et, self.resu_et, num_modes, exp_modes, num_norme
    
    def mac_num(self, num_modes, exp_modes, num_norme, exp_norme):
        return self.resu_num_extr, self.resu_num_extr, num_modes, num_modes, num_norme
        
    def mac_exp(self, num_modes, exp_modes, num_norme, exp_norme):
        return self.resu_exp_extr, self.resu_exp_extr, exp_modes, exp_modes, exp_norme

    def calc_mac_mode( self, resu1, resu2, modes1, modes2, norme, modeles ):
        """!Calcul de MAC entre deux bases modales compatibles"""
        try:
            o1 = resu1.obj
            o2 = resu2.obj
        except AttributeError:
            self.mess.disp_mess( ( "modeles = " + str(modeles[0]) ) )
            if modeles[0] == 'phi_num':
                o1 = o2 = self.resu_num.obj
            elif modeles[0] == 'phi_exp':
                o1 = o2 = self.resu_exp.obj
            else:
                self.mess.disp_mess( ( "Les modes a visualiser n existent pas !!" ) )
                self.mess.disp_mess( ( "Avez-vous cliqué sur Calculer ?" ) )
                self.mess.disp_mess( ( "                               " ) )
                return
        __MAC = MAC_MODES(BASE_1=o1,
                          BASE_2=o2,
                          MATR_ASSE=norme,
                          INFO=2
                          )
        self.mess.disp_mess( (  "      " ) ) 
        mac = extract_mac_array( __MAC )
        DETRUIRE(CONCEPT=_F(NOM=(__MAC,)))
        return mac, modes1, modes2

    def get_mac(self, modeles, num_modes, exp_modes, num_norme, exp_norme ):
        """!Calculer le MAC entre deux analyses modales

        On commence par extraire les modes sélectionnés pour le calcul et calculer
        les projections (modes étendus, modes étendus réduits) avant d'utiliser
        \see calc_mac_mode
        """

        self.mess.disp_mess("Calcul de MAC_MODE pour les modes :")        
        self.mess.disp_mess( (str(num_modes) ) )
        self.mess.disp_mess( (str(exp_modes) ) )
        self.mess.disp_mess("  ")
        oper = self.Calc_Modes[ modeles ]
        resu1, resu2, modes1, modes2, norme = oper(num_modes, exp_modes, num_norme, exp_norme)
        res = self.calc_mac_mode(resu1,resu2,modes1,modes2,norme, modeles)
        
        return res

    def prep_fichier(self, mode_num, mode_exp, mode_num_red, mode_et):#, num_modes, exp_modes ):
        """!Gestion preparation des fichiers pour l'appel de GMSH"""
        if mode_num:
            if not self.resu_num_extr:
                self.resu_num_extr = self.resu_num
            IMPR_RESU( UNITE   = _ULGMSH, 
                       FORMAT  = 'GMSH',
                       MODELE  = self.resu_num.modele,
                       RESU    = _F(RESULTAT=self.resu_num_extr.obj,),
                       )
        if mode_exp:
            if not self.resu_exp_extr:
                self.resu_exp_extr = self.resu_exp          
            IMPR_RESU( UNITE   = _ULGMSH,
                       FORMAT  = 'GMSH',
                       MODELE  = self.resu_exp.modele,
                       RESU    = _F(RESULTAT=self.resu_exp_extr.obj,),
                       )

        if mode_et:
            if not self.resu_et:
                self.mess.disp_mess( "!!   Les calculs d'expansion n'ont pas    !!" )
                self.mess.disp_mess( "!! encore ete faits. Cliquer sur Calculer !!" )
                self.mess.disp_mess( "   " )
            IMPR_RESU( UNITE   = _ULGMSH, 
                       FORMAT  = 'GMSH',
                       MODELE  = self.resu_num.modele,
                       RESU    = _F(RESULTAT=self.resu_et.obj,),
                       )
            
        if mode_num_red:
            if not self.resu_red:
                self.mess.disp_mess( "!!   Les calculs d'expansion n'ont pas    !!" )
                self.mess.disp_mess( "!! encore ete faits. Cliquer sur Calculer !!" )
                self.mess.disp_mess( "   " )                
            IMPR_RESU( UNITE   = _ULGMSH, 
                       FORMAT  = 'GMSH',
                       MODELE  = self.resu_exp.modele,
                       RESU    = _F(RESULTAT=self.resu_red.obj,),
                       )
        DEFI_FICHIER(ACTION='LIBERER', UNITE=_ULGMSH)

    def get_fichier(self):
        return "fort.%d" % _ULGMSH



#-------------------------------------------------------------------------------------------------------



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
                 mess):
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
        
        Frame.__init__(self, root, relief='raised', borderwidth=4) # Première frame
        self.mess = mess
        self.root = root
        self.macro = macro
        self.meidee_objects = meidee_objects
        self.mode_exp = IntVar()
        self.mode_etendu = IntVar()
        self.mode_nume = IntVar()
        self.mode_nume_red = IntVar()
        self.use_nume_mass = IntVar()
        self.proj_champ_meth = StringVar()
        self.proj_svd_param = StringVar()
        self.meidee = MeideeCorrelation( macro, self.mess )
        self.term = None
        self.mac_windows = []
        self.term_gmsh = []
        self.afreq = None
        self.anum = None
        self.interface_meidee()


    def setup(self):
        """!Appelée par le gestionnaire de tab lors de l'affichage"""
        pass

    def teardown(self):
        """!Appelée par le gestionnaire de tab lors du masquage (passage à un autre tab)"""
        self.meidee.cleanup()



    def interface_meidee(self):
        """!Fonction principale de création de l'interface

        """
        self.columnconfigure(0, weight=1)
        #self.rowconfigure(1, weight=1)
        self.rowconfigure(2, weight=1)
        #self.rowconfigure(3, weight=1)
        l = Label(self,text="Visualisation", pady=5, font=("Helvetica", "16") )
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
        # menu de selection du resultat numerique
        Label(f,text="Modèle numérique").grid(row=0,column=0)
        self.var_resu_num = StringVar()
        self.menu_resu_num = MyMenu( f, self.meidee_objects.get_resultats_num(), self.var_resu_num )
        self.menu_resu_num.grid(row=0, column=1)

        # menu de selection du resultat experimental
        Label(f,text="Modèle experimental").grid(row=0,column=2)
        self.var_resu_exp = StringVar()
        self.menu_resu_exp = MyMenu( f, self.meidee_objects.get_resultats_num(), self.var_resu_exp )
        self.menu_resu_exp.grid(row=0, column=3)

        # menu de selection de la norme numerique
        Label(f,text="Norme numérique").grid(row=1,column=0)
        self.var_norme_num = StringVar()
        self.menu_norme_num = MyMenu( f, self.meidee_objects.get_matr_norme(), self.var_norme_num )
        self.menu_norme_num.grid(row=1, column=1)
        
        # menu de selection de la norme experimentale
        Label(f,text="Norme experimental").grid(row=1,column=2)
        self.var_norme_exp = StringVar()
        self.menu_norme_exp = MyMenu( f, self.meidee_objects.get_matr_norme(), self.var_norme_exp )
        self.menu_norme_exp.grid(row=1, column=3)

        Button(f, text="Valider", command=self.validate_models).grid(row=4,column=0,columnspan=4)
        
        return f

    def interface_visu(self, root):
        """!Création de l'interface de visualisation

        permet de choisir d'afficher les matrices MAC ou les modes avec gmsh
        gère la compatibilité des choix de l'utilisateur (les calculs de MAC
        ne sont pas tous possibles)
        """
        f1 = Frame(root, relief='ridge', borderwidth=4 )
        b1 = Checkbutton(f1,
                         text='Modes experimentaux',
                         command=self.check_state,
                         variable=self.mode_exp)
        b2 = Checkbutton(f1,
                         text='Modes étendus',
                         command=self.check_state,
                         variable=self.mode_etendu)
        b3 = Checkbutton(f1,
                         text='Modes numériques',
                         command=self.check_state,
                         variable=self.mode_nume)
        b4 = Checkbutton(f1,
                         text='Modes etendus réduits',
                         command=self.check_state,
                         variable=self.mode_nume_red)
        b1.grid(row=0, sticky='w' )
        b2.grid(row=1, sticky='w' )
        b3.grid(row=2, sticky='w' )
        b4.grid(row=3, sticky='w' )
        self.mac_button = Button(f1,text='Afficher MAC',command=self.view_macs,state='disabled')
        self.mac_button.grid(row=0,column=1 )
        self.phi_button = Button(f1,text='Afficher Déformées',command=self.view_modes,state='disabled')
        self.phi_button.grid(row=1,column=1 )
        Button(f1, text='Exporter', command=self.export_var ).grid(row=3, column=1)
        self.export_name = StringVar()
        Entry( f1, textvariable=self.export_name ).grid(row=3, column=2 )
        return f1

    def interface_parametres(self, root):
        """!Création de l'interface de choix des paramètres de calcul

        On permet à l'utilisateur de choisir les modes numériques et expérimentaux
        ainsi que la méthode de projection
        """
        main_param = Frame(root)
        main_param.rowconfigure(1,weight=1)
        main_param.columnconfigure(0,weight=1)
        l = Label(main_param, text="Paramètres" )
        l.grid(row=0)
        f = Frame(main_param, relief='ridge', borderwidth=4 )
        # les parametres vont dans 'f'
        proj_mesu_frame = Frame(f,relief='ridge', borderwidth=4)
        
        l = Label(proj_mesu_frame, text="PROJ_MESU_MODAL")
        l.grid(row=0, column=0, columnspan=3 )
        b1 = Radiobutton(proj_mesu_frame, text="SVD", value="SVD", variable=self.proj_champ_meth )
        b1.grid(row=1, column=0)
        Label(proj_mesu_frame, text="Eps=").grid(row=1,column=1)
        Entry(proj_mesu_frame, textvariable=self.proj_svd_param ).grid(row=1,column=2)
        b2 = Radiobutton(proj_mesu_frame, text="LU", value="LU", variable=self.proj_champ_meth )
        b2.grid(row=2, column=0)
        self.proj_champ_meth.set("SVD")
        self.proj_svd_param.set("1.E-2")
        proj_mesu_frame.grid(row=0,column=0)

        #l = Checkbutton( f, text='Utiliser la matrice de masse numérique', variable=self.use_nume_mass )
        #l.grid( row=1, column=0, columnspan=2 )
        l = Button(f,text='Calculer',command=self.prepare_exp)
        l.grid( row = 1, column = 0, columnspan = 2)
        
        # liste des modes numeriques
        f.rowconfigure(0,weight=1)

        self.liste_num = ModeList( f, "Modes Numériques" )
        self.liste_num.grid(row=0, column=2, rowspan=2, sticky='w'+'e'+'s'+'n' )
        self.liste_exp = ModeList( f, "Modes Expérimentaux" )
        self.liste_exp.grid(row=0, column=3, rowspan=2, sticky='w'+'e'+'s'+'n' )
        
        self.refresh_modes()
        f.grid(row=1, sticky='w'+'e'+'s'+'n')
        return main_param

    def refresh_modes(self):
        """!Rafraichit la liste des modes expérimentaux et numériques"""
        if not self.meidee.is_valid():
            return

        self.freq_num, self.xsi_num, self.mass_num, self.modes_num, bid1, bid2 = self.meidee.get_modes_num()
        self.liste_num.fill_modes(self.freq_num, self.modes_num)
        
        self.freq_exp, self.xsi_exp, self.mass_exp, self.modes_exp, bid1, bid2 = self.meidee.get_modes_exp()
        self.liste_exp.fill_modes(self.freq_exp, self.modes_exp)


    def set_proj_svd(self):
        self.proj_champ_meth.set("SVD")
        
    def set_proj_crout(self):
        self.proj_champ_meth.set("LU")


    def check_state(self):
        """!Verifie le nombre de modes selectionnés et
        active le bouton MAC lorsque deux modes sont
        sélectionnés
        """
        modeles = []
        if self.mode_exp.get():
            modeles.append( "phi_exp" )
        if self.mode_etendu.get():
            modeles.append( "phi_et" )
        if self.mode_nume.get():
            modeles.append( "phi_num" )
        if self.mode_nume_red.get():
            modeles.append( "phi_num_red" )
        modeles.sort()
        modeles = tuple(modeles)
        if modeles in self.meidee.Calc_Modes:
            self.mac_button.configure(state='normal') # Active ou désactive le boutton Afficher MAC
        else:
            self.mac_button.configure(state='disabled')
        if len(modeles)>0:
            self.phi_button.configure(state='normal')
        else:
            self.phi_button.configure(state='disabled')
        return modeles

    def view_modes(self, *args):
        """!Visualisation des modes par GMSH. On utilise la classe
        GMSH de Stanley
        """
        if not self.meidee.is_valid():
            return
        num_modes = self.get_selected_modes(self.modes_num_list,
                                            self.freq_num,
                                            self.modes_num)

        exp_modes = self.get_selected_modes(self.modes_exp_list,
                                            self.freq_exp,
                                            self.modes_exp)
        fichiers = []
        self.meidee.prep_fichier( self.mode_nume.get(),
                                  self.mode_exp.get(),
                                  self.mode_nume_red.get(),
                                  self.mode_etendu.get())
        param = { 'mode' : 'LOCAL',
                  'SKIN' : 'NON',
                  'gmsh' : GMSH_PATH}
        term = GMSH('POST', self.meidee.get_fichier(), param, options={} )
        self.term_gmsh.append( term )


    def validate_models(self):
        """!Valide les choix de l'utilisateur et modifie l'interface
        en conséquences
        """
        mdo = self.meidee_objects
        resu_num = mdo.get_resu(self.var_resu_num.get())
        resu_exp = mdo.get_resu(self.var_resu_exp.get())
        norme_num = mdo.get_matr(self.var_norme_num.get())
        norme_exp = mdo.get_matr(self.var_norme_exp.get())
        
        self.meidee.Setup( resu_num=resu_num,
                           resu_exp=resu_exp,
                           norme_num=norme_num,
                           norme_exp=norme_exp)

        self.refresh_modes()


    

    def get_selected_modes(self, lst, freq, modes):
        """!Renvoie un tuple (numeros des modes, freq des modes)"""
        sel = lst.curselection()
        _num = []
        _txt = []
        if not sel:
            sel = [ "%d"% (i) for i in range( freq.shape[0]) ]
        for i in sel:
            idx = int(i)
            f = freq[idx,1]
            n = modes[idx,1] 
            _num.append(int(n))
            _txt.append("%.2f"% f)
        return _num, _txt

            
    def prepare_exp(self, *args):
        """! Demande le lancement de la macro MACRO_VISU_MEIDEE_PROJ
        """
        self.modes_num_list = self.liste_num.return_list()
        self.modes_exp_list = self.liste_exp.return_list()
        modeles, num_modes, exp_modes, norme_num, norme_exp = self.prepare_macs()
        res_num = self.meidee.resu_num
        res_exp = self.meidee.resu_exp
        self.meidee.calc_proj_resu(num_modes, exp_modes, res_num, res_exp, basename = None)


    def prepare_macs(self):
        """!Préparations des objets utilisés pour le calcul et
        l'affichage de MAC_MODEs

        \return un tuple (type modele1,type modele2) et les modes 1, modes 2, norme 1, norme 2
        """
        modeles = self.check_state()
        self.modes_num_list = self.liste_num.return_list()
        self.modes_exp_list = self.liste_exp.return_list()        
        num_modes = self.get_selected_modes(self.modes_num_list,
                                            self.freq_num,
                                            self.modes_num)

        exp_modes = self.get_selected_modes(self.modes_exp_list,
                                            self.freq_exp,
                                            self.modes_exp)

        norme_num = self.meidee_objects.get_matr(self.var_norme_num.get())
        norme_exp = self.meidee_objects.get_matr(self.var_norme_exp.get())
        proj_meth = self.proj_champ_meth.get()
        if proj_meth == "SVD":
            try:
                proj_param = ( float(self.proj_svd_param.get()), )
            except :
                self.proj_svd_param.set("1.E-2")
                proj_param = ( 1.E-2, )
        else:
            proj_param = ()
        self.meidee.proj_meth = proj_meth
        self.meidee.proj_param = proj_param
        return modeles, num_modes, exp_modes, norme_num, norme_exp

    def export_var(self):
        """!Exporte le calculs de mode avec le nom basename"""
        basename = self.export_name.get()
        if len(basename)>5:
            self.mess.disp_mess( "Le nom doit etre de 5 caractères maximum" )
        print "test1"            
        modeles, num_modes, exp_modes, norme_num, norme_exp = self.prepare_macs()
        resu_num = self.meidee.resu_num
        resu_exp = self.meidee.resu_exp   
        self.meidee.calc_proj_resu( num_modes, exp_modes, resu_num, resu_exp, basename )
##        self.meidee_objects.recup_objects()
        self.mess.disp_mess(("Création des concepts : " + basename + "_ET, " +
                            basename+"_NX, " + basename + "_EX, " + basename + "_RD, "))

    def view_macs(self, *args):
        """!Visualisation des modes MAC

        On vérifie la validité du ou des deux jeux de modes sélectionnés, puis on
        récupère la matrice de MAC par l'objet self.meidee et enfin on affiche le
        résultat dans une fenetre MacWindow
        """
        if not self.meidee.is_valid():
            return
        modeles, num_modes, exp_modes, norme_num, norme_exp = self.prepare_macs()
        titre = "MAC "+" + ".join(modeles)
        if self.var_norme_num.get():
            titre+=" num="+self.var_norme_num.get()
        if self.var_norme_exp.get():
            titre+=" exp="+self.var_norme_exp.get()
        
        mac, m1, m2 = self.meidee.get_mac( modeles,
                                           num_modes, exp_modes,
                                           norme_num,
                                           norme_exp )
        self.create_mac_window( titre, m1, m2, mac )

    def quit(self):
        if self.term is not None:
            self.Term.Fermer()


    def create_mac_window(self, title, modes1, modes2, mat ):
        """!Creation d'une nouvelle fenetre de visu MAC"""
        mac_win = MacWindow( self, title, modes1, modes2, mat )
        self.mac_windows.append( mac_win )

    def destroy_mac_window(self, mac_win ):
        """!Une fenetre MAC a été détruite"""
        try:
            self.mac_windows.remove( mac_win )
        except ValueError:
            pass


