#@ MODIF meidee_calcul_correlation Meidee  DATE 26/03/2008   AUTEUR BODEL C.BODEL 

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


import Numeric
from Accas import _F
from Cata.cata import modele_sdaster , mode_meca, matr_asse_depl_r, maillage_sdaster
from Cata.cata import cara_elem, cham_mater, table_sdaster, table_fonction
from Cata.cata import IMPR_RESU, RECU_TABLE, MAC_MODES, DETRUIRE
from Cata.cata import INFO_EXEC_ASTER, DEFI_FICHIER
from Cata.cata import DEFI_FICHIER, CO, MACRO_EXPANS

from Meidee.meidee_cata import Resultat

# Recuperation de deux UL disponibles pour les operations du superviseur GMSH
# TODO: proprifier ca si possible
_TUL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
_ULGMSH=_TUL['UNITE_LIBRE',1]
DEFI_FICHIER(FICHIER='TMP',UNITE=_ULGMSH)
_TUL2=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
_ULMAIL=_TUL2['UNITE_LIBRE',1]
DEFI_FICHIER(ACTION='LIBERER',UNITE=_ULGMSH)
DETRUIRE(CONCEPT = _F(NOM = (_TUL,_TUL2)), INFO=1)

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

    Cette classe un peu fourre-tout a pour but de separer les calculs
    specifique aster de l'interface graphique. L'objectif etant de pouvoir
    tester plus rapidement l'interface graphique en cours de developpement.
    
    """
    def __init__(self, macro, mess, objects):
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
        self.objects = objects
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

        On implemente __getitem__ pour les meme raison, et pour
        faire de l'allocation dynamique de numero de concept
        on se comporte comme une liste"""
        return self.concepts[n]

    def is_valid( self ):
        """!Indique si on a specifie un resultat numerique et un resultat experimental

        Valide si on a precise un resultat numerique et un resultat
        experimental"""
        bool = (self.resu_num is not None and self.resu_exp is not None)
        return bool

    def Setup( self, resu_num, resu_exp,
               norme_num, norme_exp ):
        """!Parametrage de l'objet
        
        On fournit les objets aster necessaires au calcul. Par defaut, la liste des modes
        selectionnes dans self.modes_num_liste est la liste de tous les numeros d'ordre.
        """
        self.resu_num = resu_num
        self.resu_exp = resu_exp
        self.norme_num = norme_num
        self.norme_exp = norme_exp
        self.resu_et = None
        self.resu_red = None
        self.resu_num_extr = None
        self.resu_exp_extr = None


    def get_modes_num(self):
        return self.resu_num.get_modes()

    def get_modes_exp(self):
        return self.resu_exp.get_modes()

    def calc_proj_resu(self, num_modes, exp_modes,
                       resu_num, resu_exp,
                       basename='',
                       reso=None):
        """!Projection des modes experimentaux sur la base modale numerique
            reso est different de None en mode non interactif. En mode
            interactif, on va chercher les parametres de resolution dans
            l'IHM.
            MODIF 24/07/2007:utiliser le concept de register result pour les
            concepts temporaires"""
        self.mess.disp_mess( "Debut de MACRO_EXPANS")
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
        #self.mess.disp_mess( ('Donnees num             : '+ resu_num.nom))
        #self.mess.disp_mess( ('nume_ordre selectionnes : '+ str(tuple(num_modes[0]))))        
        #self.mess.disp_mess( ('Donnees exp             : '+ resu_exp.nom))
        #self.mess.disp_mess( ('nume_ordre a etendre    : '+ str(tuple(exp_modes[0]))))
        #self.mess.disp_mess( ('type resolution         : ' + reso[0]['METHODE']))
        #self.mess.disp_mess("  ")

        MACRO_EXPANS( MODELE_CALCUL = _F(MODELE = resu_num.modele.obj,
                                         BASE   = resu_num.obj,
                                         NUME_MODE  = tuple(num_modes[0])),
                      MODELE_MESURE = _F(MODELE = resu_exp.modele.obj,
                                         MESURE = resu_exp.obj,
                                         NUME_MODE  = tuple(exp_modes[0])),
                      RESU_NX = res_nx,
                      RESU_EX = res_ex,
                      RESU_ET = res_et,
                      RESU_RD = res_rd,                           
                      RESOLUTION = reso
                     )


        if basename == "":
            self.objects.register_weakref(res_nx.nom)
            self.objects.register_weakref(res_ex.nom)
            self.objects.register_weakref(res_et.nom)
            self.objects.register_weakref(res_rd.nom)
        
        self.resu_num_extr = Resultat(None, name_nx, res_nx, self.mess, owned=False)
        self.resu_exp_extr = Resultat(None, name_ex, res_ex, self.mess, owned=False)
        self.resu_et =Resultat(None, name_et, res_et, self.mess, owned=False)
        self.resu_red=Resultat(None, name_rd, res_rd, self.mess, owned=False)

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
                self.mess.disp_mess( ( "Avez-vous clique sur Calculer ?" ) )
                self.mess.disp_mess( ( "                               " ) )
                return
        try:
            __MAC = MAC_MODES(BASE_1=o1,
                              BASE_2=o2,
                              MATR_ASSE=norme,
                              INFO=1
                              )
        except aster.FatalError:
            self.mess.disp_mess( "Calcul de MAC impossible : bases incompatibles")
            UTMESS('A','MEIDEE0_3')
            return
        self.mess.disp_mess( (  "      " ) ) 
        mac = extract_mac_array( __MAC )
        DETRUIRE(CONCEPT=_F(NOM=(__MAC,)), INFO=1)
        return mac, modes1, modes2

    def get_mac(self, modeles, num_modes, exp_modes, num_norme, exp_norme ):
        """!Calculer le MAC entre deux analyses modales

        On commence par extraire les modes selectionnes pour le calcul et calculer
        les projections (modes etendus, modes etendus reduits) avant d'utiliser
        \see calc_mac_mode
        """

        #self.mess.disp_mess("Calcul de MAC_MODE pour les modes :")
        #self.mess.disp_mess( (str(num_modes) ) )
        #self.mess.disp_mess( (str(exp_modes) ) )
        #self.mess.disp_mess("  ")
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
                       MODELE  = self.resu_num.modele.obj,
                       RESU    = _F(RESULTAT=self.resu_num_extr.obj,
                                    TYPE_CHAM = 'VECT_3D',
                                    NOM_CMP = ('DX','DY','DZ')),
                       )
        if mode_exp:
            if not self.resu_exp_extr:
                self.resu_exp_extr = self.resu_exp
            IMPR_RESU( UNITE   = _ULGMSH,
                       FORMAT  = 'GMSH',
                       MODELE  = self.resu_exp.modele.obj,
                       RESU    = _F(RESULTAT=self.resu_exp_extr.obj,
                                    TYPE_CHAM = 'VECT_3D',
                                    NOM_CMP = ('DX','DY','DZ')),
                       )

        if mode_et:
            if not self.resu_et:
                self.mess.disp_mess( "!!   Les calculs d'expansion n'ont pas    !!" )
                self.mess.disp_mess( "!! encore ete faits. Cliquer sur Calculer !!" )
                self.mess.disp_mess( "   " )
            IMPR_RESU( UNITE   = _ULGMSH,
                       FORMAT  = 'GMSH',
                       MODELE  = self.resu_num.modele.obj,
                       RESU    = _F(RESULTAT=self.resu_et.obj,
                                    TYPE_CHAM = 'VECT_3D',
                                    NOM_CMP = ('DX','DY','DZ')),
                       )
        if mode_num_red:
            if not self.resu_red:
                self.mess.disp_mess( "!!   Les calculs d'expansion n'ont pas    !!" )
                self.mess.disp_mess( "!! encore ete faits. Cliquer sur Calculer !!" )
                self.mess.disp_mess( "   " )
            IMPR_RESU( UNITE   = _ULGMSH,
                       FORMAT  = 'GMSH',
                       MODELE  = self.resu_exp.modele.obj,
                       RESU    = _F(RESULTAT=self.resu_red.obj,
                                    TYPE_CHAM = 'VECT_3D',
                                    NOM_CMP = ('DX','DY','DZ')),
                       )
        DEFI_FICHIER(ACTION='LIBERER', UNITE=_ULGMSH)

    def get_fichier(self):
        return "fort.%d" % _ULGMSH

