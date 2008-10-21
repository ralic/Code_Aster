#@ MODIF meidee_calcul_correlation Meidee  DATE 21/10/2008   AUTEUR NISTOR I.NISTOR 

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
import aster
from Accas import _F
from Utilitai.Utmess import UTMESS
from Cata.cata import modele_sdaster , matr_asse_depl_r
from Cata.cata import mode_meca, dyna_harmo, maillage_sdaster, base_modale
from Cata.cata import cara_elem, cham_mater, table_sdaster, table_fonction
from Cata.cata import IMPR_RESU, RECU_TABLE, MAC_MODES, DETRUIRE
from Cata.cata import INFO_EXEC_ASTER, DEFI_FICHIER
from Cata.cata import DEFI_FICHIER, CO, MACRO_EXPANS

from Meidee.meidee_cata import Resultat, DynaHarmo


#######################
#                     #
#  CLASSES DE CALCUL  #
#                     #
#######################

def extract_mac_array( mac_mode,nom_table ):
    """!Reconstruit un tableau numeric de modes MAC

    /param mac_mode concept Table aster
    """
    data1 = mac_mode.EXTR_TABLE().Array('NUME_MODE_1',nom_table)
    data2 = mac_mode.EXTR_TABLE().Array('NUME_MODE_2',nom_table)
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
    def __init__(self, macro, mess, objects, param_visu=None):
        """!Constructeur

        \param macro le self de l'objet macro provenant de calc_essai_ops
        """
        self.resu_num = None
        self.resu_exp = None
        self.norme_num = None
        self.norme_exp = None
##        self.Calc_Modes = {
##            ("phi_et", ) : self.mac_et,
##            ("phi_et", "phi_num" ) : self.mac_et_num,
##            ("phi_exp", ) : self.mac_exp,
##            ("phi_exp", "phi_num_red") : self.mac_exp_red,
##            ("phi_num",) : self.mac_num,
##            ("phi_num_red",) : self.mac_numred,
##            }
##        self.resu_et = None
##        self.resu_num_extr = None
##        self.resu_red = None
##        self.resu_exp_extr = None
        self.meidee_objects = objects
        self.proj_meth = "SVD"
        self.proj_param = (1.e-2,)
        self.concepts = {}       # mapping number -> (name,owned)
        self.concepts_names = {} # mapping name -> number
        self.macro = macro
        self.mess = mess
        self.param_visu = param_visu

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

##    def is_valid( self ):
##        """!Indique si on a specifie un resultat numerique et un resultat experimental
##
##        Valide si on a precise un resultat numerique et un resultat
##        experimental"""
##        bool = (self.resu_num is not None and self.resu_exp is not None)
##        return bool



    def setup( self, resu_num, mode_num_list, resu_exp, mode_exp_list ):
        """  on donne a Aster les donnees numerique spour le calcul d'expansion
        """
        self.resu_num = resu_num
        self.mode_num_list = mode_num_list
        self.resu_exp = resu_exp
        self.mode_exp_list = mode_exp_list


    def get_modes_num(self):
        return self.resu_num.get_modes()

    def get_modes_exp(self):
        return self.resu_exp.get_modes()

    def calc_proj_resu(self, basename='tmp', reso=None):
        """!Projection des modes experimentaux sur la base modale numerique
            reso est different de None en mode non interactif. En mode
            interactif, on va chercher les parametres de resolution dans
            l'IHM.
            MODIF 24/07/2007:utiliser le concept de register result pour les
            concepts temporaires"""
        self.mess.disp_mess( "Debut de MACRO_EXPANS")
        mdo = self.meidee_objects
        name_nx = basename+"_NX"
        name_ex = basename+"_EX"
        name_et = basename+"_ET"
        name_rd = basename+"_RD"
        res_et = CO(name_et)
        res_rd = CO(name_rd)
        res_nx = res_ex = None

        # Parametres de resolution
        if reso == None:
            reso = []
            if self.proj_meth == "SVD":
                reso.append({"METHODE":"SVD", "EPS":self.proj_param[0] })
            else:
                reso.append({"METHODE":"LU"})


        # Preparation des donnees de mesure
        nume = None
        mcfact_mesure = { 'MODELE': self.resu_exp.modele.obj,
                          'MESURE': self.resu_exp.obj,
                          'NOM_CHAM':self.resu_exp.nom_cham}
        if isinstance( self.resu_exp.obj, mode_meca):
            if self.mode_exp_list : 
                res_ex = CO(name_ex)
                mcfact_mesure.update({ 'NUME_ORDRE': tuple(self.mode_exp_list)})
        if isinstance( self.resu_exp.obj, dyna_harmo):
            nume = self.resu_exp.nume_ddl # aller chercher a la main le nume_ddl

        # Preparation des donnees numeriques pour la base d'expansion
        mcfact_calcul = { 'MODELE': self.resu_num.modele.obj,
                          'BASE': self.resu_num.obj}
        if self.mode_num_list :
            mcfact_calcul.update({ 'NUME_ORDRE': tuple(self.mode_num_list)})
            res_nx = CO(name_nx)

        try:
            MACRO_EXPANS(
                          MODELE_CALCUL = mcfact_calcul,
                          MODELE_MESURE = mcfact_mesure,
                          NUME_DDL = nume,
                          RESU_NX = res_nx,
                          RESU_EX = res_ex,
                          RESU_ET = res_et,
                          RESU_RD = res_rd,                           
                          RESOLUTION = reso)
        except aster.error :
            self.mess.disp_mess( "Erreur dans MACRO_EXPANS")
            UTMESS('A','MEIDEE0_7')
            return

        if basename == "":
            if res_nx : 
                mdo.register_weakref(res_nx.nom)
            if res_ex : 
                mdo.register_weakref(res_ex.nom)
            mdo.register_weakref(res_et.nom)
            mdo.register_weakref(res_rd.nom)

        # Si des resultats RESU_NX et RESU_EX ont ete crees, on en fait des instances de la classe Resultat
        if res_ex:
            mdo.update( name_ex, res_ex )
        else:
            self.resu_exp_extr = self.resu_exp
        if res_nx:
            mdo.update( name_nx, res_nx )
        else:
            self.resu_num_extr = self.resu_num
        mdo.update( name_et, res_et )
        mdo.update( name_rd, res_rd )
        
        self.mess.disp_mess( "Fin de MACRO_EXPANS")
        self.mess.disp_mess( " ")


##    def mac_et_num(self, num_modes, exp_modes, num_norme, exp_norme):
##        return self.resu_num_extr, self.resu_et, num_modes, exp_modes, num_norme
##
##    def mac_exp_red(self, num_modes, exp_modes, num_norme, exp_norme):
##        return self.resu_exp_extr, self.resu_red, num_modes, exp_modes, exp_norme
##
##    def mac_numred(self, num_modes, exp_modes, num_norme, exp_norme):
##        return self.resu_red, self.resu_red, num_modes, exp_modes, exp_norme
##
##    def mac_et(self, num_modes, exp_modes, num_norme, exp_norme):
##        return self.resu_et, self.resu_et, num_modes, exp_modes, num_norme
##    
##    def mac_num(self, num_modes, exp_modes, num_norme, exp_norme):
##        return self.resu_num_extr, self.resu_num_extr, num_modes, num_modes, num_norme
##        
##    def mac_exp(self, num_modes, exp_modes, num_norme, exp_norme):
##        return self.resu_exp_extr, self.resu_exp_extr, exp_modes, exp_modes, exp_norme

    def calc_mac_mode( self, resu1, resu2, norme):
        """!Calcul de MAC entre deux bases modales compatibles"""
        o1 = resu1.obj
        o2 = resu2.obj
        try:
            __MAC = MAC_MODES( BASE_1 = o1,
                               BASE_2 = o2,
                               MATR_ASSE = norme,
                               INFO = 1)
        except aster.FatalError:
            self.mess.disp_mess( "Calcul de MAC impossible : bases incompatibles")
            UTMESS('A','MEIDEE0_7')
            return
        self.mess.disp_mess( (  "      " ) )
        mac = extract_mac_array( __MAC, 'MAC')
        DETRUIRE( CONCEPT = _F( NOM = (__MAC,)), INFO = 1)
        return mac

##    def get_mac(self, modeles, num_modes, exp_modes, num_norme, exp_norme ):
##        """!Calculer le MAC entre deux analyses modales
##
##        On commence par extraire les modes selectionnes pour le calcul et calculer
##        les projections (modes etendus, modes etendus reduits) avant d'utiliser
##        \see calc_mac_mode
##        """
##        oper = self.Calc_Modes[ modeles ]
##        resu1, resu2, modes1, modes2, norme = oper(num_modes, exp_modes, num_norme, exp_norme)
##        res = self.calc_mac_mode(resu1,resu2,modes1,modes2,norme, modeles)
##        return res
##
##    def get_res_mac(self, modeles, num_modes, exp_modes, num_norme, exp_norme ):
##        """renvoie juste les resus qui ont servi a calculer le MAC"""
##        oper = self.Calc_Modes[ modeles ]
##        resu1, resu2, modes1, modes2, norme = oper(num_modes, exp_modes, num_norme, exp_norme)
##    return resu1, resu2



