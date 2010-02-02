#@ MODIF meidee_calcul_correlation Meidee  DATE 28/01/2010   AUTEUR BODEL C.BODEL 
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

import Numeric
import aster
from Accas import _F
from Utilitai.Utmess import UTMESS
from Cata.cata import modele_sdaster , matr_asse_depl_r
from Cata.cata import mode_meca, dyna_harmo, maillage_sdaster
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
    def __init__(self, macro, mess, objects):
        """!Constructeur

        \param macro le self de l'objet macro provenant de calc_essai_ops
        """
        self.resu_num = None
        self.resu_exp = None
        self.norme_num = None
        self.norme_exp = None
        self.meidee_objects = objects
        self.proj_meth = "SVD"
        self.proj_param = (1.e-2,)
        self.concepts = {}       # mapping number -> (name,owned)
        self.concepts_names = {} # mapping name -> number
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



    def setup( self, resu_num, mode_num_list,
               resu_exp, mode_exp_list,
               param ):
        """  on donne a Aster les donnees numerique spour le calcul d'expansion
        """
        self.resu_num = resu_num
        self.mode_num_list = mode_num_list
        self.resu_exp = resu_exp
        self.mode_exp_list = mode_exp_list
        self.param = param
        

    def calc_proj_resu(self,suffix,basename):
        """!Mancement de MACRO_EPXANS et export des resultats si demande
            4 resultats sont crees, nommes basename + suffix, ou
            suffix = ['_NX','_EX','_ET','_RD']"""
        self.mess.disp_mess( "Debut de MACRO_EXPANS")
        mdo = self.meidee_objects

        if not basename:
            basename = 'tmp'
        for suf in suffix:
            if mdo.resultats.has_key(basename+suf):
                # Destruction des concepts existants si ils existent deja
                self.mess.disp_mess("destruction de "+basename+suf)
                DETRUIRE( CONCEPT=_F(NOM=mdo.resultats[basename+suf].obj))

        # res_ET et res_RD sont initialises automatiquement
        res_ET = CO(basename+suffix[2])
        res_RD = CO(basename+suffix[3])

        # Preparation des donnees de mesure
        nume = None
        mcfact_mesure = { 'MODELE': self.resu_exp.modele.obj,
                          'MESURE': self.resu_exp.obj,
                          'NOM_CHAM':self.resu_exp.nom_cham}
        args = {}

        # modif : la partie commentee ci-dessus devrait marcher bien comme ca
        if self.mode_exp_list :
            # res_EX est genere que si on a selectionne une partie des modes
            res_EX = CO(basename+suffix[1])
            args.update({'RESU_EX':res_EX})
            mcfact_mesure.update({ 'NUME_ORDRE': tuple(self.mode_exp_list)})
            if isinstance( self.resu_exp.obj, dyna_harmo):
                nume = self.resu_exp.nume_ddl # aller chercher a la main le nume_ddl

        # Preparation des donnees numeriques pour la base d'expansion
        mcfact_calcul = { 'MODELE': self.resu_num.modele.obj,
                          'BASE'  : self.resu_num.obj}
        if self.mode_num_list :
            # res_NX est genere que si on a selectionne une partie des modes
            res_NX = CO(basename+suffix[0])
            args.update({'RESU_NX':res_NX})
            mcfact_calcul.update({ 'NUME_ORDRE': tuple(self.mode_num_list)})

        # Parametres de resolution
        parametres = self.param

        try:
            MACRO_EXPANS(
                          MODELE_CALCUL = mcfact_calcul,
                          MODELE_MESURE = mcfact_mesure,
                          NUME_DDL = nume,
                          RESU_ET = res_ET,
                          RESU_RD = res_RD,                           
                          RESOLUTION = parametres,
                          **args
                          )


        except Exception,err :
            self.mess.disp_mess( "Erreur dans MACRO_EXPANS")
            UTMESS('A','MEIDEE0_7')
            return

        if self.mode_num_list :
            mdo.update( res_NX.nom, res_NX )
        if self.mode_exp_list :
            mdo.update( res_EX.nom, res_EX )
        mdo.update( res_ET.nom, res_ET )
        mdo.update( res_RD.nom, res_RD )
        
        self.mess.disp_mess( "Fin de MACRO_EXPANS")
        self.mess.disp_mess( " ")



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
            UTMESS('A','MEIDEE0_3')
            return
        self.mess.disp_mess( (  "      " ) )
        mac = extract_mac_array( __MAC, 'MAC')
        DETRUIRE( CONCEPT = _F( NOM = (__MAC,)), INFO = 1)
        return mac



