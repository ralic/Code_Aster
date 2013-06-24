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

# person_in_charge: charles.bodel at edf.fr

import numpy
import aster
from Accas import _F
from Utilitai.Utmess import UTMESS, MESSAGE_LOGGER
from Cata.cata import modele_sdaster , matr_asse_depl_r
from Cata.cata import mode_meca, dyna_harmo, maillage_sdaster
from Calc_essai.cata_ce import Resultat, DynaHarmo


# MESSAGE_LOGGER = classe permettant de formatter et d'afficher les messages d'erreur
mess = MESSAGE_LOGGER()


def extract_mac_array( mac_mode,nom_table ):
    """!Reconstruit un tableau numpy de modes MAC

    /param mac_mode concept Table aster
    """
    data1 = mac_mode.EXTR_TABLE().Array('NUME_MODE_1',nom_table)
    data2 = mac_mode.EXTR_TABLE().Array('NUME_MODE_2',nom_table)
    N = int(numpy.maximum.reduce(data1[:,0]))
    M = int(numpy.maximum.reduce(data2[:,0]))
    mac = numpy.zeros( (N,M) )
    for i in range(data1.shape[0]):
        i1 = int(data1[i,0])-1
        i2 = int(data2[i,0])-1
        mac[ i1, i2 ] = data1[i,1]
    return mac



class CalcEssaiExpansion:
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
        self.ce_objects = objects
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
        from Cata.cata import DETRUIRE, MACRO_EXPANS, CO
        self.mess.disp_mess( "Debut de MACRO_EXPANS")
        mdo = self.ce_objects

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


        except aster.error,err :
            message = "ERREUR ASTER : " + mess.GetText('I',err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess( message) 
            UTMESS('A','CALCESSAI0_7')
            for suf in suffix:
                # destruction : les concepts ont ete initialises, il faut les detruire, meme s'ils sont vides
                DETRUIRE( CONCEPT=_F(NOM=(res_NX,res_EX,res_ET,res_RD)))

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
        from Cata.cata import MAC_MODES, DETRUIRE
        o1 = resu1.obj
        o2 = resu2.obj
        try:
            __MAC = MAC_MODES( BASE_1 = o1,
                               BASE_2 = o2,
                               MATR_ASSE = norme,
                               INFO = 1)
        except aster.error,err:
            message = "ERREUR ASTER : " + mess.GetText('I',err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess( message) 
            UTMESS('A','CALCESSAI0_3')
            return
        self.mess.disp_mess( (  "      " ) )
        mac = extract_mac_array( __MAC, 'MAC')
        DETRUIRE( CONCEPT = _F( NOM = (__MAC,)), INFO = 1)
        return mac


def make_mac_salome(mac,resu1,resu2,unite):
    
    from Cata.cata import LIRE_MAILLAGE, AFFE_MODELE, CREA_CHAMP, DETRUIRE
    from Cata.cata import INFO_EXEC_ASTER, IMPR_RESU, DEFI_FICHIER, CREA_RESU
    import random
    # dimension du MAC

    nb_l = mac.shape[0]-1
    nb_c = mac.shape[1]-1
    # fabrication d'un maillage au format aster
    unite_mesh=make_mesh_mac(nb_l,nb_c)


    __MA = LIRE_MAILLAGE(UNITE=unite_mesh,FORMAT='ASTER')

    __MO = AFFE_MODELE(MAILLAGE=__MA,
                       AFFE=(_F(TOUT='OUI',
                                MODELISATION = 'PLAN_ELDI',
                                PHENOMENE = 'MECANIQUE'),),);


    
    ##affection avec CREA_CHAMP

    ##mcfact = []
    ##ii=0
    ##for ind_l in range(1,nb_l+1):
    ##    for ind_c in range(1,nb_c+1):
    ##        mcfact.append({'MAILLE':'M%s_%s' %(ind_l,ind_c),'NOM_CMP':'X1','VALE':toto[ii]})
    ##        ii+=1

    mcfact = []
##    aller chercher les nume_ordre et les freq
    nume_ordre_1 = resu1.get_modes_data()['NUME_ORDRE']
    nume_ordre_2 = resu2.get_modes_data()['NUME_ORDRE']
    nb_mod1 = len(nume_ordre_1)
    nb_mod2=len(nume_ordre_2)
    freq1 = resu1.get_modes_data()['FREQ']
    if not freq1:freq1=[0.0 for kk in range(nb_mod1)]
    freq2 = resu2.get_modes_data()['FREQ']
    if not freq2:freq2=[0.0 for kk in range(nb_mod2)]


    for ind_l in range(1,nb_l+1):
        for ind_c in range(1,nb_c+1):
            mcfact.append({'MAILLE':'M%s_%s' %(ind_l,ind_c),'NOM_CMP':'ERREST','VALE':mac[ind_l,ind_c]})
            mcfact.append({'MAILLE':'M%s_%s' %(ind_l,ind_c),'NOM_CMP':'NUEST','VALE':nume_ordre_1[ind_l]})
            mcfact.append({'MAILLE':'M%s_%s' %(ind_l,ind_c),'NOM_CMP':'SIGCAL','VALE':freq1[ind_l]})
            mcfact.append({'MAILLE':'M%s_%s' %(ind_l,ind_c),'NOM_CMP':'TERMRE','VALE':nume_ordre_2[ind_c]})
            mcfact.append({'MAILLE':'M%s_%s' %(ind_l,ind_c),'NOM_CMP':'TERMR2 ','VALE':freq2[ind_c]})
            
    ##__CHA = CREA_CHAMP( OPERATION= 'AFFE',
    ##                    TYPE_CHAM='CART_NEUT_R' , MAILLAGE = MAIL,
    ##                    AFFE=mcfact)

    __CHA = CREA_CHAMP( OPERATION= 'AFFE',
                        MODELE=__MO,
                        PROL_ZERO='OUI',
                        TYPE_CHAM='ELEM_ERRE_R' , 
                        AFFE=mcfact)
    
    IMPR_RESU (UNITE=unite, FORMAT='MED',RESU=_F(CHAM_GD=__CHA) )

    DETRUIRE(CONCEPT=_F(NOM=(__MA,__MO,__CHA,)))

    DEFI_FICHIER(ACTION='LIBERER', UNITE=unite_mesh)
    
    return 




def make_mesh_mac(nb_l,nb_c):
    from Cata.cata import INFO_EXEC_ASTER
    _UL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
    unite=_UL['UNITE_LIBRE',1]

    f = open('./fort.%s'%unite,'w')

    f.writelines( 'TITRE : MAILLAGE CARRE POUR REPRESENTATION MAC_MODES\n')
    f.writelines( 'CREE PAR SCRIPT PYTHON AU FORMAT ASTER\n')
    f.writelines( ' FINSF\n')
    f.writelines( ' %\n')
    f.writelines( ' COOR_3D\n')
    ## Les noeuds sont appeles 'Ni1_i2' les mailles 'Mj1_j2' comme suit
    ##         N1_1    N1_2    N1_3    N1_4
    ##             M1_1    M1_2    M1_3
    ##         N2_1    N2_2    N2_3    N2_4
    ##             M2_1    M2_2    M2_3
    ##         N3_1    N3_2    N3_3    N3_4
    ### dimension du MAC

    for ind_l in range(1,nb_l+2):
        for ind_c in range(1,nb_c+2):
            nom_no = 'N%s_%s' %(ind_l,ind_c)
            coordo_x =  ind_c/(nb_c+1.0)
            coordo_y = -ind_l/(nb_l+1.0)
            coordo_z =  0.0
            f.writelines( ' '+ nom_no.ljust(8) +
                          '%22.14E%22.14E%22.14E' %(coordo_x,coordo_y,coordo_z) +
                          '\n')

    f.writelines( ' FINSF\n')
    f.writelines( ' %\n')
    f.writelines( ' QUAD4\n')

    for ind_l in range(1,nb_l+1):
        for ind_c in range(1,nb_c+1):
            nom_maille = 'M%s_%s' %(ind_l,ind_c)
            nom_noeud_1 = 'N%s_%s' %(ind_l,ind_c)
            nom_noeud_2 = 'N%s_%s' %(ind_l,ind_c+1)
            nom_noeud_3 = 'N%s_%s' %(ind_l+1,ind_c+1)
            nom_noeud_4 = 'N%s_%s' %(ind_l+1,ind_c)

            f.writelines( ' '+ nom_maille.ljust(8) + nom_noeud_1.ljust(8) +
                          nom_noeud_2.ljust(8) + nom_noeud_3.ljust(8) +
                           nom_noeud_4.ljust(8) + '\n')

    f.writelines( ' FINSF\n')
    f.writelines( ' %\n')
    f.writelines( ' FIN\n')
    f.close()
    return unite
