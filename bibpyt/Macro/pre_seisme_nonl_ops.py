﻿# coding=utf-8
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

from Noyau.N_types import force_list
from Noyau.N_utils import AsType
from Cata.cata import _F, fonction_sdaster, fonction_c, nappe_sdaster

import numpy as NP
import pprint
import Accas
import aster

from Utilitai.Utmess import UTMESS, ASSERT
from Utilitai.partition import MAIL_PY

from Cata.cata import _F, DETRUIRE, DEFI_MAILLAGE, ASSE_MAILLAGE, \
      AFFE_MATERIAU, AFFE_MODELE, AFFE_CARA_ELEM, LIRE_MAILLAGE,  \
      AFFE_CHAR_MECA, ASSE_MATRICE, DEFI_INTERF_DYNA, CALC_MATR_ELEM, \
      DEFI_BASE_MODALE, MODE_ITER_SIMULT, NUME_DDL, MODE_STATIQUE, \
      MACR_ELEM_DYNA


def pre_seisme_nonl_ops(self, **args):
     """Corps de la macro PRE_SEISME_NONL"""
     ier = 0
     # La macro compte pour 1 dans la numerotation des commandes
     self.set_icmd(1)

     # conteneur des paramètres du calcul
     param = Properties(**args)
    
     # création de l'objet PreSeismeNonL_xxx
     calcul = PreSeismeNonL.Factory(self, param)
     try:
         calcul.run()
     except aster.error, err:
        UTMESS('F', err.id_message, valk=err.valk, vali=err.vali, valr=err.valr)


class PreSeismeNonL(object):
     """Define a general methods for a PRE_SEISME_NONL calculation.
     """
     option_calcul = None

     @staticmethod
     def Factory(parent, param):
        """Factory that returns the calculation object"""
        if param['PARAMETRE'].has_key('PRE_CALC_MISS'):
            return PreCalcMiss(parent, param)
        elif param['PARAMETRE'].has_key('POST_CALC_MISS'):
            return PostCalcMiss(parent, param)
        else:
            raise NotImplementedError, "option calcul non défini"

     def __init__(self, parent, param):
         """initializations"""
         self.parent = parent
         self.param = param
         self.type_calcul = None 
         self.typ_ISS = None
         self.type_IFS = None
         self.model = None
         self.mail = None
         self.mael = None
         self.bamo = None

     def run(self):
         """Execute calculation""" 
         self.define_mesh()
         self.define_model()
         self.define_bamo()
         self.define_mael()

     def define_mesh(self):
         """Define the mesh""" 
         self.mail = Mesh(self, self.param)

     def define_model(self):
         """Define the numerical model""" 
         self.model = Model.factory(self, self.mail, self.param)
         self.model.DefineOut() 

     def define_bamo(self):
         """Define the modal basis""" 
         self.set_type()
         self.calc_base_modale()
         self.bamo.DefineOut()

     def define_mael(self):
         """Define the super-element""" 
         self.mael = MacroElement(self, self.param, self.bamo)
         self.mael.DefineOut()  

     def calc_base_modale(self):
         """Execute the eigenmodes calculation""" 
         raise NotImplementedError('must be defined in a derivated class')

     def set_type(self, txt):
         """Define different type of loads"""
         raise NotImplementedError('must be defined in a derivated class')

     def DeclareOut(self, *args):
         """Define output depending on user choices"""
         self.parent.DeclareOut(*args)



class PreCalcMiss(PreSeismeNonL):
     """Define the interface modal basis used for the soil impedance calculation (PRE_CALC_MISS).
     """
     option_calcul = 'PRE_CALC_MISS'

     def calc_base_modale(self):
         """Execute the eigenmodes calculation""" 
         if self.type_calcul == 'ISFS':
             nbmodes1 = self.param['PARAMETRE']['PRE_CALC_MISS']['NMAX_MODE_ISS']
             nbmodes2 = self.param['PARAMETRE']['PRE_CALC_MISS']['NMAX_MODE_IFS']
             bamoISS  = BaseModale(self, self.param, self.model, nbmodes1, self.typ_ISS)
             bamoISFS = BaseModale(self, self.param, self.model, nbmodes2, self.typ_IFS)
             bamoISFS.defi_base_modale()
             bamoISFS.combine_base_modale(bamoISS)
             self.bamo = bamoISFS
         else:
             nbmodes1 = self.param['PARAMETRE']['PRE_CALC_MISS']['NMAX_MODE_ISS']
             bamoISS = BaseModale(self, self.param, self.model, nbmodes1, self.typ_ISS)
             bamoISS.defi_base_modale()
             self.bamo = bamoISS

     def set_type(self):
         """Set the type of MISS calculation""" 
         self.type_calcul = self.param['PARAMETRE']['PRE_CALC_MISS']['CALC_MISS_OPTION']
         if self.param['PARAMETRE']['PRE_CALC_MISS']['REDUC_DYNA_ISS'] == 'OUI': self.typ_ISS ='DYNA'
         else: self.typ_ISS ='STAT'
         if self.param['PARAMETRE']['PRE_CALC_MISS']['REDUC_DYNA_IFS']== 'OUI' : self.typ_IFS ='DYNA'
         else: self.typ_IFS ='STAT'         



class PostCalcMiss(PreSeismeNonL):
     """Define the numerical model necessary for a transient nonlinear calculation (POST_CALC_MISS).
     """
     option_calcul = 'POST_CALC_MISS'

     def define_mesh(self):
         """Factory that returns the Model object"""
         self.mael = MacroElement(self, self.param)
         self.mail = Mesh(self, self.param, self.mael)
         self.mail.build_mesh()
         self.mail.DefineOut()

     def define_mael(self):
         """Define the super-element"""  

     def define_bamo(self):
         """Define the modal basis"""  

     def calc_base_modale(self):
         """Execute the eigenmodes calculation"""  

     def set_type(self, txt):
         """Set the type of MISS calculation""" 



class BaseModale(object):
     """Define a modal basis.
     """
     def __init__(self, parent, param, model = None, nb_modes = None, typ = None, bamo = None):
         """initializations"""
         self.parent = parent
         self.param = param
         self.model = model
         self.bamo = bamo
         self.cmd_bamo = None
         self.modes = None
         self.nb_modes = nb_modes
         self.interf_dyna = None
         self.nume_ddl = None
         if typ == 'STAT':
             self.assemblage_stat()
             self.modes_statiques()
         else:
             self.assemblage_dyna()
             self.modes_dynamiques()    
   
     def get_num_modes(self):
         """Return the number of modes stored within self.modes"""
         return self.nb_modes

     def get_modes(self):
         """Return the calculated normal modes"""
         return self.modes

     def get_bamo(self):
         """Return the complete modal basis, which might contain other complementary modes"""
         return self.bamo

     def get_mass(self):
         """Return the FE mass matrix associated to the model self.model"""
         return self.matr_mass

     def get_rigi(self):
         """Return the FE stiffness matrix associated to the model self.model"""
         return self.matr_rigi

     def check_radier_rigide(self):
         """Check if solid rigid conditions are considered for the interface"""
         charge = self.param['AFFE_CHAR_MECA'] 
         for key in charge :
             if key == 'LIAISON_SOLIDE':
                if not self.param['PARAMETRE']['PRE_CALC_MISS']['GROUP_NO_CENT']:
                    raise AsException("Le mot-clé GROUP_NO_CENT est obligatoire lorsqu'une LIAISON_SOLIDE est définie")
                return True
         return False 

     def defi_interf_dyna(self):
         """Build the dynamic interface where the substructuring approach is applied"""
         if (self.param['PARAMETRE']['PRE_CALC_MISS']['REDUC_DYNA_ISS'] == 'OUI' or
             self.param['PARAMETRE']['PRE_CALC_MISS']['REDUC_DYNA_IFS'] == 'OUI'):
             grno = self.param['PARAMETRE']['PRE_CALC_MISS']['GROUP_NO_INTERF']
         else:
             if self.check_radier_rigide() :
                 grno = self.param['PARAMETRE']['PRE_CALC_MISS']['GROUP_NO_CENT']
             else:
                 grno = self.param['PARAMETRE']['PRE_CALC_MISS']['GROUP_NO_INTERF']            
         _C_LIM0=AFFE_CHAR_MECA(MODELE=self.model.get_model(), DDL_IMPO=_F(GROUP_NO = grno, 
                                    DX=0.0, DY=0.0, DZ=0.0,),);
         _NUME0=NUME_DDL(MODELE=self.model.get_model(), CHARGE=_C_LIM0,);
         _INTERDY=DEFI_INTERF_DYNA(NUME_DDL=_NUME0, INTERFACE=_F(NOM='DROITE',
                                TYPE='CRAIGB', GROUP_NO = grno),); 
         self.interf_dyna = _INTERDY

     def assemblage_dyna(self):
         """Assemble FE matrices associated to self.model with non-static boundary conditions"""
         _MATMele = CALC_MATR_ELEM( MODELE= self.model.get_model(), CHAM_MATER= self.model.get_mate(),
                           CARA_ELEM = self.model.get_cara_elem(), OPTION= 'MASS_MECA',)
         _MATRele = CALC_MATR_ELEM( MODELE= self.model.get_model(), CHAM_MATER= self.model.get_mate(),
                           CARA_ELEM = self.model.get_cara_elem(), OPTION= 'RIGI_MECA',)
         _NUME = NUME_DDL(MATR_RIGI = _MATRele);
         _MATRIGI = ASSE_MATRICE( MATR_ELEM = _MATRele , NUME_DDL = _NUME );
         _MATMASS= ASSE_MATRICE( MATR_ELEM = _MATMele, NUME_DDL = _NUME );
         self.matr_rigi = _MATRIGI
         self.matr_mass = _MATMASS
         self.nume_ddl = _NUME

     def assemblage_stat(self):
         """Assemble FE matrices associated to self.model with static boundary conditions"""
         _MATMele = CALC_MATR_ELEM( MODELE= self.model.get_model(), CHAM_MATER= self.model.get_mate(),
                           CARA_ELEM = self.model.get_cara_elem(), CHARGE = self.model.get_cond_lim(), OPTION= 'MASS_MECA',)
         _MATRele = CALC_MATR_ELEM( MODELE= self.model.get_model(), CHAM_MATER= self.model.get_mate(),
                           CARA_ELEM = self.model.get_cara_elem(), CHARGE = self.model.get_cond_lim(), OPTION= 'RIGI_MECA',)
         _NUME = NUME_DDL(MATR_RIGI = _MATRele);
         _MATRIGI = ASSE_MATRICE( MATR_ELEM = _MATRele , NUME_DDL = _NUME );
         _MATMASS= ASSE_MATRICE( MATR_ELEM = _MATMele, NUME_DDL = _NUME );
         self.matr_rigi = _MATRIGI
         self.matr_mass = _MATMASS
         self.nume_ddl = _NUME

     def modes_statiques(self):
         """Calculate static/interface modes"""
         if self.check_radier_rigide() :
             grno = self.param['PARAMETRE']['PRE_CALC_MISS']['GROUP_NO_CENT']
         else:
             grno = self.param['PARAMETRE']['PRE_CALC_MISS']['GROUP_NO_INTERF'] 
         _modsta = MODE_STATIQUE(MATR_RIGI = self.matr_rigi,
                  MODE_STAT=_F( GROUP_NO = grno, TOUT_CMP = 'OUI',));
         self.modes = _modsta

     def modes_dynamiques(self):
         """Calculate dynamic modes or modal shapes"""
         _modyna =MODE_ITER_SIMULT(MATR_RIGI=self.matr_rigi, MATR_MASS=self.matr_mass,
                      CALC_FREQ=_F(OPTION='PLUS_PETITE', NMAX_FREQ = self.nb_modes,),);
         self.modes = _modyna

     def defi_base_modale(self):
         """Build the complete modal basis"""
         self.defi_interf_dyna()
         cmd_ritz_1 = _F(MODE_MECA=self.modes, NMAX_MODE=0,)
         cmd_ritz_2 = _F(MODE_INTF=self.modes, NMAX_MODE=self.nb_modes,)
         self.cmd_bamo = {'RITZ' : (cmd_ritz_1, cmd_ritz_2), 'INTERF_DYNA' : self.interf_dyna, 'NUME_REF' : self.nume_ddl}

     def combine_base_modale(self, bamo):
         """Combine two different modal basis"""
         _BAMO0 = DEFI_BASE_MODALE(**self.cmd_bamo);
         cmd_ritz_1 = _F(BASE_MODALE = _BAMO0,)
         cmd_ritz_2 = _F(MODE_INTF = bamo.get_modes(), NMAX_MODE = bamo.get_num_modes(),)
         self.cmd_bamo = {'RITZ' : (cmd_ritz_1, cmd_ritz_2), 'INTERF_DYNA' : self.interf_dyna, 'NUME_REF' : self.nume_ddl}

     def DefineOut(self):
         """Define output depending on user choices"""
         if self.param['RESULTAT'].has_key('BASE_MODALE'):
             self.parent.DeclareOut('_BAMO', self.param['RESULTAT']['BASE_MODALE'])
         _BAMO=DEFI_BASE_MODALE(**self.cmd_bamo);         
         self.bamo = _BAMO


class MacroElement(object):
     """Define a sub-structure, also known as super-element or macro-element.
     """
     def __init__(self, parent, param, BaMo = None):
         """initializations"""
         self.parent = parent
         self.param = param
         self.bamo = BaMo
         self.set_mael()

     def check_reduc_dyna(self):
         """Check if dynamic reduction is used within the super-element"""
         reduc_dyna = False
         if self.param['PARAMETRE'].has_key('PRE_CALC_MISS'):
             if (self.param['PARAMETRE']['PRE_CALC_MISS']['REDUC_DYNA_ISS'] == 'OUI' or
                 self.param['PARAMETRE']['PRE_CALC_MISS']['REDUC_DYNA_IFS'] == 'OUI'):
                 reduc_dyna = True
         if self.param['PARAMETRE'].has_key('POST_CALC_MISS'):
             bamo_nom_long = aster.getvectjev(self.mael.nom + (8-len(self.mael.nom))*' ' + '.MAEL_REFE')
             bamo_nom_short = bamo_nom_long[0].split()[0]
             NOEUD_CMP = aster.getvectjev(bamo_nom_short + (19-len(bamo_nom_short))*' ' + '.RS16')
             if NOEUD_CMP[0].strip() == '':
                 reduc_dyna = True
         return reduc_dyna

     def set_mael(self):
         """Build the super-element"""
         if self.param['PARAMETRE'].has_key('POST_CALC_MISS'):
             self.mael = self.param['PARAMETRE']['POST_CALC_MISS']['MACR_ELEM_DYNA']
         else:
             self.mael = None

     def get_mael(self):
         """Return the super-element"""
         return self.mael

     def DefineOut(self):
         """Define output depending on user choices"""
         Bamo = self.bamo.get_bamo()
         self.parent.DeclareOut('_Mael', self.param['RESULTAT']['MACR_ELEM_DYNA'])
         _Mael = MACR_ELEM_DYNA(BASE_MODALE= Bamo, MATR_RIGI = self.bamo.get_rigi(),    
                       MATR_MASS = self.bamo.get_mass(), SANS_GROUP_NO=self.param['PARAMETRE']['PRE_CALC_MISS']['GROUP_NO_INTERF'],)



class Properties(object):
     """Define a dictionary containing the keywords of the model properties.
     """
     def __init__(self, **kwargs):
        """initializations"""
        self._keywords = {}
        for key in kwargs.keys():
            if hasattr(kwargs[key], 'List_F'):
                self._keywords[key] = kwargs[key].List_F()[0]
            else:
                self._keywords[key] = kwargs[key]             

     def __getitem__(self, key):
        return self._keywords[key]

     def __setitem__(self, key):
        return self._keywords[key]

     def get_nested_key(self, path):
        """Get a value from a nested dictionary"""
        diction = self._keywords
        for key in path:
            if key not in diction.keys() :
                diction[key] = {}
            diction = diction[key]
        return diction

     def set_key(self, path, value):
         """Set a value within nested dictionary"""
         self.get_nested_key(path[:-1])[path[-1]] = value

     def add_MCFACT(self, path, addedKey):
         """Add new Mot-Clé Facteur into a nested dictionary"""
         diction = self._keywords[path[0]]
         if path[1] not in diction.keys() :
              self.set_key(path, [])
         self._keywords[path[0]][path[1]].insert(0,addedKey)

     def copy(self):
        """Return a new copy of the properties"""
        return self.__class__(**self._keywords.copy())

     def __repr__(self):
        """Print formatted dictionary of the attribut self._keywords"""
        return pprint.pformat(self._keywords)



class Model(object):
    """Define a numerical model.
    """
    option_calcul = None

    @staticmethod
    def factory(parent, mail, properties):
        """Factory that returns the Model object"""
        if properties['PARAMETRE'].has_key('PRE_CALC_MISS'):
            if mail.check_ficti_nodes():
                return ModelBaMoReduc(parent, mail, properties)
            else:
                return ModelBaseModale(parent, mail, properties)
        elif mail.check_ficti_nodes():
            return ModelDynaReduc(parent, mail, properties)
        else:
            return ModelMacrElem(parent, mail, properties)

    def __init__(self, parent, mail, properties):
        """initializations"""
        if not self.option_calcul:
           raise NotImplementedError, "option_calcul non défini"
        self.parent = parent
        self.args = properties.copy()
        self.mesh = mail
        self.modele = None
        self.mate = None
        self.cara_elem = None
        self.cond_lim = None
        self.affe_modele()
        self.affe_materiau()
        self.affe_cara_elem()
        self.affe_char_meca()

    def get_properties(self):
        """Return the set of keywords related to the properties of the numerical model"""
        return self.args   
  
    def get_model(self):
        """Return the numerical model"""
        return self.modele  

    def get_mate(self):
        """Return the set of applied constitutive laws"""
        return self.mate  

    def get_cara_elem(self):
        """Return the set of discrete properties"""
        return self.cara_elem  

    def get_cond_lim(self):
        """Return the set of boundary conditions"""
        return self.cond_lim  
       
    def affe_modele(self):
        """Define the physics of the numerical modelling"""
        raise NotImplementedError('must be defined in a derivated class')

    def affe_materiau(self):
        """Define constitutive discret properties"""
        raise NotImplementedError('must be defined in a derivated class')

    def affe_cara_elem(self):
        """Define discret properties"""
        raise NotImplementedError('must be defined in a derivated class')

    def affe_char_meca(self):
        """Define different type of loads"""
        raise NotImplementedError('must be defined in a derivated class')

    def DefineOut(self):
        """Define output depending on user choices"""
        if self.args['RESULTAT'].has_key('CHAM_MATER'):
            self.parent.DeclareOut('_AMa', self.args['RESULTAT']['CHAM_MATER'])
        _AMa = AFFE_MATERIAU(**self.args['AFFE_MATERIAU'])
        self.mate = _AMa
        if self.args['RESULTAT'].has_key('CARA_ELEM'):                   
            self.parent.DeclareOut('_ACa', self.args['RESULTAT']['CARA_ELEM'])
        _ACa = AFFE_CARA_ELEM(**self.args['AFFE_CARA_ELEM'])
        self.cara_elem = _ACa
        if self.args['RESULTAT'].has_key('CHARGE'):  
            for mcfact in self.args['RESULTAT']['CHARGE']:
                if mcfact['OPTION'] == 'LAPL_TEMPS':
                    self.parent.DeclareOut('ACh_LT', mcfact['NOM'])
                    ACh_LT = AFFE_CHAR_MECA(**self.args['LAPL_TEMPS'])
                elif mcfact['OPTION'] == 'COND_LIM':
                    self.parent.DeclareOut('_ACh_CL', mcfact['NOM'])
        _ACh_CL = AFFE_CHAR_MECA(**self.args['AFFE_CHAR_MECA']) 
        self.cond_lim = _ACh_CL



class ModelMacrElem(Model):
    """Define a numerical model combined with superelements.
    """
    option_calcul = 'Macro_Element'

    def affe_modele(self):
        """Define the physics of the numerical modelling""" 
        self.link_macro_elem()
        self.fiction_model()
        self.args.set_key(('AFFE_MODELE','MAILLAGE'), self.mesh.get_new_mesh()) 
        if self.args['RESULTAT'].has_key('MODELE'):
            self.parent.DeclareOut('_Modele', self.args['RESULTAT']['MODELE'])
        _Modele = AFFE_MODELE(**self.args['AFFE_MODELE']);
        self.modele = _Modele

    def fiction_model(self): 
        """Define fictitious DoF's in the numerical modelling"""   

    def fiction_cara_elem(self): 
        """Define discret properties for the fictitious DoF's"""  

    def link_macro_elem(self):
        """Link the supermesh to a physical phenomena or modelling"""
        super_ma = self.mesh.get_supermaille()
        mcfact_SupMa = _F(SUPER_MAILLE = super_ma, PHENOMENE = 'MECANIQUE')
        self.args.add_MCFACT(('AFFE_MODELE','AFFE_SOUS_STRUC'), mcfact_SupMa)
        self.args.set_key(('AFFE_MODELE','MAILLAGE'), self.mesh.get_new_mesh())        

    def affe_materiau(self): 
        """Define the constitutive law of the numerical modelling""" 
        self.args.set_key(('AFFE_MATERIAU', 'MAILLAGE'), self.mesh.get_new_mesh())

    def affe_cara_elem(self): 
        """Define the constitutive law of discret elements""" 
        self.fiction_cara_elem()
        self.args.set_key(('AFFE_CARA_ELEM', 'MODELE'), self.modele)

    def affe_char_meca(self): 
        """Define boundary conditions and external loads"""   
        self.bound_conds()
        self.seismic_loads()
        self.other_loads()

    def bound_conds(self):
        """Define boundary conditions"""   
        self.args.set_key(('AFFE_CHAR_MECA', 'MODELE'), self.modele)

    def seismic_loads(self):
        """Define seismic loads"""   
        mcfact = self.args.get_nested_key(('RESULTAT','CHARGE'))
        for mm in mcfact:
            if mm['OPTION'] == 'LAPL_TEMPS' :
                cmd_charge = {'SUPER_MAILLE' : 'STAT1'}
                if self.args['PARAMETRE']['POST_CALC_MISS']['UNITE_RESU_RIGI']:
                    UL_rigi = self.args['PARAMETRE']['POST_CALC_MISS']['UNITE_RESU_RIGI']
                    cmd_charge['UNITE_RESU_RIGI'] = UL_rigi
                if self.args['PARAMETRE']['POST_CALC_MISS']['UNITE_RESU_MASS']:
                    UL_mass = self.args['PARAMETRE']['POST_CALC_MISS']['UNITE_RESU_MASS']
                    cmd_charge['UNITE_RESU_MASS'] = UL_mass
                if self.args['PARAMETRE']['POST_CALC_MISS']['UNITE_RESU_AMOR']:
                    UL_amor = self.args['PARAMETRE']['POST_CALC_MISS']['UNITE_RESU_AMOR']
                    cmd_charge['UNITE_RESU_AMOR'] = UL_amor
                charge_sol = _F(**cmd_charge)
                self.args.set_key(('LAPL_TEMPS', 'MODELE'), self.modele)
                self.args.set_key(('LAPL_TEMPS','FORCE_SOL'), charge_sol)

    def other_loads(self):
        """Define other loading entries"""   



class ModelDynaReduc(ModelMacrElem):
    """Define a numerical model that uses dynamic reduction with superelements.
    """
    option_calcul = 'Reduction_Dynamique'

    def fiction_model(self):
        """Define fictitious DoF's in the numerical modelling"""   
        ma_fict_lst = []
        ma_fict_lst.append('MFICTIF') 
        self.ma_fict = tuple(ma_fict_lst)
        mcfact_DisTR = _F(GROUP_MA = self.ma_fict, PHENOMENE = 'MECANIQUE', MODELISATION = 'DIS_TR')
        self.args.add_MCFACT(('AFFE_MODELE','AFFE'), mcfact_DisTR)  

    def fiction_cara_elem(self): 
        """Define discret properties for the fictitious DoF's"""  
        valK = (1.E-9,1.E-9,1.E-9,1.E-9,0.,0.,0.,0.,0.,0.,)
        valM = (1.E-3,1.E-3,1.E-3,1.E-3,1.E-3,1.E-3,)
        mcfact_MEle = _F(GROUP_MA = self.ma_fict, CARA = 'M_TR_D_N', VALE = valK)
        mcfact_KEle = _F(GROUP_MA = self.ma_fict, CARA = 'K_TR_D_N', VALE = valM)
        self.args.add_MCFACT(('AFFE_CARA_ELEM', 'DISCRET'), mcfact_MEle)
        self.args.add_MCFACT(('AFFE_CARA_ELEM', 'DISCRET'), mcfact_KEle)

    def other_loads(self):  
        """Define the relation between the physical and generalized DoF's"""         
        liaison_interf = _F(MACR_ELEM_DYNA = self.args['PARAMETRE']['POST_CALC_MISS']['MACR_ELEM_DYNA'])
        self.args.add_MCFACT(('AFFE_CHAR_MECA', 'LIAISON_INTERF'), liaison_interf)



class ModelBaMoReduc(ModelDynaReduc):
    """Define a numerical model that uses dynamic reduction without superelements.
    """
    option_calcul = 'Base_Modale'

    def link_macro_elem(self): 
        """Assemble the mesh and the superelement"""
    def seismic_loads(self):
        """Define seismic loads"""   
    def other_loads(self): 
        """Define other loading entries"""  



class ModelBaseModale(ModelMacrElem):
    """Define a numerical model that uses static reduction without superelements.
    """
    option_calcul = 'Base_Modale'

    def link_macro_elem(self): 
        """Assemble the mesh and the superelement"""
    def seismic_loads(self):
        """Define seismic loads"""   
    def other_loads(self): 
        """Define other loading entries"""  



class Mesh(object):
    """Define the mesh of the numerical model.
    """
    def __init__(self, parent, param, mael = None):
        """initializations"""
        self.param = param
        self.parent = parent
        self.macro_elem = mael
        self.supermaille = []
        self.__builded = False
        self.new_mesh = param['AFFE_MODELE']['MAILLAGE']
        self.old_mesh = param['AFFE_MODELE']['MAILLAGE']
        self.add_fiction_mesh()

    def get_supermaille(self):
        """Return a set of supermeshes"""
        if self.__builded:
            return self.supermaille

    def build_mesh(self):
        """Assemble the mesh and the superelement"""
        self.old_mesh = self.new_mesh
        if self.macro_elem:
            list_SuperMa = self.__set_list_supermaille(self.macro_elem)
            _MAYADYN = DEFI_MAILLAGE(DEFI_SUPER_MAILLE = list_SuperMa,
                            RECO_GLOBAL = _F(TOUT='OUI',),
                            DEFI_NOEUD = _F(TOUT='OUI', INDEX=(1,0,1,8,),),);
            self.asse_mail = {'MAILLAGE_1': self.old_mesh,'MAILLAGE_2': _MAYADYN,'OPERATION':'SOUS_STR'}
        self.__builded = True

    def DefineOut(self):
        """Define output depending on user choices"""
        if self.__builded:
            if self.param['RESULTAT'].has_key('MAILLAGE'):
                self.parent.DeclareOut('_NewMesh', self.param['RESULTAT']['MAILLAGE'])  
            _NewMesh = ASSE_MAILLAGE(**self.asse_mail);
            self.new_mesh = _NewMesh   

    def get_new_mesh(self):
        """Return the mesh concept which might contain a superelement"""
        return self.new_mesh

    def __set_list_supermaille(self, MacroElem):
        """Build a set of supermeshes"""
        list_sma = []
        key_index = 1       
        list_me = [MacroElem.get_mael()]   
        for mm in list_me:
            name = 'STAT' + str(key_index)
            list_sma.append(_F(MACR_ELEM = mm, SUPER_MAILLE = name,))
            self.supermaille.append(name)
            key_index += 1
        return list_sma 
    
    def get_nb_ficti_no(self):
        """Count the number of fictitious cells and nodes to add to the mesh"""
        if self.macro_elem :
            mael = self.macro_elem.get_mael()
            Nb_no = len(mael._get_sdj().LINO.get())
        else:
            if self.param['PARAMETRE']['PRE_CALC_MISS']['NMAX_MODE_IFS']:
                nb_modes_IFS = self.param['PARAMETRE']['PRE_CALC_MISS']['NMAX_MODE_IFS']
            else:
                nb_modes_IFS = 0
            Nb_no = self.param['PARAMETRE']['PRE_CALC_MISS']['NMAX_MODE_ISS'] + nb_modes_IFS
        return Nb_no

    def check_ficti_nodes(self):
        """Check if fictitious cells and nodes should be added to the mesh"""
        if self.macro_elem :
            return self.macro_elem.check_reduc_dyna()
        elif (self.param['PARAMETRE']['PRE_CALC_MISS']['REDUC_DYNA_ISS'] == 'OUI' or
             self.param['PARAMETRE']['PRE_CALC_MISS']['REDUC_DYNA_IFS'] == 'OUI'):
            return True
        else:
            return False

    def add_fiction_mesh(self):
        """Add fictitious cells and nodes to the mesh"""
        if self.check_ficti_nodes() == False:
            return None
        mail=MAIL_PY()    
        mail.FromAster(self.old_mesh)
        Nb_no = self.get_nb_ficti_no()
        gr_ma=[]       
        mail.correspondance_mailles=list(mail.correspondance_mailles)
        mail.correspondance_noeuds = list(mail.correspondance_noeuds)
        mail.tm=list(mail.tm)
        mail.dime_maillage=list(mail.dime_maillage)
        nb_noeuds = mail.dime_maillage[0]
        for ii in range(0,Nb_no):
            xx = 0.0; yy = 0.0; zz = 1000000.0
            mail.cn = NP.concatenate((mail.cn,NP.array([[xx,yy,zz]]))) 
            mail.correspondance_noeuds.append('N'+str(nb_noeuds+ii+1))
            mail.dime_maillage[0]+=1
        for ii in range(0,Nb_no):
            mail.co.append(NP.array([nb_noeuds+ii]))             
            gr_ma.append(len(mail.co)-1)                         
            mail.correspondance_mailles.append('PF0%d' % ii)     
            mail.tm.append(mail.dic['POI1'] )                    
            mail.dime_maillage[2]+=1                             
        mail.gma['MFICTIF']=NP.array(gr_ma) 
        unite=mail.ToAster()  
        _mail = LIRE_MAILLAGE(UNITE=unite)
        self.new_mesh = _mail
