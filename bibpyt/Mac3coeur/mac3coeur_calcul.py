# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: samuel.geniaut at edf.fr

"""
This module defines the different types of calculations
"""

from functools import wraps

import aster_core
import aster

from Cata.cata import modele_sdaster, maillage_sdaster
from Accas import _F

from Utilitai.UniteAster import UniteAster
from Utilitai.Utmess import UTMESS

from mac3coeur_coeur import CoeurFactory
from thyc_result import lire_resu_thyc


def calc_mac3coeur_ops(self, **args):
    """Fonction d'appel de la macro CALC_MAC3COEUR"""
    self.set_icmd(1)
    analysis = Mac3CoeurCalcul.factory(self, args)
    analysis.run()

# decorator to cache values of properties
NULL = object()

def cached_property(method):
    """Decorator for the 'getter' method of a property
    It returns directly the value without calling the 'getter' method itself if
    it has already been computed (== not NULL).
    The value is cached in the attribute "_ + `method name`" of the instance.
    """
    @wraps(method)
    def wrapper(inst):
        """Real wrapper function"""
        attr = '_' +  method.__name__
        cached = getattr(inst, attr)
        if cached is not NULL:
            return cached
        computed = method(inst)
        setattr(inst, attr, computed)
        return computed
    return wrapper


class Mac3CoeurCalcul(object):
    """Base class of an analysis, intended to be inherited

    Its factory builds the proper object according to the passed keywords.
    Then, a calculation is just completed using::

        calc = Mac3CoeurCalcul.factory(...)
        calc.run()

    Inherited classes may have to adjust `_prepare_data()` and `_run()` methods.
    There are a lot of cached properties:
        - They should store only some data required by the calculation.
        - They are computed only at the first access (or after a reset,
          see `_reset()` method).
        - The cache mecanism allows to build Code_Aster objects or to run long
          operations only if necessary.
        - Prefer use standard properties for scalar values.
    """
    mcfact = None

    @staticmethod
    def factory(macro, args):
        """Factory that returns the calculation object"""
        class_ = None
        if args['DEFORMATION']:
            class_ = Mac3CoeurDeformation
        if args['LAME']:
            class_ = Mac3CoeurLame
        if not class_:
            UTMESS('F', 'DVP_1')
        return class_(macro, args)

    def __init__(self, macro, args):
        """Initialization"""
        self.macro = macro
        self.keyw = args
        self.mcf = args[self.mcfact]
        # parameters
        self._niv_fluence = 0.
        self._subdivis = 1
        self._use_archimede = None
        # cached properties
        self._coeur = NULL
        self._mesh = NULL
        self._model = NULL
        self._geofib = NULL
        self._carael = NULL
        self._cham_mater = NULL
        self._times = NULL
        self._evol_temp = NULL
        self._evol_fluence = NULL
        self._rigid_load = NULL
        self._archimede_load = NULL
        self._gravity_load = NULL
        self._periodic_load = NULL
        self._vessel_head_load = NULL
        self._vessel_dilatation_load = NULL
        self._thyc_load = NULL

    def _prepare_data(self):
        """Prepare the data for the calculation"""
        self.macro.DeclareOut('RESULT', self.macro.sd)
        coeur = self.coeur
        coeur.recuperation_donnees_geom(self.mesh)
        # force the computation of the times to ensure it is done first
        # Note that times depends on niv_fluence and subdivis.
        self.times

    def _run(self):
        """Run the calculation itself"""
        raise NotImplementedError('must be defined in a subclass')

    def _build_result(self):
        """Build the results"""

    def run(self):
        """Run all the calculation steps"""
        self._prepare_data()
        self._run()
        self._build_result()

    def _reset(self, propname):
        """Reset a property to force recomputing at the next use"""
        setattr(self, '_' + propname, NULL)

    @property
    def niv_fluence(self):
        """Return the fluence level"""
        return self._niv_fluence

    @niv_fluence.setter
    def niv_fluence(self, value):
        """Set the value of the fluence level"""
        self._niv_fluence = value

    @property
    def subdivis(self):
        """Return the factor of time splitting"""
        return self._subdivis

    @subdivis.setter
    def subdivis(self, value):
        """Set the value of the time splitting"""
        self._subdivis = value

    @property
    def use_archimede(self):
        """Tell if Archimede loadings are enabled or not ('OUI'/'NON')"""
        return self._use_archimede

    @use_archimede.setter
    def use_archimede(self, value):
        """Set the value of the time splitting"""
        self._use_archimede = value

    # cached properties
    @property
    @cached_property
    def coeur(self):
        """Return the `Coeur` object"""
        return _build_coeur(self.keyw['TYPE_COEUR'], self.macro,
                            self.keyw['TABLE_N'])

    @property
    @cached_property
    def mesh(self):
        """Return the `maillage_sdaster` object"""
        return self.coeur.affectation_maillage(self.keyw['MAILLAGE_N'])

    @property
    @cached_property
    def model(self):
        """Return the `modele_sdaster` object"""
        return self.coeur.affectation_modele(self.mesh)

    @property
    @cached_property
    def geofib(self):
        """Return the `geom_fibre` object"""
        return self.coeur.definition_geom_fibre()

    @property
    @cached_property
    def carael(self):
        """Return the `cara_elem` object"""
        return self.coeur.definition_cara_coeur(self.model, self.geofib)

    @property
    @cached_property
    def times(self):
        """Return the list of the time steps"""
        return self.coeur.definition_time(self.niv_fluence, self.subdivis)

    @property
    @cached_property
    def evol_temp(self):
        """Return the evolution of temperature"""
        return self.coeur.definition_champ_temperature(self.mesh)

    @property
    @cached_property
    def evol_fluence(self):
        """Return the evolution of the fluence fields"""
        return self.coeur.definition_fluence(self.niv_fluence, self.mesh)

    @property
    @cached_property
    def cham_mater(self):
        """Return the field of material (without contact)"""
        return self.coeur.definition_materiau(
                            self.mesh, self.geofib, self.evol_fluence,
                            self.evol_temp, CONTACT='NON')

    def snl(self, **kwds):
        """Return the common keywords for STAT_NON_LINE
        All keywords can be overridden using `kwds`."""
        keywords = {
            'MODELE' : self.model,
            'CARA_ELEM' : self.carael,
            'CHAM_MATER' : self.cham_mater,
            'COMPORTEMENT' : (_F(RELATION='MULTIFIBRE',
                                 GROUP_MA=('CRAYON', 'T_GUIDE'),
                                 PARM_THETA=0.5,
                                 DEFORMATION='GROT_GDEP',),
                              _F(RELATION='DIS_GRICRA',
                                 GROUP_MA='ELA',),
                              _F(RELATION='DIS_CHOC',
                                 GROUP_MA='RES_TOT',),
                              _F(RELATION='ELAS',
                                 GROUP_MA=('EBOINF', 'EBOSUP', 'RIG', 'DIL')),
                              _F(RELATION='VMIS_ISOT_TRAC',
                                 GROUP_MA='MAINTIEN',
                                 DEFORMATION='PETIT'),),
            'NEWTON' : _F(MATRICE='TANGENTE',
                          REAC_ITER=1,),
            'SOLVEUR' : _F(METHODE='MUMPS',
                           RENUM='AMF',
                           GESTION_MEMOIRE='OUT_OF_CORE',
                           ELIM_LAGR='NON',
                           PCENT_PIVOT=200,),
            'AFFICHAGE' : _F(INFO_RESIDU='OUI'),
        }
        keywords.update(kwds)
        return keywords

    @property
    @cached_property
    def rigid_load(self):
        """Compute the rigid body loading"""
        from Cata.cata import AFFE_CHAR_MECA
        coeur = self.coeur
        _excit_rigid = AFFE_CHAR_MECA(MODELE=self.model,
                                      LIAISON_SOLIDE=coeur.cl_rigidite_grille())
        return [_F(CHARGE=_excit_rigid), ]

    @property
    @cached_property
    def archimede_load(self):
        """Compute the Archimede loadings"""
        fmult_arch = self.coeur.definition_temp_archimede(self.use_archimede)
        load = [
            _F(CHARGE    = self.coeur.definition_archimede_nodal(self.model),
               FONC_MULT = fmult_arch,),
            _F(CHARGE    = self.coeur.definition_archimede_poutre(self.model),
               FONC_MULT = fmult_arch,), ]
        return load

    @property
    @cached_property
    def periodic_load(self):
        """Compute the periodic boundary conditions"""
        return [_F(CHARGE=self.periodic_cond()), ]

    @property
    @cached_property
    def gravity_load(self):
        """Return the gravity loading"""
        return [_F(CHARGE=self.coeur.definition_pesanteur(self.model)), ]

    @property
    @cached_property
    def vessel_head_load(self):
        """Return the loadings due to the pression of
        the vessel head"""
        coeur = self.coeur
        force = None
        if self.mcf['TYPE_MAINTIEN'] == 'FORCE':
            force = self.mcf['FORCE_MAINTIEN']
        char = coeur.definition_maintien_type(self.model,
                                              self.mcf['TYPE_MAINTIEN'], force)
        return [_F(CHARGE=char), ]

    @property
    @cached_property
    def vessel_dilatation_load(self):
        """Return the loading due to the vessel dilatation"""
        char_dilat = self.coeur.dilatation_cuve(self.model, self.mesh)
        return [_F(CHARGE=char_dilat,), ]

    @property
    @cached_property
    def thyc_load(self):
        """Return the loading due to the fluid flow"""
        coeur = self.coeur
        thyc = read_thyc(coeur, self.model, self.mcf['UNITE_THYC'])
        fmult_ax = coeur.definition_temp_hydro_axiale()
        fmult_tr = coeur.definition_effort_transverse()
        load = [
            _F(CHARGE=thyc.chax_nodal, FONC_MULT=fmult_ax,),
            _F(CHARGE=thyc.chax_poutre, FONC_MULT=fmult_ax,),
            _F(CHARGE=thyc.chtr_nodal, FONC_MULT=fmult_tr,),
            _F(CHARGE=thyc.chtr_poutre, FONC_MULT=fmult_tr,),
        ]
        return load

    def periodic_cond(self):
        """Define the boundary conditions of periodicity"""
        from Cata.cata import AFFE_CHAR_MECA
        # convenient functions to avoid repetitions
        def block(grma=None, grno=None, ddl=None):
            """Block 'ddl' of 'grma/grno' to zero"""
            kddl = {}.fromkeys(ddl, 0.)
            kddl['GROUP_MA' if grma else 'GROUP_NO'] = grma or grno
            return kddl
        def equal(ddl, grno1, grno2):
            """Return keyword to set ddl(grno1) = ddl(grno2)"""
            return _F(GROUP_NO_1=grno1,
                      GROUP_NO_2=grno2,
                      SOMMET='OUI',
                      DDL_1=ddl,
                      DDL_2=ddl,
                      COEF_MULT_1=1.,
                      COEF_MULT_2=-1.,
                      COEF_IMPO=0.)
        # definitions
        ddl_impo = [
            block(grma='CRAYON', ddl=['DRX']),
            block(grno='LISPG', ddl=['DRX', 'DRY', 'DRZ']),
            block(grma=('EBOSUP', 'EBOINF'), ddl=['DRX', 'DRY', 'DRZ']),
        ]
        liaison_group = [equal('DY', 'PMNT_S', 'PEBO_S'),
                         equal('DZ', 'PMNT_S', 'PEBO_S'),
                         equal('DY', 'PSUP', 'PEBO_S'),
                         equal('DZ', 'PSUP', 'PEBO_S'),
                         equal('DY', 'PINF', 'FIX'),
                         equal('DZ', 'PINF', 'FIX'),]
        _excit = AFFE_CHAR_MECA(MODELE=self.model,
                                DDL_IMPO=ddl_impo,
                                LIAISON_GROUP=liaison_group)
        return _excit


class Mac3CoeurDeformation(Mac3CoeurCalcul):
    """Compute the strain of the assemblies"""
    mcfact = 'DEFORMATION'

    def __init__(self, macro, args):
        """Initialization"""
        super(Mac3CoeurDeformation, self).__init__(macro, args)
        self.etat_init = None

    def _prepare_data(self):
        """Prepare the data for the calculation"""
        self.niv_fluence = self.mcf['NIVE_FLUENCE']
        if self.keyw['TYPE_COEUR'] == "MONO":
            self.subdivis = 5
        self.use_archimede = self.mcf['ARCHIMEDE']
        super(Mac3CoeurDeformation, self)._prepare_data()

    @property
    @cached_property
    def mesh(self):
        """Return the `maillage_sdaster` object"""
        mesh = self.keyw['MAILLAGE_N']
        resu_init = self.mcf['RESU_INIT']
        if not (mesh or resu_init):
            UTMESS('F', 'COEUR0_7')
        elif resu_init:
            if mesh:
                UTMESS('A', 'COEUR0_1')
            self.etat_init = _F(EVOL_NOLI=resu_init)
            nom_ma = aster.dismoi('NOM_MAILLA', resu_init.nom,
                                  'RESULTAT', 'F')[2]
            mesh = self.macro.get_concept_by_type(nom_ma, maillage_sdaster)
        else:
            mesh = super(Mac3CoeurDeformation, self).mesh
        return mesh

    @property
    @cached_property
    def model(self):
        """Return the `modele_sdaster` object"""
        resu_init = self.mcf['RESU_INIT']
        if resu_init:
            nom_mo = aster.dismoi('NOM_MODELE', resu_init.nom,
                                  'RESULTAT', 'F')[2]
            model = self.macro.get_concept_by_type(nom_mo, modele_sdaster)
        else:
            model = super(Mac3CoeurDeformation, self).model
        return model

    def _run(self):
        """Run the main part of the calculation"""
        from Cata.cata import STAT_NON_LINE
        coeur = self.coeur
        if self.keyw['TYPE_COEUR'] == "MONO":
            chmat_contact = self.cham_mater
        else:
            chmat_contact = coeur.definition_materiau(
                            self.mesh, self.geofib, self.evol_fluence,
                            self.evol_temp, CONTACT='OUI')
        constant_load = self.rigid_load + self.archimede_load + \
                        self.periodic_load + self.gravity_load + \
                        self.vessel_dilatation_load
        # T0 - T8
        RESULT = STAT_NON_LINE(**self.snl(
                               CHAM_MATER=chmat_contact,
                               INCREMENT=_F(LIST_INST=self.times,
                                            INST_FIN=coeur.temps_simu['T8']),
                               EXCIT=constant_load + self.vessel_head_load + \
                                     self.thyc_load,
                               ETAT_INIT=self.etat_init,
                               ))
        # T8 - T8b
        RESULT = STAT_NON_LINE(**self.snl(
                               reuse=RESULT,
                               CHAM_MATER=chmat_contact,
                               ETAT_INIT=_F(EVOL_NOLI=RESULT),
                               EXCIT=constant_load + self.vessel_head_load,
                               INCREMENT=_F(LIST_INST=self.times,
                                            INST_FIN=coeur.temps_simu['T8b']),
                               ))
        # T8b - Tf
        RESULT = STAT_NON_LINE(**self.snl(
                               reuse=RESULT,
                               CHAM_MATER=self.cham_mater,
                               ETAT_INIT=_F(EVOL_NOLI=RESULT),
                               EXCIT=constant_load,
                               INCREMENT=_F(LIST_INST=self.times),
                               ))


class Mac3CoeurLame(Mac3CoeurCalcul):
    """Compute the thinkness of water from deformed assemblies"""
    mcfact = 'LAME'

    def _prepare_data(self):
        """Prepare the data for the calculation"""
        self.use_archimede = 'OUI'
        super(Mac3CoeurLame, self)._prepare_data()

    def _run(self):
        """Run the main part of the calculation"""
        from Cata.cata import STAT_NON_LINE, PERM_MAC3COEUR
        coeur = self.coeur

        _DILAT   = coeur.dilatation_cuve(_MO_N,_MA_N)
        fmult_arch = coeur.definition_temp_archimede(use_archimede)
        char_arch_nod  = coeur.definition_archimede_nodal(_MO_N)
        char_arch_pout= coeur.definition_archimede_poutre(_MO_N)
        char_pesant  = coeur.definition_pesanteur(_MO_N)
        eff_maintien  = coeur.definition_effor_maintien(_MO_N)

        _CL_LAME = coeur.affe_char_lame(_MO_N)

        _CL_PER_2  = AFFE_CHAR_MECA( MODELE   = _MO_N,
                                  LIAISON_GROUP = (_F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                      DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                   _F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                      DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),

                                                   _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                      DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                   _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                      DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),

                                                   _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                      DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                   _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                      DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  ),
                                 );

        # calcul de deformation d'apres DAMAC
        _SNL_LAME = STAT_NON_LINE( MODELE  = _MO_N,
                               CHAM_MATER  = self.cham_mater,
                               CARA_ELEM   = _CARA,
                               EXCIT       = (
                                     _F(CHARGE = char_arch_nod,   FONC_MULT = fmult_arch,),
                                     _F(CHARGE = char_arch_pout, FONC_MULT = fmult_arch,),
                                     _F(CHARGE = eff_maintien,  ),
                                     _F(CHARGE = _DILAT,   ),
                                     _F(CHARGE = char_pesant,  ),
                     _F(CHARGE = _CL_LAME, ),
                     _F(CHARGE = _CL_PER_2,),),
                               COMPORTEMENT   =(
                                     _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, DEFORMATION = 'GROT_GDEP', ),
                                     _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                     _F(RELATION='DIS_CHOC',   GROUP_MA ='RES_TOT',),
                                     _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),
                                     _F(RELATION='VMIS_ISOT_TRAC',GROUP_MA ='MAINTIEN',DEFORMATION='PETIT',),),
                               INCREMENT   = _F(LIST_INST = self.times, INST_FIN = coeur.temps_simu['T1'],),
                               NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                               SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',GESTION_MEMOIRE='OUT_OF_CORE',ELIM_LAGR='NON',PCENT_PIVOT=80,),
                               )

        _TAB_NP1   = self.mcf['TABLE_NP1']
        _tabp1     = _TAB_NP1.EXTR_TABLE()

        # on recupere le nom du coeur
        namep1 = _tabp1.para[0]

        # et on renomme la colonne qui identifie les assemblages
        _tabp1.Renomme(namep1, 'idAC')
        _coeurp1 = coeur_factory.get(_typ_coeur)(namep1, _typ_coeur, self, datg)
        _coeurp1.init_from_table(_tabp1)

        _MA1     = self.mcf['MAILLAGE_NP1']

        __resuf   = PERM_MAC3COEUR( TYPE_COEUR   = _typ_coeur,
                                   RESU_N       = _SNL_LAME,
                                   TABLE_N      = _TAB_N,
                                   TABLE_NP1    = _TAB_NP1,
                                   MAILLAGE_NP1 = _MA1,)

        _MVDEPL = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM ='DEPL', RESULTAT = __resuf)

        iret,ibid,nom_ma = aster.dismoi('NOM_MAILLA',__resuf.nom,'RESULTAT','F')
        nom_ma = nom_ma.strip()
        iret,ibid,nom_mo = aster.dismoi('NOM_MODELE',__resuf.nom,'RESULTAT','F')
        nom_mo = nom_mo.strip()

        _MO_NP1 = self.get_concept_by_type(nom_mo, modele_sdaster)
        _MA_NP1 = self.get_concept_by_type(nom_ma, maillage_sdaster)

        _coeurp1.recuperation_donnees_geom(_MA_NP1)
        _GFF_NP1  = _coeurp1.definition_geom_fibre()
        _CARANP1  = _coeurp1.definition_cara_coeur(_MO_NP1,_GFF_NP1)

        _timep1   = _coeurp1.definition_time(self.niv_fluence,1.)
        _FLU_NP1  = _coeurp1.definition_fluence(self.niv_fluence,_MA_NP1)
        _CHTHNP1  = _coeurp1.definition_champ_temperature(_MA_NP1)
        char_dilat1  = _coeurp1.dilatation_cuve(_MO_NP1,_MA_NP1)
        _AFACNP1  = _coeurp1.definition_materiau(_MA_NP1,_GFF_NP1,_FLU_NP1,_CHTHNP1, CONTACT='OUI')
        _AFSCNP1  = _coeurp1.definition_materiau(_MA_NP1,_GFF_NP1,_FLU_NP1,_CHTHNP1, CONTACT='NON')

        char_pesant1  = _coeurp1.definition_pesanteur(_MO_NP1)
        eff_maintien1  = _coeurp1.definition_effor_maintien(_MO_NP1)
        char_arch_nod1  = _coeurp1.definition_archimede_nodal(_MO_NP1)
        _FOARCH1  = _coeurp1.definition_archimede_poutre(_MO_NP1)
        _ARCHF11  = _coeurp1.definition_temp_archimede(use_archimede)

        # on rajoute les efforts thyc
        UL = UniteAster()
        _unit_eftx = self.mcf['UNITE_THYC']

        # si le MC est facultatif, il faudra verifier s'il est renseigner ou pas
        # _unit_eftx vaut alors None

        nomfich=UL.Nom(_unit_eftx)
        thyc = lire_resu_thyc(_coeurp1, _MO_NP1, nomfich)
        fmult_ax = _coeurp1.definition_temp_hydro_axiale()
        fmult_tr = _coeurp1.definition_effort_transverse()

        _MA_NP1 = MODI_MAILLAGE( reuse = _MA_NP1, MAILLAGE = _MA_NP1, DEFORME = _F( OPTION = 'TRAN', DEPL = _MVDEPL))

        cl_liaison_solide = _coeurp1.cl_rigidite_grille()

        _BLOC2  = AFFE_CHAR_MECA( MODELE   = _MO_NP1,
                                  DDL_IMPO = ( _F(GROUP_MA = 'CRAYON',           DRX=0.,               ),
                                               _F(GROUP_NO = 'LISPG',            DRX=0., DRY=0., DRZ=0.),
                                               _F(GROUP_MA =('EBOSUP','EBOINF'), DRX=0., DRY=0., DRZ=0.),),
                                  LIAISON_GROUP = (_F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                      DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                   _F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                      DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),

                                                   _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                      DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                   _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                      DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),

                                                   _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                      DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                   _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                      DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  ),
                                  LIAISON_SOLIDE = cl_liaison_solide,
                                 );

        RESULT = STAT_NON_LINE( MODELE      = _MO_NP1,
                               CHAM_MATER  = _AFACNP1,
                               CARA_ELEM   = _CARANP1,
                               EXCIT   =(
                                     _F(CHARGE = char_arch_nod1,  FONC_MULT = _ARCHF11,),
                                     _F(CHARGE = _FOARCH1,  FONC_MULT = _ARCHF11,),
                                     _F(CHARGE = eff_maintien1,  ),
                                     _F(CHARGE = char_pesant1,  ),
                                     _F(CHARGE = char_dilat1,  ),
                                     _F(CHARGE = _BLOC2,),
                                     _F(CHARGE = thyc.chtr_nodal,  FONC_MULT = fmult_tr,),
                                     _F(CHARGE = thyc.chtr_poutre,  FONC_MULT = fmult_tr,),
                                     _F(CHARGE = thyc.chax_nodal,   FONC_MULT = fmult_ax,),
                                     _F(CHARGE = thyc.chax_poutre, FONC_MULT = fmult_ax,),
                                        ),
                               COMPORTEMENT   =(
                                     _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, DEFORMATION = 'GROT_GDEP', ),
                                     _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                     _F(RELATION='DIS_CHOC',   GROUP_MA ='RES_TOT',),
                                     _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),
                                     _F(RELATION='VMIS_ISOT_TRAC',GROUP_MA ='MAINTIEN',DEFORMATION='PETIT',),),
                               INCREMENT   = _F(LIST_INST = _timep1, INST_FIN = _coeurp1.temps_simu['T4'],),
                               NEWTON      = _F(MATRICE='TANGENTE',REAC_ITER=1,),
                               SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',GESTION_MEMOIRE='OUT_OF_CORE',ELIM_LAGR='NON',PCENT_PIVOT=200,),
                               );

# helper functions
def _build_coeur(typ_coeur, macro, sdtab):
    """Return a `Coeur` object of the given type"""
    datg = aster_core.get_option("repdex")
    factory = CoeurFactory(datg)
    # prepare the Table object
    tab = sdtab.EXTR_TABLE()
    name = tab.para[0]
    tab.Renomme(name, 'idAC')
    coeur = factory.get(typ_coeur)(name, typ_coeur, macro, datg)
    coeur.init_from_table(tab)
    return coeur

def read_thyc(coeur, model, unit):
    """Read a file containing THYC results"""
    res = None
    try:
        UL = UniteAster()
        fname = UL.Nom(unit)
        res = lire_resu_thyc(coeur, model, fname)
    finally:
        UL.EtatInit()
    return res
