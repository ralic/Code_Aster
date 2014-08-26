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
from Utilitai.utils import _debug

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
        _debug(computed, attr)
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
        - They are computed only at the first access (or after being deleted).
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
        self._init_properties()

    def _init_properties(self):
        """Initialize all the cached properties to NULL"""
        self._coeur = NULL
        self._mesh = NULL
        self._model = NULL
        self._geofib = NULL
        self._carael = NULL
        self._cham_mater_contact = NULL
        self._cham_mater_free = NULL
        self._times = NULL
        self._evol_temp = NULL
        self._evol_fluence = NULL
        self._rigid_load = NULL
        self._archimede_load = NULL
        self._gravity_load = NULL
        self._vessel_head_load = NULL
        self._vessel_dilatation_load = NULL
        self._thyc_load = NULL
        self._symetric_cond = NULL
        self._periodic_cond = NULL

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

    @coeur.setter
    def coeur(self, value):
        """Setter method that ensure that the attribute is NULL"""
        assert self._coeur is NULL, 'attribute must be set only once or resetted'
        self._coeur = value

    @coeur.deleter
    def coeur(self):
        """Reset the attribute"""
        self._coeur = NULL

    @property
    @cached_property
    def mesh(self):
        """Return the `maillage_sdaster` object"""
        return self.coeur.affectation_maillage(self.keyw['MAILLAGE_N'])

    @mesh.setter
    def mesh(self, value):
        """Setter method that ensure that the attribute is NULL"""
        assert self._mesh is NULL, 'attribute must be set only once or resetted'
        self._mesh = value

    @mesh.deleter
    def mesh(self):
        """Reset the attribute"""
        self._mesh = NULL

    @property
    @cached_property
    def model(self):
        """Return the `modele_sdaster` object"""
        return self.coeur.affectation_modele(self.mesh)

    @model.setter
    def model(self, value):
        """Setter method that ensure that the attribute is NULL"""
        assert self._model is NULL, 'attribute must be set only once or resetted'
        self._model = value

    @model.deleter
    def model(self):
        """Reset the attribute"""
        self._model = NULL

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
    def cham_mater_free(self):
        """Return the field of material (without contact)"""
        return self.coeur.definition_materiau(
                            self.mesh, self.geofib, self.evol_fluence,
                            self.evol_temp, CONTACT='NON')

    @property
    @cached_property
    def cham_mater_contact(self):
        """Return the field of material (with contact enabled)"""
        return self.coeur.definition_materiau(
                            self.mesh, self.geofib, self.evol_fluence,
                            self.evol_temp, CONTACT='OUI')

    # loadings
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
    def gravity_load(self):
        """Return the gravity loading"""
        return [_F(CHARGE=self.coeur.definition_pesanteur(self.model)), ]

    @property
    @cached_property
    def vessel_head_load(self):
        """Return the loadings due to the pression of
        the vessel head"""
        coeur = self.coeur
        dicv = self.mcf[0].cree_dict_valeurs(self.mcf[0].mc_liste)
        typ = dicv.get('TYPE_MAINTIEN') or 'DEPL_PSC'
        force = None
        if typ == 'FORCE':
            force = self.mcf['FORCE_MAINTIEN']
        char = coeur.definition_maintien_type(self.model, typ, force)
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

    @property
    @cached_property
    def symetric_cond(self):
        """Define the boundary conditions of symetry"""
        from Cata.cata import AFFE_CHAR_MECA
        def block(grma=None, grno=None, ddl=None):
            """Block 'ddl' of 'grma/grno' to zero"""
            kddl = {}.fromkeys(ddl, 0.)
            kddl['GROUP_MA' if grma else 'GROUP_NO'] = grma or grno
            return kddl
        ddl_impo = [
            block(grma='CRAYON', ddl=['DRX']),
            block(grno='LISPG', ddl=['DRX', 'DRY', 'DRZ']),
            block(grma=('EBOSUP', 'EBOINF'), ddl=['DRX', 'DRY', 'DRZ']),
        ]
        _excit = AFFE_CHAR_MECA(MODELE=self.model,
                                DDL_IMPO=ddl_impo)
        return [_F(CHARGE=_excit), ]

    @property
    @cached_property
    def periodic_cond(self):
        """Define the boundary conditions of periodicity"""
        from Cata.cata import AFFE_CHAR_MECA
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
        liaison_group = [equal('DY', 'PMNT_S', 'PEBO_S'),
                         equal('DZ', 'PMNT_S', 'PEBO_S'),
                         equal('DY', 'PSUP', 'PEBO_S'),
                         equal('DZ', 'PSUP', 'PEBO_S'),
                         equal('DY', 'PINF', 'FIX'),
                         equal('DZ', 'PINF', 'FIX'),]
        _excit = AFFE_CHAR_MECA(MODELE=self.model,
                                LIAISON_GROUP=liaison_group)
        return [_F(CHARGE=_excit), ]

    def snl(self, **kwds):
        """Return the common keywords for STAT_NON_LINE
        All keywords can be overridden using `kwds`."""
        keywords = {
            'MODELE' : self.model,
            'CARA_ELEM' : self.carael,
            'CHAM_MATER' : self.cham_mater_free,
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

    def set_from_resu(self, what, resu):
        """Extract a parameter from a result"""
        assert what in ('mesh', 'model')
        key, typ = { 'mesh' : ('NOM_MAILLA', maillage_sdaster),
                     'model' : ('NOM_MODELE', modele_sdaster) }[what]
        nom_co = aster.dismoi(key, resu.nom, 'RESULTAT', 'F')[2].strip()
        return self.macro.get_concept_by_type(nom_co, typ)


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
            mesh = self.set_from_resu('mesh', resu_init)
        else:
            mesh = super(Mac3CoeurDeformation, self).mesh
        return mesh

    @property
    @cached_property
    def model(self):
        """Return the `modele_sdaster` object"""
        resu_init = self.mcf['RESU_INIT']
        if resu_init:
            model = self.set_from_resu('model', resu_init)
        else:
            model = super(Mac3CoeurDeformation, self).model
        return model

    def _run(self):
        """Run the main part of the calculation"""
        from Cata.cata import STAT_NON_LINE
        coeur = self.coeur
        if self.keyw['TYPE_COEUR'] == "MONO":
            chmat_contact = self.cham_mater_free
        else:
            chmat_contact = self.cham_mater_contact
        constant_load = self.rigid_load + self.archimede_load + \
                        self.gravity_load + self.vessel_dilatation_load + \
                        self.symetric_cond + self.periodic_cond
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
                               CHAM_MATER=self.cham_mater_free,
                               ETAT_INIT=_F(EVOL_NOLI=RESULT),
                               EXCIT=constant_load,
                               INCREMENT=_F(LIST_INST=self.times),
                               ))


class Mac3CoeurLame(Mac3CoeurCalcul):
    """Compute the thinkness of water from deformed assemblies"""
    mcfact = 'LAME'

    def _init_properties(self):
        """Initialize all the cached properties to NULL"""
        super(Mac3CoeurLame, self)._init_properties()
        self._layer_load = NULL

    @property
    @cached_property
    def layer_load(self):
        """Return the loading due to the displacements of the water layer"""
        return [_F(CHARGE=self.coeur.affe_char_lame(self.model)), ]

    def update_coeur(self, resu, table):
        """Update the `Coeur` object from the given `Table` and result"""
        self._init_properties()
        self.mesh = self.set_from_resu('mesh', resu)
        self.model = self.set_from_resu('model', resu)
        self.coeur = _build_coeur(self.keyw['TYPE_COEUR'], self.macro, table)
        # initializations
        self.coeur.recuperation_donnees_geom(self.mesh)
        self.times

    def deform_mesh(self, resu):
        """Use the displacement of the result to deform the mesh"""
        from Cata.cata import CREA_CHAMP, MODI_MAILLAGE
        _depl = CREA_CHAMP(OPERATION='EXTR',
                           TYPE_CHAM='NOEU_DEPL_R',
                           NOM_CHAM='DEPL',
                           RESULTAT=resu)
        _debug(_depl, "mesh deformation")
        _mesh = MODI_MAILLAGE(reuse=self.mesh,
                              MAILLAGE=self.mesh,
                              DEFORME=_F(OPTION='TRAN',
                                         DEPL=_depl))
        del self.mesh
        self.mesh = _mesh

    def _prepare_data(self):
        """Prepare the data for the calculation"""
        self.use_archimede = 'OUI'
        super(Mac3CoeurLame, self)._prepare_data()

    def _run(self):
        """Run the main part of the calculation"""
        from Cata.cata import STAT_NON_LINE, PERM_MAC3COEUR
        coeur = self.coeur
        # calcul de deformation d'apres DAMAC / T0 - T1
        _snl_lame = STAT_NON_LINE(**self.snl(
                                  INCREMENT=_F(LIST_INST=self.times,
                                               INST_FIN=coeur.temps_simu['T1']),
                                  EXCIT=self.archimede_load + self.vessel_head_load + \
                                        self.vessel_dilatation_load + self.gravity_load + \
                                        self.layer_load + self.periodic_cond,
                                  ))
        _debug(_snl_lame, "result STAT_NON_LINE 1")
        # updated coeur
        __resuf = PERM_MAC3COEUR(TYPE_COEUR=self.keyw['TYPE_COEUR'],
                                 RESU_N=_snl_lame,
                                 TABLE_N=self.keyw['TABLE_N'],
                                 TABLE_NP1=self.mcf['TABLE_NP1'],
                                 MAILLAGE_NP1=self.mcf['MAILLAGE_NP1'])
        _debug(__resuf, "result PERM_MAC3COEUR")
        self.update_coeur(__resuf, self.mcf['TABLE_NP1'])
        # WARNING: element characteristics and the most of the loadings must be
        # computed on the initial (not deformed) mesh
        keywords = self.snl(CHAM_MATER=self.cham_mater_contact,
                            INCREMENT=_F(LIST_INST=self.times,
                                         INST_FIN=coeur.temps_simu['T4']),
                            EXCIT=self.rigid_load + self.archimede_load + \
                                  self.vessel_head_load + \
                                  self.vessel_dilatation_load + \
                                  self.gravity_load + \
                                  self.symetric_cond + self.periodic_cond + \
                                  self.thyc_load,
                            )
        self.deform_mesh(__resuf)
        RESULT = STAT_NON_LINE(**keywords)
        _debug(RESULT, "result STAT_NON_LINE 2")


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
