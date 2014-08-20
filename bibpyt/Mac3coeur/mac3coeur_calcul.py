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

import aster_core
import aster

from Cata.cata import modele_sdaster, maillage_sdaster
from Accas import _F

from Utilitai.UniteAster import UniteAster
from Utilitai.Utmess import UTMESS

from mac3coeur_coeur import CoeurFactory
from thyc_result import lire_resu_thyc

# decorator to cache values of properties
NULL = object()

def cached_property(attr):
    """Decorator for the 'getter' method of a property
    It returns directly the value without calling the 'getter' method itself if
    it has already been computed (== not NULL).
    :param attr: attribute name containing the property value
    :type attr: string
    """
    def wrap(method):
        """Wrapping method"""
        def wrapper(inst):
            """Real wrapper function"""
            cached = getattr(inst, attr)
            if cached is not NULL:
                return cached
            computed = method(inst)
            setattr(inst, attr, computed)
            return computed
        wrapper.__doc__ = method.__doc__ + " (cached property)"
        return wrapper
    return wrap


class Mac3CoeurCalcul(object):
    """Base class of an analysis
    Must be derivated."""
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
        # cached properties
        self._coeur = NULL
        self._mesh = NULL
        self._model = NULL
        self._carael = NULL
        self._geofib = NULL

    def _prepare_data(self):
        """Prepare the data for the calculation"""
        self.macro.DeclareOut('RESULT', self.macro.sd)

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
    @cached_property('_coeur')
    def coeur(self):
        """Compute and return the `Coeur` object"""
        return _build_coeur(self.keyw['TYPE_COEUR'], self.macro,
                            self.keyw['TABLE_N'])

    @property
    @cached_property('_mesh')
    def mesh(self):
        """Compute and return the `maillage_sdaster` object"""
        return self.coeur.affectation_maillage(self.keyw['MAILLAGE_N'])

    @property
    @cached_property('_model')
    def model(self):
        """Compute and return the `modele_sdaster` object"""
        return self.coeur.affectation_modele(self.mesh)

    @property
    @cached_property('_geofib')
    def geofib(self):
        """Compute and return the `geom_fibre` object"""
        return self.coeur.definition_geom_fibre()

    @property
    @cached_property('_carael')
    def carael(self):
        """Compute and return the `cara_elem` object"""
        return self.coeur.definition_cara_coeur(self.model, self.geofib)

    def snl_keywords(self, **kwds):
        """Return the common keywords for STAT_NON_LINE
        All keywords can be overridden using `kwds`."""
        keywords = {
            'MODELE' : self.model,
            'CARA_ELEM' : self.carael,
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


class Mac3CoeurDeformation(Mac3CoeurCalcul):
    """Compute the strain of the assemblies"""
    mcfact = 'DEFORMATION'

    def __init__(self, macro, args):
        """Initialization"""
        super(Mac3CoeurDeformation, self).__init__(macro, args)
        self.etat_init = None
        # cached properties
        self._constant_load = NULL
        self._vessel_head_load = NULL
        self._vessel_dilatation_load = NULL
        self._thyc_load = NULL

    def _prepare_data(self):
        """Prepare the data for the calculation"""
        super(Mac3CoeurDeformation, self)._prepare_data()

    @property
    @cached_property('_mesh')
    def mesh(self):
        """Compute and return the `maillage_sdaster` object"""
        mesh = super(Mac3CoeurDeformation, self).mesh
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
        return mesh

    @property
    @cached_property('_model')
    def model(self):
        """Compute and return the `modele_sdaster` object"""
        model = super(Mac3CoeurDeformation, self).model
        resu_init = self.mcf['RESU_INIT']
        if resu_init:
            nom_mo = aster.dismoi('NOM_MODELE', resu_init.nom,
                                  'RESULTAT', 'F')[2]
            model = self.macro.get_concept_by_type(nom_mo, modele_sdaster)
        return model

    def _run(self):
        """Run the main part of the calculation"""
        from Cata.cata import STAT_NON_LINE, AFFE_CHAR_MECA
        coeur = self.coeur
        coeur.recuperation_donnees_geom(self.mesh)

        niv_fluence = self.mcf['NIVE_FLUENCE']
        contact = 'OUI'
        subdivis = 1
        if self.keyw['TYPE_COEUR'] == "MONO":
            contact = 'NON'
            subdivis = 5
        times = coeur.definition_time(niv_fluence, subdivis)
        evol_fluence = coeur.definition_fluence(niv_fluence, self.mesh)
        chtemp = coeur.definition_champ_temperature(self.mesh)
        chmat_contact = coeur.definition_materiau(self.mesh, self.geofib, evol_fluence,
                                                  chtemp, CONTACT=contact)
        chmat_libre = coeur.definition_materiau(self.mesh, self.geofib, evol_fluence,
                                                chtemp, CONTACT='NON')
        # T0 - T8
        snl_keywords = self.snl_keywords()
        RESULT = STAT_NON_LINE(CHAM_MATER=chmat_contact,
                               INCREMENT=_F(LIST_INST=times,
                                            INST_FIN=coeur.temps_simu['T8']),
                               EXCIT=self.constant_load + self.vessel_head_load + \
                                     self.thyc_load + self.vessel_dilatation_load,
                               ETAT_INIT=self.etat_init,
                               **snl_keywords)
        # T8 - T8b
        RESULT = STAT_NON_LINE(reuse=RESULT,
                               CHAM_MATER=chmat_contact,
                               ETAT_INIT=_F(EVOL_NOLI=RESULT),
                               EXCIT=self.constant_load + self.vessel_head_load + \
                                     self.vessel_dilatation_load,
                               INCREMENT=_F(LIST_INST=times,
                                            INST_FIN=coeur.temps_simu['T8b']),
                               **snl_keywords)
        # T8b - Tf
        RESULT = STAT_NON_LINE(reuse=RESULT,
                               CHAM_MATER=chmat_libre,
                               ETAT_INIT=_F(EVOL_NOLI=RESULT),
                               EXCIT=self.constant_load + self.vessel_dilatation_load,
                               INCREMENT=_F(LIST_INST=times),
                               **snl_keywords)

    @property
    @cached_property('_constant_load')
    def constant_load(self):
        """Compute and return the "constant" loadings (always applied)"""
        from Cata.cata import AFFE_CHAR_MECA
        coeur = self.coeur
        _excit_rigid = AFFE_CHAR_MECA(MODELE=self.model,
                                      LIAISON_SOLIDE=coeur.cl_rigidite_grille())
        fmult_arch = coeur.definition_temp_archimede(self.mcf['ARCHIMEDE'])
        load = [
            _F(CHARGE    = coeur.definition_archimede_nodal(self.model),
               FONC_MULT = fmult_arch,),
            _F(CHARGE    = coeur.definition_archimede_poutre(self.model),
               FONC_MULT = fmult_arch,),
            _F(CHARGE    = _excit_rigid,),
            _F(CHARGE    = self.periodic_cond(),),
            _F(CHARGE    = coeur.definition_pesanteur(self.model),)
        ]
        return load

    @property
    @cached_property('_vessel_head_load')
    def vessel_head_load(self):
        """Compute and return the loadings due to the pression of
        the vessel head"""
        coeur = self.coeur
        force = None
        if self.mcf['TYPE_MAINTIEN'] == 'FORCE':
            force = self.mcf['FORCE_MAINTIEN']
        char = coeur.definition_maintien_type(self.model,
                                              self.mcf['TYPE_MAINTIEN'], force)
        return [_F(CHARGE=char), ]

    @property
    @cached_property('_vessel_dilatation_load')
    def vessel_dilatation_load(self):
        """Compute and return the loading due to the vessel dilatation"""
        char_dilat = self.coeur.dilatation_cuve(self.model, self.mesh)
        return [_F(CHARGE=char_dilat,), ]

    @property
    @cached_property('_thyc_load')
    def thyc_load(self):
        """Compute and return the loading due to the fluid flow"""
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


class Mac3CoeurLame(Mac3CoeurCalcul):
    """Compute the thinkness of water from deformed assemblies"""
    mcfact = 'LAME'

    def _run(self):
        """Run the main part of the calculation"""
        STAT_NON_LINE    = self.get_cmd('STAT_NON_LINE')
        MODI_MAILLAGE    = self.get_cmd('MODI_MAILLAGE')
        AFFE_CHAR_MECA   = self.get_cmd('AFFE_CHAR_MECA')
        CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
        PERM_MAC3COEUR   = self.get_cmd('PERM_MAC3COEUR')

        coeur = self.coeur

        niv_fluence = 0.0

        use_archimede='OUI'

        _MA_N = coeur.affectation_maillage(_MA0)
        _MO_N = coeur.affectation_modele(_MA_N)
        coeur.recuperation_donnees_geom(_MA_N)
        _GFF  = coeur.definition_geom_fibre()
        _CARA = coeur.definition_cara_coeur(_MO_N,_GFF)
        times    = coeur.definition_time(niv_fluence,1.)
        evol_fluence  = coeur.definition_fluence(niv_fluence,_MA_N)
        _CHTH    = coeur.definition_champ_temperature(_MA_N)
        _DILAT   = coeur.dilatation_cuve(_MO_N,_MA_N)
        chmat_libre  = coeur.definition_materiau(_MA_N,_GFF,evol_fluence,_CHTH, CONTACT='NON')
        char_pesant  = coeur.definition_pesanteur(_MO_N)
        eff_maintien  = coeur.definition_effor_maintien(_MO_N)
        char_arch_nod  = coeur.definition_archimede_nodal(_MO_N)
        char_arch_pout= coeur.definition_archimede_poutre(_MO_N)
        fmult_arch = coeur.definition_temp_archimede(use_archimede)

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
                               CHAM_MATER  = chmat_libre,
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
                               INCREMENT   = _F(LIST_INST = times, INST_FIN = coeur.temps_simu['T1'],),
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

        _timep1   = _coeurp1.definition_time(niv_fluence,1.)
        _FLU_NP1  = _coeurp1.definition_fluence(niv_fluence,_MA_NP1)
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
