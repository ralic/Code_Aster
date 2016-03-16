# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
Module dédié à la macro MAC3COEUR.

Par simplicité (pour la diffusion), les modules définissant les propriétés
des assemblages sont placés dans datg (avec un suffixe '.datg').
C'est pour cette raison qu'on utilise un objet ACFactory qui importe
les modules "catalogues d'assemblages" et retourne les objets associés.

On définit ici la classe principale Assemblage et ses dérivées pour
certains types d'assemblages.
"""

from math import pi

from mac3coeur_factory import Mac3Factory


class Assemblage(object):

    """Classe définissant un assemblage combustible."""
    typeAC = None
    required_parameters = [
        # Caractéristiques matériau
        # Rigidité  en rotation des liaisons grille-tube guide
        'KR_GM', 'KR_GE',
        # Rigidité caractéristique des grilles de mélanges
        'KNAXM', 'KY_CM', 'KM1', 'KM2',
        # Rigidité caractéristique des grilles extremites
        'KNAXE', 'KY_CE', 'KE1', 'KE2',
        # Caractéristiques géométriques
        # des grilles
        'altitude', 'epaisseur', 'longueur',
        # des tubes-guides
        'NBTG', 'RAY1GU', 'EP1GU', 'RAY2GU', 'EP2GU', 'EPMOY',
        'LONTU', 'XINFT', 'XSUPT',
        # des crayons
        'NBCR', 'RAYCRA', 'EPCRA', 'LONCR', 'XINFC', 'XSUPC',
        # des grilles
        'NBGR', 'm_gri',
        # des embouts
        'EEI0', 'EES0',
        'Leinf', 'Keinf', 'Lesup', 'Kesup', 'Heinf', 'Hesup',
        # des tubes-guides
        'S_TG_C', 'I_TG_C', 'S_TG_R', 'I_TG_R', 'S_TG_B', 'I_TG_B',
        # des crayons
        'S_CR', 'I_CR',
        # Perte de charges
        'K_EBSU', 'K_GRE', 'K_GRM', 'K_TUB', 'K_EBIN',
        # Poussee d Archimede de chaque élément
        'AFEBSU_1', 'AFGRE_1', 'AFGRM_1', 'AFTG_1', 'AFCRA_1', 'AFEBIN_1',
    ]
    optionnal_parameters = []

    def __init__(self, typ_coeur):
        """Initialisation d'un type d'assemblage."""
        self._posdam = None
        self._posast = None
        self.typ_coeur = typ_coeur
        self._cycle = None
        self._checked = False
        self.mate = None
        self.deforme = None
        self._para = {}
        self._keys = {}.fromkeys(self.required_parameters +
                                 self.optionnal_parameters, True)
        self._init_from_attrs()
        self._position_toaster = None
        self._position_todamac = None

    def __get_posast(self):
        """Retourne la position Aster de l'assemblage."""
        return self._posast

    def __set_posast(self, position):
        """Donne la position Aster de l'assemblage."""
        self._posast = position
        self._posdam = self._position_todamac(position)

    def __get_posdam(self):
        """Retourne la position Damac de l'assemblage."""
        return self._posdam

    def __set_posdam(self, position):
        """Donne la position Damac de l'assemblage."""
        self._posast = self._position_toaster(position)
        self._posdam = position

    def __get_posthyc(self):
        """Retourne la position Damac de l'assemblage."""
        return self._posdam

    def __set_posthyc(self, position):
        """Donne la position Damac de l'assemblage."""
        self._posast = self._position_toaster(position)
        self._posdam = position

    idAST = property(__get_posast, __set_posast)
    idDAM = property(__get_posdam, __set_posdam)

    def _init_from_attrs(self):
        """Initialisation à partir des attributs de classe."""
        for attr in dir(self):
            if self._keys.get(attr):
                self._para[attr] = getattr(self, attr)

    def __getattr__(self, para):
        """Retourne la valeur d'un paramètre."""
        if self._para.get(para) is None:
            raise KeyError("parameter not defined : '%s'" % para)
        return self._para.get(para)

    def register_position(self, func_toaster, func_todamac):
        """Enregistre les fonctions 'position'."""
        self._position_toaster = func_toaster
        self._position_todamac = func_todamac

    def place(self, position, cycle,name ):
        """Place l'assemblage, définit ses propriétés."""
        self.idDAM = position
        self._cycle = cycle
        self.name   = name

    def definition(self, **params):
        """Définition des paramètres.
        On peut appeler plusieurs fois, notamment quand certains paramètres
        dépendent de la valeur d'autres."""
        for para, value in params.items():
            if not self._keys.get(para):
                raise KeyError("unknown parameter : '%s'" % para)
            self._para[para] = value

    def post_definition(self):
        """Méthode appelée après toutes les définitions."""

    def _check_00_parameters(self):
        """Vérification des caractéristiques obligatoires."""
        req = [key for key in self.required_parameters
               if self._para.get(key) is None]
        assert len(req) == 0, "Missing parameters: %s" % repr(req)

    def check(self):
        """Vérification des données."""
        if self._checked:
            return
        self.post_definition()
        # call all '_check_*' methods
        for name in dir(self):
            if name.startswith('_check_'):
                fcheck = getattr(self, name)
                if callable(fcheck):
                    fcheck()
        assert self.typeAC, "typeAC must be set by the subclass."
        assert self.mate, "'mate' not set."
        assert self._position_toaster and self._position_todamac, \
            "'position' functions not registered."
        self._checked = True

    def set_materiau(self, mate):
        """Défini le matériau de l'assemblage."""
        self.mate = mate

    def set_deforDAM(self, deforme):
        """Défini le matériau de l'assemblage."""
        self.deforme = deforme

    def mcf_geom_fibre(self):
        """Retourne les mots-clés facteurs pour DEFI_GEOM_FIBRE."""
        from Cata.cata import _F

        def vale_4fibres(surf, iner):
            """Retourne les triplets (y, z, val) pour les 4 fibres."""
            squart = surf / 4.
            excent = (iner / (2 * squart)) ** 0.5
            return (0., excent, squart,
                    0., -excent, squart,
                    excent, 0., squart,
                    -excent, 0., squart)
        mcf = (
            # crayon
            _F(GROUP_FIBRE='CR_' + self.idAST,
               COOR_AXE_POUTRE=(0., 0.),
               CARA="SURFACE",
               VALE=vale_4fibres(self.S_CR, self.I_CR,),),
            # partie courante des tubes-guides
            _F(GROUP_FIBRE='LG_' + self.idAST,
               COOR_AXE_POUTRE=(0., 0.),
               CARA="SURFACE",
               VALE=vale_4fibres(self.S_TG_C, self.I_TG_C,),),
            # biais des tubes-guides
            _F(GROUP_FIBRE='BI_' + self.idAST,
               COOR_AXE_POUTRE=(0., 0.),
               CARA="SURFACE",
               VALE=vale_4fibres(self.S_TG_B, self.I_TG_B,),),
            # retreint des tubes-guides
            _F(GROUP_FIBRE='RE_' + self.idAST,
               COOR_AXE_POUTRE=(0., 0.),
               CARA="SURFACE",
               VALE=vale_4fibres(self.S_TG_R, self.I_TG_R,),),
        )
        return mcf

    def mcf_cara_multifibre(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/MULTIFIBRE."""
        from Cata.cata import _F
        mcf = (
            _F(GROUP_MA='CR_' + self.idAST,
               GROUP_FIBRE='CR_' + self.idAST,),
            _F(GROUP_MA='LG_' + self.idAST,
               GROUP_FIBRE='LG_' + self.idAST,),
            _F(GROUP_MA='BI_' + self.idAST,
               GROUP_FIBRE='BI_' + self.idAST,),
            _F(GROUP_MA='RE_' + self.idAST,
               GROUP_FIBRE='RE_' + self.idAST,),
        )
        return mcf

    def mcf_cara_barre(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/BARRE."""
        from Cata.cata import _F
        # On donne une section unitaire
        mcf = (
            _F(GROUP_MA='MNT_' + self.idAST,
               SECTION='GENERALE',
               CARA='A',
               VALE=1.,),)
        return mcf

    def mcf_AC_mater(self):
        """Retourne les mots-clés facteurs pour AFFE_MATERIAU/AFFE."""
        from Cata.cata import _F
        mcf = (
            _F(GROUP_MA='CR_' + self.idAST, MATER=self.mate.mate['CR'],),
            _F(GROUP_MA='TG_' + self.idAST, MATER=self.mate.mate['TG'],),
            _F(GROUP_MA='ES_' + self.idAST, MATER=self.mate.mate['ES'],),
            _F(GROUP_MA='EI_' + self.idAST, MATER=self.mate.mate['EI'],),
            _F(GROUP_MA='MNT_' + self.idAST, MATER=self.mate.mate['MNT'],),
            _F(GROUP_MA='GC_' + self.idAST + '_B',
               MATER=self.mate.mate['GC_EB'],),
            _F(GROUP_MA='GC_' + self.idAST +
               '_T', MATER=self.mate.mate['GC_EH'],),
            _F(GROUP_MA='GC_' + self.idAST + '_M',
               MATER=self.mate.mate['GC_ME'],),
        )

        return mcf

    def mcf_cara_poutre(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/POUTRE."""
        from Cata.cata import _F
        mcf = (
            # crayons
            _F(GROUP_MA='CR_' + self.idAST,
               SECTION='GENERALE',
               CARA=('A', 'IZ', 'IY', 'JX', 'AY', 'AZ', 'EY', 'EZ'),
               VALE=(
                   self.S_CR, self.I_CR, self.I_CR, self.I_CR * 2., 1., 1., 0., 0.),),
            # partie courante des tubes-guides
            _F(GROUP_MA='LG_' + self.idAST,
               SECTION='GENERALE',
               CARA=('A', 'IZ', 'IY', 'JX', 'AY', 'AZ'),
               VALE=(
                   self.S_TG_C, self.I_TG_C, self.I_TG_C, self.I_TG_C * 2., 1., 1.),),
            # biais des tubes-guides
            _F(GROUP_MA='BI_' + self.idAST,
               SECTION='GENERALE',
               CARA=('A', 'IZ', 'IY', 'JX', 'AY', 'AZ'),
               VALE=(
                   self.S_TG_B, self.I_TG_B, self.I_TG_B, self.I_TG_B * 2., 1., 1.),),
            # retreint des tubes-guides
            _F(GROUP_MA='RE_' + self.idAST,
               SECTION='GENERALE',
               CARA=('A', 'IZ', 'IY', 'JX', 'AY', 'AZ'),
               VALE=(
                   self.S_TG_R, self.I_TG_R, self.I_TG_R, self.I_TG_R * 2., 1., 1.),),
            # embouts inférieurs
            _F(GROUP_MA='EI_' + self.idAST,
               SECTION='RECTANGLE',
               CARA=('H',),
               VALE=(self.Heinf,),),
            # embouts supérieurs
            _F(GROUP_MA='ES_' + self.idAST,
               SECTION='RECTANGLE',
               CARA=('H',),
               VALE=(self.Hesup,),),
        )
        return mcf

    def mcf_cara_discret(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/DISCRET."""
        from Cata.cata import _F

        def vale_K_TR_D_L(nb, kr):
            """Retourne les valeurs pour un K_TR_D_L."""
            kx = ky = kry = 1.e9 * nb / 4.
            kz = 0.
            krx = krz = kr * nb / 8.
            return (kx, ky, kz, krx, kry, krz)

        def vale_K_TR_L(kn_ax, ky_c, k1, k2, nbcr):
            """Retourne les valeurs pour un K_TR_L."""
            carel = [0.] * 78
            carel[1 - 1] = kn_ax * nbcr
            carel[3 - 1] = ky_c * nbcr / 4.
            carel[10 - 1] = nbcr * (k1 - k2) / 2.
            carel[21 - 1] = nbcr * k2 / 2.
            carel[28 - 1] = kn_ax * nbcr
            carel[36 - 1] = ky_c * nbcr / 4.
            carel[55 - 1] = nbcr * (k1 - k2) / 2.
            carel[78 - 1] = nbcr * k2 / 2.
            carel[22 - 1] = -1. * kn_ax * nbcr
            carel[30 - 1] = -1. * ky_c * nbcr / 4.
            carel[49 - 1] = -1. * nbcr * (k1 - k2) / 2.
            carel[72 - 1] = -1. * nbcr * k2 / 2.
            return carel

        mcf = (
            # --- Pour les discrets des liaisons grilles / crayons
            # --- Pour les grilles de melanges
            _F(GROUP_MA='GC_' + self.idAST + '_M',
               REPERE='LOCAL',
               CARA='K_TR_L',
               VALE=vale_K_TR_L(
                   self.KNAXM, self.KY_CM, self.KM1, self.KM2, self.NBCR),),
            _F(GROUP_MA='GC_' + self.idAST + '_M',
               REPERE='LOCAL',
               CARA='M_TR_D_L',
               VALE=(0., 0., 0., 0.,),),
            # --- Pour les grilles extremites
            _F(GROUP_MA=('GC_' + self.idAST + '_B', 'GC_' + self.idAST + '_T',),
               REPERE='LOCAL',
               CARA='K_TR_L',
               VALE=vale_K_TR_L(
                   self.KNAXE, self.KY_CE, self.KE1, self.KE2, self.NBCR),),
            _F(GROUP_MA=('GC_' + self.idAST + '_B', 'GC_' + self.idAST + '_T',),
               REPERE='LOCAL',
               CARA='M_TR_D_L',
               VALE=(0., 0., 0., 0.,),),

            # discrets des liaisons grilles / tubes-guide
            # grilles de mélanges
            _F(GROUP_MA='GT_' + self.idAST + '_M',
               REPERE='LOCAL',
               CARA='K_TR_D_L',
               VALE=vale_K_TR_D_L(self.NBTG, self.KR_GM),),
            _F(GROUP_MA='GT_' + self.idAST + '_M',
               REPERE='LOCAL',
               CARA='M_TR_D_L',
               VALE=(0., 0., 0., 0.,),),
            # grilles extrémités
            _F(GROUP_MA='GT_' + self.idAST + '_E',
               REPERE='LOCAL',
               CARA='K_TR_D_L',
               VALE=vale_K_TR_D_L(self.NBTG, self.KR_GE),),
            _F(GROUP_MA='GT_' + self.idAST + '_E',
               REPERE='LOCAL',
               CARA='M_TR_D_L',
               VALE=(0., 0., 0., 0.,),),
            # poids des grilles
            _F(GROUP_MA='GR_' + self.idAST,
               REPERE='LOCAL',
               CARA='M_T_D_N',
               VALE=self.m_gri / 4.,),
            # ressort pour blocage bas crayons
             _F(GROUP_MA='CREI',
               REPERE='GLOBAL',
               CARA='K_T_D_L',
               VALE=(1.e4,1.e4,1.e4)),
             _F(GROUP_MA='CREIC',
               REPERE='GLOBAL',
               CARA='K_T_D_L',
               VALE=(1.e9,1.e9,1.e9)),
        )
        return mcf

    def mcf_deform_impo(self):
        """Retourne les mots-clés facteurs pour AFFE_CHAR_CINE/MECA_IMPO."""
        from Cata.cata import _F
        mcf = []
        for igr in range(1, self.NBGR + 1):
            mcf.append(_F(GROUP_NO='G_' + self.idAST + '_' + str(igr),
                          DY=self.deforme['DY' + str(igr)],
                          DZ=self.deforme['DZ' + str(igr)]))
        return mcf

    def mcf_archimede_nodal(self):
        """Retourne les mots-clés facteurs pour AFFE_CHAR_MECA/FORCE_NODALE
            dans la prise en compte de la poussée d Archimede."""
        from Cata.cata import _F
        mcf = []
        mcf.append(_F(GROUP_NO='PS_' + self.idAST, FX=self.AFEBSU_1,),)
        mcf.append(_F(GROUP_NO='PI_' + self.idAST, FX=self.AFEBIN_1,),)

        mcf.append(
            _F(GROUP_NO='G_' + self.idAST + '_' + str(1), FX=self.AFGRE_1 / 4.,),)
        for igr in range(1, self.NBGR - 1):
            mcf.append(
                _F(GROUP_NO='G_' + self.idAST + '_' + str(igr + 1), FX=self.AFGRM_1 / 4.,),)

        mcf.append(
            _F(GROUP_NO='G_' + self.idAST + '_' + str(self.NBGR), FX=self.AFGRE_1 / 4.,),)
        return mcf

    def mcf_archimede_poutre(self, FXTG, FXCR):
        """Retourne les mots-clés facteurs pour AFFE_CHAR_MECA_F/FORCE_POUTRE
            dans la prise en compte de la poussée d Archimede."""
        from Cata.cata import _F
        mcf = (
            _F(GROUP_MA='TG_' + self.idAST, FX=FXTG,),
            _F(GROUP_MA='CR_' + self.idAST, FX=FXCR,),
        )
        return mcf


class AssemblageAFAXL(Assemblage):

    """Particularités du type AFAXL."""

    def post_definition(self):
        """Méthode appelée après toutes les définitions.
        Définition des caractéristiques déterminées à partir des
        autres"""
        self.definition(
            EPMOY=(self.EP1GU + self.EP2GU) / 2,
            Heinf = ((self.Leinf * self.Keinf) / self.EEI0) ** 0.5,
            Hesup = ((self.Lesup * self.Kesup) / self.EES0) ** 0.5,
        )

        self.definition(
            S_TG_C=pi *
            (self.RAY1GU ** 2 - (self.RAY1GU - self.EP1GU) ** 2),
            I_TG_C=pi / 4 *
            (self.RAY1GU ** 4 - (self.RAY1GU - self.EP1GU) ** 4),
            S_TG_R=pi *
            (self.RAY2GU ** 2 - (self.RAY2GU - self.EP2GU) ** 2),
            I_TG_R=pi / 4 *
            (self.RAY2GU ** 4 - (self.RAY2GU - self.EP2GU) ** 4),
            S_TG_B=pi *
            (self.RAY2GU ** 2 - (self.RAY2GU - self.EPMOY) ** 2),
            I_TG_B=pi / 4 *
            (self.RAY2GU ** 4 - (self.RAY2GU - self.EPMOY) ** 4),

            S_CR=pi * (self.RAYCRA ** 2 - (self.RAYCRA - self.EPCRA) ** 2),
            I_CR=pi / 4 *
            (self.RAYCRA ** 4 - (self.RAYCRA - self.EPCRA) ** 4),
        )


class ACFactory(Mac3Factory):

    """Classe pour construire les objets Assemblage."""

    def build_supported_types(self):
        """Construit la liste des types autorisés."""
        ctxt = {}
        for obj, val in globals().items():
            if type(val) is type and issubclass(val, Assemblage):
                ctxt[obj] = val
        return ctxt
