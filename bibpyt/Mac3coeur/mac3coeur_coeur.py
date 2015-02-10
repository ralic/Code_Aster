# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

Définition d'une conception de coeur (ensemble d'assemblages).
"""

import os

from mac3coeur_factory import Mac3Factory
from mac3coeur_assemblage import ACFactory


class Coeur(object):

    """Classe définissant un coeur de reacteur."""
    type_coeur = None
    required_parameters = [
        # Nombre d'assemblages pour définir le coeur
        'NBAC',
        # Position des grilles pour definition du champ de fluence
        'altitude',
        # Position des crayons et tubes-guides pour definition du champ de
        # fluence
        'XINFT', 'XSUPT', 'XINFC', 'XSUPC', 'LONCR', 'LONTU',
        # Caractéristique de la cuve
        'pas_assemblage',
        'XINFCUVE', 'XSUPCUVE',
        #---fleche des ressorts de maintien à la fermeture de la cuve
        'flechResMaint',
        #---Dimensions de la cavité entre PIC (ou FSC) et PSC
        'Hcav1centre', 'Hcav2centre', 'Hcav3centre', 'Hcav4centre',
        'Hcav1periph', 'Hcav2periph', 'Hcav3periph', 'Hcav4periph',
        # Températures caractérisitiques
        'TP_REF', 'ARRET_FR', 'ARRET_CH', 'TINFCUVE', 'TSUPCUVE', 'TENVELOP', 'TP_TG1', 'TP_TG2', 'TXX1', 'TXX2', 'TXX3', 'TXX4',
        # Abscisses caracteristiques pour le profil de temperature des crayons
        'SXX2', 'SXX3',
        # paramètres de l'interpolation linéaire
        # du coefficient de dilatation des internes de cuve
        'ALPH1', 'ALPH2',
        # Geometrie du coeur
        'ALPHABET', 'ALPHAMAC', 'NumV',
        # Post-traitement des lames
        'nomContactAssLame', 'nomContactCuve',
    ]
    _time = ('T0', 'T0b', 'T1', 'T2', 'T3',
             'T4', 'T5', 'T6', 'T7', 'T8', 'T8b', 'T9',)
    _subtime = ('N0', 'N0b', 'N1', 'N2', 'N3',
                'N4', 'N5', 'N6', 'N7', 'N8', 'N8b', 'N9')

    def __init__(self, name, typ_coeur, macro, datg):
        """Initialisation d'un type de coeur."""
        self.name = name
        self.macro = macro
        self.typ_coeur = typ_coeur
        self.nbac = 0
        self.factory = ACFactory(datg)
        self.collAC = {}
        self._mateAC = {}
        self.nameAC = {}
        self.temps_simu = {}.fromkeys(self._time)
        self.sub_temps_simu = {}.fromkeys(self._subtime)
        self._para = {}
        self._keys = {}.fromkeys(self.required_parameters)
        self._init_from_attrs()
        self.dcorr1 = dict(zip(self.ALPHABET, self.ALPHAMAC))
        self.dcorr2 = dict(zip(self.ALPHAMAC, self.ALPHABET))
        self.dnume2 = dict(
            zip(self.ALPHAMAC, range(1, len(self.ALPHAMAC) + 1)))

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

    def get_geom_coeur(self):
        """Retourne la géométrie du coeur."""
        raise NotImplementedError

    def position_toaster(self, position):
        """Retourne la position Aster correspondant à la position DAMAC."""
        lig, col = position[0], position[1:]
        ind = int(col) - 1
        try:
            posi_aster = self.ALPHAMAC[ind] + "_" + self.dcorr1[lig]
        except (IndexError, KeyError):
            raise KeyError("invalid damac position : %s" % position)
        return posi_aster

    def position_todamac(self, position):
        """Retourne la position DAMAC correspondant à la position Aster."""
        col, lig = position.split("_")
        try:
            posi_damac = self.dcorr2[lig] + '%02d' % (self.dnume2[col])
        except KeyError:
            raise KeyError("invalid aster position : %s" % position)

        return posi_damac

    def init_from_table(self, tab):
        """Initialise le coeur à partir d'une table."""
        self.nbac = len(tab)
        for rows in tab:
            idAC = rows['idAC'].strip()
            typeAC = rows['Milieu'].strip()
            nameAC = rows['Repere'].strip()
            ac = self.factory.get(typeAC)(self.typ_coeur)
            ac.register_position(self.position_toaster, self.position_todamac)
            ac.place(idAC, rows['Cycle'])
            if self._mateAC.get(typeAC) is None:
                self._mateAC[typeAC] = MateriauAC(typeAC, self.macro)
            ac_def = {}
            for igr in range(0, ac._para['NBGR']):
                ac_def['DY' + str(igr + 1)] = rows[
                    'XG' + str(igr + 1)] / 1000.0
                ac_def['DZ' + str(igr + 1)] = - rows[
                    'YG' + str(igr + 1)] / 1000.0
            ac.set_deforDAM(ac_def)
            ac.set_materiau(self._mateAC[typeAC])
            ac.check()
            self.collAC[idAC] = ac
            self.nameAC[nameAC] = ac.idAST

    def mcf_deform_impo(self):
        """Retourne les mots-clés facteurs pour AFFE_CHAR_CINE/MECA_IMPO."""
        from Accas import _F
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_deform_impo())

        mtmp = (_F(GROUP_MA='CRAYON',           DRX=0.,),
                _F(GROUP_NO='LISPG',            DRX=0., DRY=0., DRZ=0.),
                _F(GROUP_MA=('EBOSUP', 'EBOINF'), DRX=0., DRY=0., DRZ=0.),)
        mcf.extend(mtmp)
        return mcf

    def repr(self):
        """Liste les assemblages."""
        txt = ["Lecture du Coeur %s - composé de %d assemblages"
               % (self.name, self.nbac)]
        all = self.collAC.items()
        all.sort()
        txt.append(
            "position_DAMAC correspondance_Code_Aster Type_de_conception Nombre_de_cycle")
        for idAC, ac in all:
            txt.append("%8s %8s %8s %i" %
                       (idAC, ac.idAST, ac.typeAC, ac._cycle))
        return os.linesep.join(txt)

    def mcf_geom_fibre(self):
        """Retourne les mots-clés facteurs pour DEFI_GEOM_FIBRE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_geom_fibre())
        return mcf

    def mcf_cara_multifibre(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/MULTIFIBRE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_cara_multifibre())
        return mcf

    def mcf_cara_barre(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/BARRE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_cara_barre())
        return mcf

    def mcf_cara_poutre(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/POUTRE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_cara_poutre())
        return mcf

    def mcf_cara_discret(self):
        """Retourne les mots-clés facteurs pour AFFE_CARA_ELEM/DISCRET."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_cara_discret())
        return mcf

    def definition_geom_fibre(self):
        DEFI_GEOM_FIBRE = self.macro.get_cmd('DEFI_GEOM_FIBRE')

        mcf = self.mcf_geom_fibre()
        _GFF = DEFI_GEOM_FIBRE(FIBRE=mcf,)

        return _GFF

    def affe_char_lame(self, MODELE):
        AFFE_CHAR_CINE = self.macro.get_cmd('AFFE_CHAR_CINE')
        mcf = self.mcf_deform_impo()

        _AF_CIN = AFFE_CHAR_CINE(MODELE=MODELE, MECA_IMPO=mcf)
        return _AF_CIN

    def mcf_archimede_nodal(self):
        """Retourne les mots-clés facteurs pour AFFE_CHAR_MECA/FORCE_NODALE."""
        mcf = []
        for ac in self.collAC.values():
            mcf.extend(ac.mcf_archimede_nodal())
        return mcf

    def definition_archimede_nodal(self, MODELE):
        AFFE_CHAR_MECA = self.macro.get_cmd('AFFE_CHAR_MECA')
        mcf = self.mcf_archimede_nodal()

        _ARCH_1 = AFFE_CHAR_MECA(MODELE=MODELE, FORCE_NODALE=mcf)
        return _ARCH_1

    def mcf_archimede_poutre(self):
        """Retourne les mots-clés facteurs pour AFFE_CHAR_MECA_F/FORCE_POUTRE."""
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        mcf = []
        for ac in self.collAC.values():
            _FCT_TG = DEFI_FONCTION(
                NOM_PARA='X', PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
                VALE=(
                    ac.XINFT, (ac.AFTG_1 / ac.LONTU),
                    ac.XSUPT, (ac.AFTG_1 / ac.LONTU)))
            _FCT_CR = DEFI_FONCTION(
                NOM_PARA='X', PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
                VALE=(
                    ac.XINFC, (ac.AFCRA_1 / ac.LONCR),
                    ac.XSUPC, (ac.AFCRA_1 / ac.LONCR)))
            mcf.extend(ac.mcf_archimede_poutre(_FCT_TG, _FCT_CR))
        return mcf

    def definition_archimede_poutre(self, MODELE):
        AFFE_CHAR_MECA_F = self.macro.get_cmd('AFFE_CHAR_MECA_F')
        mcf = self.mcf_archimede_poutre()

        _FOARCH_1 = AFFE_CHAR_MECA_F(MODELE=MODELE, FORCE_POUTRE=mcf)
        return _FOARCH_1

    def definition_temp_archimede(self, use_archimede):
        """ Valeur à froid (20 degrés) de la force d'Archimède = 860/985.46*1000.52 """
        assert self.temps_simu[
            'T0'] is not None, '`definition_time` must be called first!'
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')

        assert use_archimede in ('OUI', 'NON')

        # cas ou la force d'archimede est activee
        if use_archimede == 'OUI':

            ARCHFR1 = 873.  # Valeur en arret a froid (20 degres)
            ARCHFR2 = 860.  # Valeur en arret a froid (60 degres)
            ARCHCH = 620.  # Valeur a chaud (307 degres)

        # cas ou la force d'archimede n'est pas activee
        elif use_archimede == 'NON':

            ARCHFR1 = 0.
            ARCHFR2 = 0.
            ARCHCH = 0.

        _ARCH_F1 = DEFI_FONCTION(NOM_PARA='INST',
                                 PROL_DROITE='CONSTANT',
                                 PROL_GAUCHE='CONSTANT',
                                 VALE=(self.temps_simu['T0'], ARCHFR1,
                                       self.temps_simu['T1'], ARCHFR1,
                                       self.temps_simu['T2'], ARCHFR2,
                                       self.temps_simu['T4'], ARCHCH,
                                       self.temps_simu['T5'], ARCHCH,
                                       self.temps_simu['T7'], ARCHFR2,
                                       self.temps_simu['T8'], ARCHFR1,
                                       self.temps_simu['T9'], ARCHFR1,),)

        return _ARCH_F1

    def definition_temp_hydro_axiale(self):
        """ Fonction multiplicative de la force hydrodynamique axiale.
            On multiplie par 0.722 les forces hydrodynamiques a froid pour obtenir celles a chaud."""
        assert self.temps_simu[
            'T0'] is not None, '`definition_time` must be called first!'
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        FOHYFR_1 = 1.0    # Valeur a froid
        FOHYCH_1 = 0.722  # Valeur a chaud

        _HYDR_F1 = DEFI_FONCTION(
            NOM_PARA='INST', PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
            VALE=(self.temps_simu['T0'], 0.0,
                  self.temps_simu['T1'], 0.0,
                  self.temps_simu['T2'], FOHYFR_1,
                  self.temps_simu['T3'], FOHYCH_1,
                  self.temps_simu['T4'], FOHYCH_1,
                  self.temps_simu['T5'], FOHYCH_1,
                  self.temps_simu['T6'], FOHYCH_1,
                  self.temps_simu['T7'], FOHYFR_1,
                  self.temps_simu['T8'], 0.0,
                  self.temps_simu['T9'], 0.0,),)
        return _HYDR_F1

    def definition_effort_transverse(self):
        """ Fonction multiplicative pour la prise en compte des efforts transverses."""
        assert self.temps_simu[
            'T0'] is not None, '`definition_time` must be called first!'
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        AVEC = 1.0
        SANS = 0.0

        _F_TRAN1 = DEFI_FONCTION(
            NOM_PARA='INST', PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
            VALE=(self.temps_simu['T0'], SANS,
                  self.temps_simu['T1'], SANS,
                  self.temps_simu['T2'], SANS,
                  self.temps_simu['T4'], AVEC,
                  self.temps_simu['T5'], AVEC,
                  self.temps_simu['T7'], SANS,
                  self.temps_simu['T8'], SANS,
                  self.temps_simu['T9'], SANS,),)
        return _F_TRAN1

    def definition_cara_coeur(self, MODELE, _GFF):
        from Accas import _F
        AFFE_CARA_ELEM = self.macro.get_cmd('AFFE_CARA_ELEM')

        mcm = self.mcf_cara_multifibre()
        mcr = self.mcf_cara_barre()
        mcp = self.mcf_cara_poutre()
        mtmp = _F(GROUP_MA='DIL', SECTION='RECTANGLE',
                  CARA=('HY', 'HZ'), VALE=(0.03, 0.21338),)
        mcp.append(mtmp)
        mcd = self.mcf_cara_discret()
        mtmp = _F(GROUP_MA='RES_TOT', REPERE='LOCAL',
                  CARA='K_T_D_L', VALE=(0., 0., 0.,),)
        mcd.append(mtmp)
        mtmp = _F(GROUP_MA='RES_TOT', REPERE='LOCAL', CARA='M_T_D_L', VALE=0.,)
        mcd.append(mtmp)

        _CARA = AFFE_CARA_ELEM(MODELE=MODELE,
                               POUTRE=mcp,
                               BARRE=mcr,
                               GEOM_FIBRE=_GFF,
                               MULTIFIBRE=mcm,
                               DISCRET=mcd,
                               ORIENTATION=(_F(GROUP_MA=('ELA_EX', 'ELA_ME', 'RIG_EX', 'RIG_ME', 'DIL'), CARA='VECT_Y', VALE=(1., 0., 0.),),),)
        return _CARA

    def definition_pesanteur(self, MODELE):
        from Accas import _F
        AFFE_CHAR_MECA = self.macro.get_cmd('AFFE_CHAR_MECA')

        _PESA = AFFE_CHAR_MECA(MODELE=MODELE,
                               PESANTEUR=_F(GRAVITE=9.81, DIRECTION=(-1., 0., 0.),),)
        return _PESA

    def definition_maintien_type(self, model, typ, force=None):
        """Retourne le chargement dû au couvercle de la cuve selon le type"""
        assert typ in ('FORCE', 'DEPL_PSC')
        if typ != 'FORCE':
            return self.definition_effor_maintien(model)
        else:
            return self.definition_effor_maintien_force(model, force)

    def definition_effor_maintien(self, MODELE):
        """Retourne les déplacements imposés aux noeuds modélisant la PSC
        et traduisant la fermeture de la cuve"""
        assert self.temps_simu[
            'T0'] is not None, '`definition_time` must be called first!'
        from Accas import _F
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        AFFE_CHAR_MECA_F = self.macro.get_cmd('AFFE_CHAR_MECA_F')

        _DXpsc = DEFI_FONCTION(NOM_PARA='INST',
                               VALE=(-2.0,   0.,
                                     -1.0,   0.,
                                     self.temps_simu['T0'],   0.,
                                     self.temps_simu['T0b'],   0.,
                                     self.temps_simu[
                                     'T1'],   -1. * self.flechResMaint,
                                     self.temps_simu[
                                     'T2'],   -1. * self.flechResMaint,
                                     self.temps_simu[
                                     'T3'],   -1. * self.flechResMaint,
                                     self.temps_simu[
                                     'T4'],   -1. * self.flechResMaint,
                                     self.temps_simu[
                                     'T5'],   -1. * self.flechResMaint,
                                     self.temps_simu[
                                     'T6'],   -1. * self.flechResMaint,
                                     self.temps_simu[
                                     'T7'],   -1. * self.flechResMaint,
                                     self.temps_simu[
                                     'T8'],   -1. * self.flechResMaint,
                                     self.temps_simu[
                                     'T8b'],  -1. * self.flechResMaint / 3.,
                                     self.temps_simu['T9'],   0.,),
                               PROL_DROITE='CONSTANT',
                               PROL_GAUCHE='CONSTANT',)

        _F_EMBO = AFFE_CHAR_MECA_F(MODELE=MODELE,
                                   DDL_IMPO=_F(GROUP_NO='PMNT_S',           DX=_DXpsc,),)
        return _F_EMBO

    def definition_effor_maintien_force(self, MODELE, ForceMaintien):
        """Retourne le chargement d'effort de maintien considéré constant"""
        assert self.temps_simu[
            'T0'] is not None, '`definition_time` must be called first!'
        from Accas import _F
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        AFFE_CHAR_MECA_F = self.macro.get_cmd('AFFE_CHAR_MECA_F')

        _FXpsc = DEFI_FONCTION(NOM_PARA='INST',
                               VALE=(-2.0,   0.,
                                     -1.0,   0.,
                                     self.temps_simu['T0'],   0.,
                                     self.temps_simu['T0b'],   0.,
                                     self.temps_simu[
                                         'T1'],   -1. * ForceMaintien,
                                     self.temps_simu[
                                         'T2'],   -1. * ForceMaintien,
                                     self.temps_simu[
                                         'T3'],   -1. * ForceMaintien,
                                     self.temps_simu[
                                         'T4'],   -1. * ForceMaintien,
                                     self.temps_simu[
                                         'T5'],   -1. * ForceMaintien,
                                     self.temps_simu[
                                         'T6'],   -1. * ForceMaintien,
                                     self.temps_simu[
                                         'T7'],   -1. * ForceMaintien,
                                     self.temps_simu[
                                         'T8'],   -1. * ForceMaintien,
                                     self.temps_simu[
                                     'T8b'],  -1. * ForceMaintien / 30.,
                                     self.temps_simu['T9'],   0.,),
                               PROL_DROITE='CONSTANT',
                               PROL_GAUCHE='CONSTANT',)

        _F_EMBO = AFFE_CHAR_MECA_F(MODELE=MODELE,
                                   FORCE_NODALE=_F(GROUP_NO='PMNT_S',           FX=_FXpsc,),)
        return _F_EMBO

    def affectation_maillage(self, MA0):
        from Accas import _F
        CREA_MAILLAGE = self.macro.get_cmd('CREA_MAILLAGE')
        DEFI_GROUP = self.macro.get_cmd('DEFI_GROUP')

        LISGRIL = []
        LISGRILI = []
        LISGRILE = []
        LISG = []
        LIS_PG = []
        nbgrmax = 0
        for ac in self.collAC.values():
            nbgrmax = max(nbgrmax, ac._para['NBGR'])
            LIS_GNO = []
            for igr in range(0, ac._para['NBGR']):
                LIS_GNO.append('G_' + ac.idAST + '_' + str(igr + 1))
                LIS_PG.append('P_' + ac.idAST + '_' + str(igr + 1))

            DICG = {}
            DICG["GROUP_NO"] = tuple(LIS_GNO)
            DICG["NOM_GROUP_MA"] = 'GR_' + ac.idAST
            LISG.append(DICG)

        for igr in range(0, nbgrmax):
            DICGRIL = {}
            DICGRIL["GROUP_NO"] = 'GRIL_' + str(igr + 1)
            DICGRIL["NOM_GROUP_MA"] = 'GRIL_' + str(igr + 1)
            LISGRIL.append(DICGRIL)

            if igr == 0:
                LISGRILE.append('GRIL_' + str(igr + 1))
            elif igr == (nbgrmax - 1):
                LISGRILE.append('GRIL_' + str(igr + 1))
            else:
                LISGRILI.append('GRIL_' + str(igr + 1))

        _MA = CREA_MAILLAGE(MAILLAGE=MA0,
                            CREA_POI1=tuple(LISGRIL + LISG),)

        _MA = DEFI_GROUP(reuse=_MA, ALARME='NON',
                         MAILLAGE=_MA,
                         CREA_GROUP_MA=(
                             _F(NOM='GRIL_I', UNION=tuple(LISGRILI),),
                         _F(NOM='GRIL_E', UNION=tuple(LISGRILE),),),
                         CREA_GROUP_NO=(
                         _F(GROUP_MA=('T_GUIDE', 'EBOSUP', 'EBOINF',
                                      'CRAYON', 'ELA', 'DIL', 'MAINTIEN',),),
                         _F(NOM='LISPG', UNION=tuple(LIS_PG),),),)

        return _MA

    def recuperation_donnees_geom(self, MAILL):
        """recuperation de donnees géometrique a partir du maillage"""
        from Accas import _F
        CREA_MAILLAGE = self.macro.get_cmd('CREA_MAILLAGE')
        RECU_TABLE = self.macro.get_cmd('RECU_TABLE')
        DETRUIRE = self.macro.get_cmd('DETRUIRE')

        #--- recuperation de donnees géometriques ---
        # nombre d'assemblages dans le coeur
        self.NBAC = len(self.collAC.values())

        # altitudes mini et maxi de la cavité de coeur
        _ma_tmp = CREA_MAILLAGE(
            MAILLAGE=MAILL, RESTREINT=_F(GROUP_MA='EBOINF',),)
        _TAB_tmp = RECU_TABLE(CO=_ma_tmp, NOM_TABLE='CARA_GEOM',)
        self.XINFCUVE = _TAB_tmp['X_MIN', 1]
        DETRUIRE(CONCEPT=_F(NOM=_ma_tmp), INFO=1,)
        DETRUIRE(CONCEPT=_F(NOM=_TAB_tmp), INFO=1,)

        _ma_tmp = CREA_MAILLAGE(
            MAILLAGE=MAILL, RESTREINT=_F(GROUP_MA='MAINTIEN',),)
        _TAB_tmp = RECU_TABLE(CO=_ma_tmp, NOM_TABLE='CARA_GEOM',)
        self.XSUPCUVE = _TAB_tmp['X_MAX', 1]
        DETRUIRE(CONCEPT=_F(NOM=_ma_tmp), INFO=1,)
        DETRUIRE(CONCEPT=_F(NOM=_TAB_tmp), INFO=1,)

        # altitudes mini et maxi, et longueur de l'ensemble des crayons
        _ma_tmp = CREA_MAILLAGE(
            MAILLAGE=MAILL, RESTREINT=_F(GROUP_MA='CRAYON',),)
        _TAB_tmp = RECU_TABLE(CO=_ma_tmp, NOM_TABLE='CARA_GEOM',)
        self.XINFC = _TAB_tmp['X_MIN', 1]
        self.XSUPC = _TAB_tmp['X_MAX', 1]
        self.LONCR = _TAB_tmp['X_MAX', 1] - _TAB_tmp['X_MIN', 1]
        DETRUIRE(CONCEPT=_F(NOM=_ma_tmp), INFO=1,)
        DETRUIRE(CONCEPT=_F(NOM=_TAB_tmp), INFO=1,)

        # altitudes mini et maxi, et longueur de l'ensemble des tubes
        _ma_tmp = CREA_MAILLAGE(
            MAILLAGE=MAILL, RESTREINT=_F(GROUP_MA='T_GUIDE',),)
        _TAB_tmp = RECU_TABLE(CO=_ma_tmp, NOM_TABLE='CARA_GEOM',)
        self.XINFT = _TAB_tmp['X_MIN', 1]
        self.XSUPT = _TAB_tmp['X_MAX', 1]
        self.LONTU = _TAB_tmp['X_MAX', 1] - _TAB_tmp['X_MIN', 1]
        DETRUIRE(CONCEPT=_F(NOM=_ma_tmp), INFO=1,)
        DETRUIRE(CONCEPT=_F(NOM=_TAB_tmp), INFO=1,)

        # altitudes moyennes des grilles
        self.altitude = []
        _ma_tmp = CREA_MAILLAGE(
            MAILLAGE=MAILL, RESTREINT=_F(GROUP_MA='ELA',),)
        _TAB_tmp = RECU_TABLE(CO=_ma_tmp, NOM_TABLE='CARA_GEOM',)
        altimax = _TAB_tmp['X_MAX', 1]
        DETRUIRE(CONCEPT=_F(NOM=_ma_tmp), INFO=1,)
        DETRUIRE(CONCEPT=_F(NOM=_TAB_tmp), INFO=1,)
        altimaxtmp = 0
        while altimaxtmp != altimax:  # tant que l'on ne dépasse pas la grille la plus haute
            _ma_tmp = CREA_MAILLAGE(MAILLAGE=MAILL, RESTREINT=_F(
                GROUP_MA='GRIL_' + str(len(self.altitude) + 1),),)
            _TAB_tmp = RECU_TABLE(CO=_ma_tmp, NOM_TABLE='CARA_GEOM',)
            altimintmp = _TAB_tmp['X_MAX', 1]
            altimaxtmp = _TAB_tmp['X_MAX', 1]
            self.altitude.append((altimintmp + altimaxtmp) / 2.)
            DETRUIRE(CONCEPT=_F(NOM=_ma_tmp), INFO=1,)
            DETRUIRE(CONCEPT=_F(NOM=_TAB_tmp), INFO=1,)

        return

    def cl_rigidite_grille(self):
        from Accas import _F

        mcf = []
        for ac in self.collAC.values():
            LIS_GNO = []
            for igr in range(0, ac._para['NBGR']):
                mcf.append(_F(GROUP_NO='G_' + ac.idAST + '_' + str(igr + 1)))
        return mcf

    def affectation_modele(self, MAILLAGE):
        from Accas import _F
        AFFE_MODELE = self.macro.get_cmd('AFFE_MODELE')
        _MODELE = AFFE_MODELE(MAILLAGE=MAILLAGE,
                              AFFE=(_F(GROUP_MA='CRAYON',
                                       PHENOMENE='MECANIQUE',
                                       MODELISATION='POU_D_TGM',),
                                    _F(GROUP_MA='T_GUIDE',
                                       PHENOMENE='MECANIQUE',
                                       MODELISATION='POU_D_TGM',),
                                    _F(GROUP_MA=('EBOSUP', 'EBOINF'),
                                       PHENOMENE='MECANIQUE',
                                       MODELISATION='POU_D_T',),
                                    _F(GROUP_MA=('ELA', 'RIG'),
                                       PHENOMENE = 'MECANIQUE',
                                       MODELISATION = 'DIS_TR',),
                                    _F(GROUP_MA = 'DIL',
                                       PHENOMENE = 'MECANIQUE',
                                       MODELISATION = 'POU_D_E',),
                                    _F(GROUP_MA =('GRIL_I', 'GRIL_E',),
                                       PHENOMENE = 'MECANIQUE',
                                       MODELISATION = 'DIS_T',),
                                    _F(GROUP_MA =('MAINTIEN',),
                                       PHENOMENE = 'MECANIQUE',
                                       MODELISATION = 'BARRE',),
                                    _F(GROUP_MA ='RES_TOT',
                                       PHENOMENE = 'MECANIQUE',
                                       MODELISATION = 'DIS_T',),),)

        return _MODELE

    def definition_time(self, fluence, subdivis):
        """Return the list of timesteps"""
        from Accas import _F
        DEFI_LIST_REEL = self.macro.get_cmd('DEFI_LIST_REEL')
        DEFI_LIST_INST = self.macro.get_cmd('DEFI_LIST_INST')

        def m_time(a):
            m_time = (
                _F(JUSQU_A=self.temps_simu[self._time[a]], NOMBRE=self.sub_temps_simu[self._subtime[a]],),)
            return m_time

        self.init_temps_simu(fluence, subdivis)

        _list = []
        for _time in range(1, len(self._time)):
            _list.extend(m_time(_time))

        _LI = DEFI_LIST_REEL(DEBUT=self.temps_simu['T0'], INTERVALLE=_list,)

        _TE = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=_LI,),
                             ECHEC=_F(SUBD_PAS=2, SUBD_NIVEAU=5,),)

        return _TE

    def init_temps_simu(self, fluence, subdivis):
        """Initialise les temps caracteristiques"""
        Dt = 1.e-3
        self.temps_simu['T0'] = 0.0
        self.temps_simu['T0b'] = self.temps_simu['T0'] + Dt / 2
        self.temps_simu['T1'] = self.temps_simu['T0'] + Dt
        self.temps_simu['T2'] = self.temps_simu['T1'] + Dt
        self.temps_simu['T3'] = self.temps_simu['T2'] + Dt
        self.temps_simu['T4'] = self.temps_simu['T3'] + Dt
        self.temps_simu['T5'] = self.temps_simu['T4'] + max(fluence, Dt)
        self.temps_simu['T6'] = self.temps_simu['T5'] + Dt
        self.temps_simu['T7'] = self.temps_simu['T6'] + Dt
        self.temps_simu['T8'] = self.temps_simu['T7'] + Dt
        self.temps_simu['T8b'] = self.temps_simu['T8'] + Dt / 2
        self.temps_simu['T9'] = self.temps_simu['T8'] + Dt

        self.sub_temps_simu['N0'] = 2
        self.sub_temps_simu['N0b'] = 1
        self.sub_temps_simu['N1'] = 2 * subdivis
        self.sub_temps_simu['N2'] = 2
        self.sub_temps_simu['N3'] = 2 * subdivis
        self.sub_temps_simu['N4'] = 2 * subdivis
        self.sub_temps_simu['N5'] = 50
        self.sub_temps_simu['N6'] = 2 * subdivis
        self.sub_temps_simu['N7'] = 2 * subdivis
        self.sub_temps_simu['N8'] = 2
        self.sub_temps_simu['N8b'] = 2 * subdivis * 2
        self.sub_temps_simu['N9'] = 1

    def definition_fluence(self, fluence, MAILLAGE):
        """Return the time evolution of the field of fluence"""
        assert self.temps_simu[
            'T0'] is not None, '`definition_time` must be called first!'
        from Accas import _F
        CREA_CHAMP = self.macro.get_cmd('CREA_CHAMP')
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        DEFI_NAPPE = self.macro.get_cmd('DEFI_NAPPE')
        FORMULE = self.macro.get_cmd('FORMULE')
        CREA_RESU = self.macro.get_cmd('CREA_RESU')

        #
        # CREATION D UNE NAPPE DE FLUX NEUTRONIQUE DANS LE COEUR   #
        #
        # CREATION DE LA PARTIE GEOMETRIQUE        #
        #
        _CHXN = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='NOEU_GEOM_R',
                           NOM_CHAM='GEOMETRIE', MAILLAGE=MAILLAGE)

        #
        # CREATION DU PROFIL AXIAL DE FLUX   #
        #
        _FLUXAX1 = DEFI_FONCTION(NOM_PARA='X',
                                 VALE=(self.altitude[0],  0.54,
                                       self.altitude[1],  1.,
                                       self.altitude[-3], 1.,
                                       self.altitude[-2], 0.85,
                                       self.altitude[-1], 0.06,),
                                 PROL_DROITE = 'CONSTANT',
                                 PROL_GAUCHE = 'CONSTANT',)

        #
        # DEFINITION DU CHAMP NEUTRONIQUE RADIAL (CONSTANT)        #
        #
        Y_1 = -1.0
        Y_2 = 1.0

        _FLY_1 = DEFI_FONCTION(
            NOM_PARA='Y', VALE=(Y_1, 1.0, Y_2, 1.0), PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)

        _FLY_2 = DEFI_FONCTION(NOM_PARA='Y', VALE=(
            Y_1, 1.0, Y_2, 1.0), PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)

        _FLUXRD1 = DEFI_NAPPE(NOM_PARA='Z', PARA=(Y_1, Y_2), FONCTION=(
            _FLY_1, _FLY_2), PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)

        #------------------------------------------------
        # CREATION DU CHAMP ASSOCIE A LA FONCTION FLUXAX1
        #------------------------------------------------
        _CH_FAX = CREA_CHAMP(
            OPERATION='AFFE', TYPE_CHAM='NOEU_NEUT_F', MAILLAGE=MAILLAGE,
            AFFE=(_F(GROUP_MA=('T_GUIDE', 'CRAYON', 'ELA', 'MAINTIEN',), NOM_CMP='X1', VALE_F=_FLUXAX1,),),)

        _CH_FAXR = CREA_CHAMP(
            OPERATION='EVAL', TYPE_CHAM='NOEU_NEUT_R', CHAM_F=_CH_FAX, CHAM_PARA=_CHXN)

        #-----------------------------------------------
        # CREATION DU CHAMP ASSOCIE A LA FONCTION FLUXRD1
        #-----------------------------------------------
        _CH_FRD = CREA_CHAMP(
            OPERATION='AFFE', TYPE_CHAM='NOEU_NEUT_F', MAILLAGE=MAILLAGE,
            AFFE=(_F(TOUT='OUI', NOM_CMP='X2', VALE_F=_FLUXRD1),),)

        _CH_FRDR = CREA_CHAMP(
            OPERATION='EVAL', TYPE_CHAM='NOEU_NEUT_R', CHAM_F=_CH_FRD, CHAM_PARA=_CHXN)

        _MULT = FORMULE(
            NOM_PARA=('X1', 'X2', 'INST'), VALE='X1*X2*INST')

        _CHRES = CREA_CHAMP(
            OPERATION='AFFE', TYPE_CHAM='NOEU_NEUT_F', MAILLAGE=MAILLAGE,
            AFFE=(_F(GROUP_MA=('T_GUIDE', 'CRAYON', 'ELA', 'MAINTIEN',), NOM_CMP='X1', VALE_F=_MULT),),)

        #-----------------------------------------------------
        # CREATION DU CHAMP FLUENC1 ASSOCIE A LA LISTE LINST
        #-----------------------------------------------------

        _INST_0 = CREA_CHAMP(
            OPERATION='AFFE', TYPE_CHAM='NOEU_INST_R', MAILLAGE=MAILLAGE,
            AFFE=(_F(GROUP_MA=('T_GUIDE', 'CRAYON', 'ELA', 'MAINTIEN',), NOM_CMP='INST', VALE=0.0),),)

        _REST_0 = CREA_CHAMP(
            OPERATION='EVAL', TYPE_CHAM='NOEU_NEUT_R', CHAM_F=_CHRES,
            CHAM_PARA=(_CH_FAXR, _CH_FRDR, _INST_0,))

        _RES_0 = CREA_CHAMP(
            OPERATION='ASSE', TYPE_CHAM='NOEU_IRRA_R', MAILLAGE=MAILLAGE,
            ASSE=(_F(GROUP_MA=('T_GUIDE', 'CRAYON', 'ELA', 'MAINTIEN',), CHAM_GD=_REST_0, NOM_CMP='X1', NOM_CMP_RESU='IRRA',),),)

        _INST_1 = CREA_CHAMP(
            OPERATION='AFFE', TYPE_CHAM='NOEU_INST_R', MAILLAGE=MAILLAGE,
            AFFE=(_F(GROUP_MA=('T_GUIDE', 'CRAYON', 'ELA', 'MAINTIEN',), NOM_CMP='INST', VALE=fluence),),)

        _REST_1 = CREA_CHAMP(
            OPERATION='EVAL', TYPE_CHAM='NOEU_NEUT_R', CHAM_F=_CHRES,
            CHAM_PARA=(_CH_FAXR, _CH_FRDR, _INST_1,))

        _RES_1 = CREA_CHAMP(
            OPERATION='ASSE', TYPE_CHAM='NOEU_IRRA_R', MAILLAGE=MAILLAGE,
            ASSE=(_F(GROUP_MA=('T_GUIDE', 'CRAYON', 'ELA', 'MAINTIEN',), CHAM_GD=_REST_1, NOM_CMP='X1', NOM_CMP_RESU='IRRA',),),)

        _FLUENC = CREA_RESU(
            TYPE_RESU='EVOL_VARC', NOM_CHAM='IRRA', OPERATION='AFFE',
            AFFE=(
                _F(CHAM_GD=_RES_0, INST=self.temps_simu[
                   'T0'], PRECISION=1.E-6),
                _F(CHAM_GD=_RES_0, INST=self.temps_simu[
                   'T4'], PRECISION=1.E-6),
                _F(CHAM_GD=_RES_1, INST=self.temps_simu[
                   'T5'], PRECISION=1.E-6),
                _F(CHAM_GD=_RES_1, INST=self.temps_simu['T9'], PRECISION=1.E-6),),)

        return _FLUENC

    def definition_champ_temperature(self, MAILLAGE):
        """Return the time evolution of the field of temperature"""
        assert self.temps_simu[
            'T0'] is not None, '`definition_time` must be called first!'
        from Accas import _F
        CREA_CHAMP = self.macro.get_cmd('CREA_CHAMP')
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        CREA_RESU = self.macro.get_cmd('CREA_RESU')

        #
        # Temperatures utiles pour les calculs sous flux neutronique #
        #
        # Temperature de reference #
        #
        # TP_REF   =
        # ARRET_FR =  arret a froid (temp moyenne cuve)
        # ARRET_CH =  arret a chaud (297.2 dans doc TF JD DC 1494)
                    # c est une temperature moyenne en cuve

        # profil lineaire de temperature pour les TG
        # TP_TG1 = temperature TG pour xinft
        # TP_TG2 = temperature TG pour xsupt

        #
        # DEFINITION DES TEMPERATURES NODALES EVOLUTIVES   #
        #
        # TEMPERATURE DE REFERENCE (A L'ARRET)         #
        #

        _F_TP1_1 = DEFI_FONCTION(
            NOM_PARA='X', NOM_RESU='TEMP', PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
            VALE=(self.XINFT, self.TP_REF, self.XSUPT, self.TP_REF),)

        #
        # AFFECTATION DE REFENCE DU CHAMP DE TEMPERATURE  #
        # D UN AC (A l'ARRET)                 #
        #

        _CHTEM11 = CREA_CHAMP(
            TYPE_CHAM='NOEU_TEMP_F', MAILLAGE=MAILLAGE, OPERATION='AFFE',
            AFFE=(_F(GROUP_NO=('T_GUIDE', 'EBOSUP', 'EBOINF', 'CRAYON', 'ELA', 'DIL', 'MAINTIEN',), NOM_CMP='TEMP', VALE_F=_F_TP1_1,),),)

        #
        # TEMPERATURE EN PHASE ARRET A FROID           #
        #

        _F_TP2_1 = DEFI_FONCTION(
            NOM_PARA='X', NOM_RESU='TEMP', PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
            VALE=(self.XINFT, self.ARRET_FR, self.XSUPT, self.ARRET_FR),)

        #
        # AFFECTATION DE REFENCE DU CHAMP DE TEMPERATURE  #
        # D UN AC PENDANT LA PHASE D'ARRET A FROID             #
        #

        _CHTEM21 = CREA_CHAMP(
            TYPE_CHAM='NOEU_TEMP_F', MAILLAGE=MAILLAGE, OPERATION='AFFE',
            AFFE=(_F(GROUP_NO=('T_GUIDE', 'EBOSUP', 'EBOINF', 'CRAYON', 'ELA', 'DIL', 'MAINTIEN',), NOM_CMP='TEMP', VALE_F=_F_TP2_1,),),)

        #
        # TEMPERATURE EN PHASE ARRET A CHAUD           #
        #

        _F_TP3_1 = DEFI_FONCTION(
            NOM_PARA='X', NOM_RESU='TEMP', PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
            VALE=(self.XINFT, self.ARRET_CH, self.XSUPT, self.ARRET_CH),)

        #
        # AFFECTATION DE REFENCE DU CHAMP DE TEMPERATURE  #
        # D UN AC PENDANT LA PHASE D'ARRET A CHAUD             #
        #

        _CHTEM31 = CREA_CHAMP(
            TYPE_CHAM='NOEU_TEMP_F', MAILLAGE=MAILLAGE, OPERATION='AFFE',
            AFFE=(_F(GROUP_NO=('T_GUIDE', 'EBOSUP', 'EBOINF', 'CRAYON', 'ELA', 'DIL', 'MAINTIEN',), NOM_CMP='TEMP', VALE_F=_F_TP3_1,),),)

        #
        # EVOLUTION DE LA TEMPERATURE DANS LES CRAYONS     #
        # PENDANT LA PHASE D'IRRADIATION             #
        #

        XX1 = self.XINFC
        XX2 = XX1 + self.LONCR * self.SXX2
        XX3 = XX1 + self.LONCR * self.SXX3
        XX4 = XX1 + self.LONCR

        _F_CR3 = DEFI_FONCTION(
            NOM_PARA='X', NOM_RESU='TEMP', PROL_DROITE='LINEAIRE', PROL_GAUCHE='LINEAIRE',
            VALE=(XX1, self.TXX1, XX2, self.TXX2, XX3, self.TXX3, XX4, self.TXX4),)

        #
        # EVOLUTION DE LA TEMPERATURE DANS LES TUBES-GUIDE #
        # ET AUTRES COMPOSANTS EN PHASE D'IRRADIATION      #
        #

        _F_TP4_1 = DEFI_FONCTION(
            NOM_PARA='X', NOM_RESU='TEMP', PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
            VALE=(self.XINFT, self.TP_TG1, self.XSUPT, self.TP_TG2),)

        _CHTEM41 = CREA_CHAMP(
            TYPE_CHAM='NOEU_TEMP_F', MAILLAGE=MAILLAGE, OPERATION='AFFE',
            AFFE=(
                _F(GROUP_NO=('T_GUIDE', 'EBOSUP', 'EBOINF', 'ELA', 'DIL', 'MAINTIEN',),
                   NOM_CMP='TEMP', VALE_F=_F_TP4_1,),
                _F(GROUP_NO='CRAYON',                                  NOM_CMP='TEMP', VALE_F=_F_CR3,),),)

        _CHTH_1 = CREA_RESU(
            TYPE_RESU='EVOL_THER', NOM_CHAM='TEMP', OPERATION='AFFE',
            AFFE=(
                _F(CHAM_GD=_CHTEM11, INST=self.temps_simu[
                   'T0'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM11, INST=self.temps_simu[
                   'T1'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM21, INST=self.temps_simu[
                   'T2'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM31, INST=self.temps_simu[
                   'T3'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM41, INST=self.temps_simu[
                   'T4'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM41, INST=self.temps_simu[
                   'T5'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM31, INST=self.temps_simu[
                   'T6'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM21, INST=self.temps_simu[
                   'T7'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM11, INST=self.temps_simu[
                   'T8'], PRECISION=1.E-6),
                _F(CHAM_GD=_CHTEM11, INST=self.temps_simu['T9'], PRECISION=1.E-6),),)

        return _CHTH_1

    def definition_materiau(self, MAILLAGE, GFF, FLUENCE, CHTH, CONTACT='NON'):
        from Accas import _F
        DEFI_COMPOR = self.macro.get_cmd('DEFI_COMPOR')
        DEFI_MATERIAU = self.macro.get_cmd('DEFI_MATERIAU')
        AFFE_MATERIAU = self.macro.get_cmd('AFFE_MATERIAU')

        # TP_REF = 20. ;

        if CONTACT == 'OUI':
            _M_RES = DEFI_MATERIAU(DIS_CONTACT=_F(RIGI_NOR=1.E9))
        else:
            _M_RES = DEFI_MATERIAU(DIS_CONTACT=_F(RIGI_NOR=1.E1))

        # Affectation des materiau dans le coeur
        _A_MAT = AFFE_MATERIAU(MAILLAGE=MAILLAGE,
                               AFFE_VARC=(_F(NOM_VARC='IRRA',
                                             TOUT='OUI',
                                             EVOL=FLUENCE,
                                             PROL_DROITE='CONSTANT'),
                                          _F(NOM_VARC='TEMP',
                                             TOUT='OUI',
                                             EVOL=CHTH,
                                             PROL_DROITE='CONSTANT',
                                             VALE_REF=self.TP_REF)),
                               AFFE=self.mcf_coeur_mater(_M_RES),
                               AFFE_COMPOR=self.mcf_compor_fibre(GFF))
        return _A_MAT

    def mcf_compor_fibre(self, GFF):
        from Accas import _F
        DEFI_COMPOR = self.macro.get_cmd('DEFI_COMPOR')
        mcf = []
        for ac in self.collAC.values():
            _CMPC = DEFI_COMPOR(GEOM_FIBRE=GFF,
                                MATER_SECT=ac.mate.mate['CR'],
                                MULTIFIBRE=_F(
                                GROUP_FIBRE='CR_' + ac.idAST,
                                MATER=ac.mate.mate[
                                'CR'],
                                RELATION='GRAN_IRRA_LOG',),)
            _CMPT = DEFI_COMPOR(GEOM_FIBRE=GFF,
                                MATER_SECT=ac.mate.mate['TG'],
                                MULTIFIBRE=_F(
                                GROUP_FIBRE=(
                                'LG_' + ac.idAST, 'BI_' + ac.idAST, 'RE_' + ac.idAST,),
                                MATER=ac.mate.mate[
                                'TG'],
                                RELATION='GRAN_IRRA_LOG',),)
            mtmp = (_F(GROUP_MA='CR_' + ac.idAST, COMPOR=_CMPC,),
                    _F(GROUP_MA='TG_' + ac.idAST, COMPOR=_CMPT,),)
            mcf.extend(mtmp)

        return mcf

    def mcf_coeur_mater(self, _M_RES):
        from Accas import _F
        DEFI_MATERIAU = self.macro.get_cmd('DEFI_MATERIAU')
        # Definition d'un materiau bidon pour les elements de poutres
        _MAT_BID = DEFI_MATERIAU(
            ELAS=_F(E=1.0,   NU=0.0, RHO=0.0, ALPHA=0.0,),)
        _MAT_GR = DEFI_MATERIAU(
            ELAS=_F(E=1.E14, NU=0.3, RHO=0.0, ALPHA=0.0,),)

        mcf = []
        mtmp = (_F(GROUP_MA='RES_TOT', MATER=_M_RES,),)
        mcf.extend(mtmp)

        for ac in self.collAC.values():
            mcf.extend(ac.mcf_AC_mater())
            mtmp = (
                _F(GROUP_MA=('GT_' + ac.idAST + '_M',
                   'GT_' + ac.idAST + '_E',), MATER=_MAT_BID,),
                _F(GROUP_MA='GR_' + ac.idAST,                                  MATER=_MAT_GR,),)
            mcf.extend(mtmp)
            # ATTENTION ici on definit pour tout le group_ma 'DIL' le materiau de type ac.collAC.
            # Cette affectation concerne le calcul avec dilatation thermique des grilles et de la cuve.
            # C'est donc le dernier qui sera pris en compte car aujourd'hui on considere que l'ensemble
            # se dilate de la meme facon.
            # On repete en ecrasant a chaque fois avec la meme valeur pour tout
            # le groupe DIL
        mtmp = (_F(GROUP_MA='DIL', MATER=ac.mate.mate['DIL'],),)
        mcf.extend(mtmp)
        return mcf

    def dilatation_cuve(self, MODEL, MAILL,is_char_ini=False):
        """Retourne les déplacements imposés aux noeuds modélisant les internes de cuves
        (supports inférieur (PIC ou FSC), supérieur (PSC) et cloisons)
        et traduisant les dilatations thermiques des internes et leurs deformations de natures mecaniques"""
        # XXX trop long pour être lisible, création des formules fragile
        assert self.temps_simu[
            'T0'] is not None, '`definition_time` must be called first!'
        from Accas import _F
        DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
        FORMULE = self.macro.get_cmd('FORMULE')
        AFFE_CHAR_MECA_F = self.macro.get_cmd('AFFE_CHAR_MECA_F')
        RECU_TABLE = self.macro.get_cmd('RECU_TABLE')

        # definition des evolutions de températures
        # sur la PIC/FSC, la PSC et l'enveloppe
        _TEMPPIC = DEFI_FONCTION(NOM_PARA='INST',
                                 NOM_RESU='TEMP',
                                 VALE=(-2.0,   self.TP_REF,
                                       -1.0,   self.TP_REF,
                                       self.temps_simu['T0'],   self.TP_REF,
                                       self.temps_simu['T1'],   self.TP_REF,
                                       self.temps_simu['T2'],   self.ARRET_FR,
                                       self.temps_simu['T3'],   self.ARRET_CH,
                                       self.temps_simu['T4'],   self.TINFCUVE,
                                       self.temps_simu['T5'],   self.TINFCUVE,
                                       self.temps_simu['T6'],   self.ARRET_CH,
                                       self.temps_simu['T7'],   self.ARRET_FR,
                                       self.temps_simu['T8'],   self.TP_REF,
                                       self.temps_simu['T9'],   self.TP_REF,),
                                 PROL_DROITE='CONSTANT',
                                 PROL_GAUCHE='CONSTANT',)

        _TEMPPSC = DEFI_FONCTION(NOM_PARA='INST',
                                 NOM_RESU='TEMP',
                                 VALE=(-2.0,   self.TP_REF,
                                       -1.0,   self.TP_REF,
                                       self.temps_simu['T0'],   self.TP_REF,
                                       self.temps_simu['T1'],   self.TP_REF,
                                       self.temps_simu['T2'],   self.ARRET_FR,
                                       self.temps_simu['T3'],   self.ARRET_CH,
                                       self.temps_simu['T4'],   self.TSUPCUVE,
                                       self.temps_simu['T5'],   self.TSUPCUVE,
                                       self.temps_simu['T6'],   self.ARRET_CH,
                                       self.temps_simu['T7'],   self.ARRET_FR,
                                       self.temps_simu['T8'],   self.TP_REF,
                                       self.temps_simu['T9'],   self.TP_REF,),
                                 PROL_DROITE='CONSTANT',
                                 PROL_GAUCHE='CONSTANT',)

        _TEMPENV = DEFI_FONCTION(NOM_PARA='INST',
                                 NOM_RESU='TEMP',
                                 VALE=(-2.0,   self.TP_REF,
                                       -1.0,   self.TP_REF,
                                       self.temps_simu['T0'],   self.TP_REF,
                                       self.temps_simu['T1'],   self.TP_REF,
                                       self.temps_simu['T2'],   self.ARRET_FR,
                                       self.temps_simu['T3'],   self.ARRET_CH,
                                       self.temps_simu['T4'],   self.TENVELOP,
                                       self.temps_simu['T5'],   self.TENVELOP,
                                       self.temps_simu['T6'],   self.ARRET_CH,
                                       self.temps_simu['T7'],   self.ARRET_FR,
                                       self.temps_simu['T8'],   self.TP_REF,
                                       self.temps_simu['T9'],   self.TP_REF,),
                                 PROL_DROITE='CONSTANT',
                                 PROL_GAUCHE='CONSTANT',)

        TP_REFlocal = self.TP_REF

        # interpolation linéaire du coefficient de dilatation
        # des internes de cuve en fonction de la température
        ALPH1local = self.ALPH1
        ALPH2local = self.ALPH2
        ALPHENV = '(%(ALPH1local)e*' + \
            _TEMPENV.nom + '(INST) + %(ALPH2local)e)'
        ALPHPIC = '(%(ALPH1local)e*' + \
            _TEMPPIC.nom + '(INST) + %(ALPH2local)e)'
        ALPHPSC = '(%(ALPH1local)e*' + \
            _TEMPPSC.nom + '(INST) + %(ALPH2local)e)'

        # Donnees geometriques
        # coordonnees centre cuve
        _TABG = RECU_TABLE(CO=MAILL,
                           NOM_TABLE='CARA_GEOM',)
        xmin = _TABG['X_MIN', 1]
        xmax = _TABG['X_MAX', 1]
        ymin = _TABG['Y_MIN', 1]
        ymax = _TABG['Y_MAX', 1]
        zmin = _TABG['Z_MIN', 1]
        zmax = _TABG['Z_MAX', 1]
        Y0 = (ymin + ymax) / 2.
        Z0 = (zmin + zmax) / 2.
        # rayon de la PSC
        Rpsc = (ymax - ymin) / 2.

        #---------------------------------------------------------------
        #--                  Dilatations radiales                     --
        #--      du cloisonnement, de la PIC/FSC, et de la PSC        --
        #---------------------------------------------------------------
        L = '(sqrt( ((Y-%(Y0)f)**2)+ ((Z-%(Z0)f)**2)))'
        epsilon = 1.E-6
        # on rentre un epsilon pour le cas où L=0 (assemblage central)
        # pour éviter la division par zéro
        COSTE = '(Y-%(Y0)f)/(' + L + '+%(epsilon)e)'
        SINTE = '(Z-%(Z0)f)/(' + L + '+%(epsilon)e)'
        Dcth = L + ' * ' + ALPHENV + \
            ' * (' + _TEMPENV.nom + '(INST)-%(TP_REFlocal)f) '
        f_DthY = Dcth + '*' + COSTE
        f_DthZ = Dcth + '*' + SINTE
        _DthY = FORMULE(
            NOM_PARA=('X', 'Y', 'Z', 'INST'), VALE=f_DthY % locals())
        _DthZ = FORMULE(
            NOM_PARA=('X', 'Y', 'Z', 'INST'), VALE=f_DthZ % locals())

        Dthpic = L + ' * ' + ALPHPIC + \
            ' * (' + _TEMPPIC.nom + '(INST)-%(TP_REFlocal)f) '
        f_DthYpic = Dthpic + '*' + COSTE
        f_DthZpic = Dthpic + '*' + SINTE
        _DthYpic = FORMULE(
            NOM_PARA=('X', 'Y', 'Z', 'INST'), VALE=f_DthYpic % locals())
        _DthZpic = FORMULE(
            NOM_PARA=('X', 'Y', 'Z', 'INST'), VALE=f_DthZpic % locals())

        Dthpsc = L + ' * ' + ALPHPSC + \
            ' * (' + _TEMPPSC.nom + '(INST)-%(TP_REFlocal)f) '
        f_DthYpsc = Dthpsc + '*' + COSTE
        f_DthZpsc = Dthpsc + '*' + SINTE
        _DthYpsc = FORMULE(
            NOM_PARA=('X', 'Y', 'Z', 'INST'), VALE=f_DthYpsc % locals())
        _DthZpsc = FORMULE(
            NOM_PARA=('X', 'Y', 'Z', 'INST'), VALE=f_DthZpsc % locals())

        #---------------------------------------------------------------
        #--                  Deplacements verticaux                   --
        #--                      de la PIC/FSC                        --
        #---------------------------------------------------------------
        # le déplacement de la PIC est égal à la différence de hauteur de cavité
        # (entre l'instant "cuve fermée à 20C"et l'instant considéré)
        #
        # centre du coeur
        _DthXpicCentre = DEFI_FONCTION(NOM_PARA='INST',
                                       VALE=(-2.0,   0.,
                                             -1.0,   0.,
                                             self.temps_simu['T0'],   0.,
                                             self.temps_simu['T0b'],   0.,
                                             self.temps_simu[
                                             'T1'],   self.Hcav1centre - self.Hcav1centre,
                                             self.temps_simu[
                                             'T2'],   self.Hcav1centre - self.Hcav2centre,
                                             self.temps_simu[
                                             'T3'],   self.Hcav1centre - self.Hcav3centre,
                                             self.temps_simu[
                                             'T4'],   self.Hcav1centre - self.Hcav4centre,
                                             self.temps_simu[
                                             'T5'],   self.Hcav1centre - self.Hcav4centre,
                                             self.temps_simu[
                                             'T6'],   self.Hcav1centre - self.Hcav3centre,
                                             self.temps_simu[
                                             'T7'],   self.Hcav1centre - self.Hcav2centre,
                                             self.temps_simu[
                                             'T8'],   self.Hcav1centre - self.Hcav1centre,
                                             self.temps_simu['T9'],   0.,),
                                       PROL_DROITE='CONSTANT',
                                       PROL_GAUCHE='CONSTANT',)
        # peripherie du coeur
        _DthXpicPeriph = DEFI_FONCTION(NOM_PARA='INST',
                                       VALE=(-2.0,   0.,
                                             -1.0,   0.,
                                             self.temps_simu['T0'],   0.,
                                             self.temps_simu['T0b'],   0.,
                                             self.temps_simu[
                                             'T1'],   self.Hcav1periph - self.Hcav1periph,
                                             self.temps_simu[
                                             'T2'],   self.Hcav1periph - self.Hcav2periph,
                                             self.temps_simu[
                                             'T3'],   self.Hcav1periph - self.Hcav3periph,
                                             self.temps_simu[
                                             'T4'],   self.Hcav1periph - self.Hcav4periph,
                                             self.temps_simu[
                                             'T5'],   self.Hcav1periph - self.Hcav4periph,
                                             self.temps_simu[
                                             'T6'],   self.Hcav1periph - self.Hcav3periph,
                                             self.temps_simu[
                                             'T7'],   self.Hcav1periph - self.Hcav2periph,
                                             self.temps_simu[
                                             'T8'],   self.Hcav1periph - self.Hcav1periph,
                                             self.temps_simu['T9'],   0.,),
                                       PROL_DROITE='CONSTANT',
                                       PROL_GAUCHE='CONSTANT',)

        f_DthXpic = '( (' + _DthXpicPeriph.nom + '(INST) -'  + _DthXpicCentre.nom + \
            '(INST) ) /(%(Rpsc)f)**2   )*(' +  L + \
            ')**2   +' + _DthXpicCentre.nom + '(INST)'
        _DthXpic = FORMULE(
            NOM_PARA=('X', 'Y', 'Z', 'INST'), VALE=f_DthXpic % locals())

        #---------------------------------------------------------------
        #--                Deplacements  verticaux                    --
        #--               des noeuds du cloisonnement                 --
        #---------------------------------------------------------------
        XINFCUVElocal = self.XINFCUVE
        XSUPCUVElocal = self.XSUPCUVE
        f_DthX = '(-1.*'  + _DthXpicPeriph.nom + \
            '(INST)/(%(XSUPCUVElocal)f-%(XINFCUVElocal)f) * X  +'  + \
            _DthXpicPeriph.nom + '(INST))'
        _DthX = FORMULE(NOM_PARA=('X', 'INST'), VALE=f_DthX % locals())

        #---------------------------------------------------------------
        #--                  chargement resultant                     --
        #---------------------------------------------------------------
        if (is_char_ini) :
            _dilatation = AFFE_CHAR_MECA_F( MODELE   = MODEL,
                                           DDL_IMPO = (_F(GROUP_NO = 'FIX', 
                                                          DX=_DthXpic,),
                                                       _F(GROUP_NO = 'P_CUV',  
                                                          DX=_DthX,   ),),)
        else :
            _dilatation = AFFE_CHAR_MECA_F(MODELE=MODEL,
                                       DDL_IMPO=(_F(GROUP_NO='FIX',
                                                    DX=_DthXpic,
                                                    DY=_DthYpic,
                                                    DZ=_DthZpic),
                                                 _F(GROUP_NO='PMNT_S',
                                                    DY=_DthYpsc,
                                                    DZ=_DthZpsc,),
                                                 _F(GROUP_NO='P_CUV',
                                                    DX=_DthX,
                                                    DY=_DthY,
                                                    DZ=_DthZ),),)
        return _dilatation


class CoeurFactory(Mac3Factory):

    """Classe pour construire les objets Coeur."""
    # Ex.: La classe "Coeur" sera nommée Coeur_900 dans le fichier
    # Coeur_900.datg
    prefix = 'Coeur_'

    def build_supported_types(self):
        """Construit la liste des types autorisés."""
        ctxt = {}
        for obj, val in globals().items():
            if type(val) is type and issubclass(val, Coeur):
                ctxt[obj] = val
        return ctxt


class MateriauAC(object):

    """Conteneur des matériaux d'un assemblage."""
    _types = ('DIL', 'MNT', 'ES', 'EI', 'CR', 'TG', 'GC_ME', 'GC_EB', 'GC_EH')

    def __init__(self, typeAC, macro):
        """Initialisation"""
        self.typeAC = typeAC
        self.macro = macro
        self.mate = {}.fromkeys(self._types)
        self.include_materiau()

    def include_materiau(self):
        """Crée les matériaux"""
        INCLUDE_MATERIAU = self.macro.get_cmd("INCLUDE_MATERIAU")

        for typ in self._types:
            _mat = INCLUDE_MATERIAU(NOM_AFNOR=self.typeAC + '_' + typ,
                                    TYPE_MODELE='REF',
                                    VARIANTE='A',
                                    TYPE_VALE='NOMI')
            self.mate[typ] = _mat
