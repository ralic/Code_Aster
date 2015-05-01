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
# person_in_charge: mathieu.courtois at edf.fr

"""
------------------------------------------------------------------------
              Module catalogue de loi de comportement.
------------------------------------------------------------------------

   Contenu du module :

   - CataLoiComportement : le catalogue
      Méthodes : add, get, create, get_info, get_vari, query, get_algo

   - LoiComportement : l'objet loi de comportement
      Définition, stockage et récupération des valeurs

   - KIT : permet l'assemblage de loi de comportement


Interfaces Fortran/Python :

   Fonctionnement dans NMDORC : exemple VMIS_ISOT_TRAC + DEBORST
      1. on construit le comportement rencontré
         CALL LCCREE(NBKIT, LKIT, COMPOR)
         ==> comport = catalc.create(*list_kit)

      2. récupération du numéro de routine et le nbre de variables internes
         CALL LCINFO(COMPOR, NUMLC, NBVARI)
         ==> num_lc, nb_vari = catalc.get_info(COMPOR)

      3. récupère la liste des variables internes
         CALL LCVARI(COMPOR, NBVARI, LVARI)
         ==> nom_vari = catalc.get_vari(COMPOR)

      4. est-ce que VALEUR est un valeur autorisée de PROPRIETE ?
         CALL LCTEST(COMPOR, PROPRIETE, VALEUR, IRET)
         ==> iret = catalc.query(COMPOR, PROPRIETE, VALEUR)

      5. récupère le nom du 1er algorithme d'integration
         CALL LCALGO(COMPOR, ALGO)
         ==> algo_inte = catalc.get_algo(COMPOR)

------------------------------------------------------------------------

"""

import os

from Noyau.N_types import force_tuple
from Noyau.N_utils import Singleton

from Execution.strfunc import convert, ufmt
from cata_vari import DICT_NOM_VARI


class CataComportementError(Exception):

    """Error"""

    def __init__(self, msg):
        self.msg = msg

    def __str__(self):
        return convert(self.msg)

# utilitaires


def force_to_tuple(value):
    """Retourne systématiquement un tuple (vide si value==None)."""
    if value is None:
        return tuple()
    return force_tuple(value)


def check_type(typ, value, accept_None=False):
    """Vérification de type"""
    l_typ = force_to_tuple(typ)
    l_val = force_to_tuple(value)
    for val in l_val:
        if val is None and accept_None:
            pass
        elif type(val) not in l_typ:
            msg = ufmt(u"%r n'est pas d'un type autorisé : %r", val, l_typ)
            raise CataComportementError(msg)


def union(args):
    """Union de N séquences."""
    res = []
    for seq in args:
        if seq is None:
            continue
        assert type(seq) in (list, tuple)
        res.extend(seq)
    return tuple(res)


def intersection(args):
    """Intersection de N séquences."""
    args = [i for i in args if i is not None]
    if len(args) < 1:
        return tuple()
    res = list(args[0])
    for seq in args[1:]:
        if seq in (None, ()):
            continue
        assert type(seq) in (list, tuple), seq
        d = {}.fromkeys(res, 1)
        res = [elt for elt in seq if d.get(elt)]
    return tuple(res)


def first(args):
    """Retourne la 1ère valeur."""
    return args[0]


class Base(object):

    """Classe de base : partie commune loi de comportement/kit."""

    __properties__ = ('deformation', 'mc_mater', 'modelisation', 'nb_vari',
                      'nom_varc', 'nom_vari', 'proprietes', 'algo_inte',
                      'type_matr_tang')

    def copy(self):
        """Copie d'un objet LoiComportement"""
        import copy
        return copy.copy(self)

    def gen_property(prop, typ, comment):
        """Génère la propriété 'prop' avec ses fonctions get, set, del"""
        def getfunc(comport):
            return getattr(comport, '_' + prop)

        def setfunc(comport, value):
            check_type(typ, value)
            setattr(comport, '_' + prop, force_to_tuple(value))

        def delfunc(comport):
            setattr(comport, '_' + prop, None)

        return property(getfunc, setfunc, delfunc, comment)
    gen_property = staticmethod(gen_property)

    def gen_getfunc(oper, prop):
        """Génère la fonction get de la propriété 'prop'.
        'oper' est une fonction qui prend N valeurs de 'prop' et en retourne une"""
        def func(kit):
            return oper([getattr(comport, prop) for comport in kit.list_comport])
        return func
    gen_getfunc = staticmethod(gen_getfunc)

    def dict_info(self):
        dico = {
            'nom': self.nom,
            'num_lc': self.num_lc,
            'doc': self.doc,
        }
        for attr in self.__properties__:
            dico[attr] = getattr(self, attr)
        return dico

    def short_repr(self):
        return "<%s(%x,%r)>" % (self.__class__.__name__, id(self), self.nom)

    def long_repr(self):
        template = """Loi de comportement : %(nom)s
'''%(doc)s'''
   routine                    : %(num_lc)r
   nb de variables internes   : %(nb_vari)r
   nom des variables internes : %(nom_vari)r
   modélisations disponibles  : %(modelisation)r
   types de déformations      : %(deformation)r
   mots-clés du matériau      : %(mc_mater)r
   variables de commandes     : %(nom_varc)r
   schémas d'intégration      : %(algo_inte)r
   type de matrice tangente   : %(type_matr_tang)r
   propriétés supplémentaires : %(proprietes)r
"""
        return template % self.dict_info()

    def __repr__(self):
        """Représentation par défaut"""
        return self.long_repr()

    @property
    def ldctype(self):
        """Return the class type"""
        return self._ldctype

    def getPropertiesNames(self):
        """Return the names of the properties"""
        return self.__properties__


class LoiComportement(Base):

    """Définition d'une loi de comportement.

    nom_vari, proprietes : definition, affectation de valeurs
    mc_mater : besoin de regles ENSEMBLE, AU_MOINS_UN, UN_PARMI
    modelisation, deformation, nom_varc, algo_inte, type_matr_tang : listes des
    valeurs acceptees"""
    _ldctype = 'std'

    def __init__(self, nom, num_lc, doc='', **kwargs):
        """Initialisations"""
        self.nom = nom
        self.num_lc = num_lc
        self.doc = doc
        for prop in self.__properties__:
            setattr(self, '_' + prop, None)
        # propriétés fournies
        for prop in [k for k in kwargs.keys() if k in self.__properties__]:
            setattr(self, prop, kwargs.get(prop))

    def get_nb_vari(self):
        return self._nb_vari or 0

    def set_nb_vari(self, value):
        check_type((long, int), value)
        self._nb_vari = value
        self.check_vari()

    def del_nb_vari(self):
        self._nb_vari = None

    nb_vari = property(get_nb_vari, set_nb_vari,
                       del_nb_vari, "Nombre de variables internes")

    def get_nom_vari(self):
        return self._nom_vari

    def set_nom_vari(self, value):
        check_type((str, unicode), value)
        self._nom_vari = force_to_tuple(value)
        self.check_vari()

    def del_nom_vari(self):
        self._nom_vari = None

    nom_vari = property(get_nom_vari, set_nom_vari,
                        del_nom_vari, "Noms des variables internes")

# définition des propriétés simples (juste vérification de type à
# l'affectation)
    mc_mater = Base.gen_property(
        'mc_mater',       (str, unicode), "Mots-clés matériaux")
    modelisation = Base.gen_property(
        'modelisation',   (str, unicode), "Modélisations")
    deformation = Base.gen_property(
        'deformation',    (str, unicode), "Types de déformation")
    nom_varc = Base.gen_property(
        'nom_varc',       (str, unicode), "Noms des variables de commandes")
    algo_inte = Base.gen_property(
        'algo_inte',      (str, unicode), "Schéma d'intégration")
    type_matr_tang = Base.gen_property(
        'type_matr_tang', (str, unicode), "Type de matrice tangente")
    proprietes = Base.gen_property(
        'proprietes',     (str, unicode), "Propriétés")

    def check_vari(self):
        """Vérifie la cohérence de la définition des variables internes"""
        if self._nb_vari is not None and self._nom_vari is not None \
           and self._nb_vari != len(self._nom_vari):
            print self
            msg = ufmt(u"Nombre de variables internes = %d, "
                       u"incohérent avec la liste des variables internes %s",
                       self._nb_vari, self._nom_vari)
            raise CataComportementError(msg)
        if self._nom_vari:
            err = set(self._nom_vari).difference(DICT_NOM_VARI.keys())
            if len(err) > 0:
                strerr = ', '.join(err)
                msg = ufmt(u"Comportement '%s', nom de variables internes non "
                           u"autorisés : %s", self.nom, strerr)
                raise CataComportementError(msg)


class LoiComportementMFront(LoiComportement):

    """Définition d'une loi de comportement MFront.

    symbol_mfront : nom de la fonction dans la bibliothèque MFront
    modelisation, deformation, algo_inte : listes des valeurs acceptees
    """
    _ldctype = 'mfront'
    __properties__ = tuple(list(LoiComportement.__properties__) +
                           ['symbol_mfront'])

    def __init__(self, nom, symbol_mfront, doc='', **kwargs):
        """Initialisations"""
        # fixes les valeurs sans objet pour MFront (elles ne peuvent donc
        # pas être fournies dans kwargs)
        super(LoiComportementMFront, self).__init__(
            nom,
            symbol_mfront=symbol_mfront,
            num_lc=58,
            mc_mater=nom,
            nb_vari=0,
            nom_vari=None,
            nom_varc=None,
            type_matr_tang=None,
            doc=doc,
            **kwargs)

    symbol_mfront = Base.gen_property(
        'symbol_mfront', (str, unicode), "Fonction MFront")

    def long_repr(self):
        template = """Loi de comportement : %(nom)s
'''%(doc)s'''
   routine                    : %(num_lc)r
   modélisations disponibles  : %(modelisation)r
   types de déformations      : %(deformation)r
   schémas d'intégration      : %(algo_inte)r
   nom du symbole             : %(symbol_mfront)r
"""
        return template % self.dict_info()


class KIT(Base):

    """Définit un assemblage de loi de comportement par KIT par un 'nom' et une
    liste de comportements"""
    _ldctype = 'kit'

    def __init__(self, nom, *list_comport):
        """Initialisations"""
        if not type(nom) in (str, unicode):
            raise CataComportementError(
                u"'KIT' : argument 1 (nom) de type invalide")
        self.nom = nom
        self.list_comport = [comport.copy() for comport in list_comport]
        self.list_nom = [comport.nom for comport in self.list_comport]
        txt = ['Assemblage composé de %s' % ', '.join(self.list_nom)]
        for comport in self.list_comport:
            if comport.doc:
                txt.append('%s : %s' % (comport.nom, comport.doc))
        self.doc = os.linesep.join(txt)

    # definition des propriétés (seulement la méthode get)
    num_lc = property(Base.gen_getfunc(first,        'num_lc'))
    nb_vari = property(Base.gen_getfunc(sum,          'nb_vari'))
    nom_vari = property(Base.gen_getfunc(union,        'nom_vari'))
    mc_mater = property(Base.gen_getfunc(union,        'mc_mater'))
    modelisation = property(Base.gen_getfunc(intersection, 'modelisation'))
    deformation = property(Base.gen_getfunc(intersection, 'deformation'))
    nom_varc = property(Base.gen_getfunc(intersection, 'nom_varc'))
    algo_inte = property(Base.gen_getfunc(intersection, 'algo_inte'))
    type_matr_tang = property(Base.gen_getfunc(intersection, 'type_matr_tang'))
    proprietes = property(Base.gen_getfunc(intersection, 'proprietes'))
    symbol_mfront = property(Base.gen_getfunc(first,        'symbol_mfront'))

    @property
    def ldctype(self):
        """Return the class type"""
        typs = [i.ldctype for i in self.list_comport]
        if 'mfront' in typs:
            return 'mfront'
        return self._ldctype


class KIT_META(KIT):

    """Définit un assemblage de loi de comportement par KIT par un 'nom' et une
    liste de comportements"""

    def __init__(self, nom, *list_comport):
        """Initialisations"""
        if len(list_comport) != 2:
            raise CataComportementError(u"KIT_META : il faut 2 comportements")
        elif not list_comport[0].nom.startswith('META_'):
            raise CataComportementError(u"KIT_META : le premier doit être un "
                                        "comportement META_xx")
        KIT.__init__(self, nom, *list_comport)

        self.init_nb_vari()

    def init_nb_vari(self):
        """Composition des variables internes"""
        meta, mat = self.list_comport
        # variables internes
        self._nb_vari = meta.nb_vari * mat.nb_vari + meta.nb_vari + 1
        self._nom_vari = []
        # variables internes pour chaque phase
        for phase in mat.nom_vari:
            for vari in meta.nom_vari:
                self._nom_vari.append('%s_%s' % (vari, phase))

        # si écrouissage isotrope, l'indicateur de plasticité arrive avant !
        if meta.nb_vari == 1:
            self._nom_vari.append('INDICAT')

        # variables internes moyennes (écrouissage moyen)
        for vari in meta.nom_vari:
            self._nom_vari.append('%s_MOYEN' % vari)

        # si écrouissage cinématique, l'indicateur de plasticité arrive après !
        if meta.nb_vari > 1:
            self._nom_vari.append('INDICAT')

        if len(self._nom_vari) != self._nb_vari:
            raise CataComportementError, "Nombre de variables internes = %d, incohérent avec la liste des variables internes %s" % (
                self._nb_vari, self._nom_vari)

    def get_nb_vari(self):
        return self._nb_vari or 0

    nb_vari = property(get_nb_vari, "nb_vari")

    def get_nom_vari(self):
        return self._nom_vari

    nom_vari = property(get_nom_vari, "nom_vari")


class CataLoiComportement(Singleton):

    """Catalogue de loi de comportement.
    Il s'agit juste d'un dictionnaire contenant les objets de type LoiComportement
    et quelques méthodes d'interrogation"""

    def __init__(self):
        """Initialisations"""
        self._dico = {}
        # pour nommer les comportements
        self._name_num = 0
        self._name_template = 'COMP%012d'     # 16 caractères
        self.debug = False

    def _name(self):
        """Retourne un nom de comportement (K16)"""
        self._name_num += 1
        return self._name_template % self._name_num

    def add(self, comport):
        """Ajoute une loi de comportement au catalogue"""
        loi = comport.nom
        if self._dico.get(loi) is not None:
            msg = ufmt(u"Comportement déjà défini : %s", loi)
            raise CataComportementError(msg)
        self._dico[loi] = comport

    def get(self, loi):
        """Retourne l'objet LoiComportement dont le nom est 'loi'"""
        comport = self._dico.get(loi.strip())
        if comport is None:
            raise CataComportementError(
                ufmt(u"Comportement inexistant : %s", loi))
        return comport

    def create(self, *list_kit):
        """Créer un assemblage de LC composé des comportements listés dans 'list_kit'
        et retourne le nom attribué automatiquement à ce comportement.

        CALL LCCREE(NBKIT, LKIT, COMPOR)
        ==> comport = catalc.create(*list_kit)"""
        if self.debug:
            print 'catalc.create - args =', list_kit
        nom = self._name()
        list_comport = [self.get(kit) for kit in list_kit]
        comport = KIT(nom, *list_comport)
        self.add(comport)
        return nom

    def get_info(self, loi):
        """Retourne le numéro de routine et le nbre de variables internes

        CALL LCINFO(COMPOR, NUMLC, NBVARI)
        ==> num_lc, nb_vari = catalc.get_info(COMPOR)"""
        if self.debug:
            print 'catalc.get_info - args =', loi
        comport = self.get(loi)
        return comport.num_lc, comport.nb_vari

    def get_vari(self, loi):
        """Retourne la liste des variables internes

        CALL LCVARI(COMPOR, NBVARI, LVARI)
        ==> nom_vari = catalc.get_vari(COMPOR)"""
        if self.debug:
            print 'catalc.get_vari - args =', loi
        comport = self.get(loi)
        return comport.nom_vari

    def query(self, loi, attr, valeur):
        """Est-ce que VALEUR est un valeur autorisée de PROPRIETE ?
           CALL LCTEST(COMPOR, PROPRIETE, VALEUR, IRET)
           ==> iret = catalc.query(COMPOR, PROPRIETE, VALEUR)"""
        if self.debug:
            print 'catalc.query - args =', loi, ':', attr, ':', valeur.strip(), ':'
        attr = attr.lower()
        comport = self.get(loi)
        if not attr in comport.getPropertiesNames():
            raise CataComportementError(ufmt(u"Propriete invalide : %s", attr))
        # retourner 1 si (valeur est dans comport.attr), 0 sinon.
        return int(valeur.strip() in getattr(comport, attr))

    def get_algo(self, loi):
        """Retourne le 1er algorithme d'integration

        CALL LCALGO(COMPOR, ALGO)
        ==> algo_inte = catalc.get_algo(COMPOR)"""
        if self.debug:
            print 'catalc.get_algo - args =', loi
        comport = self.get(loi)
        return comport.algo_inte

    def get_type(self, loi):
        """Retourne le type de Comportement (std, mfront ou kit)

        CALL LCTYPE(COMPOR, TYPE)
        ==> ldctype = catalc.get_type(COMPOR)"""
        if self.debug:
            print 'catalc.get_type - args =', loi
        comport = self.get(loi)
        return comport.ldctype

    def get_symbol(self, loi):
        """Retourne le nom de la fonction dans la bibliothèque MFront

        CALL LCSYMB(COMPOR, NAME)
        ==> name = catalc.get_symbol(COMPOR)"""
        if self.debug:
            print 'catalc.get_symbol - args =', loi
        comport = self.get(loi)
        return comport.symbol_mfront

    def __repr__(self):
        """Représentation du catalogue"""
        sep = '-' * 90
        txt = [sep]
        for k in self._dico.keys():
            txt.append(repr(self.get(k)))
            txt.append(sep)
        return os.linesep.join(txt)

# instance unique du catalogue
catalc = CataLoiComportement()
