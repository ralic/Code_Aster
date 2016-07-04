# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#
#
# ======================================================================


"""
    Ce module contient la classe ENTITE qui est la classe de base
    de toutes les classes de definition d'EFICAS.
"""

import re
import N_CR
import N_OPS
import N_VALIDATOR
from N_VALIDATOR import ValError, TypeProtocol, listProto
from strfunc import ufmt

try:
    from PyQt4 import QtCore
    stringTypes = (str, unicode, QtCore.QString)
except ImportError:
    stringTypes = (str, unicode)


class ENTITE:

    """
       Classe de base pour tous les objets de definition : mots cles et commandes
       Cette classe ne contient que des methodes utilitaires
       Elle ne peut être instanciee et doit d abord être specialisee
    """
    CR = N_CR.CR
    factories = {'validator': N_VALIDATOR.validatorFactory}

    def __init__(self, validators=None):
        """
           Initialise les deux attributs regles et entites d'une classe dérivée
           à : pas de règles et pas de sous-entités.

           L'attribut regles doit contenir la liste des regles qui s'appliquent
           sur ses sous-entités

           L'attribut entités doit contenir le dictionnaires des sous-entités
           (clé = nom, valeur=objet)
        """
        self.regles = ()
        self.entites = {}
        if validators:
            self.validators = self.factories['validator'](validators)
        else:
            self.validators = validators

    def affecter_parente(self):
        """
            Cette methode a pour fonction de donner un nom et un pere aux
            sous entités qui n'ont aucun moyen pour atteindre leur parent
            directement
            Il s'agit principalement des mots cles
        """
        for k, v in self.entites.items():
            v.pere = self
            v.nom = k

    def verif_cata(self):
        """
            Cette methode sert à valider les attributs de l'objet de définition
        """
        raise NotImplementedError("La méthode verif_cata de la classe %s doit être implémentée"
                                  % self.__class__.__name__)

    def __call__(self):
        """
            Cette methode doit retourner un objet dérivé de la classe OBJECT
        """
        raise NotImplementedError("La méthode __call__ de la classe %s doit être implémentée"
                                  % self.__class__.__name__)

    def report(self):
        """
           Cette méthode construit pour tous les objets dérivés de ENTITE un
           rapport de validation de la définition portée par cet objet
        """
        self.cr = self.CR()
        self.verif_cata()
        for k, v in self.entites.items():
            try:
                cr = v.report()
                cr.debut = u"Début " + v.__class__.__name__ + ' : ' + k
                cr.fin = u"Fin " + v.__class__.__name__ + ' : ' + k
                self.cr.add(cr)
            except:
                self.cr.fatal(
                    _(u"Impossible d'obtenir le rapport de %s %s"), k, `v`)
                print "Impossible d'obtenir le rapport de %s %s" % (k, `v`)
                print "père =", self
        return self.cr

    def verif_cata_regles(self):
        """
           Cette méthode vérifie pour tous les objets dérivés de ENTITE que
           les objets REGLES associés ne portent que sur des sous-entités
           existantes
        """
        for regle in self.regles:
            l = []
            for mc in regle.mcs:
                if not self.entites.has_key(mc):
                    l.append(mc)
            if l != []:
                txt = str(regle)
                self.cr.fatal(
                    _(u"Argument(s) non permis : %r pour la règle : %s"), l, txt)

    def check_definition(self, parent):
        """Verifie la definition d'un objet composite (commande, fact, bloc)."""
        args = self.entites.copy()
        mcs = set()
        for nom, val in args.items():
            if val.label == 'SIMP':
                mcs.add(nom)
                # XXX
                # if val.max != 1 and val.type == 'TXM':
                    # print "#CMD", parent, nom
            elif val.label == 'FACT':
                val.check_definition(parent)
                # CALC_SPEC !
                # assert self.label != 'FACT', \
                   #'Commande %s : Mot-clef facteur present sous un mot-clef facteur : interdit !' \
                   #% parent
            else:
                continue
            del args[nom]
        # seuls les blocs peuvent entrer en conflit avec les mcs du plus haut
        # niveau
        for nom, val in args.items():
            if val.label == 'BLOC':
                mcbloc = val.check_definition(parent)
                # XXX
                # print "#BLOC", parent, re.sub('\s+', ' ', val.condition)
                assert mcs.isdisjoint(mcbloc), "Commande %s : Mot(s)-clef(s) vu(s) plusieurs fois : %s" \
                    % (parent, tuple(mcs.intersection(mcbloc)))
        return mcs

    def check_op(self, valmin=-9999, valmax=9999):
        """Vérifie l'attribut op."""
        if self.op is not None and \
           (type(self.op) is not int or self.op < valmin or self.op > valmax):
            self.cr.fatal(_(u"L'attribut 'op' doit être un entier "
                            u"compris entre %d et %d : %r"), valmin, valmax, self.op)

    def check_proc(self):
        """Vérifie l'attribut proc."""
        if self.proc is not None and not isinstance(self.proc, N_OPS.OPS):
            self.cr.fatal(
                _(u"L'attribut op doit être une instance d'OPS : %r"), self.proc)

    def check_regles(self):
        """Vérifie l'attribut regles."""
        if type(self.regles) is not tuple:
            self.cr.fatal(_(u"L'attribut 'regles' doit être un tuple : %r"),
                          self.regles)

    def check_fr(self):
        """Vérifie l'attribut fr."""
        if type(self.fr) not in stringTypes:
            self.cr.fatal(
                _(u"L'attribut 'fr' doit être une chaine de caractères : %r"),
                self.fr)

    def check_docu(self):
        """Vérifie l'attribut docu."""
        if type(self.docu) not in stringTypes:
            self.cr.fatal(
                _(u"L'attribut 'docu' doit être une chaine de caractères : %r"),
                self.docu)

    def check_nom(self):
        """Vérifie l'attribut proc."""
        if type(self.nom) is not str:
            self.cr.fatal(
                _(u"L'attribut 'nom' doit être une chaine de caractères : %r"),
                self.nom)

    def check_reentrant(self):
        """Vérifie l'attribut reentrant."""
        if self.reentrant not in ('o', 'n', 'f'):
            self.cr.fatal(
                _(u"L'attribut 'reentrant' doit valoir 'o','n' ou 'f' : %r"),
                self.reentrant)

    def check_statut(self, into=('o', 'f', 'c', 'd')):
        """Vérifie l'attribut statut."""
        if self.statut not in into:
            self.cr.fatal(_(u"L'attribut 'statut' doit être parmi %s : %r"),
                          into, self.statut)

    def check_condition(self):
        """Vérifie l'attribut condition."""
        if self.condition != None:
            if type(self.condition) is not str:
                self.cr.fatal(
                    _(u"L'attribut 'condition' doit être une chaine de caractères : %r"),
                    self.condition)
        else:
            self.cr.fatal(_(u"La condition ne doit pas valoir None !"))

    def check_min_max(self):
        """Vérifie les attributs min/max."""
        if type(self.min) != int:
            if self.min != '**':
                self.cr.fatal(
                    _(u"L'attribut 'min' doit être un entier : %r"), self.min)
        if type(self.max) != int:
            if self.max != '**':
                self.cr.fatal(
                    _(u"L'attribut 'max' doit être un entier : %r"), self.max)
        if self.min > self.max:
            self.cr.fatal(
                _(u"Nombres d'occurrence min et max invalides : %r %r"),
                self.min, self.max)

    def check_validators(self):
        """Vérifie les validateurs supplémentaires"""
        if self.validators and not self.validators.verif_cata():
            self.cr.fatal(_(u"Un des validateurs est incorrect. Raison : %s"),
                          self.validators.cata_info)

    def check_homo(self):
        """Vérifie l'attribut homo."""
        if self.homo != 0 and self.homo != 1:
            self.cr.fatal(
                _(u"L'attribut 'homo' doit valoir 0 ou 1 : %r"), self.homo)

    def check_into(self):
        """Vérifie l'attribut into."""
        if self.into != None:
            if type(self.into) not in (list, tuple):
                self.cr.fatal(
                    _(u"L'attribut 'into' doit être un tuple : %r"), self.into)

    def check_position(self):
        """Vérifie l'attribut position."""
        if self.position not in ('local', 'global', 'global_jdc'):
            self.cr.fatal(_(u"L'attribut 'position' doit valoir 'local', 'global' "
                            u"ou 'global_jdc' : %r"), self.position)

    def check_defaut(self):
        """Vérifie l'attribut defaut."""
        if self.defaut != None:
            typeProto = TypeProtocol("type", typ=self.type)
            lval = listProto.adapt(self.defaut)
            for val in lval:
                try:
                    typeProto.adapt(val)
                except ValError:
                    self.cr.fatal(
                        _(u"La valeur de l'attribut 'defaut' n'est pas cohérente " \
                          u"avec le type %r : %r"), self.type, val)

    def check_inout(self):
        """Vérifie l'attribut inout."""
        if self.inout not in ('in', 'out', 'inout'):
            self.cr.fatal(
                _(u"L'attribut 'inout' doit valoir 'in','out' ou 'inout' : %r"),
                self.inout)

