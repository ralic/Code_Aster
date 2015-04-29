# coding=utf-8
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

# person_in_charge: mathieu.courtois at edf.fr

import types

import aster
from Cata.cata import _F
from Cata.cata import DEFI_FICHIER
from Cata.cata import INFO_EXEC_ASTER
from Cata.cata import DETRUIRE


class UniteAster:

    """Classe pour manipuler les fichiers en Python en accord avec les unités
    logiques utilisées en Fortran.
    De manière analogue au Fortran, les états possibles d'une unité sont :
       'F' : fermé, 'O' : ouvert, 'R' : réservé.

    Méthodes :
       Nom      : Retourne le nom du fichier associé à une unité,
       Etat     : Retourne l'état d'une unité,
       Libre    : Retourne un numéro d'unité libre,
       EtatInit : Remet une, plusieurs ou toutes les unités dans leur état initial.

    Méthode privée :
       _setinfo : pour remplir le dictionnaire des 'infos'
    Attribut privé :
       infos[numéro unité] = { 'nom' : x, 'etat' : x , 'etat_init' : x }
    """

    def __init__(self):
        """Initialise le dictionnaire des unités.
        """
        self.infos = {}

    def _setinfo(self, ul):
        """Remplit les infos de l'unité 'ul'.
        """
        # ul peut etre un entier Aster
        try:
            unit = ul.valeur
        except:
            unit = int(ul)
        # Si la clé n'existe pas
        ini = False
        if not self.infos.has_key(unit):
            self.infos[unit] = {}
            self.infos[unit]['nom'] = ''
            self.infos[unit]['etat'] = '?'
            self.infos[unit]['etat_init'] = '?'
            ini = True

        __tab = INFO_EXEC_ASTER(UNITE=unit, LISTE_INFO=('ETAT_UNITE'))

        # O:ouvert, F:fermé, R:réservé
        self.infos[unit]['etat'] = __tab['ETAT_UNITE', 1].strip()[0]
        if ini:
            self.infos[unit]['etat_init'] = self.infos[unit]['etat']

        # nom du fichier
        if self.infos[unit]['etat'] in ['O', 'R']:
            nomfich = ''.join([__tab['NOMFIC%d' % i, 1]
                              for i in range(1, 5)]).strip()
        elif self.infos[unit]['etat'] == 'F':
            nomfich = 'fort.' + str(unit)
        else:
            message = "Etat de l'unité inconnu : %s" % self.infos[unit]['etat']
            print __tab.EXTR_TABLE()
            raise aster.error, "<F> <UniteAster._setinfo> %s" % message
        self.infos[unit]['nom'] = nomfich
        # print 'DEBUG infos[unit] = ', self.infos[unit]
        DETRUIRE(CONCEPT=_F(NOM=__tab), INFO=1)

    def Libre(self, nom=None, action='RESERVER'):
        """Réserve/associe et retourne une unité libre en y associant, s'il est
        fourni, le fichier 'nom'.
        """
        __tab = INFO_EXEC_ASTER(LISTE_INFO=('UNITE_LIBRE'))
        unit = __tab['UNITE_LIBRE', 1]
        DETRUIRE(CONCEPT=_F(NOM=__tab), INFO=1)
        if nom == None:
            nom = 'fort.' + str(unit)

        # Si la clé existe, c'est que le fichier n'était pas libre
        if self.infos.has_key(unit):
            message = "Cette unité est déjà affectée au fichier %s" % \
                self.infos[unit]['nom']
            raise aster.error, "<F> <UniteAster.Libre> %s" % message

        DEFI_FICHIER(ACTION=action, UNITE=unit, FICHIER=nom.strip())
        self.infos[unit] = {}
        self.infos[unit]['nom'] = nom.strip()
        self.infos[unit]['etat'] = 'R'
        self.infos[unit]['etat_init'] = 'F'
        return unit

    def Nom(self, ul):
        """Retourne le nom du fichier associé à l'unité 'ul'.
        """
        # ul peut etre un entier Aster
        try:
            unit = ul.valeur
        except:
            unit = int(ul)
        # Si la clé n'existe pas
        if not self.infos.has_key(unit):
            self._setinfo(unit)
        return self.infos[unit]['nom']

    def Unite(self, nom):
        """Retourne l'unité logique associée au fichier `nom`.
        On retourne 0 si le nom n'a pas été trouvé."""
        ul = 0
        for unit, infos in self.infos.items():
            if infos['nom'] == nom.strip():
                ul = unit
                break
        return ul

    def Etat(self, ul, **kargs):
        """Retourne l'état de l'unité si 'etat' n'est pas fourni
        et/ou change son état :
           kargs['etat']  : nouvel état,
           kargs['nom']   : nom du fichier,
           kargs['TYPE']  : type du fichier à ouvrir ASCII/BINARY/LIBRE,
           kargs['ACCES'] : type d'accès NEW/APPEND/OLD (APPEND uniquement en ASCII).
        """
        # ul peut etre un entier Aster
        try:
            unit = ul.valeur
        except:
            unit = int(ul)
        # Si la clé n'existe pas
        if not self.infos.has_key(unit):
            self._setinfo(unit)
        if not kargs.has_key('etat'):
            return self.infos[unit]['etat']

        # En fonction de la demande, on bascule son état ou pas
        new = kargs.get('etat')
        if not new in ['R', 'F', 'O']:
            message = "Nouvel état de l'unité incorrect : %s" % new
            raise aster.error, "<F> <UniteAster.Etat> %s" % message

        if self.infos[unit]['etat'] == new:
            pass
        elif new == 'R':
            if self.infos[unit]['etat'] == 'O':
                DEFI_FICHIER(ACTION='LIBERER',  UNITE=unit)
            DEFI_FICHIER(ACTION='RESERVER',
                         UNITE=unit,
                         FICHIER=kargs.get('nom', self.infos[unit]['nom']))
            self._setinfo(unit)
        elif new == 'F':
            DEFI_FICHIER(ACTION='LIBERER', UNITE=unit)
        elif new == 'O':
            if self.infos[unit]['etat'] == 'R':
                DEFI_FICHIER(ACTION='LIBERER', UNITE=unit)
            # valeurs par défaut
            typ = kargs.get('TYPE', 'ASCII')
            if typ == 'ASCII':
                acces = 'APPEND'
            else:
                acces = 'OLD'
            acces = kargs.get('ACCES', acces)
            DEFI_FICHIER(ACTION='ASSOCIER',
                         UNITE=unit,
                         FICHIER=kargs.get('nom', self.infos[unit]['nom']),
                         TYPE=typ,
                         ACCES=acces,)
            self._setinfo(unit)
        self.infos[unit]['etat'] = new
        return self.infos[unit]['etat']

    def EtatInit(self, ul=None):
        """Remet l'unité 'ul' dans son état initial.
        Si 'ul' est omis, toutes les unités sont remises dans leur état initial.
        """
        if ul == None:
            for uli, vul in self.infos.items():
                self.Etat(uli, etat=vul['etat_init'])
        else:
            if not type(ul) in [types.ListType, types.TupleType]:
                ul = [ul, ]
            for u in ul:
                # u peut etre un entier Aster
                try:
                    unit = u.valeur
                except:
                    unit = int(u)
                # Si la clé n'existe pas
                if not self.infos.has_key(unit):
                    self._setinfo(unit)
                else:
                    self.Etat(unit, etat=self.infos[unit]['etat_init'])
