#@ MODIF miss_utils Miss  DATE 16/10/2012   AUTEUR DEVESA G.DEVESA 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# RESPONSABLE COURTOIS M.COURTOIS

"""Module d'utilitaires pour la gestion des calculs Miss3D.

Les objets/fonctions définis sont :
    dict_format     : formats d'impression des réels et des entiers
    MISS_PARAMETERS : rassemble les paramètres pour un calcul
    lire_nb_valeurs : lit des valeurs dans un fichier texte
    en_ligne        : formatte des valeurs en colonnes
"""

import os.path as osp
import re
import pprint
from math import log

import numpy as NP

import aster
from Noyau.N_types import force_list
from Utilitai.Utmess import UTMESS, ASSERT
from Utilitai.UniteAster import UniteAster
from Utilitai.transpose import transpose
from Utilitai.utils import _printDBG

dict_format = {
    'R'  : "15.6E",
    'sR' : "%15.6E",
    'I'  : "6d",
    'sI' : "%6d",
    'F'  : "6.6f",
}


def get_max_dabsc(fonction):
    """Retourne le maximum et le pas des abscisses de la fonction."""
    tfunc = fonction.convert()
    dx = tfunc.vale_x[1:] - tfunc.vale_x[:-1]
    dxmax = max(dx)
    dxmin = min(dx)
    if abs((dxmax - dxmin) / dxmax) > 1.e-3:
        raise aster.error('MISS0_9', valk=fonction.nom)
    return max(tfunc.vale_x), dxmax


class MISS_PARAMETER(object):
    """Stocke les paramètres nécessaires au calcul à partir des mots-clés.
    """
    def __init__(self, initial_dir, **kwargs):
        """Enregistrement des valeurs des mots-clés.
        - Comme il n'y a qu'une occurrence de PARAMETRE, cela permet de
          remonter tous les mots-clés dans un seul dictionnaire.
        - On peut ajouter des vérifications infaisables dans le capy.
        - On ajoute des paramètres internes.
        """
        self._defaults = {
            '_INIDIR'    : initial_dir,
            '_WRKDIR'    : osp.join(initial_dir, 'tmp_miss3d'),
            '_NBM_DYN'   : None,
            '_NBM_STAT'  : None,
            '_exec_Miss' : False,
            'EXCIT_HARMO' : None,
            'INST_FIN' : None,
            'PAS_INST' : None,
            'FICHIER_SOL_INCI' : 'NON', #Si ondes inclinees : None
            #XXX en attendant de savoir si on réutilisera
            #'ISSF' : 'NON',
        }
        self._keywords = {}
        # une seule occurence du mcfact
        mcfact = kwargs.get('PARAMETRE')
        if mcfact is not None:
            mcfact = mcfact[0]
            self._keywords.update( mcfact.cree_dict_valeurs(mcfact.mc_liste) )
        # autres mots-clés
        others = kwargs.keys()
        others.remove('PARAMETRE')
        for key in others + self._defaults.keys():
            val = kwargs.get(key)
            if val is None:
                val = self._defaults.get(key)
            self._keywords[key] = val
        if self['REPERTOIRE']:
            self._keywords['_WRKDIR'] = self['REPERTOIRE']
        self.UL = UniteAster()
        self.check()

    def __del__(self):
        """A la destruction."""
        self.UL.EtatInit()

    def check(self):
        """Vérification des règles impossible à écrire dans le .capy"""
        # unités logiques
        if self.get('UNITE_RESU_IMPE') is None:
            self.set('_exec_Miss', True)
            self['UNITE_RESU_IMPE'] = self.UL.Libre(action='ASSOCIER')
        if self.get('UNITE_RESU_FORC') is None:
            self.set('_exec_Miss', True)
            self['UNITE_RESU_FORC'] = self.UL.Libre(action='ASSOCIER')

        # fréquences
        if self['LIST_FREQ'] is not None and self['TYPE_RESU'] not in ('FICHIER','HARM_GENE'):
            raise aster.error('MISS0_17')

        # si base modale, vérifier/compléter les amortissements réduits
        if self['BASE_MODALE']:
            res = aster.dismoi('C', 'NB_MODES_DYN', self['BASE_MODALE'].nom, 'RESULTAT')
            ASSERT(res[0] == 0)
            self['_NBM_DYN'] = res[1]
            res = aster.dismoi('C', 'NB_MODES_STA', self['BASE_MODALE'].nom, 'RESULTAT')
            ASSERT(res[0] == 0)
            self['_NBM_STAT'] = res[1]
            if self['AMOR_REDUIT']:
                self.set('AMOR_REDUIT', force_list(self['AMOR_REDUIT']))
                nval = len(self['AMOR_REDUIT'])
                if nval < self['_NBM_DYN']:
                    # complète avec le dernier
                    nadd = self['_NBM_DYN'] - nval
                    self._keywords['AMOR_REDUIT'].extend([self['AMOR_REDUIT'][-1],] * nadd)
                    nval = self['_NBM_DYN']
                if nval < self['_NBM_DYN'] + self['_NBM_STAT']:
                    # on ajoute 0.
                    self._keywords['AMOR_REDUIT'].append(0.)

    def __getitem__(self, key):
        return self._keywords[key]

    def __setitem__(self, key, value):
        ASSERT(self.get(key) is None)
        self.set(key, value)

    def set(self, key, value):
        self._keywords[key] = value

    def get(self, key):
        return self._keywords.get(key)

    def __repr__(self):
        return pprint.pformat(self._keywords)


def lire_nb_valeurs(file_object, nb, extend_to, conversion,
                    nb_bloc=1, nb_ignore=0, max_per_line=-1,
                    regexp_label=None):
    """Lit nb valeurs dans file_object et les ajoute à extend_to
    après les avoir converties en utilisant la fonction conversion.
    Ignore nb_ignore lignes pour chacun des nb_bloc lus et lit au
    maximum max_per_line valeurs par ligne.
    Retourne le nombre de lignes lues."""
    if max_per_line < 0:
        max_per_line = nb
    ln = 0
    _printDBG("LIRE_NB_VALEURS nb=", nb, ", nb_bloc=", nb_bloc, ", nb_ignore=", nb_ignore)
    for i in range(nb_bloc):
        val = []
        j = 0
        label = []
        while j < nb_ignore:
            ln += 1
            line = file_object.readline()
            if line.strip() == '':
                continue
            j += 1
            label.append(line)
        if nb_ignore:
            _printDBG("IGNORE", label)
            slabel = ''.join(label)
            if regexp_label:
                mat = re.search(regexp_label, slabel, re.M)
                if mat is None:
                    _printDBG("LABEL", regexp_label, "non trouve, recule de",
                              nb_ignore, "lignes.", i, "bloc(s) lu(s).")
                    curpos = file_object.tell()
                    file_object.seek(curpos - len(slabel))
                    return 0
        while len(val) < nb:
            ln += 1
            line = file_object.readline()
            if line.strip() == '':
                continue
            add = [conversion(v) for v in line.split()[:max_per_line]]
            val.extend(add)
        ASSERT(len(val) == nb, "%d valeurs attendues, %d valeurs lues" % (nb, len(val)))
        extend_to.extend(val)
        _printDBG("BLOC", i, ",", nb, "valeurs lues, debut :", repr(val[:3]))
    return ln

def en_ligne(valeurs, format, cols, separateur=" ", format_ligne="%(valeurs)s"):
    """Formatte valeurs en cols colonnes en utilisant le format.
    """
    res = []
    ind = -1
    while len(valeurs) > 0:
        lv = []
        for i in range(cols):
            if len(valeurs) == 0:
                break
            lv.append(format % valeurs.pop(0))
        ind += 1
        line = format_ligne % {
            "valeurs" : separateur.join(lv),
            "index"   : ind,
            "index_1" : ind + 1,
        }
        res.append(line)
    return res

def convert_double(fich1, fich2):
    """Convertit les 1.D+09 en 1.E+09.
    """
    txt = open(fich1, "r").read()
    # see python doc (module re)
    expr = re.compile("([\-\+]?\d+(\.\d*)?|\.\d+)([eEdD])([\-\+]?\d+)?")
    new = expr.sub("\\1E\\4", txt)
    open(fich2, "w").write(new)

def double(string):
    """Convertit la chaine en réelle (accepte le D comme exposant).
    """
    string = re.sub('([0-9]+)([\-\+][0-9])', '\\1e\\2', string)
    return float(string.replace("D", "e"))

def get_puis2(nval):
    """Retourne N, la plus grande puissance de 2 telle que 2**N <= nval
    """
    return int(log(nval, 2.))
