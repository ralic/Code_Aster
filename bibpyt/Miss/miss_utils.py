#@ MODIF miss_utils Miss  DATE 01/03/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    _print          : fonction d'impression
    MISS_PARAMETERS : rassemble les paramètres pour un calcul
    lire_nb_valeurs : lit des valeurs dans un fichier texte
    en_ligne        : formatte des valeurs en colonnes
"""

import os.path as osp
import re
import pprint

import numpy as NP

import aster
from Utilitai.Utmess import ASSERT
from Utilitai.UniteAster import UniteAster


dict_format = {
    'R'  : "15.6E",
    'sR' : "%15.6E",
    'I'  : "6d",
    'sI' : "%6d",
}

DEBUG = False
def set_debug(value):
    """Positionne la variable de déboggage"""
    global DEBUG
    DEBUG = value


def _print(*args):
    l_str = []
    for arg in args:
        if type(arg) not in (str, unicode):
            arg = repr(arg)
        l_str.append(arg)
    text = " ".join(l_str)
    aster.affiche('MESSAGE', text)


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
        if self['UNITE_RESU_IMPE'] is None:
            self.set('_exec_Miss', True)
            self['UNITE_RESU_IMPE'] = self.UL.Libre(action='ASSOCIER')
        if self['UNITE_RESU_FORC'] is None:
            self.set('_exec_Miss', True)
            self['UNITE_RESU_FORC'] = self.UL.Libre(action='ASSOCIER')
        # fréquences : on remplit les mots-clés absents, on pourra ainsi utiliser
        # l'un ou l'autre selon les cas.
        if self['FREQ_MIN'] is None:
            assert len(self['LFREQ_LISTE']) == self['LFREQ_NB']
            self['FREQ_MIN'] = self['LFREQ_LISTE'][0]
            self['FREQ_MAX'] = self['LFREQ_LISTE'][-1]
            lfreq = NP.array(self['LFREQ_LISTE'])
            pasini = lfreq[1] - lfreq[0]
            assert pasini != 0.
            pas = lfreq[1:] - lfreq[:-1]
            dpasm = max(abs((pas - pasini) / pas))
            assert dpasm < 1.e-3
            self['FREQ_PAS'] = pasini
        else:
            self['LFREQ_NB'] = int(round((self['FREQ_MAX'] - self['FREQ_MIN']) / self['FREQ_PAS'])) + 1
            self['LFREQ_LISTE'] = NP.arange(self['FREQ_MIN'], self['FREQ_MAX'] + self['FREQ_PAS'],
                self['FREQ_PAS']).tolist()
        # si base modale, vérifier/compléter les amortissements réduits
        if self['BASE_MODALE']:
            res = aster.dismoi('C', 'NB_MODES_DYN', self['BASE_MODALE'].nom, 'RESULTAT')
            assert res[0] == 0
            self['_NBM_DYN'] = res[1]
            res = aster.dismoi('C', 'NB_MODES_STA', self['BASE_MODALE'].nom, 'RESULTAT')
            assert res[0] == 0
            self['_NBM_STAT'] = res[1]
            if self['AMOR_REDUIT']:
                if type(self['AMOR_REDUIT']) not in (list, tuple):
                    self.set('AMOR_REDUIT', [self['AMOR_REDUIT'],])
                self.set('AMOR_REDUIT', list(self['AMOR_REDUIT']))
                nval = len(self['AMOR_REDUIT'])
                if nval < self['_NBM_DYN']:
                    # complète avec le dernier
                    nadd = self['_NBM_DYN'] - nval
                    self._keywords['AMOR_REDUIT'].extend([self['AMOR_REDUIT'][-1],] * nadd)
                    nval = self['_NBM_DYN']
                if nval < self['_NBM_DYN'] + self['_NBM_STAT']:
                    # on ajoute 0.
                    self._keywords['AMOR_REDUIT'].append(0.)
        # excitations
        if self['TYPE_RESU'] == 'TABLE':
            nbcalc = 0
            for p in ('ACCE_X', 'ACCE_Y', 'ACCE_Z', 'INST_FIN', 'PAS_INST'):
                val = self._keywords[p]
                if val is not None:
                    if nbcalc != 0 and nbcalc != len(val):
                        raise RuntimeError("ACCE_X, ACCE_Y, ACCE_Z, INST_FIN et PAS_INST " \
                                           "doivent avoir le même cardinal !")
                    nbcalc = len(val)
            assert nbcalc > 0


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
        assert len(val) == nb, "%d valeurs attendues, %d valeurs lues" % (nb, len(val))
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


def _printDBG(*args):
    """Print debug informations."""
    if not DEBUG:
        return
    _print(*args)

