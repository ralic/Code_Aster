#@ MODIF utils Utilitai  DATE 23/05/2011   AUTEUR COURTOIS M.COURTOIS 
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


"""
Module fournissant quelques fonctions utilitaires.
"""

import os
import time

from Utilitai.string_utils import maximize_lines

try:
   import aster
   aster_exists = True
except:
   aster_exists = False


DEBUG = False
def set_debug(value):
    """Positionne la variable de déboggage"""
    global DEBUG
    DEBUG = value

def _print(*args):
    """Fonction 'print'."""
    l_str = []
    for arg in args:
        if type(arg) not in (str, unicode):
            arg = repr(arg)
        l_str.append(arg)
    text = " ".join(l_str)
    if aster_exists:
        aster.affiche('MESSAGE', text)
    else:
        print text

def _printDBG(*args):
    """Print debug informations."""
    if not DEBUG:
        return
    _print(*args)


def miss_dble(list1, list2):
    """miss = elements de list1 absents de list2
      dble = elements de list2 presents dans list1, fournis plusieurs fois."""
    s_ini = set(list1)
    inter = s_ini.intersection(list2)
    miss = s_ini.copy()
    dble = set()
    for para in list2:
        try:
            miss.remove(para)
        except KeyError:
            if set(para).issubset(s_ini) and para.strip() != '':
                dble.add(para)
    return miss, inter, dble


# les commandes fortran pourraient appeller cette fonction
def get_titre_concept(co=None):
    """Retourne un titre automatique."""
    # ASTER 10.01.25 CONCEPT tab0 CALCULE LE 21/05/2010 A 17:58:50 DE TYPE TABLE_SDASTER
    import aster
    from Noyau.N_ASSD import ASSD
    if not isinstance(co, ASSD):
        co = None
    fmt = {
        "version" : "ASTER %(version)s",
        "nomco" : "CONCEPT %(nom_concept)s",
        "etatco" : "CALCULE",
        "dateheure" : "%(dateheure)s",
        "typeco" : "DE TYPE %(type_concept)s",
    }
    format = [fmt["version"], ]
    dfmt = {
        "version" : aster.__version__,
        "dateheure" : time.strftime("LE %m/%d/%Y A %H:%M:%S"),
    }
    if co:
        dfmt["nom_concept"] = co.nom
        format.append(fmt["nomco"])
        format.append(fmt["etatco"])
    format.append(fmt["dateheure"])
    if co:
        dfmt["type_concept"] = co.__class__.__name__.upper()
        format.append(fmt["typeco"])
    globfmt = " ".join(format)
    titre = globfmt % dfmt
    ltit = titre.split()
    ltit = maximize_lines(ltit, 80, " ")
    return os.linesep.join(ltit)


if __name__ == '__main__':
    npar = ('X', 'Y',)
    nuti = ('DX', 'DY', 'X', 'X')
    print miss_dble(npar, nuti)

