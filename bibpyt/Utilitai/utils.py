# coding=utf-8
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
# person_in_charge: mathieu.courtois at edf.fr

"""
Module fournissant quelques fonctions utilitaires.
"""

import os
import re
import time

from Utilitai.string_utils import maximize_lines
from Execution.strfunc import convert

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
    text = convert(" ".join(l_str))
    if aster_exists:
        aster.affiche('MESSAGE', text)
    else:
        print text

def _printDBG(*args):
    """Print debug informations."""
    if not DEBUG:
        return
    _print(*args)

# les commandes fortran pourraient appeller cette fonction
def get_titre_concept(co=None):
    """Retourne un titre automatique."""
    # ASTER 10.01.25 CONCEPT tab0 CALCULE LE 21/05/2010 A 17:58:50 DE TYPE TABLE_SDASTER
    import aster_core
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
        "version" : aster_core.get_version(),
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

def fmtF2PY(fformat):
   """Convertit un format Fortran en format Python (printf style).
   Gère uniquement les fortrans réels, par exemple : E12.5, 1PE13.6, D12.5...
   """
   fmt=''
   matP=re.search('([0-9]+)P',fformat)
   if matP:
      fmt+=' '*int(matP.group(1))
   matR=re.search('([eEdDfFgG]{1})([\.0-9]+)',fformat)
   if matR:
      fmt+='%'+matR.group(2)+re.sub('[dD]+','E',matR.group(1))
   try:
      s=fmt % -0.123
   except (ValueError, TypeError), msg:
      fmt='%12.5E'
      print 'Error :',msg
      print 'Format par défaut utilisé :',fmt
   return fmt

if __name__ == '__main__':
    npar = ('X', 'Y',)
    nuti = ('DX', 'DY', 'X', 'X')
    print miss_dble(npar, nuti)
