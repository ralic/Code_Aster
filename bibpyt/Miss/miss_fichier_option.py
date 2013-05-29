# coding=utf-8
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
# person_in_charge: mathieu.courtois at edf.fr

"""Module permettant de produire le fichier des options pour Miss3D (OPTMIS).
"""

import os

import aster
from Miss.miss_utils import dict_format

sfmt = dict_format['sR']


def fichier_option(param):
    """Produit le contenu du fichier OPTMIS à partir des paramètres du calcul."""
    content = []
    # fréquences
    if param['LIST_FREQ']:
        nb = len(param['LIST_FREQ'])
        content.append("LFREQ %d" % nb)
        fmt = (sfmt * nb).strip()
        content.append(fmt % tuple(param['LIST_FREQ']))
    else:
        fmt = "FREQ" + 3*sfmt
        content.append(fmt % (param['FREQ_MIN'], param['FREQ_MAX'], param['FREQ_PAS']))
    # Z0
    content.append(("Z0" + sfmt) % param['Z0'])
    # SURF / ISSF
    if param['SURF'] == "OUI":
        content.append("SURF")
    if param['ISSF'] == "OUI":
        content.append("ISSF")
    # type binaire/ascii
    if param['TYPE'] == "BINAIRE":
        content.append("BINA")
    # RFIC
    if param['RFIC'] != 0.:
        content.append(("RFIC" + sfmt) % param['RFIC'])
    # terminer le fichier par un retour chariot
    content.append("")
    return os.linesep.join(content)
