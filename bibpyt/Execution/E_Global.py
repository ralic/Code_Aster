#@ MODIF E_Global Execution  DATE 25/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

# RESPONSABLE MCOURTOI M.COURTOIS

"""
    Module dans lequel on définit des fonctions pour la phase d'exécution.
    Ces fonctions sont indépendantes des étapes (sinon elles seraient dans
    B_ETAPE/E_ETAPE) et des concepts/ASSD, et elles sont destinées à etre
    appelées par le fortran via astermodule.c.

    Ce module sera accessible via la variable globale `static_module`
    de astermodule.c.
"""

import time
import platform

# MessageLog est l'objet qui gère l'impression des messages
# (appelé par astermodule.c)
import aster
from Utilitai.Utmess import MessageLog, UTMESS
from Noyau.N_utils import Enum

LABEL = Enum('STABLE', 'STABLE_UPDATES', 'TESTING', 'UNSTABLE')

def checksd(nomsd, typesd):
    """Vérifie la validité de la SD `nom_sd` (nom jeveux) de type `typesd`.
    Exemple : typesd = sd_maillage
    C'est le pendant de la "SD.checksd.check" à partir d'objets nommés.
    Code retour :
      0 : tout est ok
      1 : erreurs lors du checksd
      4 : on n'a meme pas pu tester
    """
    nomsd  = nomsd.strip()
    typesd = typesd.lower().strip()
    # import
    iret = 4
    try:
        sd_module = __import__('SD.%s' % typesd, globals(), locals(), [typesd])
    except ImportError, msg:
        UTMESS('F', 'SDVERI_1', valk=typesd)
        return iret
    # on récupère la classe typesd
    clas = getattr(sd_module, typesd, None)
    if clas:
        objsd = clas(nomj=nomsd)
        chk = objsd.check()
        ichk = min([1,] + [level for level, obj, msg in chk.msg])
        if ichk == 0:
            iret = 1
        else:
            iret = 0
    # on imprime les messages d'erreur (level=0):
    for level, obj, msg in chk.msg:
        if level == 0 :
            aster.affiche('MESSAGE',repr(obj)+msg)
    return iret

_version_label = None
def version_label():
    """Retourne le label de la version."""
    global _version_label
    if _version_label is not None:
        return _version_label
    from Accas import properties
    # version stabilisée, exemple : 11.3.0
    last = properties.version.split('.')[-1]
    try:
        patch = int(last)
    except ValueError:
        patch = -1
    sta = patch == 0
    label = LABEL.UNSTABLE
    if sta:
        if properties.exploit:
            label = LABEL.STABLE
        else:
            label = LABEL.TESTING
    else:
        if properties.exploit:
            label = LABEL.STABLE_UPDATES
        else:
            label = LABEL.UNSTABLE
    _version_label = label
    return label

def version_shortname():
    """Retourne le nom 'court' de la version."""
    shortname = {
        LABEL.STABLE : "stable",
        LABEL.STABLE_UPDATES : "stable-updates",
        LABEL.TESTING : "testing",
        LABEL.UNSTABLE : "unstable",
    }
    return shortname[version_label()]

def print_header():
    """Appelé par entete.F pour afficher des informations sur
    la machine."""
    from Accas import properties
    from i18n import localization
    names = {
        LABEL.STABLE : _(u"""EXPLOITATION (stable)"""),
        LABEL.STABLE_UPDATES : _(u"""CORRECTIVE AVANT STABILISATION (stable-updates)"""),
        LABEL.TESTING : _(u"""DÉVELOPPEMENT STABILISÉE (testing)"""),
        LABEL.UNSTABLE : _(u"""DÉVELOPPEMENT (unstable)"""),
    }
    typvers = names[version_label()]
    lang_settings = '%s (%s)' % localization.get_current_settings()

    UTMESS('I', 'SUPERVIS2_10',
        valk=(typvers,
              properties.version, properties.date,
              "1991", time.strftime('%Y'),
              time.strftime('%c'),
              platform.node(),
              platform.architecture()[0],
              platform.machine(),
              platform.system() + ' ' + platform.release(),
              lang_settings,
            ),
    )
