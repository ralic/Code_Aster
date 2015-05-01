# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
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
#
#
# ======================================================================

_root = None
_cata = None
debug = 0
from Noyau.N_info import message, SUPERV

# Le "current step" est l'étape courante.
# Une macro se déclare étape courante dans sa méthode Build avant de construire
# ses étapes filles ou dans BuildExec avant de les exécuter.
# Les étapes simples le font aussi : dans Execute et BuildExec.
# (Build ne fait rien pour une étape)


def set_current_step(step):
    """
       Fonction qui permet de changer la valeur de l'étape courante
    """
    global _root
    if _root:
        raise Exception("Impossible d'affecter _root. Il devrait valoir None")
    _root = step
    # message.debug(SUPERV, "current_step = %s", step and step.nom,
    # stack_id=-1)


def get_current_step():
    """
       Fonction qui permet d'obtenir la valeur de l'étape courante
    """
    return _root


def unset_current_step():
    """
       Fonction qui permet de remettre à None l'étape courante
    """
    global _root
    _root = None


def set_current_cata(cata):
    """
       Fonction qui permet de changer l'objet catalogue courant
    """
    global _cata
    if _cata:
        raise Exception("Impossible d'affecter _cata. Il devrait valoir None")
    _cata = cata


def get_current_cata():
    """
       Fonction qui retourne l'objet catalogue courant
    """
    return _cata


def unset_current_cata():
    """
       Fonction qui permet de remettre à None le catalogue courant
    """
    global _cata
    _cata = None
