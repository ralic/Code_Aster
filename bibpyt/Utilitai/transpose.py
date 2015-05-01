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


# Il NE faudrait utiliser cette fonction QUE sur des tableaux hétérogènes.
# Pour les tableaux homogènes (int, float, string), utiliser numpy.transpose.

def transpose(liste):
    """Transposition de double liste
    """
    import numpy
    if isinstance(liste, numpy.ndarray):
        from warnings import warn
        warn('prefer use of numpy.transpose instead',
             DeprecationWarning, stacklevel=2)

    n = range(len(liste[0]))
    m = range(len(liste))
    liste_t = [[] for i in n]
    for i in n:
        for j in m:
            liste_t[i].append(liste[j][i])
    return liste_t
