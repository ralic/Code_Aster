#@ MODIF sd_util SD  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
   Utilitaires pour la vérification des SD
"""

# pour utilisation dans eficas
try:
   import aster
except:
   pass

from sets import Set
import copy


#  1) Utilitaires pour vérifier certaines propriétés.
#     Ces utilitaires ne provoquent pas d'arret mais écrivent des messages dans un "checker"
#  -----------------------------------------------------------------------------------------


#   1.1 Utilitaires pour des scalaires :
#   ------------------------------------

def sdu_assert(ojb, checker, bool,comment=''):
    # Vérifie que le booléen (bool) est vrai
    if not bool :
            checker.err(ojb, "condition non respectée :  (%s)" % (comment,))

def sdu_compare(ojb, checker, val1, comp, val2, comment=''):
    # Vérifie que la relation de comparaison entre val1 et val2 est respectée :
    #   comp= '==' / '!=' / '>=' / '>' / '<=' / '<'
    comp=comp.strip()
    ok = 0
    if comp == "==" :
       ok = val1 == val2
    elif comp == "!=" :
       ok = val1 != val2
    elif comp == ">=" :
       ok = val1 >= val2
    elif comp == "<=" :
       ok = val1 <= val2
    elif comp == ">" :
       ok = val1 > val2
    elif comp == "<" :
       ok = val1 < val2
    else :
       sdu_assert(ojb, checker, 0, 'sdu_compare: opérateur de comparaison interdit: '+comp)

    if not ok :
            checker.err(ojb, "condition non respectée : %s  %s  %s (%s)" % (val1,comp,val2,comment))


#   1.2 Utilitaires pour des séquences :
#   ------------------------------------

def sdu_tous_differents(ojb,checker,sequence=None,comment=''):
    # Vérifie que les éléments de la séquence sont tous différents.
    # Si l'argument sequence est None, on prend l'ensemble de l'ojb.

    if sequence :
        seq=sequence
    else :
        seq=ojb.get()

    sdu_compare(ojb, checker, len(seq), '==', len(Set(seq)), comment='Tous différents: '+comment)


def sdu_tous_non_blancs(ojb,checker,sequence=None,comment=''):
    # Vérifie que les éléments (chaines) de la séquence sont tous "non blancs".
    # Si l'argument sequence est None, on prend l'ensemble de l'ojb.

    if sequence :
        seq=sequence
    else :
        seq=ojb.get()

    for elem in seq :
        assert len(elem.strip()) > 0 , (seq,self, 'tous "non blancs" '+comment)


def sdu_tous_compris(ojb,checker,sequence=None,vmin=None,vmax=None,comment=''):
    # Vérifie que toutes les valeurs de la sequence sont comprises entre vmin et vmax
    # Les bornes vmin et vmax sont autorisées
    # Si l'argument sequence est None, on prend l'ensemble de l'ojb.

    assert (not vmin is None) or (not vmax is None),'Il faut fournir au moins une des valeurs vmin ou vmax'
    if sequence :
        seq=sequence
    else:
        seq=ojb.get()

    ier = 0
    for v in seq :
       if vmin and v < vmin : ier = 1
       if vmax and v > vmax : ier = 1
    if ier == 1 : checker.err( ojb, "L'objet doit contenir des valeurs dans l'intervalle : [%s, %s] "  % (vmin,vmax))



def sdu_monotone(seqini) :
    #-------------------------------
    # vérifie qu'une séquence est triée par ordre croissant (ou décroissant)
    # retourne :
    #    3 : ni croissant ni décroissant  (désordre)
    #    1 : croissant
    #   -1 : décroissant
    #    0 : constant

    n=len(seqini)
    if isinstance(seqini,tuple) :
        seq=list(seqini)
    else :
        seq=seqini

    seq2=copy.deepcopy(seq)
    seq2.sort()
    seq3=copy.deepcopy(seq)
    seq3.sort()
    seq3.reverse()

    croiss=1
    decroiss=1
    for k in range(n) :
        if seq[k] != seq2[k] :
            if 0 : print "AJACOT non croissant ",k,seq[k],seq2[k]
            croiss=0
        if seq[k] != seq3[k] :
            if 0 : print "AJACOT non décroissant ",k,seq[k],seq2[k]
            decroiss=0

    if   croiss==1 and decroiss==1 :
        return 0
    elif croiss==1 and decroiss==0 :
        return 1
    elif croiss==0 and decroiss==1 :
        return -1
    elif croiss==0 and decroiss==0 :
        return 3



#  2) Utilitaires de questionnement :
#  -----------------------------------------------------------------------------------------

def sdu_nom_gd(numgd) :
    # retourne le nom de la grandeur de numéro (numgd)
    assert numgd > 0 and numgd <1000 , numgd
    ptn=aster.getvectjev('&CATA.GD.NOMGD')
    return ptn[numgd-1].strip()

def sdu_licmp_gd(numgd) :
    # retourne la liste des cmps de la grandeur de numéro (numgd)
    nomgd=sdu_nom_gd(numgd)
    nocmp=aster.getcolljev('&CATA.GD.NOMCMP')
    return nocmp[nomgd.ljust(8)]
