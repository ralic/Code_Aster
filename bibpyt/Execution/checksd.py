#@ MODIF checksd Execution  DATE 23/05/2007   AUTEUR PELLET J.PELLET 
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
   Utilitaires pour tester la sd produite par une commande.
"""

from sets import Set

from Utilitai.Utmess import U2MESS as UTMESS

# pour utilisation dans eficas
try:
    import aster
except:
    pass


def get_list_objects():
    """Retourne la liste (Set) des objets jeveux présents à un moment donné
    """
    return Set(aster.jeveux_getobjects(' '))


def check(checker, sd, l_before):
    """Vérifie la cohérence de la SD produite :
       - type des objets / ceux déclarés dans le catalogue de la SD
       - présence d'objets imprévus dans le catalogue
    l_before : liste des objets jeveux présents avant la création de la SD.
    """

    type_concept = type(sd).__name__
    if 0 : print "AJACOT checksd "+type_concept+" >"+sd.nomj.nomj+'<'

    # l_new = objets créés par la commande courante
    l_after = get_list_objects()
    l_new = l_after - l_before

    # on vérifie le contenu de la SD sur la base de son catalogue
    checker = sd.check(checker)

    # on imprime les messages d'erreur stockés dans le checker :
    lerreur=[]
    for level, obj, msg in checker.msg:
        if level == 0 : lerreur.append((obj,msg))
    lerreur.sort()
    if len(lerreur) > 0 :
        # pour "ouvrir" le message :
        UTMESS("E+", 'SDVERI_30')
        for obj, msg in lerreur :
            UTMESS("E+", 'SDVERI_31', valk=(obj, msg))

        # pour "fermer" le message :
        UTMESS("E", 'SDVERI_32')

    # on détruit les messages déjà imprimés pour ne pas les réimprimer avec la SD suivante :
    checker.msg=[]

    # on vérifie que le commande n'a pas créé d'objets interdits
    l_possible = Set(checker.names.keys())
    l_interdit = list(l_new - l_possible)
    l_interdit.sort()
    if len(l_interdit) > 0 :
        # pour "ouvrir" le message :
        UTMESS("E+", 'SDVERI_40',valk=type_concept)
        for x in l_interdit :
            UTMESS('E+', 'SDVERI_41',valk=x)

        # pour "fermer" le message :
        UTMESS("E", 'SDVERI_42')

    return checker
