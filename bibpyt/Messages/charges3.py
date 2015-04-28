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

# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

    1 : _(u"""
  Le concept EVOL_CHAR %(k1)s ne contient aucun champ.
"""),

    2 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s.
L'extraction du chargement volumique 3D a échoué pour l'instant %(r1)f.
Le chargement est mal défini:
- soit %(k1)s n'est pas indexé par l'instant;
- soit le chargement n'a pas été trouvé pour cet instant;
"""),

    3 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s.
L'extraction du chargement volumique 2D a échoué pour l'instant %(r1)f.
Le chargement est mal défini:
- soit %(k1)s n'est pas indexé par l'instant;
- soit le chargement n'a pas été trouvé pour cet instant;
"""),


    4 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s pour l'instant %(r1)f.
Il y a simultanément un chargement de type volumique 2D et un chargement de type surfacique 3D.
"""),

    5 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s.
L'extraction du chargement surfacique 3D a échoué pour l'instant %(r1)f.
Le chargement est mal défini:
- soit %(k1)s n'est pas indexé par l'instant;
- soit le chargement n'a pas été trouvé pour cet instant;
"""),

    6 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s pour l'instant %(r1)f.
Il y a simultanément un chargement de type volumique 3D et un chargement de type surfacique 2D.
"""),

    7 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s.
L'extraction du chargement surfacique 2D a échoué pour l'instant %(r1)f.
Le chargement est mal défini:
- soit %(k1)s n'est pas indexé par l'instant;
- soit le chargement n'a pas été trouvé pour cet instant;
"""),

    8 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s.
L'extraction du chargement de pression a échoué pour l'instant %(r1)f.
Le chargement est mal défini:
- soit %(k1)s n'est pas indexé par l'instant;
- soit le chargement n'a pas été trouvé pour cet instant;
"""),

    9 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s.
L'interpolation de la vitesse a échoué pour l'instant %(r1)f.
Le chargement est mal défini:
- soit %(k1)s n'est pas indexé par l'instant;
- soit le chargement n'a pas été trouvé pour cet instant;
"""),

    10: _(u"""
Les composantes dans le champ de vent %(k1)s doivent être exactement DX, DY et DZ.
"""),

    11: _(u"""
Les chargements de type EVOL_CHAR ne sont pas traités par CALC_VECT_ELEM.
"""),

    12 : _(u"""
Problème lors du traitement du chargement de type EVOL_CHAR %(k1)s.
L'extraction du chargement a échoué pour l'instant %(r1)f.
Le chargement est mal défini:
- soit %(k1)s n'est pas indexé par l'instant;
- soit le chargement n'a pas été trouvé pour cet instant;
- soit il manque l'un des deux champs nécessaires;
"""),

}
