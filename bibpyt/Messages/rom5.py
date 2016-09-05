# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: mickael.abbas at edf.fr

cata_msg = {

    1 : _(u"""Création de la matrice des clichés."""),

    2 : _(u"""Sauvegarde des %(i1)d modes empiriques dans la base empirique %(k1)s."""),

    4 : _(u"""Comptage du nombre de modes empiriques à sélectionner"""), 

    5 : _(u"""On a %(i1)d valeurs singulières comprises entre %(r1)13.6G et %(r2)13.6G . Avec les paramètres, on a retenu %(i2)d modes empiriques."""),   

    6 : _(u"""Le critère de sélection des valeurs singulières ne permet pas d'extraire au moins un mode empirique. Il faut changer la tolérance ou le nombre de modes."""),

    7 : _(u"""Calcul des modes empiriques par décomposition aux valeurs singulières."""),

    8 : _(u"""Échec lors du calcul des modes empiriques."""),

    9 : _(u"""Initialisations de toutes les structures de données."""),

   10 : _(u"""Lecture de tous les paramètres."""),

   11 : _(u"""On ne trouve pas de champ de type %(k1)s dans la structure de données résultat."""),

   12 : _(u"""Préparation de la numérotation des modes linéiques."""),

   13 : _(u"""On a détecté %(i1)d tranches pour la définition des modes linéiques."""),

   19 : _(u"""Vérifications de la conformité de la structure de données résultat utilisée de nom %(k1)s."""),

   20 : _(u"""On ne peut utiliser que des maillages tridimensionnels."""),
 
   21 : _(u"""Le champ de type %(k1)s ne doit contenir que %(i1)d composante"""),
 
   22 : _(u"""Le champ contient des conditions limites dualisés (AFFE_CHAR_THER ou AFFE_CHAR_MECA). 
              Ce n'est pas possible avec cet opérateur, utilisez AFFE_CHAR_CINE"""),

   23 : _(u"""Le champ contient une composante au noeud %(k1)s qui n'est pas utilisable avec cet opérateur"""),
}
