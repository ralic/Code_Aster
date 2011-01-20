#@ MODIF discrets Messages  DATE 19/01/2011   AUTEUR MASSIN P.MASSIN 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

# Messages pour les éléments discrets non-linéaires
cata_msg={

1: _("""
Pour l'élément discret %(k1)s .
Il n'y a pas de rotation non-linéaire possible.
"""),

2: _("""
Pour l'élément discret %(k1)s .
Il n'y a pas de comportement non-lineaire possible suivant Z
ou en rotation autour de X,Y en 2D.
"""),

3: _("""
Pour l'élément discret %(k1)s .
Il n'y a pas de comportement non-linéaire possible en rotation
ou suivant Z en 2D.
"""),

4: _("""
Pour l'élément discret.
Le pas de temps est devenu trop petit : %(r1)12.5E .
"""),

5: _("""
Pour l'élément discret %(k5)s .
Les caractéristiques sont obligatoirement données dans le repère local du discret.

Pour Info :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comport. : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

6: _("""
Pour les éléments discrets il faut définir un repère dans AFFE_CARA_ELEM

Pour Info :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comport. : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

7 : _("""
Le Comportement <%(k4)s> affecté à un DISCRET est non valide
Les comportements valides sont :
   COMP_ELAS   ELAS

   COMP_INCR   ELAS           DIS_GRICRA  DIS_VISC  DIS_ECRO_CINE
               DIS_BILI_ELAS  ASSE_CORN   ARME      DIS_CHOC
               DIS_GOUJ2E

Pour Info :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comport. : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

8 : _("""
Pour les discrets, avec COMP_ELAS le seul comportement valide est ELAS.

Pour Info :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comport. : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

10 : _("""
Pour l'élément DISCRET de modèle <%(k1)s> la matrice de décharge est non développée.

Pour Info :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comport. : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

11 : _("""
La loi <%(k4)s> doit etre utilisée avec des éléments du type DIS_TR_L : élément SEG2 + modélisation DIS_TR

Pour Info :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comport. : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

12 : _("""
La commande %(k4)s ne sait pas traiter les matrices non-symétriques, pour l'option %(k1)s.
Message de la routine %(k3)s, pour l'élément %(k2)s.
"""),

13 : _("""
L'élément %(k1)s est inconnu pour la maille %(k3)s.
Message de la routine %(k2)s.
"""),

14 : _("""
L'option %(k1)s est inconnue pour l'élément %(k2)s.
Message de la routine %(k3)s.
"""),

15 : _("""
L'option %(k1)s ne sait pas traiter l'élément %(k2)s.
Message de la routine %(k3)s.
"""),

16 : _("""
Il est interdit d'avoir des éléments discrets 2D et 3D dans un modèle.
"""),

17 : _("""
Votre modélisation ne comporte pas d'élément discret.
"""),

20 : _("""
Votre modélisation doit être soit 2D soit 3D.
Il est interdit d'avoir des discrets sur une modélisation %(k1)s.
"""),

}
