#@ MODIF discrets Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

# Messages pour les éléments discrets non-linéaires
cata_msg={

1: _(u"""
Pour l'élément discret %(k1)s .
Il n'y a pas de rotation non-linéaire possible.
"""),

2: _(u"""
Pour l'élément discret %(k1)s .
Il n'y a pas de comportement non-linéaire possible suivant Z
ou en rotation autour de X,Y en 2D.
"""),

3: _(u"""
Pour l'élément discret %(k1)s .
Il n'y a pas de comportement non-linéaire possible en rotation
ou suivant Z en 2D.
"""),

4: _(u"""
Pour l'élément discret.
Le pas de temps est devenu trop petit : %(r1)12.5E .
"""),

5: _(u"""
Pour l'élément discret %(k5)s .
Les caractéristiques sont obligatoirement données dans le repère local du discret.

Pour information :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comportement : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

6: _(u"""
Pour les éléments discrets il faut définir un repère dans AFFE_CARA_ELEM

Pour information :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comportement : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

7 : _(u"""
Le Comportement <%(k4)s> affecté à un DISCRET est non valide
Les comportements valides sont :
   COMP_ELAS   ELAS

   COMP_INCR   ELAS           DIS_GRICRA  DIS_VISC  DIS_ECRO_CINE
               DIS_BILI_ELAS  ASSE_CORN   ARME      DIS_CHOC
               DIS_GOUJ2E

Pour information :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comportement : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

8 : _(u"""
Pour les discrets, avec COMP_ELAS le seul comportement valide est ELAS.

Pour information :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comportement : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

10 : _(u"""
Pour l'élément DISCRET de modèle <%(k1)s> la matrice de décharge est non développée.

Pour information :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comportement : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

11 : _(u"""
La loi <%(k4)s> doit être utilisée avec des éléments du type DIS_TR_L : élément SEG2 + modélisation DIS_TR

Pour information :
   Modèle   : <%(k1)s>, Option   : <%(k2)s>
   Comportement : <%(k3)s>, Relation : <%(k4)s>
   Maille   : <%(k5)s>
"""),

12 : _(u"""
La commande %(k4)s ne sait pas traiter les matrices non-symétriques, pour l'option %(k1)s.
Message de la routine %(k3)s, pour l'élément %(k2)s.
"""),

13 : _(u"""
L'élément %(k1)s est inconnu pour la maille %(k3)s.
Message de la routine %(k2)s.
"""),

14 : _(u"""
L'option %(k1)s est inconnue pour l'élément %(k2)s.
Message de la routine %(k3)s.
"""),

15 : _(u"""
L'option %(k1)s ne sait pas traiter l'élément %(k2)s.
Message de la routine %(k3)s.
"""),

16 : _(u"""
Il est interdit d'avoir des éléments discrets 2D et 3D dans un modèle.
"""),

17 : _(u"""
Votre modélisation ne comporte pas d'élément discret.
"""),

20 : _(u"""
Votre modélisation doit être soit 2D soit 3D.
Il est interdit d'avoir des discrets sur une modélisation %(k1)s.
"""),

21 :_(u"""
AFFE_CARA_ELEM/RIGI_PARASOL
  Le nombre de valeurs fournies sous VALE ne correspond pas au nombre attendu.
  Vous devez vérifier l'adéquation des dimensions des éléments sous CARA avec le nombre de valeur sous VALE.
"""),

25 :_(u"""
Vous utilisez des discrets %(k1)s alors que vous n'avez pas affecté ses caractéristiques.
Il faut vérifier les affectations sous AFFE_CARA_ELEM/DISCRET OU DISCRET_2D.
"""),

26 :_(u"""
Vous utilisez la matrice de MASSE pour un discret %(k1)s alors que vous n'avez pas affecté
les caractéristiques de masses. Par défaut la masse est nulle.
Il faut vérifier les affectations sous AFFE_CARA_ELEM/DISCRET.
"""),

27 :_(u"""
Vous utilisez la matrice de RAIDEUR pour un discret %(k1)s alors que vous n'avez pas affecté
les caractéristiques de raideurs. Par défaut la raideur est nulle.
Il faut vérifier les affectations sous AFFE_CARA_ELEM/DISCRET.
"""),

28 :_(u"""
Vous utilisez la matrice d'amortissement pour un discrets %(k1)s alors que vous n'avez pas affecté
les caractéristiques d'amortissements. Par défaut l'amortissement est nul.
Il faut vérifier les affectations sous AFFE_CARA_ELEM/DISCRET.
"""),

30 :_(u"""Informations :
   Maille de nom %(k1)s, de %(i1)d noeud(s).
   Nom et coordonnées des noeuds :
"""),
31 :_(u"""      %(k1)8s   %(r1)12.5E   %(r2)12.5E   %(r3)12.5E
"""),


40 :_(u"""
L'utilisation des discrets non-symétriques n'est actuellement pas possible pour des calculs non-linéaires.
"""),

}
