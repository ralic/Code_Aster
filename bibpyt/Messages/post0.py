#@ MODIF post0 Messages  DATE 08/04/2013   AUTEUR LADIER A.LADIER 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={
1: _(u"""
Définition incorrecte de la ligne de coupe.
"""),

2: _(u"""
Valeurs incorrectes pour VECT_Y.
"""),

3: _(u"""
Valeurs incorrectes pour VECT_Y: X colinéaire à Y.
"""),

4: _(u"""
Le vecteur Y n'est pas orthogonal à la ligne de coupe.
Le vecteur Y a été orthonormalisé pour vous.
VECT_Y=(%(r1)f,%(r2)f,%(r3)f)
"""),

5: _(u"""
Le type %(k1)s n'est pas cohérent avec le choix du repère (REPERE %(k2)s).
"""),

6: _(u"""
Définition incorrecte de COOR_ORIG et CENTRE.
"""),

7: _(u"""
Définition incorrecte de DNOR.
"""),

8: _(u"""
Attention la ligne de coupe traverse des zones sans matière :
 - Les coordonnées des points sur la ligne de coupe sont :
            %(k1)s
 - Les coordonnées des points éliminés (car hors de la matière) sont:
            %(k2)s
"""),

9: _(u"""
Nom du modèle absent dans le concept résultat %(k1)s.
"""),

10: _(u"""
Veuillez renseigner le MODELE si vous utilisez un CHAM_GD.
"""),

11: _(u"""
Dimensions de maillage et de coordonnées incohérentes.
"""),

12: _(u"""
Le mot-clé 'DNOR' est obligatoire en 3D pour le type 'ARC'.
"""),

13: _(u"""
Le GROUP_NO %(k1)s n'est pas dans le maillage %(k2)s.
"""),

14: _(u"""
Le GROUP_MA %(k1)s n'est pas dans le maillage %(k2)s.
"""),

15: _(u"""
le GROUP_MA %(k1)s contient la maille %(k2)s qui n'est pas de type SEG.
"""),

16: _(u"""
On ne peut pas combiner des lignes de coupes de type ARC
avec des lignes de coupes SEGMENT ou GROUP_NO.
"""),

17: _(u"""
Le champ %(k1)s n'est pas traité par MACR_LIGNE_COUPE en repère %(k2)s.
Le calcul est effectué en repère global.
"""),

18: _(u"""
%(k1)s est un type de champ aux éléments, non traité par PROJ_CHAMP, donc par MACR_LIGN_COUPE

Conseil : pour un champ aux points de Gauss, veuillez passer par un champ ELNO
"""),

19: _(u"""
La SD RESULTAT ne contient aucun champ pour le numéro d'ordre %(i1)d.
On ne peut pas calculer les efforts.
"""),

21: _(u"""
Le point à l'occurrence %(i1)d a une cote h= %(r1)f, donc non nulle.
Les efforts étant intégrés sur la section, on n en tient pas compte.
"""),

22: _(u"""
Le point à l'occurrence %(i1)d n a que 3 coordonnées. Pour le calcul
des déformations on doit rentrer une position dans l'épaisseur.
"""),

23: _(u"""
Le concept résultat %(k1)s ne contient pas de modèle.
On ne pourra donc pas projeter de champs aux éléments (champs de contraintes, etc.).
Pour projeter des champs aux noeuds, on utilisera directement le maillage. 
"""),

24: _(u"""
Attention la ligne de coupe %(i1)d traverse des zones sans matière :
 - Les coordonnées des points sur la ligne de coupe sont :
            %(k1)s
 - Le nombre de points éliminés (car hors de la matière) est:
            %(i2)d
"""),

25: _(u"""
La SD RESULTAT ne contient aucun champ au numéro d ordre %(i1)d.
"""),

26: _(u"""
La SD RESULTAT ne contient aucun champ à l instant %(r1)f.
"""),

33 : _(u"""
Sur certains points de la fissure %(k1)s le calcul de l'ouverture de fissure n'a pas été possible. Trois situations sont envisageables : 
      (1) Le point est sur le bord
      (2) L'endommagement maximal n'a été atteint sur la zone endommagée
      (3) La valeur BORNE_MAX est trop élevée
"""),

34: _(u"""
Pour la commande POST_ENDO_FISS, le maillage doit être contenu dans un plan parallèle aux plans XY, XZ, YZ.
"""),

35: _(u"""
Le champ pour la recherche de l'ouverture de fissure doit être un champ aux noeuds.
"""),



39 : _(u"""
En 3D, le mot-clé NORMALE doit être renseigné dans la commande DEFI_FOND_FISS.
"""),

41 : _(u"""
L'instant ou numéro d'ordre demandé n'existe pas dans le résultat et champ renseignés en entrée de la commande.
"""),

42 : _(u"""
Le champ demandé pour la recherche du trajet de fissuration n'existe pas dans le résultat renseigné en entrée de la commande.
"""),

43 : _(u"""
Si OUVERTURE='OUI' il est nécessaire de renseigner un concept RESULTAT en entrée de la commande (et pas un CHAM_GD).
"""),

44 : _(u"""
Si OUVERTURE='OUI' il est nécessaire de renseigner le mot-clé BORNE_MAX.
"""),
}
