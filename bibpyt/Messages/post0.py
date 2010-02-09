#@ MODIF post0 Messages  DATE 08/02/2010   AUTEUR DESROCHES X.DESROCHES 
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

def _(x) : return x

cata_msg={
1: _("""
Définition incorrecte de la ligne de coupe.
"""),

2: _("""
Valeurs incorrectes pour VECT_Y.
"""),

3: _("""
Valeurs incorrectes pour VECT_Y: X colinéaire à Y.
"""),

4: _("""
Le vecteur Y n'est pas orthogonal à la ligne de coupe.
Le vecteur Y a été orthonormalisé pour vous.
VECT_Y=(%(r1)f,%(r2)f,%(r3)f)
"""),

5: _("""
Le type %(k1)s n'est pas cohérent avec le choix du repère (REPERE %(k2)s).
"""),

6: _("""
Définition incorrecte de COOR_ORIG et CENTRE.
"""),

7: _("""
Définition incorrecte de DNOR.
"""),

8: _("""
Attention la ligne de coupe traverse des zones sans matière :
 - Les coordonnées des points sur la ligne de coupe sont :
            %(k1)s
 - Les coordonnées des points éliminés (car hors de la matière) sont:
            %(k2)s
"""),

9: _("""
Nom du modèle absent dans le concept résultat %(k1)s.
"""),

10: _("""
Veuillez renseigner le MODELE si vous utilisez un CHAM_GD.
"""),

11: _("""
Dimensions de maillage et de coordonnées incohérentes.
"""),

12: _("""
Le mot-clé 'DNOR' est obligatoire en 3D pour le type 'ARC'.
"""),

13: _("""
Le GROUP_NO %(k1)s n'est pas dans le maillage %(k2)s.
"""),

14: _("""
Le GROUP_MA %(k1)s n'est pas dans le maillage %(k2)s.
"""),

15: _("""
le group_ma %(k1)s contient la maille %(k2)s qui n'est pas de type SEG.
"""),

16: _("""
On ne peut pas combiner des lignes de coupes de type ARC
avec des lignes de coupes SEGMENT ou GROUP_NO.
"""),

17: _("""
Le champ %(k1)s n'est pas traité par MACR_LIGNE_COUPE en repère %(k2)s.
Le calcul est effectué en repère global.
"""),

18: _("""
%(k1)s est un type de champ aux éléments, non traité par PROJ_CHAMP, donc par MACR_LIGN_COUPE

Conseil : pour un champ aux points de Gauss, veuillez passer par un champ ELNO
"""),

19: _("""
La SD RESULTAT ne contient aucun champ pour le numéro d'ordre %(i1)d.
On ne peut pas calculer les efforts.
"""),

20: _("""
La SD RESULTAT ne contient aucun champ à l instant %(r1)f.
On ne peut pas calculer les efforts.
"""),

21: _("""
Le point à l occurence %(i1)d a une côte h= %(r1)f, donc non nulle.
Les efforts étant intégrés sur la section, on n en tient pas compte.
"""),

22: _("""
Le point à l occurence %(i1)d n a que 3 coordonnées. Pour le calcul
des déformations on doit rentrer une position dans l'épaisseur.
"""),

23: _("""
La position dans l'épaisseur du point à l occurence %(i1)d est %(r1)f.
Elle doit être comprise entre -1. et 1. (on divise par la demi-épaisseur)
"""),

}
