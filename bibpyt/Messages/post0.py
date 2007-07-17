#@ MODIF post0 Messages  DATE 17/07/2007   AUTEUR REZETTE C.REZETTE 
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
Valeurs incorrectes pour VECT_Y: x colinéaire a y.
"""),


4: _("""
Le vecteur Y n'est pas orthogonal a la ligne de coupe.
Le vecteur Y a ete orthonormalise pour vous.
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
Le group_no %(k1)s n'est pas dans le maillage %(k2)s.
"""),

14: _("""
Le group_ma %(k1)s n'est pas dans le maillage %(k2)s.
"""),

15: _("""
le group_ma %(k1)s contient la maille %(k2)s qui n'est pas de type SEG.
"""),

16: _("""
On ne peut pas combiner des lignes de coupes de type ARC
avec des lignes de coupes SEGMENT ou GROUP_NO.
"""),

17: _("""
Le champ %(k1)s n'est pas traité par macr_ligne_coupe en repère %(k2)s.
Le calcul est effectué en repère global.
"""),

18: _("""
Redéfinition du DISPLAY vers %(k1)s.
"""),

19: _("""
Stanley fonctionne en mode validation de non-regression.
"""),

20: _("""
Aucune variable d'environnement DISPLAY définie !
STANLEY ne pourra pas fonctionner. On l'ignore.

Si vous etes en Interactif, cochez le bouton Suivi Interactif
dans ASTK.

Vous pouvez également préciser votre DISPLAY dans les arguments
de la commande STANLEY :

STANLEY(DISPLAY='adresse_ip:0.0');
"""),

}
