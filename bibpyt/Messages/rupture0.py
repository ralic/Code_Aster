#@ MODIF rupture0 Messages  DATE 17/12/2007   AUTEUR GALENNE E.GALENNE 
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
Interpolation hors du domaine (prolongement constant utilisé).
"""),

2: _("""
Le label %(k1)s doit etre présent dans la table %(k2)s
"""),

4: _("""
Le label COOR_Z doit etre présent dans la table %(k1)s
"""),

5: _("""
Il faut définir ELAS dans DEFI_MATERIAU.
"""),

6: _("""
Les proprietés matériaux, nécessaires aux calculs
des coefficients d'intensite des contraintes, ont ete obtenues a la
température de réference du materiau et non à la température calculée.
"""),

7: _("""
L operateur CALC_G -option CALC_K_G- calcule plus précisement les %(k1)s
"""),

10: _("""
Modélisation non implantée.
"""),

11: _("""
Problème à la récuperation des noeuds du fond de fissure.
"""),

12: _("""
Type de mailles du fond de fissure non défini.
"""),

13: _("""
Le group_no %(k1)s n'est pas dans le maillage.
"""),

14: _("""
Le group_no %(k1)s n'est pas dans le maillage.
"""),

15: _("""
le noeud %(k1)s n'appartient pas au fond de fissure.
"""),

16: _("""
Le mot clé RESULTAT est obligatoire pour TYPE_MAILLAGE = LIBRE.
"""),

17: _("""
Le nombre de noeuds NB_NOEUD_COUPE doit etre supérieur a 3 : 
On prend la valeur par défaut
"""),

18: _("""
Problème a la récuperation du modèle dans la sd résultat fournie.
"""),

19: _("""
Problème à la récupération des noeuds de la lèvre sup : 
vérifier que le mot-clé LEVRE_SUP est bien renseigné dans DEFI_FOND_FISS.
"""),

20: _("""
Problème à la récuperation des noeuds de la lèvre inf : 
vérifier que le mot-clé LEVRE_INF est bien renseigné dans DEFI_FOND_FISS.
"""),

21: _("""
Les noeuds ne sont pas en vis-a-vis dans le plan
perpendiculaire au noeud %(k1)s
"""),

22: _("""
Il manque des points dans le plan défini par la lèvre
supérieure et perpendiculaire au fond %(k1)s.
"""),

23: _("""
Vérifier les tangentes extremités ou
"""),

24: _("""
Augmenter PREC_NORM dans DEFI_FOND_FISS.
"""),

25: _("""
Augmenter ABSC_CURV_MAXI.
"""),

26: _("""
Il manque des points dans le plan défini par la lèvre
inférieure et perpendiculaire au fond  %(k1)s.
"""),


30: _("""
Calcul possible pour aucun noeud du fond : vérifier les donnees.
"""),

31: _("""
Problème a la récuperation du modele dans la sd résultat fournie.'
"""),

32: _("""
Différence entre le vecteur VECT_K1 et la normale au plan de la fissure %(k1)s.
On continue avec la normale au plan : (%(r1)f,%(r2)f,%(r3)f)
"""),

33: _("""
Problème dans la récuperation du saut de déplacement sur les lèvres :
vérifier que le résultat correspond bien a un calcul sur des élements x-fem 
et que le maillage fourni est bien le maillage linéaire initial.
"""),

34: _("""
Le nombre de noeuds NB_NOEUD_COUPE doit etre supérieur à 3 : 
on prend la valeur par défaut.
"""),

35: _("""
TABL_DEPL_SUP et TABL_DEPL_INF sont obligatoires si SYME_CHAR=SANS.
"""),

36: _("""
TABL_DEPL_SUP et TABL_DEPL_INF sont obligatoires si SYME_CHAR=SANS.
"""),

37: _("""
Le numéro d'ordre %(i1)d n'a pas été trouvé dans la table.
"""),

38: _("""
Pas d'instant trouvé dans la table pour l'instant %(r1)f.
"""),

39: _("""
Plusieurs instants trouvés dans la table pour l'instant %(r1)f.
"""),

40: _("""
ABSC_CURV non croissants pour TABL_DEPL_INF.'
"""),

41: _("""
ABSC_CURV non croissants pour TABL_DEPL_SUP.'
"""),

42: _("""
Différence de points entre la lèvre supérieure et la lèvre inférieure
"""),

43: _("""
Pour traiter le noeud %(k1)s:
 Nombre de points - levre superieure : %(i1)d
 Nombre de points - levre inferieure : %(i2)d
"""),

44: _("""
Les noeuds ne sont pas en VIS_A_VIS.
"""),

45: _("""
Problème dans la récupération du saut de déplacement sur les lèvres :
vérifier que le résultat correspond bien à un calcul sur des élements x-fem
et que le maillage fourni est bien le maillage linéaire initial.
"""),

46: _("""
Il faut au moins trois noeuds dans le plan défini par les lèvres
et perpendiculaire au fond de fissure.
"""),

47: _("""
Noeud %(k1)s 
"""),


49: _("""
Déplacement normal du noeud %(k1)s non nul
et SYME_CHAR = %(k2)s.
Vérifier les conditions aux limites et VECT_K1.
"""),

50: _("""
Nombre de modes différent entre la base modale
et %(k1)s : on prend le minimum des deux %(i1)d.
"""),

51: _("""
Le numéro d'ordre %(i1)d n'appartient pas au résultat %(k1)s.
"""),

52: _("""
Pas d'instant trouvé dans la table pour l'instant %(r1)f.
"""),

53: _("""
Plusieurs instants trouves dans la table pour l instant %(r1)f.
"""),

54: _("""
Aucun instant ou numéro d'ordre trouvé.
"""),

55: _("""
-> Attention:
   En présence d'une SD Résultat de type mult_elas, les mots-clés EXCIT et NOM_CAS sont obligatoires.
-> Risque & conseil:
   Risque de résultats faux si un des chargements impacte le calcul de G et de K (par exemple force de pression sur 
   les lèvres de la fissure, force volumique...).
"""),

56 : _("""
CALC_G - option CALC_K_G : le calcul est impossible sur un point de rayon nul (point sur 
l'axe de rotation).
-> Risque et Conseils
Modifier les couronnes R_INF et R_SUP pour qu'elles soient toutes les deux plus petites que
le rayon du fond de fissure. De manière générale en axisymétrie, le calcul de K est 
d'autant plus précis que le rayon des couronnes est petit devant le rayon du fond de fissure.
"""),
}
