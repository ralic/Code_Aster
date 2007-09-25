#@ MODIF contact Messages  DATE 24/09/2007   AUTEUR ABBAS M.ABBAS 
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
La méthode de résolution du contact utilisée suppose la symétrie de la
matrice du système à résoudre.
Dans le cas où votre modélisation ferait intervenir une matrice 
non-symétrique, on force sa symétrie. On émet une alarme pour vous 
en avertir. 

CONSEIL : 
Vous pouvez supprimer cette alarme en renseignant SYME='OUI' sous le 
mot-clé facteur SOLVEUR.
"""),

2: _("""
Contact methode GCP. Nombre d'itérations maximal (%(i1)s) dépassé pour le GCP.
Vous pouvez essayer d'augmenter ITER_GCP_MAXI.
La liste des noeuds présentant une interpénétration est donnée ci-dessous.
"""),

3: _("""
Contact methode GCP. Nombre d'itérations maximal (%(i1)s) dépassé pour le préconditionneur.
Vous pouvez essayer d'augmenter ITER_PRE_MAXI
"""),

6: _("""
Contact methode GCP. On ne peut utiliser le solveur GCPC avec le contact 
"""),

7: _("""
Contact methode GCP. Le pas d'avancement est negatif ; risque de comportement hasardeux de l'algorithme
"""),

9: _("""
Contact liaison glissiere. Des noeuds se decollent plus que la valeur d'ALARME_JEU:
"""),

10: _("""
Contact méthodes discrètes. Une maille maitre de type SEG a une longueur nulle. Verifiez votre maillage.
"""),

11: _("""
Contact méthodes discrètes. Le vecteur tangent défini par VECT_Y est colinéaire au vecteur normal.
"""),

12: _("""
Contact méthodes discrètes. Le vecteur normal est colinéaire au plan de projection.
"""),


14: _("""
Contact méthodes discrètes. La projection quadratique pour les triangles n'est pas disponible
"""),

15: _("""
Contact méthodes discrètes. Une maille maitre de type TRI a une surface nulle. Verifiez votre maillage.
"""),


27: _("""
Contact méthodes discrètes. On n'a pas trouve de noeud maitre proche du noeud esclave : contacter les developpeurs
"""),

30: _("""
Contact méthodes discrètes. On ne sait pas traiter ce type de maille
"""),


32: _("""
Contact méthodes discrètes. Pas de lissage des normales possible avec l'appariement nodal : contacter les developpeurs
"""),

38: _("""
Contact. Erreur dans la definition symetrique : contacter les developpeurs
"""),

54: _("""
Contact méthodes discrètes. On ne peut pas utiliser une direction d'appariement fixe VECT_NORM_ESCL si l'appariement n'est pas NODAL.
"""),

55: _("""
Contact méthodes discrètes. La commande VECT_Y n'est utilisable qu'en 3D.
"""),

56: _("""
Contact méthodes discrètes. La commande VECT_ORIE_POU n'est utilisable qu'en 3D.
"""),


60: _("""
Contact méthodes discrètes. Vous utilisez des mailles de type SEG2/SEG3 en 3D sans definir un repere pour l'appariement. Voir les mots-clefs VECT_Y et VECT_ORIE_POU.
"""),

75: _("""
Contact méthodes discrètes. Un POI1 ne peut pas etre une maille maitre.
"""),

76: _("""
Contact. On ne peut pas avoir plus de 3 ddls impliques dans la meme relation unilaterale : contacter les developpeurs
"""),

83: _("""
Contact. Il y a plusieurs charges contenant des conditions de contact.
"""),

84: _("""
Contact. Melange 2d et 3d dans le contact.
"""),

85: _("""
Contact. Melange dimensions maillage dans le contact.
"""),

86: _("""
Contact. Code methode contact incorrect : contacter les developpeurs
"""),

87: _("""
Contact. La norme tangentielle de frottement est negative: contacter les developpeurs
"""),

88: _("""
Contact. Ne pas utiliser REAC_INCR=0 avec le frottement.
"""),

93: _("""
Contact methode VERIF.
 -> Interpénétrations des surfaces.
 -> Risque & Conseil :
    Vérifier si le niveau d'interpénétration des surfaces est acceptable dans
    votre problème.
"""),

96: _("""
Contact méthode continue. Pour l'option SANS_GROUP_NO_FR, il faut que le frottement soit activé.
"""),

97: _("""
Contact méthode continue. Pour l'option SANS_GROUP_NO et SANS_GROUP_NO_FR, l'intégration aux noeuds est obligatoire.
"""),



}
