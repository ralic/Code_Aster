#@ MODIF contact Messages  DATE 13/03/2007   AUTEUR ABBAS M.ABBAS 
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

6: _("""
 Contact methode GCP. On ne peut utiliser le solveur gcpc avec le contact 
"""),

7: _("""
 Contact methode GCP. Denom est negatif : contacter les developpeurs
"""),

8: _("""
 Contact methode GCP. Le pas d'avancement est negatif : contacter les developpeurs
"""),

9: _("""
 Contact liaison glissiere. Des noeuds se decollent plus que la valeur d'ALARME_JEU:
"""),

10: _("""
 Contact methodes discretes. Une maille maitre de type SEG a une longueur nulle. Verifiez votre maillage.
"""),

11: _("""
 Contact methodes discretes. Le vecteur tangent defini par VECT_Y est colineaire au vecteur normal.
"""),

12: _("""
 Contact methodes discretes. Le vecteur normal est colineaire au plan de projection.
"""),

13: _("""
 Contact methodes discretes. Il faut reactualiser la projection : contacter les developpeurs
"""),

14: _("""
 Contact methodes discretes. La projection quadratique pour les triangles n'est pas disponible
"""),

15: _("""
 Contact methodes discretes. Une maille maitre de type TRI a une surface nulle. Verifiez votre maillage.
"""),

22: _("""
 Contact methodes discretes. Cette methode d'appariement n'existe pas : contacter les developpeurs.
"""),

23: _("""
 Contact methodes discretes. Erreur d'appel par l'option d'appariement n'existe pas : contacter les developpeurs.
"""),

24: _("""
 Contact methodes discretes. Erreur de dimensionnement nombre maximal de noeuds esclaves depasse : contacter les developpeurs
"""),

25: _("""
 Contact methodes discretes. Erreur de dimensionnement des tableaux apcoef et apddl : contacter les developpeurs
"""),

26: _("""
 Contact methodes discretes. Erreur du nombre de noeuds esclaves sur la zone : contacter les developpeurs
"""),

27: _("""
 Contact methodes discretes. On n'a pas trouve de noeud maitre proche du noeud esclave : contacter les developpeurs
"""),

29: _("""
 Contact methodes discretes. La recherche nodale par boites n'est pas operationnel
"""),

30: _("""
 Contact methodes discretes. On ne sait pas traiter ce type de maille
"""),

31: _("""
 Contact methodes discretes. Le noeud esclave n'a pas pu s'apparier avec la maille quadrangle : contacter les developpeurs
"""),

32: _("""
 Contact methodes discretes. Pas de lissage des normales possible avec l'appariement nodal : contacter les developpeurs
"""),

38: _("""
 Contact. Erreur dans la definition symetrique : contacter les developpeurs
"""),

44: _("""
 Contact methode CONTINUE. Ne correspond a aucune methode du mot cle FORMULATION.
"""),

45: _("""
 Contact methode CONTINUE. Ne correspond a aucune methode du mot cle INTEGRATION.
"""),

46: _("""
 Contact methode CONTINUE. Ne correspond a aucune methode du mot cle FROTTEMENT.
"""),

47: _("""
 Contact methodes discretes. Ne correspond a aucune methode du mot cle METHODE.            
"""),

48: _("""
 Contact. Les methodes de contact sont differentes pour les zones de contact.
"""),

49: _("""
 Contact. Ne correspond a aucune methode du mot cle APPARIEMENT.
"""),

50: _("""
 Contact. Ne correspond a aucune methode du mot cle PROJECTION.             
"""),

51: _("""
 Contact. Ne correspond a aucune methode du mot cle RECHERCHE.
"""),

52: _("""
 Contact. Ne correspond a aucune methode du mot cle LISSAGE.
"""),

53: _("""
 Contact. Ne correspond a aucune methode du mot cle NORMALE.
"""),

54: _("""
 Contact. On ne peut pas utiliser une direction d'appariement fixe si l'appariement n'est pas nodal.
"""),

55: _("""
 Contact. La commande VECT_Y n'intervient pas en 2D.
"""),

56: _("""
 Contact. La commande VECT_ORIE_POU n'intervient pas en 2D.
"""),

57: _("""
 Contact. Ne correspond a aucune methode du mot cle REAC_GEOM.
"""),

58: _("""
 Contact. Ne correspond a aucune methode du mot cle STOP_SINGULIER.
"""),

59: _("""
 Contact. Ne correspond a aucune methode du mot cle SANS_NOEUD_QUAD.
"""),

60: _("""
 Contact methodes discretes. Vous utilisez des mailles de type SEG2/SEG3 en 3D sans definir un repere pour l'appariement. Voir les mots-clefs VECT_Y et VECT_ORIE_POU.
"""),

74: _("""
 Contact. Erreur de dimensionnement car le nombre de noeuds est superieur a 9 : contacter les developpeurs
"""),

75: _("""
 Contact. Un POI1 ne peut pas etre une maille maitre.
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

94: _("""
 Operation d'appariement inconnue : contacter les developpeurs
"""),

97: _("""
 Contact methode CONTINUE. Pour l'option SANS_GROUP_NO, l'intégration aux noeuds est obligatoire.
"""),
}
