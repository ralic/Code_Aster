#@ MODIF elements Messages  DATE 23/01/2007   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
 arret du code
"""),

2: _("""
 abs(rayon2-rayon1).gt.epsi
"""),

3: _("""
 abs(theta2-theta1).gt.epsi
"""),

4: _("""
 abs(omega2-omega1).gt.epsi
"""),

5: _("""
 probleme de maillage tuyau :  pour une maille definie par les noeuds n1 n2 n3  le noeud n3 doit etre le noeud milieu
"""),

6: _("""
  gene_tuyau il faut donner un vecteur non colineaire au tuyau
"""),

7: _("""
 angle du coude trop grand
"""),

8: _("""
  mailler plus fin 
"""),

9: _("""
 il faut renseigner le coefficient e_n  dans les cas des deformations planes et de l'axisymetrie, on ne regarde donc que le cas des contraintes planes.
"""),

10: _("""
 condition non respectee
"""),

11: _("""
 la modelisation :  %(k1)s n'est pas traitee.
"""),

12: _("""
 nombre de couches obligatoirement superieur a 0 
"""),

13: _("""
 nombre de couches limite a 10 pour les coques 3d 
"""),

14: _("""
 le type d'element :  %(k1)s n'est pas prevu.
"""),

15: _("""
 la nature du materiau  %(k1)s  n'est pas traitee, seules sont considerees les natures : elas, elas_istr, elas_orth .
"""),

16: _("""
 type de maille inconnu
"""),

17: _("""
 noeuds confondus pour un element
"""),

18: _("""
 nno invalide
"""),

19: _("""
 pb1
"""),

20: _("""
 aucun type d elements necorrespond au type demande
"""),

21: _("""
 predicteur elas hors champs
"""),

22: _("""
 irep (indicateur de changement de repere) doit etre egal a 0 ou 1 
"""),

23: _("""
 piles saturees
"""),

24: _("""
 derivatives of "mp" not defined
"""),

25: _("""
 on passe en mecanisme 2
"""),

26: _("""
 chargement en meca 2 trop important a verifier
"""),

27: _("""
 on poursuit en mecanisme 2
"""),

28: _("""
 decharge negative sans passer par meca 1 diminuer le pas de temps
"""),

29: _("""
 on revient en mecanisme 1
"""),

30: _("""
 pas de retour dans meca 1 trop important diminuer le pas de temps
"""),

31: _("""
 type element  %(k1)s  incompatible avec  %(k2)s 
"""),

32: _("""
 comportement inattendu
"""),

33: _("""
 nombre d iterations > 1000 .
"""),

34: _("""
 element non traite  %(k1)s 
"""),

35: _("""
 pas d excentrement avec stat_non_line maille  %(k1)s 
"""),

36: _("""
 nombre de couches  negatif ou nul :  %(k1)s 
"""),

37: _("""
 temperature non trouvee.
"""),

38: _("""
 ! pb phesen.ne.phenom !
"""),

39: _("""
 l'axe de reference est normal a un element de plaque anisotrope
"""),

40: _("""
  -> L'axe de référence pour le calcul du repère local est normal à un
     au moins un élément de plaque.
  -> Risque & Conseil :
     Il faut modifier l'axe de référence (axe X par défaut) en utilisant
     ANGL_REP ou VECTEUR.

"""),

41: _("""
 impossibilite : vous avez un materiau de type "elas_coque" et vous n'avez pas defini la raideur de membrane, ni sous la forme "memb_l", ni sous la forme "m_llll".
"""),

42: _("""
 comportement materiau non admis
"""),

43: _("""
 impossibilite : vous avez un materiau de type "elas_coque" et le determinant de la sous-matrice de hooke relative au cisaillement est nul.
"""),

44: _("""
 unknown option demanded
"""),

45: _("""
 n < 0
"""),

46: _("""
 nombre de couches negatif ou nul
"""),

47: _("""
 pas d excentrement avec forc_noda maille  %(k1)s 
"""),

48: _("""
 impossibilite, la surface de l'element est nulle. 
"""),

49: _("""
 l'axe de reference est normal a un element de plaque - calcul option impossible - orienter ces mailles
"""),

50: _("""
 comportement elastique inexistant
"""),

51: _("""
  -> Le type de comportement %(k1)s n'est pas prévu pour le calcul de
     SIGM_ELNO_DEPL. Les seuls comportements autorisés sont :
     ELAS, ELAS_COQUE, ou ELAS_ORTH
  -> Risque & Conseil :
     Pour les autres comportements, utiliser SIEF_ELNO_ELGA (efforts)
     ou SIGM_ELNO_COQU (contraintes en un point de l'épaisseur).
"""),

52: _("""
 lorsqu'il y a variation de temperature dans l'epaisseur, utiliser "stat_non_line"
"""),

53: _("""
 probleme: temperature sur la maille: %(k1)s : il manque la composante "temp"
"""),

54: _("""
 element non prevu
"""),

55: _("""
 elrefa inconnu:  %(k1)s 
"""),

56: _("""
  erreur programmeur ecrasement de dff, dimf est inferieur au nb de noeuds * nb cmps
"""),

57: _("""
  erreur programmeur: ecrasement de ff, dimf est inferieur au nb de noeuds
"""),

58: _("""
 la nature du materiau  %(k1)s  necessite la definition du coefficient  b_endoge dans defi_materiau.
"""),

59: _("""
 bizarre :  %(k1)s 
"""),

60: _("""
 on ne sait pas traiter :  %(k1)s 
"""),

61: _("""
 axi : r=0
"""),

62: _("""
 group_ma :  %(k1)s  inconnu dans le maillage
"""),

63: _("""
 erreurs group_ma
"""),

64: _("""
  le liaison_*** de  %(k1)s  implique les noeuds physiques  %(k2)s  et  %(k3)s et traverse l'interface
"""),

65: _("""
  le liaison_*** de  %(k1)s  implique le noeud physique  %(k2)s et touche l'interface
"""),

66: _("""
 si noeud_orig : donner un group_ma ou une liste de mailles. on ne reordonne pas les group_no et les listes de noeuds. 
"""),

67: _("""
 le group_no :  %(k1)s n'existe pas.
"""),

68: _("""
 le noeud origine  %(k1)s ne fait pas parti du chemin
"""),

69: _("""
 le noeud origine  %(k1)s  n'est pas une extremite
"""),

70: _("""
 pas d'origine definie par noeud_orig ou group_no_orig
"""),

71: _("""
 echec dans la recherche du noeud origine
"""),

72: _("""
 group_no oriente : noeud origine =  %(k1)s 
"""),

73: _("""
 le group_ma :  %(k1)s n'existe pas.
"""),

74: _("""
 si le fond est une courbe fermee maille_orig ou group_ma_orig doit accompagner noeud_orig
"""),

75: _("""
 le noeud_orig n'appartient pas a la maille_orig
"""),

76: _("""
 la maille_orig %(k1)s  n'appartient pas au fond de fissure
"""),

77: _("""
 le noeud extremite  %(k1)s  n'est pas le dernier noeud
"""),

78: _("""
 group_no oriente : noeud extremite =  %(k1)s 
"""),

79: _("""
 il y a a la fois des elements volumiques de degre 1 et de degre 2 dans le modele.  on a besoin de savoir si on est en lineaire ou en quadratique pour choisir la methode de lissage.
"""),

80: _("""
 le concept fond_fiss est mal cree
"""),

81: _("""
 le .vale du cham_no dire_theta n'a pas la bonne taille
"""),

82: _("""
 l'option de lissage 'lagrang2' n'a pas ete developpee lorsque le nombre de noeuds d'un fond de fissure ferme est pair.
"""),

83: _("""
 le type des mailles des                                levres doit etre quadrangle ou triangle
"""),

84: _("""
  %(k1)s cham_no inexistant
"""),

85: _("""
 initial point nm out of domain
"""),

86: _("""
 ndicho  .gt.  10000
"""),

87: _("""
 bad definition of mp1 and mp2
"""),

88: _("""
 le fond de fissure ne doit etre defini que par un noeud ( revoir le group_no )
"""),

89: _("""
 le fond de fissure ne                             doit etre defini que par un noeud
"""),

90: _("""
 le noeud  %(k1)s  n appartient pas au maillage :  %(k2)s 
"""),

91: _("""
 mot cle fond_fiss obligatoire
"""),

92: _("""
 le fond de fissure ne doit etre defini que par un noeud ( revoir fond_fiss )
"""),

93: _("""
  %(k1)s  n'est pas un group_no ou un group_ma
"""),

94: _("""
 arret sur erreur(s) utilisateur.
"""),

95: _("""
 les mailles des levres doivent etre lineiques
"""),

96: _("""
 erreur : la levre superieure possede une maille repetee 2 fois : maille  %(k1)s . revoir les donnees
"""),

97: _("""
 erreur : la levre inferieure possede une maille repetee 2 fois : maille  %(k1)s . revoir les donnees
"""),

98: _("""
 erreur : la levre inferieure et la levre superieure ont une maille surfacique en commun. revoir les donnees
"""),

99: _("""
 probleme dans le calcul de la normale a la fissure
"""),
}
