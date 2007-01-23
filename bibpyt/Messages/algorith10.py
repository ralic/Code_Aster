#@ MODIF algorith10 Messages  DATE 23/01/2007   AUTEUR ABBAS M.ABBAS 
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
 la variable  %(k1)s  n existe pas dans la loi  %(k2)s 
"""),

2: _("""
 tailles matrices incompatibles
"""),

3: _("""
 le vecteur normal est colineaire au plan de projection
"""),

4: _("""
 on ne sait pas traiter ce type de maille
"""),

5: _("""
 le noeud esclave n a pas pu s apparier
"""),

6: _("""
 seule la methode quatre triangles est disponible pour quadrangle
"""),

7: _("""
 il faut reactualiser la projection
"""),

8: _("""
 une maille maitre a une surface nulle
"""),

9: _("""
 la projection quadratique pour les triangles n'est pas disponible
"""),

10: _("""
 taille produit matrice-vecteur incompatible
"""),

11: _("""
 le champ de deplacement n'a pas ete calcule.
"""),

12: _("""
 le champ de vitesse n'a pas ete calcule.
"""),

13: _("""
 le champ d'acceleration n'a pas ete calcule.
"""),

14: _("""
 developpement non prevu pour le mult_appui ou corr_stat.
"""),

15: _("""
 developpement non prevu pour la sous-structuration.
"""),

16: _("""
 le champ  %(k1)s  n'a pas ete calcule dans le mode_meca  %(k2)s 
"""),

17: _("""
 l'option  %(k1)s  s'aplique sur toute la structure
"""),

18: _("""
 le groupe de noeuds  %(k1)s  ne fait pas partie du maillage  %(k2)s 
"""),

19: _("""
 le noeud  %(k1)s  ne fait pas partie du maillage  %(k2)s 
"""),

20: _("""
  le comportement :  %(k1)s  n'a pas etet defini
"""),

21: _("""
 dist_refe est obligatoire a la premiere occurence de reco_global
"""),




26: _("""
 la recherche directe de la maille la plus proche n'est pas operationnelle
"""),

27: _("""
 la recherche par voisinage du passe n'est pas operationnelle
"""),

28: _("""
 la recherche par boites n'est pas operationnelle
"""),



31: _("""
 la bande de frequence retenue ne comporte pas de modes propres
"""),

32: _("""
 vous avez demande des modes qui ne sont pas calcules
"""),

33: _("""
 il n y a pas de mode statique         calcule pour le couple noeud-cmp ci dessus
"""),

34: _("""
 redecoupage demande apres non convergence locale. redecoupage global
"""),

35: _("""
 redecoupage excessif du pas de temps interne : reduisez votre pas de temps ou augmenter abs(iter_inte_pas). redecoupage global.
"""),

36: _("""
 il manque sigm_refe
"""),

37: _("""
 il manque resi_hyd1_refe
"""),

38: _("""
 il manque resi_hyd2_refe
"""),

39: _("""
 il manque resi_ther_refe
"""),

40: _("""
 vecteur nul entrainant une division par zero dans nmconv
"""),

41: _("""
 incoherence de a ou h
"""),

42: _("""
 incoherence de donnees
"""),

43: _("""
 incoherence de c, phi ou a
"""),

44: _("""
 champ 'depl' non calcule
"""),

45: _("""
 champ 'vite' non calcule
"""),

46: _("""
 champ 'acce' non calcule
"""),

47: _("""
 lecture des instants erronee
"""),

48: _("""
 axe de rotation indefini.
"""),

49: _("""
 la porosite initiale f0 ne peut etre nulle ou negative
"""),

50: _("""
 la porosite initiale f0 ne peut etre egal ou plus grand que un
"""),

51: _("""
 comportement de rousselier version petit_reac non implante en contraintes planes
"""),

52: _("""
 la porosite initiale f0 ne peut etre negative
"""),

53: _("""
 pb2, variables de pilotages
"""),

54: _("""
 rk41. erreur d integration dans runge-kutta. trop d iteration.
"""),

55: _("""
 rk42. erreur d integration dans. runge-kutta.
"""),

56: _("""
 on ne sait pas post-traiter le champ de type:  %(k1)s 
"""),

57: _("""
 choix incorrect du schema d'integration numerique pour le contact
"""),

58: _("""
 choix incorrect de l'algorithme de restriction de l'espace des mulitplicateurs
"""),

59: _("""
 forte probabilite presence modes propres multiples. calcul sensibilite limite actuellement aux modes propres simples
"""),

60: _("""
 taille vecteurs incompatible
"""),

61: _("""
 il faut definir une bande ou un nume_ordre
"""),

62: _("""
 il faut definir une "bande" ou une liste de "nume_ordre"
"""),

63: _("""
 dimension spectrale fausse
"""),

64: _("""
 l'interspectre modal est de type "acce", on ne peut que restitituer une acceleration
"""),

65: _("""
 l'interspectre modal est de type "vite", on ne peut que restitituer une vitesse
"""),

66: _("""
 l'interspectre modal est de type "depl", on ne peut pas restitituer une acceleration
"""),

67: _("""
 l'interspectre modal est de type "depl", on ne peut pas restitituer une vitesse
"""),

68: _("""
 il faut autant de "noeud"  que de "nom_cmp"
"""),

69: _("""
 il faut autant de "maille"  que de "noeud"
"""),

70: _("""
 le noeud  %(k1)s  n'existe pas.
"""),

71: _("""
 la composante  %(k1)s  du noeud  %(k2)s  n'existe pas.
"""),

72: _("""
 il faut definir une liste de mailles pour post-traiter un cham_elem
"""),

73: _("""
 la composante  %(k1)s  du noeud  %(k2)s  pour la maille  %(k3)s  n'existe pas.
"""),

74: _("""
 on ne traite pas le "poi1"
"""),

75: _("""
 type de maille non traitee
"""),

76: _("""
 mot-cle nb_bloc inoperant on prend 1 bloc
"""),

77: _("""
 element degenere
"""),

78: _("""
 nume_suivi incorrect dans suivi_ddl
"""),

79: _("""
 ddl inconnu sur le noeud ou  la maille specifiee pour le suivi
"""),

80: _("""
 option indisponible pourle suivi
"""),

81: _("""
 trop de suivis (limite a 4)
"""),

82: _("""
 pas de suivi attache a la demande d affichage
"""),

83: _("""
 trop de lignes dans le titre
"""),

84: _("""
 erreur dvt dans le type d extrema
"""),

85: _("""
 le nombre de suivi ddl est limite a 4 !
"""),

86: _("""
 melange de champs de nature differente dans le meme mot-clef facteur suivi
"""),

87: _("""
 vecteur de norme trop petite
"""),

88: _("""
 comp_elas non implante
"""),

89: _("""
 z negatif
"""),

90: _("""
 la definition de la temperature est obligatoire pour une loi de couplage de type  %(k1)s 
"""),

91: _("""
 probleme dans la definition de la saturation
"""),

92: _("""
 echec dans elimination temps
"""),

93: _("""
 il faut un nom de champ
"""),

94: _("""
 pas de champ autre que depl ou vite ou acce
"""),

95: _("""
 pour interpoler il faut fournir une liste de frequences ou instants.
"""),

96: _("""
 calcul du transitoire: pas de solution trouvee. utiliser l'option etat_stat = non.
"""),

97: _("""
 duree de l'excitation trop courte pour le calcul du transitoire.
"""),

98: _("""
 pivot nul
"""),

99: _("""
 on ne sait pas encore traiterla sous structuration en axisymetrique
"""),
}
