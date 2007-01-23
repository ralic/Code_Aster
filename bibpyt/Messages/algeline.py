#@ MODIF algeline Messages  DATE 23/01/2007   AUTEUR ABBAS M.ABBAS 
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
 option invalide !
"""),

2: _("""
 probleme objet  %(k1)s .fetg . il est de longueur impaire !
"""),

3: _("""
 pb division par zerodans la construction du alpha !
"""),

4: _("""
 valeur inferieure a la tolerance
"""),

5: _("""
 pour l'instant, on ne traiteque le cas des matrices reelles.
"""),

7: _("""
 Impossible de récupérer les valeurs dans la table.
"""),

8: _("""
 attention
"""),

9: _("""
 l'origine de l'obstacle est mal positionnee par rapport au noeud de choc de numero  %(k1)s , de nom  %(k2)s , par rapport au jeu.
"""),

10: _("""
 l'origine de l'obstacle est mal positionnee par rapport au noeud de choc de numero  %(k1)s , de nom  %(k2)s , dans le plan normal au choc.
"""),

11: _("""
 la normale a l'obstacle fait un angle nul avec le noeud de choc de numero  %(k1)s , avec l'axe du tube.
"""),

12: _("""
 la normale a l'obstacle fait un angle inferieur a 10 degres au noeud de choc de numero  %(k1)s , avec l'axe du tube.
"""),

13: _("""
 la normale a l'obstacle fait un angle inferieur a 45 degres au noeud de choc de numero  %(k1)s , avec l'axe du tube.
"""),

14: _("""
 les mailles doivent etre de  type quad4 ou tri3 et non de type  %(k1)s 
"""),

15: _("""
 l'angle au noeud  %(k1)s  forme par le vecteur normal de la maille  %(k2)s  et le vecteur normal de la maille  %(k3)s  est superieur a 90 degres et vaut  %(k4)s  degres.
"""),

16: _("""
 pref_noeud est trop long ou pref_nume est trop grand
"""),

17: _("""
 pref_maille est trop long ou pref_nume est trop grand
"""),

18: _("""
 mot-cle facteur  %(k1)s  non traite
"""),

19: _("""
 le group_no  %(k1)s  n'existe pas
"""),

20: _("""
 le nombre de noeuds n'est pas le meme pour les deux group_no
"""),

21: _("""
 les group_no ne contiennent qu'un seul noeud
"""),

22: _("""
 creation quad4 degenere
"""),

23: _("""
  le noeud  %(k1)s  n est pas equidistant des  noeuds  %(k2)s  et  %(k3)s  pour la maille : %(k4)s . ameliorez le  maillage. on arrete pour eviter des resultats faux
"""),

24: _("""
 valeur negative ou nulle pour la puissance quatrieme du nombre d ondes => valeur de l ordre de coque mal determinee => il faut affiner le maillage sur les coques : reduire le pas angulaire pour definir plus de noeuds sur les contours
"""),

25: _("""
 nombre de noeuds sur la generatrice inferieur a 4 donc insuffisant pour determiner les coefficients de la deformee axiale
"""),

26: _("""
 deplacement radial maximum nul sur la generatrice
"""),

27: _("""
  -> Il y a au moins un point d'une zone dont la vitesse réduite locale est
     extérieure à la zone des vitesses réduites explorées expérimentalement.
  -> Risque & Conseil :
     Les valeurs sont extrapolees en dehors des donnees d'essais.
     Les resultats du calcul seront a prendre avec circonspection.
"""),

28: _("""
 type de pas inconnu
"""),

29: _("""
 type inconnu
"""),

30: _("""
 somme des carres des termes diagonaux nulle => critere indefini
"""),

31: _("""
 somme des carres des termes diagonaux negligeable => critere indefini
"""),

32: _("""
 cham_cine /= o. sur des ddls non elimines.
"""),

33: _("""
 la carte des caracteristiques geometriques des elements de poutre n existe pas
"""),

34: _("""
 caracteristiques geometriques elementaires de poutre non definies pour la maille  %(k1)s 
"""),

35: _("""
 l une ou l autre des composantes <r1> et <r2> n existe pas dans le champ de la grandeur
"""),

36: _("""
 la section de l element de poutre considere n est pas circulaire
"""),

37: _("""
 rayon exterieur nul a l une ou l autre des extremites de l element considere
"""),

38: _("""
 le rayon exterieur n est pas constant sur l element considere
"""),

39: _("""
 arret justifie par le message ci-dessus.
"""),

40: _("""
 option de calcul non prevue !
"""),

41: _("""
 option non prevue en non compile mpi
"""),

42: _("""
 les vitesses reduites des fichiers .70 et .71 ne sont pas coherentes
"""),

43: _("""
 les vitesses etudiees doivent etre strictement positives. le sens de l ecoulement est defini par le choix de la configuration experimentale grappe2 de reference
"""),

44: _("""
 seuls les cas d'enceintes circulaires et rectangulaires sont traites.
"""),

45: _("""
 le nombre total de tubes ne correspond pas a la somme des tubes des groupes d'equivalence 
"""),

46: _("""
 la direction des tubes n est pas parallele a l un des axes.
"""),

47: _("""
 la direction des tubes n est la meme que celle de l axe directeur.
"""),

48: _("""
 les vitesses etudiees doivent toutes etre du meme signe, sinon il y a ambiguite sur les positions d entree/sortie
"""),

49: _("""
 nombre de noeuds insuffisant sur la coque interne
"""),

50: _("""
 coque interne de longueur nulle
"""),

51: _("""
 nombre de noeuds insuffisant sur la coque externe
"""),

52: _("""
 coque externe de longueur nulle
"""),

53: _("""
 le domaine de recouvrement des coques interne et externe n existe pas
"""),

54: _("""
 la carte des caracteristiques geometriques des elements de coque n existe pas. il faut prealablement affecter ces caracteristiques aux groupes de mailles correspondant aux coques interne et externe, par l operateur <affe_cara_elem>
"""),

55: _("""
 ce programme suppose que la grandeur "cacoqu" a 2 entiers codes
"""),

56: _("""
 les caracteristiques des elements de coque n ont pas ete affectees distinctement a l un ou(et) l autre des groupes de mailles associes aux coques interne et externe
"""),

57: _("""
 la composante <ep> n existe pas dans le champ de la grandeur
"""),

58: _("""
 pas d epaisseur affectee aux elements de la coque interne
"""),

59: _("""
 epaisseur de la coque interne nulle
"""),

60: _("""
 pas d epaisseur affectee aux elements de la coque externe
"""),

61: _("""
 epaisseur de la coque externe nulle
"""),

62: _("""
 incoherence dans la definition de la configuration : le rayon d une des coques est nul
"""),

63: _("""
 incoherence dans la definition de la configuration : jeu annulaire negatif ou nul
"""),

64: _("""
 element  %(k1)s  non traite
"""),

65: _("""
 on ne peut depasser  %(k1)s  mailles
"""),

66: _("""
 coefficient de type non prevu
"""),

67: _("""
 caract. de matrice non prevue
"""),

68: _("""
 la zone d excitation du fluide, de nom  %(k1)s , est reduite a un point.
"""),

69: _("""
 la zone d excitation du fluide, de nom  %(k1)s , recoupe une autre zone.
"""),

70: _("""
 le noeud d application de l excitation doit appartenir a deux mailles, ni plus ni moins
"""),

71: _("""
 le noeud d application de l excitation est situe a la jonction de deux elements de diametres exterieurs differents => ambiguite pour le dimensionnement de l excitation
"""),

72: _("""
 autres configurations non traitees
"""),

73: _("""
 le cylindre  %(k1)s  n a pas un axe rectiligne
"""),

74: _("""
 erreur dans carces
"""),

75: _("""
 la composante  n'est pas dans le chamelem
"""),

76: _("""
 resolution impossible matrice singuliere, peut etre a cause des erreurs d arrondis
"""),

77: _("""
 erreur dans l'inversion de la masse
"""),

78: _("""
 ererur dans la recherche des valeurs propres - pas de convergence de l algorithme qr 
"""),

79: _("""
 le nombre de modes resultats:  %(k1)s  n est pas correct 
"""),

80: _("""
 les cylindres  %(k1)s  et  %(k2)s  se touchent
"""),

81: _("""
 le cylindre  %(k1)s deborde de l enceinte circulaire
"""),

82: _("""
 pas de groupes  de noeuds a creer 
"""),

83: _("""
 la grille numero  %(k1)s  deborde du domaine de definition du faisceau
"""),

84: _("""
 les grilles numero  %(k1)s  et numero  %(k2)s  se recouvrent
"""),

85: _("""
 cas d enceintes circulaire etet rectangulaire seulement 
"""),

86: _("""
 pas de groupe de mailles sous la racine commune  %(k1)s 
"""),

87: _("""
 pas de groupes de mailles sous la racine commune 
"""),

88: _("""
 un cote de l enceinte est delongueur nulle
"""),

89: _("""
 les quatres sommets de l enceinte ne forment pas un rectangle
"""),

90: _("""
 le cylindre  %(k1)s  deborde de l enceinte rectangulaire
"""),

91: _("""
  la renumerotation  %(k1)s  est incompatible avec le solveur multi_frontal.
"""),

92: _("""
 absence de relation de comportement de type <elas> pour le materiau constitutif de la coque interne
"""),

93: _("""
 absence d un ou de plusieurs parametres de la relation de comportement <elas> pour le materiau constitutif de la coque interne
"""),

94: _("""
 valeur du module d young nulle pour le materiau constitutif de la coque interne
"""),

95: _("""
 absence de relation de comportement de type <elas> pour le materiau constitutif de la coque externe
"""),

96: _("""
 absence d un ou de plusieurs parametres de la relation de comportement <elas> pour le materiau constitutif de la coque externe
"""),

97: _("""
 valeur du module d young nulle pour le materiau constitutif de la coque externe
"""),

98: _("""
 les deux coques interne et externe sont en mouvement pour le  %(k1)s ieme mode.
"""),

99: _("""
 non convergence pour le calcul des modes en eau au repos
"""),
}
