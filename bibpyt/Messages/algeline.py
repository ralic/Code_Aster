# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg={

1: _(u"""
 Résolution FETI : option invalide
"""),

2: _(u"""
 Résolution FETI : problème objet  %(k1)s .FETG . il est de longueur impaire
"""),

3: _(u"""
 Résolution FETI : division par zéro dans la construction du alpha
"""),

4: _(u"""
 valeur inférieure à la tolérance
"""),

5: _(u"""
 Combinaison linéaire de matrices :
 pour l'instant, on ne traite que le cas des matrices réelles
"""),

6: _(u"""
 La base modale à normer est issue de DEFI_BASE_MODALE.
 Pour pouvoir mettre à jour les paramètres modaux,
 il faut donc donner les matrices de raideur et de masse.

 Conseil :
  Renseignez les mots-clés RAIDE et MASSE.
"""),

7: _(u"""
 TBLIVA : impossible de récupérer les valeurs dans la table
"""),

8: _(u"""
 La base modale est issue de DEFI_BASE_MODALE et contient des modes complexes.
 Pour normer par rapport à RIGI_GENE ou MASS_GENE,
 il faut donner une matrice d'amortissement.

 Conseil :
  Renseignez le mot-clé AMOR.
"""),

9: _(u"""
 l'origine de l'obstacle est mal positionnée par rapport au noeud de choc
 de numéro  %(k1)s , de nom  %(k2)s , par rapport au jeu.
"""),

10: _(u"""
 l'origine de l'obstacle est mal positionnée par rapport au noeud de choc
 de numéro  %(k1)s , de nom  %(k2)s , dans le plan normal au choc.
"""),

11: _(u"""
 la normale à l'obstacle fait un angle nul avec le noeud de choc
 de numéro  %(k1)s , avec l'axe du tube.
"""),

12: _(u"""
 la normale à l'obstacle fait un angle inférieur à 10 degrés au noeud de choc
 de numéro  %(k1)s , avec l'axe du tube.
"""),

13: _(u"""
 la normale à l'obstacle fait un angle inférieur à 45 degrés au noeud de choc
 de numéro  %(k1)s , avec l'axe du tube.
"""),

14: _(u"""
 les mailles doivent être de type QUAD4 ou TRI3 et non de type  %(k1)s
"""),

15: _(u"""
 l'angle au noeud  %(k1)s  formé par :
    - le vecteur normal de la maille  %(k2)s
 et - le vecteur normal de la maille  %(k3)s
 est supérieur à 90 degrés et vaut  %(k4)s  degrés.
"""),

16: _(u"""
 PREF_NOEUD est trop long ou PREF_NUME est trop grand
"""),

17: _(u"""
 PREF_MAILLE est trop long ou PREF_NUME est trop grand
"""),

18: _(u"""
 mot-clé facteur  %(k1)s  non traité
"""),

19: _(u"""
 le GROUP_NO  %(k1)s  n'existe pas
"""),

20: _(u"""
 le nombre de noeuds n'est pas le même pour les deux GROUP_NO
"""),

21: _(u"""
 les GROUP_NO ne contiennent qu'un seul noeud
"""),

22: _(u"""
 création QUAD4 dégénéré
"""),

23: _(u"""
 le noeud  %(k1)s  n'est pas équidistant des noeuds  %(k2)s  et  %(k3)s  pour la maille : %(k4)s
 Améliorez le  maillage. Le code s'arrête pour éviter des résultats faux.
 - distance n1-n3 = %(r1)g
 - distance n2-n3 = %(r2)g
 - tolérance      = %(r3)g
"""),

24: _(u"""
 valeur négative ou nulle pour la puissance quatrième du nombre d'ondes.
 La valeur de l'ordre de coque est mal déterminée.
 Il faut affiner le maillage sur les coques :
 => réduire le pas angulaire pour définir plus de noeuds sur les contours.
"""),

25: _(u"""
 Nombre de noeuds sur la génératrice inférieur à 4 :
 c'est insuffisant pour déterminer les coefficients de la déformée axiale
"""),

26: _(u"""
 déplacement radial maximum nul sur la génératrice
"""),

27: _(u"""
  -> Il y a au moins un point d'une zone dont la vitesse réduite locale est
     extérieure à la zone des vitesses réduites explorées expérimentalement.
  -> Risque & Conseil :
     Les valeurs sont extrapolées en dehors des données d'essais.
     Les résultats du calcul seront a prendre avec circonspection.
"""),

28: _(u"""
 Détermination des coefficients de la déformée axiale,
 erreur relative sur la norme des déplacements radiaux : %(r1)g
"""),

29: _(u"""
 L'ordre de coque est peut-être mal identifié.
 La base modale est trop riche ou le nombre de noeuds du maillage sur une circonférence
 est trop faible
"""),

30: _(u"""
 somme des carrés des termes diagonaux nulle
 => critère indéfini
"""),

31: _(u"""
 somme des carrés des termes diagonaux négligeable
 => critère indéfini
"""),

32: _(u"""
 CHAM_CINE différent de zéro sur des DDL non éliminés.
"""),

33: _(u"""
 la carte des caractéristiques géométriques des éléments de poutre n'existe pas
"""),

34: _(u"""
 caractéristiques géométriques élémentaires de poutre non définies pour la maille  %(k1)s
"""),

35: _(u"""
 l'une ou l'autre des composantes <R1> et <R2> n'existe pas dans le champ de la grandeur
"""),

36: _(u"""
 la section de l'élément de poutre considéré n'est pas circulaire
"""),

37: _(u"""
 rayon extérieur nul à l'une ou l'autre des extrémités de l'élément considéré
"""),

38: _(u"""
 le rayon extérieur n'est pas constant sur l'élément considéré
"""),

42: _(u"""
 les vitesses réduites des fichiers .70 et .71 ne sont pas cohérentes
"""),

43: _(u"""
 les vitesses étudiées doivent être strictement positives
 le sens de l'écoulement est défini par le choix de la configuration expérimentale GRAPPE2 de référence
"""),

44: _(u"""
 seuls les cas d'enceintes circulaires et rectangulaires sont traités.
"""),

45: _(u"""
 le nombre total de tubes ne correspond pas à la somme des tubes des groupes d'équivalence
"""),

46: _(u"""
 la direction des tubes n'est pas parallèle à l'un des axes.
"""),

47: _(u"""
 la direction des tubes n'est la même que celle de l'axe directeur.
"""),

48: _(u"""
 les vitesses étudiées doivent toutes être du même signe
 sinon il y a ambiguïté sur les positions d entrée/sortie
"""),

49: _(u"""
 nombre de noeuds insuffisant sur la coque interne
"""),

50: _(u"""
 coque interne de longueur nulle
"""),

51: _(u"""
 nombre de noeuds insuffisant sur la coque externe
"""),

52: _(u"""
 coque externe de longueur nulle
"""),

53: _(u"""
 le domaine de recouvrement des coques interne et externe n'existe pas
"""),

54: _(u"""
 la carte des caractéristiques géométriques des éléments de coque n'existe pas. il faut préalablement affecter ces caractéristiques aux groupes de mailles correspondant aux coques interne et externe, par l opérateur <AFFE_CARA_ELEM>
"""),

56: _(u"""
 les caractéristiques des éléments de coque n'ont pas été affectées
 distinctement à l'un ou(et) l'autre des groupes de mailles associés
 aux coques interne et externe
"""),

57: _(u"""
 la composante <EP> n'existe pas dans le champ de la grandeur
"""),

58: _(u"""
 pas d'épaisseur affectée aux éléments de la coque interne
"""),

59: _(u"""
 épaisseur de la coque interne nulle
"""),

60: _(u"""
 pas d'épaisseur affectée aux éléments de la coque externe
"""),

61: _(u"""
 épaisseur de la coque externe nulle
"""),

62: _(u"""
 incohérence dans la définition de la configuration : le rayon d une des coques est nul
"""),

63: _(u"""
 incohérence dans la définition de la configuration :
 jeu annulaire négatif ou nul
"""),

64: _(u"""
 élément  %(k1)s  non traite
"""),

65: _(u"""
 on ne peut dépasser  %(k1)s  mailles
"""),

66: _(u"""
 coefficient de type non prévu
"""),


68: _(u"""
 la zone d excitation du fluide, de nom  %(k1)s , est réduite a un point.
"""),

69: _(u"""
 la zone d'excitation du fluide, de nom  %(k1)s , recoupe une autre zone.
"""),

70: _(u"""
 le noeud d'application de l'excitation doit appartenir à deux mailles
 ni plus ni moins
"""),

71: _(u"""
 le noeud d'application de l excitation est situe à la jonction
 de deux éléments de diamètres extérieurs différents
 => ambiguïté pour le dimensionnement de l'excitation
"""),

72: _(u"""
 autres configurations non traitées
"""),

73: _(u"""
 le cylindre  %(k1)s  n a pas un axe rectiligne
"""),

75: _(u"""
 la composante n'est pas dans le CHAM_ELEM
"""),

76: _(u"""
 résolution impossible matrice singulière
 peut être à cause des erreurs d'arrondis
"""),

77: _(u"""
 erreur dans l'inversion de la masse
"""),

78: _(u"""
 erreur dans la recherche des valeurs propres
 pas de convergence de l algorithme QR
"""),

79: _(u"""
 le nombre de modes résultats:  %(k1)s  n'est pas correct
"""),

80: _(u"""
 les cylindres  %(k1)s  et  %(k2)s  se touchent
"""),

81: _(u"""
 le cylindre  %(k1)s déborde de l'enceinte circulaire
"""),

82: _(u"""
 pas de groupes de noeuds à créer
"""),

83: _(u"""
 la grille numéro  %(k1)s  déborde du domaine de définition du faisceau
"""),

84: _(u"""
 les grilles numéro  %(k1)s  et numéro  %(k2)s  se recouvrent
"""),

85: _(u"""
 cas d enceintes circulaire et rectangulaire seulement
"""),

86: _(u"""
 pas de groupe de mailles sous la racine commune  %(k1)s
"""),

87: _(u"""
 pas de groupes de mailles sous la racine commune
"""),

88: _(u"""
 une cote de l'enceinte est de longueur nulle
"""),

89: _(u"""
 les quatre sommets de l'enceinte ne forment pas un rectangle
"""),

90: _(u"""
 le cylindre  %(k1)s  déborde de l'enceinte rectangulaire
"""),

91: _(u"""
  la renumérotation  %(k1)s est incompatible avec le solveur MULT_FRONT.
"""),

92: _(u"""
 absence de relation de comportement de type <ELAS> pour le matériau constitutif de la coque interne
"""),

93: _(u"""
 absence d'un ou de plusieurs paramètres de la relation de comportement <ELAS>
 pour le matériau constitutif de la coque interne
"""),

94: _(u"""
 La valeur du module de YOUNG est nulle pour le matériau constitutif de la coque interne
"""),

95: _(u"""
 absence de relation de comportement de type <ELAS>
 pour le matériau constitutif de la coque externe
"""),

96: _(u"""
 absence d'un ou de plusieurs paramètres de la relation de comportement <ELAS>
 pour le matériau constitutif de la coque externe
"""),

97: _(u"""
 La valeur du module de YOUNG est nulle pour le matériau constitutif de la coque externe
"""),

98: _(u"""
 Les deux coques (interne et externe) sont en mouvement pour le  %(k1)s ème mode.
"""),

99: _(u"""
 non convergence pour le calcul des modes en eau au repos
"""),
}
