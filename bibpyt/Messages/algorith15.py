# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg = {

1 : _(u"""
 arrêt sur nombres de DDL interface non identiques
 nombre de ddl interface droite:  %(i1)d
 nombre de ddl interface gauche:  %(i2)d
"""),

2 : _(u"""
 arrêt sur dimension matrice TETA incorrecte
 dimension effective :  %(i1)d
 dimension en argument:  %(i2)d
"""),

3 : _(u"""
  erreur de répétitivité cyclique
"""),

4 : _(u"""
  il manque un DDL sur un noeud gauche
  type du DDL  -->  %(k1)s
  nom du noeud -->  %(k2)s
"""),



6 : _(u"""
  il manque un DDL sur un noeud droite
  type du ddl  -->  %(k1)s
  nom du noeud -->  %(k2)s
"""),

7 : _(u"""
 arrêt sur problème de répétitivité cyclique
"""),

8 : _(u"""
 la composante : %(k1)s  est une composante indéfinie
"""),




10 : _(u"""
 arrêt sur type de DDL non défini
"""),

11 : _(u"""
 "NB_POIN" est inférieur au nombre de points de l'interspectre.
 le spectre est tronqué à la fréquence :  %(r1)f
"""),

12 : _(u"""
 le "NB_POIN" donné est modifié
 (en une puissance de 2 compatible avec l'interspectre)
 le "NB_POIN" retenu est :   %(i1)d
"""),

13 : _(u"""
 la durée est trop grande ou NB_POIN et trop petit par rapport
 à la fréquence max (théorème de Shannon).
 on choisit NBPOIN =  %(i1)d
"""),

14 : _(u"""
 la durée est petite par rapport au pas de discrétisation de l'interspectre.
 choisir plutôt : durée >  %(r1)f
"""),

15 : _(u"""
 "NB_POIN" est petit par rapport au pas de discrétisation de l'interspectre.
 NB_POIN =  %(i1)d
 il faudrait un nombre supérieur à :  %(r1)f
"""),

16 : _(u"""
 on n'a pas trouve le DDL pour le noeud :  %(k1)s
"""),

17 : _(u"""
    de la sous-structure :  %(k1)s
"""),

18 : _(u"""
    et sa composante :  %(k1)s
"""),

19 : _(u"""
  il manque le seuil  pour la fonction interprétée  %(k1)s
"""),

20 : _(u"""
 l'abscisse linéaire est nulle pour la courbe :  %(k1)s
 abscisse :  %(r1)f
"""),







24 : _(u"""
 au moins un terme de ALPHA est négatif à l'abscisse :  %(i1)d
"""),

25 : _(u"""
 ALPHA est nul et le nombre de mesures est strictement inférieur au nombre de modes
 risque de matrice singulière
"""),

26 : _(u"""
 calcul moindre norme
"""),

27 : _(u"""
  problème calcul valeurs singulières
  pas      =   %(i1)d
  abscisse =    %(r1)f
"""),

28 : _(u"""
  la matrice (PHI)T*PHI + ALPHA n'est pas inversible
  pas      =   %(i1)d
  abscisse =    %(r1)f
"""),






















45 : _(u"""
  on ne trouve pas DPMAX
"""),

46 : _(u"""
  nombre d'itérations insuffisant
"""),

47 : _(u"""
  F(XMIN) > 0
"""),

48 : _(u"""
  maille :  %(k1)s
  nombre d itérations =  %(i1)d
  ITER_INTE_MAXI =  %(i2)d
"""),

49 : _(u"""
  DP    actuel =  %(r1)f
  F(DP) actuel =  %(r2)f
"""),

50 : _(u"""
  DP    initial   =  %(r1)f
  F(DP) initial   =  %(r2)f
"""),

51 : _(u"""
  DP    maximum   =  %(r1)f
  F(DP) maximum   =  %(r2)f
"""),

52 : _(u"""
  allure de la fonction
  nombre points :  %(i1)d
"""),

53 : _(u"""
  DP     =  %(r1)f
  F(DP)  =  %(r2)f
"""),




55 : _(u"""
  incohérence détectée
"""),

56 : _(u"""
  le noeud :  %(k1)s  de l'interface dynamique :  %(k2)s
  n'appartient pas la sous-structure:  %(k3)s
"""),





58 : _(u"""
  le noeud :  %(k1)s  de l interface dynamique :  %(k2)s
  n'est pas correctement référencé dans le squelette :  %(k3)s
"""),

59: _(u"""
  Le nombre de secteur doit être supérieur ou égal à 2 (mot clé NB_SECTEUR)
"""),

60 : _(u"""
  le noeud :  %(k1)s  de l'interface dynamique :  %(k2)s
  n'appartient pas la sous-structure:  %(k3)s
"""),




62 : _(u"""
  le noeud :  %(k1)s  de l'interface dynamique :  %(k2)s
  n'est pas correctement référencé dans le squelette :  %(k3)s
"""),

63 : _(u"""
  conflit mot clés TOUT et GROUP_NO dans RECO_GLOBAL
"""),

64 : _(u"""
  erreur de nom
  la sous-structure :  %(k1)s  n a pas été trouvée
"""),

65 : _(u"""
  incohérence de nom
  l interface dynamique  :  %(k1)s
  de la sous-structure   :  %(k2)s
  a pour groupe de noeud :  %(k3)s
  or GROUP_NO_1 =  %(k4)s
"""),

66 : _(u"""
  erreur de nom
  la sous-structure :  %(k1)s  n'a pas été trouvée
"""),

67 : _(u"""
  incohérence de nom
  l interface dynamique  :  %(k1)s
  de la sous-structure   :  %(k2)s
  a pour groupe de noeud :  %(k3)s
  or GROUP_NO_2 =  %(k4)s
"""),

68 : _(u"""
 nombre de points pas période             :  %(i1)d
 coefficient de remontée du pas de temps  :  %(r1)f
 coefficient de division du pas de temps  :  %(r2)f
 pas de temps minimal                     :  %(r3)f
 pas de temps maximal                     :  %(r4)f
 nombre maximal de réductions du pas      :  %(i2)d
 vitesse minimale variable                :  %(k1)s
"""),

69 : _(u"""
 nombre incorrect de sous-structures
 il vaut :  %(i1)d
 alors que le nombre total de sous-structures vaut :  %(i2)d
"""),

70 : _(u"""
 nombre incorrect de sous-structures
 pour le chargement numéro : %(i1)d
 il en faut exactement :  %(i2)d
 vous en avez          :  %(i3)d
"""),

71 : _(u"""
 nombre incorrect de vecteurs chargements
 pour le chargement numéro : %(i1)d
 il en faut exactement :  %(i2)d
 vous en avez          :  %(i3)d
"""),

72 : _(u"""
 un PROF_CHNO n'est pas défini
 il manque pour le chargement : %(k1)s
"""),

73 : _(u"""
 on doit avoir le même type de forces pour un même chargement global
 or, la grandeur vaut   :  %(i1)d
 pour la sous-structure    %(k1)s
 et elle vaut           :  %(i2)d
 pour la sous-structure    %(k2)s
"""),

74 : _(u"""
 une des bases modales a un type incorrect
 elle est associée à la sous-structure  %(k1)s
"""),

75 : _(u"""
 les numérotations ne coïncident pas pour la sous-structure : %(k1)s
 le PROF_CHNO pour la base modale est :  %(k2)s
 et celui pour le second membre       :  %(k3)s
"""),

85 : _(u"""
  L'interface de droite %(k1)s n'existe pas
  Conseil: vérifiez si vous avez défini cette interface dans le modèle
"""),

86 : _(u"""
  l'interface de gauche %(k1)s n'existe pas
  Conseil: vérifiez si vous avez défini cette interface dans le modèle
"""),

87 : _(u"""
  l'interface axe %(k1)s n'existe pas
  Conseil: vérifiez si vous avez défini cette interface dans le modèle
"""),

88 : _(u"""
 arrêt sur problème interfaces de type différents
"""),

89 : _(u"""
 arrêt sur problème de type interface non supporté
 type interface -->  %(k1)s
"""),

90 : _(u"""
 le nombre d'amortissements réduits est trop grand
 le nombre de modes propres vaut  %(i1)d
 et le nombre de coefficients  :  %(i2)d
 on ne garde donc que les  %(i3)d premiers coefficients
"""),

91 : _(u"""
 le nombre d'amortissements réduits est insuffisant
 il en manque :  %(i1)d
 car le nombre de modes vaut :  %(i2)d
 on rajoute %(i3)d coefficients avec la valeur du dernier coefficient.
"""),

92 : _(u"""
 Nombre de modes propres calculés insuffisant.
"""),

93 : _(u"""
 MODE_MECA : %(k1)s
"""),

94 : _(u"""
 Nombre de modes propres limités à : %(i1)d
"""),

95 : _(u"""
 l'entrée d'amortissements réduits est incompatible
 avec des matrices de type  %(k1)s
 Il faut des matrices de type MATR_ASSE_GENE_*
"""),

96 : _(u"""
 le nombre d'amortissements réduits est trop grand
 le nombre de modes propres vaut  %(i1)d
 et le nombre de coefficients :  %(i2)d
 on ne garde donc que les  %(i3)d premiers coefficients

"""),

97 : _(u"""
 le nombre d'amortissements réduits est insuffisant
 il en manque :  %(i1)d
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d amortissement réduits avec la valeur du dernier mode propre.
"""),

98 : _(u"""
 incohérence dans le DATASET 58
 le nombre de valeurs fournies ne correspond pas au nombre de valeurs attendues
 mesure concernée :  %(i1)d

"""),

99 : _(u"""
 le nombre maximum d'itérations  %(i1)d  est atteint sans converger
 le résidu relatif final est  : %(r1)f
 l instant de calcul vaut     : %(r2)f

"""),

}
