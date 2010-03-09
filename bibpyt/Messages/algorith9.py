#@ MODIF algorith9 Messages  DATE 09/03/2010   AUTEUR ABBAS M.ABBAS 
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

cata_msg = {

1 : _("""
 le champ de température : TEMP_INIT(NUM_INIT) n'existe pas.
"""),

2 : _("""
 CHAM_NO invalide
"""),

4 : _("""
 valeur de THETA illicite
"""),

5 : _("""
 la charge  %(k1)s  n'est pas thermique
"""),

7 : _("""
 la charge  %(k1)s  n'est pas compatible avec FONC_MULT
"""),

10 : _("""
 nombre de vecteurs demandé trop grand
 on prend tous les modes du concept MODE_MECA
"""),

12 : _("""
 borne inférieure incorrecte
"""),

16 : _("""
 le pas est nul
"""),

17 : _("""
 le nombre de pas est négatif
"""),

18 : _("""
 les matrices assemblées généralisées doivent avoir un stockage plein (cf. NUME_DDL_GENE)
"""),

19 : _("""
 COEFF_VAR_AMOR non nul et amortissement non présent
"""),

26 : _("""
 le modèle est obligatoire
"""),

27 : _("""
 impossible de combiner les mots cles CHARGE et VECT_ASSE en dehors des ondes planes
"""),

28 : _("""
 concept réentrant : "RESULTAT" doit porter le meme nom que la sortie
"""),

29 : _("""
 concept réentrant : "RESULTAT" est d'un type différent
"""),

30 : _("""
 argument en double pour "NOM_CHAM"
"""),

34 : _("""
 les matrices ne possèdent pas toutes la meme numérotation
"""),

35 : _("""
  un VECT_ASSE n'est ni à valeurs réelles, ni à valeurs complexes.
"""),

39 : _("""
 base modale et MATR_ASSE avec numérotations différentes
"""),

40 : _("""
  type de matrice inconnu:  %(k1)s
"""),

41 : _("""
 base modale et VECT_ASSE avec  numérotations différentes
"""),

42 : _("""
 la base constituée ne forme pas une famille libre
"""),

43 : _("""
 le nombre de valeurs doit etre pair.
"""),

44 : _("""
 trop d'arguments pour "NOM_CHAM"
"""),

45 : _("""
 pour calculer une ACCE_ABSOLU, il faut "ACCE_MONO_APPUI"
"""),

46 : _("""
 pour restituer sur un squelette, il faut "MODE_MECA"
"""),

47 : _("""
 mots-cles'SOUS_STRUC' et'SQUELETTE'interdits
"""),

48 : _("""
 le mot-clé 'MODE_MECA' doit etre présent
"""),

49 : _("""
 l'instant de récuperation est en dehors du domaine de calcul.
"""),

50 : _("""
 pas de mailles fournies
"""),

55 : _("""
 mauvaise définition de l'interspectre.
"""),

56 : _("""
 le "NOMB_PTS" doit etre une puissance de 2.
"""),

57 : _("""
 si les mots-cles NUME_ORDRE et AMOR_REDUIT sont utilisés,
 il faut autant d'arguments pour l'un et l'autre
"""),

58 : _("""
 le concept MODE_MECA d'entrée doit etre celui correspondant à la base modale initiale
 pour le calcul de couplage fluide-structure
"""),

60 : _("""
 tous les modes non couplés étant retenus, le nombre d'arguments valide
 pour le mot-clé AMOR_REDUIT est la différence entre le nombre de modes
 de la base modale initiale et le nombre de modes pris en compte pour
 le couplage fluide-structure
"""),

61 : _("""
 les numéros d'ordre fournis ne correspondent pas à des modes non perturbés
"""),

62 : _("""
 option symétrie : la dimension de POINT et AXE_1 doit etre identique.
"""),

63 : _("""
 option symétrie : AXE_2 est inutile en 2D, il est ignoré.
"""),

64 : _("""
 option symétrie : la dimension de POINT et AXE_2 doit etre identique.
"""),




69 : _("""
 on ne sait pas traiter le champ de type:  %(k1)s
 champ :  %(k2)s
"""),

74 : _("""
 attention, mode sur-amorti
"""),

75 : _("""
 attention, mode instable
"""),

80 : _("""
 pour utiliser le comportement "HYDR", il faut surcharger le code
 en "mode devéloppement" avec les routines "PERMEA" et "SATURA".
"""),

81 : _("""
 le vecteur directeur est nul.
"""),

83 : _("""
 nombre maximum d'itérations atteint
"""),

84 : _("""
 précision machine depassée
"""),

85 : _("""
 problème pilo : 3 solutions ou plus
"""),

86 : _("""
 matrice mat non inversible
"""),

87 : _("""
 problème pilo
"""),

88 : _("""
 loi de comportement non disponible pour le pilotage
"""),

89 : _("""
 le pilotage PRED_ELAS nécessite ETA_PILO_MIN et ETA_PILO_MAX
 pour la loi ENDO_ISOT_BETON
"""),

90 : _("""
 le pilotage PRED_ELAS nécessite ETA_PILO_MIN et ETA_PILO_MAX
 pour la loi ENDO_ORTH_BETON
"""),

91 : _("""
 le nombre de noeuds mesuré doit etre inférieur au nombre de noeuds du modèle
"""),

92 : _("""
 maille SEG2 non trouvée
"""),

93 : _("""
 intégration élastoplastique de loi BETON_DOUBLE_DP :
 pas de convergence lors de la projection au sommet des cones de traction et de compression
 --> utiliser le redécoupage automatique du pas de temps.
"""),

94 : _("""
 intégration élastoplastique de loi BETON_DOUBLE_DP :
 pas de convergence lors de la resolution pour NSEUIL =  %(k1)s
 --> utiliser le redécoupage automatique du pas de temps.
"""),

95 : _("""
 non convergence à la maille:  %(k1)s
"""),

96 : _("""
 la saturation n'est pas une variable interne pour la loi de couplage  %(k1)s
"""),

97 : _("""
 la pression de vapeur n'est pas une variable interne pour la loi de couplage  %(k1)s
"""),

99 : _("""
 la variable  %(k1)s  n'existe pas dans la loi CJS en 2D
"""),

}
