#@ MODIF algorith15 Messages  DATE 04/10/2010   AUTEUR GREFFET N.GREFFET 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg = {

1 : _("""
 arret sur nombres de DDL interface non identiques 
 nombre de ddl interface droite:  %(i1)d 
 nombre de ddl interface gauche:  %(i2)d 
"""),

2 : _("""
 arret sur dimension matrice TETA incorrecte
 dimension effective :  %(i1)d 
 dimension en argument:  %(i2)d 
"""),

3 : _("""
  erreur de répétitivité cyclique
"""),

4 : _("""
  il manque un DDL sur un noeud gauche
  type du DDL  -->  %(k1)s 
  nom du noeud -->  %(k2)s 
"""),



6 : _("""
  il manque un DDL sur un noeud droite
  type du ddl  -->  %(k1)s 
  nom du noeud -->  %(k2)s 
"""),

7 : _("""
 arret sur problème de répétitivité cyclique
"""),

8 : _("""
 la composante : %(k1)s  est une composante indefinie
"""),




10 : _("""
 arret sur type de DDL non défini
"""),

11 : _("""
 "NB_POIN" est inférieur au nombre de points de l'interspectre.
 le spectre est tronqué à la fréquence :  %(r1)f 
"""),

12 : _("""
 le "NB_POIN" donné est modifié
 (en une puissance de 2 compatible avec l'interspectre)
 le "NB_POIN" retenu est :   %(i1)d 
"""),

13 : _("""
 la durée est trop grande ou NB_POIN et trop petit par rapport
 à la fréquence max (théorème de Shannon).
 on choisit NBPOIN =  %(i1)d 
"""),

14 : _("""
 la durée est petite par rapport au pas de discrétisation de l'interspectre.
 choisir plutot : durée >  %(r1)f 
"""),

15 : _("""
 "NB_POIN" est petit par rapport au pas de discrétisation de l'interspectre.
 NB_POIN =  %(i1)d 
 il faudrait un nombre supérieur à :  %(r1)f 
"""),

16 : _("""
 on n'a pas trouve le DDL pour le noeud :  %(k1)s 
"""),

17 : _("""
    de la sous-structure :  %(k1)s 
"""),

18 : _("""
    et sa composante :  %(k1)s 
"""),

19 : _("""
  il manque le seuil  pour la fonction interprétée  %(k1)s 
"""),

20 : _("""
 l'abscisse lineaire est nulle pour la courbe :  %(k1)s 
 abscisse :  %(r1)f 
"""),







24 : _("""
 au moins un terme de ALPHA est négatif à l'abscisse :  %(i1)d 
"""),

25 : _("""
 ALPHA est nul et le nombre de mesures est strictement inférieur au nombre de modes
 risque de matrice singulière
"""),

26 : _("""
 calcul moindre norme 
"""),

27 : _("""
  problème calcul valeurs singulieres
  pas      =   %(i1)d
  abscisse =    %(r1)f 
"""),

28 : _("""
  la matrice (PHI)T*PHI + ALPHA n'est pas inversible
  pas      =   %(i1)d 
  abscisse =    %(r1)f 
"""),






















45 : _("""
  on ne trouve pas DPMAX 
"""),

46 : _("""
  nombre d'itérations insuffisant 
"""),

47 : _("""
  F(XMIN) > 0 
"""),

48 : _("""
  maille :  %(k1)s
  nombre d iterations =  %(i1)d
  ITER_INTE_MAXI =  %(i2)d 
"""),

49 : _("""
  DP    actuel =  %(r1)f
  F(DP) actuel =  %(r2)f 
"""),

50 : _("""
  DP    initial   =  %(r1)f
  F(DP) initial   =  %(r2)f 
"""),

51 : _("""
  DP    maximum   =  %(r1)f
  F(DP) maximum   =  %(r2)f 
"""),

52 : _("""
  allure de la fonction
  nb points :  %(i1)d 
"""),

53 : _("""
  DP     =  %(r1)f
  F(DP)  =  %(r2)f 
"""),




55 : _("""
  incohérence détectée 
"""),

56 : _("""
  le noeud :  %(k1)s  de l'interface dynamique :  %(k2)s 
  n'appartient pas la sous-structure:  %(k3)s 
"""),





58 : _("""
  le noeud :  %(k1)s  de l interface dynamique :  %(k2)s 
  n'est pas correctement référencé dans le squelette :  %(k3)s 
"""),

59: _("""
  Le nombre de secteur doit etre supérieur ou égal à 2 (mot clé NB_SECTEUR)
"""),

60 : _("""
  le noeud :  %(k1)s  de l'interface dynamique :  %(k2)s 
  n'appartient pas la sous-structure:  %(k3)s 
"""),




62 : _("""
  le noeud :  %(k1)s  de l'interface dynamique :  %(k2)s 
  n'est pas correctement référencé dans le squelette :  %(k3)s 
"""),

63 : _("""
  conflit mot clés TOUT et GROUP_NO dans RECO_GLOBAL 
"""),

64 : _("""
  erreur de nom
  la sous-structure :  %(k1)s  n a pas ete trouvée 
"""),

65 : _("""
  incohérence de nom
  l interface dynamique  :  %(k1)s 
  de la sous-structure   :  %(k2)s 
  a pour groupe de noeud :  %(k3)s 
  or group_no_1 =  %(k4)s 
"""),

66 : _("""
  erreur de nom
  la sous-structure :  %(k1)s  n'a pas ete trouvée 
"""),

67 : _("""
  incohérence de nom
  l interface dynamique  :  %(k1)s 
  de la sous-structure   :  %(k2)s 
  a pour groupe de noeud :  %(k3)s 
  or group_no_2 =  %(k4)s 
"""),

68 : _("""
 nombre de points pas période             :  %(i1)d 
 coefficient de remontee du pas de temps  :  %(r1)f 
 coefficient de division du pas de temps  :  %(r2)f 
 pas de temps minimal                     :  %(r3)f 
 pas de temps maximal                     :  %(r4)f 
 nombre maximal de réductions du pas      :  %(i2)d 
 vitesse minimale variable                :  %(k1)s 
"""),

69 : _("""
 nombre incorrect de sous-structures
 il vaut :  %(i1)d 
 alors que le nombre total de sous-structures vaut :  %(i2)d 
"""),

70 : _("""
 nombre incorrect de sous-structures
 pour le chargement numero : %(i1)d 
 il en faut exactement :  %(i2)d 
 vous en avez          :  %(i3)d 
"""),

71 : _("""
 nombre incorrect de vecteurs chargements
 pour le chargement numero : %(i1)d 
 il en faut exactement :  %(i2)d 
 vous en avez          :  %(i3)d 
"""),

72 : _("""
 un PROF_CHNO n'est pas défini
 il manque pour le chargement : %(k1)s 
"""),

73 : _("""
 on doit avoir le meme type de forces pour un meme chargement global
 or, la grandeur vaut   :  %(i1)d 
 pour la sous-structure    %(k1)s 
 et elle vaut           :  %(i2)d 
 pour la sous-structure    %(k2)s 
"""),

74 : _("""
 une des bases modales a un type incorrect
 elle est associée à la sous-structure  %(k1)s 
"""),

75 : _("""
 les numérotations ne coincident pas pour la sous-structure : %(k1)s 
 le PROF_CHNO pour la base modale est :  %(k2)s 
 et celui pour le second membre       :  %(k3)s 
"""),

85 : _("""
  L'interface de droite %(k1)s n'existe pas
  Conseil: vérifiez si vous avez défini cette interface dans le modèle
"""),

86 : _("""
  l'interface de gauche %(k1)s n'existe pas
  Conseil: vérifiez si vous avez défini cette interface dans le modèle
"""),

87 : _("""
  l'interface axe %(k1)s n'existe pas
  Conseil: vérifiez si vous avez défini cette interface dans le modèle
"""),

88 : _("""
 arret sur problème interfaces de type différents
"""),

89 : _("""
 arret sur problème de type interface non supporté
 type interface -->  %(k1)s 
"""),

90 : _("""
 le nombre d'amortissements réduits est trop grand
 le nombre de modes propres vaut  %(i1)d 
 et le nombre de coefficients  :  %(i2)d 
 on ne garde donc que les  %(i3)d premiers coefficients
"""),

91 : _("""
 le nombre d'amortissements réduits est insuffisant
 il en manque :  %(i1)d 
 car le nombre de modes vaut :  %(i2)d 
 on rajoute %(i3)d coefficients avec la valeur du dernier coefficient.
"""),

92 : _("""
 Nombre de modes propres calculés insuffisant.
"""),

93 : _("""
 MODE_MECA : %(k1)s 
"""),

94 : _("""
 Nombre de modes propres limités à : %(i1)d 
"""),

95 : _("""
 l'entrée d'amortissements réduits est incompatible 
 avec des matrices de type  %(k1)s 
 Il faut des matrices de type MATR_ASSE_GENE_*
"""),

96 : _("""
 le nombre d'amortissements réduits est trop grand
 le nombre de modes propres vaut  %(i1)d 
 et le nombre de coefficients :  %(i2)d 
 on ne garde donc que les  %(i3)d premiers coefficients 
 
"""),

97 : _("""
 le nombre d'amortissements réduits est insuffisant
 il en manque :  %(i1)d 
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d amortissement reduits avec la valeur du dernier mode propre.
"""),

98 : _("""
 incohérence dans le DATASET 58
 le nombre de valeurs fournies ne correspond pas au nombre de valeurs attendues
 mesure concernée :  %(i1)d 
 
"""),

99 : _("""
 le nombre maximum d'itérations  %(i1)d  est atteint sans converger 
 le résidu relatif final est  : %(r1)f 
 l instant de calcul vaut     : %(r2)f 
 
"""),

}
