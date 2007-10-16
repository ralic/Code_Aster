#@ MODIF algorith5 Messages  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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
 le type de résultat DYNA_TRANS ne supporte pas les données complexes
"""),

2 : _("""
 le type de résultat DYNA_HARMO ne supporte pas les données reelles
"""),

3 : _("""
 on ne traite pas actuellement les déformations complexes
"""),

4 : _("""
 nombre de dataset 58 supérieur à NBNOEUD * NBCMP 
"""),

5 : _("""
 erreur dans la lecture du fichier IDEAS
"""),

6 : _("""
 seules les données de type déplacement, vitesse, accélération, déformation
 ou contrainte sont actuellement traitées 
"""),

7 : _("""
 données : complexes, incompatibles avec DYNA_TRANS
"""),

8 : _("""
 données : réelles, incompatibles avec DYNA_HARMO ou HARM_GENE
"""),

9 : _("""
 on ne traite pas actuellement la redéfinition des orientations
 pour les champs de contrainte 
"""),

10 : _("""
 on ne traite pas actuellement la redéfinition des orientations
 pour les champs de déformation 
"""),

11 : _("""
 la condition GAMMA/KSI <= 1 n'est pas respectée
"""),

12 : _("""
 incohérence des relations SIGMA_C SIGMA_P1 M_PIC A_PIC A_E et M_E
"""),

13 : _("""
 erreur d'intégration
 - essai d'intégration numéro  %(k1)s
 - divergence de l'intégration locale
 - redécoupage du pas de temps
"""),

14 : _("""
  incrément de deformation cumulée (dv) = - %(k1)s 
"""),

16 : _("""
 le profil de la matrice n est surement pas plein.
 on continue mais s'il vous arrive des problèmes plus loin...
"""),

17 : _("""
 le profil de la matrice n'est surement pas plein.
 on continue mais attention ....
"""),

18 : _("""
 le profil de la matrice n'est pas plein.
 on arrete tout ....
"""),

19 : _("""
 matrice singulière
"""),

20 : _("""
 inversion seulement en dimension 3
"""),

22 : _("""
 la matrice masse est singulière.
"""),

23 : _("""
 pas de temps minimal atteint
"""),

24 : _("""
 données erroneés.
"""),

25 : _("""
  GAMMA = 0 : valeur par défaut 
"""),

26 : _("""
  dispositif anti-sismique :  la distance des noeuds 1 et 2 est nulle
"""),

27 : _("""
 le noeud  %(k1)s  n'est pas un noeud du maillage  %(k2)s 
"""),

28 : _("""
 on n'a pas trouvé le ddl "DX" pour le noeud  %(k1)s 
"""),

29 : _("""
 on n'a pas trouvé le ddl "DY" pour le noeud  %(k1)s 
"""),

30 : _("""
 on n'a pas trouvé le ddl "DZ" pour le noeud  %(k1)s 
"""),

31 : _("""
 calcul non-linéaire par sous-structuration :
 le mot-cle SOUS_STRUC_1 est obligatoire
"""),

32 : _("""
 argument du mot-cle "SOUS_STRUC_1" n'est pas un nom de sous-structure
"""),

33 : _("""
 calcul non-linéaire par sous-structuration entre 2 structures mobiles :
 le mot-clé SOUS_STRUC_2 est obligatoire
"""),

34 : _("""
 l'argument du mot-clé "SOUS_STRUC_2" n'est pas un nom de sous-structure
"""),

35 : _("""
  obstacle BI_CERC_INT : DIST_2 doit etre supérieur ou égal a DIST_1
"""),

36 : _("""
 calcul non-linéaire par sous-structuration :
 pas de dispositif anti-sismique ou de flambage possible 
"""),

37 : _("""
 le multi-appui + sous-structuration n'est pas developpé - bon courage
"""),

38 : _("""
 conflit entre choc et flambage au meme lieu de choc :
 le calcul sera de type flambage
"""),

39 : _("""
 argument du mot-cle "REPERE" inconnu
"""),

40 : _("""
 les rigidités de chocs doivent etre strictement positives
"""),

41 : _("""
 incohérence dans les données de la loi de flambage :
 les caractéristiques introduites peuvent conduire à
 un ecrasement résiduel négatif 
"""),

42 : _("""
 les bases utilisées pour la projection sont différentes.
"""),

43 : _("""
 les bases utilisées n'ont pas le meme nombre de vecteurs.
"""),

44 : _("""
 les numérotations des matrices sont différentes.
"""),

45 : _("""
 les numérotations des vecteurs d'excitation sont différentes.
"""),

46 : _("""
 on n'a pas pu trouver les déplacements initiaux 
"""),

47 : _("""
 on n'a pas pu trouver les vitesses initiales 
"""),

48 : _("""
 on n'a pas pu trouver les variables internes initiales :
 reprise choc avec flambage 
"""),

49 : _("""
 absence de terme de forcage externe.
 l'algorithme ITMI n'est pas prévu pour calculer la réponse libre
 d'une structure.
"""),

50 : _("""
 abscence de non-linéarites de choc.
 pour traiter le régime linéaire, préciser une non-linéarité de choc
 avec un jeu important.
"""),

51 : _("""
 impossible de traiter le type d'obstacle choisi avec methode ITMI
 (obstacle de type  %(k1)s  au noeud  %(k2)s ).
"""),

52 : _("""
 durée de la simulation temporelle après transitoire inférieure à
 la durée demandée (excitation temporelle trop courte)
"""),

53 : _("""
 variation du déplacement entre deux instants successifs supérieure à
 la valeur de tolérance proposée
"""),

54 : _("""
 le calcul de la réponse temporelle n'est pas possible pour le type
 de structure etudiée.
"""),

55 : _("""
 le couplage fluide-structure n'a pas été pris en compte en amont.
"""),

56 : _("""
 NB_MODE est superieur au nombre de modes du concept  %(k1)s .
 on impose donc NB_MODE =  %(k2)s ,
 i.e. égal au nombre de modes du concept  %(k3)s .
"""),

58 : _("""
 le calcul des paramètres du mode no %(k1)s  par l'opérateur <CALC_FLUI_STRU>
 n'a pas convergé pour la vitesse no %(k2)s .
 le calcul de la réponse dynamique de la sructure n'est donc pas possible.
"""),

59 : _("""
 pas de mot-cle <NB_MODE_FLUI>.
 les  %(k1)s  modes du concept  %(k2)s  sont pris en compte pour le calcul
 du saut de force fluidelastique d'amortissement au cours des phases de choc.
"""),

60 : _("""
 NB_MODE_FLUI est plus grand que le nombre de modes du concept  %(k1)s .
 %(k2)s  modes sont pris en compte pour le calcul du saut de force fluidelastique
 d'amortissement au cours des phases de choc.
"""),

61 : _("""
 la matrice KTILDA est singulière.
"""),

62 : _("""
  instant initial non trouvé
  valeur prise : 0 
"""),

63 : _("""
 RELA_EFFO_DEPL par sous-structuration, le mot-cle SOUS_STRUC est obligatoire
"""),

64 : _("""
 l'argument du mot-cle "SOUS_STRUC" n'est pas un nom de sous-structure
"""),

65 : _("""
 type de base inconnu.
"""),

66 : _("""
 le taux de souplesse negligée est supérieur au seuil.
"""),

67 : _("""
 algorithme de DEVOGE: développement "AMOR_GENE" non implanté.
"""),

68 : _("""
 algorithme ITMI :
 il faut renseigner obligatoirement l'un ou l'autre des mots cles :
 AMOR_REDUIT, AMOR_GENE
"""),

69 : _("""
 algorithme ITMI :
 il faut renseigner obligatoirement les mots-cles
 BASE_ELAS_FLUI et NUME_VITE_FLUI
 pour définir une base modale sous écoulement
"""),

70 : _("""
 algorithme ITMI :
 il faut renseigner obligatoirement le mot cle PAS ,
 i.e. donner la valeur du pas de temps initial
"""),

71 : _("""
 algorithme ITMI : lorsque l'on affecte "OUI" à ETAT_STAT,
 il faut renseigner TS_REG_ETAB
"""),

72 : _("""
 calcul non-linéaire par sous-structuration :
 option SOUS_STRUC_1 non implantée dans la methode ITMI.
"""),

73 : _("""
  l'option NOEUD_2 n'est pas implantée dans la methode ITMI.
"""),

74 : _("""
 calcul non-linéaire par sous-structuration :
 option SOUS_STRUC_2 non implantée dans la methode ITMI.
"""),

75 : _("""
 algorithme de NEWMARK: développement %(k1)s non implanté.
"""),

76 : _("""
 NUME_ORDRE plus grand que le nombre de modes de la base
"""),

78 : _("""
 mauvaise définition de l'excitation
 mot clé VECT_GENE non autorisé pour ITMI
"""),

79 : _("""
 KSIB non inversible
"""),

82 : _("""
 projection nulle sur la boule unité (dvlp)
"""),

}
