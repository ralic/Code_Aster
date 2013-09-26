# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
Le type de résultat DYNA_TRANS ne supporte pas les données complexes.
"""),

2 : _(u"""
Le type de résultat DYNA_HARMO ne supporte pas les données réelles.
"""),

3 : _(u"""
On ne traite pas les déformations complexes.
"""),

4 : _(u"""
Le nombre de DATASET de type 58 est supérieur à NBNOEUD * NBCMP
"""),

5 : _(u"""
Erreur lors de la lecture du fichier IDEAS.
"""),

6 : _(u"""
Seules les données de type déplacement, vitesse, accélération, déformation
 ou contrainte sont traitées.
"""),

9 : _(u"""
On ne traite pas la redéfinition des orientations pour les champs de contrainte.
"""),

10 : _(u"""
On ne traite pas la redéfinition des orientations pour les champs de déformation.
"""),

11 : _(u"""
La condition GAMMA/KSI <= 1 n'est pas respectée.
"""),

12 : _(u"""
Incohérence des relations SIGMA_C, SIGMA_P1, M_PIC, A_PIC, A_E et M_E.
"""),

16 : _(u"""
Le profil de la matrice n'est sûrement pas plein.
On continue pour vérifier.
"""),

17 : _(u"""
Le profil de la matrice n'est sûrement pas plein.
On continue pour vérifier.
"""),

18 : _(u"""
Le profil de la matrice n'est pas plein.
On arrête tout.
"""),

19 : _(u"""
Le déterminant de la matrice à inverser est nul.
"""),

22 : _(u"""
La matrice masse est singulière.
"""),

23 : _(u"""
Le pas de temps minimal a été atteint. Le calcul s'arrête.
"""),

24 : _(u"""
Données erronées.
"""),

25 : _(u"""
Pour l'angle nautique GAMMA, la valeur par défaut est zéro.
"""),

26 : _(u"""
Dispositif anti-sismique :  la distance entre les noeuds 1 et 2 est nulle.
"""),

27 : _(u"""
Le noeud  %(k1)s  n'est pas un noeud du maillage %(k2)s .
"""),

28 : _(u"""
On n'a pas trouvé le ddl DX pour le noeud  %(k1)s .
"""),

29 : _(u"""
On n'a pas trouvé le ddl DY pour le noeud  %(k1)s .
"""),

30 : _(u"""
On n'a pas trouvé le ddl DZ pour le noeud  %(k1)s .
"""),

31 : _(u"""
 calcul non-linéaire par sous-structuration :
 le mot-clé SOUS_STRUC_1 est obligatoire
"""),

32 : _(u"""
 argument du mot-clé "SOUS_STRUC_1" n'est pas un nom de sous-structure
"""),

33 : _(u"""
 calcul non-linéaire par sous-structuration entre 2 structures mobiles :
 le mot-clé SOUS_STRUC_2 est obligatoire
"""),

34 : _(u"""
 l'argument du mot-clé "SOUS_STRUC_2" n'est pas un nom de sous-structure
"""),

35 : _(u"""
  obstacle BI_CERC_INT : DIST_2 doit être supérieur ou égal a DIST_1
"""),

36 : _(u"""
 calcul non-linéaire par sous-structuration :
 pas de dispositif anti-sismique ou de flambage possible
"""),

37 : _(u"""
 La sous-structuration en présence de multiappui n'est pas développée.
"""),

38 : _(u"""
 conflit entre choc et flambage au même lieu de choc :
 le calcul sera de type flambage
"""),

39 : _(u"""
 argument du mot-clé "REPERE" inconnu
"""),

40 : _(u"""
 les rigidités de chocs doivent être strictement positives
"""),

41 : _(u"""
 incohérence dans les données de la loi de flambage :
 les caractéristiques introduites peuvent conduire à
 un écrasement résiduel négatif
"""),

42 : _(u"""
 les bases utilisées pour la projection sont différentes.
"""),

43 : _(u"""
 les bases utilisées n'ont pas le même nombre de vecteurs.
"""),

44 : _(u"""
 les numérotations des matrices sont différentes.
"""),

45 : _(u"""
 les numérotations des vecteurs d'excitation sont différentes.
"""),

46 : _(u"""
 on n'a pas pu trouver les déplacements initiaux
"""),

47 : _(u"""
 on n'a pas pu trouver les vitesses initiales
"""),

48 : _(u"""
 on n'a pas pu trouver les variables internes initiales :
 reprise choc avec flambage
"""),

49 : _(u"""
 absence de terme de forçage externe.
 l'algorithme ITMI n'est pas prévu pour calculer la réponse libre
 d'une structure.
"""),

50 : _(u"""
 absence de non-linéarités de choc.
 pour traiter le régime linéaire, préciser une non-linéarité de choc
 avec un jeu important.
"""),

51 : _(u"""
 impossible de traiter le type d'obstacle choisi avec méthode ITMI
 (obstacle de type  %(k1)s  au noeud  %(k2)s ).
"""),

52 : _(u"""
 durée de la simulation temporelle après transitoire inférieure à
 la durée demandée (excitation temporelle trop courte)
"""),

53 : _(u"""
 variation du déplacement entre deux instants successifs supérieure à
 la valeur de tolérance proposée
"""),

54 : _(u"""
 le calcul de la réponse temporelle n'est pas possible pour le type
 de structure étudiée.
"""),

55 : _(u"""
 le couplage fluide-structure n'a pas été pris en compte en amont.
"""),

56 : _(u"""
 NB_MODE est supérieur au nombre de modes du concept  %(k1)s .
 on impose donc NB_MODE =  %(k2)s ,
 i.e. égal au nombre de modes du concept  %(k3)s .
"""),

58 : _(u"""
 le calcul des paramètres du mode no %(k1)s  par l'opérateur <CALC_FLUI_STRU>
 n'a pas convergé pour la vitesse no %(k2)s .
 le calcul de la réponse dynamique de la structure n'est donc pas possible.
"""),

59 : _(u"""
 pas de mot-clé <NB_MODE_FLUI>.
 les  %(k1)s  modes du concept  %(k2)s  sont pris en compte pour le calcul
 du saut de force fluidélastique d'amortissement au cours des phases de choc.
"""),

60 : _(u"""
 NB_MODE_FLUI est plus grand que le nombre de modes du concept  %(k1)s .
 %(k2)s  modes sont pris en compte pour le calcul du saut de force fluidélastique
 d'amortissement au cours des phases de choc.
"""),

61 : _(u"""
 la matrice KTILDA est singulière.
"""),

62 : _(u"""
  instant initial non trouvé
  valeur prise : 0
"""),

63 : _(u"""
 RELA_EFFO_DEPL par sous-structuration, le mot-clé SOUS_STRUC est obligatoire
"""),

64 : _(u"""
 l'argument du mot-clé "SOUS_STRUC" n'est pas un nom de sous-structure
"""),

65 : _(u"""
 type de base inconnu.
"""),

66 : _(u"""
 le taux de souplesse négligée est supérieur au seuil.
"""),

67 : _(u"""
 algorithme de DEVOGE: développement "AMOR_GENE" non implanté.
"""),

69 : _(u"""
 algorithme ITMI :
 il faut renseigner obligatoirement les mots-clés
 BASE_ELAS_FLUI et NUME_VITE_FLUI
 pour définir une base modale sous écoulement
"""),

70 : _(u"""
 algorithme ITMI :
 il faut renseigner obligatoirement le mot clé PAS ,
 i.e. donner la valeur du pas de temps initial
"""),

71 : _(u"""
 algorithme ITMI : lorsque l'on affecte "OUI" à ETAT_STAT,
 il faut renseigner TS_REG_ETAB
"""),

72 : _(u"""
 calcul non-linéaire par sous-structuration :
 option SOUS_STRUC_1 non implantée dans la méthode ITMI.
"""),

73 : _(u"""
  l'option NOEUD_2 n'est pas implantée dans la méthode ITMI.
"""),

74 : _(u"""
 calcul non-linéaire par sous-structuration :
 option SOUS_STRUC_2 non implantée dans la méthode ITMI.
"""),

75 : _(u"""
 algorithme de NEWMARK: développement %(k1)s non implanté.
"""),

76 : _(u"""
 NUME_ORDRE plus grand que le nombre de modes de la base
"""),

78 : _(u"""
 mauvaise définition de l'excitation
 mot clé VECT_GENE non autorisé pour ITMI
"""),

79 : _(u"""
 KSIB non inversible
"""),

80 : _(u"""
 la prise en compte des fissures dans les rotors n'est possible que pour SCHEMA_TEMP=EULER
"""),

}
