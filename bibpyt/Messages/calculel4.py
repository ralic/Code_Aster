#@ MODIF calculel4 Messages  DATE 12/02/2013   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg = {

1 : _(u"""
 Erreur utilisateur :
   Le CHAM_ELEM %(k1)s a des valeurs indéfinies.
 Conseils :
   * Si le problème concerne la commande CREA_CHAMP :
     1) Vous devriez imprimer le contenu du champ créé pour vérifications (INFO=2) ;
     2) Vous devriez peut-être utiliser le mot clé PROL_ZERO='OUI' .
"""),

2 : _(u"""
Erreur utilisateur dans la commande IMPR_RESU / RESTREINT :
  Les SD_RESULTAT que l'on veut imprimer : %(k1)s, %(k2)s
  sont associées à des maillages différents : %(k3)s, %(k4)s
  C'est interdit.
"""),

3 : _(u"""
Erreur utilisateur dans la commande IMPR_RESU / RESTREINT :
  Seul FORMAT='MED' est autorisé.
"""),

4 : _(u"""
Erreur :
  On cherche à modifier le "type" (réel(R), complexe(C), entier(I), fonction(K8)) d'un champ.
  C'est impossible.
  Types incriminés : %(k1)s et %(k2)s
Conseils :
  Il s'agit peut-être d'une erreur de programmation.
  S'il s'agit de la commande CREA_CHAMP, vérifiez le mot clé TYPE_CHAM.

"""),

5 : _(u"""
Erreur utilisateur dans la commande IMPR_RESU / RESTREINT :
  Quand on utilise le mot clé RESTREINT, le mot clé RESULTAT est obligatoire
  pour chaque occurrence du mot clé facteur RESU

Conseil :
  On ne peut pas restreindre un champ "isolé".
  On ne traite que les SD_RESULTAT.
  Il faut donc au préalable créer une SD_RESULTAT avec CREA_RESU
"""),

6 : _(u"""
Erreur utilisateur (ou programmeur) :
 On veut imposer la numérotation des ddls du CHAM_NO %(k1)s
 avec le NUME_DDL %(k2)s.
 Mais ces 2 structures de données sont incompatibles.
 Par exemple :
    - ce n'est pas le même maillage sous-jacent
    - ce n'est pas la même grandeur sous-jacente.
"""),

7 : _(u"""
 La restriction du champ %(k1)s n'a pas été possible à tous les numéros
 d'ordre. Il ne sera donc pas imprimé dans le fichier MED.
 Cela peut se produire lorsque le mot-clé RESTREINT est utilisé
 et que le champ n'existe pas sur les entités géométriques sur
 lesquelles on tente de le restreindre.
"""),

8 : _(u"""
 Le résultat %(k1)s n'existe pas.
"""),

9 : _(u"""
Problème lors de la projection d'un champ aux noeuds de la grandeur (%(k1)s) sur un autre maillage.
 Pour le noeud "2" %(k2)s (et pour la composante %(k3)s) la somme des coefficients de pondération
 des noeuds de la maille "1" en vis à vis est très faible (inférieure à 1.e-3).

 Cela peut arriver par exemple quand le champ à projeter ne porte pas la composante sur
 tous les noeuds de la maille "1" et que le noeud "2"  sur lequel on cherche à projeter
 se trouve tout près d'un noeud "1" qui ne porte pas la composante.
 Quand cela arrive, la projection est imprécise sur le noeud.

Risques et conseils :
 Si le champ à projeter a des composantes qui n'existent que sur les noeuds sommets des éléments,
 on peut faire une double projection en passant par un maillage intermédiaire linéaire.
"""),

10 : _(u"""
 Erreur d'utilisation :
   On ne trouve pas de variables de commandes ('TEMP', 'HYDR', ...) :
   Option: %(k2)s  type_élément: %(k3)s )

 Risques & conseils :
   La cause la plus fréquente de cette erreur est d'avoir oublié de
   renseigner AFFE_MATERIAU/AFFE_VARC.
   (Ou de n'avoir renseigné que AFFE_VARC/VALE_REF sans avoir renseigné EVOL ou CHAMP_GD)
"""),

11 : _(u"""
 Erreur d'utilisation lors de l'affectation des variables de commande (AFFE_MATERIAU/AFFE_VARC):
   Pour la variable de commande %(k1)s,
   Vous avez oublié d'utiliser l'un des 2 mots clés CHAMP_GD ou EVOL.
   L'absence de ces 2 mots clés n'est permise que pour NOM_VARC='TEMP' (modélisations THM)
"""),

12 : _(u"""
 Erreur de programmation (catalogue des éléments finis) :
 Les éléments finis ayant l'attribut VF_AVEC_VOISIN='OUI' ne peuvent créer que des
 matrices élémentaires non-symétriques.
"""),

13 : _(u"""
 Erreur d'utilisation (AFFE_MATERIAU/AFFE_VARC) :
  Le maillage associé au calcul (%(k1)s) est différent de celui associé
  aux champs (ou EVOL_XXXX) affectés dans AFFE_MATERIAU/AFFE_VARC (%(k2)s).

 Conseil :
  Il faut corriger AFFE_MATERIAU.
"""),

14 : _(u"""
 Erreur d'utilisation de la commande CREA_RESU / PREP_VRC[1|2] :
    Le CARA_ELEM (%(k1)s) ne contient pas d'éléments à "couches"
 Il n'y a aucune raison d'utiliser l'option PREP_VRC[1|2]
"""),

15 : _(u"""
 Erreur d'utilisation (CREA_RESU/PREP_VRC.) :
   Le modèle associé au CARA_ELEM (%(k1)s) est différent de celui fourni à la commande.
"""),

16 : _(u"""
 Erreur d'utilisation :
   L'option %(k1)s est nécessaire pour le calcul de l'option %(k2)s.
   Or %(k1)s est un champ qui ne contient que des sous-points, ce cas n'est pas traité.
   Vous devez d'abord extraire %(k1)s sur un sous-point avec la commande POST_CHAMP.
"""),










21 : _(u"""
 Erreur utilisateur :
   La commande CREA_RESU / ASSE concatène des structures de données résultat.
   Mais il faut que les instants consécutifs soient croissants (en tenant compte de TRANSLATION).
   Ce n'est pas le cas ici pour les instants : %(r1)f  et %(r2)f
"""),

22 : _(u"""
 Information utilisateur :
   La commande CREA_RESU / ASSE concatène des structures de données résultat.
   Mais il faut que les instants consécutifs soient croissants (en tenant compte de TRANSLATION).
   Ici, l'instant %(r1)f  est affecté plusieurs fois.
   Pour cet instant, les champs sont écrasés.
"""),

23 : _(u"""
 Erreur utilisateur :
   Incohérence du MODELE et du CHAM_MATER :
     Le MODELE de calcul est associé au maillage %(k1)s
     Le CHAM_MATER de calcul est associé au maillage %(k2)s
"""),

24 : _(u"""
 Alarme utilisateur :
   IMPR_RESU / CONCEPT / FORMAT='MED'
     Le format MED n'accepte pas plus de 80 composantes pour un champ.
     Le champ %(k1)s ayant plus de 80 composantes, on n'imprime
     que les 80 premières.
"""),










43 : _(u"""
 le NOM_PARA n'existe pas
"""),

44 : _(u"""
 0 ligne trouvée pour le NOM_PARA
"""),

45 : _(u"""
 plusieurs lignes trouvées
"""),

46 : _(u"""
 code retour de "tbliva" inconnu
"""),

47 : _(u"""
 TYPE_RESU inconnu:  %(k1)s
"""),

53 : _(u"""
 longueurs des modes locaux incompatibles entre eux.
"""),

54 : _(u"""
 aucuns noeuds sur lesquels projeter.
"""),

55 : _(u"""
 pas de mailles a projeter.
"""),

56 : _(u"""
  %(k1)s  pas trouve.
"""),

57 : _(u"""
 il n'y a pas de mailles a projeter.
"""),

58 : _(u"""
 les maillages a projeter sont ponctuels.
"""),

59 : _(u"""
Erreur utilisateur :
 Les maillages associés aux concepts %(k1)s et %(k2)s sont différents : %(k3)s et %(k4)s.
"""),

60 : _(u"""
 maillages 2 différents.
"""),

61 : _(u"""
 problème dans l'examen de  %(k1)s
"""),

62 : _(u"""
 aucun numéro d'ordre dans  %(k1)s
"""),

63 : _(u"""
 On n'a pas pu projeter le champ %(k1)s de la SD_RESULTAT %(k2)s
 vers la SD_RESULTAT %(k3)s pour le numéro d'ordre %(i1)d
"""),

64 : _(u"""
 Aucun champ projeté.
"""),

65 : _(u"""
  maillages non identiques :  %(k1)s  et  %(k2)s
"""),

66 : _(u"""
 pas de champ de matériau
"""),

67 : _(u"""
 erreur dans etanca pour le problème primal
"""),

68 : _(u"""
 erreur dans etenca pour le problème dual
"""),

69 : _(u"""
 Erreur utilisateur :
    On ne trouve pas la variable de commande :  %(k1)s
    pour la maille                : %(k2)s
    pour l'instant de calcul      : '%(k3)s'

 Conseils :
    Les variables de commande sont des variables connues a priori qui influencent
    le calcul du comportement des matériaux (exemple : la température).

    Lorsque le comportement mécanique dépend d'une variable de commande, il faut que l'utilisateur
    la fournisse au calcul.
    Cela se fait via la commande AFFE_MATERIAU / AFFE_VARC.

    Les variables de commande les plus utilisées sont :
      'TEMP'  : la température
      'HYDR'  : l'hydratation
      'SECH'  : le séchage
      'CORR'  : la corrosion
      'IRRA'  : l'irradiation

    Attention au fait que les variables de commandes doivent pouvoir être calculées pour TOUS
    les instants du calcul. Pour cela, si on utilise une structure de données EVOL_XXX pour
    renseigner une variable de commande (AFFE_MATERIAU/AFFE_VARC/EVOL), il faut faire attention
    à utiliser éventuellement les mots clés PROL_GAUCHE et PROL_DROIT.
"""),

70 : _(u"""
Erreur utilisateur dans CREA_CHAMP :
  Vous avez demandé la création d'un champ '%(k1)s' (mot clé TYPE_CHAM)
  Mais le code a créé un champ '%(k2)s'.
Conseil :
  Il faut sans doute modifier la valeur de TYPE_CHAM
"""),

71 : _(u"""
Erreur utilisateur dans CREA_CHAMP :
  Vous avez demandé la création d'un champ de %(k1)s (mot clé TYPE_CHAM)
  Mais le code a créé un champ de %(k2)s.
Conseil :
  Il faut sans doute modifier la valeur de TYPE_CHAM
"""),

72 : _(u"""
Erreur utilisateur dans la commande PROJ_CHAMP :
 Le mot clé MODELE_2 a été utilisé. Le maillage associé à ce modèle (%(k1)s)
 est différent du maillage "2" (%(k1)s)  qui a servi à fabriquer la matrice de projection.
"""),

73 : _(u"""
Erreur utilisateur dans la commande PROJ_CHAMP :
   On veut projeter des champs aux éléments (CHAM_ELEM), le mot clé MODELE_2
   est alors obligatoire.
"""),





78 : _(u"""
Erreur utilisateur dans CREA_CHAMP :
  Le maillage associé au champ créé par la commande (%(k1)s) est différent
  de celui qui est fourni par l'utilisateur via les mots clés MAILLAGE ou MODELE (%(k2)s).
Conseil :
  Il faut vérifier les mots clés MAILLAGE ou MODELE.
  Remarque : ces mots clés sont peut être inutiles pour cette utilisation de CREA_CHAMP.
"""),

79 : _(u"""
 La grandeur :  %(k1)s  n'existe pas dans le catalogue des grandeurs.
"""),

80 : _(u"""
 le nom de la grandeur  %(k1)s  ne respecte pas le format XXXX_c
"""),

81 : _(u"""
 problème dans le catalogue des grandeurs simples, la grandeur complexe %(k1)s
 ne possède pas le même nombre de composantes que son homologue réelle %(k2)s
"""),

82 : _(u"""
 Problème dans le catalogue des grandeurs simples, la grandeur %(k1)s
 ne possède pas les mêmes champs que son homologue réelle %(k2)s
"""),

83 : _(u"""
 erreur: le calcul des contraintes ne fonctionne que pour le phénomène mécanique
"""),

84 : _(u"""
 erreur numéros des noeuds bords
"""),

85 : _(u"""
 erreur: les éléments supportes sont tria3 ou tria6
"""),

86 : _(u"""
 erreur: les éléments supportes sont QUAD4 ou QUAD8 ou QUAD9
"""),

87 : _(u"""
 maillage mixte TRIA-QUAD non supporte pour l estimateur ZZ2
"""),

88 : _(u"""
 erreur: les mailles supportées sont tria ou QUAD
"""),

89 : _(u"""
 Erreur: un élément du maillage possède tous ses sommets sur une frontière.
 Il faut au moins un sommet interne.
 Pour pouvoir utiliser ZZ2 il faut remailler le coin de telle façon que
 tous les triangles aient au moins un sommet intérieur.
"""),

91 : _(u"""
 On ne trouve pas de routine te0NPQ.
 NPQ doit être compris entre 1 et 600 ici : NPQ = %(k1)s
"""),

92 : _(u"""
  relation :  %(k1)s  non implantée sur les poulies
"""),

93 : _(u"""
  déformation :  %(k1)s  non implantée sur les poulies
"""),

94 : _(u"""
 l'attribut:  %(k1)s  n'existe pas pour le type:  %(k2)s
"""),

95 : _(u"""
 Erreur de programmation ou d'utilisation :
   On ne trouve pas dans les arguments de la routine calcul de champ a associer
   au paramètre: %(k1)s  (option: %(k2)s  type_élément: %(k3)s )
"""),

96 : _(u"""
 Erreur de programmation :
 on n'a pas pu extraire toutes les composantes voulues du champ global associe
 au paramètre: %(k1)s  (option: %(k2)s  type_élément: %(k3)s )
"""),

98 : _(u"""
 on n'a pas pu récupérer le paramètre THETA dans le résultat  %(k1)s
 valeur prise pour THETA: 0.57
"""),

}
