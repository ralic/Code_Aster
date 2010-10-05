#@ MODIF calculel6 Messages  DATE 04/10/2010   AUTEUR PELLET J.PELLET 
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

cata_msg={

1: _("""
Erreur utilisateur (dans la commande AFFE_MATERIAU) :
  Dans le CHAM_MATER %(k1)s, vous avez affecté le matériau %(k2)s.
  Dans ce matériau, il existe un coefficient de dilatation (ELAS/ALPHA)
  qui est une fonction de la température.
  Pour pouvoir utiliser cette fonction, il est nécessaire de transformer
  cette fonction (changement de repère : TEMP_DEF_ALPHA -> TEMP_REF).
  Pour cela, l'utilisateur doit fournir une température de référence.

Solution :
  Vérifier que les mailles affectées par le matériau %(k2)s sont bien
  toutes affectées par une température de référence
  (AFFE/TEMP_REF ou AFFE_VARC/NOM_VARC='TEMP',VALE_REF).
"""),

2: _("""
  Problème d'extraction : Résultat généralisé %(k1)s
"""),

3: _("""
  Le paramètre n'existe pas.
"""),

4: _("""
  0 ligne trouvée pour les NOM_PARA.
"""),

5: _("""
  Plusieurs lignes trouvées.
"""),

6: _("""
  Erreur de Programmation: code retour de 'TBLIVA' inconnu.
"""),

7: _("""
  Filtre non valide.
"""),

8: _("""
  Les types ne correspondent pas.
"""),


10: _("""
  L'option %(k1)s est inconnue.
"""),

11: _("""
  Erreur d'utilisation :
    Vous avez dépassé une des limites de la programmation concernant les champs de matériaux :
    On ne pas utiliser plus de 9999 matériaux différents
"""),

13: _("""
 interpolation déformations anélastiques :
 evol_noli: %(k1)s
 instant  : %(r1)f
 icoret   : %(i1)d
"""),

15: _("""
  L'élément diagonal u( %(i1)d , %(i2)d ) de la factorisation est nul. %(k1)s
  la solution et les estimations d'erreurs ne peuvent etre calculées. %(k2)s
"""),

17: _("""
 recherche nbre de cmp: erreur:  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s
"""),

20: _("""
 recherche nbre de cmp: erreur: grandeur ligne numero  %(i1)d  de nom  %(k1)s
 grandeur colonne numero  %(i2)d
  de nom  %(k2)s
 grandeur mere numero  %(i3)d
  de nom  %(k3)s
"""),

21: _("""
 recherche nbre de cmp: erreur: grandeur %(i1)d a un code inconnu:  %(i2)d
"""),

22: _("""
 recherche nbre d entiers codes  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s
"""),

25: _("""
 recherche nbre d entiers codes grandeur ligne numero  %(i1)d  de nom  %(k1)s
 grandeur colonne numero  %(i2)d de nom  %(k2)s
 grandeur mere numero  %(i3)d de nom  %(k3)s
"""),

26: _("""
 recherche nbre d entiers codes grandeur %(i1)d a un code inconnu:  %(i2)d
"""),


42: _("""
 La prise en compte de l'erreur sur une condition aux limites
 de type ECHANGE_PAROI n'a pas été encore implantée
"""),

43: _("""
 le mot cle EXCIT contient plusieurs occurences de type %(k1)s
 seule la dernière sera prise en compte
"""),

46: _("""
 champ de température vide pour le numéro d'ordre : %(i1)d
"""),

47: _("""
 champ FLUX_ELNO_TEMP vide pour numéro d'ordre :  %(i1)d
"""),

49: _("""
 erreurs données composante inconnue  %(k1)s  pour la grandeur  %(k2)s
"""),








52: _("""
 Erreur Utilisateur :

 Variables internes initiales non cohérentes (nb sous-points) avec le comportement choisi.
 Pour la maille : %(k1)s
 nb sous-points "k-1" :  %(i1)d
 nb sous-points "k"   :  %(i2)d
"""),


54: _("""
 Problème d'utilisation du parallélisme :
   Les fonctionnalités de parallélisme utilisées ici (calculs distribués) conduisent à créer
   des structures de données "incomplètes" (i.e. partiellement calculées sur chaque processeur).

   Malheureusement, dans la suite des traitements, le code a besoin que les structures de données soient
   "complètes". On est donc obligé d'arreter le calcul.

 Conseils pour l'utilisateur :
   1) Il faut émettre une demande d'évolution du code pour que le calcul demandé aille à son terme.
   2) En attendant, il ne faut pas utiliser la "distribution" des structures de donnée.
      Aujourd'hui, cela veut dire :
        - éviter de se retrouver avec une "partition" du modèle dans la commande où le problème a été
          détecté.
        - pour cela, juste avant l'appel à la commande problématique, il faut appeler la commande :
          MODI_MODELE(reuse=MO, MODELE=MO, PARTITION=_F(PARALLELISME='NON'))
"""),

55: _("""
 Problème d'utilisation du parallélisme :
   On cherche à faire la combinaison linéaire de plusieurs matrices. Certaines de ces matrices
   ne sont pas calculées complètement et d'autres le sont. On ne peut donc pas les combiner.

 Conseils pour l'utilisateur :
   1) Il faut émettre une demande d'évolution du code pour que le calcul demandé aille à son terme.
      Aide pour le développeur : Noms de deux matrices incompatibles : %(k1)s  et %(k2)s
   2) En attendant, il ne faut pas utiliser la "distribution" des structures de donnée.
      Aujourd'hui, cela veut dire :
        - éviter de se retrouver avec une "partition" du modèle dans la commande où le problème a été
          détecté.
        - pour cela, juste avant l'appel à la commande problématique, il faut appeler la commande :
          MODI_MODELE(reuse=MO, MODELE=MO, PARTITION=_F(PARALLELISME='NON'))
"""),


56: _("""
 Erreur d'utilisation (rcmaco/alfint) :
 Un des matériaux du CHAM_MATER %(k1)s contient un coefficient de dilation ALPHA=f(TEMP).
 Mais la température de référence n'est pas fournie sous AFFE_MATERIAU/AFFE_VARC/VALE_REF

 Conseil :
 Renseignez la température de référence à l'aide de AFFE_MATERIAU/AFFE_VARC/NOM_VARC='TEMP' + VALE_REF
"""),

57: _("""
 Erreur d'utilisation (préparation des variables de commande) :
 Pour la variable de commande %(k1)s, il y a une incohérence du
 nombre de "sous-points" entre le CARA_ELEM %(k2)s
 et le CHAM_MATER %(k3)s.

 Conseil :
 N'avez-vous pas défini plusieurs CARA_ELEM conduisant à des nombres de
 "sous-points" différents (COQUE_NCOU, TUYAU_NCOU, ...) ?
"""),

58: _("""
 Erreur de programmation :
 Pour la variable de commande %(k1)s, on cherche à utiliser la famille
 de points de Gauss '%(k2)s'.
 Mais cette famille n'est pas prévue dans la famille "liste" (MATER).

 Contexte de l'erreur :
    option       : %(k3)s
    type_element : %(k4)s

 Conseil :
 Emettez une fiche d'anomalie
"""),

59: _("""
 Erreur d'utilisation (préparation des variables de commande) :
 Dans le CHAM_MATER %(k1)s et pour la variable de commande %(k2)s,
 on a trouvé la composante 'TEMP_INF'.
 Cela veut sans doute dire que vous avez oublié de "préparer"
 la variable de commande 'TEMP' avec   CREA_RESU / OPERATION='PREP_VRC2'
"""),

60: _("""
 Erreur d'utilisation (préparation des variables de commande) :
 Dans le CHAM_MATER %(k1)s et pour la variable de commande %(k2)s,
 la liste donnée pour le mot clé VALE_REF n'a pas la bonne longueur.
"""),


61:_("""
 Erreur de programmation (fointa) :
    Le type de la fonction est invalide : %(k1)s
"""),

62: _("""
 Erreur de programmation (fointa) :
    Pour l'interpolation de la fonction %(k1)s sur la maille %(k3)s,
    il manque le paramètre %(k2)s
"""),


63: _("""
 Erreur lors de l'interpolation (fointa) de la fonction %(k1)s :
 Code retour: %(i1)d
"""),

64: _("""
 Variables internes en nombre différent aux instants '+' et '-' pour la maille %(k1)s
 Instant '-' : %(i1)d
 Instant '+' : %(i2)d
"""),

65: _("""
 Vous avez fourni %(i1)d charges alors qu'il n'y a %(i2)d dans la SD résultat.

 Risque & Conseil :
   Vous pouvez obtenir des résultats faux si les charges sont différentes.
   Vérifiez que vous n'avez pas oublié de charge ou que vous n'en avez pas ajouté.
"""),

66: _("""
 Le couple (charge-fonction) fourni par l'utilisateur n'est pas présent dans la SD_resultat.
 On poursuit le calcul avec le chargement fourni par l'utilisateur.
   Charge   (utilisateur) : %(k1)s
   Fonction (utilisateur) : %(k2)s
   Charge   (SD_resultat) : %(k3)s
   Fonction (SD_resultat) : %(k4)s

"""),

67: _("""
 Erreur utilisateur :
   Un calcul élémentaire nécessite une ou plusieurs variables de commande (CVRC).
   Sur la maille : %(k1)s, on ne trouve pas le bon nombre de "CVRC" :
   On attend : %(i2)d "CVRC",  mais on n'en trouve que : %(i1)d

 Conseil :
   Vérifier les occurences de AFFE_MATERIAU/AFFE_VARC pour la maille concernée.
"""),

68: _("""
 la liste des composantes fournies à NOCART est incorrecte.
 composantes dans catalogue:
"""),

69: _("""
   %(k1)s
"""),

70: _("""
 composantes dans EDITGD:
"""),

71: _("""
   %(k1)s
"""),

72: _("""

"""),

73: _("""
  Le jacobien est négatif.
"""),

74: _("""
 élément  :  %(i1)d
 jacobien :  %(r1)f
 Attention, le calcul d'erreur est faux si la maille n'est pas correctement orientée.
"""),

75: _("""
  Problème de parallélisation des calculs élémentaires avec FETI.
  Incompatiblité entre LIGRELs dans la routine CALCUL.

  Risques & conseils :
  Essayez de passer en séquentiel ou de changer de solveur linéaire.
"""),

76: _("""
  Problème de parallélisation des calculs élémentaires avec FETI.
  Incompatiblité LIGREL/numéro de maille dans la routine CALCUL.

  Risques & conseils :
  Essayez de passer en séquentiel ou de changer de solveur linéaire.
"""),

77: _("""
  Problème lors de l'affectation du champ %(k1)s.
  Des valeurs n'ont pas été recopiées dans le CHAM_ELEM final.
  Ce problème peut être dû à l'utilisation du mot-clé TOUT='OUI'.
  Il est possible de vérifier le champ produit avec INFO=2.

"""),

79: _("""
  Problème lors du calcul de l'option %(k1)s pour les éléments X-FEM :
  le champ produit est incomplet sur les éléments X-FEM.

  Risque & Conseils :
  Ce champ ne pourra être utilisé sur des éléments non X-FEM.
  Il vaut mieux utiliser les commandes de post-traitement spécifique
  POST_MAIL_XFEM et POST_CHAM_XFEM avant le CALC_ELEM.
"""),

80 : _("""
  L'amortissement du MACR_ELEM %(k1)s n'a pas encore été calculé.
 """),

81 : _("""
  Il manque des amortissements.
  """),

82: _("""
  Le groupe de noeuds %(k1)s n'appartient pas au maillage %(k2)s.
"""),

83 : _("""
  L'option %(k1)s n'est pas traitée pour un résultat de type fourier_elas
(produit par MACRO_ELAS_MULT). Il faut faire après MACRO_ELAS_MULT une
 recombinaison de Fourier par l'opérateur COMB_FOURIER.
"""),

84: _("""
  Le mot-clé MODELE est obligatoire quand RESULTAT est absent
"""),

85: _("""
  Parametre %(k1)s inexistant dans la table.
"""),

86: _("""
  Objet %(k1)s inexistant.
"""),

87: _("""
  Objet %(k1)s non testable.
"""),

88: _("""
  La composante %(k1)s n'existe pas pour ce champ.
"""),

89: _("""
  Le champ %(k1)s est à valeurs de type %(k2)s et la valeur de réference de
  type %(k3)s.
"""),

90: _("""
  Le champ de type %(k1)s sont interdits.
"""),

91: _("""
  Le ddl %(k1)s n'existe pas dans la grandeur %(k2)s.
"""),

92: _("""
  On ne trouve pas le noeud %(k1)s.
"""),

93: _("""
  On ne trouve pas le ddl.
"""),

94: _("""
  Pas d'accès au résultat.
"""),

95: _("""
  Type de la valeur de réference incompatible avec le type des valeurs du champ.
"""),

96: _("""
  Champ absent au numéro d'ordre %(i1)s dans le résultat %(k1)s:
    -> champ : %(k2)s %(k3)s
"""),

97: _("""
  Mot-clé POINT interdit pour le champ au noeud issu de %(k1)s à l'ordre %(i1)s:
    -> champ : %(k2)s %(k3)s
"""),

98: _("""
  Composante généralisée non trouvée.
"""),

99: _("""
  Pas d'accès au résultat généralisé %(k1)s
"""),

}
