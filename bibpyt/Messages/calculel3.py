#@ MODIF calculel3 Messages  DATE 21/07/2008   AUTEUR NISTOR I.NISTOR 
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
 manque les accélérations
"""),

2 : _("""
 pour une SD RESULTAT de type DYNA_TRANS,
 seuls les mots-clés FONC_MULT et COEF_MULT sont autorisés
"""),

3 : _("""
 pour une SD RESULTAT de type  EVOL_ELAS,
 seul le mot-clé fonc_mult est autorisé
"""),

4 : _("""
 l'utilisation du mot-clé FONC_MULT n'est licite que pour
 les SD RESULTATS :  EVOL_ELAS, DYNA_TRANS, DYNA_HARMO
"""),

5 : _("""
  pour calculer  %(k1)s  il faut SIEF_ELNO_ELGA ou EFGE_ELNO_DEPL
"""),

6 : _("""
  option  %(k1)s  non traitée pour un résultat de type  %(k2)s
"""),

7 : _("""
 calcul de  %(k1)s  impossible.
"""),

8 : _("""
 les champs SIEF_ELGA_DEPL, SIEF_ELGA, SIGM_ELNO_COQU et SIGM_ELNO_DEPL sont absents :
 on ne peut pas calculer l'option %(k1)s  avec la SD de type  %(k2)s
"""),

9 : _("""
 le champ  SIGM_ELNO_DEPL est absent :
 on ne peut pas calculer l'option %(k1)s  avec la SD de type  %(k2)s
"""),

10 : _("""
 le champ  SIGM_ELNO_COQU est absent :
 on ne peut pas calculer l'option %(k1)s  avec la SD de type  %(k2)s
"""),

11 : _("""
 le résultat  %(k1)s  doit comporter un champ de déplacement au numéro d'ordre  %(k2)s  .
"""),

12 : _("""
 le mot cle PREC_ERR est obligatoire avec l'option SING_ELEM
"""),

13 : _("""
 le mot cle PREC_ERR doit etre strictement superieur à zéro et inférieur ou egal à 1
"""),

14 : _("""
 pas d indicateur d erreur- on ne calcule pas l'option sing_elem
"""),

16 : _("""
 par défaut on utilise ERZ2_ELEM_SIGM
"""),

17 : _("""
 le résultat  %(k1)s  doit comporter un champ de contraintes au numéro d'ordre  %(k2)s  .
"""),

18 : _("""
 pas de champ de contraintes pour calculer  %(k1)s
"""),

19 : _("""
 probleme à l'appel de ALCHML pour  %(k1)s
"""),

20 : _("""
 pas de champ d'endommagement pour calculer  %(k1)s
"""),

21 : _("""
 le calcul avec l'option ENDO_ELNO_ELGA nécessite au préalable un calcul avec l'option ENDO_ELGA
"""),

22 : _("""
  option inexistante: %(k1)s
"""),

23 : _("""
 option :  %(k1)s
"""),

25 : _("""
 calcul non disponible
"""),

27 : _("""
 type :  %(k1)s  incompatible avec l'option :  %(k2)s
"""),

28 : _("""
 type de champ inconnu
"""),

30 : _("""
 il faut un modèle ou des charges.
"""),

31 : _("""
 la masse du MACR_ELEM : %(k1)s  n'a pas encore ete calculée.
"""),

32 : _("""
 il manque des masses.
"""),

33 : _("""
 la rigidité du MACR_ELEM : %(k1)s  n'a pas encore été calculée.
"""),

34 : _("""
 il manque des rigidités.
"""),

35 : _("""
 le modèle doit contenir des éléments finis ou des sous-structures.
"""),

38 : _("""
 on ne traite pas le type_scalaire: %(k1)s
"""),

39 : _("""
 le modèle contient des éléments de structure
 il faut probablement utiliser le mot-clé CARA_ELEM.
"""),

40 : _("""
  -> Le modèle a probablement besoin d'un champ de matériau (mot-clé CHAM_MATER).

  -> Risque & Conseil :
     Ce message peut aider à comprendre un éventuel problème ultérieur lors de calculs élémentaires
     nécessitant des caractéristiques matérielles.
     Vérifiez si votre modélisation nécessite un CHAM_MATER.
"""),

41 : _("""
 les charges ne s'appuient pas toutes sur le meme modèle.
"""),

42 : _("""
 les charges ne s'appuient pas sur le modèle donné en argument.
"""),

43 : _("""
 les charges sont de type différent.
"""),

44 : _("""
 les charges ne s'appuient pas toutes sur le meme modèle
"""),

45 : _("""
 données incorrectes.
"""),

50 : _("""
  la commande a besoin d'un nom de modèle
"""),

52 : _("""
 le champ doit être un CHAM_ELEM.
"""),

53 : _("""
 ne traite qu'un CHAM_ELEM réel
"""),

54 : _("""
 longueurs des modes locaux imcompatibles entre eux.
"""),

56 : _("""
 il n'y a pas autant de composantes
"""),

57 : _("""
 on ne sait pas moyenner cette composante negative
"""),

58 : _("""
 champs sur modeles differents
"""),

59 : _("""
  %(k1)s  doit etre un cham_elem.
"""),

60 : _("""
 longueurs des modes locaux champ1 imcompatibles entre eux.
"""),

61 : _("""
 longueurs des modes locaux champ2 imcompatibles entre eux.
"""),

62 : _("""
 composante non definie
"""),

63 : _("""
 champ de geometrie non trouve
"""),

64 : _("""
 l'instant du calcul est pris  arbitrairement a 0.0
"""),

65 : _("""
  on n'accepte un instant arbitraire que si le concept deformations anelastiques n'a qu'1 champ.
"""),

66 : _("""
  le concept evol_noli :  %(k1)s  ne contient aucun champ de déformations anélastiques.
"""),

71 : _("""
 il faut 1 chargement de rotation et un seul.
"""),

72 : _("""
  il ne faut pas definir plus d"un champ de vitesse
"""),

73 : _("""
 le champ:  %(k1)s  n'est ni un cham_elem ni un resuelem
"""),

74 : _("""
 type scalaire interdit : %(k1)s
"""),

79 : _("""
 la matrice A est singulière
"""),

81 : _("""
 cette fonction ne marche que pour des modes locaux de type chno, vect, ou mat
"""),

82 : _("""
 le mode local est de type matrice non_carree
"""),

84 : _("""
 il n y a pas de parametre  %(k1)s  associe a la grandeur: %(k2)s  dans l option: %(k3)s
"""),

85 : _("""
 il y a plusieurs parametres  %(k1)s  associes a la grandeur: %(k2)s  dans l option: %(k3)s
"""),

89 : _("""
 les charges ne s'appuient pas toutes sur le même modèle.
"""),

90 : _("""
 le champ de THETA est inexistant dans la structure de données  %(k1)s  de type THETA_GEOM .
"""),

91 : _("""
 une des charges n'est pas mécanique
"""),

92 : _("""
 erreur: une des charges n'est pas thermique
"""),

93 : _("""
 une des charges n'est pas acoustique
"""),

94 : _("""
 le champ doit être un CHAM_ELEM aux points de gauss
"""),

95 : _("""
 avec un CHAM_ELEM calcule sur une liste de maille,
 il faut utiliser le mot cle "MODELE"
"""),

96 : _("""
  pour prendre en compte les termes d'inertie,
  il est préférable d'utiliser la commande "CALC_ELEM".
  le mot cle "ACCE" n'est pas traité et les résultats risquent d'être faux.
"""),

97 : _("""
  Erreur d'utilisation :
    Fonctionnalité : PROJ_CHAMP
    On cherche à projeter des mailles sur certains noeuds.
    Mais la la liste de noeuds est vide.

  Conseil :
    Cette erreur peut venir d'une mauvaise utilisation du mot clé
    PROJ_CHAMP/DISTANCE_MAX
"""),

}
