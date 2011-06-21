#@ MODIF calculel3 Messages  DATE 21/06/2011   AUTEUR PELLET J.PELLET 
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
# RESPONSABLE DELMAS J.DELMAS

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
  pour calculer  %(k1)s  il faut SIEF_ELNO ou EFGE_ELNO
"""),

6 : _("""
  option  %(k1)s  non traitée pour un résultat de type  %(k2)s
"""),

7 : _("""
 Calcul de  %(k1)s  impossible.
"""),

8 : _("""
 les champs SIEF_ELGA, SICO_ELNO et SIGM_ELNO sont absents :
 on ne peut pas calculer l'option %(k1)s  avec la SD de type  %(k2)s
"""),

9 : _("""
 Le champ  SIGM_ELNO est absent :
 on ne peut pas calculer l'option %(k1)s  avec la SD de type  %(k2)s.
"""),

10 : _("""
 Le champ  SICO_ELNO est absent :
 on ne peut pas calculer l'option %(k1)s  avec la SD de type  %(k2)s.
"""),

11 : _("""
 le résultat  %(k1)s  doit comporter un champ de déplacement au numéro d'ordre  %(k2)s  .
"""),

12 : _("""
 Le mot clé PREC_ERR est OBLIGATOIRE avec l'option SING_ELEM.
 Il faut renseigner le mot clé PREC_ERR avec une valeur comprise entre 0 et 1.
"""),

13 : _("""
 Le mot clé PREC_ERR doit etre strictement positif.
"""),

14 : _("""
 Il n'y a pas de champ d'estimateur d'erreur dans la structure de donnée résultat.
 On ne calcule pas l'option SING_ELEM.
 Le calcul préalable d'un estimateur d'erreur est OBLIGATOIRE pour le calcul de cette option.
"""),

15: _("""
 Par défaut on utilise l'estimateur en résidu ERME_ELEM.
"""),

16 : _("""
 Par défaut on utilise l'estimateur basé sur les contraintes lissées version 2 ERZ2_ELEM.
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
 le calcul avec l'option ENDO_ELNO nécessite au préalable un calcul avec l'option ENDO_ELGA
"""),

22 : _("""
  L'option %(k1)s est inexistante.
"""),

23 : _("""
 option :  %(k1)s
"""),

24: _("""
 <I> L'estimateur que vous avez choisi pour le calcul de l'option SING_ELEM est %(k1)s.
"""),

25 : _("""
 calcul non disponible
"""),

26: _("""
 L'estimateur %(k1)s que vous avez choisi pour le calcul de l'option SING_ELEM
 n'existe pas dans la structure de donnée résultat %(k2)s.
 L'option SING_ELEM n'est pas calculée.
"""),

27 : _("""
 type :  %(k1)s  incompatible avec l'option :  %(k2)s
"""),

28 : _("""
 type de champ inconnu
"""),

29 : _("""
 Il n'y a pas de champ d'énergie dans la structure de donnée résultat.
 On ne calcule pas l'option SING_ELEM.
 Le calcul préalable de l'option  EPOT_ELEM ou ETOT_ELEM est OBLIGATOIRE
 pour le calcul de cette option.
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

36 : _("""
 A cause des alarmes précédentes, l'option SING_ELEM n'est pas calculée.
"""),

37 : _("""
 Attention : Certains ddls sont "imposés" plusieurs fois par AFFE_CHAR_CINE.
 Pour ces ddls, la valeur imposée sera la SOMME des différentes valeurs imposées.
 Ce n'est peut-etre pas ce qui est voulu.

 Exemple d'un ddl imposé plusieurs fois :
   Noeud : %(k1)s  Composante : %(k2)s

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





47 : _("""
Possible erreur d'utilisation :
  Vous voulez "poursuivre" un calcul non-linéaire (STAT_NON_LINE ou DYNA_NON_LINE).
  Pour cela, vous précisez un état initial (mot clé ETAT_INIT / EVOL_NOLI).
  Pour le calcul du 1er pas de temps, le champ des variables internes du début du pas est pris
  dans le concept evol_noli fourni.
  Pour l'élément porté par la maille %(k1)s, ce champ de variables internes a été calculé avec
  la relation de comportement %(k2)s, mais le comportement choisi pour le calcul est différent (%(k3)s).

Risques & conseils :
  Ce changement de comportement est-il volontaire ou s'agit-il d'une faute de frappe ?
"""),

48 : _("""
Possible erreur d'utilisation :
  Vous voulez "poursuivre" un calcul non-linéaire (STAT_NON_LINE ou DYNA_NON_LINE).
  Pour cela, vous précisez un état initial (mot clé ETAT_INIT / VARI=chvari).
  Pour le calcul du 1er pas de temps, le champ des variables internes utilisé pour le début du pas
  est "chvari".
  Pour l'élément porté par la maille %(k1)s, ce champ de variables internes n'a pas le meme nombre de
  variables internes (%(i1)d) que le nombre attendu par le comportement choisi pour le calcul (%(i2)d).

  Il y a donc un changement de comportement pour la maille %(k1)s

  Un changement de comportement lors d'un transitoire est a priori "douteux".
  Il semble que vous soyez dans l'un des cas tolérés par le code :
    / comportement "-" élastique
    / comportement "+" élastique

  Sur cet élément, les variables internes "-" sont mises à zéro.

Risques & conseils :
  Ce changement de comportement est-il volontaire ou s'agit-il d'une faute de frappe ?
"""),

49 : _("""
Erreur d'utilisation :
  Vous voulez "poursuivre" un calcul non-linéaire (STAT_NON_LINE ou DYNA_NON_LINE).
  Pour cela, vous précisez un état initial (mot clé ETAT_INIT / VARI=chvari).
  Pour le calcul du 1er pas de temps, le champ des variables internes utilisé pour le début du pas
  est "chvari".
  Pour l'élément porté par la maille %(k1)s, ce champ de variables internes n'a pas le meme nombre de
  variables internes (%(i1)d) que le nombre attendu par le comportement choisi pour le calcul (%(i2)d).

  Il y a donc un changement de comportement pour la maille %(k1)s
  Le code n'accepte de changement de comportement que dans quelques cas très particuliers :
    - LEMAITRE <-> VMIS_ISOT_XXXX
    - ELAS     <-> XXXX
  Il ne semble pas que vous soyez dans ce cas de figure. L'exécution est arretée.

Risques & conseils :
  Vérifiez le comportement affecté sur cette maille.
"""),

50 : _("""
 La commande a besoin d'un nom de modèle.
"""),

51 : _("""
  Erreur Utilisateur :
    On essaie d'utiliser dans la commande %(k1)s
    un modèle pour lequel des éléments finis ont été affectés directement
    sur des noeuds (AFFE_MODELE / AFFE / GROUP_NO).
    Ceci est interdit.

  Conseils :
    Il faut définir le modèle avec les mots clé GROUP_MA et MAILLE.
    Pour cela, il faut créer dans le maillage des mailles de type POI1.
    C'est possible avec la commande CREA_MAILLAGE / CREA_POI1.
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

55 : _("""
Erreur utilisateur dans CALC_ELEM:
  Pour pouvoir calculer SIEQ_ELNO sur un concept de type comb_fourier,
  il faut avoir calculé au préalable l'option 'SIEF_ELGA'
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

78 : _("""
Utilisation de LIAISON_ELEM / OPTION='%(k1)s', occurence %(i1)d :
Le noeud "poutre" (GROUP_NO_2) n'est pas situé géométriquement au même endroit que
le centre de gravité de la section (GROUP_MA_1). La distance entre les 2 noeuds est
supérieure à %(r7)g%% du "rayon" (Aire/Pi)^0.5 de la section.
   Position du centre de gravité de la section :
      %(r1)g   %(r2)g   %(r3)g
   Position du noeud "poutre" :
      %(r4)g   %(r5)g   %(r6)g
   Distance : %(r9)g
   Rayon    : %(r8)g
"""),

79 : _("""
 la matrice A est singulière
"""),

80 : _("""
 Utilisation de LIAISON_ELEM / OPTION='%(k1)s', occurence %(i1)d :
Le noeud "poutre" (GROUP_NO_2) n'est pas situé géométriquement au même endroit que
le centre de gravité de la section (GROUP_MA_1). La distance entre les 2 noeuds est
supérieure à %(r7)g%% du "rayon" (Aire/Pi)^0.5 de la section.
   Position du centre de gravité de la section :
      %(r1)g   %(r2)g   %(r3)g
   Position du noeud "poutre" :
      %(r4)g   %(r5)g   %(r6)g
   Distance : %(r9)g
   Rayon    : %(r8)g

Risque et conseils :
   Vérifiez la position du noeud "poutre".
   Rappel : on ne peut pas utiliser ce type de liaison pour relier une poutre avec
   une section 3D qui ne serait que partiellement maillée (symétrie du maillage).
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

88: _("""
 Les charges ne s'appuie pas sur le MODELE fourni.
"""),

89 : _("""
 les charges ne s'appuient pas toutes sur le même modèle.
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
    Fonctionnalité : projection de maillage
    On cherche à projeter des mailles sur certains noeuds.
    Mais la liste des noeuds que l'on arrive à projeter dans les mailles est vide.

  Conseil :
    Cette erreur peut venir d'une mauvaise utilisation du mot clé
    PROJ_CHAMP/DISTANCE_MAX
"""),

98 : _("""
 Le calcul de carte de taille et de détection de singularité n'est pas
 programmé en 3D pour les éléments de type HEXA, PENTA et PYRAM.
"""),

99 : _("""
 Problème de convergence pour calculer la nouvelle carte de taille.
"""),


}
