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
 manque les accélérations
"""),

2 : _(u"""
 pour une SD RESULTAT de type DYNA_TRANS,
 seuls les mots-clés FONC_MULT et COEF_MULT sont autorisés
"""),

3 : _(u"""
 pour une SD RESULTAT de type  EVOL_ELAS,
 seul le mot-clé FONC_MULT est autorisé
"""),

4 : _(u"""
 l'utilisation du mot-clé FONC_MULT n'est licite que pour
 les structures de données résultat :  EVOL_ELAS, DYNA_TRANS, DYNA_HARMO
"""),

6 : _(u"""
 La composante %(k1)s n'existe pas dans le champ sur la maille spécifiée.
"""),

7 : _(u"""
 Calcul de  %(k1)s  impossible.
"""),

11 : _(u"""
 le résultat  %(k1)s  doit comporter un champ de déplacement au numéro d'ordre  %(k2)s  .
"""),

12 : _(u"""
 Le mot clé PREC_ERR est OBLIGATOIRE avec l'option SING_ELEM.
 Il faut renseigner le mot clé PREC_ERR avec une valeur comprise entre 0 et 1.
"""),

13 : _(u"""
 Le mot clé PREC_ERR doit être strictement positif.
"""),

14 : _(u"""
 Il n'y a pas de champ d'estimateur d'erreur dans la structure de donnée résultat.
 On ne calcule pas l'option SING_ELEM.
 Le calcul préalable d'un estimateur d'erreur est OBLIGATOIRE pour le calcul de cette option.
"""),

15: _(u"""
 Par défaut on utilise l'estimateur en résidu ERME_ELEM.
"""),

16 : _(u"""
 Par défaut on utilise l'estimateur basé sur les contraintes lissées version 2 ERZ2_ELEM.
"""),

17 : _(u"""
Erreur utilisateur dans la commande CREA_CHAMP / EXTR :
   Le champ que l'on veut extraire (%(k1)s n'existe pas dans la structure
   de donnée CARA_ELEM ou CHAR_MECA.

Conseil :
  Pour "voir" les champs existants dans la structure de donnée XXXX,
  vous pouvez utiliser la commande :
   IMPR_CO(CONCEPT=_F(NOM=XXXX), NIVEAU=-1)

"""),

18 : _(u"""
 Erreur utilisateur :
   Pour les modélisations DKTG et Q4GG, la température doit être 
   fournie sous le forme 'TEMP_SUP', 'TEMP_INF', ['TEMP_MIL'] et
   non pas sous la forme 'TEMP'.
"""),

19 : _(u"""
 problème à l'appel de ALCHML pour  %(k1)s
"""),




22 : _(u"""
  L'option %(k1)s est inexistante.
"""),

24: _(u"""
 <I> L'estimateur que vous avez choisi pour le calcul de l'option SING_ELEM est %(k1)s.
"""),

26: _(u"""
 L'estimateur %(k1)s que vous avez choisi pour le calcul de l'option SING_ELEM
 n'existe pas dans la structure de donnée résultat %(k2)s.
 L'option SING_ELEM n'est pas calculée.
"""),

27 : _(u"""
 type :  %(k1)s  incompatible avec l'option :  %(k2)s
"""),

28 : _(u"""
PROJ_CHAMP / METHODE='ECLA_PG' :
 On va traiter les mailles de dimension :  %(i1)d
 Les autres mailles sont ignorées
"""),

29 : _(u"""
 Il n'y a pas de champ d'énergie dans la structure de donnée résultat.
 On ne calcule pas l'option SING_ELEM.
 Le calcul préalable de l'option  EPOT_ELEM ou ETOT_ELEM est OBLIGATOIRE
 pour le calcul de cette option.
"""),





31 : _(u"""
 la masse du MACR_ELEM : %(k1)s  n'a pas encore été calculée.
"""),

32 : _(u"""
 il manque des masses.
"""),

33 : _(u"""
 la rigidité du MACR_ELEM : %(k1)s  n'a pas encore été calculée.
"""),

34 : _(u"""
 il manque des rigidités.
"""),

35 : _(u"""
 le modèle doit contenir des éléments finis ou des sous-structures.
"""),

36 : _(u"""
 A cause des alarmes précédentes, l'option SING_ELEM n'est pas calculée.
"""),

37 : _(u"""
 Attention : Certains ddls sont "imposés" plusieurs fois par AFFE_CHAR_CINE.
 Pour ces ddls, la valeur imposée sera la SOMME des différentes valeurs imposées.
 Ce n'est peut-être pas ce qui est voulu.

 Exemple d'un ddl imposé plusieurs fois :
   Noeud : %(k1)s  Composante : %(k2)s

"""),

38 : _(u"""
 on ne traite pas le type_scalaire: %(k1)s
"""),

39 : _(u"""
 le modèle contient des éléments de structure
 il faut probablement utiliser le mot-clé CARA_ELEM.
"""),

40 : _(u"""
  -> Le modèle a probablement besoin d'un champ de matériau (mot-clé CHAM_MATER).

  -> Risque & Conseil :
     Ce message peut aider à comprendre un éventuel problème ultérieur lors de calculs élémentaires
     nécessitant des caractéristiques matérielles.
     Vérifiez si votre modélisation nécessite un CHAM_MATER.
"""),

41 : _(u"""
 les charges ne s'appuient pas toutes sur le même modèle.
"""),

42 : _(u"""
 les charges ne s'appuient pas sur le modèle donné en argument.
"""),

43 : _(u"""
 les charges sont de type différent.
"""),

44 : _(u"""
 les charges ne s'appuient pas toutes sur le même modèle
"""),

45 : _(u"""
 données incorrectes.
"""),

46 : _(u"""
La MATR_ASSE et le CHAM_NO ont des numérotations différentes (%(k1)s et %(k2)s).
Si la MATR_ASSE contient des ddls LAGR, ceux-ci sont mis à zéro.
"""),

47 : _(u"""
Alarme utilisateur pour le calcul de SIRO_ELEM :
  La maille volumique %(k1)s qui borde la maille de peau %(k2)s
  est dégénérée ou bien elle n'est pas du côté "-" de la maille de peau.
  On ne fera donc pas le calcul de SIRO_ELEM sur la maille de peau.

Conseil :
  Le problème vient peut-être du fait que la peau du maillage est mal
  orientée. On peut réorienter les mailles de peau avec la commande
  MODI_MAILLAGE / ORIE_PEAU_3D.
"""),

48 : _(u"""
Erreur utilisateur (EXTR_RESU / RESTREINT) :
 Le concept fourni après le mot clé CARA_ELEM : (%(k1)s) est associé
 au maillage : %(k3)s.
 Mais le maillage associé à la SD RESULTAT est différent : %(k2)s.
"""),

49 : _(u"""
Erreur utilisateur (EXTR_RESU / RESTREINT) :
 Le concept fourni après le mot clé CHAM_MATER : (%(k1)s) est associé
 au maillage : %(k3)s.
 Mais le maillage associé à la SD RESULTAT est différent : %(k2)s.
"""),

50 : _(u"""
 La commande a besoin d'un nom de modèle.
"""),

51 : _(u"""
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

52 : _(u"""
 le champ doit être un CHAM_ELEM.
"""),

53 : _(u"""
 ne traite qu'un CHAM_ELEM réel
"""),

54 : _(u"""
 longueurs des modes locaux incompatibles entre eux.
"""),

57 : _(u"""
 on ne sait pas moyenner cette composante négative
"""),

58 : _(u"""
 champs sur des modèles différents
"""),

59 : _(u"""
  %(k1)s  doit être un CHAM_ELEM.
"""),

60 : _(u"""
 longueurs des modes locaux champ1 incompatibles entre eux.
"""),

61 : _(u"""
 longueurs des modes locaux champ2 incompatibles entre eux.
"""),

62 : _(u"""
 composante non définie
"""),





71 : _(u"""
 il faut 1 chargement de rotation et un seul.
"""),

72 : _(u"""
  il ne faut pas définir plus d"un champ de vitesse
"""),

73 : _(u"""
 le champ:  %(k1)s  n'est ni un CHAM_ELEM ni un resuelem
"""),

74 : _(u"""
 type scalaire interdit : %(k1)s
"""),

78 : _(u"""
Utilisation de LIAISON_ELEM / OPTION='%(k1)s', occurrence %(i1)d :
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

79 : _(u"""
 la matrice A est singulière
"""),

80 : _(u"""
 Utilisation de LIAISON_ELEM / OPTION='%(k1)s', occurrence %(i1)d :
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

81 : _(u"""
 cette fonction ne marche que pour des modes locaux de type champ aux noeuds, vecteur, ou matrice.
"""),

82 : _(u"""
 le mode local est de type matrice non_carrée
"""),

84 : _(u"""
 il n y a pas de paramètre  %(k1)s  associe a la grandeur: %(k2)s  dans l option: %(k3)s
"""),

85 : _(u"""
 il y a plusieurs paramètres  %(k1)s  associes a la grandeur: %(k2)s  dans l option: %(k3)s
"""),

88: _(u"""
 Les charges ne s'appuie pas sur le MODELE fourni.
"""),

89 : _(u"""
 les charges ne s'appuient pas toutes sur le même modèle.
"""),

90 : _(u"""
 le champ %(k1)s doit être une CARTE.
"""),

91 : _(u"""
 une des charges n'est pas mécanique
"""),

92 : _(u"""
 erreur: une des charges n'est pas thermique
"""),

93 : _(u"""
 une des charges n'est pas acoustique
"""),



96 : _(u"""
  Pour prendre en compte les termes d'inertie,
  il est préférable d'utiliser la commande CALC_CHAMP.
  Le mot-clé ACCE n'est pas traité et les résultats risquent d'être faux.
"""),

97 : _(u"""
  Erreur d'utilisation :
    Fonctionnalité : projection de maillage
    On cherche à projeter des mailles sur certains noeuds.
    Mais la liste des noeuds que l'on arrive à projeter dans les mailles est vide.

  Conseil :
    Cette erreur peut venir d'une mauvaise utilisation du mot clé
    PROJ_CHAMP/DISTANCE_MAX
"""),

98 : _(u"""
 Le calcul de carte de taille et de détection de singularité n'est pas
 programmé en 3D pour les éléments de type HEXA, PENTA et PYRAM.
"""),

99 : _(u"""
 Problème de convergence pour calculer la nouvelle carte de taille.
"""),


}
