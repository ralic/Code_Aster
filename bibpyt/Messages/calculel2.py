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

cata_msg={

1: _(u"""
Erreur utilisateur :
 Les deux champs suivants : %(k1)s et %(k2)s
 sont associés à deux maillages différents : %(k3)s et %(k4)s
Risques & conseils :
 Si l'un des champs est de type ETAT_INIT, il faut que son maillage
 soit le même que celui qui est associé au modèle.
"""),

2: _(u"""
 le CHAMP_S:  %(k1)s  est a la fois CHAM_ELEM_S et CHAM_NO_S.
"""),

3: _(u"""
 le CHAMP_S:  %(k1)s n'existe pas.
"""),

4: _(u"""
Erreur de programmation :
 On essaye de calculer l'intégrale d'un CHAM_ELEM / ELGA.
 Malheureusement, la famille de points de Gauss : %(k1)s n'est pas autorisée dans la programmation.

Conseil :
  Si nécessaire, il faut demander une évolution du code.
"""),

5: _(u"""
Erreur dans CREA_RESU :
  Quand on utilise la commande CREA_RESU avec le mot clé AFFE / CHAM_GD, les
  composantes du champ de fonctions %(k2)s (de la géométrie et/ou du temps),
  doivent être au même rang que celles du champ de réels %(k1)s.
Incohérence dans les grandeurs %(k1)s et %(k2)s :
  Le rang de la composante %(k3)s de %(k1)s correspond
  au rang de la composante %(k4)s de %(k2)s.

Conseil :
  Si nécessaire, il faut demander une évolution du code.
"""),

6: _(u"""
Erreur utilisateur dans PROJ_CHAMP :
  Le champ utilisé dans le mot clé CHAM_NO_REFE (%(k1)s) est associé au maillage %(k2)s
  Il doit être associé au maillage %(k3)s
"""),

7: _(u"""
 trop d'antécédents
 vérifiez si le maillage de l'interface ne contient pas de noeuds coïncidents ou diminuez DIST_REFE.
"""),

8: _(u"""
  %(k1)s  valeurs de CHAMNO de déplacement n'ont pas été recopiées sur  %(k2)s  noeuds
  a affecter  ce qui peut entraîner des erreurs de calcul sur la masse ajoutée des sous structures
  déduites par rotation et translation définies dans le modèle  généralisé. augmentez DIST_REFE
  ou assurez vous de l' invariance du maillage de structure par la translation et la rotation
  définies dans le modèle généralisé.
"""),

9: _(u"""
  -> plus de 50 %% des valeurs de CHAM_NO de déplacement n'ont pas été recopiées
     ce qui peut entraîner des erreurs graves de calcul sur la masse ajoutée des
     sous structures déduites par rotation et translation définies dans le modèle généralisé
  -> Risque & Conseil :
     augmentez DIST_REFE
"""),

10: _(u"""
 trop de noeuds affectés
"""),

11: _(u"""
 Erreur d'utilisation :
   Le maillage associé au modèle : %(k1)s
   n'est pas le même que celui du champ de matériaux : %(k2)s
"""),

12: _(u"""
Erreur lors de la transformation du CHAM_NO_S (%(k1)s) en CHAM_NO (%(k2)s):
Le CHAM_NO_S est vide (i.e. il n'a aucune valeur).
"""),

13: _(u"""
Erreur lors d'une transformation d'un CHAM_NO_S en CHAM_NO :
  Il manque la composante: %(k1)s  sur le noeud: %(k2)s pour le CHAM_NO: %(k3)s

Risques & conseils :
  Si cette erreur se produit dans la commande CREA_CHAMP, il est possible de
  mettre à zéro les composantes manquantes en utilisant le mot-clé PROL_ZERO.
"""),

14: _(u"""
Erreur utilisateur dans la commande POST_CHAMP :
 On demande l'extraction des champs sur une couche de numéro supérieur au nombre de couches.
"""),

15: _(u"""
Erreur utilisateur dans la commande POST_CHAMP :
 On demande l'extraction pour des champs n'ayant pas de "sous-points".
"""),

16: _(u"""
Erreur utilisateur dans la commande POST_CHAMP :
 On demande l'extraction des champs sur une fibre de numéro supérieur au nombre de fibres.
"""),

17: _(u"""
Erreur utilisateur dans la commande CREA_CHAMP :
 Incohérence entre le champ %(k1)s associé au maillage %(k2)s
 et le maillage %(k3)s
"""),

18: _(u"""
Erreur utilisateur dans la commande POST_CHAMP / MIN_MAX_SP :
 La composante demandée %(k1)s n'est pas trouvée dans le champ %(k2)s
"""),

19: _(u"""
Erreur utilisateur dans la commande POST_CHAMP / COQUE_EXCENT :
 Pour l'occurrence %(i1)d du mot clé COQUE_EXCENT,
 et pour le numéro d'ordre %(i2)d le champ calculé est vide.
"""),

20: _(u"""
Erreur utilisateur dans la commande POST_CHAMP / COQUE_EXCENT :
 La structure de donnée produite est vide.
"""),

21: _(u"""
 grandeur :  %(k1)s  inexistante au catalogue
"""),

22: _(u"""
 composante :  %(k1)s  inexistante au catalogue pour la grandeur : %(k2)s
"""),

23: _(u"""
 la grandeur : %(k1)s  n est pas de type réel.
"""),

24: _(u"""
 on traite un super-élément  et le noeud courant n'est ni un noeud Lagrange,
 ni un noeud physique du maillage.
"""),

25: _(u"""
 le LIGREL :  %(k1)s  ne contient pas d éléments finis
"""),

26: _(u"""
 l'option  %(k1)s  n'existe pas.
"""),

27: _(u"""
 Erreur utilisateur :
   Le maillage associé au champ: %(k1)s  (%(k3)s)
   est différent de celui associe au LIGREL:  %(k2)s  (%(k4)s)
"""),

28: _(u"""
  erreur programmeur : appel a calcul, le champ: %(k1)s  est un champ "in" et un champ "out".
"""),

29: _(u"""
 la grandeur associée au champ  %(k1)s : %(k2)s
 n'est pas celle associée au paramètre  %(k3)s : %(k4)s  (option: %(k5)s
"""),

30: _(u"""
  on n'arrive pas a étendre la carte:  %(k1)s
"""),

31: _(u"""
Erreur utilisateur dans la commande AFFE_CARA_ELEM :
  On a affecté un excentrement non nul (mot clé COQUE / EXCENTREMENT)
  sur un élément qui ne sait pas traiter l'excentrement (maille %(k1)s).
"""),

33: _(u"""
Erreur Utilisateur :
 Pour le modèle  %(k1)s  on ne peut pas visualiser ensemble plusieurs champs ELGA (%(k2)s,  ...)
 car les familles de points de Gauss sont différentes
"""),

35: _(u"""
Erreur Utilisateur :
 Aucun élément du modèle n'est visualisable avec ECLA_PG
"""),

36: _(u"""
 On ne trouve aucun point de Gauss
"""),

37: _(u"""
 le type de RESU_INIT est différent de celui du résultat.
"""),

38: _(u"""
 la liste de numéros d'ordre est vide.
"""),

39: _(u"""
 les seuls champs autorisés pour ECLA_PG sont les champs réels.
"""),

40: _(u"""
Erreur :
 Après avoir retiré tous les éléments à sous-points du champ %(k1)s (grandeur: %(k2)s), celui-ci est vide.
"""),

41: _(u"""
 les seuls champs autorises sont ELGA.
"""),

42: _(u"""
 le TYPE_ELEM:  %(k1)s  n'a pas le nombre de points de Gauss déclaré dans la routine ECLAU1.
"""),

43: _(u"""
 nombre de noeuds > 27
"""),

44: _(u"""
   Le modèle n'a pas été trouvé. Le calcul n'est pas possible.
"""),

45: _(u"""
 famille de points de Gauss "liste" interdite: %(k1)s
"""),

46: _(u"""
  mode ligne  %(k1)s  /= mode colonne  %(k2)s
"""),

47: _(u"""
  le mode  %(k1)s  de code  %(k2)s  référence le mode  %(k3)s  dont le code :  %(k4)s  > 3
"""),

48: _(u"""
  pour le mode  %(k1)s  nombre de points  %(k2)s  < argument k :  %(k3)s
"""),

49: _(u"""
 carte inexistante.
"""),

50: _(u"""
Erreur utilisateur :
  Le champ %(k1)s n'est pas associé au maillage %(k2)s.
"""),

51: _(u"""
 Erreur :
   Le code cherche à utiliser dans un calcul élémentaire un CHAM_ELEM "étendu" (VARI_R ou sous-points).
   La programmation de la routine exchml.f ne sait pas encore traiter ce cas.
 Conseil :
   Il y a peut-être lieu d'émettre une demande d'évolution pour traiter ce cas.
"""),

52: _(u"""
 problème noeud tardif pour un champ à représentation constante
"""),

53: _(u"""
 Le calcul de l'option %(k1)s n'est pas possible. Il manque le CARA_ELEM.
"""),

54: _(u"""
 Le calcul de l'option %(k1)s n'est pas possible. Il manque le CHAM_MATER.
"""),

55: _(u"""
 Le calcul de l'option %(k1)s n'est pas possible. Il manque le MODELE.
"""),

56: _(u"""
  erreur lors d'une extraction:
  le champ associe au paramètre :  %(k1)s  n'est pas dans la liste des champs paramètres.
"""),








61: _(u"""
 Erreur développeur :
 L'option que l'on calcule ne connaît pas le paramètre :  %(k1)s
 Erreur probable dans un catalogue(typelem)
"""),

62: _(u"""
 Erreur utilisateur POST_CHAMP /MIN_MAX_SP :
  Il n'y a rien à calculer car le champ  %(k1)s n'existe pas pour les numéros d'ordre indiqués.
"""),

63: _(u"""
  -> La maille %(k1)s porte un élément fini de bord, mais elle ne borde
     aucun élément ayant une "rigidité".

  -> Risque & Conseil :
     Cela peut entraîner des problèmes de "pivot nul" lors de la résolution.
     Si la résolution des systèmes linéaires ne pose pas de problèmes, vous
     pouvez ignorer ce message.
     Sinon, vérifier la définition du modèle (AFFE_MODELE) en évitant l'utilisation
     de l'opérande TOUT='OUI'.
"""),

64: _(u"""
  -> Le modèle %(k1)s n'a pas d'éléments sachant calculer la rigidité.

  -> Risque & Conseil :
     Ce modèle ne pourra donc pas (en général) être utilisé pour faire des calculs.
     Vérifier la définition du modèle (AFFE_MODELE) et assurez-vous que les
     types de mailles du maillage (SEG2, TRIA3, QUAD4, ...) sont compatibles avec votre
     modélisation.
     Exemples d'erreur :
       * affecter une modélisation "3D" sur un maillage formé de facettes.
       * affecter une modélisation qui ne sait pas traiter tous les types de mailles du maillage
         (par exemple 'PLAN_DIAG' en thermique, 'AXIS_SI' en mécanique)
"""),

65: _(u"""
Erreur d'utilisation :
  -> Le modèle %(k1)s n'a pas d'éléments sachant calculer la rigidité.

  -> Risque & Conseil :
     Ce modèle ne peut pas être utilisé pour faire des calculs.
     Vérifier la définition du modèle (AFFE_MODELE) et assurez-vous que les
     types de mailles du maillage (SEG2, TRIA3, QUAD4, ...) sont compatibles avec votre
     modélisation.
     Exemples d'erreur :
       * affecter une modélisation "3D" sur un maillage formé de facettes.
       * affecter une modélisation qui ne sait pas traiter tous les types de mailles du maillage
         (par exemple 'PLAN_DIAG' en thermique, 'AXIS_SI' en mécanique)
"""),

66: _(u"""Erreur d'utilisation :
 On ne peut pas utiliser plus de 50 paramètres pour évaluer une fonction.
 Ici, les différents champs du mot-clé CHAM_PARA possèdent au total plus de 50 composantes.
"""),

67: _(u"""Erreur d'utilisation :
 On ne peut pas filtrer les mailles de type %(k1)s car ce n'est pas un type de maille connu.
"""),

68: _(u"""
 maille partiellement affectée.
"""),

69: _(u"""
 le paramètre: %(k1)s  n'est pas un paramètre de l'option: %(k2)s
"""),

70: _(u"""
 le paramètre: %(k1)s  n'est pas un paramètre de l'option: %(k2)s  pour le type_élément:  %(k3)s
"""),

71: _(u"""
 on ne trouve pas dans les arguments de la routine CALCUL de champ à associer au paramètre: %(k1)s
  - option: %(k2)s
  - type_élément: %(k3)s
"""),

72: _(u"""
Erreur utilisateur dans un calcul élémentaire de forces réparties :
  On n'a pas trouvé toutes les composantes voulues du champ pour le paramètre : %(k1)s
   - option        : %(k2)s
   - type_élément  : %(k3)s
   - maille        : %(k4)s
  On a trouvé un noeud sur lequel il existe des composantes mais pas toutes.
  On ne peut pas continuer

Risques et conseils :
  Si le champ provient de CREA_CHAMP/AFFE, vérifier que vous avez bien affecté FX,FY [FZ]
"""),

73: _(u"""
Erreur dans un calcul élémentaire :
  On n'a pas trouvé toutes les composantes voulues du champ pour le paramètre : %(k1)s
   - option        : %(k2)s
   - type_élément  : %(k3)s
   - maille        : %(k4)s

Risques et conseils :
  Certaines informations sur le contexte de cette erreur sont imprimées ci-dessous.
  Elles peuvent aider à comprendre une éventuelle erreur d'utilisation.
"""),

74: _(u"""
Erreur utilisateur dans un calcul élémentaire :
  Le matériau est nécessaire sur la maille : %(k4)s
  - option de calcul élémentaire : %(k2)s
  - type_élément                 : %(k3)s

Conseils :
  * Peut-être avez-vous oublié de renseigner le mot clé CHAM_MATER dans la commande courante.
  * Dans la commande AFFE_MATERIAU, avez-vous affecté un matériau sur la maille incriminée ?
"""),

75: _(u"""
Erreur utilisateur dans un calcul élémentaire :
  Des caractéristiques de "coque" sont nécessaires sur la maille : %(k4)s
  - option de calcul élémentaire : %(k2)s
  - type_élément                 : %(k3)s

Conseils :
  * Peut-être avez-vous oublié de renseigner le mot clé CARA_ELEM dans la commande courante.
  * Dans la commande AFFE_CARA_ELEM, avez-vous affecté des caractéristiques de "coque"
    sur la maille incriminée ?
"""),

76: _(u"""
Erreur utilisateur dans un calcul élémentaire :
  Des caractéristiques de "poutre" sont nécessaires sur la maille : %(k4)s
  - option de calcul élémentaire : %(k2)s
  - type_élément                 : %(k3)s

Conseils :
  * Peut-être avez-vous oublié de renseigner le mot clé CARA_ELEM dans la commande courante.
  * Dans la commande AFFE_CARA_ELEM, avez-vous affecté des caractéristiques de "poutre"
    sur la maille incriminée ?
"""),

77: _(u"""
Erreur utilisateur dans un calcul élémentaire :
  Des caractéristiques d'"orientation" sont nécessaires sur la maille : %(k4)s
  - option de calcul élémentaire : %(k2)s
  - type_élément                 : %(k3)s

Conseils :
  * Peut-être avez-vous oublié de renseigner le mot clé CARA_ELEM dans la commande courante.
  * Dans la commande AFFE_CARA_ELEM, avez-vous affecté des caractéristiques d'"orientation"
    sur la maille incriminée ?
"""),

78: _(u"""
Erreur utilisateur :
  Pour évaluer une fonction, Il ne faut pas fournir plusieurs fois le même paramètre.
  Ici, le paramètre %(k1)s a été fourni plus d'une fois.

Conseil :
  Si les champs utilisés avec le mot clé CHAM_PARA ont été obtenus avec la commande CREA_CHAMP,
  on peut voir leur contenu avec le mot clé INFO=2.
"""),

79: _(u"""
Erreur dans CREA_RESU :
  Quand on utilise la commande CREA_RESU / EVOL_VARC avec le mot clé AFFE / CHAM_GD et
  que le champ de fonctions est %(k2)s_F (de la géométrie et/ou du temps),
  NOM_CHAM doit être %(k2)s et pas %(k1)s.
"""),

80: _(u"""
Erreur dans CREA_RESU :
  Quand on utilise la commande CREA_RESU avec le mot clé AFFE / CHAM_GD :
  le champ de fonctions %(k2)s doit avoir le même nombre d'entier codés que le
  champ de réels %(k1)s.
    %(k2)s : %(i2)d entier codé
    %(k1)s : %(i1)d entier codé

Conseil :
  Si nécessaire, il faut demander une évolution du code.
"""),

81: _(u"""
Erreur utilisateur :
  Calcul de la déformation thermique d'un élément de grille.
  On ne trouve pas de température sur le maille %(k1)s.
"""),

82: _(u"""
 il faut un MODELE
"""),

86: _(u"""
 La carte de COMPORTEMENT est absente.
 Votre résultat a peut-être été produit par LIRE_RESU ou CREA_RESU.
 Si votre résultat a été produit par LIRE_RESU, il faut renseigner le mot-clé COMP_INCR.
"""),

88: _(u"""
 L'option %(k1)s  n'est disponible pour aucun des éléments de votre modèle.
 Le calcul d'indicateur d'erreur est donc impossible.
"""),

89: _(u"""
 Alarme utilisateur :
   Le champ  %(k1)s  n'a pas pu être calculé.

 Risques & conseils :
   * Si le champ est un champ par éléments, c'est que le calcul élémentaire n'est pas disponible
     pour les éléments finis utilisés. Cela peut se produire soit parce que ce
     calcul n'a pas été encore programmé, soit parce que ce calcul n'a pas de sens.
     Par exemple, le champ EFGE_ELNO n'a pas de sens pour les éléments de la modélisation '3D'.
   * Si le champ est un champ aux noeuds (XXXX_NOEU), cela veut dire que le champ XXXX_ELNO
     n'existe pas sur les éléments spécifiés.
     Par exemple, le calcul de SIGM_NOEU sur les éléments de bord est impossible.

"""),

90: _(u"""
Erreur dans CREA_RESU :
  Quand on utilise la commande CREA_RESU avec le mot clé AFFE / CHAM_GD et que
  le champ de fonctions est %(k2)s_F (de la géométrie et/ou du temps),
  le champ de réels doit être %(k1)s_R.

Conseil :
  Vérifier la construction du champ de fonctions est %(k2)s_F
"""),

92: _(u"""
 votre chargement contient plus d'une charge répartie
 le calcul n'est pas possible pour les modèles de poutre.
"""),

94: _(u"""
 pour un modèle comportant des éléments de plaque ou de coque
 il faut fournir le "CARA_ELEM"
"""),

}
