#@ MODIF calculel2 Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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
Erreur utilisateur dans CREA_RESU :
  Quand on utilise la commande CREA_RESU avec le mot clé AFFE / CHAM_GD et que le
  champ est un champ de fonctions (de la géométrie et/ou du temps), il faut que la grandeur
  associée à ce champ soit TEMP_F, DEPL_F, PRES_F ou FORC_F.

  Ici, la grandeur est : %(k1)s
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
Erreur lors d'une transformation de CHAM_NO_S en CHAM_NO :
 Il manque la composante: %(k1)s  sur le noeud: %(k2)s pour le CHAM_NO: %(k3)s
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






41: _(u"""
 les seuls champs autorises sont ELGA.
"""),

42: _(u"""
 le TYPE_ELEM:  %(k1)s  n'a pas le nombre de points de Gauss déclaré dans la routine ECLAU1.
"""),

43: _(u"""
 nombre de noeuds > 27
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






56: _(u"""
  erreur lors d'une extraction:
  le champ associe au paramètre :  %(k1)s  n'est pas dans la liste des champs paramètres.
"""),

61: _(u"""
 Erreur développeur :
 L'option que l'on calcule ne connaît pas le paramètre :  %(k1)s
 Erreur probable dans un catalogue(typelem)
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





81: _(u"""
 pas de champ de géométrie
"""),

82: _(u"""
 il faut un MODELE
"""),

86: _(u"""
 La carte de COMPORTEMENT est absente.
 Votre résultat a peut-être été produit par LIRE_RESU ou CREA_RESU.
 Si votre résultat a été produit par LIRE_RESU, il faut renseigner le mot-clé COMP_INCR.
"""),

87: _(u"""
 impossible lire  %(k1)s
"""),

88: _(u"""
 L'option %(k1)s  n'est disponible pour aucun des éléments de votre modèle.
 Le calcul d'indicateur d'erreur est donc impossible.
"""),

89: _(u"""
 option  %(k1)s  non disponible sur les éléments du modèle
 pas de champ créé
"""),

92: _(u"""
 votre chargement contient plus d'une charge répartie
 le calcul n'est pas possible pour les modèles de poutre.
"""),

93: _(u"""
  -> Vous avez renseigné un des mots-clés FONC_MULT_*, COEF_MULT_*,
     PHAS_DEG, PUIS_PULS, or votre charge ne contient pas d'effort réparti
     sur des poutres. Ces mots-clés seront donc ignorés.
  -> Risque & Conseil :
"""),

94: _(u"""
 pour un modèle comportant des éléments de plaque ou de coque
 il faut fournir le "CARA_ELEM"
"""),

98: _(u"""
 la charge doit être une charge mécanique
"""),

99: _(u"""
 option  %(k1)s non licite pour un calcul non linéaire.
"""),
}
