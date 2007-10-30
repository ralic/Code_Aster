#@ MODIF calculel2 Messages  DATE 29/10/2007   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={

2: _("""
 le CHAMP_S:  %(k1)s  est a la fois CHAM_ELEM_S et CHAM_NO_S.
"""),

3: _("""
 le CHAMP_S:  %(k1)s n'existe pas.
"""),

7: _("""
 trop d'antécédents
 vérifiez si le maillage de l'interface ne contient pas de noeuds coincidents ou diminuez DIST_REFE.
"""),

8: _("""
  %(k1)s  valeurs de chamno de deplacement n'ont pas ete recopiees sur  %(k2)s  noeuds a affecter  ce qui peut entrainer des erreurs de calcul sur la masse ajoutee des sous structures deduites par rotation et translation definies dans le modele  generalise. augmentez dist_refe ou assurez vous de l' invariance du maillage de structure par la translation et la rotation definies dans le modele generalise.
"""),

9: _("""
  -> plus de 50 %% des valeurs de CHAM_NO de déplacement n'ont pas été recopiées
     ce qui peut entrainer des erreurs graves de calcul sur la masse ajoutée des
     sous structures déduites par rotation et translation définies dans le modèle généralisé
  -> Risque & Conseil :
     augmentez DIST_REFE
"""),

10: _("""
 trop de noeuds affectés
"""),

12: _("""
 Le CHAM_NO %(k3)s est vide.
"""),

13: _("""
 Il manque la composante: %(k1)s  sur le noeud: %(k2)s pour le CHAM_NO: %(k3)s
"""),

21: _("""
 grandeur :  %(k1)s  inexistante au catalogue
"""),

22: _("""
 composante :  %(k1)s  inexistante au catalogue pour la grandeur : %(k2)s
"""),

23: _("""
 la grandeur : %(k1)s  n est pas de type reel.
"""),

24: _("""
 on traite un superelement  et le noeud courant n'est ni un noeud lagrange, ni un noeud physqiue du maillage.
"""),

25: _("""
 le ligrel :  %(k1)s  ne contient pas d elements finis
"""),

26: _("""
 l'option  %(k1)s  n'existe pas.
"""),

27: _("""
 le maillage associe au champ: %(k1)s  est different de celui associe au ligrel:  %(k2)s
"""),

28: _("""
  erreur programmeur : appel a calcul, le champ: %(k1)s  est un champ "in" et un champ "out".
"""),

29: _("""
 la grandeur associee au champ  %(k1)s : %(k2)s  n est pas celle associee au parametre  %(k3)s : %(k4)s  (option: %(k5)s
"""),

30: _("""
  on n'arrive pas a etendre la carte:  %(k1)s
"""),

31: _("""
 maille  %(k1)s  indisponible
"""),

32: _("""
 probleme creation champ
"""),

33: _("""
 pour le modele  %(k1)s  on ne peut pas visualiser les champs ensemble  %(k2)s  ... car les familles de pg sont differentes
"""),

35: _("""
 aucun élément du modèle n'est visualisable avec ECLA_PG
"""),

36: _("""
 on ne trouve aucun point de Gauss
"""),

37: _("""
 le type de RESU_INIT est différent de celui du résultat.
"""),

38: _("""
 la liste de numéros d'ordre est vide.
"""),

39: _("""
 les seuls champs autorisés pour ECLA_PG sont les champs réels.
"""),

40: _("""
 le champ:  %(k1)s  a des éléments ayant des sous-points
 ces elements ne seront pas traités
"""),

41: _("""
 les seuls champs autorises sont elga.
"""),

42: _("""
 le TYPE_ELEM:  %(k1)s  n'a pas le nombre de points de Gauss déclaré dans la routine ECLAU1
 nom_cham= %(k2)s
"""),

43: _("""
 nombre de noeuds > 27
"""),

45: _("""
 famille de pg "liste" interdite: %(k1)s
"""),

46: _("""
  mode ligne  %(k1)s  /= mode colonne  %(k2)s
"""),

47: _("""
  le mode  %(k1)s  de code  %(k2)s  reference le mode  %(k3)s  dont le code :  %(k4)s  > 3
"""),

48: _("""
  pour le mode  %(k1)s  nombre de points  %(k2)s  < argument k :  %(k3)s
"""),

49: _("""
 carte inexistante.
"""),

51: _("""
 cham_elem etendu a faire ...
"""),

52: _("""
 probleme noeud tardif pour un champ à représentation constante
"""),

55: _("""
  erreur dans l'extraction d'un resuelem pour le grel :  %(k1)s
  le champ n'existe pas
"""),

56: _("""
  erreur lors d'une extraction:
  le champ associe au parametre :  %(k1)s  n'est pas dans la liste des champs parametres.
"""),

61: _("""
 Erreur développeur :
 L'option que l'on calcule ne connait pas le paramètre :  %(k1)s
 Erreur probable dans un catalogue(typelem)
"""),

63: _("""
  -> La maille %(k1)s porte un élément fini de bord, mais elle ne borde
     aucun élément ayant une "rigidité".

  -> Risque & Conseil :
     Cela peut entrainer des problèmes de "pivot nul" lors de la résolution.
     Si la résolution des systèmes linéaires ne pose pas de problèmes, vous
     pouvez ignorer ce message.
     Sinon, vérifier la définition du modèle (AFFE_MODELE) en évitant l'utilisation
     de l'opérande TOUT='OUI'.
"""),

64: _("""
  -> Le modèle %(k1)s n'a pas d'éléments calculant de la rigidité.

  -> Risque & Conseil :
     Ce modèle ne poura donc pas (en général) etre utilisé pour faire des calculs.
     Vérifier la définition du modèle (AFFE_MODELE) et assurez-vous que les
     types de mailles du maillage (SEG2, TRIA3, QUAD4, ...) sont compatibles avec votre
     modélisation.
     Exemple d'erreur : affecter une modélisation "3D" sur un maillage formé de facettes.

"""),

68: _("""
 maille partiellement affectée.
"""),

69: _("""
 le parametre: %(k1)s  n'est pas un paramètre de l'option: %(k2)s
"""),

70: _("""
 le parametre: %(k1)s  n'est pas un paramètre de l'option: %(k2)s  pour le type_element:  %(k3)s
"""),

71: _("""
 on ne trouve pas dans les arguments de la routine CALCUL de champ à associer au parametre: %(k1)s
  - option: %(k2)s
  - type_element: %(k3)s
"""),

73: _("""
 on n'a pas pu extraire toutes les cmps voulues du champ associé au paramètre: %(k1)s
 - option: %(k2)s
 - type_element: %(k3)s )
"""),

79: _("""
 ce chargement n'est pas prévu en lagrange
"""),

80: _("""
 le calcul lagrangien avec les températures n'est pas encore disponible
"""),

81: _("""
 pas de chgeom
"""),

82: _("""
 il faut un MODELE
"""),

83: _("""
 il n'y a pas de rigidité sur le modèle.
"""),

84: _("""
 il n'y a pas de masse sur le modèle.
"""),

85: _("""
 G_BILI : champ initial impossible
"""),

86: _("""
 il faut fournir COMPOR
"""),

87: _("""
 impossible lire  %(k1)s
"""),

88: _("""
 option  %(k1)s  non disponible sur les éléments du modèle
 pas de champ créé
"""),

92: _("""
 votre chargement contient plus d'une charge répartie
 le calcul n'est pas possible pour les modèles de poutre.
"""),

93: _("""
  -> Vous avez renseigné un des mots-clés fonc_mult_*, coef_mult_*,
     PHAS_DEG, PUIS_PULS, or votre charge ne contient pas d'effort réparti
     sur des poutres. Ces mots-clés seront donc ignorés.
  -> Risque & Conseil :
"""),

94: _("""
 pour un modèle comportant des éléments de plaque ou de coque
 il faut fournir le "CARA_ELEM"
"""),

98: _("""
 la charge doit être une charge mécanique
"""),

99: _("""
 option  %(k1)s non licite pour un calcul non linéaire.
"""),
}
