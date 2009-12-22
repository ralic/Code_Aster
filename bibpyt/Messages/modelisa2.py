#@ MODIF modelisa2 Messages  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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

1: _("""
 Formule interdite pour définir ALPHA(TEMP) : la fonction soit etre tabulée.
 Utilisez CALC_FONC_INTERP
"""),

2: _("""
 resserrer le mot cle PRECISION pour le materiau ELAS_FO
"""),

3: _("""
 calcul de la tension le long du cable numéro %(k1)s  :
 la longueur sur laquelle on devrait prendre en compte les pertes de tension par recul de l ancrage
 est supérieure à la longueur du cable"""),

4: _("""
  calcul de la tension le long du cable numéro %(k1)s  :
  la longueur sur laquelle on doit prendre en compte les pertes de tension par recul de l ancrage
  est égale à la longueur du cable"""),

5: _("""
 Formule interdite pour le calcul d'intégrale : la fonction soit etre tabulée.
 Utilisez CALC_FONC_INTERP pour tabuler la formule %(k1)s
"""),

9: _("""
  Erreur utilisateur :
    l'objet %(k1)s n'existe pas. On ne peut pas continuer.
  Risques & conseils :
    Dans ce contexte, les seuls solveurs autorisés sont MULT_FRONT et LDLT
"""),

13: _("""
 problème pour recuperer une grandeur dans la table CARA_GEOM
"""),

14: _("""
 plus petite taille de maille négative ou nulle
"""),

15: _("""
 groupe de maille GROUP_MA_1= %(k1)s  inexistant dans le maillage  %(k2)s
"""),

16: _("""
 groupe de maille GROUP_MA_2= %(k1)s  inexistant dans le maillage  %(k2)s
"""),

17: _("""
 les groupes de mailles GROUP_MA_1= %(k1)s  et GROUP_MA_2= %(k2)s  ont des cardinaux différents
"""),

18: _("""
 nombre de noeuds incoherent sous les 2 GROUP_MA a coller
"""),

19: _("""
 un noeud de group_ma_2 n est geometriquement appariable avec aucun de GROUP_MA_1
"""),

20: _("""
 les 2 maillages doivent être du même type : 2d (ou 3d).
"""),

21: _("""
  -> Le group_ma %(k1)s est présent dans les 2 maillages que l'on assemble.
     Il y a conflit de noms. Le group_ma du 2eme maillage est renommé.
  -> Risque & Conseil :
     Vérifiez que le nom du group_ma retenu est bien celui souhaité.
"""),

22: _("""
  -> Le group_no %(k1)s est présent dans les 2 maillages que l'on assemble.
     Il y a conflit de noms. Le group_no du 2eme maillage est renommé.
  -> Risque & Conseil :
     Vérifiez que le nom du group_no retenu est bien celui souhaité.
"""),

23: _("""
 traitement non prévu pour un modèle avec mailles tardives.
"""),

24: _("""
 absence de carte d'orientation des éléments. la structure etudiée n'est pas une poutre.
"""),

25: _("""
 problème pour déterminer les rangs des composantes <ALPHA> , <BETA> , <GAMMA> de la grandeur <CAORIE>
 """),

26: _("""
 erreur a l'appel de la routine ETENCA pour extension de la carte  %(k1)s .
"""),

27: _("""
 Détection d'un élément d'un type non admissible. La structure étudiée n'est pas une poutre droite.
"""),

28: _("""
 l'élément supporté par la maille numéro %(k1)s  n'a pas ete orienté.
"""),

29: _("""
 carte d'orientation incomplète pour l'élément supporté par la maille numéro %(k1)s .
"""),

30: _("""
 les éléments de la structure ne sont pas d'un type attendu. la structure étudiée n'est pas une poutre droite.
"""),

31: _("""
 l'axe directeur de la poutre doit etre parallèle avec l'un des axes du repère global.
"""),

32: _("""
 la structure étudiée n'est pas une poutre droite.
"""),


37: _("""
 valeur inattendue:  %(k1)s
"""),

38: _("""
 les courbures KY et KZ ne sont pas prises en compte pour les poutres courbes
"""),

42: _("""
Erreur Utilisateur :
 Le paramètre ALPHA (dilatation) du matériau est une fonction de la température.
 Cette fonction (%(k1)s) n'est définie que par un point.
 TEMP_DEF_ALPHA et TEMP_REF ne sont pas identiques.
 On ne peut pas faire le changement de variable TEMP_DEF_ALPHA -> TEMP_REF.
 On s'arrête donc.

Risque & Conseil:
 Il faut définir la fonction ALPHA avec plus d'un point.
"""),



43: _("""
 deux mailles POI1 interdit
"""),

45: _("""
 aucun noeud ne connait le DDL:  %(k1)s
"""),

46: _("""
 le descripteur_grandeur associé au modèle ne tient pas sur dix entiers codés
"""),

47: _("""
 FONREE non traite  %(k1)s
"""),

48: _("""
 recuperation des caracteristiques elementaires du cable no %(k1)s  : detection d un element different du type <meca_barre>
"""),

49: _("""
 les caractéristiques matérielles n'ont pas ete affectées à la maille no %(k1)s  appartenant au cable no %(k2)s
 """),

50: _("""
 des matériaux différents ont été affectés aux mailles appartenant au cable no %(k1)s
"""),

51: _("""
 récupération des caractéristiques du matériau ACIER associé au cable no %(k1)s  : absence de relation de comportement de type <elas>
"""),

52: _("""
 récupération des caractéristiques du matériau ACIER associé au cable no %(k1)s , relation de comportement <ELAS> : module d'Young indéfini
"""),

53: _("""
 récupération des caractéristiques du matériau ACIER associé au cable no %(k1)s , relation de comportement <ELAS> : valeur invalide pour le module d'Young
"""),

54: _("""
 récupération des caractéristiques du matériau ACIER associé au cable no %(k1)s  : absence de relation de comportement de type <BPEL_ACIER>
"""),

55: _("""
 récupération des caractéristiques du matériau ACIER associé au cable no %(k1)s , relation de comportement <BPEL_ACIER> : au moins un paramètre indéfini
"""),

56: _("""
 récupération des caractéristiques du matériau ACIER associé au cable no %(k1)s , relation de comportement <BPEL_ACIER> : au moins une valeur de paramètre invalide
 """),

57: _("""
 les caractéristiques géométriques n'ont pas été affectées à la maille no %(k1)s  appartenant au cable no %(k2)s
 """),

58: _("""
 l'aire de la section droite n a pas été affectée à la maille no %(k1)s  appartenant au cable no %(k2)s
"""),

59: _("""
 valeur invalide pour l'aire de la section droite affectée à la maille numéro %(k1)s  appartenant au cable numéro %(k2)s
"""),

60: _("""
 des aires de section droite différentes ont été affectées aux mailles appartenant au cable no %(k1)s
"""),



62: _("""
  numero d'occurence négatif
"""),

63: _("""
 pas de blocage de déplacement tangent sur des faces d'éléments 3D.
 rentrer la condition aux limites par DDL_IMPO ou LIAISON_DDL
"""),

64: _("""
 il faut choisir entre : FLUX_X ,  FLUX_Y , FLUX_Z et FLUN , FLUN_INF , FLUN_SUP.
"""),

65: _("""
 le descripteur_grandeur des forces ne tient pas sur dix entiers codés
"""),

66: _("""
 trop de valeurs d'angles, on ne garde que les 3 premiers.
"""),


82: _("""
 pour LIAISON_UNIF, entrer plus d'un seul noeud
"""),

83: _("""
 on doit utiliser le mot cle CHAM_NO pour donner le CHAM_NO dont les composantes seront les seconds membres de la relation linéaire.
 
 """),

84: _("""
 il faut que le CHAM_NO dont les termes servent de seconds membres à la relation linéaire à écrire ait été défini.
 """),

85: _(""" 
 on doit donner un CHAM_NO après le mot cle CHAM_NO derrière le mot facteur CHAMNO_IMPO .
"""),

86: _("""
 il faut definir la valeur du coefficient de la relation linéaire après le mot clé COEF_MULT derrière le mot facteur CHAMNO_IMPO
"""),

87: _("""
 le descripteur_grandeur de la grandeur de nom  %(k1)s  ne tient pas sur dix entiers codés
"""),

89: _("""
 Le contenu de la table n'est pas celui attendu !
"""),

90: _("""
 Le calcul par l'opérateur <CALC_FLUI_STRU> des paramètres du mode no %(i1)d
 n'a pas convergé pour la vitesse no %(i2)d. On ne calcule donc pas
 d'interspectres de réponse modale pour cette vitesse.
"""),

91: _("""
La fonction n'a pas été trouvée dans la colonne %(k1)s de la table %(k2)s
(ou bien le paramètre %(k1)s n'existe pas dans la table).
"""),

92: _("""
Les mots-cles admissibles pour définir la premiere liste de noeuds sous le mot-facteur  %(k1)s sont :
"GROUP_NO_1" ou "NOEUD_1" ou "GROUP_MA_1" ou "MAILLE_1".
"""),

93: _("""
Les mots-cles admissibles pour définir la seconde liste de noeuds sous le mot-facteur  %(k1)s  sont :
"GROUP_NO_2" ou "NOEUD_2" ou "GROUP_MA_2" ou "MAILLE_2".
"""),

94: _("""
  LIAISON_GROUP : on ne sait pas calculer la normale à un noeud. Il faut passer par les mailles
"""),

95: _("""
 le groupe  %(k1)s ne fait pas partie du maillage :  %(k2)s
"""),

96: _("""
  %(k1)s   %(k2)s ne fait pas partie du maillage :  %(k3)s
"""),
 
97: _("""
  Assemblage de maillages : Présence de noeuds confondus dans la zone à coller GROUP_MA_1.
"""),
 
98: _("""
 on doit utiliser le mot clé CHAM_NO pour donner le CHAM_NO dont les composantes seront les coefficients de la relation linéaire.
"""),

99: _("""
 il faut que le CHAM_NO dont les termes servent de coefficients à la relation linéaire à écrire ait été défini.
"""),
}
