#@ MODIF algorith14 Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

3 : _(u"""
 type d'interface non supportée en cyclique
 type interface -->  %(k1)s
"""),

4 : _(u"""
 arrêt sur type de résultat non supporté
 type donné      -->  %(k1)s
 types supportés -->  %(k2)s %(k3)s
"""),

8 : _(u"""
 il manque la déformée modale pour le mode  %(i1)d
"""),

10 : _(u"""
 la maille %(k2)s n'existe pas dans le maillage %(k1)s
"""),

11 : _(u"""
 le noeud %(k2)s n'existe pas dans le maillage %(k1)s
"""),

13 : _(u"""
 arrêt sur base modale de type illicite
 base modale  -->  %(k1)s
 type         -->  %(k2)s
 type attendu -->  %(k3)s
"""),

14 : _(u"""
 arrêt sur matrice de raideur non unique
"""),

15 : _(u"""
 arrêt sur matrice de masse non unique
"""),

16 : _(u"""
 arrêt sur matrice d'amortissement non unique en argument
"""),

17 : _(u"""
 Le type de matrice %(k1)s est inconnu. Erreur développeur
"""),

21 : _(u"""
 les matrices assemblées n'ont pas la même numérotation
 masse   = %(k1)s
 raideur = %(k2)s
"""),

22 : _(u"""
 les matrices assemblées n'ont pas la même numérotation
 amortissement = %(k1)s
 raideur       = %(k2)s
"""),

23 : _(u"""

 les matrices assemblées et la base modale n'ont pas le même maillage initial
 maillage matrice     : %(k1)s
 maillage base modale : %(k2)s
"""),

24 : _(u"""
 arrêt sur problème de cohérence
 MODE_MECA donné       -->  %(k1)s
 numérotation associée -->  %(k2)s
 INTERF_DYNA donnée    -->  %(k3)s
 numérotation associée -->  %(k4)s
"""),

25 : _(u"""
 sous-structure inexistante dans le modèle généralisé
 modèle généralisé : %(k1)s
 sous-structure    : %(k2)s
"""),

26 : _(u"""
 problème de cohérence du nombre de champs de la base modale
 base modale                  : %(k1)s
 nombre de champs de la base  : %(i1)d
 nombre de degrés généralisés : %(i2)d
"""),

27 : _(u"""
 le maillage %(k1)s n'est pas un maillage SQUELETTE
"""),

28 : _(u"""
  aucun type d'interface défini pour la sous-structure :  %(i1)d
  pas de mode rigide d'interface
  le calcul de masses effectives risque d'être imprécis %(i2)d
"""),

30 : _(u"""
 incohérence détectée dans le squelette
 objet non trouvé :  %(k1)s
"""),

31 : _(u"""
 problème de cohérence entre le nombre de concepts MODE_MECA et
 la liste des NMAX_MODE:
 nombre de concepts MODE_MECA dans la liste MODE_MECA     : %(i1)d
 nombre de valeurs de la liste NMAX_MODE                  : %(i2)d
 Conseils & solution:
  Les deux listes doivent avoir la même taille.
"""),

32 : _(u"""
 structure de données résultat  %(k1)s, le champ %(k2)s n'existe pas
 pour le NUME_ORDRE  %(i1)d
"""),

33 : _(u"""
 structure de données résultat  %(k1)s, le champ %(k2)s n'a pas été duplique
 pour le NUME_ORDRE  %(i1)d
"""),

35 : _(u"""
 aucun champ n'est calculé dans la structure de données  %(k1)s
"""),

36 : _(u"""
 les numérotations des champs ne coïncident pas celui de  %(k1)s  est :  %(k2)s
 et celui de  %(k3)s
  est :  %(k4)s
"""),

50 : _(u"""
 il faut au moins 1 mode !
"""),

51 : _(u"""
 il faut au moins un MODE_MECA a la 1ère occurrence de RITZ
"""),

55 : _(u"""
 le champ de "TEMP" n'existe pas pour le numéro d'ordre  %(i1)d
"""),

59 : _(u"""
 le champ de "META_ELNO"  n'existe pas pour le numéro d'ordre  %(i1)d
"""),

61 : _(u"""
 le pas de temps du calcul métallurgique ne correspond pas
 au pas de temps du calcul thermique
 - numéro d'ordre              : %(i1)d
 - pas de temps thermique      : %(r1)f
 - pas de temps métallurgique  : %(r2)f
"""),

62 : _(u"""
 il manque la déformée modale NOM_CHAM  %(k1)s  pour le mode  %(i1)d
"""),

63 : _(u"""
 données incompatibles :
 pour le MODE_STAT  :  %(k1)s
 il manque le champ :  %(k2)s
"""),

64 : _(u"""
 il manque le mode statique NOM_CHAM  %(k1)s  pour le mode  %(i1)d
"""),

66 : _(u"""
 Taille de bloc insuffisante
 taille de bloc demandée (kr8): %(r1)f
 taille de bloc utilisée (kr8): %(r2)f
"""),

68 : _(u"""
  Estimation de la durée du régime transitoire :
  valeur minimale conseillée :  %(r1)f
"""),

69 : _(u"""
 non-linéarité incompatible avec la définition du modèle généralisé
 NOEUD_1      :  %(k1)s
 SOUS_STRUC_1 :  %(k2)s
 NOEUD_2      :  %(k3)s
 SOUS_STRUC_2 :  %(k4)s
"""),

70 : _(u"""
 problème de cohérence du nombre de noeuds d'interface
 sous-structure1            : %(k1)s
 interface1                 : %(k2)s
 nombre de noeuds interface1: %(i1)d
 sous-structure2            : %(k3)s
 interface2                 : %(k4)s
 nombre de noeuds interface2: %(i2)d
"""),

71 : _(u"""
 problème de cohérence des interfaces orientées
 sous-structure1           : %(k1)s
 interface1                : %(k2)s
 présence composante sur 1 : %(k3)s
 sous-structure2           : %(k4)s
 interface2                : %(k5)s
 composante inexistante sur 2 %(k6)s
"""),

72 : _(u"""
 problème de cohérence des interfaces orientées
 sous-structure2           : %(k1)s
 interface2                : %(k2)s
 présence composante sur 2 : %(k3)s
 sous-structure1           : %(k4)s
 interface1                : %(k5)s
 composante inexistante sur 1 %(k6)s
"""),

73 : _(u"""
 Sous-structures incompatibles
 sous-structure 1             : %(k1)s
 MACR_ELEM associé            : %(k2)s
 numéro grandeur sous-jacente : %(i1)d
 sous-structure 2             : %(k3)s
 MACR_ELEM associé            : %(k4)s
 numéro grandeur sous-jacente : %(i2)d
"""),

74 : _(u"""
 arrêt sur incompatibilité de sous-structures
"""),

75 : _(u"""
  Erreur développement : code retour 1 dans NMCOMP en calculant la matrice tangente
 """),

77 : _(u"""
 les types des deux matrices sont différents
 type de la matrice de raideur :  %(k1)s
 type de la matrice de masse   :  %(k2)s
"""),

78 : _(u"""
 les numérotations des deux matrices sont différentes
 numérotation matrice de raideur :  %(k1)s
 numérotation matrice de masse   :  %(k2)s
"""),

79 : _(u"""
 coefficient de conditionnement des multiplicateurs de Lagrange :  %(r1)f
"""),

80 : _(u"""
 affichage des coefficients d'amortissement :
 premier coefficient d'amortissement : %(r1)f
 second  coefficient d'amortissement : %(r2)f
"""),

82 : _(u"""
 calcul du nombre de diamètres modaux demandé impossible
 nombre de diamètres demandé --> %(i1)d
"""),

83 : _(u"""
 calcul des modes propres limité au nombre de diamètres maximum --> %(i1)d
"""),

84 : _(u"""
 calcul cyclique :
 aucun nombre de diamètres nodaux licite
"""),

85 : _(u"""
 liste de fréquences incompatible avec l'option
 nombre de fréquences --> %(i1)d
 option               --> %(k1)s
"""),

87 : _(u"""
  résolution du problème généralisé complexe
  nombre de modes dynamiques:  %(i1)d
  nombre de ddl droite      :  %(i2)d
"""),

88 : _(u"""
  nombre de ddl axe         :  %(i1)d
         dont cycliques     :  %(i2)d
         dont non cycliques :  %(i3)d
"""),

89 : _(u"""
  dimension max du problème :  %(i1)d
"""),

91 : _(u"""
 noeud sur l'axe_z
 noeud :  %(k1)s
"""),

93 : _(u"""
 arrêt sur dimension matrice TETA incorrecte
 dimension effective   :  %(i1)d
 dimension en argument :  %(i2)d
"""),

94: _(u"""
 erreur de répétitivité cyclique
"""),

95: _(u"""
 il manque un ddl sur un noeud  axe type du ddl -->  %(k1)s
 nom du noeud -->  %(k2)s
"""),

99 : _(u"""
 arrêt sur nombres de noeuds interface non identiques
 nombre de noeuds interface droite:  %(i1)d
 nombre de noeuds interface gauche:  %(i2)d
"""),

}
