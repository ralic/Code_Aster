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
 erreur fortran de dimensionnement de tableau (nbmmai>nbmmax)
"""),

2 : _(u"""
 lecture 1 : il manque les coordonnées !
"""),

3 : _(u"""
 lecture 1 : il manque les mailles !
"""),

4 : _(u"""
 transcodage : le noeud  %(k1)s  déclaré dans la connectivité de la maille  %(k2)s  n existe pas dans les coordonnées
"""),

5 : _(u"""
 transcodage : le noeud  %(k1)s  déclare dans le GROUP_NO:  %(k2)s  n'existe pas dans les coordonnées
"""),

6 : _(u"""
 le noeud :  %(k1)s  est en double dans le GROUP_NO:  %(k2)s . on élimine les doublons
"""),

7 : _(u"""
 transcodage : la maille  %(k1)s  déclaré dans le GROUP_MA:  %(k2)s  n'existe pas dans les connectivités
"""),

8 : _(u"""
 la maille :  %(k1)s  est en double dans le GROUP_MA:  %(k2)s . on élimine les doublons
"""),

9 : _(u"""
 transcodage : une incohérence a été détectée entre les déclarations de noms de noeuds ou de mailles lors du transcodage des objets groupes et connectivités
"""),

10 : _(u"""
 Erreur utilisateur dans CREA_CHAMP / COMB :
   Les champs que l'on cherche à combiner doivent tous être des champs aux noeuds.
"""),

11 : _(u"""
 Erreur utilisateur dans CREA_CHAMP / COMB :
   Les champs que l'on cherche à combiner doivent tous avoir la même grandeur (DEPL_R, ...).
   Ce doit être la même que celle donnée dans TYPE_CHAM).
"""),

12 : _(u"""
 Erreur utilisateur dans CREA_CHAMP / COMB :
   Les champs que l'on cherche à combiner doivent tous avoir la même numérotation.
"""),

13 : _(u"""
 Erreur utilisateur dans CREA_CHAMP / COMB :
   Les champs que l'on cherche à combiner doivent tous s'appuyer sur le même maillage.
"""),

32 : _(u"""
 il faut fournir des mailles
"""),

33 : _(u"""
 on attend 1 et 1 seule maille
"""),

34 : _(u"""
 on n'a pas trouve la maille
"""),

35 : _(u"""
 que des mailles de type "SEG"
"""),

36 : _(u"""
 un GROUP_MA n'a pas de nom, suppression de ce groupe.
"""),

37 : _(u"""
 un GROUP_NO n'a pas de nom, suppression de ce groupe.
"""),

40 : _(u"""
 absence de convergence j
"""),

41 : _(u"""
 absence de convergence i
"""),

42 : _(u"""
 pas de convergence
"""),

43 : _(u"""
 erreur programmeur. type de maille inconnu
"""),

44 : _(u"""
 paramètre bêta non trouve
"""),

45 : _(u"""
 paramètre lambda non trouve
"""),

47 : _(u"""
 paramètre AFFINITE non trouve
"""),

48 : _(u"""
  option calcul de l ABSC_CURV sur  un GROUP_MA non implantée.
"""),

49 : _(u"""
  -> La phase de vérification du maillage a été volontairement désactivée.

  -> Risque & Conseil :
     Soyez sur de votre maillage. Si des mailles dégénérées sont présentes elles
     ne seront pas détectées. Cela pourra nuire à la qualité des résultats.
"""),

50 : _(u"""
 la grandeur associée au mot clé :  %(k1)s  doit être:  %(k2)s  mais elle est:  %(k3)s
"""),

51 : _(u"""
 pour affecter une liste de modélisations, il faut qu'elles soient de même dimension topologique.
"""),

52 : _(u"""
 aucune maille n a êtes affectée par des éléments finis pour le maillage  %(k1)s
"""),

53 : _(u"""
  -> Le maillage est 3D (tous les noeuds ne sont pas dans le même plan Z = constante),
     mais les éléments du modèle sont de dimension 2.

  -> Risque & Conseil :
     Si les facettes supportant les éléments ne sont pas dans un plan Z = constante,
     les résultats seront faux.
     Assurez-vous de la cohérence entre les mailles à affecter et la
     modélisation souhaitée dans la commande AFFE_MODELE.
"""),

54 : _(u"""
 il est interdit d'avoir ,pour un modèle donne, a la fois des éléments discrets 2d et 3d .
"""),

55 : _(u"""
 VERIF : 2 arguments max
"""),

56 : _(u"""
 il manque le mot clé facteur POUTRE.
"""),

57 : _(u"""
 erreur(s) rencontrée(s) lors de la vérification des affectations.
"""),

58 : _(u"""
 -> Bizarre :
     Les éléments du modèle sont de dimension 2.
     Mais les noeuds du maillage sont un même plan Z = a avec a != 0.,

 -> Risque & Conseil :
     Il est d'usage d'utiliser un maillage Z=0. pour les modélisations planes ou Axis.
"""),

59 : _(u"""
 une erreur d affectation a été détectée : certaines mailles demandées possèdent un type élément incompatible avec les données a affecter
"""),

60 : _(u"""
 des poutres ne sont pas affectées
"""),

61 : _(u"""
 des barres ne sont pas affectées
"""),

62 : _(u"""
 des câbles ne sont pas affectes
"""),

63 : _(u"""
 le paramètre "RHO" n'est pas défini pour toutes les couches.
"""),

64 : _(u"""
 un seul ELAS svp
"""),

65 : _(u"""
 <FAISCEAU_TRANS> deux zones d excitation du fluide ont même nom
"""),

66 : _(u"""
 SPEC_EXCI_POINT : si INTE_SPEC alors autant d arguments pour nature, ANGLE et noeud
"""),

67 : _(u"""
 SPEC_EXCI_POINT : si grappe_2 alors un seul noeud
"""),

68 : _(u"""
 SPEC_FONC_FORME : le nombre de fonctions fournies doit être égal a la dimension de la matrice interspectrale
"""),

69 : _(u"""
 SPEC_EXCI_POINT : le nombre d arguments pour nature, ANGLE et noeud doit être égal a la dimension de la matrice interspectrale
"""),

70 : _(u"""
 mauvaise définition de la plage  de fréquence.
"""),

71 : _(u"""
 mauvaise définition de la plage de fréquence. les modèles ne tolèrent pas des valeurs négatives ou nulles.
"""),

72 : _(u"""
 le nombre de points pour la discrétisation fréquentielle doit être une puissance de 2.
"""),

73 : _(u"""
 les spectres de type "longueur de corrélation"  ne peuvent être combines avec des spectres d un autre type.
"""),

74 : _(u"""
 le spectre de nom  %(k1)s  est associe a la zone  %(k2)s  qui n existe pas dans le concept  %(k3)s
"""),

75 : _(u"""
 le spectre de nom  %(k1)s  est associe a la zone de nom  %(k2)s
"""),

76 : _(u"""
 deux spectres sont identiques
"""),

77 : _(u"""
 les spectres de noms  %(k1)s  et  %(k2)s  sont associes au même profil de vitesse, de nom  %(k3)s
"""),

78 : _(u"""
 pas le bon numéro de mode
"""),

79 : _(u"""
 le calcul de tous les interspectres de réponse modale n est pas possible car seuls les auto spectres d excitation ont été calcules.
"""),

80 : _(u"""
 la composante sélectionnée pour la restitution en base physique des interspectres est différente de celle choisie pour le couplage fluide-structure.
"""),

81 : _(u"""
 la TABL_INTSP de réponse modale ne contient que des auto spectres. le calcul demande n est donc pas réalisable.
"""),





83 : _(u"""
 mot-clé <DEFI_CABLE>, occurrence no  %(k1)s , opérande <NOEUD_ANCRAGE> : il faut définir 2 noeuds d'ancrage
"""),

84 : _(u"""
 mot-clé <DEFI_CABLE>, occurrence no  %(k1)s , opérande <GROUP_NO_ANCRAGE> : il faut définir 2 GROUP_NO d'ancrage
"""),

85 : _(u"""
 mot-clé <DEFI_CABLE>, occurrence no  %(k1)s , opérande <NOEUD_ANCRAGE> : les 2 noeuds d'ancrage doivent être distincts
"""),

86 : _(u"""
 mot-clé <DEFI_CABLE>, occurrence no  %(k1)s , opérande <GROUP_NO_ANCRAGE> : les 2 GROUP_NO d'ancrage doivent être distincts
"""),

87 : _(u"""
 mot-clé <DEFI_CABLE>, occurrence no  %(k1)s , opérande type ancrage : les 2 extrémités sont passives -> armature passive
"""),

88 : _(u"""
 mot-clé <DEFI_CABLE>, occurrence no  %(k1)s , opérande type ancrage : les 2 extrémités sont passives et la tension que vous voulez imposer est non nulle : impossible !
"""),

89 : _(u"""
 la carte des caractéristiques matérielles des éléments n existe pas. il faut préalablement affecter ces caractéristiques en utilisant la commande <AFFE_MATERIAU>
"""),

90 : _(u"""
 la carte des caractéristiques géométriques des éléments de barre de section générale n existe pas. il faut préalablement affecter ces caractéristiques en utilisant la commande <AFFE_CARA_ELEM>
"""),

91 : _(u"""
 problème pour déterminer le rang de la composante <a1> de la grandeur <cagnba>
"""),

92 : _(u"""
 problème sur une relation : les coefficients sont trop petits
"""),

94 : _(u"""
 impossibilité, la maille  %(k1)s  doit être une maille de peau, i.e. de type "QUAD" ou "tria" en 3d ou de type "SEG" en 2d, et elle est de type :  %(k2)s
"""),

95 : _(u"""
 vous avez utilise le mot clé ORIE_PEAU_2d alors que le problème est 3d. utilisez ORIE_PEAU_3d
"""),

96 : _(u"""
 vous avez utilise le mot clé ORIE_PEAU_3d alors que le problème est 2d. utilisez ORIE_PEAU_2d
"""),

97 : _(u"""
 erreur données : le noeud  %(k1)s  n'existe pas
"""),

98 : _(u"""
 impossibilité de mélanger des "SEG" et des "tria" ou "QUAD" !
"""),

99 : _(u"""
 Lors de la vérification automatique de l'orientation des mailles de bord, une erreur a été rencontrée : les groupes de mailles de bord ne forment pas un ensemble connexe.

 Conseils :
 - Commencez par vérifier que les groupes de mailles de bord fournies sont correctement définis.
 - Si ces groupes de mailles ont des raisons d'être non connexes, vous pouvez désactiver la vérification automatique en renseignant VERI_NORM='NON'.
"""),

}
