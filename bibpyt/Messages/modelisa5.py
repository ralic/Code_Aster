#@ MODIF modelisa5 Messages  DATE 05/07/2010   AUTEUR SELLENET N.SELLENET 
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

cata_msg = {

1 : _("""
 erreur fortran de dimensionnement de tableau (nbmmai>nbmmax)
"""),

2 : _("""
 lecture 1 : il manque les coordonnees !
"""),

3 : _("""
 lecture 1 : il manque les mailles !
"""),

4 : _("""
 transcodage : le noeud  %(k1)s  declare dans la connectivite de la maille  %(k2)s  n existe pas dans les coordonnees
"""),

5 : _("""
 transcodage : le noeud  %(k1)s  declare dans le group_no:  %(k2)s  n'existe pas dans les coordonnees
"""),

6 : _("""
 le noeud :  %(k1)s  est en double dans le group_no:  %(k2)s . on elimine les doublons
"""),

7 : _("""
 transcodage : la maille  %(k1)s  declare dans le group_ma:  %(k2)s  n'existe pas dans les connectivitees
"""),

8 : _("""
 la maille :  %(k1)s  est en double dans le group_ma:  %(k2)s . on elimine les doublons
"""),

9 : _("""
 transcodage : une incoherence a ete detectee entre les declarations de noms de noeuds ou de mailles lors du transcodage des objets groupes et connectivitees
"""),

10 : _("""
 Erreur utilisateur dans CREA_CHAMP / COMB :
   Les champs que l'on cherche à combiner doivent tous etre des champs aux noeuds.
"""),

11 : _("""
 Erreur utilisateur dans CREA_CHAMP / COMB :
   Les champs que l'on cherche à combiner doivent tous avoir la meme grandeur (DEPL_R, ...).
   Ce doit etre la meme que celle donnée dans TYPE_CHAM).
"""),

12 : _("""
 Erreur utilisateur dans CREA_CHAMP / COMB :
   Les champs que l'on cherche à combiner doivent tous avoir la meme numérotation.
"""),

13 : _("""
 Erreur utilisateur dans CREA_CHAMP / COMB :
   Les champs que l'on cherche à combiner doivent tous s'appuyer sur le meme maillage.
"""),












32 : _("""
 il faut fournir des mailles
"""),

33 : _("""
 on attend 1 et 1 seule maille
"""),

34 : _("""
 on n'a pas trouve la maille
"""),

35 : _("""
 que des mailles de type "seg"
"""),

36 : _("""
 un group_ma n'a pas de nom, suppression de ce groupe.
"""),

37 : _("""
 un group_no n'a pas de nom, suppression de ce groupe.
"""),

40 : _("""
 absence de convergence j
"""),

41 : _("""
 absence de convergence i
"""),

42 : _("""
 pas de convergence
"""),

43 : _("""
 erreur programmeur. type de maille inconnu
"""),

44 : _("""
 parametre beta non trouve
"""),

45 : _("""
 parametre lambda non trouve
"""),

47 : _("""
 parametre affinite non trouve
"""),

48 : _("""
  option calcul de l absc_curv sur  un group_ma non implantee.
"""),

49 : _("""
  -> La phase de vérification du maillage a été volontairement désactivée.

  -> Risque & Conseil :
     Soyez sur de votre maillage. Si des mailles dégénérées sont présentes elles
     ne seront pas détectées. Cela pourra nuire à la qualité des résultats.
"""),

50 : _("""
 la grandeur associee au mot cle:  %(k1)s  doit etre:  %(k2)s  mais elle est:  %(k3)s
"""),

51 : _("""
 pour affecter une liste de modelisations, il faut qu'elles soient de meme dimension topologique.
"""),

52 : _("""
 aucune maille n a ete affectee par des elements finis pour le maillage  %(k1)s
"""),

53 : _("""
  -> Le maillage est 3D (tous les noeuds ne sont pas dans le meme plan Z = cste),
     mais les éléments du modèle sont de dimension 2.

  -> Risque & Conseil :
     Si les facettes supportant les éléments ne sont pas dans un plan Z = cste,
     les résultats seront faux.
     Assurez-vous de la cohérence entre les mailles à affecter et la
     modélisation souhaitée dans la commande AFFE_MODELE.
"""),

54 : _("""
 il est interdit d'avoir ,pour un modele donne, a la fois des elements discrets 2d et 3d .
"""),

55 : _("""
 verif : 2 arguments maxi
"""),

56 : _("""
 il manque le mot cle facteurpoutre.
"""),

57 : _("""
 erreur(s) rencontree(s) lors de la verification des affectations.
"""),

58 : _("""
 -> Bizarre :
     Les éléments du modèle sont de dimension 2.
     Mais les noeuds du maillage sont un meme plan Z = a avec a != 0.,

 -> Risque & Conseil :
     Il est d'usage d'utiliser un maillage Z=0. pour les modélisations planes ou Axis.
"""),

59 : _("""
 une erreur d affectation a ete detectee : certaines mailles demandees possedent un type element incompatible avec les donnees a affecter
"""),

60 : _("""
 des poutres ne sont pas affectees
"""),

61 : _("""
 des barres ne sont pas affectees
"""),

62 : _("""
 des cables ne sont pas affectes
"""),

63 : _("""
 le parametre "rho" n'est pas defini pour toutes les couches.
"""),

64 : _("""
 un seul elas svp
"""),

65 : _("""
 <faisceau_trans> deux zones d excitation du fluide ont meme nom
"""),

66 : _("""
 spec_exci_point : si inte_spec alors autant d arguments pour nature, angl et noeud
"""),

67 : _("""
 spec_exci_point : si grappe_2 alors un seul noeud
"""),

68 : _("""
 spec_fonc_forme : le nombre de fonctions fournies doit etre egal a la dimension de la matrice interspectrale
"""),

69 : _("""
 spec_exci_point : le nombre d arguments pour nature, angl et noeud doit etre egal a la dimension de la matrice interspectrale
"""),

70 : _("""
 mauvaise definition de la plage  de frequence.
"""),

71 : _("""
 mauvaise definition de la plage de frequence. les modeles ne tolerent pas des valeurs negatives ou nulles.
"""),

72 : _("""
 le nombre de points pour la discr. freq. doit etre une puissance de 2.
"""),

73 : _("""
 les spectres de type "longueur de correlation"  ne peuvent etre combines avec des spectres d un autre type.
"""),

74 : _("""
 le spectre de nom  %(k1)s  est associe a la zone  %(k2)s  qui n existe pas dans le concept  %(k3)s
"""),

75 : _("""
 le spectre de nom  %(k1)s  est associe a la zone de nom  %(k2)s
"""),

76 : _("""
 deux spectres sont identiques
"""),

77 : _("""
 les spectres de noms  %(k1)s  et  %(k2)s  sont associes au meme profil de vitesse, de nom  %(k3)s
"""),

78 : _("""
 pas le bon numero de mode
"""),

79 : _("""
 le calcul de tous les interspectres de reponse modale n est pas possible car seuls les autospectres d excitation ont ete calcules.
"""),

80 : _("""
 la composante selectionnee pour la restitution en base physique des interspectres est differente de celle choisie pour le couplage fluide-structure.
"""),

81 : _("""
 la tabl_intsp de reponse modale ne contient que des autospectres. le calcul demande n est donc pas realisable.
"""),

82 : _("""
 le champ des contraintes modales doit etre calcule par <calc_elem> option <sipo_elno_depl>.
"""),

83 : _("""
 mot-cle <defi_cable>, occurence no  %(k1)s , operande <noeud_ancrage> : il faut definir 2 noeuds d'ancrage
"""),

84 : _("""
 mot-cle <defi_cable>, occurence no  %(k1)s , operande <group_no_ancrage> : il faut definir 2 group_no d'ancrage
"""),

85 : _("""
 mot-cle <defi_cable>, occurence no  %(k1)s , operande <noeud_ancrage> : les 2 noeuds d'ancrage doivent etre distincts
"""),

86 : _("""
 mot-cle <defi_cable>, occurence no  %(k1)s , operande <group_no_ancrage> : les 2 group_no d'ancrage doivent etre distincts
"""),

87 : _("""
 mot-cle <defi_cable>, occurence no  %(k1)s , operande type ancrage : les 2 extremites sont passives -> armature passive
"""),

88 : _("""
 mot-cle <defi_cable>, occurence no  %(k1)s , operande type ancrage : les 2 extremites sont passives et la tension que vous voulezimposer est non-nulle : impossible !
"""),

89 : _("""
 la carte des caracteristiques materielles des elements n existe pas. il faut prealablement affecter ces caracteristiques en utilisant la commande <affe_materiau>
"""),

90 : _("""
 la carte des caracteristiques geometriques des elements de barre de section generale n existe pas. il faut prealablement affecter ces caracteristiques en utilisant la commande <affe_cara_elem>
"""),

91 : _("""
 probleme pour determiner le rang de la composante <a1> de la grandeur <cagnba>
"""),

92 : _("""
 probleme sur une relation : les coefficients sont trop petits
"""),

94 : _("""
 impossibilite, la maille  %(k1)s  doit etre une maille de peau, i.e. de type "quad" ou "tria" en 3d ou de type "seg" en 2d, et elle est de type :  %(k2)s
"""),

95 : _("""
 vous avez utilise le mot cle orie_peau_2d alors que le probleme est 3d. utilisez orie_peau_3d
"""),

96 : _("""
 vous avez utilise le mot cle orie_peau_3d alors que le probleme est 2d. utilisez orie_peau_2d
"""),

97 : _("""
 erreur donnees : le noeud  %(k1)s  n'existe pas
"""),

98 : _("""
 impossibilite de melanger des "seg" et des "tria" ou "quad" !
"""),

99 : _("""
 Lors de la vérification automatique de l'orientation des mailles de bord, une erreur a été rencontrée : les groupes de mailles de bord ne forment pas un ensemble connexe.
 
 Conseils :
 - Commencez par vérifier que les groupes de mailles de bord fournies sont correctement définis.
 - Si ces groupes de mailles ont des raisons d'être non connexes, vous pouvez désactiver la vérification automatique en renseignant VERI_NORM='NON'.
"""),

}
