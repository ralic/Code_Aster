#@ MODIF prepost2 Messages  DATE 07/11/2011   AUTEUR COURTOIS M.COURTOIS 
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

1 : _(u"""
  Il faut autant de nom pour NOM_CHAM_MED que pour NOM_CHAM.
"""),

2 : _(u"""
Modèle inconnu, pas d'impression du champ  %(k1)s
"""),

3 : _(u"""
On ne sait pas écrire des champs par élément aux points de gauss au format CASTEM
"""),

4 : _(u"""
 erreur programmation
"""),

35 : _(u"""
   désolé on ne sait pas écrire les champs aux noeuds de représentation constante et a valeurs complexes au format  %(k1)s
"""),

36 : _(u"""
   désole on ne sait pas écrire le champ aux noeuds  %(k1)s  au format  %(k2)s
"""),

40 : _(u"""
 aucune des composantes demandées sous le mot-clé NOM_CMP pour l'impression du CHAM_GD  %(k1)s  n'est présente dans la grandeur  %(k2)s
"""),

41 : _(u"""
 aucune des composantes demandées sous le mot-clé NOM_CMP pour l'impression du champ  %(k1)s  du concept  %(k2)s  n'est présente dans la grandeur  %(k3)s
"""),

46 : _(u"""
  numéro d'ordre  %(k1)s  non licite
"""),

51 : _(u"""
 type de structure non traite:  %(k1)s
"""),

52 : _(u"""
 on imprime que des champs ELNO
"""),

53 : _(u"""
 nombre de composantes différent
"""),

54 : _(u"""
 composante inconnue %(k1)s
"""),

55 : _(u"""
 L'ordre des composantes établi lorsque que vous avez renseigné le mot-clé
 NOM_CMP est différent de celui du catalogue Aster:
    - ordre des composantes fournies     : %(k1)s %(k2)s %(k3)s %(k4)s %(k5)s %(k6)s
    - ordre des composantes du catalogue : %(k7)s %(k8)s %(k9)s %(k10)s %(k11)s %(k12)s
"""),

56 : _(u"""
 on imprime que des champs "ELGA" ou "ELEM"
"""),

57 : _(u"""
 nombre de sous-points différent de 1
"""),

58 : _(u"""
 pas de correspondance
"""),

59 : _(u"""
 aucun champ trouve, pas d'impression au format "GMSH"
"""),

60 : _(u"""
 On ne sait pas imprimer au format "GMSH" le champ %(k1)s
 car il est de type %(k2)s.
"""),

61 : _(u"""
 erreur de programmation : nombre de composantes différent de 1 ou 3.
"""),

62 : _(u"""
 on ne peut pas traiter des éléments a plus de 99 noeuds !
"""),

63 : _(u"""
 erreur de programmation
"""),

64 : _(u"""
 SEG4 élément inexistant dans CASTEM, converti en SEG2
"""),

65 : _(u"""
 tria7 élément inexistant dans CASTEM, converti en tria6
"""),

66 : _(u"""
 QUAD9 élément inexistant dans CASTEM, converti en QUAD8
"""),

67 : _(u"""
 les champs n'ont pas la même grandeur
"""),

68 : _(u"""
 les champs n'ont pas la même numérotation
"""),

69 : _(u"""
 les champs n'ont pas le même type de valeurs
"""),

70 : _(u"""
   désolé on ne sait pas écrire les champs aux noeuds de représentation constante au format IDEAS
"""),

71 : _(u"""
 élément noeud non disponible dans ENSIGHT
"""),

72 : _(u"""
 élément  %(k1)s  non disponible dans ENSIGHT
"""),

73 : _(u"""
 les éléments tria7 seront réduits a des tria6
"""),

74 : _(u"""
 les éléments QUAD9 seront réduits a des QUAD8
"""),

75 : _(u"""
 les éléments PENTA15 seront réduits a des PENTA6
"""),

76 : _(u"""
 il y a des groupes de noeuds dans le maillage  %(k1)s  qui n'apparaîtront pas dans le fichier géométrie ENSIGHT: seuls des groupes de mailles peuvent y être intégrés
"""),

77 : _(u"""
 la dimension du problème est invalide : il faut : 1d, 2d, 3d.
"""),

78 : _(u"""
 HEXA27 élément inexistant dans IDEAS, converti en HEXA20
"""),

79 : _(u"""
 tria7 élément inexistant dans IDEAS, converti en tria6
"""),

80 : _(u"""
 QUAD9 élément inexistant dans IDEAS, converti en QUAD8
"""),

81 : _(u"""
 SEG4 élément inexistant dans IDEAS, converti en SEG2
"""),

82 : _(u"""
 élément PYRAM5 non disponible dans IDEAS
"""),

83 : _(u"""
 élément PYRAM13 non disponible dans IDEAS
"""),

84 : _(u"""
 Le champ %(k1)s est un champ aux noeuds par éléments
 contenant des %(k2)s
 Or l'impression de ce type de champ n'est pas encore possible au format MED
 On n'imprimera donc pas ce champ dans le fichier MED
"""),

85 : _(u"""
 L'élément PENTA18 est inexistant dans IDEAS, il est converti en PENTA15.
"""),

86 : _(u"""
 L'élément PENTA18 est inexistant dans CASTEM, il est converti en PENTA15.
"""),

93 : _(u"""
 on ne sait pas écrire les mailles de type  %(k1)s
"""),

}
