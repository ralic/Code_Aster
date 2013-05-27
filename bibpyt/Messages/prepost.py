# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
 GROUP_NO :  %(k1)s  inconnu dans le maillage
"""),

2 : _(u"""

Attention :
  Vous avez demandé l'impression d'un CHAM_ELEM de VARI_R :
  on complète le nombre de composantes V1, V2, ... avec des zéros
  pour que toutes les mailles aient le même nombre de composantes.

"""),

4 : _(u"""
 La méthode  %(k1)s  est illicite
"""),

5 : _(u"""
 la longueur du défaut n'est pas en accord avec les tables définies
 coté revêtement et coté métal de base
"""),

8 : _(u"""
 prolongement à gauche exclu
"""),

9 : _(u"""
 prolongement à droite exclu
"""),

10 : _(u"""
 phénomène non valide
"""),

13 : _(u"""
 Les valeurs non existantes du champ %(k1)s lues sur le maillage donné
 sont considérées nulles.
"""),

14 : _(u"""
 Le NUME_DDL a été déterminé à partir de la matrice de rigidité %(k1)s.
"""),

21 : _(u"""
 Intersection Droite / Cercle
 pas d'intersection trouvée
"""),

28 : _(u"""
 volume supérieur à 1.d6
"""),

31 : _(u"""
 structure de données RESULTAT inconnue  %(k1)s
"""),

36 : _(u"""
 Le champ de:  %(k1)s  a des éléments ayant des sous-points.
 Ces éléments ne seront pas traités.
"""),

38 : _(u"""
 le vecteur défini sous le mot clé ACTION/AXE_Z a une norme nulle.
"""),

46 : _(u"""
 erreur dans la création du fichier de maillage au format GIBI.
 Celui-ci ne contient pas d'objet de type maillage.

 Risque & Conseil:
 Assurez vous que votre procédure GIBI sauvegarde bien les objets du maillage (pile 32 dans le formalisme GIBI)
"""),

51 : _(u"""
 l'option de calcul " %(k1)s " n'existe pas dans la structure de données %(k2)s
"""),

52 : _(u"""
 le champ " %(k1)s " pour l'option de calcul " %(k2)s ", n'a pas été notée
 dans la structure de données %(k3)s
"""),

53 : _(u"""
 la dimension du problème est invalide : il faut : 1d, 2d ou 3d.
"""),

54 : _(u"""
 nombre de noeuds supérieur au maximum autorisé : 27.
"""),

55 : _(u"""
 objet &&GILIRE.INDIRECT inexistant
 problème à la lecture des points
"""),

56 : _(u"""
 type de maille :  %(k1)s  inconnu de la commande PRE_GIBI.
"""),

57 : _(u"""
 nombre d'objets supérieur au maximum autorisé : 99999.
"""),

59 : _(u"""
 le maillage GIBI est peut être erroné :
 il est écrit : "NIVEAU RREUR N_ERR"  avec N_ERR est >0 .
 on continue quand même, mais si vous avez des problèmes plus loin ...
"""),

60 : _(u"""
 arrêt sur erreur(s)
"""),

69 : _(u"""
 problème à l'ouverture du fichier
"""),

70 : _(u"""
 problème à la fermeture du fichier
"""),

74 : _(u"""
 la variable  %(k1)s  n'existe pas
"""),

75 : _(u"""
 pas d'impression du champ
"""),

76 : _(u"""
  -> Il y a des groupes de noeuds dans le maillage %(k1)s.
     Ils  n'apparaîtront pas dans le fichier géométrie ENSIGHT.
     Seuls des groupes de mailles peuvent y être intégrés.
"""),

77 : _(u"""
  incompatibilité entre les GREL
"""),

79 : _(u"""
  le nombre de couches est supérieur à 1
"""),

80 : _(u"""
 on traite les TRIA7 QUAD9 en oubliant le noeud centre
"""),

83: _(u"""
 Certaines composantes sélectionnées ne font pas partie du LIGREL
"""),

84 : _(u"""
 Élément PYRAM5 non disponible dans IDEAS
"""),

85 : _(u"""
 Élément PYRAM13 non disponible dans IDEAS
"""),

86 : _(u"""
 On traite les PENTA18 en oubliant le(s) noeud(s)
 au centre et les SEG4 en oubliant les 2 noeuds centraux.
"""),

87 : _(u"""
 On ne sait pas imprimer le champ de type:  %(k1)s
 champ :  %(k2)s
"""),

90 : _(u"""
 On ne sait pas imprimer le champ  %(k1)s  au format  %(k2)s
"""),

97 : _(u"""
 On ne sait pas imprimer les champs de type " %(k1)s "
"""),

98 : _(u"""
 Le champ:  %(k1)s  a des éléments ayant des sous-points.
 Il est écrit avec un format différent.
"""),

99 : _(u"""
 Le champ:  %(k1)s  a des éléments ayant des sous-points.
 Ces éléments ne seront pas écrits.
"""),

}
