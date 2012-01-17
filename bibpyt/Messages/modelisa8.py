#@ MODIF modelisa8 Messages  DATE 16/01/2012   AUTEUR CHEIGNON E.CHEIGNON 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
 la section de la poutre est nulle
"""),

2 : _(u"""
 l'inertie de la poutre suivant OY est nulle
"""),

3 : _(u"""
 l'inertie de la poutre suivant OZ est nulle
"""),

4 : _(u"""
 La somme des aires des fibres est différente de l'aire de la section de la poutre.

 L'erreur relative est supérieure a la précision définie par le mot clé PREC_AIRE :

   - occurrence de multi-fibres : %(r1).0f

   - aire de la poutre       : %(r2)12.5E

   - aire des fibres         : %(r3)12.5E

   - erreur relative         : %(r4)12.5E
"""),

5 : _(u"""
 La somme des moments d'inertie des fibres par rapport a l'axe 0Y est différente du moment de la poutre.

 L'erreur relative est supérieure a la précision définie par le mot clé PREC_INERTIE :

   - occurrence de multi-fibres       : %(r1).0f

   - moment d'inertie de la poutre : %(r2)12.5E

   - aire d'inertie des fibres     : %(r3)12.5E

   - erreur relative               : %(r4)12.5E
"""),

6 : _(u"""
 La somme des moments d'inertie des fibres par rapport a l'axe 0Z est différente du moment de la poutre.

 L'erreur relative est supérieure a la précision définie par le mot clé PREC_INERTIE :

   - occurrence de multi-fibres       : %(r1).0f

   - moment d'inertie de la poutre : %(r2)12.5E

   - aire d'inertie des fibres     : %(r3)12.5E

   - erreur relative               : %(r4)12.5E
"""),

7 : _(u"""
 actuellement on ne peut mettre que %(k1)s groupes de fibres sur un élément
"""),

8 : _(u"""
 Le groupe de fibre %(k1)s n'a pas été défini dans DEFI_GEOM_FIBRE
"""),

9 : _(u"""
 mot clé facteur  "DEFI_ARC", occurrence  %(i1)d , GROUP_MA :  %(k1)s
 le centre n'est pas vraiment  le centre du cercle %(k2)s
"""),

10 : _(u"""
 mot clé facteur  "DEFI_ARC", occurrence  %(i1)d , GROUP_MA :  %(k1)s
 le point de tangence n est pas équidistant des points extrémités %(k2)s
"""),

11 : _(u"""
 mot clé facteur  "DEFI_ARC", occurrence  %(i1)d , maille :  %(k1)s
 le centre n'est pas vraiment  le centre du cercle %(k2)s
"""),

13 : _(u"""
  , maille :  %(i1)d la maille n'est pas située  sur le cercle %(k1)s
"""),

14 : _(u"""
  , maille :  %(i1)d
 la maille n'est pas orientée  dans le même sens que les autres sur le cercle %(k1)s
"""),

16 : _(u"""
  , maille :  %(i1)d problème produit scalaire %(k1)s
"""),

17 : _(u"""
 La première colonne de la table %(k1)s doit contenir des chaînes K8 ou K24.
"""),

18 : _(u"""
 La table %(k1)s ne contient pas de nom de section %(k2)s.
"""),

25 : _(u"""
   l'ensemble des mailles comporte plus de 2 extrémités %(k1)s
"""),

26 : _(u"""
 DEFI_ARC l'ensemble des mailles  forme un cercle : a subdiviser  %(k1)s
"""),

27 : _(u"""
 le ddl  %(k1)s est interdit pour le noeud %(k2)s
"""),

28 : _(u"""
 affectation déjà effectuée du ddl  %(k1)s du noeud %(k2)s : on applique la règle de surcharge
"""),

29 : _(u"""
 nombre de composantes supérieur au max nmaxcmp=  %(i1)d ncmp   =  %(i2)d
"""),

30 : _(u"""
 Erreur utilisateur:
    On cherche à imposer une condition aux limites sur le ddl %(k1)s
    du noeud %(k2)s.
    Mais ce noeud ne porte pas ce ddl.

    Conseils :
     - vérifier le modèle et les conditions aux limites :
        - le noeud incriminé fait-il partie du modèle ?
        - le noeud porte-t-il le ddl que l'on cherche à contraindre ?
"""),

31 : _(u"""
 nombre de mots-clés supérieur au max nmaxocl=  %(i1)d nmocl  =  %(i2)d
"""),

34 : _(u"""
 erreur dans les données le paramètre  %(k1)s n existe pas dans la table  %(k2)s
"""),

35 : _(u"""
 erreur dans les données pas de valeur pour le paramètre  %(k1)s
"""),

36 : _(u"""
 erreur dans les données plusieurs valeurs pour le GROUP_MA  %(k1)s
"""),

40 : _(u"""
 la maille de nom :  %(k1)s n'est pas de type  %(k2)s  ou  %(k3)s
 elle ne sera pas affectée par  %(k4)s
"""),

43 : _(u"""

 le nombre de ddl_1 figurant dans  la liaison n'est pas égal au nombre de COEF_MULT_1 :
   %(i1)d
   %(i2)d
"""),

44 : _(u"""

 le nombre de ddl_2 figurant dans  la liaison n'est pas égal au nombre de COEF_MULT_2 :
   %(i1)d
   %(i2)d
"""),

46 : _(u"""

 le nombre de ddls figurant dans  la liaison n'est pas égal au nombre de  COEF_MULT/COEF_MULT_FONC :
   %(i1)d
   %(i2)d
"""),

47 : _(u"""

 le nombre de ddls figurant dans  la liaison n'est pas égal au nombre de noeuds :
   %(i1)d
   %(i2)d
"""),

49 : _(u"""

 la direction normale est calculée sur la face esclave. il faut donner des mailles
  de facettes, mots clés :  %(k1)s %(k2)s
"""),

52 : _(u"""
 les noeuds n1 et n2 sont confondus COOR(n1): %(r1)f   %(r2)f COOR(n2): %(r3)f
   %(r4)f
 norme   : %(r5)f
"""),

53 : _(u"""
 n3 colinéaires COOR(n1): %(r1)f   %(r2)f   %(r3)f COOR(n2): %(r4)f   %(r5)f
   %(r6)f
 COOR(n3): %(r7)f
   %(r8)f
   %(r9)f
 norme   : %(r10)f
"""),

55 : _(u"""
Interpolation interdite pour un résultat de type :  %(k1)s
Seule l'extraction est possible : OPERATION='EXTR'
"""),

56 : _(u"""
Dans le groupe de mailles %(k1)s, il y a %(i1)d mailles mal orientées. Utilisez la commande MODI_MAILLAGE pour orienter la normale aux surfaces.
"""),

57 : _(u"""
La maille %(k1)s est mal orientée. Utilisez la commande MODI_MAILLAGE pour orienter la normale aux surfaces.
"""),


68 : _(u"""
 Certaines mailles constituant le groupe de mailles %(k1)s ne sont pas
 des mailles surfaciques.
 Risques & Conseils :
 Vérifiez la constitution des groupes de mailles renseignées sous le
 mot-clé GROUP_MA_ESCL.
"""),

69 : _(u"""
 le code:  %(i1)d   %(k1)s
"""),

70 : _(u"""
 Possible erreur utilisateur dans la commande AFFE_MODELE :
   Un problème a été détecté lors de l'affectation des éléments finis.
   Pour l'occurrence AFFE de numéro %(i1)d, certaines mailles de même dimension topologique
   que la (ou les) modélisation(s) (ici dimension = %(i3)d) n'ont pas pu être affectées.

   Cela veut dire que la (ou les) modélisation(s) que l'on cherche à affecter
   ne supporte(nt) pas tous les types de mailles présents dans le maillage.

   Le nombre de mailles que l'on n'a pas pu affecter (pour cette occurrence de AFFE) est :  %(i2)d

 Risques & conseils :
   * Comme certaines mailles n'ont peut-être pas été affectées, il y a un risque
     de résultats faux (présence de "trous" dans la modélisation).
     Pour connaître les mailles non affectées (à la fin de l'opérateur), on peut utiliser INFO=2.
   * Ce problème est fréquent quand on souhaite une modélisation "sous intégrée"
     (par exemple AXIS_SI). Pour l'éviter, il faut donner une modélisation de
     "substitution" pour les mailles qui n'existent pas dans la modélisation désirée (ici 'AXIS_SI').
     On fera par exemple :
        MO=AFFE_MODELE( MAILLAGE=MA,  INFO=2,
                        AFFE=_F(TOUT='OUI', PHENOMENE='MECANIQUE', MODELISATION=('AXIS','AXIS_SI')))

     Ce qui aura le même effet (mais sans provoquer l'alarme) que :
        MO=AFFE_MODELE( MAILLAGE=MA,  INFO=2, AFFE=(
                        _F(TOUT='OUI', PHENOMENE='MECANIQUE', MODELISATION=('AXIS')),
                        _F(TOUT='OUI', PHENOMENE='MECANIQUE', MODELISATION=('AXIS_SI')),
                        ))

"""),

71 : _(u"""
 matériau non valide matériau :  %(k1)s
"""),

72 : _(u"""
 matériaux non valides on ne peut avoir a la fois  %(k1)s  et  %(k2)s
"""),

75 : _(u"""
 erreur données le GROUP_NO n'existe pas  %(k1)s
"""),

77 : _(u"""
 Il y a un conflit dans les vis-à-vis des noeuds. Le noeud  %(k1)s est
 à la fois le vis-à-vis du noeud %(k2)s et du noeud %(k3)s.

 Risques & conseils :
   Vérifiez les groupes en vis-à-vis, il se peut que les maillages soient incompatibles.
   Il faut également s'assurer que la distance entre les deux maillages soit du même
   ordre de grandeur que la longueur caractéristique du maillage (distance entre deux noeuds).
"""),

79 : _(u"""
 conflit dans les vis-à-vis  générés successivement le noeud  %(k1)s
  a pour vis-à-vis le noeud %(k2)s
  et le noeud %(k3)s
"""),

80 : _(u"""
 conflit dans les vis-à-vis  générés successivement
 le noeud de la première liste %(k1)s
  n"est l"image d"aucun  %(k2)s
 noeud par la correspondance inverse %(k3)s
"""),

87 : _(u"""
 conflit dans les vis-à-vis  générés successivement a partir des listes  %(k1)s
 et  %(k2)s
 le noeud  %(k3)s
 a pour vis-à-vis le noeud %(k4)s
  et le noeud %(k5)s
"""),

88 : _(u"""
 conflit dans les vis-à-vis  générés successivement a partir des listes  %(k1)s
 et  %(k2)s
 le noeud de la première liste %(k3)s
  n"est l"image d"aucun  %(k4)s
 noeud par la correspondance inverse %(k5)s
"""),

89 : _(u"""
 on ne trouve pas dans la paroi 2 de maille de type :  %(i1)d
"""),

90 : _(u"""
 conflit dans les VIS_A_VIS les mailles  %(k1)s  et  %(k2)s
  ont toutes les 2 comme VIS_A_VIS la maille %(k3)s
"""),

93 : _(u"""

 évaluation impossible  d une fonction matériau - on déborde a gauche  pour la température
 TEMP : %(r1)f
"""),

94 : _(u"""

 évaluation impossible  d une fonction matériau - on déborde a droite  pour la température
 TEMP : %(r1)f
"""),

98 : _(u"""
 il manque le paramètre  %(k1)s dans la table %(k2)s
 .sa présence est indispensable a la  création du champ nodal. %(k3)s
"""),

99 : _(u"""
 le paramètre  %(k1)s ne doit pas apparaître dans la  table %(k2)s
 pour la création d'un champ nodal. %(k3)s
"""),

}
