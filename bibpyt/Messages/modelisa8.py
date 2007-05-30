#@ MODIF modelisa8 Messages  DATE 29/05/2007   AUTEUR PELLET J.PELLET 
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

def _(x) : return x

cata_msg={

1: _("""
 la section de la poutre est nulle
"""),
2: _("""
 l'inertie de la poutre suivant OY est nulle
"""),
3: _("""
 l'inertie de la poutre suivant OZ est nulle
"""),
4: _("""
 La somme des aires des fibres est differente de l'aire de la section de la poutre.\n
 L'erreur relative est superieure a la precision definie par le mot cle PREC_AIRE :\n
   - occurence de multifire : %(r1)d \n
   - aire de la poutre      : %(r2)12.5E \n
   - aire des fibres        : %(r3)12.5E \n
   - erreur relative        : %(r4)12.5E
"""),
5: _("""
 La somme des moments d'inertie des fibres par rapport a l'axe 0Y est differente du moment de la poutre.\n
 L'erreur relative est superieure a la precision definie par le mot cle PREC_INERTIE :\n
   - occurence de multifire : %(r1)d \n
   - moment d'inertie de la poutre : %(r2)12.5E \n
   - aire d'inertie des fibres     : %(r3)12.5E \n
   - erreur relative               : %(r4)12.5E
"""),
6: _("""
 La somme des moments d'inertie des fibres par rapport a l'axe 0Z est differente du moment de la poutre.\n
 L'erreur relative est superieure a la precision definie par le mot cle PREC_INERTIE :\n
   - occurence de multifire : %(r1)d \n
   - moment d'inertie de la poutre : %(r2)12.5E \n
   - aire d'inertie des fibres     : %(r3)12.5E \n
   - erreur relative               : %(r4)12.5E
"""),

7: _("""
 actuellemnt on ne peut mettre que %(k1)s groupes de fibres sur un element
"""),

8: _("""
 Le groupe de fibre %(k1)s n'a pas ete defini dans DEFI_GEOM_FIBRE
"""),

9: _("""
 mot cle facteur  "defi_arc", occurence  %(i1)d , group_ma :  %(k1)s
 le centre n'est pas vraiment  le centre du cercle %(k2)s
"""),

10: _("""
 mot cle facteur  "defi_arc", occurence  %(i1)d , group_ma :  %(k1)s
 le point de tangence n est pas equidistant des points extremites %(k2)s
"""),

11: _("""
 mot cle facteur  "defi_arc", occurence  %(i1)d , maille :  %(k1)s
 le centre n'est pas vraiment  le centre du cercle %(k2)s
"""),






13: _("""
  , maille :  %(i1)d la maille n'est pas situee  sur le cercle %(k1)s
"""),

14: _("""
  , maille :  %(i1)d
 la maille n'est pas orientee  dans le meme sens que les autres sur le cercle %(k1)s
"""),






16: _("""
  , maille :  %(i1)d pb produit scalaire %(k1)s
"""),









































25: _("""
   l'ensemble des mailles comporte plus de 2 extremites %(k1)s
"""),

26: _("""
 defi_arcl'ensemble des mailles  forme un cercle : a subdiviser  %(k1)s
"""),

27: _("""
 le ddl  %(k1)s est interdit pour le noeud %(k2)s
"""),

28: _("""
 affectation deja effectuee du ddl  %(k1)s du noeud %(k2)s   %(k3)s
"""),

29: _("""
 nombre de cmps superieur au max nmaxcmp=  %(i1)d ncmp   =  %(i2)d
"""),

30: _("""
 Erreur utilisateur:
    On cherche à imposer une condition aux limites sur le ddl %(k1)s
    du noeud %(k2)s.
    Mais ce noeud ne porte pas ce ddl.

    Conseils :
     - vérifier le modèle et les conditions aux limites :
        - le noeud incriminé fait-il partie du modèle ?
        - le noeud porte-t-il le ddl que l'on cherche à contraindre ?
"""),


31: _("""
 nombre de motcles superieur au max nmaxocl=  %(i1)d nmocl  =  %(i2)d
"""),














34: _("""
 erreur dans les donneesle parametre  %(k1)s n existe pas dans la table  %(k2)s
"""),

35: _("""
 erreur dans les donneespas de valeur pour le parametre  %(k1)s
"""),

36: _("""
 erreur dans les donneesplusieurs valeurs pour le group_ma  %(k1)s
"""),
















40: _("""
 la maille de nom :  %(k1)s n'est pas de type  %(k2)s  ou  %(k3)s
 elle ne sera pas affectee par  %(k4)s
"""),

41: _("""
 mgrappmasse de la tige calculee:      %(r1)f
 masse de l'araignee calculee:  %(r2)f
 masse du crayon calculee:       %(r3)f
 masse grappe=tige+araignee+crayon:  %(r4)f
"""),

42: _("""
 erreurs dans les donneeslongueur modelisee:  %(r1)f longueur donnee   :  %(r2)f
"""),

43: _("""

 le nombre de ddl_1 figurant dans  la liaison n'est pas egal au nombre de coef_mult_1 :
   %(i1)d
   %(i2)d
"""),

44: _("""

 le nombre de ddl_2 figurant dans  la liaison n'est pas egal au nombre de coef_mult_2 :
   %(i1)d
   %(i2)d
"""),








46: _("""

 le nombre de ddls figurant dans  la liaison n'est pas egal au nombre de  coef_mult/coef_mult_fonc :
   %(i1)d
   %(i2)d
"""),

47: _("""

 le nombre de ddls figurant dans  la liaison n'est pas egal au nombre de noeuds :
   %(i1)d
   %(i2)d
"""),








49: _("""

 la direction normale est calculee sur la face esclave. il faut donner des mailles
  de facettes, mots cles :  %(k1)s %(k2)s
"""),















52: _("""
 les noeuds n1 et n2 sont confondus coor(n1): %(r1)f   %(r2)f coor(n2): %(r3)f
   %(r4)f
 norme   : %(r5)f
"""),

53: _("""
 n3 colineaires coor(n1): %(r1)f   %(r2)f   %(r3)f coor(n2): %(r4)f   %(r5)f
   %(r6)f
 coor(n3): %(r7)f
   %(r8)f
   %(r9)f
 norme   : %(r10)f
"""),








55: _("""
 interpolation interditepour un resultat de type :  %(k1)s
"""),

56: _("""
 group_ma  %(k1)s mailles mal orientees  %(i1)d
"""),

57: _("""
 maille mal orientee:  %(k1)s
"""),

58: _("""
  composante existante sur  le noeud:  %(k1)s  composante:  %(k2)s
"""),
















































66: _("""
 le nom de groupe numero  %(i1)d  est en double. %(k1)s
 premier nom med  :  %(k2)s
 second nom med   :  %(k3)s
 nom aster retenu :  %(k4)s
"""),

67: _("""

 le numero de la maille de type  nomtyp(ityp)  est superieur au nombre total de  mailles :
 numero de la maille :  %(i1)d
 nombre de mailles   :  %(i2)d
"""),

68: _("""
  la maille  de num  %(i1)d  de nom  %(k1)s
"""),

69: _("""
 le code:  %(i1)d   %(k1)s
"""),

70: _("""
 pour l'occurence de affe   numero  %(i1)d
  on n'a pas pu affecter  %(i2)d
  mailles de dimension  %(i3)d
  (info=2 pour details)  %(i4)d
"""),

71: _("""
 materiau non valide materiau :  %(k1)s
"""),

72: _("""
 materiaux non valideson ne peut avoir a la fois  %(k1)s  et  %(k2)s
"""),















75: _("""
 erreur donneesle group_no n'existe pas  %(k1)s
"""),








77: _("""
 conflit dans les vis_a_vis  des noeuds le noeud  %(k1)s
  est le vis-a-vis des noeuds  %(k2)s
  et  %(k3)s
"""),








79: _("""
 conflit dans les vis-a-vis  generes successivement le noeud  %(k1)s
  a pour vis-a-vis le noeud %(k2)s
  et le noeud %(k3)s
"""),

80: _("""
 conflit dans les vis-a-vis  generes successivement
 le noeud de la premiere liste %(k1)s
  n"est l"image d"aucun  %(k2)s
 noeud par la correspondance inverse %(k3)s
"""),











































87: _("""
 conflit dans les vis-a-vis  generes successivement a partir des listes  %(k1)s
 et  %(k2)s
 le noeud  %(k3)s
 a pour vis-a-vis le noeud %(k4)s
  et le noeud %(k5)s
"""),

88: _("""
 conflit dans les vis-a-vis  generes successivement a partir des listes  %(k1)s
 et  %(k2)s
 le noeud de la premiere liste %(k3)s
  n"est l"image d"aucun  %(k4)s
 noeud par la correspondance inverse %(k5)s
"""),

89: _("""
 on ne trouve pas dans la paroi 2 de maille de type :  %(i1)d
"""),

90: _("""
 conflit dans les vis_a_vis les mailles  %(k1)s  et  %(k2)s
  ont toutes les 2 comme vis_a_vis la maille %(k3)s
"""),















93: _("""

 evaluation impossible  d une fonction materiau - on deborde a gauche  pour la temperature
 temp : %(r1)f
"""),

94: _("""

 evaluation impossible  d une fonction materiau - on deborde a droite  pour la temperature
 temp : %(r1)f
"""),

95: _("""
 erreur dans les parametres valeur de typem :  %(k1)s
 les valeurs possibles sont no_maille,  no_noeud, nu_maille et nu_noeud %(k2)s
"""),

96: _("""
 erreur dans les parametres pour l'indice :  %(i1)d
 la valeur de tymocl est :  %(k1)s
 les valeurs possibles sont maille et group_ma %(k2)s
 car type2 vaut  %(k3)s
"""),

97: _("""
 erreur dans les parametres pour l'indice :  %(i1)d
 la valeur de tymocl est :  %(k1)s
 les valeurs possibles sont maille, noeud, %(k2)s
 group_ma,group_no et tout %(k3)s
"""),

98: _("""
 il manque le parametre  %(k1)s dans la table %(k2)s
 .sa presence est indispensable a la  creation du champ nodal. %(k3)s
"""),

99: _("""
 le parametre  %(k1)s ne doit pas apparaitre dans la  table %(k2)s
 pour la creation d'un champ nodal. %(k3)s
"""),

}
