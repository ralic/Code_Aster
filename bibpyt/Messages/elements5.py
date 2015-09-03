# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

    2 : _(u"""
 tuyau : le nombre de couches est limite a  %(i1)d
"""),

    3 : _(u"""
 tuyau : le nombre de secteurs est limite a  %(i1)d
"""),

    4 : _(u"""
 Le nombre de sous-points est limité à %(i1)d, or vous en avez définis %(i2)d !
 Veuillez contacter votre assistance technique.
"""),

    15 : _(u"""
A l'occurrence %(i1)d, les objets précédemment évoqués sont inexistants ou de type incompatible.
"""),

    16 : _(u"""
Les mailles fournies sont non consécutives dans la numérotation des noeuds.
En effet, les mailles segment doivent être ordonnées de telle sorte que pour deux segments
consécutifs, le deuxième noeud sommet du premier segment soit le même que le premier noeud
sommet du deuxième segment.

Conseil : Pour ordonner les mailles du fond de fissure, veuillez
utiliser NOEUD_ORIG (ou GROUP_NO_ORIG) et NOEUD_EXTR (ou GROUP_NO_EXTR).
"""),

    17 : _(u"""
Un seul noeud doit constituer le groupe de noeuds %(k1)s. On n'utilisera que le noeud %(k2)s.
"""),

    19 : _(u"""
 A l'occurrence %(i1)d, la maille %(k1)s est inexistante.
"""),

    20 : _(u"""
 A l'occurrence %(i1)d, la maille %(k1)s n'est pas linéique.
"""),

    21 : _(u"""
 A l'occurrence %(i1)d, le mélange de SEG2 et de SEG3 (maille %(k1)s) n'est possible.
"""),

    22 : _(u"""
   Erreur, le nombre de noeuds d'un élément de joint 3D n'est pas correct
"""),

    23 : _(u"""
   Erreur, le nombre de points de Gauss d'un élément de joint 3D n'est pas correct
"""),

    24 : _(u"""
  le nombre de mailles du modèle %(i1)d est différent de la somme des mailles des
  sous-domaines %(i2)d
"""),





    28 : _(u"""
  le modèle comporte %(i1)d mailles de plus que l'ensemble des sous-domaines
"""),

    29 : _(u"""
  le modèle comporte %(i1)d mailles de moins que l'ensemble des sous-domaines
"""),

    30 : _(u"""
 jacobien négatif ou nul : jacobien =  %(r1)f
"""),







    39 : _(u"""
 Échec de la recherche de zéro a l'itération :  %(i1)d
  fonction décroissante - pour x=a:  %(r1)f
  / fonction(a):  %(r2)f
                          et   x=b:  %(r3)f
  / fonction(b):  %(r4)f

  fonction x=:  %(r5)f  %(r6)f  %(r7)f  %(r8)f  %(r9)f  %(r10)f %(r11)f %(r12)f %(r13)f %(r14)f
                %(r15)f %(r16)f %(r17)f %(r18)f %(r19)f %(r20)f %(r21)f %(r22)f %(r23)f %(r24)f

  fonction f=:  %(r25)f  %(r26)f  %(r27)f  %(r28)f  %(r29)f  %(r30)f %(r31)f %(r32)f %(r33)f %(r34)f
                %(r35)f  %(r36)f  %(r37)f  %(r38)f  %(r39)f  %(r40)f %(r41)f %(r42)f %(r43)f %(r44)f

"""),

    40 : _(u"""
 Échec de la recherche de zéro a l'itération :  %(i1)d
  fonction constante    - pour x=a:  %(r1)f
  / fonction(a):  %(r2)f
                          et   x=b:  %(r3)f
  / fonction(b):  %(r4)f

  fonction x=:  %(r5)f  %(r6)f  %(r7)f  %(r8)f  %(r9)f  %(r10)f %(r11)f %(r12)f %(r13)f %(r14)f
                %(r15)f %(r16)f %(r17)f %(r18)f %(r19)f %(r20)f %(r21)f %(r22)f %(r23)f %(r24)f

  fonction f=:  %(r25)f  %(r26)f  %(r27)f  %(r28)f  %(r29)f  %(r30)f %(r31)f %(r32)f %(r33)f %(r34)f
                %(r35)f  %(r36)f  %(r37)f  %(r38)f  %(r39)f  %(r40)f %(r41)f %(r42)f %(r43)f %(r44)f

"""),

    41 : _(u"""
     Température négative à la maille :  %(k1)s
"""),

    42 : _(u"""
 L'épaisseur définie dans DEFI_GLRC et celle définie dans AFFE_CARA_ELEM ne sont pas cohérentes.
 Épaisseur dans DEFI_GLRC: %(r1)f
 Épaisseur dans AFFE_CARA_ELEM: %(r2)f
"""),

    44 : _(u"""
Erreur utilisateur :
  L'état métallurgique initial produit par CREA_CHAMP est incomplet.
Conseil :
  Pour le Zircaloy, il faut renseigner V1, V2 et V4
  Pour l'acier, il faut renseigner V1, V2, ..., V5
"""),

    45 : _(u"""
OPTION MASS_INER : la masse volumique RHO doit être non nulle
"""),

    46 : _(u"""
  relation :  %(k1)s  non implantée pour les éléments COQUE_3D
  relation : ELAS obligatoirement
"""),

    47 : _(u"""
    Il n'est pas possible d'utiliser ANGL_AXE et ORIG_AXE de AFFE_CARA_ELEM pour les modélisations XXX_INTERFACE
"""),

    48 : _(u"""
    Il n'est pas possible d'utiliser ANGL_AXE et ORIG_AXE de AFFE_CARA_ELEM pour les modélisations XXX_JHMS
"""),
    49 : _(u"""
   La méthode IMPLEX ne peut pas être utilisée avec la loi de comportement que vous
   avez choisi ; sur les éléments BARRE elle n'est utilisable qu'avec VMIS_ISOT_LINE et ELAS
"""),

    50 : _(u"""
   La méthode IMPLEX ne peut pas être utilisée avec la loi de comportement que vous
   avez choisi ; sur les éléments 2D et 3D elle n'est utilisable qu'avec VMIS_ISOT_LINE,
   ENDO_FRAGILE et ENDO_ISOT_BETON
"""),

    51 : _(u"""
  CHAMP :  %(k1)s  non traité sous le type COQUE_GENE. Les champs traités sont
  EFGE et DEGE (ELGA ou ELNO)
"""),

    52 : _(u"""
  CHAMP :  %(k1)s  non traité sous le type TENS_3D. Les champs traités sont
  SIGM et EPSI (ELGA ou ELNO)
"""),

    53 : _(u"""
  TYPE :  %(k1)s  non traité pour les coques. Les types traités sont
  TENS_3D et COQUE_GENE.
"""),

    54 : _(u"""
  Le nombre de sous-points est : %(i1)d. Il doit soit valoir 1 (si on a déjà extrait le champ) soit un
  multiple de 3 (si le champ est complet).
"""),

    55 : _(u"""
  Le changement de repère : %(k1)s sur les coques n'est pas traité pour les champs de type : %(k2)s.
"""),

}
