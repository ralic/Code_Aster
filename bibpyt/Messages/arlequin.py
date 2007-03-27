#@ MODIF arlequin Messages  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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
 Pour Arlequin, la dimension topologique des modelisations de GROUP_MA_1 et GROUP_MA_2 doivent etre les memes:
  - C_PLAN (2D)
  - ou D_PLAN (2D)
  - ou AXIS (2D)
  - ou 3D/DKT/DST/COQUE_3D/Q4G (3D)
"""),

2: _("""
 Le groupe de maille <%(k1)s> n'existe pas dans le maillage
"""),

3: _("""
 Il y a plusieurs modelisations dans le meme groupe de maille
"""),

4: _("""
 Il y a plusieurs cinematiques (melange elements de structures/elements de milieu continu) dans le meme groupe de maille
"""),

5: _("""
 Aucune maille du groupe n'est utilisable dans Arlequin, on rappelle ce qui est utilisable:
  - elements de deformations planes (D_PLAN)
  - elements de contraintes planes (C_PLAN)
  - elements axisymetriques (AXIS)
  - elements 3D
  - elements de structure de type coques et plaques (DKT/DST/COQUE_3D/Q4G)
"""),

6: _("""
 La normale au noeud <%(k1)s> de la maille <%(k2)s> est nulle.
 Verifiez votre maillage (pas de mailles aplaties par exemple)
"""),

7: _("""
 La normale moyenne sur la maille <%(k1)s> est nulle.
 Verifiez votre maillage (orientations des mailles par exemple)
"""),

8: _("""
 Il faut renseigner le mot-clef CARA_ELEM lorsqu'on utilise des elements coques
"""),

9: _("""
 Les deux domaines ne se recouvrent pas. Verifiez vos groupes.
 """),

10: _("""
 Le groupe de mailles de collage (GROUP_MA_COLL) doit etre un sous ensemble d'un des deux
 sous domaines GROUP_MA_1 ou GROUP_MA_2
 """),

11: _("""
 La maille <%(k1)s> est de type %(k2)s: elle ne peut etre mise en boite.
 Ce type de maille n'est pas pris en compte.
"""),

12: _("""
 Aucune maille de la zone de collage n'est appariee
"""),

13: _("""
 Nombre de couples apparies sous-estime - Erreur avancee: contacter le support
"""),

14: _("""
 La gestion des conditions limites redondantes a ete deconnectee dans Arlequin
"""),

23: _("""
 Mauvaise intersection
"""),

24: _("""
 Nombre de composantes connexes maximal prevu insuffisant
"""),

25: _("""
 Polyedre non etoile au centre de gravite
"""),

}
