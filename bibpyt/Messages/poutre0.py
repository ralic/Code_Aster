# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    1: _(u"""
GROUP_MA et GROUP_MA_BORD incohérents.
"""),

    2: _(u"""
GROUP_MA et NOEUD incohérents.
"""),

    3: _(u"""
Il faut donner un noeud unique.
"""),

    4: _(u"""
Poutre circulaire à variation de section homothétique.

Le rapport d'homothétie est assez différent entre les rayons et les épaisseurs :
    - rayon 1 = %(r1)16.9g
    - rayon 2 = %(r2)16.9g
        `- rapport d'homothétie = %(r5)16.9g

    - épaisseur 1 = %(r3)16.9g
    - épaisseur 2 = %(r4)16.9g
        `- rapport d'homothétie = %(r6)16.9g

La différence entre les rapports d'homothétie est supérieure à 1%%.
Les hypothèses du modèle de poutre à variation homothétique ne sont donc pas
respectées (consultez la documentation de référence des éléments poutre).


Risques et conseil:
    - Les calculs seront inexacts.
    - Raffiner le maillage permet de minimiser les écarts.
"""),


   10: _(u"""La caractéristique %(k1)8s est négative ou nulle %(r1)e
"""),

   11: _(u"""
Problème lors de l'utilisation de MACR_CARA_POUTRE

La section présente des caractéristiques mécaniques négatives.

Vous avez renseigné l'option TABLE_CARA='OUI'. Si vous utilisez les résultats dans la commande
AFFE_CARA_ELEM / POUTRE avec TABLE_CARA, vous risquez d'avoir des résultats faux.

Conseil : La discrétisation de la section a un impact sur la qualité des résultats.
          Raffiner le maillage devrait permettre de lever cette erreur.
"""),

   12: _(u"""
Les coordonnées du centre de gravité de la section sont G=(%(r1)e, %(r2)e)

Si vous utilisez des MULTIFIBRES et que le maillage de description des fibres est le même que
celui utilisé dans cette commande, il faut dans la commande DEFI_GEOM_FIBRE renseigner le mot
clef COOR_AXE_POUTRE de façon à faire correspondre le centre de gravité des fibres à l'axe
neutre de la poutre : COOR_AXE_POUTRE = (%(r1)e, %(r2)e)

Vous avez renseigné l'option TABLE_CARA='OUI'. Si vous utilisez les résultats dans la commande :
   AFFE_CARA_ELEM / POUTRE avec TABLE_CARA,
                  / GEOM_FIBRE
                  / MULTIFIBRE
Vous risquez d'avoir des résultats inattendus, si vous ne renseignez pas COOR_AXE_POUTRE.
"""),

   13: _(u"""
Le repère principal d'inertie est tourné d'un angle de %(r1)f° par rapport aux axes du maillage.

Si vous utilisez des MULTIFIBRES et que le maillage de description des fibres est le même que
celui utilisé dans cette commande, il faut dans la commande DEFI_GEOM_FIBRE renseigner le mot
clef ANGLE de façon à faire correspondre le repère principal d'inertie aux axes du maillage de
la section : ANGLE = %(r2)e

Vous avez renseigné l'option TABLE_CARA='OUI'. Si vous utilisez les résultats dans la commande
AFFE_CARA_ELEM / POUTRE avec TABLE_CARA, il est peut-être nécessaire dans la commande
AFFE_CARA_ELEM de renseigner le mot clef ORIENTATION de façon à définir la position du repère
principal d'inertie par rapport au repère global.

ORIENTATION=(
    _F(GROUP_MA='....', CARA='ANGL_VRIL', VALE= ??? )
)
Par défaut ANGL_VRIL= 0

Vous risquez d'avoir des résultats inattendus, si vous ne renseignez ni :
 - ANGLE dans DEFI_GEOM_FIBRE
 - ORIENTATION dans AFFE_CARA_ELEM
"""),

}
