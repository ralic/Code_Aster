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

cata_msg={

1 : _(u"""
 On ne trouve pas de valeurs dans le champ.
"""),

2 : _(u"""
 Le champ %(k1)s n'est pas défini sur la totalité des noeuds précisés
 dans le mot-clé GROUP_MA_ESCL, GROUP_NO_ESCL, MAILLE_ESCL ou
 NOEUD_ESCL.
 
 Il vous faut compléter le champ %(k1)s.
"""),

3: _(u"""
 La composante %(k1)s du champ  n'existe pas.
"""),

4 : _(u"""
 On ne trouve pas de mailles dans les groupes fournis.
"""),

5 : _(u"""
 On ne trouve pas de noeuds dans les groupes fournis.
"""),

6 : _(u"""
 Le champ %(k1)s n'existe pas au numéro d'ordre %(i1)d dans
 le concept résultat %(k2)s.
"""),

7 : _(u"""
 On ne sait pas calculer le critère %(k1)s pour les champs de la grandeur %(k2)s.
"""),

8 : _(u"""
    Numéro d'ordre %(i1)4d : %(i2)d mailles ont été affectées (%(i3)d mailles dans le maillage).
"""),

9 : _(u"""
 Le champ %(k1)s n'existe ni dans le concept résultat %(k2)s, ni dans %(k3)s
 au numéro d'ordre %(i1)d.
"""),

10 : _(u"""
    Numéro d'ordre %(i1)4d : %(i2)d noeuds ont été affectés (%(i3)d noeuds dans le maillage).
"""),

11 : _(u"""
 Pour extraire la valeur d'un champ constant par élément (type ELEM), il est nécessaire de fournir
 un nom de maille ou un groupe de mailles.

 Conseil:
   - Renseignez un des mots-clés MAILLE ou GROUP_MA.
"""),

12 : _(u"""
 Pour extraire la valeur d'un champ par élément aux noeuds (type ELNO), il est nécessaire de fournir
 un nom de maille ou un groupe de mailles et un nom de noeud ou un groupe de noeuds.

 Conseil:
   - Renseignez un des mots-clés MAILLE ou GROUP_MA et un des mots-clés NOEUD, GROUP_NO ou POINT.
"""),

13 : _(u"""
 Pour extraire la valeur d'un champ par élément aux points de Gauss (type ELGA), il est nécessaire
 de fournir un nom de maille ou un groupe de mailles et le numéro de point de Gauss.

 Conseil:
   - Renseignez un des mots-clés MAILLE ou GROUP_MA et le mot-clé POINT.
"""),

14 : _(u"""
Il n'est pas possible de créer le champ '%(k1)s' dans la structure
de donnée '%(k2)s'.

Conseil:
    Vérifiez que le champ n'existe pas déjà.
    Il est possible que cette structure de donnée n'accepte pas ce type de champ.

"""),

}
