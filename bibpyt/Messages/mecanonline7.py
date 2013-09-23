# coding=utf-8
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


# Attention a ne pas faire de retour à la ligne !

cata_msg = {

1 : _(u"""
  Temps CPU consommé dans ce pas de temps  : %(k1)s
"""),

2 : _(u"""    * Temps moyen par itération de Newton  : %(k1)s ( %(i1)d itérations     )"""),

3 : _(u"""    * Temps total factorisation matrice    : %(k1)s ( %(i1)d factorisations )"""),

4 : _(u"""    * Temps total intégration comportement : %(k1)s ( %(i1)d intégrations   )"""),

5 : _(u"""    * Temps total résolution K.U=F         : %(k1)s ( %(i1)d résolutions    )"""),

6 : _(u"""    * Temps autres opérations              : %(k1)s"""),

7 : _(u"""    * Temps post-traitement                : %(k1)s"""),

8 : _(u"""    * Nombre d'itérations de recherche linéaire   : %(i1)d"""),


10 : _(u"""    * Temps résolution contact             : %(k1)s ( %(i1)d itérations     )"""),

11 : _(u"""    * Temps appariement contact            : %(k1)s ( %(i1)d appariements   )"""),

12 : _(u"""    * Temps construction second membre     : %(k1)s"""),

13 : _(u"""    * Temps assemblage matrice             : %(k1)s"""),

20 : _(u"""    * Temps construction matrices contact  : %(k1)s ( %(i1)d constructions  )"""),

22 : _(u"""    * Temps frottement                     : %(k1)s ( %(i1)d boucles        )"""),

23 : _(u"""    * Temps contact                        : %(k1)s ( %(i1)d boucles        )"""),

24 : _(u"""    * Temps préparation données contact    : %(k1)s ( %(i1)d préparations   )"""),

25 : _(u"""    * Temps construction vecteurs contact  : %(k1)s ( %(i1)d constructions  )"""),

30 :_(u"""
  Statistiques du contact dans ce pas de temps
"""),

31 : _(u"""    * Nombre de liaisons de contact               : %(i1)d"""),

32 : _(u"""    * Nombre de liaisons de frottement adhérentes : %(i1)d"""),

}
