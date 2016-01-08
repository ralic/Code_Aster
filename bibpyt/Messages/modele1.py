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

    1 : _(u"""
Pour affecter une liste de modélisations, il faut qu'elles soient de même dimension topologique.
"""),

    2 : _(u"""
La maille %(k1)s de type %(k2)s n'a pas pu être affectée.
"""),

    3 : _(u"""
Le noeud %(k1)s n'a pas pu être affecté.
"""),

    4 : _(u"""
Sur les %(i1)d mailles du maillage %(k1)s, on a demandé l'affectation de %(i2)d, on a pu en affecter %(i3)d
"""),

    5 : _(u"""
Sur les %(i1)d noeuds du maillage %(k1)s, on a demandé l'affectation de %(i2)d, on a pu en affecter %(i3)d
"""),

    6 : _(u"""
Aucune maille du maillage %(k1)s n'a été affectée par des éléments finis.
"""),

    7 : _(u"""
Attention l'élément HEXA8 en 3D_SI ne fonctionne correctement que sur les parallélépipèdes.
Sur les éléments quelconques on peut obtenir des résultats faux.
"""),

    8 : _(u"""Liste des noeuds affectés pour la modélisation:"""),

    9 : _(u"""Liste des mailles affectées pour la modélisation:"""),

    14 : _(u"""
Le modèle contient un mélange d'éléments finis 2D (plan Oxy) et 3D

  -> Risque & Conseil :
     Sur ce genre de modèle, on ne sait pas déterminer s'il est 2D ou 3D.
     Parfois, cela empêche de faire le "bon choix".
"""),

    20 : _(u""" Modélisation     Type maille  Élément fini     Nombre"""),

    21 : _(u""" %(k1)-16s %(k2)-12s %(k3)-16s %(i1)d"""),

    38 : _(u"""%(k1)-8s %(k2)-8s %(k3)-8s %(k4)-8s %(k5)-8s %(k6)-8s %(k7)-8s %(k8)-8s"""),

    53 : _(u"""
  -> Le maillage est 3D (tous les noeuds ne sont pas dans le même plan Z = constante),
     mais les éléments du modèle sont de dimension 2.

  -> Risque & Conseil :
     Si les facettes supportant les éléments ne sont pas dans un plan Z = constante,
     les résultats seront faux.
     Assurez-vous de la cohérence entre les mailles à affecter et la
     modélisation souhaitée dans la commande AFFE_MODELE.
"""),

    54 : _(u"""
Il est interdit de mélanger des éléments discrets 2D et 3D dans le même modèle.
"""),


    58 : _(u"""
 -> Bizarre :
     Les éléments du modèle sont de dimension 2.
     Mais les noeuds du maillage sont un même plan Z = a avec a != 0.,

 -> Risque & Conseil :
     Il est d'usage d'utiliser un maillage Z=0. pour les modélisations planes ou Axis.
"""),

    63: _(u"""
  -> La maille %(k1)s porte un élément fini de bord, mais elle ne borde
     aucun élément ayant une "rigidité".

  -> Risque & Conseil :
     Cela peut entraîner des problèmes de "pivot nul" lors de la résolution.
     Si la résolution des systèmes linéaires ne pose pas de problèmes, vous
     pouvez ignorer ce message.
     Sinon, vérifier la définition du modèle (AFFE_MODELE) en évitant l'utilisation
     de l'opérande TOUT='OUI'.
"""),

    64: _(u"""
  -> Le modèle %(k1)s n'a pas d'éléments sachant calculer la rigidité.

  -> Risque & Conseil :
     Ce modèle ne pourra donc pas (en général) être utilisé pour faire des calculs.
     Vérifier la définition du modèle (AFFE_MODELE) et assurez-vous que les
     types de mailles du maillage (SEG2, TRIA3, QUAD4, ...) sont compatibles avec votre
     modélisation.
     Exemples d'erreur :
       * affecter une modélisation "3D" sur un maillage formé de facettes.
       * affecter une modélisation qui ne sait pas traiter tous les types de mailles du maillage
         (par exemple 'PLAN_DIAG' en thermique, 'AXIS_SI' en mécanique)
"""),

    70 : _(u"""
 Possible erreur utilisateur dans la commande AFFE_MODELE :
   Un problème a été détecté lors de l'affectation des éléments finis.
   Pour l'occurrence AFFE de numéro %(i1)d, certaines mailles de même dimension topologique
   que la (ou les) modélisation(s) (ici dimension = %(i3)d) n'ont pas pu être affectées.

   Cela veut dire que la modélisation que l'on cherche à affecter
   ne supporte pas tous les types de mailles présents dans le maillage.

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


}
