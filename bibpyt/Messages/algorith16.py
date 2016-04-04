# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
  Informations sur les noeuds de choc
  lieu de choc   :  %(i1)d
  noeud de choc  :  %(k1)s
"""),

    3 : _(u"""
 sous-structure : %(k1)s
"""),

    4 : _(u"""
 coordonnées    : x :  %(r1)f
                  y :  %(r2)f
                  z :  %(r3)f
"""),

    5 : _(u"""
 noeud de choc  : %(k1)s
"""),

    8 : _(u"""
 amortissement tangent utilise :  %(r1)f

 origine choc x : %(r2)f
              y : %(r3)f
              z : %(r4)f

 NORM_OBST sin(alpha) : %(r5)f
           cos(alpha) : %(r6)f
           sin(bêta)  : %(r7)f
           cos(bêta)  : %(r8)f

 ANGL_VRILLE : sin(gamma) : %(r9)f
               cos(gamma) : %(r10)f
"""),

    9 : _(u"""
 jeu initial :  %(r1)f
"""),

    10 : _(u"""
 <INFO> Pour l'occurrence numéro %(i1)d du mot-clé facteur CHOC, RIGI_TAN est
 renseigné mais pas AMOR_TAN. Le code a donc attribué à AMOR_TAN une valeur
 optimisée : %(r1)f
"""),

    14 : _(u"""
 pas de temps utilisateur trop grand :   %(r1)e
 pas de temps nécessaire pour le calcul: %(r2)e
 risques de problèmes de précision

"""),

    15 : _(u"""
 pas de temps utilisateur trop grand :   %(r1)e
 pas de temps nécessaire pour le calcul: %(r2)e
 paramètres de calcul dans ce cas
 nombre de pas de calcul :  %(i1)d

"""),

    16 : _(u"""
 pas de temps utilisateur trop grand   : %(r1)e
 pas de temps nécessaire pour le calcul: %(r2)e
"""),

    17 : _(u"""
 paramètres de calcul dans ce cas
 nombre de pas de calcul :  %(i1)d

"""),

    18 : _(u"""
 le nombre d'amortissements réduits est trop grand
 le nombre de modes propres vaut  %(i1)d
 et le nombre de coefficients :  %(i2)d
 on ne garde donc que les  %(i3)d
   %(k1)s

"""),

    19 : _(u"""
 le nombre d'amortissements réduits est insuffisant il en manque :  %(i1)d
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d
 amortissements réduits avec la valeur du dernier mode propre

"""),

    20 : _(u"""
 mode dynamique           :  %(i1)d
 amortissement trop grand :  %(r1)f
 amortissement critique   :  %(r2)f
 problèmes de convergence possibles %(k1)s

"""),

    21 : _(u"""
 taux de souplesse négligée : %(r1)f
"""),

    44 : _(u"""
 les interfaces de la liaison n'ont pas la même longueur
  sous-structure 1 -->  %(k1)s
  interface 1      -->  %(k2)s
  sous-structure 2 -->  %(k3)s
  interface 2      -->  %(k4)s

"""),

    45 : _(u"""
 conflit dans les VIS_A_VIS des noeuds le noeud  %(k1)s
 est le vis-à-vis des noeuds  %(k2)s
 et  %(k3)s

"""),

    46 : _(u"""
 Le critère de vérification ne peut être relatif dans votre cas,
 la longueur caractéristique de l'interface de la sous-structure étant nulle.
  sous-structure 1 -->  %(k1)s
  interface 1      -->  %(k2)s
  sous-structure 2 -->  %(k3)s
  interface 2      -->  %(k4)s

"""),

    47 : _(u"""
 les interfaces ne sont pas compatibles sous-structure 1 -->  %(k1)s
  interface 1      -->  %(k2)s
  sous-structure 2 -->  %(k3)s
  interface 2      -->  %(k4)s

"""),

    48 : _(u"""
 les interfaces ne sont pas compatibles sous-structure 1 -->  %(k1)s
  interface 1      -->  %(k2)s
  sous-structure 2 -->  %(k3)s
  interface 2      -->  %(k4)s

"""),


    50 : _(u"""
 les deux interfaces ont pas même nombre de noeuds
 nombre noeuds interface droite -->  %(i1)d
 nombre noeuds interface gauche -->  %(i2)d

"""),

    51 : _(u"""
 conflit dans les VIS_A_VIS des noeuds
 le noeud  %(k1)s
 est le vis-à-vis des noeuds  %(k2)s et  %(k3)s

"""),

    52 : _(u"""
 axe de symétrie cyclique différent de Oz
 numéro du couple de noeuds :  %(i1)d
 noeud droite -->  %(k1)s
 noeud gauche -->  %(k2)s

"""),

    53 : _(u"""
  problème de rayon droite gauche différents
  numéro du couple de noeuds :  %(i1)d
 noeud droite -->  %(k1)s
 noeud gauche -->  %(k2)s

"""),

    54 : _(u"""
 problème signe angle entre droite et gauche
 numéro du couple de noeuds:  %(i1)d
 noeud droite -->  %(k1)s
 noeud gauche -->  %(k2)s

"""),

    55 : _(u"""
 problème valeur angle répétitivité cyclique
 numéro du couple de noeuds:  %(i1)d
 noeud droite -->  %(k1)s
 noeud gauche -->  %(k2)s

"""),

    56 : _(u"""
  vérification répétitivité : aucune erreur détectée
"""),

    57 : _(u"""
 les noeuds des interfaces ne sont pas alignés en vis-à-vis
 les noeuds ont été réordonnés

"""),

    58 : _(u"""
  arrêt sur problème répétitivité cyclique
  tentative de diagnostic:  %(k1)s
"""),

    60 : _(u"""
 VISCOCHAB : erreur d'intégration
  - Essai d'intégration numéro :  %(i1)d
  - Convergence vers une solution non conforme,
  - Incrément de déformation cumulée négative = %(r1)f,
  - Changer la taille d'incrément.
"""),

    68 : _(u"""
 Arrêt par manque de temps CPU au numéro d'ordre %(i1)d

   - Temps moyen par incrément de temps : %(r1)f
   - Temps restant                      : %(r2)f

 La base globale est sauvegardée, elle contient les pas archivés avant l'arrêt

 """),

    72 : _(u"""
Erreur utilisateur :
  On veut déplacer "au quart" les noeuds milieux des arêtes près du fond de fissure
  (MODI_MAILLAGE / MODI_MAILLE / OPTION='NOEUD_QUART') pour obtenir des éléments de Barsoum.

  Mais on ne trouve aucun noeud à déplacer !

Risques & conseils :
  * Avez-vous vérifié que le maillage est "quadratique" ?
  * Si votre maillage est linéaire et que vous souhaitez une solution précise
    grâce aux éléments de Barsoum, vous devez au préalable utiliser la commande :
      CREA_MAILLAGE / LINE_QUAD  pour rendre le maillage quadratique.
 """),




    77 : _(u"""
   Arrêt par manque de temps CPU au numéro d'ordre : %(i1)d
     - Dernier instant archivé :      %(r1)f
     - Numéro d'ordre correspondant : %(i2)d
     - Temps moyen par pas de temps : %(r2)f
     - Temps restant     :            %(r3)f
  """),

    88 : _(u"""
   Arrêt par manque de temps CPU au pas de temps : %(i1)d
     - A l'instant  :                %(r1)f
     - Temps moyen par pas :         %(r2)f
     - Temps restant     :           %(r3)f
  """),

    89 : _(u"""
   On passe outre car VERI_PAS = NON
  """),

    94 : _(u"""
  il manque les paramètres de Van Genuchten
 """),

    95 : _(u"""
  Van Genuchten non autorisé pour ce modèle de couplage
 """),

    96 : _(u"""
  Comportement ZEDGAR : la dérivée est nulle.
"""),







}
