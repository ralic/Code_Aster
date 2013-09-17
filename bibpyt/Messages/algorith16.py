# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
 nombre de pas de calcul       :  %(i1)d
 nombre d'itérations           :  %(i2)d
 ----------------------------------------------

"""),

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

11 : _(u"""
 le nombre max d'itérations  %(i1)d  est atteint sans converger
 le résidu relatif final est  : %(r1)f

"""),

12 : _(u"""
 le nombre d'amortissements réduits est trop grand
 le nombre de modes retenus vaut  %(i1)d
 et le nombre de coefficients :  %(i2)d
 on ne garde donc que les  %(i3)d
   %(k1)s
"""),

13 : _(u"""
 le nombre d'amortissements réduits est insuffisant
 il en manque :  %(i1)d
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d
 amortissements réduits avec la valeur du dernier mode propre

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

22 : _(u"""
 calcul par superposition modale :
 la base de projection est un %(k1)s
 le nombre d'équations est          : %(i1)d
 la méthode utilisée est        : %(k2)s
 la base utilisée est           : %(k3)s
 le nombre de vecteurs de base est  :  %(i2)d
"""),

23 : _(u"""
 le pas de temps initial est  : %(r1)f
 le nombre de pas d'archive est     :  %(i1)d
"""),

24 : _(u"""
 NUME_VITE_FLUI                 :  %(i1)d
 vitesse gap                    :  %(r1)f
 le nombre de modes de BASE_FLUI    :  %(i2)d
 le nombre total de modes de la base:  %(i3)d
 le pas de temps initial est    :  %(r2)f
 durée de l'excitation          :  %(r3)f
"""),

25 : _(u"""
 le nombre de pas d'archive est     :  %(i1)d
"""),

26 : _(u"""
 le pas de temps du calcul est  : %(r1)f
 le nombre de pas de calcul est     :  %(i1)d
 le nombre de pas d'archive est     :  %(i2)d
"""),

38 : _(u"""
 mode dynamique           :  %(i1)d
 amortissement trop grand :  %(r1)f
 amortissement critique   :  %(r2)f
 problème de convergence possible %(k1)s
"""),

39 : _(u"""
 sous-structuration dynamique
 calcul par superposition modale

 la numérotation utilisée est   :  %(k1)s
 le nombre d'équations est          :  %(i1)d
 la méthode utilisée est        :  %(k2)s
    - nombre de vecteurs dynamiques :  %(i2)d
    - nombre de déformées statiques :  %(i3)d
"""),

40 : _(u"""
 le pas de temps initial est  : %(r1)f
"""),

41 : _(u"""
 le pas de temps du calcul est  : %(r1)f
 le nombre de pas de calcul est     :  %(i1)d
"""),

42 : _(u"""
 le nombre de pas d'archive est     :  %(i1)d
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
   - Temps CPU restant :                  %(r2)f

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
     - Temps CPU restant :            %(r3)f
  """),

78 : _(u"""
  Précision du transitoire : %(r1)f
  """),

79 : _(u"""
 Couplage temporel avec NB modes : %(i1)d
  """),

80 : _(u"""
 Le nombre de lieu(x) de choc est : %(i1)d
  """),

81 : _(u"""
 Le nombre de dispositifs anti-sismique est : %(i1)d
  """),

82 : _(u"""
 le nombre de lieu(x) de choc avec flambement est : %(i1)d
  """),

83 : _(u"""
 Le nombre de RELA_EFFO_DEPL est : %(i1)d
  """),

84 : _(u"""
 Le nombre de RELA_EFFO_VITE est : %(i1)d
  """),

87 : _(u"""
   Arrêt par manque de temps CPU
     - Instant courant :              %(r1)f
     - Nombre d'appels à ALITMI :     %(i1)d
     - Temps moyen par pas de temps : %(r2)f
     - Temps CPU restant :            %(r3)f
  """),

88 : _(u"""
   Arrêt par manque de temps CPU au pas de temps : %(i1)d
     - A l'instant  :                %(r1)f
     - Temps moyen par pas :         %(r2)f
     - Temps CPU restant :           %(r3)f
  """),

89 : _(u"""
   On passe outre car VERI_PAS = NON
  """),

91 : _(u"""
   La sous-structuration n'est compatible qu'avec un mode de parallélisme centralisé.

   Conseil :
     - Renseignez le mot-clé PARTITION/PARALLELISME de AFFE_MODELE (ou MODI_MODELE) avec 'CENTRALISE'
  """),

92 : _(u"""
   Au noeud de choc %(k1)s
  """),

93 : _(u"""
 Il y a moins de mailles (%(i1)d) dans le modèle que de processeurs participant au calcul (%(i2)d).

 Conseils :
   - vérifiez qu'un calcul parallèle est approprié pour votre modèle
   - diminuez le nombre de processeurs du calcul
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

97 : _(u"""
Erreur d'utilisation pour le parallélisme :
 Le mode de répartition des éléments entre les différents processeurs (PARTITION / PARALLELISME='GROUP_ELEM')
 ne peut pas être utilisé ici car il y a moins de groupes d'éléments (%(i1)d) que de processeurs (%(i2)d).
 En d'autres termes, il n'y a pas assez d'éléments à répartir (le modèle est trop petit).

 Conseils :
   - diminuez le nombre de processeurs du calcul
   - changez le mode de distribution des mailles avec le mot-clé PARTITION / PARALLELISME de l'opérateur
     AFFE_MODELE (ou MODI_MODELE)
"""),

98: _(u"""
  La maille de numéro:  %(i1)d appartient à plusieurs sous-domaines !
"""),

99 : _(u"""
 Le paramètre CHARGE_PROC0_SD du mot-clé facteur PARTITION est mal renseigné.
 Il faut qu'il reste au moins un sous domaine par processeur une fois affectés tous les sous-domaines du processeur 0.

 Conseils :
   - laissez le mot-clé CHARGE_PROC0_SD à sa valeur par défaut
   - diminuez le nombre de processeurs du calcul ou bien augmentez le nombre de sous-domaines de la partition du mot-clé PARTITION
"""),

}
