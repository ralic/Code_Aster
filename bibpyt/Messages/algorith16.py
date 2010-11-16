#@ MODIF algorith16 Messages  DATE 15/11/2010   AUTEUR PELLET J.PELLET 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg = {

1 : _("""
 nombre de pas de calcul       :  %(i1)d
 nombre d'itérations           :  %(i2)d
 ----------------------------------------------

"""),

2 : _("""
  infos noeuds de choc
  lieu de choc   :  %(i1)d
  noeud de choc  :  %(k1)s
"""),

3 : _("""
 sous-structure : %(k1)s
"""),

4 : _("""
 coordonnées    : x :  %(r1)f
                  y :  %(r2)f
                  z :  %(r3)f
"""),

5 : _("""
 noeud de choc  : %(k1)s
"""),

8 : _("""
 amortissement tangent utilise :  %(r1)f

 origine choc x : %(r2)f
              y : %(r3)f
              z : %(r4)f

 norm_obst sin(alpha) : %(r5)f
           cos(alpha) : %(r6)f
           sin(beta)  : %(r7)f
           cos(beta)  : %(r8)f

 angl_vrille : sin(gamma) : %(r9)f
               cos(gamma) : %(r10)f
"""),

9 : _("""
 jeu initial :  %(r1)f
"""),

10 : _("""
 <INFO> Pour l'occurrence numéro %(i1)d du mot-clé facteur CHOC, RIGI_TAN est
 renseigné mais pas AMOR_TAN. Le code a donc attribué à AMOR_TAN une valeur
 optimisée : %(r1)f
"""),

11 : _("""
 le nb max d'iterations  %(i1)d  est atteint sans converger
 le résidu relatif final est  : %(r1)f

"""),

12 : _("""
 le nombre d'amortissements réduits est trop grand
 le nombre de modes retenus vaut  %(i1)d
 et le nombre de coefficients :  %(i2)d
 on ne garde donc que les  %(i3)d
   %(k1)s
"""),

13 : _("""
 le nombre d'amortissements réduits est insuffisant
 il en manque :  %(i1)d
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d
 amortissements réduits avec la valeur du dernier mode propre

"""),

14 : _("""
 pas de temps utilisateur trop grand :   %(r1)f
 pas de temps necessaire pour le calcul: %(r2)f
 risques de problemes de precision

"""),

15 : _("""
 pas de temps utilisateur trop grand :   %(r1)f
 pas de temps necessaire pour le calcul: %(r2)f
 parametres de calcul dans ce cas
 nb de pas de calcul :  %(i1)d

"""),

16 : _("""
 pas de temps utilisateur trop grand   : %(r1)f
 pas de temps necessaire pour le calcul: %(r2)f
"""),

17 : _("""
 parametres de calcul dans ce cas
 nb de pas de calcul :  %(i1)d

"""),

18 : _("""
 le nombre d'amortissements reduits est trop grand
 le nombre de modes propres vaut  %(i1)d
 et le nombre de coefficients :  %(i2)d
 on ne garde donc que les  %(i3)d
   %(k1)s

"""),

19 : _("""
 le nombre d'amortissements reduits est insuffisantil en manque :  %(i1)d
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d
 amortissements réduits avec la valeur du dernier mode propre

"""),

20 : _("""
 mode dynamique           :  %(i1)d
 amortissement trop grand :  %(r1)f
 amortissement critique   :  %(r2)f
 problemes de convergence possibles %(k1)s

"""),

21 : _("""
 taux de souplesse negligée : %(r1)f
"""),

22 : _("""
 calcul par superposition modale :
 la base de projection est un %(k1)s
 le nb d'équations est          : %(i1)d
 la methode utilisée est        : %(k2)s
 la base utilisée est           : %(k3)s
 le nb de vecteurs de base est  :  %(i2)d
"""),

23 : _("""
 le pas de temps initial est  : %(r1)f
 le nb de pas d'archive est     :  %(i1)d
"""),

24 : _("""
 NUME_VITE_FLUI                 :  %(i1)d
 vitesse gap                    :  %(r1)f
 le nb de modes de BASE_FLUI    :  %(i2)d
 le nb total de modes de la base:  %(i3)d
 le pas de temps initial est    :  %(r2)f
 durée de l'excitation          :  %(r3)f
"""),

25 : _("""
 le nb de pas d'archive est     :  %(i1)d
"""),

26 : _("""
 le pas de temps du calcul est  : %(r1)f
 le nb de pas de calcul est     :  %(i1)d
 le nb de pas d'archive est     :  %(i2)d
"""),

38 : _("""
 mode dynamique           :  %(i1)d
 amortissement trop grand :  %(r1)f
 amortissement critique   :  %(r2)f
 probleme de convergence possible %(k1)s
"""),

39 : _("""
 sous-structuration dynamique
 calcul par superposition modale
 ----------------------------------------------
 ! la numerotation utilisee est   :  %(k1)s
 ! le nb d'equations est          :  %(i1)d
 ! la methode utilisee est        :  %(k2)s
 !    - nb de vecteurs dynamiques :  %(i2)d
 !    - nb de deformees statiques :  %(i3)d
"""),

40 : _("""
 ! le pas de temps initial est  : %(r1)f
"""),

41 : _("""
 ! le pas de temps du calcul est  : %(r1)f
 ! le nb de pas de calcul est     :  %(i1)d
"""),

42 : _("""
 ! le nb de pas d'archive est     :  %(i1)d
"""),

44 : _("""
 les interfaces de la liaison n'ont pas la meme longueur
  sous-structure 1 -->  %(k1)s
  interface 1      -->  %(k2)s
  sous-structure 2 -->  %(k3)s
  interface 2      -->  %(k4)s

"""),

45 : _("""
 conflit dans les vis_a_vis des noeudsle noeud  %(k1)s
 est le vis-a-vis des noeuds  %(k2)s
 et  %(k3)s

"""),

46 : _("""
 Le critère de vérification ne peut etre relatif dans votre cas,
 la longueur caracteristique de l'interface de la sous-structure etant nulle.
  sous-structure 1 -->  %(k1)s
  interface 1      -->  %(k2)s
  sous-structure 2 -->  %(k3)s
  interface 2      -->  %(k4)s

"""),

47 : _("""
 les interfaces ne sont pas compatibles sous-structure 1 -->  %(k1)s
  interface 1      -->  %(k2)s
  sous-structure 2 -->  %(k3)s
  interface 2      -->  %(k4)s

"""),

48 : _("""
 les interfaces ne sont pas compatibles sous-structure 1 -->  %(k1)s
  interface 1      -->  %(k2)s
  sous-structure 2 -->  %(k3)s
  interface 2      -->  %(k4)s

"""),


50 : _("""
 les deux interfaces ont pas meme nombre de noeuds
 nombre noeuds interface droite -->  %(i1)d
 nombre noeuds interface gauche -->  %(i2)d

"""),

51 : _("""
 conflit dans les vis_a_vis des noeuds
 le noeud  %(k1)s
 est le vis-a-vis des noeuds  %(k2)s et  %(k3)s

"""),

52 : _("""
 axe de symétrie cyclique différent de Oz
 numéro du couple de noeuds :  %(i1)d
 noeud droite -->  %(k1)s
 noeud gauche -->  %(k2)s

"""),

53 : _("""
  probleme de rayon droite-gauche differents
  numero du couple de noeuds :  %(i1)d
 noeud droite -->  %(k1)s
 noeud gauche -->  %(k2)s

"""),

54 : _("""
 probleme signe angle entre droite et gauche
 numero du couple de noeuds:  %(i1)d
 noeud droite -->  %(k1)s
 noeud gauche -->  %(k2)s

"""),

55 : _("""
 probleme valeur angle répétitivité cyclique
 numero du couple de noeuds:  %(i1)d
 noeud droite -->  %(k1)s
 noeud gauche -->  %(k2)s

"""),

56 : _("""
  vérification répétitivité : aucune erreur détectée
"""),

57 : _("""
 les noeuds des interfaces ne sont pas alignés en vis-a-vis
 les noeuds ont ete réordonnés

"""),

58 : _("""
  arret sur probleme répétitivité cyclique
  tentative de diagnostic:  %(k1)s
"""),

60 : _("""
 VISCOCHABOCHE : erreur d'intégration
  - Essai d'intégration numéro :  %(i1)d
  - Convergence vers une solution non conforme,
  - Incrément de déformation cumulée négative = %(r1)f,
  - Changer la taille d'incrément.
"""),

68 : _("""
 Arret par manque de temps CPU au numéro d'ordre %(i1)d

   - Temps moyen par incrément de temps : %(r1)f
   - Temps CPU restant :                  %(r2)f

 La base globale est sauvegardée, elle contient les pas archivés avant l'arret

 """),

69 : _("""
    %(k1)s: ERREUR
      - Non convergence a itération maxi : %(i1)d
      - Convergence régulière mais trop lente
      - ERREUR > %(r1)f
      - Diminuer la taille d'incrément
 """),

70 : _("""
    %(k1)s: ERREUR
     - Non convergence à itération maxi : %(i1)d
     - Convergence irrégulière & erreur > %(r1)f
     - Diminuer la taille d'incrément
 """),

71 : _("""
    %(k1)s: ERREUR
     - Non convergence à itération maxi : %(i1)d
     - ERREUR > %(r1)f
     - Diminuer la taille d'incrément

 """),

72 : _("""
Erreur utilisateur :
  On veut déplacer "au quart" les noeuds milieux des arêtes près du fond de fissure
  (MODI_MAILLAGE / MODI_MAILLE / OPTION='NOEUD_QUART') pour obtenir des éléments de Barsoum.

  Mais on ne trouve aucun noeud à déplacer !

Risques & conseils :
  * Avez-vous vérifié que le maillage est "quadratique" ?
  * Si votre maillage est linéaire et que vous souhaitez une solution précise
    grace aux éléments de Barsoum, vous devez au préalable utiliser la commande :
      CREA_MAILLAGE / LINE_QUAD  pour rendre le maillage quadratique.
 """),




77 : _("""
   Arret par manque de temps CPU au numéro d'ordre : %(i1)d
     - Dernier instant archivé :      %(r1)f
     - Numéro d'ordre correspondant : %(i2)d
     - Temps moyen par pas de temps : %(r2)f
     - Temps CPU restant :            %(r3)f
  """),

78 : _("""
  Précision du transitoire : %(r1)f
  """),

79 : _("""
 Couplage temporel avec NB modes : %(i1)d
  """),

80 : _("""
 Le nombre de lieu(x) de choc est : %(i1)d
  """),

81 : _("""
 Le nombre de dispositifs anti-sismique est : %(i1)d
  """),

82 : _("""
 le nombre de lieu(x) de choc avec flambement est : %(i1)d
  """),

83 : _("""
 Le nombre de RELA_EFFO_DEPL est : %(i1)d
  """),

84 : _("""
 Le nombre de RELA_EFFO_VITE est : %(i1)d
  """),

87 : _("""
   Arret par manque de temps CPU
     - Instant courant :              %(r1)f
     - Nombre d'appels à ALITMI :     %(i1)d
     - Temps moyen par pas de temps : %(r2)f
     - Temps CPU restant :            %(r3)f
  """),

88 : _("""
   Arret par manque de temps CPU au pas de temps : %(i1)d
     - A l'instant  :                %(r1)f
     - Temps moyen par pas :         %(r2)f
     - Temps CPU restant :           %(r3)f
  """),

89 : _("""
   On passe outre car VERI_PAS = NON
  """),

91 : _("""
   La sous-structuration n'est compatible qu'avec un mode de parallélisme centralisé.

   Conseil :
     - Renseignez le mot-clé PARTITION/PARALLELISME de AFFE_MODELE (ou MODI_MODELE) avec 'CENTRALISE'
  """),

92 : _("""
   Au noeud de choc %(k1)s
  """),

93 : _("""
 Il y a moins de mailles (%(i1)d) dans le modèle que de processeurs participant au calcul (%(i2)d).

 Conseils :
   - vérifiez qu'un calcul parallèle est approprié pour votre modèle
   - diminuez le nombre de processeurs du calcul
"""),

94 : _("""
  il manque les paramètres de Van_Genuchten
 """),

95 : _("""
  Van_Genuchten non autorisé pour ce modèle de couplage
 """),

96 : _("""
  Comportement ZEDGAR : la dérivée est nulle.
"""),

97 : _("""
Erreur d'utilisation pour le parallélisme :
 Le mode de répartition des éléments entre les différents processeurs (PARTITION / PARALLELISME='GROUP_ELEM')
 ne peut pas etre utilisé ici car il y a moins de groupes d'éléments (%(i1)d) que de processeurs (%(i2)d).
 En d'autres termes, il n'y a pas assez d'éléments à répartir (le modèle est trop petit).

 Conseils :
   - diminuez le nombre de processeurs du calcul
   - changez le mode de distribution des mailles avec le mot-clé PARTITION / PARALLELISME de l'opérateur
     AFFE_MODELE (ou MODI_MODELE)
"""),

98: _("""
  La maille de numéro:  %(i1)d appartient à plusieurs sous-domaines !
"""),

99 : _("""
 Le paramètre CHARGE_PROC0_SD du mot-clé facteur PARTITION est mal renseigné.
 Il faut qu'il reste au moins un sous domaine par processeur une fois affectés tous les sous-domaines du processeur 0.

 Conseils :
   - laissez le mot-clé CHARGE_PROC0_SD à sa valeur par défaut
   - diminuez le nombre de processeurs du calcul ou bien augmentez le nombre de sous-domaines de la partition du mot-clé PARTITION
"""),

}
