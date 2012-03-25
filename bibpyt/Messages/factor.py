#@ MODIF factor Messages  DATE 26/03/2012   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg={

1: _(u"""
Le système linéaire à résoudre contient %(i1)d noeuds dont:
   - %(i2)d noeuds portant des degrés de liberté physiques
   - %(i3)d noeuds portant des degrés de liberté de Lagrange
Pour un total de %(i4)d équations.
"""),

2: _(u"""
La matrice est de taille %(i1)d équations.
Mais elle contient:
   -  %(i2)d termes non nuls si elle est symétrique (soit un taux de remplissage de  %(r1)9.2e %%)
   -  %(i3)d termes non nuls si elle n'est pas symétrique (soit un taux de remplissage de  %(r2)9.2e %%)
Le nombre de termes non nuls est susceptible de varier si l'on utilise le contact en méthode continue
ou la méthode XFEM avec contact.
"""),



10: _(u"""
Problème : la matrice n'est pas factorisable :
  Lors de la factorisation de la matrice, on a rencontré un problème
  (pivot nul ou presque nul) à la ligne %(i1)d
  Le degré de liberté associé à cette ligne est de type : (%(k4)s)
  Le nombre de décimales "perdues" pour cette ligne est : %(i2)d.
  Ce nombre de décimales perdues est lié au degré de singularité de la matrice. Plus il est grand,
  plus le système est singulier. Quand il dépasse 8, on considère que l'on a perdu 50%% de la précision
  sur les nombres réels (qui ont 15 décimales environ).

  Les différents types du degré de liberté sont :
    * (A) : Degré liberté physique associé au noeud : %(k1)s et à la composante : %(k2)s.
    * (B) : Degré de liberté de Lagrange associé au blocage : (%(k3)s).
    * (C) : Degré de liberté de Lagrange associé à une relation linéaire entre plusieurs degrés de liberté.
            La liste des noeuds impliqués dans cette relation linéaire est imprimée ci-dessus.
    * (D) : Degré de liberté d'un système "généralisé".
            Nom du noeud : %(k5)s
            Composante   : %(k6)s
            Information complémentaire (éventuellement)   : %(k7)s

Conventions :
  Ce message peut être émis dans des situations différentes pour lesquelles on ne connaît
  pas toutes les informations imprimées ci-dessus.
  On adopte alors les conventions suivantes :
    * Si le numéro de la ligne est -999 :
        Soit la matrice est vraiment singulière et la factorisation n'a pu aller jusqu'au bout.
        Soit on ne sait pas attribuer la singularité de la matrice à une ligne de la matrice.
    * Si le nombre de décimales perdues est -999 :
        On ne sait pas déterminer la perte de décimales sur la ligne incriminée,

Risques et conseils :
   * Si la ligne correspond a un degré de liberté physique (A), il s'agit probablement d'un mouvement
     de corps rigide mal bloqué.
     Vérifiez les conditions aux limites.
     Si vous faites du contact, il ne faut pas que la structure ne "tienne" que par le contact.
     Vérifiez également les caractéristiques matériaux (module d'Young, ...).

   * Si la ligne correspond a un degré de liberté de Lagrange (B) ou (C), il s'agit sans doute d'une condition
     limite redondante.
     En particulier, il se peut que la relation linéaire surabondante provienne des conditions de contact.
     Peut-être devriez vous exclure certains noeuds des conditions de contact
     (mots clés SANS_NOEUD et SANS_GROUP_NO).

   * Si le solveur utilisé est LDLT ou MULT_FRONT, vous pouvez utiliser le solveur MUMPS
     car celui-ci est le seul à pouvoir factoriser les matrices qui ne sont pas définies positives.

   * Il se peut aussi que ce phénomène soit tout à fait normal avec X-FEM si la fissure passe
     très près d'un noeud.
     Si le nombre de décimales perdues n'est pas trop grand (max 10 décimales),
     vous pouvez relancer le calcul en augmentant le nombre de décimales perdues autorisé :
     mot-clé NPREC du mot clé facteur SOLVEUR.
     Sinon, contactez l'équipe de développement.

"""),


13: _(u"""
Solveur FETI :
  Le solveur FETI est impossible dans ce contexte.
Solution :
  Il faut changer de solveur.
"""),



42: _(u"""
Matrice non factorisable :
  Le solveur MUMPS considère la matrice comme singulière (en structure ou numériquement).

Conseil :
  Il peut s'agir d'une erreur de programmation ou d'un problème de mise en données (blocage
  absent ou surabondant).
"""),


50: _(u"""
 Solveur MUMPS :
   -> Vous avez demandé comme numéroteur RENUM = '%(k1)s', or MUMPS en a
      utilisé un autre.
   -> Risque & Conseil :
      Il se peut que votre version de MUMPS n'ait pas été compilée avec
      le support de ce numéroteur. Dans le doute, RENUM='AUTO' permet
      de laisser MUMPS faire le meilleur choix.
 """),

52: _(u"""
  -> Vous avez demandé une analyse de stabilité et vous utilisez le solveur linéaire '%(k1)s'.
     Ces deux fonctionnalités ne sont pas compatibles.

  -> Conseil :
     Changez de solveur linéaire en utilisant le mot-clé METHODE de SOLVEUR.
"""),

53: _(u"""
Solveur MUMPS :
  MUMPS manque de mémoire lors de la factorisation de la matrice.
Solution :
  Il faut augmenter la valeur du mot clé  SOLVEUR/PCENT_PIVOT.
Remarque : on a le droit de dépasser la valeur 100.
"""),

54: _(u"""
Solveur MUMPS :
  Le solveur MUMPS manque de mémoire lors de la factorisation de la matrice.

Solution :
  Il faut augmenter la mémoire accessible à MUMPS (et autres programmes hors fortran de Code_Aster).
  Pour cela, il faut diminuer la mémoire donnée à JEVEUX (ASTK : case "dont Aster (Mo)") ou bien
  augmenter la mémoire totale (ASTK : case "Mémoire totale (Mo))".
"""),

55: _(u"""
Solveur MUMPS :
  Problème ou alarme dans le solveur MUMPS.
  Le code retour de MUMPS (INFOG(1)) est : %(i1)d
Solution :
  Consulter le manuel d'utilisation de MUMPS.
  Prévenir l'équipe de développement de Code_Aster.
"""),

56: _(u"""
Solveur MUMPS :
  Il ne faut pas utiliser TYPE_RESOL = '%(k1)s'
  Pour une matrice non-symétrique.
Solution :
  Il faut utiliser TYPE_RESOL = 'NONSYM' (ou 'AUTO').
"""),

57: _(u"""
Solveur MUMPS :
  La solution du système linéaire est trop imprécise :
  Erreur calculée   : %(r1)g
  Erreur acceptable : %(r2)g   (RESI_RELA)
Solution :
  On peut augmenter la valeur du mot clé SOLVEUR/RESI_RELA.
"""),

59: _(u"""
Solveur MUMPS :
  La matrice est déjà factorisée. On ne fait rien.
Solution :
  Il y a sans doute une erreur de programmation.
  Contactez l'assistance.
"""),

60: _(u"""
Solveur MUMPS :
  Limite atteinte : le solveur MUMPS est utilisé par plus de 5 matrices simultanément.
Solution :
  Contactez l'assistance.
"""),

61: _(u"""
Erreur Programmeur lors de la résolution d'un système linéaire :
 La numérotation des inconnues est incohérente entre la matrice et le second membre.
 Matrice       : %(k1)s
 Second membre : %(k2)s

 Si solveur : 'FETI' : numéro du sous-domaine (ou domaine global) : %(i1)d
"""),

62: _(u"""
Alarme Solveur MUMPS :
  La procédure de raffinement itératif aurait besoin de plus que les %(i1)d d'itérations
  imposées en dur dans l'appel MUMPS par Code_Aster.
Solution :
  On peut essayer la valeur du mot-clé POSTTRAITEMENTS='FORCE'.
"""),

64: _(u"""
Solveur MUMPS :
  Le solveur MUMPS manque de mémoire lors de la phase d'analyse de la matrice.

Solution :
  Il faut augmenter la mémoire accessible à MUMPS (et autres programmes hors fortran de Code_Aster).
  Pour cela, il faut diminuer la mémoire donnée à JEVEUX (ASTK : case "dont Aster (Mo)") ou bien
  augmenter la mémoire totale (ASTK : case "Mémoire totale (Mo))".
"""),

65: _(u"""
Solveur MUMPS :
  MUMPS ne peut pas factoriser la matrice à cause d'un dépassement d'entiers.

Solution :
  Si vous utilisez la version séquentielle, alors il vous faut passer à la version parallèle.
  Si vous utilisez déjà la version parallèle, alors il faut augmenter le nombre de processeurs
  alloués au calcul.
"""),

66: _(u"""
Solveur MUMPS :
  Échec de la factorisation OUT-OF-CORE de MUMPS.
  Consulter les  messages délivrés  par MUMPS.
Conseil: Augmenter  le nombre de processeurs utilisés.
"""),

67: _(u"""
Erreur d'utilisation (commande RESOUDRE) :
  La matrice et le second membre fournis à la commande RESOUDRE
  ne sont pas de même dimension (nombre de ddls).
Conseil: Vérifier la cohérence des arguments MATR et CHAM_NO.
"""),

68: _(u"""
Erreur d'utilisation (commande RESOUDRE) :
  La matrice et le second membre fournis à la commande RESOUDRE
  ne sont pas du même type (réel/complexe).
Conseil: Vérifier la cohérence des arguments MATR et CHAM_NO.
"""),

70: _(u"""
Solveur MUMPS :
  Vous avez activé l'option IMPR='OUI_SOLVE' en surchargeant AMUMPS.F. La résolution
  du système linéaire en cours va donc s'effectuer normalement mais en plus
  sa matrice et son second membre vont être écrits dans le fichier d'unité logique
  %(i1)d. Vous pouvez le récupérer (sur le processeur 0) via ASTK.
"""),

71: _(u"""
Solveur MUMPS :
  Vous avez activé l'option IMPR='OUI_NOSOLVE' en surchargeant AMUMPS.F. La résolution
  du système linéaire en cours ne va donc pas s'effectuer mais sa matrice et
  son second membre vont être écrits dans le fichier d'unité logique %(i1)d.
  Après cette écriture, l'exécution Aster s'arrête en ERREUR_FATALE pour vous
  permettre de récupérer plus rapidement votre fichier.
  Vous pouvez le récupérer (sur le processeur 0) via ASTK.
"""),

72: _(u"""
Solveur MUMPS :
  Vous utilisez une version de MUMPS antérieure à la 4.7.3: la %(k1)s.
  Celle-ci n'est plus supportée pour le couplage Code_Aster/MUMPS.
Solution:
  Télécharger et installer une version de MUMPS plus récente.
"""),

73: _(u"""
Solveur MUMPS :
  Lors de la factorisation numérique, le pourcentage de pivots, %(r1).0f %%, a dépassé le
  pourcentage prévu par le paramètre SOLVEUR/PCENT_PIVOT= %(r2).0f %%.
  Cela peut engendrer un résultat de mauvaise qualité. Vérifiez bien la qualité de celui-ci
  en fin de résolution via la mot-clé RESI_RELA.
Solution:
  Pour améliorer la qualité de la solution vous pouvez activez les options de pré et
  post-traitements (PRETRAITEMENTS='AUTO' et POSTTRAITEMENTS='FORCE' ou 'AUTO'), durcir le critère
  de qualité RESI_RELA ou, si vous avez beaucoup de Lagrange (>10%% de la taille du problème),
  désactivez l'option ELIM_LAGR2 (ELIM_LAGR2='NON').
  Sinon, contactez l'équipe de développement.
"""),

74: _(u"""
Solveur MUMPS :
  Vous utilisez une version de MUMPS antérieure à la 4.8.4: la %(k1)s.
  Celle-ci ne permet pas la détection de singularité. On désactive cette fonctionnalité avec
  une valeur SOLVEUR/NPREC négative.
Attention:
  Cette désactivation peut nuire à certains type de calculs (modal, option CRIT_FLAMB...).
"""),




80: _(u"""
(solveur linéaire MUMPS) Problème de paramétrage du solveur !

  Attention, vous avez paramétré le solveur linéaire MUMPS de manière a résoudre un système
  linéaire SPD (réel Symétrique Défini Positif): mot-clé SOLVEUR/TYPE_RESOL='SYMDEF'. Or votre
  matrice est a valeur complexe. Ceci est contradictoire.

    -> Conseil & Risque :
      Utilisez le solveur linéaire MUMPS avec TYPE_RESOL='AUTO'.
"""),



84: _(u"""
(solveur linéaire MUMPS) Problème de paramétrage du solveur !

Attention, vous avez paramètre le solveur linéaire MUMPS de manière a résoudre un système
linéaire SPD (réel Symétrique Défini Positif): mot-clé SOLVEUR/TYPE_RESOL='SYMDEF'. Or votre
matrice comporte des termes négatifs ou nuls sur sa diagonale. Ceci est contradictoire.

    -> Conseil & Risque :
      Si il s'agit d'un test vous voila averti, sinon utilisez le solveur linéaire MUMPS
      avec TYPE_RESOL='AUTO'.
"""),
}
