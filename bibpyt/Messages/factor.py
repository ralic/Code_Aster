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
# person_in_charge: josselin.delmas at edf.fr

cata_msg={

1: _(u"""
Le système linéaire à résoudre a %(i1)d degrés de liberté:
   - %(i2)d sont des degrés de liberté physiques 
     (ils sont portés par %(i3)d noeuds du maillage)
   - %(i4)d sont les couples de paramètres de Lagrange associés 
     aux %(i5)d relations linéaires dualisées.
"""),

2: _(u"""
La matrice est de taille %(i1)d équations.
Elle contient %(i2)d termes non nuls si elle est symétrique et %(i3)d termes non nuls si elle n'est pas symétrique (le nombre de termes non nuls est susceptible de varier si l'on utilise le contact en formulation continue ou la méthode XFEM avec contact).
Soit un taux de remplissage de  %(r1)6.3f %%.
"""),


10: _(u"""
Problème : la matrice est singulière ou presque singulière :
  Lors de la factorisation de la matrice, on a rencontré un problème
  (pivot nul ou presque nul) à la ligne %(i1)d
  Le degré de liberté associé à cette ligne est de type : (%(k4)s)
  Le nombre de décimales "perdues" pour cette ligne est : %(i2)d.
  Ce nombre de décimales perdues est lié au degré de singularité de la matrice. 
  Plus il est grand, plus le système est singulier. 
  Quand on a perdu plus de 8 décimales, on estime que l'on a perdu 50%% de la précision
  des nombres réels (qui ont 15 décimales environ).

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






42: _(u"""
Solveur MUMPS :
  La Matrice est non factorisable. Elle est détectée comme étant singulière
  (en structure ou numériquement).

Conseils :
  Il peut s'agir d'une erreur de programmation ou d'un problème de mise en données (blocage
  absent ou surabondant).
"""),


50: _(u"""
Solveur MUMPS :
   Vous avez demandé comme numéroteur RENUM = '%(k1)s', or MUMPS en a
   utilisé un autre.

Conseils :
   Il se peut que votre version de MUMPS n'ait pas été compilée avec le support de ce numéroteur.
   Dans le doute, RENUM='AUTO' permet de laisser MUMPS faire le meilleur choix.
 """),

51: _(u"""
Solveur MUMPS :
  JEVEUX a déchargé sur disque le maximum d'objets possibles afin de laisser de la
  place à MUMPS. Il a ainsi gagné %(i1)d Mo. Mais ce déchargement peut ralentir le calcul.

Conseils :
  La prochaine fois, relancez avec plus de mémoire globale ou avec une option de calcul plus économe
  (par ordre décroissant d'efficacité):
    - Si le calcul est parallèle, réduisez la consommation MUMPS en augmentant le nombre de processeurs,
    - Dans tous les cas, réduisez la consommation MUMPS en activant l'option OUT_OF_CORE (SOLVEUR/GESTION_MEMOIRE='OUT_OF_CORE'),
    - Si le calcul est parallèle, réduisez la consommation JEVEUX en activant l'option SOLVEUR/MATR_DISTRIBUEE.
"""),

52: _(u"""
  -> Vous avez demandé une analyse de stabilité et vous utilisez le solveur linéaire '%(k1)s'.
     Ces deux fonctionnalités ne sont pas compatibles.

  -> Conseil :
     Changez de solveur linéaire en utilisant le mot-clé METHODE de SOLVEUR.
"""),

53: _(u"""
Solveur MUMPS :
  MUMPS a manqué de mémoire lors de la factorisation de la matrice à cause du pivotage.
  L'espace supplémentaire requis par ce pivotage est difficilement prévisible a priori.
  Il est contrôlé par le paramètre SOLVEUR/PCENT_PIVOT.
  MUMPS a essayé, sans succès, plusieurs valeurs de ce paramètre:
                 partant de %(i2)d %% pour finir à %(i3)d %%.
Conseils :
  - Augmenter la valeur du mot clé  SOLVEUR/PCENT_PIVOT. On a le droit de dépasser la valeur 100
    (surtout sur les petits systèmes),
  - Passer en mode de gestion mémoire automatique (GESTION_MEMOIRE='AUTO'),
  - En dernier ressort, passer en mode OUT_OF_CORE (GESTION_MEMOIRE='OUT_OF_CORE') ou augmenter la mémoire
    dévolue au calcul ou le nombre de processeurs (si le calcul est parallèle).
"""),

54: _(u"""
Solveur MUMPS :
  Le solveur MUMPS manque de mémoire lors de la factorisation de la matrice.

Conseils :
  Il faut augmenter la mémoire accessible à MUMPS (et autres programmes hors fortran de Code_Aster).
  Pour cela, il faut diminuer la mémoire donnée à JEVEUX (ASTK : case "dont Aster (Mo)") ou bien
  augmenter la mémoire totale (ASTK : case "Mémoire totale (Mo))".
"""),

55: _(u"""
Solveur MUMPS :
  Problème ou alarme dans le solveur MUMPS.
  Le code retour de MUMPS (INFOG(1)) est : %(i1)d

Conseils :
  Consulter le manuel d'utilisation de MUMPS.
  Prévenir l'équipe de développement de Code_Aster.
"""),

56: _(u"""
Solveur MUMPS :
  Il ne faut pas utiliser TYPE_RESOL = '%(k1)s'
  Pour une matrice non-symétrique.

Conseils :
  Il faut utiliser TYPE_RESOL = 'NONSYM' (ou 'AUTO').
"""),

57: _(u"""
Solveur MUMPS :
  La solution du système linéaire est trop imprécise :
  Erreur calculée   : %(r1)g
  Erreur acceptable : %(r2)g   (RESI_RELA)

Conseils :
  On peut augmenter la valeur du mot clé SOLVEUR/RESI_RELA.
"""),

58: _(u"""
Solveur MUMPS :
  MUMPS manque de mémoire lors de la factorisation de la matrice à cause du pivotage.
  L'espace supplémentaire requis par ce pivotage est difficilement prévisible a priori.
  MUMPS vient d'échouer avec une valeur de cet espace supplémentaire égale à:
                               SOLVEUR/PCENT_PIVOT=%(i1)d %%.
  On va réessayer avec une valeur plus importante: %(i2)d %%.
  La prochaine fois, relancer votre calcul en prenant ces nouvelles valeurs du paramétrage.
  C'est la tentative n %(i3)d de factorisation !

Attention :
  Ce procédé automatique de correction est limité à %(i4)d tentatives !
  Cette nouvelle valeur SOLVEUR/PCENT_PIVOT devient la valeur par défaut pour les
  futures résolutions de systèmes linéaires avec MUMPS dans cet opérateur.
"""),

59: _(u"""
Solveur MUMPS :
  La matrice est déjà factorisée. On ne fait rien.

Conseils :
  Il y a sans doute une erreur de programmation.
  Contactez l'assistance.
"""),

60: _(u"""
Solveur MUMPS :
  Limite atteinte : le solveur MUMPS est utilisé par plus de 5 matrices simultanément.

Conseils :
  Contactez l'assistance.
"""),









62: _(u"""
Solveur MUMPS :
  La procédure de raffinement itératif aurait besoin de plus que les %(i1)d d'itérations
  imposées en dur dans l'appel MUMPS par Code_Aster.

Conseils :
  On peut essayer la valeur du mot-clé POSTTRAITEMENTS='FORCE'.
"""),

64: _(u"""
Solveur MUMPS :
  Le solveur MUMPS manque de mémoire lors de la phase d'analyse de la matrice.

Conseils :
  Il faut augmenter la mémoire accessible à MUMPS (et autres programmes hors fortran de Code_Aster).
  Pour cela, il faut diminuer la mémoire donnée à JEVEUX (ASTK : case "dont Aster (Mo)") ou bien
  augmenter la mémoire totale (ASTK : case "Mémoire totale (Mo))".
"""),

65: _(u"""
Solveur MUMPS :
  MUMPS ne peut pas factoriser la matrice à cause d'un dépassement d'entiers.

Conseils :
  Si vous utilisez la version séquentielle, alors il vous faut passer à la version parallèle.
  Si vous utilisez déjà la version parallèle, alors il faut augmenter le nombre de processeurs
  alloués au calcul.
"""),

66: _(u"""
Solveur MUMPS :
  Échec de la factorisation OUT_OF_CORE de MUMPS.
  Consulter les  messages délivrés  par MUMPS.

Conseils :
   Augmenter  le nombre de processeurs utilisés.
"""),

67: _(u"""
Erreur d'utilisation (commande RESOUDRE) :
  La matrice et le second membre fournis à la commande RESOUDRE
  ne sont pas de même dimension (nombre de ddls).

Conseils :
  Vérifier la cohérence des arguments MATR et CHAM_NO.
"""),

68: _(u"""
Erreur d'utilisation (commande RESOUDRE) :
  La matrice et le second membre fournis à la commande RESOUDRE
  ne sont pas du même type (réel/complexe).

Conseils :
  Vérifier la cohérence des arguments MATR et CHAM_NO.
"""),


70: _(u"""
Solveur MUMPS :
  Vous avez activé l'option IMPR="OUI_SOLVE" en surchargeant AMUMPS.F. La résolution
  du système linéaire en cours va donc s'effectuer normalement mais en plus
  sa matrice et son second membre vont être écrits dans le fichier d'unité logique
  %(i1)d. Vous pouvez le récupérer (sur le processeur 0) via ASTK.
"""),

71: _(u"""
Solveur MUMPS :
  Vous avez activé l'option IMPR="OUI_NOSOLVE" en surchargeant AMUMPS.F. La résolution
  du système linéaire en cours ne va donc pas s'effectuer mais sa matrice et
  son second membre vont être écrits dans le fichier d'unité logique %(i1)d.
  Après cette écriture, l'exécution Aster s'arrête en ERREUR_FATALE pour vous
  permettre de récupérer plus rapidement votre fichier.
  Vous pouvez le récupérer (sur le processeur 0) via ASTK.
"""),

72: _(u"""
Solveur MUMPS :
  Votre exécutable Aster embarque la version de MUMPS: %(k1)s
  Les seules versions de MUMPS supportée dans Code_Aster sont, pour l'instant:
                              la 4.9.2 et la 4.10.0.

Conseils :
  Télécharger, installer et relier à Code_Aster une version de MUMPS adéquate.
  Utiliser un autre solveur linéaire (mot-clé SOLVEUR/METHODE, par exemple "MULT_FRONT" ou "PETSC")
  Sinon, contactez l'équipe de développement.
"""),

73: _(u"""
Solveur MUMPS :
  Lors de la factorisation numérique, le pourcentage de pivots, %(r1).0f %%, a dépassé le
  pourcentage prévu par le paramètre SOLVEUR/PCENT_PIVOT= %(r2).0f %%.
  Cela peut engendrer un résultat de mauvaise qualité. Vérifiez bien la qualité de celui-ci
  en fin de résolution via la mot-clé RESI_RELA.

Conseils :
  Pour améliorer la qualité de la solution vous pouvez activez les options de pré et
  post-traitements (PRETRAITEMENTS='AUTO' et POSTTRAITEMENTS='FORCE' ou 'AUTO'), durcir le critère
  de qualité RESI_RELA ou, si vous avez beaucoup de Lagrange (>10%% de la taille du problème),
  désactivez l'option ELIM_LAGR='LAGR2' (ELIM_LAGR='NON').
  Sinon, contactez l'équipe de développement.
"""),

74: _(u"""
Solveur MUMPS :
  Attention, vous avez demandé un calcul IN_CORE, mais MUMPS estime avoir besoin pour cela de
  %(i1)d Mo (avec %(i2)d %% de marge), alors qu'il n'y a que %(i3)d Mo de disponible sur ce processeur !
  Le calcul pourrait donc être ralenti ou achopper pour cette raison.
  Nombre de systèmes linéaires à factoriser en même temps: %(i4)d.

Conseils :
  La prochaine fois, relancez avec plus de mémoire globale ou avec une option de calcul plus économe
  (par ordre décroissant d'efficacité):
    - Si le calcul est parallèle, réduisez la consommation MUMPS en augmentant le nombre de processeurs,
    - Dans tous les cas, réduisez la consommation MUMPS en activant l'option OUT_OF_CORE (SOLVEUR/GESTION_MEMOIRE='OUT_OF_CORE')
      ou passer en mode automatique (SOLVEUR/GESTION_MEMOIRE='AUTO'),
    - Si le calcul est parallèle, réduisez la consommation Aster en activant l'option SOLVEUR/MATR_DISTRIBUEE.
"""),

75: _(u"""
Solveur MUMPS :
  Attention, vous avez demandé un calcul OUT_OF_CORE, mais MUMPS estime avoir besoin pour cela de
  %(i1)d Mo (avec %(i2)d %% de marge), alors qu'il n'y a que %(i3)d Mo de disponible sur ce processeur !
  Le calcul pourrait donc être ralenti ou achopper pour cette raison.
  Nombre de systèmes linéaires à factoriser en même temps: %(i4)d.

Conseils :
  La prochaine fois, relancez avec plus de mémoire globale ou avec une option de calcul plus économe
  (par ordre décroissant d'efficacité):
   - Si le calcul est parallèle, réduisez la consommation MUMPS en augmentant le nombre de processeurs,
   - Si le calcul est parallèle, réduisez la consommation Aster en activant l'option SOLVEUR/MATR_DISTRIBUEE,
  Ou, plus radicalement:
   - Utilisez un solveur moins exigeant en mémoire (par exemple: SOLVEUR/METHODE='PETSC' + PRECOND='LDLT_SP').
"""),

76: _(u"""
Solveur MUMPS :
  Pour essayer de passer en mémoire, il a fallu décharger sur disque un maximum d'objets JEVEUX.
  Ce déchargement a pu ralentir le calcul.
  La mémoire disponible est ainsi passée de %(i1)d Mo à %(i2)d Mo. Mais cela n'a pas été suffisant car
  MUMPS a besoin, suivant son mode de gestion mémoire (paramétré par le mot-clé SOLVEUR/GESTION_MEMOIRE),
  d'au moins:
     - IN_CORE    : %(i3)d Mo,
     - OUT_OF_CORE: %(i4)d Mo.
  Estimations à %(i5)d %% de marge près.
  Nombre de systèmes linéaires à factoriser en même temps: %(i6)d.

Conseils :
  La prochaine fois, relancez avec plus de mémoire globale ou avec une option de calcul
  plus économe (par ordre décroissant d'efficacité):
   - Si le calcul est parallèle, réduisez la consommation MUMPS en augmentant le nombre de processeurs ou
     réduisez celle de JEVEUX en activant l'option MATR_DISTRIBUEE.
  Ou, plus radicalement:
   - Utilisez un solveur moins exigeant en mémoire (par exemple: SOLVEUR/METHODE='PETSC' + PRECOND='LDLT_SP').
"""),

77: _(u"""
Solveur MUMPS :
  Vous avez demandé les estimations mémoire (mémoire et disque, Aster et MUMPS) de votre calcul.
  Une fois ces estimations affichées (sur le processeur 0), l'exécution Aster s'arrête en
  ERREUR_FATALE pour vous permettre de relancer votre calcul en tenant compte de ces éléments.
"""),

78: _(u"""
Solveur MUMPS :
  Dépassement de capacité des termes de la matrice. On a détecté au moins:
    - %(i1)d termes trop petits,
    - %(i2)d termes trop grands.
"""),

79: _(u"""
Solveur MUMPS :
  Dépassement de capacité du terme n %(i1)d du second membre. Sa valeur absolue vaut %(r1)g alors que la
  limite est fixée à %(r2)g.
"""),

80: _(u"""
Solveur MUMPS :

  Attention, vous avez paramétré le solveur linéaire de manière a résoudre un système linéaire
  SPD (réel Symétrique Défini Positif): mot-clé SOLVEUR/TYPE_RESOL='SYMDEF'. Or votre système
  linéaire à valeur complexe. Ceci est contradictoire.

Conseil :
  Utilisez le solveur linéaire MUMPS avec TYPE_RESOL='AUTO'.
"""),

81: _(u"""

- Taille du système linéaire: %(i1)d
- Nombre de systèmes linéaires à factoriser en même temps: %(i8)d.

- Mémoire minimale consommée par Code_Aster (JEVEUX, Superviseur, Python...) : %(i2)d Mo
- Estimation de la mémoire MUMPS avec GESTION_MEMOIRE='IN_CORE'              : %(i3)d Mo
- Estimation de la mémoire MUMPS avec GESTION_MEMOIRE='OUT_OF_CORE'          : %(i4)d Mo
- Estimation de l'espace disque pour MUMPS avec GESTION_MEMOIRE='OUT_OF_CORE': %(i5)d Mo

  > Pour ce calcul, il faudrait donc une quantité de mémoire au minimum de
        - %(i6)d Mo si GESTION_MEMOIRE='IN_CORE',
        - %(i7)d Mo si GESTION_MEMOIRE='OUT_OF_CORE'.
  En cas de doute, utilisez GESTION_MEMOIRE='AUTO'.

"""),
82: _(u"""
Solveur MUMPS :
  La machine sur laquelle ce calcul a été lance ne permet pas l'évaluation du pic mémoire
  'VmPeak'. En mode SOLVEUR/GESTION_MEMOIRE='AUTO' cela peut fausser les évaluations
  de mémoire disponibles.
  > Par précaution on bascule automatiquement en mode GESTION_MEMOIRE='OUT_OF_CORE'.

Conseils :
  Pour accélérer le calcul vous pouvez (conseils cumulatifs):
    - passer en mode IN_CORE (GESTION_MEMOIRE='IN_CORE'),
    - lancer le calcul en parallèle,
    - changer de machine en laissant le mode GESTION_MEMOIRE='AUTO'.
"""),
83: _(u"""
Solveur MUMPS :
  La machine sur laquelle ce calcul a été lancé ne permet pas l'évaluation du pic mémoire
  'VmPeak'. En mode SOLVEUR/GESTION_MEMOIRE='EVAL' cela peut fausser la calibration
  de la mémoire minimale consommée par Code_Aster.

Conseil :
  Pour lancer le calcul, il faut donc se fier plutôt aux estimations mémoire MUMPS.
"""),
84: _(u"""
Solveur MUMPS :

  Attention, vous avez paramétré le solveur linéaire MUMPS de manière a résoudre un système
  linéaire SPD (réel Symétrique Défini Positif): mot-clé SOLVEUR/TYPE_RESOL='SYMDEF'. Or votre
  matrice comporte des termes négatifs ou nuls sur sa diagonale. Ceci est contradictoire.

Conseil :
  Si il s'agit d'un test vous voila averti, sinon utilisez le solveur linéaire MUMPS
  avec TYPE_RESOL='AUTO'.
"""),

85: _(u"""
Solveur MUMPS :
  MUMPS essaye d'allouer %(i1)d Mo de mémoire mais il n'y arrive pas. Il se base
  sur une estimation de la mémoire disponible et celle-ci est sans doute faussée. Cela peut
   arriver, par exemple, sur d'anciens noyaux système.

  > Par précaution, on va réessayer en laissant complètement la main à MUMPS et en passant en
    gestion mémoire OUT_OF_CORE (SOLVEUR/GESTION_MEMOIRE='OUT_OF_CORE'). Le calcul sera
    potentiellement plus lent, mais cela gommera ces problèmes d'estimations mémoire préalables tout
    en économisant au maximum la consommation mémoire de MUMPS.

Conseil :
  Contactez l'équipe de développement.

"""),
86: _(u"""
Solveur MUMPS :
  Un opérateur a demandé à MUMPS de ne pas conserver les termes de la matrice factorisée.
  Cela permet d'optimiser un peu les performances dans les cas ou seul le déterminant, la
  détection de singularité ou le test de Sturm sont requis (par ex. MODE_ITER_SIMULT option 'BANDE').
  Or la version de MUMPS embarquée dans votre exécutable: %(k1)s
  ne permet pas cette optimisation.

  > On ne tient donc pas compte de cette optimisation et on stocke, comme pour un calcul standard,
    tous les termes de la factorisée.

"""),
87: _(u"""
Solveur MUMPS :
  Un opérateur a demandé à MUMPS de calculer un déterminant. Or cette option n'est disponible
  qu'a partir de la v 4.10.0. Or votre exécutable est lié a MUMPS v: %(k1)s.

  Conseils:
    - Reconstruisez un exécutable en prenant une bonne version de MUMPS,
    - Changer de solveur linéaire (par exemple, mot-clé SOLVEUR/METHODE='MULT_FRONT') dans l'opérateur
      incriminé.

"""),
88: _(u"""
Solveur MUMPS :
  Un opérateur a demandé à MUMPS de calculer un déterminant. Pour ne pas fausser ce calcul on a
  débranché automatiquement l'option SOLVEUR/ELIM_LAGR : 'LAGR2' -> 'NON'
Attention :
  Cette nouvelle valeur de SOLVEUR/ELIM_LAGR devient la valeur par défaut pour les
  futures résolutions de systèmes linéaires avec MUMPS dans cet opérateur.

"""),
}
