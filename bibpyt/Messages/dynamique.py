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

    1: _(u"""
 schéma inconnu
"""),

    2: _(u"""
 la liste d'instants fournie ne respecte pas la condition de stabilité.
"""),

    3: _(u"""
 la condition de stabilité n'a pas pu être calculée pour tous les éléments. elle peut être trop grande.
"""),

    4: _(u"""
  -> La condition de stabilité n'a pu être calculée pour aucun élément.
  -> Risque & Conseil :
     Vous prenez le risque de sortir du cadre de la stabilité conditionnelle du schéma de temps explicite. Vérifiez bien
     que vos éléments finis ont une taille et un matériau (module de Young) compatibles avec le respect de la condition
     de Courant vis-à-vis du pas de temps que vous avez imposé (temps de propagation des ondes dans la maille, voir
     documentation). Si c'est le cas, lever l'arrêt fatal en utilisant l'option "STOP_CFL", à vos risques et périls
     (risques de résultats faux).
"""),

    5: _(u"""
 Pas de temps maximal (condition CFL) pour le schéma des différences centrées : %(r1)g s, sur la maille : %(k1)s
"""),

    6: _(u"""
  Pas de temps maximal (condition CFL) pour le schéma de Tchamwa-Wilgosz : %(r1)g s, sur la maille : %(k1)s
"""),

    7: _(u"""
 Pas de temps maximal (condition CFL) pour le schéma des différences centrées : %(r1)g s
"""),

    8: _(u"""
  Pas de temps maximal (condition CFL) pour le schéma de Tchamwa-Wilgosz : %(r1)g s
"""),

    9: _(u"""
  On ne peut pas avoir plus d'une charge de type FORCE_SOL.
"""),

    10: _(u"""
   Arrêt par manque de temps CPU au groupe de pas de temps : %(i1)d
                                 au "petit" pas de temps   : %(i2)d
      - Temps moyen par "petit" pas : %(r1)f
      - Temps restant               : %(r2)f

   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    11: _(u"""
   Arrêt par manque de temps CPU après le calcul de %(i1)d pas.
      - Dernier instant archivé : %(r1)f
      - Numéro d'ordre correspondant : %(i2)d
      - Temps moyen pour les %(i3)d pas de temps : %(r2)f
      - Temps restant                : %(r3)f

   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    12: _(u"""
 Dans l'intervalle : %(i2)d
 Le pas de temps est trop grand : %(r1)f
 le pas de temps maximal est    : %(r2)f

 Avec le pas de temps maximal, le nombre de pas de calcul est %(i1)d
"""),

    13: _(u"""
   Arrêt par manque de temps CPU à la fréquence : %(i1)d
      - Temps moyen par pas fréquence : %(r1)f
      - Temps restant                 : %(r2)f

   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    14: _(u"""
   La matrice est presque singulière à la fréquence : %(r1)f
   Cette fréquence est probablement une fréquence propre du système.
"""),

    15 : _(u"""
 Pas de temps maximal (mot-clé PAS_MAXI) demandé : %(r1)f plus petit que
 le pas de temps initial demandé par l'utilisateur (mot-clé PAS) : %(r2)f
 Il faut s'assurer que PAS est bien inférieur ou égal à PAS_MAXI
"""),

    16 : _(u"""
 Pas de temps maximal calculé pour le schéma ADAPT : %(r1)f

 Risque & Conseil : la méthode de calcul automatique de ce pas maximal semble être prise en défaut.
 On recommande donc de définir explicitement cette valeur avec le mot-clé PAS_MAXI (sous INCREMENT).
"""),

    17 : _(u"""
 Pas de temps maximal (mot-clé PAS_MAXI) demandé trop grand :   %(r1)f
 Pas de temps nécessaire pour le calcul: %(r2)f
 Risques de problèmes de précision
"""),

    18 : _(u"""
 Le nombre maximal de sous division du pas : %(i1)d est atteint à l'instant : %(r1)f
 Le pas de temps vaut alors : %(r2)f
 On continue cependant la résolution en passant au pas suivant.

 Risque & Conseil : la solution calculée risque d'être imprécise.
 Il faudrait relancer la calcul en autorisant le schéma ADAPT à utiliser un pas de temps plus petit.
 Pour cela on peut jouer sur au moins un des trois paramètres suivants :
 - diminuer le pas de temps initial (mot-clé PAS),
 - augmenter le nombre maximal de sous découpages du pas (mot-clé NMAX_ITER_PAS),
 - augmenter le facteur de division du pas (mot-clé COEF_DIVI_PAS)
"""),

    19 : _(u"""
 Le chargement contient plus d'une charge répartie.
 Le calcul n'est pas possible pour les modèles de poutre.
"""),

    20 : _(u"""
 La fréquence d'actualisation de FORCE_SOL est prise dans le fichier des raideurs.
"""),

    21 : _(u"""
 La fréquence d'actualisation de FORCE_SOL est prise dans le fichier des masses.
"""),

    22 : _(u"""
 La fréquence d'actualisation de FORCE_SOL est prise dans le fichier des amortissements.
"""),

    23 : _(u"""
    Nombre de fréquences: %(i1)d
    Intervalle des fréquences: %(r1)f
"""),

    25 : _(u"""
 La fréquence d'actualisation de FORCE_SOL n'est pas cohérente avec la fréquence d'archivage des résultats dans
 DYNA_NON_LINE.
"""),

    26 : _(u"""
 Deux des fréquences %(r1)f Hz et %(r2)f HZ de la liste LIST_RAFFINE sont proches.
Les intervalles de raffinement entourant ces deux valeurs se chevauchent.
Si une valeur du premier intervalle est trop proche d'une valeur du deuxième
intervalle (écart inférieur à PAS_MINI), l'une des deux sera supprimée de la liste.
"""),

    27 : _(u"""
 L'écart entre les fréquences %(r1)f Hz et %(r2)f Hz est inférieur à PAS_MINI. Toutefois on conserve
ces deux valeurs car l'une d'elles peut correspondre à une fréquence de résonance"
"""),

    28 : _(u"""
 Le nombre d'obstacles de choc est limité à %(i1)d en cas de traitement implicite des non-linéarités dans
 DYNA_VIBRA.
 Conseil : si la modélisation ne permet pas de réduire le nombre de lieux de choc, il faudrait
 repasser le calcul mais avec un traitement explicite des chocs.
"""),

    29 : _(u"""
 La matrice d'amortissement n'est pas diagonale. Or, la méthode ITMI ne permet pas de modéliser
 de couplage dynamique par l'amortissement. Les termes diagonaux seront alors extraits pour la suite
 du calcul.
"""),

    30 : _(u"""
 La fréquence d'actualisation de FORCE_SOL dans le fichier des masses est incohérente avec
celle choisie précédemment.
"""),

    31 : _(u"""
 La fréquence d'actualisation de FORCE_SOL dans le fichier des amortissements est incohérente avec
celle choisie précédemment.
"""),

    32 : _(u"""
La condition de stabilité n'a pas pu être calculée car il s'agit d'élasticité non-isotrope.
"""),

    33 : _(u"""
Il y a une incohérence dans les type de résultats, le résultat selon X n'est pas le même que celui selon Y.
"""),

    34 : _(u"""
Il y a une incohérence dans les type de résultats, le résultat selon X et Y n'est pas le même que celui selon Z.
"""),

    35 : _(u"""
Il semble que vos calculs dynamiques ont été réalisés sur des listes de fréquences ou d'instants différentes.
L'utilisation de la macro-commande nécessite d'avoir réalisé les calculs dynamiques sur une unique liste de fréquences ou d'instants.
"""),

    36 : _(u"""
Les signaux d'entraînements ont une fréquence finale inférieure à celle du calcul dynamique.
"""),

    37 : _(u"""
Les signaux d'entraînements ont un instant final inférieur à celui du calcul dynamique.
"""),

    38 : _(u"""
Les signaux servant de supports à la détermination des signaux d'entraînement ont une fréquence finale inférieure à celle du calcul dynamique.
"""),

    39 : _(u"""
Les signaux servant de supports à la détermination des signaux d'entraînement ont un instant final inférieur à  celui du calcul dynamique.
"""),

    40 : _(u"""
Les signaux d'entraînements ne sont pas discrétisés de la même manière. Vérifier le pas de chaque signaux ainsi que leur longueur.
"""),

    41 : _(u"""
Les signaux servant de supports à la détermination des signaux d'entraînement ne sont pas discrétisés de la même manière. Vérifier le pas de chaque signaux ainsi que leur longueur.
"""),

    50 : _(u"""
Schéma multi-pas
On n'a pas trouvé l'instant précédent dans la structure de données résultat du mot-clef ETAT_INIT.
C'est probablement parce qu'il n'y a pas assez d'instants archivés.
On ignore donc le calcul du second membre pour cet instant.
"""),

    51 : _(u"""
Schéma multi-pas
On n'a pas trouvé l'instant précédent dans la structure de données résultat du mot-clef ETAT_INIT.
C'est probablement parce que la structure de données vient d'un calcul statique (STAT_NON_LINE) ou d'une lecture directe (LIRE_RESU).
On ignore donc le calcul du second membre pour cet instant.
"""),

    52 : _(u"""
Schéma multi-pas
L'instant précédent et l'instant initial sont presque confondus.
On ignore donc le calcul du second membre pour cet instant.
"""),

    53 : _(u"""
Schéma multi-pas
On n'a pas de structure de données résultat dans le mot-clef ETAT_INIT parce que l'état initial est entré champ par champ.
On ignore donc le calcul du second membre pour cet instant.
"""),

    55 : _(u"""
--------------------------------------------------------------------------------------

%(k1)s
=====================================================================================
                     Calcul %(k2)s sur base %(k3)s
=====================================================================================
"""),

    56 : _(u"""
Superposition modale classique
--------------------------------------------------------------------------------------
    >> base modale de projection : %(k1)s
    >> nombre de DDL avant projection (physiques) : %(i1)d"""),

    57 : _(u"""
Modèle de sous-structuration dynamique
--------------------------------------------------------------------------------------
    >> modèle généralisé : %(k1)s
    >> numérotation généralisée : %(k2)s"""),

    58 : _(u"""
Modèle sous interaction fluide-structure
--------------------------------------------------------------------------------------
    >> base de couplage fluide-élastique : %(k1)s
    >> vitesse du fluide  :%(r1)12.5e"""),

    59 : _(u"""    >> nombre de modes    : %(i1)d
    >> fréquence minimale :%(r1)12.5e
    >> fréquence maximale :%(r2)12.5e
"""),

    60 : _(u"""
Matrices dynamiques pour la résolution
--------------------------------------------------------------------------------------"""),

    61 : _(u"""    >> matrice de masse        : %(k1)s
    >> matrice de rigidité     : %(k2)s"""),

    62 : _(u"""    >> matrice d'amortissement : %(k1)s
"""),

    63 : _(u"""    >> amortissement modal diagonal
"""),

    64 : _(u"""    >> système conservatif, sans amortissement.
"""),

    65 : _(u"""    >> masse diagonale extraite de la base de couplage fluide-élastique
    >> rigidité diagonale extraite de la base de couplage fluide-élastique
    >> amortissement modal diagonal, extrait de la base de couplage fluide-élastique
"""),

    66 : _(u"""
Schéma d'intégration %(k1)s à pas adaptatif
--------------------------------------------------------------------------------------
    >> type de schéma                 : explicite
    >> pas d'intégration initial      :%(r1)12.5e
    >> pas d'intégration minimal      :%(r2)12.5e  (arrêt du calcul si inférieur)
    >> pas d'intégration maximal      :%(r3)12.5e  (plafond maximal du pas d'intégration)"""),

    67 : _(u"""    >> tolérance                      :%(r1)12.5e"""),

    68 : _(u"""    >> coefficient de division du pas :%(r1)12.5e"""),

    69 : _(u"""    >> nombre minimum de pas calculés : %(i1)d
    >> nombre maximum de pas calculés : %(i2)d"""),

    70 : _(u"""
Schéma d'intégration %(k1)s à pas constant
--------------------------------------------------------------------------------------
    >> type de schéma         : %(k2)s
    >> pas d'intégration      :%(r1)12.5e
    >> nombre de pas calculés : %(i1)d"""),

    71 : _(u"""
Non-linéarités localisées
--------------------------------------------------------------------------------------"""),

    72 : _(u"""    >> nombre de lieux de choc                  : %(i1)d
    >> méthode de traitement de chocs           : %(k1)s"""),

    73 : _(u"""    >> nombre de dispositifs anti-sismique      : %(i1)d"""),

    74 : _(u"""    >> nombre de lieux de choc avec flambement  : %(i1)d"""),

    75 : _(u"""    >> nombre de relations effort-déplacement   : %(i1)d"""),

    76 : _(u"""    >> nombre de relations effort-vitesse       : %(i1)d"""),

    77 : _(u"""    >> nombre de couplages dissipatifs visqueux : %(i1)d"""),

    78 : _(u"""
État initial
--------------------------------------------------------------------------------------
    >> extrait à partir du résultat : %(k1)s
"""),

    79 : _(u"""
État initial
--------------------------------------------------------------------------------------
    >> déplacement  : %(k1)s
    >> vitesse      : %(k2)s"""),

    80 : _(u"""
Durée de la simulation
--------------------------------------------------------------------------------------
    >> instant initial :%(r1)12.5e
    >> instant final   :%(r2)12.5e"""),

    81 : _(u"""
Archivage
--------------------------------------------------------------------------------------
    >> fréquence d'archivage         : tous les %(i1)d pas calculés
    >> instants d'archivage forcés   : instants initial et final, et %(i2)d autres instants
    >> champs archivés (généralisés) : DEPL, VITE, ACCE

======================================================================================

Avancement du calcul
--------------------------------------------------------------------------------------
"""),

    82 : _(u"""
Modèle physique
--------------------------------------------------------------------------------------
    >> modèle mécanique : %(k1)s
    >> nombre de DDL physiques : %(i1)d
"""),

    83 : _(u"""
La méthode intégrale (ITMI) est uniquement disponible si les matrices dynamiques sont
diagonales. Vérifiez que le stockage diagonal a été choisi lors de la numérotation des
DDL généralisés.
"""),
    84 : _(u"""    >> accélération : %(k1)s"""),

    85 : _(u"""
Archivage
--------------------------------------------------------------------------------------
    >> fréquence d'archivage : tous les %(i1)d pas calculés"""),

    86 : _(u"""
Archivage
--------------------------------------------------------------------------------------
    >> nombre d'instants d'archivage : %(i1)d instants"""),

    87 : _(u"""    >> matrice d'impédance     : %(k1)s"""),

    88 : _(u"""
Résolution
--------------------------------------------------------------------------------------
    >> fréquence minimale :%(r1)12.5e
    >> fréquence maximale :%(r2)12.5e
    >> nombre de fréquences calculées : %(i1)d

Archivage
--------------------------------------------------------------------------------------
    >> fréquences archivées : toutes (%(i1)d fréquences)
    >> champs archivés      : %(k1)s

======================================================================================

Avancement du calcul
--------------------------------------------------------------------------------------
"""),

    89 : _(u"""[%(i1)3d%%] Instant calculé :%(r1)12.5e, dernier instant archivé :%(r2)12.5e, au numéro d'ordre : %(i2)5d"""),

    90 : _(u"""
Entrée/Changement d'état de choc détecté à l'instant %(r1)12.5e
---------------------------------------------------------------------------------------------
    >> Descriptif de l'état :"""),

    91 : _(u"""           %(k1)s
          | Choc numéro | %(k2)s"""),


    92 : _(u"""          | État        %(k1)s
           %(k2)s"""),

    93 : _(u"""    >> Repassage en état de vol libre détecté à l'instant %(r1)12.5e

"""),

    94 : _(u"""    >> Premier passage dans cet état, mise à jour des matrices dynamiques et calcul d'une
       nouvelle base de modes propres :
       """),
    95 : _(u"""[%(i1)3d%%] Fréquence calculée :%(r1)12.5e, archivée au numéro d'ordre : %(i2)5d"""),

    96 : _(u"""    >> champs archivés       : %(k1)s

======================================================================================

Avancement du calcul
--------------------------------------------------------------------------------------
"""),

    97 : _(u"""
======================================================================================
                                                                        Fin du calcul

"""),

    98 : _(u"""    >> nombre de modélisations de rotor fissuré : %(i1)d"""),

    99 : _(u"""    >> nombre de modélisations de palier : %(i1)d"""),

}
