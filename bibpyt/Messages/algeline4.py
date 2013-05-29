# coding=utf-8
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

cata_msg = {

1 : _(u"""
 Seules les méthodes de résolution LDLT, MUMPS et MULT_FRONT sont autorisées.
"""),

2 : _(u"""
 solveur interne PETSc interdit pour l'instant avec FETI
"""),

3 : _(u"""
 Solveur GCPC :
 La résolution du système linéaire a échoué car le critère de convergence n'a pu être satisfait avec le nombre d'itérations autorisées.
   norme du résidu relatif après %(i1)d itérations :  %(r1)f
   critère de convergence à satisfaire             :  %(r2)f

 Conseils :
  * Augmentez le nombre d'itérations autorisées (SOLVEUR/NMAX_ITER).
  * Vous pouvez aussi augmenter le niveau de remplissage pour la factorisation incomplète (SOLVEUR/NIVE_REMPLISSAGE).
  * Si vous utilisez une commande non-linéaire (STAT_NON_LINE par exemple), diminuez la précision demandée pour la convergence (SOLVEUR/RESI_RELA).
    Prenez garde cependant car cela peut empêcher la convergence de l'algorithme non-linéaire.
"""),

4 : _(u"""
  Manque de mémoire :
     Mémoire disponible = %(i1)d
     Mémoire nécessaire = %(i2)d
"""),

5 : _(u"""
Erreur utilisateur dans la commande CREA_MAILLAGE :
  Pour créer le nouveau maillage, il faut créer de nouveaux noeuds.
  Ici, on cherche à créer le noeud (%(k1)s) mais il existe déjà.

  Par défaut, les nouveaux noeuds ont des noms commençant par le préfixe 'NS', mais ce préfixe
  peut être modifié par l'utilisateur (mots clés XXXX / PREF_NOEUD).

Risques & conseils :
  Quand on utilise 2 fois de suite la commande CREA_MAILLAGE, il est en général nécessaire
  d'utiliser au moins une fois l'un des mots clés PREF_NOEUD
"""),

6 : _(u"""
 Solveur GCPC :
 La résolution du système linéaire a échoué car le critère de convergence n'a pu être satisfait avec le nombre d'itérations autorisées.
   norme du résidu relatif après %(i1)d itérations :  %(r1)f
   critère de convergence à satisfaire             :  %(r2)f

 Conseils :
  * Augmentez le nombre d'itérations autorisées (SOLVEUR/NMAX_ITER).
  * Vous pouvez aussi réactualiser plus souvent le préconditionneur en diminuant la valeur du mot-clé SOLVEUR/REAC_PRECOND.
  * Si vous utilisez une commande non-linéaire (STAT_NON_LINE par exemple), diminuez la précision demandée pour la convergence (SOLVEUR/RESI_RELA).
    Prenez garde cependant car cela peut empêcher la convergence de l'algorithme non-linéaire.
"""),

7 : _(u"""
 Erreur données : maille déjà existante :  %(k1)s
"""),
8 : _(u"""
 Erreur lors de la résolution du système linéaire d'interface de FETI :
 Non convergence  avec le nombre d'itérations autorisées :  %(i1)d
   norme du résidu (absolu)  :  %(r1)f
   norme du résidu (relatif) :  %(r2)f

 Conseils :
  * Vous pouvez augmenter le nombre d'itérations autorisées (SOLVEUR/NMAX_ITER).
  * Vous pouvez activer le préconditionneur si cela n'est pas fait (PRE_COND='LUMPE').
  * Vous pouvez vérifier votre interface via INFO_FETI (cf. U2.08.03).
  * Vous pouvez vérifier le nombre de modes rigides en mode INFO=2.
"""),
9 : _(u"""
 Erreur données GROUP_MA déjà existant :  %(k1)s
"""),

10 : _(u"""
 Votre problème modal n'est pas un problème généralisé à matrices réelles symétriques :
 il comporte des matrices non symétriques et/ou complexes, ou bien il s'agit d'un problème quadratique.
 Son spectre n'est donc pas uniquement restreint à l'axe réel, il représente une zone du plan complexe.

 Conseil :
 Il faut donc relancer votre calcul avec le mot-clé TYPE_RESU='MODE_COMPLEXE' et les opérandes associées.
"""),

11 : _(u"""
 erreur données GROUP_NO déjà existant :  %(k1)s
"""),

12 : _(u"""
 Réduction sous forme de Hessenberg supérieure: erreur LAPACK %(i1)d !
"""),

13 : _(u"""
 L'algorithme APM a atteint le nombre maximal de discrétisations du contour,
 c'est à dire %(i1)d, sans convergence du procédé.

 Conseils :
 Vous pouvez:
  - Changer les dimensions du contour de manière à réduire son périmètre,
  - Changer sa localisation. Il passe peut-être très près de valeurs propres
    ce qui peut induire des perturbations numériques.
"""),

14 : _(u"""
 L'algorithme APM a atteint son nombre maximal d'itérations, c'est à dire %(i1)d,
 sans convergence du procédé.

 Conseils :
  Vous pouvez:
  - Augmenter ce nombre maximal d'itérations via le paramètre NMAX_ITER_CONTOUR,
  - Augmenter la discrétisation initiale du contour via NBPOINT_CONTOUR,
  - Changer les dimensions du contour de manière à réduire son périmètre,
  - Changer sa localisation. Il passe peut-être très près de valeurs propres
    ce qui peut induire des perturbations numériques.
"""),

15 : _(u"""
 L'algorithme APM avec le calcul du polynôme caractéristique via une factorisation
 LDLT a un problème numérique: le point de vérification (%(r1)f +i*%(r2)f)
 est très proche d'une valeur propre ou le solveur linéaire a eu un problème.

 Conseils :
 Vous pouvez:
 - Augmenter les dimensions du contour pour englober cette valeur propre,
 - Changer la discrétisation du contour (plus risqué).
 - Changer le paramétrage du solveur linéaire, ou le solveur linéaire lui-même (expert).
"""),

17: _(u"""
 La variante 'ROMBOUT' de la méthode de comptage 'APM' est en cours de fiabilisation.
 Elle n'a pas encore portée pour:
   - les matrices complexes et/ou non symétriques,
   - les problèmes quadratiques,
   - les matrices généralisées.

 Conseil :
 Vous pouvez utiliser l'autre variante de la méthode 'APM' via le paramétrage
 COMPTAGE/POLYNOME_CHARAC='LDLT'.
"""),

19 : _(u"""
 Matrice de masse non définie.

 Conseil : essayer un autre algorithme de résolution.
"""),

20: _(u"""
 Pour l'instant, on est obligé de choisir pour un résultat de type 'DYNAMIQUE' ou
 'FLAMBEMENT', la méthode de comptage 'STURM', et pour 'MODE_COMPLEXE', la méthode
 'APM'.
 Si vos choix ne respectent pas cette règle, on fait le changement pour vous, en
 se référant au type de problème que vous avez choisi.
"""),

21 : _(u"""
 Manque de place mémoire longueur de bloc insuffisante:  %(i1)d
 le super noeud  %(i2)d
  nécessite un bloc de %(i3)d
"""),

22 : _(u"""
 L'algorithme APM a convergé sur un nombre de fréquences aberrant !

 Conseils:
 Vous pouvez:
  - Augmenter la discrétisation initiale du contour via NBPOINT_CONTOUR,
  - Changer les dimensions du contour de manière à réduire son périmètre,
  - Changer sa localisation. Il passe peut-être très près de valeurs propres
    ce qui peut induire des perturbations numériques.
"""),

24 : _(u"""
 %(k1)s   pour le mot clé :  %(k2)s    noeud :  %(k3)s composante :  %(k4)s
"""),

25 : _(u"""
 combinaison non prévue   type résultat :  %(k1)s    type matrice  :  %(k2)s
    type constante:  %(k3)s
"""),

27 : _(u"""
 combinaison non prévue
 type résultat :  %(k1)s
 type matrice  :  %(k2)s
"""),

31 : _(u"""
 combinaison non prévue
 type résultat :  %(k1)s
"""),

33 : _(u"""
 la normalisation doit se faire en place
 il est impossible d'avoir comme concept produit  %(k1)s et %(k2)s comme concept d'entrée.
"""),

36 : _(u"""
 l'option de normalisation  %(k1)s  n'est pas implantée. %(i1)d
"""),

37 : _(u"""
 problème(s) rencontré(s) lors de la factorisation de la matrice : %(k1)s
"""),

38 : _(u"""
 appel erroné :
 code retour de RSEXCH : %(i1)d
 Problème CHAM_NO %(k1)s
"""),

39 : _(u"""
 Au moins une des matrices est non symétrique.
 Dans ce cas, l'option 'BANDE' n'est pas utilisable.
"""),

40 : _(u"""
 Au moins une des matrices est non symétrique.
 Dans ce cas, la détection des modes de corps rigide (OPTION='MODE_RIGIDE')
 n'est pas utilisable.
"""),

41 : _(u"""
 Au moins une des matrices est non symétrique.
 Dans ce cas, le calcul de flambement ne peut pas être mené.
"""),

42 : _(u"""
 pas de produit car les valeurs de la MATRICE sont  %(k1)s
 et celles du CHAM_NO sont  %(k2)s
"""),

43 : _(u"""
 la maille de nom  %(k1)s  existe déjà %(k2)s
"""),

55 : _(u"""
 pas d'extraction pour  %(k1)s
 pour le numéro d'ordre  %(i1)d
"""),

56 : _(u"""
 pas de mode extrait pour  %(k1)s
"""),

57 : _(u"""
 NUME_MODE identique pour le %(i1)d
 mode d'ordre  %(i2)d
"""),

58 : _(u"""
  problème dans le préconditionnement de la matrice MATAS par LDLT incomplet
  pivot nul à la ligne :  %(i1)d
"""),

60 : _(u"""
  incohérence n2 NBDDL sans multiplicateurs de Lagrange %(i1)d NBDDL reconstitués %(i2)d
"""),

61 : _(u"""
 pas de mode statique pour le noeud :  %(k1)s  et sa composante :  %(k2)s
"""),

62 : _(u"""
 pour les modes statiques, on attend un :  %(k1)s
 noeud :  %(k2)s
 composante   :  %(k3)s
"""),








64 : _(u"""
 détection d'un terme nul sur la sur diagonale
 valeur de BETA   %(r1)f
 valeur de ALPHA  %(r2)f
"""),

65 : _(u"""
 La  %(i1)d -ème valeur propre du système réduit est complexe.
 Partie imaginaire =  %(r1)f
 et partie imaginaire / partie réelle =  %(r2)f
 """),

66 : _(u"""
 la valeur propre est :   %(r1)f
"""),




74 : _(u"""
 Calcul d'erreur modale :
 une valeur propre réelle est détectée à partir du couple (fréquence, amortissement réduit).
 On ne peut plus la reconstruire.
 Par convention l'erreur modale est fixée à : %(r1)f.
"""),



76 : _(u"""
 la réorthogonalisation diverge après  %(i1)d  itération(s).
"""),

77 : _(u"""
 l'option de normalisation  %(k1)s  n'est pas implantée.
"""),





80 : _(u"""
 type de valeurs inconnu   %(k1)s
"""),



82 : _(u"""
 incohérence de certains paramètres modaux propres à ARPACK
 numéro d'erreur  %(i1)d
"""),




85 : _(u"""
 appel erroné mode numéro %(i1)d position modale %(i2)d
 code retour de RSEXCH : %(i3)d
 Problème CHAM_NO %(k1)s
"""),

86 : _(u"""
 la réorthogonalisation diverge après  %(i1)d  itération(s) %(i2)d
       vecteur traité :  %(i3)d
       vecteur testé  :  %(i4)d
 arrêt de la réorthogonalisation %(k1)s
"""),

87 : _(u"""
 pour le problème réduit
 valeur(s) propre(s) réelle(s)                  :  %(i1)d
 valeur(s) propre(s) complexe(s) avec conjuguée :  %(i2)d
 valeur(s) propre(s) complexe(s) sans conjuguée :  %(i3)d
"""),

88 : _(u"""
 votre problème est fortement amorti.
 valeur(s) propre(s) réelle(s)                  :  %(i1)d
 valeur(s) propre(s) complexe(s) avec conjuguée :  %(i2)d
 valeur(s) propre(s) complexe(s) sans conjuguée :  %(i3)d
"""),

93 : _(u"""
 Problème généralisé complexe.
"""),

94 : _(u"""
 Problème quadratique complexe.
"""),

95 : _(u"""
 Problème quadratique.
"""),

96 : _(u"""
 Amortissement (réduit) de décalage supérieur en valeur absolue à %(r1)f.
 On le ramène à la valeur : %(r2)f.
"""),

98 : _(u"""
 nombre de valeurs propres convergées  %(i1)d < nombre de fréquences demandées  %(i2)d
 erreur ARPACK numéro :  %(i3)d
 --> le calcul continue, la prochaine fois
 -->   augmenter DIM_SOUS_ESPACE =  %(i4)d
 -->   ou NMAX_ITER_SOREN =  %(i5)d
 -->   ou PREC_SOREN =  %(r1)f
 si votre problème est fortement amorti, il est possible que
 des modes propres non calculés soient sur amortis
 --> diminuez le nombre de fréquences demandées
"""),

}
