# coding=utf-8

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

1: _(u"""
 La somme de matrices distribuées n'ayant pas le même profil est impossible
"""),

4: _(u"""
 erreur LAPACK (ou BLAS) au niveau de la routine  %(k1)s
  le paramètre numéro  %(i1)d
  n'a pas une valeur cohérente %(i2)d
"""),






6: _(u"""
 Résolution MULTI_FRONTALE :
 problème dans le traitement des résultats de AMDBAR
 tous les NDS du SN %(i1)d ont NV nul
"""),

10: _(u"""
  le nombre de noeuds de la structure   :  %(i1)d
  la base utilisée est              :  %(k1)s
  les caractéristiques élémentaires :  %(k2)s
  diamètre de la structure          :  %(r1)f
  type de pas                       :  %(i2)d
"""),

11: _(u"""
  le profil de vitesse de la zone :  %(k1)s
  type de réseau de la zone       :  %(i1)d
"""),

13: _(u"""
  le noeud d'application            :  %(k1)s
  la base utilisée est              :  %(k2)s
  les caractéristiques élémentaires :  %(k3)s
  diamètre de la structure          :  %(r1)f
  type de configuration             :  %(k4)s
  le coefficient de masse ajoutée   :  %(r2)f
  le profil de masse volumique      :  %(r3)f
"""),

14: _(u"""
    pas de couplage pris en compte
"""),

15: _(u"""
   pour le concept  %(k1)s, le mode numéro  %(i1)d
"""),

16: _(u"""
  de fréquence  %(r1)f
"""),

17: _(u"""
  de charge critique  %(r1)f
"""),

18: _(u"""
  a une norme d'erreur de  %(r1)f  supérieure au seuil admis  %(r2)f.
"""),



20: _(u"""
  est en dehors de l'intervalle de recherche : [ %(r2)f,  %(r3)f ].
"""),



23: _(u"""
   pour le concept  %(k1)s,
"""),

24: _(u"""
  dans l'intervalle [%(r1)f  ,  %(r2)f]
  il y a théoriquement  %(i1)d fréquence(s) propres()
  et on en a calculé  %(i2)d.
"""),

25: _(u"""
  dans l'intervalle [%(r1)f  ,  %(r2)f]
  il y a théoriquement  %(i1)d charge(s) critique(s)
  et on en a calculé  %(i2)d.
"""),

26: _(u"""
 Ce problème peut apparaître lorsqu'il y a des modes multiples (structure avec symétries)
 ou une forte densité modale.
"""),

27: _(u"""
 La valeur du SHIFT %(r1)f coïncide avec une fréquence propre.
"""),

28: _(u"""
 les nombres de termes des matrices RIGI et MASSE différent
 celui de la matrice MASSE vaut :  %(i1)d
 celui de la matrice RIGI  vaut :  %(i2)d

"""),

29: _(u"""
 le nombre d'amortissements réduits est trop grand
 le nombre de modes propres vaut  %(i1)d
 et le nombre de coefficients :   %(i2)d
 on ne garde donc que les %(i3)d premiers coefficients

"""),

30: _(u"""
 le nombre d'amortissements réduits est insuffisant, il en manque :  %(i1)d,
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d amortissements réduits avec la valeur du dernier mode propre.
"""),

31: _(u"""
  incohérence :
   DEEQ I      =  %(i1)d
   DEEQ(2*I-1) =  %(i2)d
   DEEQ(2*I)   =  %(i3)d

"""),

32: _(u"""
  erreur de type DELG(IDDL) différent de -1 ou -2  %(i1)d
"""),

33: _(u"""
 un ddl bloqué a au moins 2 LAMBDA1 ou 2 LAMBDA2
 le ddl bloqué est  %(i1)d

"""),

34: _(u"""
 incohérence des multiplicateurs de Lagrange
 DDL %(i1)d
 LAMBDA1 %(i2)d
 LAMBDA2 %(i3)d
"""),

35: _(u"""
 erreur programmeur
 le LAMBDA2  %(i1)d a moins de 2 voisins
 il faut le LAMBDA1 et au moins un DDL

"""),

36: _(u"""
 Problème dans le calcul des DDL :
 NUM devrait être égal à n1 :
 NUM = %(i1)d , n1 = %(i2)d
 impression des multiplicateurs de Lagrange
"""),

37: _(u"""
 NUME_DDL incohérence des multiplicateurs de Lagrange
  DDL     %(i1)d
  LAMBDA1 %(i2)d
  LAMBDA2 %(i3)d
"""),

38: _(u"""
 nombre de relations linéaires %(i1)d
"""),

39: _(u"""
 LAMBDA1 de R linéaire : %(i1)d
 LAMBDA2 de R linéaire : %(i2)d
"""),

40: _(u"""
 Données erronées
"""),

41: _(u"""
 pas de mode statique pour  le noeud :  %(k1)s  et sa composante :  %(k2)s

"""),

42: _(u"""
 pour les modes statiques :
 on attend un :  %(k1)s
 noeud :  %(k2)s
 composante   :  %(k3)s

"""),

43: _(u"""
 champ inexistant.
 champ :  %(k1)s
 noeud :  %(k2)s
 composante   :  %(k3)s

"""),

48: _(u"""
 incohérence de certains paramètres modaux propres à ARPACK
  numéro d'erreur  %(i1)d

"""),

49: _(u"""
 nombre de valeurs propres convergées  %(i1)d < nombre de fréquences demandées  %(i2)d
 erreur ARPACK numéro :  %(i3)d
 --> le calcul continue, la prochaine fois
 -->   augmenter DIM_SOUS_ESPACE =  %(i4)d
 -->   ou NMAX_ITER_SOREN =  %(i5)d
 -->   ou PREC_SOREN =  %(r1)f

"""),

51: _(u"""
 La valeur propre numéro  %(i1)d a une partie imaginaire non nulle.
 Partie réelle     = %(r1)12.5E
 Partie imaginaire = %(r2)12.5E

 Ce phénomène numérique est fréquent sur les premières valeurs propres
 lorsque le spectre recherché est très étendu.
"""),

52: _(u"""
 LAIGLE: Erreur
   - Non convergence à l'itération max : %(i1)d
   - Convergence irrégulière & erreur >   %(r1)f
   - Diminuer la taille d'incrément.
"""),

53: _(u"""
 Erreur de programmation MULT_FRONT (NUME_DDL / PREML0) :
   * Sur connexité des Lagrange Lambda1
"""),

54: _(u"""
     ==== Type de maille Aster / Type de maille GMSH ====
"""),

55: _(u"""
    %(i1)d  éléments %(k1)s découpés en %(i2)d  éléments %(k2)s a %(i3)d noeuds
"""),

56: _(u"""
    La matrice factorisée produit par l'opérateur FACTOR ne peut faire l'objet
    d'un concept réentrant car la méthode de résolution définie dans NUME_DDL
    est 'GCPC'.
"""),

57: _(u"""
    Le préconditionnement d'une matrice assemblée complexe n'est pas permis.
"""),

58: _(u"""
    La masse du modèle est nulle.
    On ne peut donc pas normer par rapport à la masse.
"""),

59: _(u"""
 MULT_FRONT: Erreur dans la renumérotation
   - Le super noeud : %(i1)d
   - devrait être le fils de   %(i2)d

 Risques & conseils :
   - Vous devriez rencontrer des problèmes lors de la factorisation.
   - Essayez un autre algorithme pour la renumérotation : 'MD', 'MDA', ...
"""),

60: _(u"""
    La variante 'QZ_QR' de la méthode 'QZ' ne fonctionne qu'avec une matrice %(k1)s symétrique réelle
    et %(k2)s symétrique réelle définie positive. Donc elle ne traite pas les problèmes de flambement,
    les Lagrange issus de AFFE_CHAR_MECA, des matrices complexes ou non symétriques,
    ni les problèmes modaux quadratiques.
"""),

61: _(u"""
    Méthode 'QZ' : propriété spectrale non respectée sur la valeur propre numéro %(i1)d.
    Les relations |alpha| < ||A|| et |bêta| < ||B|| ne sont pas vérifiées :
          |alpha|=%(r1)f,  ||A||=%(r2)f
          |bêta| =%(r3)f,  ||B||=%(r4)f
"""),

62: _(u"""
    Méthode QZ dans MODE_ITER_SIMULT: On trouve un nombre de valeurs propres
    %(i1)d différent du nombre de ddls physiques actifs %(i2)d !
"""),

63: _(u"""
    Méthode QZ dans MODE_ITER_SIMULT + OPTION='BANDE': On trouve un nombre de
    valeurs propres %(i1)d différent du nombre de valeurs propres détectées
    dans la bande %(i2)d !
"""),

64: _(u"""
    La méthode de 'JACOBI' n'est pas utilisable pour un problème modal quadratique
    (présence d'une matrice %(k1)s).

    Conseil :
    Utiliser la méthode 'SORENSEN' ou 'TRI_DIAG'.
"""),

65: _(u"""
    L'option de calcul 'TOUT' (sous le mot-clé facteur %(k1)s)
    est licite seulement avec METHODE='QZ'.
"""),

66: _(u"""
    Méthode QZ dans MODE_ITER_SIMULT : On souhaite un nombre de valeurs
    propres %(i1)d supérieur au nombre de valeurs propres détectées %(i2)d !
"""),



68: _(u"""
    Méthode 'QZ' dans MODE_ITER_SIMULT: erreur LAPACK %(i1)d !
"""),

69: _(u"""
    Au moins une des matrices est non symétrique.
    Pour l'instant, seules les méthodes 'SORENSEN' et 'QZ' peuvent traiter le cas de
    matrices non symétriques.

    Conseils :
    - Si le problème modal est de petite taille (quelques centaines de DDL),
      utiliser la méthode 'QZ'.
    - Sinon, utiliser 'SORENSEN'.
"""),

70: _(u"""
    Au moins une des matrices est non symétrique, et la matrice %(k1)s est complexe.
    Pour l'instant, ce cas n'a pas été développé dans le code.
"""),

71: _(u"""
    Cette fonctionnalité requiert un solveur linéaire permettant de
    détecter les éventuelles singularités des matrices.

    Conseil :
    changer de solveur linéaire : sous le mot-clé facteur SOLVEUR,
    utiliser 'MULT_FRONT' ou 'MUMPS'.
"""),

73: _(u"""
    On a besoin d'effectuer un calcul de déterminant.
    Pour l'instant seuls les solveurs linéaires directs 'MULT_FRONT', 'LDLT'
    et MUMPS (à partir de la version 4.10.0) peuvent effectuer ce type de calcul.

    Conseil :
    Choisir un de ces deux solveurs (mot-clé METHODE sous le mot-clé facteur SOLVEUR).
    De préférence, utiliser 'MUMPS' (à partir de la version 4.10.0) qui est souvent
    le plus efficace pour des gros problèmes et/ou des problèmes difficiles
    (X-FEM, incompressibilité, THM, ...).
"""),

74: _(u"""
    Vous utilisez une fonctionnalité qui nécessite de connaître le degré de singularité de matrices associées à
    des systèmes linéaires. Or, vous avez désactivé la détection de singularité avec le mot-clé NPREC.

    Conseils :
      - Relancez le calcul avec NPREC > 0 (par exemple 8) sous le mot-clé facteur SOLVEUR.
      - S'il vous est indispensable de désactiver la détection de singularité, essayez d'utiliser un autre solveur linéaire, comme MULT_FRONT par exemple.
"""),

75: _(u"""
    Le solveur modal n'a pas réussi à capturer tous les modes propres souhaités
    avec le niveau de convergence requis.

    Conseils :
    Pour améliorer la convergence des algorithmes modaux vous pouvez par exemple :
     - Diminuer le nombre de modes recherchés à chaque fois en découpant votre calcul modal en plusieurs bandes
       (avec l'opérateur MACRO_MODE_MECA, ou à la main).
       Cela améliore aussi souvent grandement les performances des calculs.
     - Avec la méthode de 'SORENSEN', augmenter la taille de l'espace de projection (DIM_SOUS_ESPACE/COEF_DIM_ESPACE)
       ou jouer sur les paramètres numériques qui pilotent la convergence (PREC_SOREN et NMAX_ITER_SOREN).
     - Avec la méthode 'QZ', diminuer NMAX_FREQ ou changer de variante (mot-clé TYPE_QZ).
     Si vous voulez tout de même utiliser les modes ainsi calculés (à vos risques et périls),
     relancer le calcul en augmentant la valeur du seuil de convergence (mot-clé SEUIL)
     ou en utilisant l'option STOP_ERREUR='NON' sous le mot-clé facteur VERI_MODE.
"""),

76 : _(u"""
   Solveur GCPC :
   la création du préconditionneur 'LDLT_SP' a échoué car on manque de mémoire.

   Conseil :
   augmenter la valeur du paramètre PCENT_PIVOT sous le mot-clé facteur SOLVEUR.
"""),

77 : _(u"""
Conseils :
Si vous utilisez METHODE='SORENSEN' ou 'TRI_DIAG' ou 'JACOBI', vous pouvez améliorer cette norme :
 - en augmentant la valeur de COEF_DIM_ESPACE (la valeur par défaut est 4 pour 'TRI_DIAG' et 2 pour 'SORENSEN' et 'JACOBI'),
 - en réduisant le nombre de valeurs propres recherchées (%(k1)s ou taille de la BANDE).
"""),

78 : _(u"""
Conseils :
Vous pouvez améliorer cette norme :
 - en augmentant les nombres d'itérations des algorithmes
    (paramètres NMAX_ITER_SEPARE/AJUSTE sous le mot-clé facteur %(k1)s
     et/ou paramètre NMAX_ITER sous le mot-clé facteur CALC_MODE),
 - en augmentant la précision requise
    (paramètres PREC_SEPARE/PREC_AJUSTE sous le mot-clé facteur %(k1)s
     et/ou paramètre PREC sous le mot-clé facteur CALC_MODE),
 - en changeant d'algorithme
    (mot-clé OPTION sous le mot-clé facteur %(k1)s
     et/ou mot-clé OPTION sous le mot-clé facteur CALC_MODE).
"""),

79: _(u"""
    On souhaite un nombre de valeurs propres NMAX_%(k1)s=%(i1)d
    supérieur au nombre de valeurs propres détectées NUM=%(i2)d !
"""),

80: _(u"""
    Pour poursuivre le calcul, on impose NMAX_%(k1)s=NUM.
"""),

81: _(u"""
    Ce problème peut être dû :
    - à un mauvais tri dans les valeurs propres complexes conjuguées.
      Contacter l'équipe de développement.

    - à une mauvaise convergence de la méthode.
      Regarder les paramètres permettant d'améliorer celle-ci.

    - à une action incomplète du SHIFT.
      En diminuant la valeur %(k1)s de l'option 'CENTRE'
      et en augmentant le nombre de valeurs propres retenues (NMAX_%(k1)s),
      on peut souvent capter tous les couples (lambda,conjugué(lambda)) souhaités.

    Sinon utiliser METHODE='QZ' pour les problèmes de petites tailles (<500 ddls).
"""),

82 : _(u"""
L'option 'PLUS_GRANDE' n'est pas utilisable en présence d'une matrice d'amortissement,
d'une matrice de rigidité complexe, ou de matrices non symétriques.
"""),

}
