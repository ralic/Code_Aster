#@ MODIF algeline5 Messages  DATE 26/10/2010   AUTEUR BOITEAU O.BOITEAU 
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

cata_msg={

1: _("""
 La manipulation (produit ou somme) de matrice distribuée est impossible
"""),

4: _("""
 erreur LAPACK (ou BLAS) au niveau de la routine  %(k1)s
  le paramètre numéro  %(i1)d
  n'a pas une valeur cohérente %(i2)d
"""),

5: _("""
 !! Attention, vous utilisez l'option de test FETI de l'interface.
 On va donc simuler la résolution d'un système diagonal canonique,
 pour provoquer un test d'ensemble de l'algorithme qui doit trouver
 la solution U=1 sur tous les noeuds.
 Vos résultats sont donc articiellement faussés pour les besoins de
 ce test. Pour réaliser effectivement votre calcul, désactiver cette
 option (INFO_FETI(12:12)='F' au lieu de 'T') !!
"""),

6: _("""
 Résolution MULTI_FRONTALE :
 problème dans le traitement des résultats de AMDBAR
 tous les NDS du SN %(i1)d ont NV nul
"""),

10: _("""
 ! le nb de noeuds de la structure   :  %(i1)d
 ! la base utilisée est              :  %(k1)s
 ! les caractéristiques élémentaires :  %(k2)s
 ! diamètre de la structure          :  %(r1)f
 ! type de pas                       :  %(i2)d
 ----------------------------------------------
"""),

11: _("""
 ! le profil de vitesse de la zone:  %(k1)s
 !   type de réseau de la zone    :  %(i1)d
 ----------------------------------------------
"""),

12: _("""

"""),

13: _("""
 ! le noeud d'application           :  %(k1)s
 ! la base utilisée est             :  %(k2)s
 ! les caractéristiques élémentaires:  %(k3)s
 ! diamètre de la structure         :  %(r1)f
 ! type de configuration            :  %(k4)s
 ! le coefficient de masse ajoutée  :  %(r2)f
 ! le profil de masse volumique     :  %(r3)f
 ----------------------------------------------

"""),

14: _("""
    pas de couplage pris en compte
 ----------------------------------------------
"""),

15: _("""
   pour le concept  %(k1)s  le mode numéro  %(i1)d
"""),

16: _("""
  de frequence  %(r1)f
"""),

17: _("""
  de charge critique  %(r1)f
"""),

18: _("""
  a une norme d'erreur de  %(r1)f  supérieure au seuil admis  %(r2)f
  
  Conseil: Si vous utiliser la méthode 'TRI_DIAG' ou 'SORENSEN' vous
  pouvez améliorer cette norme en:
    - augmentant la valeur de COEF_DIM_ESPACE (par défaut valeur 4 pour TRI_DIAG
      et 2 pour SORENSEN),
    - réduire le nombre de valeurs propres recherchées (NMAX_FREQ ou taille de
     la BANDE).
"""),

19: _("""
   pour le concept  %(k1)s  le mode numéro  %(i1)d
"""),

20: _("""
  de fréquence  %(r1)f
  est en dehors de l'intervalle de recherche : %(r2)f
  ,  %(r3)f
"""),

21: _("""
  de charge critique  %(r1)f
  est en dehors de l'intervalle de recherche : %(r2)f
  ,  %(r3)f
"""),

22: _("""

"""),

23: _("""
   pour le concept  %(k1)s
"""),

24: _("""
  dans l'intervalle  ( %(r1)f  ,  %(r2)f )
  il y a théoriquement  %(i1)d frequence(s)
  et on en a calculé  %(i2)d
"""),

25: _("""
  dans l'intervalle  ( %(r1)f  ,  %(r2)f )
  il y a théoriquement  %(i1)d charge(s) critique(s)
  et on en a calculé  %(i2)d
"""),

26: _("""

"""),

27: _("""
 la valeur du shift %(r1)f  est une fréquence propre
"""),

28: _("""
 les nombres de termes des matrices RIGI et MASSE différent
 celui de la matrice MASSE vaut :  %(i1)d
 celui de la matrice RIGI  vaut :  %(i2)d

"""),

29: _("""
 le nombre d'amortissements reduits est trop grand
 le nombre de modes propres vaut  %(i1)d
 et le nombre de coefficients :   %(i2)d
 on ne garde donc que les %(i3)d premiers coefficients

"""),

30: _("""
 le nombre d'amortissements réduits est insuffisant, il en manque :  %(i1)d,
 car le nombre de modes vaut :  %(i2)d
 on rajoute  %(i3)d amortissements réduits avec la valeur du dernier mode propre.
"""),

31: _("""
  incoherence :
   DEEQ I      =  %(i1)d
   DEEQ(2*I-1) =  %(i2)d
   DEEQ(2*I)   =  %(i3)d

"""),

32: _("""
  erreur de type DELG(IDDL) différent de -1 ou -2  %(i1)d
"""),

33: _("""
 un ddl bloqué a au moins 2 LAMBDA1 ou 2 LAMBDA2
 le ddl bloqué est  %(i1)d

"""),

34: _("""
 incohérence des lagranges
 DDL %(i1)d
 LAMBDA1 %(i2)d
 LAMBDA1 %(i3)d
"""),

35: _("""
 erreur programmeur
 le LAMBDA2  %(i1)d a moins de 2 voisins
 il faut le LAMBDA1 et au moins un DDL

"""),

36: _("""
 Problème dans le calcul des DDL :
 NUM devrait etre égal à n1 :
 num = %(i1)d , n1 = %(i2)d
 impression des lagranges
"""),

37: _("""
 NUME_DDL incohérence des lagranges
  ddl     %(i1)d
  lambda1 %(i2)d
  lambda1 %(i3)d
"""),

38: _("""
 nombre de relations linéaires %(i1)d
"""),

39: _("""
 LAMBDA1 de R linéaire : %(i1)d
 LAMBDA2 de R linéaire : %(i2)d
"""),

40: _("""
 Données erronées
"""),

41: _("""
 pas de mode statique pour  le noeud :  %(k1)s  et sa composante :  %(k2)s

"""),

42: _("""
 pour les modes statiques :
 on attend un :  %(k1)s
 noeud :  %(k2)s
 cmp   :  %(k3)s

"""),

43: _("""
 champ inexistant.
 champ :  %(k1)s
 noeud :  %(k2)s
 cmp   :  %(k3)s

"""),

48: _("""
 incohérence de certains paramètres modaux propres à ARPACK
  numéro d'erreur  %(i1)d

"""),

49: _("""
 nombre de valeurs propres convergées  %(i1)d < nombre de fréquences demandées  %(i2)d
 erreur ARPACK numéro :  %(i3)d
 --> le calcul continue, la prochaine fois
 -->   augmenter DIM_SOUS_ESPACE =  %(i4)d
 -->   ou NMAX_ITER_SOREN =  %(i5)d
 -->   ou PREC_SOREN =  %(r1)f

"""),

51: _("""
 la valeur propre numéro  %(i1)d a une partie imaginaire non nulle
 re(vp) = %(r1)f
 im(vp) = %(r2)f
 --> ce phénomène numérique est fréquent
 --> sur les premières valeurs propres
 --> lorsque le spectre recherche est
 --> tres étendu (en pulsation)

"""),

52: _("""
 LAIGLE: Erreur
   - Non convergence à l'itération maxi : %(i1)d
   - Convergence irrégulière & erreur >   %(r1)f
   - Diminuer la taille d'incrément.
"""),

53: _("""
 Erreur de programmation MULT_FRONT (NUME_DDL / PREML0) :
   * Sur-connexion des Lagranges Lambda1
"""),

54: _("""
     ==== Type de maille Aster / Type de maille GMSH ====
"""),

55: _("""
    %(i1)d  éléments %(k1)s découpés en %(i2)d  éléments %(k2)s a %(i3)d noeuds
"""),

56: _("""
    La matrice factorisée produit par l'opérateur FACTOR ne peut faire l'objet
    d'un concept réentrant car la méthode de résolution définie dans NUME_DDL
    est 'GCPC'.
"""),

57: _("""
    Le préconditionnement d'une matrice assemblée complexe n'est pas permis.
"""),

58: _("""
    La masse du modele est nulle. On ne peut normer par rapport a la masse.
"""),

59: _("""
 MULT_FRONT: Erreur dans la renumerotation
   - Le Super-Noeud : %(i1)d
   - devrait etre le fils de   %(i2)d

 Risques & conseils :
   - Vous devriez rencontrer des problèmes lors de la factorisation.
   - Essayez un autre algorithme pour la renumérotation : 'MD', 'MDA', ...
"""),

60: _("""
    Méthode QZ dans MODE_ITER_SIMULT: La variante QR ne fonctionne qu'avec une
    matrice A symétrique réelle et B symétrique réelle définie positive ! 
    Donc elle n'accepte pas le flambement, les Lagranges d'AFFE_CHAR_MECA, 
    une matrice de rigidité/de masse complexe ou non symétrique, ainsi que les
    problèmes modaux quadratiques.
"""),
61: _("""
    Méthode QZ dans MODE_ITER_SIMULT: propriété spectrale non respectée sur la
    valeur propre n %(i1)d !. On a pas |alpha| < ||A|| et |béta| < ||B|| 
                     |alpha|=%(r1)f, ||A||=%(r2)f
                     | béta|=%(r3)f, ||B||=%(r4)f
"""),
62: _("""
    Méthode QZ dans MODE_ITER_SIMULT: On trouve un nombre de valeurs propres 
    %(i1)d différent du nombre de ddls physiques actifs %(i2)d ! 
"""),
63: _("""
    Méthode QZ dans MODE_ITER_SIMULT + OPTION='BANDE': On trouve un nombre de 
    valeurs propres %(i1)d différent du nombre de valeurs propres détectées
    dans la bande %(i2)d ! 
"""),
64: _("""
    Problème modal quadratique et méthode de JACOBI sont incompatible !
    Essayer plutôt la méthode de SORENSEN (METHODE='SORENSEN'). 
"""),
65: _("""
    L'option de calcul 'TOUT' n'est licite qu'avec METHODE='QZ'! 
"""),
66: _("""
    Méthode QZ dans MODE_ITER_SIMULT : On souhaite un nombre de valeurs
    propres %(i1)d supérieur au nombre de valeurs propres détectées %(i2)d ! 
"""),
67: _("""
    Attention on souhaite un nombre de valeurs propres NMAX_FREQ=%(i1)d supérieur
    au nombre de valeurs propres détectées NCONV=%(i2)d !
    Pour poursuivre le calcul on impose NMAX_FREQ=NCONV.
    Sans doute est-ce du, soit:
     -à un mauvais tri dans les valeurs propres complexes conjuguées.
      Contacter l'équipe de développement.
     -à une mauvaise convergence de la méthode. Regarder les paramètres permettant
      d'améliorer celle-ci.
     -à une action incomplète du shift. En diminuant la valeur de l'option CENTRE
      (FREQ) et en augmentant le nombre de valeurs propres retenues (NMAX_FREQ) on
      peut souvent capter tous les couples (lambda,conjg(lambda)) souhaités.
      Sinon utiliser METHODE='QZ' pour les problèmes de petites tailles (<500 ddls).
"""),
68: _("""
    Méthode QZ dans MODE_ITER_SIMULT: erreur LAPACK %(i1)d !
"""),
69: _("""
    Matrices de raideur, de masse et/ou d'amortissement non symétrique(s) dans
    MODE_ITER_SIMULT. Pour l'instant, seules les méthodes QZ et SORENSEN 
    acceptent ce type de matrice.
    La méthode QZ est à réserver aux petits problèmes modaux (centaines de ddls)
    dont on souhaite connaitre une grosse partie du spectre.
"""),
70: _("""
    Matrices de  raideur, de masse et/ou d'amortissement non symétrique(s) dans
    MODE_ITER_SIMULT avec,une matrice de raideur complexe. 
    Pour l'instant, ce cas n'a pas été pris en compte.
"""),
71: _("""
    Cette fonctionnalité requiert un solveur linéaire permettant de détecter les
    éventuelles singularités des matrices.
    Conseil:
    ========
    Changer de solveur linéaire (mot-clé SOLVEUR), utiliser MULT_FRONT ou MUMPS.
"""),
72: _("""
    Les matrices utilisées ne s'appuient pas sur des données issues d'un maillage.
    Dans ce cas on doit utiliser, soit le solveur linéaire LDLT, soit celui MUMPS.
    On a donc changé le paramétrage pour vous et selectionné le solveur linéaire
    MUMPS (avec son paramétrage usuel par défaut).
    Conseil:
    ========
    La prochaine fois, dans une telle situation (NUME_DDL_GENE...), paramétrer
    explicitement SOLVEUR/METHODE='MUMPS'.
"""),
73: _("""
    On a besoin d'effectuer un calcul de déterminant. Pour l'instant seuls les
    solveurs linéaires directs 'MULT_FRONT' et 'LDLT' l'effectuent. Veuillez chan
    ger le paramétrage du mot-clé SOLVEUR/METHODE pour prendre en compte un de
    ces deux solveurs. De préférence utilisez 'MULT_FRONT' qui est plus efficace
    que 'LDLT'.
"""),
74: _("""
    Lors d'un calcul modal (MODE_ITER_SIMULT/INV, CRIT_FLAMB...) utilisant le
    solveur linéaire MUMPS (SOLVEUR/METHODE='MUMPS'), il ne faut pas débrancher
    la détection de singularité.
    Conseil:
    ========
    Relancer le calcul avec SOLVEUR/NPREC>0 (par exemple 8).
"""),
}
