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
# person_in_charge: nicolas.brie at edf.fr

cata_msg={

1: _(u"""  ---------------------------------------
  Impression de niveau 2.

  Nom de la base modale  : %(k1)s
  ---------------------
"""),

2: _(u"""  Type de la base modale : Classique
  ----------------------

  INTERF_DYNA               : %(k1)s
  Numérotation              : %(k2)s
  Matrice de raideur        : %(k3)s
  Matrice de masse          : %(k4)s
  Nombre de modes propres   : %(i1)d
  Nombre de modes statiques : %(i2)d

"""),

3: _(u"""  Type de la base modale : Cyclique
  ----------------------

  INTERF_DYNA  : %(k1)s
  Numérotation : %(k2)s

"""),

4: _(u"""  Type de la base modale : Ritz
  ----------------------

  Numérotation           : %(k1)s
  Dimension de la base   : %(i1)d

"""),

5: _(u"""
       Définition des déformées de la base modale.
       -------------------------------------------

"""),

6: _(u"""  NUME_ORDRE : %(i1)d
  Type                : Mode PROPRE
  Fréquence           : %(r1)12.5E Hz
  Masse généralisée   : %(r2)12.5E
  Raideur généralisée : %(r3)12.5E

"""),

7: _(u"""  NUME_ORDRE : %(i1)d
  Type       : Mode %(k1)s
  Noeud      : %(k2)s
  Composante : %(k3)s

"""),

8: _(u"""  Concept %(k1)s de type %(k2)s issu de l'opérateur %(k3)s.

"""),

9: _(u"""  Il y a %(i1)d valeur(s) propre(s) dans la bande (%(r1)12.5E ,%(r2)12.5E ) .
"""),

10: _(u"""  Problème quadratique.
  La fréquence de décalage est négative. Comme les valeurs propres sont
  conjuguées deux à deux, on la prend positive.

"""),

11: _(u"""      Informations sur le calcul demandé:

  Nombre de modes recherchés  : %(i1)d

"""),

12: _(u"""      Informations sur le calcul demandé:

  Il y a trop de modes demandés pour le nombre de DDL actifs. On en calcule
  le maximum à savoir : %(i1)d

"""),

13: _(u"""  La dimension de l'espace réduit est : %(i1)d
"""),

14: _(u"""  Elle est inférieure au nombre de modes, on la prend égale à : %(i1)d
"""),

15: _(u"""  Elle est supérieure au nombre de DDL actifs, on la ramène à ce nombre : %(i1)d
"""),

16: _(u"""  Les fréquences calculées sont comprises entre :
      Fréquence inférieure : %(r1)12.5E
      Fréquence supérieure : %(r2)12.5E

"""),

17: _(u"""  Les charges critiques calculées sont comprises entre :
      Charge critique inférieure : %(r1)12.5E
      Charge critique supérieure : %(r2)12.5E

"""),

18: _(u"""  La première fréquence inférieure non retenue est : %(r1)12.5E
"""),

19: _(u"""  La première fréquence supérieure non retenue est : %(r1)12.5E
"""),

20: _(u"""  La première charge critique inférieure non retenue est : %(r1)12.5E
"""),

21: _(u"""  La première charge critique supérieure non retenue est : %(r1)12.5E
"""),

22: _(u"""         Vérification à posteriori des modes

"""),

23: _(u"""  Dans l'intervalle (%(r1)12.5E ,%(r2)12.5E ) il y a bien %(i1)d fréquence(s).
"""),

24: _(u"""  Dans l'intervalle (%(r1)12.5E ,%(r2)12.5E ) il y a bien %(i1)d charges(s) critique(s).
"""),

25: _(u"""
  Méthode 'QZ': La norme L1 de :
    -la matrice de raideur K est      : %(r1)12.4E
    -la matrice de masse M est        : %(r2)12.4E
    -la matrice d'amortissement C est : %(r3)12.4E

  Le coefficient multiplicateur du problème linéarisé est : %(r4)12.4E

"""),

26: _(u""" 
  Méthode 'QZ': Norme infinie de A : %(r1)10.2E   et de B : %(r2)10.2E

  Méthode 'QZ': Norme L1 de A : %(r3)10.2E   et de B : %(r4)10.2E

"""),

27: _(u"""

     =============================================
     =       Méthode de Sorensen (code ARPACK)   =
     =       Version :  2.4                      =
     =          Date :  07/31/96                 =
     =============================================

     Nombre de redémarrages                     = %(i1)5d
     Nombre de produits OP*X                    = %(i2)5d
     Nombre de produits B*X                     = %(i3)5d
     Nombre de réorthogonalisations  (étape 1)  = %(i4)5d
     Nombre de réorthogonalisations  (étape 2)  = %(i5)5d
     Nombre de redémarrages du a un V0 nul      = %(i6)5d

"""),

28: _(u"""  Vérification du spectre de charges critiques (Méthode de Sturm)

"""),

29: _(u"""  Vérification du spectre de charges critiques (Méthode de Sturm -
  Résultat issu d'un INFO_MODE précédent ).

"""),

30: _(u"""  Il n'y a pas de charge critique dans la bande numéro %(i1)d de bornes (%(r1)10.3E ,%(r2)10.3E )
"""),

31: _(u"""  Nombre de charges critiques inférieures à %(r1)10.3e Hz : %(i1)d
"""),

32: _(u"""  Nombre de charges critiques dans la bande numéro %(i1)d de bornes (%(r1)10.3E ,%(r2)10.3E ) : %(i2)d
"""),

33: _(u"""  Vérification du spectre de fréquences (Méthode de Sturm)

"""),

34: _(u"""  Vérification du spectre de fréquences (Méthode de Sturm-
  Résultat issu d'un INFO_MODE précédent )

"""),

35: _(u"""  Il n'y a pas de fréquence dans la bande numéro %(i1)d de bornes (%(r1)10.3E ,%(r2)10.3E )
"""),

36: _(u"""  Nombre de fréquences propres inférieures à %(r1)10.3E Hz : %(i1)d
"""),

37: _(u"""  Nombre de fréquences dans la bande numéro %(i1)d de bornes (%(r1)10.3E ,%(r2)10.3E ) : %(i2)d
"""),

38: _(u"""  Vérification du spectre en fréquences (Méthode de l'argument principal)

"""),

39: _(u"""  Il n'y a pas de fréquence dans le disque centré en (%(r1)10.3E ,%(r2)10.3E ) et de rayon %(r3)10.3E .
"""),

40: _(u"""  Nombre de fréquences dans le disque centré en (%(r1)10.3E ,%(r2)10.3E ) et de rayon %(r3)10.3E : %(i1)d
"""),

41: _(u"""  L'option choisie est : %(k1)s
"""),

42: _(u"""  La valeur de décalage en fréquence est : %(r1)12.5E
"""),

43: _(u"""  La valeur de décalage en charge critique est : %(r1)12.5E
"""),

44: _(u"""  La valeur de décalage (OMEGA2) est inférieure à la valeur de corps rigide.
  On la modifie et elle devient : %(r1)12.5E
"""),

45: _(u"""  On augmente la valeur de décalage de %(r1)12.5E %%.
  La valeur centrale devient : %(r2)12.5E
"""),

46: _(u"""  La valeur minimale est inférieure à la valeur de corps rigide.
  On la modifie et elle devient : %(r1)12.5E
"""),

47: _(u"""  On diminue la valeur minimale de %(r1)12.5E %%.
  La valeur minimale devient : %(r2)12.5E
"""),

48: _(u"""  La valeur maximale est inférieure à la valeur de corps rigide.
  On la modifie et elle devient : %(r1)12.5E
"""),

49: _(u"""  On augmente la valeur maximale de %(r1)12.5E %%.
  La valeur maximale devient : %(r2)12.5E
"""),

50: _(u"""
                                    Masse      effective      unitaire

NUME_ORDRE  NUME_MODE     fréquence   MASS_EFFE_UN_DX   CUMUL_DX    MASS_EFFE_UN_DY   CUMUL_DY    MASS_EFFE_UN_DZ   CUMUL_DZ
"""),

51: _(u"""%(i1)10d   %(i2)8d   %(r1)12.5E   %(r2)12.5E   %(r5)12.5E   %(r3)12.5E   %(r6)12.5E   %(r4)12.5E   %(r7)12.5E
"""),

52: _(u"""
                      Masse      effective      unitaire

NUME_ORDRE  NUME_MODE     fréquence   MASS_EFFE_UN_DX   MASS_EFFE_UN_DY   MASS_EFFE_UN_DZ
"""),

53: _(u"""%(i1)10d   %(i2)8d   %(r1)12.5E   %(r2)12.5E   %(r3)12.5E   %(r4)12.5E
"""),

54: _(u"""
                   Masse      généralisée

NUME_ORDRE  NUME_MODE     fréquence   MASS_GENE   CUMUL_MASS_GENE
"""),

55: _(u"""%(i1)10d   %(i2)8d   %(r1)12.5E   %(r2)12.5E   %(r3)12.5E
"""),

56: _(u"""
           Masse      généralisée

NUME_ORDRE  NUME_MODE     fréquence   MASS_GENE
"""),

57: _(u"""%(i1)10d   %(i2)8d   %(r1)12.5E   %(r2)12.5E
"""),

58: _(u"""
  Norme d'erreur moyenne   : %(r1)12.5E
"""),

59: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Bathe et Wilson

numéro    fréquence (HZ)     norme d'erreur    ITER_BATH    ITER_JACOBI
"""),

60: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Bathe et Wilson

numéro    charge critique    norme d'erreur    ITER_BATH    ITER_JACOBI
"""),

61: _(u""" %(i1)4d      %(r2)12.5E       %(r1)12.5E      %(i2)4d          %(i3)4d
"""),

62: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Lanczos

numéro    fréquence (HZ)     norme d'erreur    ITER_QR
"""),

63: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Lanczos

numéro    charge critique    norme d'erreur    ITER_QR
"""),

64: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Lanczos

numéro    fréquence (HZ)     amortissement    ITER_QR
"""),

65: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Lanczos

numéro    charge critique    amortissement    ITER_QR
"""),

66: _(u""" %(i1)4d     %(r2)12.5E       %(r1)12.5E     %(i2)4d
"""),

67: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Sorensen

numéro    fréquence (HZ)     norme d'erreur
"""),

68: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Sorensen

numéro    charge critique    norme d'erreur
"""),

69: _(u""" %(i1)4d      %(r2)12.5E       %(r1)12.5E
"""),

70: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Sorensen

numéro    fréquence (HZ)     amortissement    norme d'erreur
"""),

71: _(u"""     Calcul modal : Méthode d'itération simultanée
                    Méthode de Sorensen

numéro    charge critique    amortissement    norme d'erreur
"""),


72: _(u"""%(i1)4d       %(r2)12.5E      %(r1)12.5E      %(r3)12.5E
"""),

73: _(u"""     Calcul modal : Méthode globale de type QR
                    Algorithme %(k1)s

numéro    fréquence (HZ)     norme d'erreur
"""),

74: _(u"""     Calcul modal : Méthode globale de type QR
                    Algorithme %(k1)s

numéro    charge critique    norme d'erreur
"""),

75: _(u"""     Calcul modal : Méthode globale de type QR
                    Algorithme %(k1)s

numéro    fréquence (HZ)     amortissement    norme d'erreur
"""),

76: _(u"""     Calcul modal : Méthode globale de type QR
                    Algorithme %(k1)s

numéro    charge critique    amortissement    norme d'erreur
"""),

77: _(u"""     Calcul modal : Méthode d'itération inverse

                                                   INVERSE
numéro    fréquence (HZ)     amortissement    NB_ITER    précision    norme d'erreur
"""),

78: _(u"""     Calcul modal : Méthode d'itération inverse

                                                   INVERSE
numéro    charge critique    amortissement    NB_ITER    précision    norme d'erreur
"""),

79: _(u""" %(i1)4d      %(r1)12.5E       %(r2)12.5E     %(i2)4d    %(r3)12.5E    %(r4)12.5E
"""),

80: _(u"""     Calcul modal : Méthode d'itération inverse

                                             DICHOTOMIE       SECANTE                 INVERSE
numéro    fréquence (HZ)     amortissement    NB_ITER    NB_ITER    précision    NB_ITER    précision    norme d'erreur
"""),

81: _(u"""     Calcul modal : Méthode d'itération inverse

                                             DICHOTOMIE       SECANTE                 INVERSE
numéro    charge critique    amortissement    NB_ITER    NB_ITER    précision    NB_ITER    précision    norme d'erreur
"""),

82: _(u""" %(i1)4d      %(r1)12.5E       %(r2)12.5E     %(i2)4d       %(i3)4d    %(r3)12.5E    %(i4)4d    %(r4)12.5E    %(r5)12.5E
"""),

83: _(u"""     Calcul modal : Méthode d'itération inverse

                                             DICHOTOMIE        INVERSE
numéro    fréquence (HZ)     amortissement    NB_ITER    NB_ITER    précision    norme d'erreur
"""),

84: _(u"""     Calcul modal : Méthode d'itération inverse

                                             DICHOTOMIE        INVERSE
numéro    charge critique    amortissement    NB_ITER    NB_ITER    précision    norme d'erreur
"""),

85: _(u""" %(i1)4d      %(r1)12.5E       %(r2)12.5E     %(i2)4d       %(i3)4d    %(r3)12.5E    %(r4)12.5E
"""),

86: _(u"""     Calcul modal : Méthode d'itération inverse

                                                    MULLER                 INVERSE
numéro    fréquence (HZ)     amortissement    NB_ITER    précision    NB_ITER    précision    norme d'erreur
"""),

87: _(u"""     Calcul modal : Méthode d'itération inverse

                                                    MULLER                 INVERSE
numéro    charge critique    amortissement    NB_ITER    précision    NB_ITER    précision    norme d'erreur
"""),

88: _(u""" %(i1)4d      %(r1)12.5E       %(r2)12.5E     %(i2)4d    %(r3)12.5E    %(i3)4d    %(r4)12.5E    %(r5)12.5E
"""),

89: _(u"""Le nombre maximal d'itérations NMAX_ITER_SOREN = %(i1)d  a été atteint.
"""),

90: _(u"""Aucun shift ne peut être appliqué.
"""),

91: _(u"""Problème de factorisation de Arnoldi.
"""),

92: _(u""" On modifie la valeur de décalage de %(r2)12.5E %%
 La valeur de décalage devient : %(r1)12.5E
"""),

93: _(u"""  La valeur minimale en fréquence est : %(r1)12.5E
  La valeur maximale en fréquence est : %(r2)12.5E
"""),

94: _(u"""  La valeur minimale en charge critique est : %(r1)12.5E
  La valeur maximale en charge critique est : %(r2)12.5E
"""),

95: _(u"""----------------------------------------------------
                CALC_MODE_CYCL 

Impressions de niveau :2

Définition du secteur
---------------------
Maillage    : %(k1)s
Base modale : %(k2)s
INTERF_DYNA : %(k3)s

"""),

96: _(u"""  Définition de la liaison
  ------------------------
Type de base modale : %(k1)s
Interface droite    : %(k2)s
Interface gauche    : %(k3)s

                           Résultats modaux :
                           ----------------

"""),

97: _(u"""  Définition de la liaison
  ------------------------
Type de base modale : %(k1)s
Interface droite    : %(k2)s
Interface gauche    : %(k3)s
Interface axe       : %(k4)s

                           Résultats modaux :
                           ----------------

"""),

98: _(u"""  Modes à %(i1)d diamètres nodaux
  -------------------------------

  numéro    fréquence(HZ)
"""),

99: _(u"""  %(i1)4d       %(r1)12.5E
"""),
}
