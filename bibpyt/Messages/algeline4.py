#@ MODIF algeline4 Messages  DATE 03/04/2007   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

def _(x) : return x

cata_msg={

1: _("""
 Seules les méthodes de résolution LDLT et MULT_FRONT sont autorisées.
"""),


3: _("""
 non convergence  nombre d'iterations:  %(i1)d
   norme du residu abs:  %(r1)f
   norme du residu rel:  %(r2)f
"""),

4: _("""
  manque de memoire memoire disponible %(i1)d et memoire necessaire %(i2)d
"""),

5: _("""
 erreur donnees noeud deja existant :  %(k1)s
"""),

6: _("""
 erreur donnees noeud deja existant :  %(k1)s
"""),

7: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

8: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

9: _("""
 erreur donnees group_ma deja existant :  %(k1)s
"""),

10: _("""
 erreur donnees group_ma deja existant :  %(k1)s
"""),

11: _("""
 erreur donnees group_no deja existant :  %(k1)s
"""),

12: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

13: _("""
 erreur donnees group_ma deja existant :  %(k1)s
"""),

14: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

15: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

16: _("""
 erreur donnees group_no deja existant :  %(k1)s
"""),

17: _("""
  inigpc %(i1)d
"""),

18: _("""
 non convergence  nombre d'iterations:  %(i1)d
   norme du residu abs:  %(r1)f
   norme du residu rel:  %(r2)f
"""),

19: _("""
 Matrice masse non définie, il faudrait essayer l'autre algorithme de résolution.
"""),

20: _("""
 Matrice masse non définie, il faudrait essayer l'autre algorithme de résolution.
"""),

21: _("""
 manque de place memoire longueur de bloc insuffisante:  %(i1)d
 le super-noeud  %(i2)d
  neccessite un bloc de  %(i3)d
"""),

22: _("""
 manque de place memoire longueur de bloc insuffisante:  %(i1)d
 le super-noeud  %(i2)d neccessite un bloc de  %(i3)d
"""),

23: _("""
 par nueq n'est pas l'identite.indice %(i1)d nueq  %(i2)d
"""),

24: _("""
 %(k1)s   pour le mot cle :  %(k2)s    noeud :  %(k3)s composante :  %(k4)s
"""),

25: _("""
 combinaison non prevue   type resultat :  %(k1)s    type matrice  :  %(k2)s
    type constante:  %(k3)s
"""),

26: _("""
 combinaison non prevue   type resultat :  %(k1)s    type matrice  :  %(k2)s
    type constante:  %(k3)s
"""),

27: _("""
 combinaison non prevue   type resultat :  %(k1)s    type matrice  :  %(k2)s
"""),

28: _("""
 combinaison non prevue   type resultat :  %(k1)s    type matrice  :  %(k2)s
    type constante:  %(k3)s
"""),

29: _("""
 combinaison non prevue   type resultat :  %(k1)s    type matrice  :  %(k2)s
    type constante:  %(k3)s
"""),

30: _("""
 combinaison non prevue   type resultat :  %(k1)s    type matrice  :  %(k2)s
"""),

31: _("""
 combinaison non prevue   type resultat :  %(k1)s
"""),

32: _("""
 charge critique retenue :  %(r1)f  numero de mode stocke  %(i1)d
"""),

33: _("""
 la normalisation doit se  faire en place et donc il est impossible d'avoir comme concept produit  %(k1)s
 et  %(k2)s
 comme concept d'entree. %(k3)s
 comme le dit la sagesse populaire, on  ne peut avoir le beurre et l'argent du  beurre (de charente poitou). %(k4)s
"""),

34: _("""
 erreur dans les donnees la masse n existe pas dans la table  %(k1)s
"""),

35: _("""
 erreur dans les donnees la masse n existe pas dans la table  %(k1)s
"""),

36: _("""
 l'option de normalisation  %(k1)s  n'est pas implantee. %(i1)d
"""),

37: _("""
 probleme(s) rencontre(s) lors de la factorisation de la matrice : %(k1)s
"""),

38: _("""
 appel errone :   code retour de rsexch : %(i1)d    pb cham_no %(k1)s
"""),

39: _("""
 appel errone :   code retour de rsexch : %(i1)d    pb cham_no %(k1)s
"""),

40: _("""
 appel errone :   code retour de rsexch : %(i1)d    pb cham_no %(k1)s
"""),

41: _("""
 appel errone :   code retour de rsexch : %(i1)d    pb cham_no %(k1)s
"""),

42: _("""
 pas de produit car les valeurs de la matrice sont  %(k1)s
 et celles du cham_no sont  %(k2)s
"""),

43: _("""
 la maille de nom  %(k1)s  existe deja %(k2)s
"""),

44: _("""
 erreur donnees noeud deja existant :  %(k1)s
"""),

45: _("""
 erreur donnees noeud deja existant :  %(k1)s
"""),

46: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

47: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

48: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

49: _("""
 erreur donnees maille deja existante :  %(k1)s
"""),

50: _("""
 erreur donnees group_ma deja existant :  %(k1)s
"""),

51: _("""
 erreur donnees group_ma deja existant :  %(k1)s
"""),

52: _("""
 erreur donnees group_no deja existant :  %(k1)s
"""),

53: _("""
 erreur donnees group_ma deja existant :  %(k1)s
"""),

54: _("""
 erreur donnees group_ma deja existant :  %(k1)s
"""),

55: _("""
 ***** alarme *****pas d'extraction pour  %(k1)s
  pour le numero d'ordre  %(i1)d
"""),

56: _("""
 ***** alarme *****pas de mode extrait pour  %(k1)s
"""),

57: _("""
 ***** alarme *****nume_mode identique pour le %(i1)d
  mode d'ordre  %(i2)d
"""),

58: _("""

  probleme dans le preconditionnement  de la matrice  matas par ldlt imcomplet
  pivot nul a la ligne :  %(i1)d
"""),

59: _("""
  manque de memoire memoire disponible %(i1)d et memoire necessaire %(i2)d
"""),

60: _("""
  incoherence n2 nbddl sans lagranges %(i1)d nbddl reconstitues %(i2)d
"""),

61: _("""
 pas de mode statique pour le noeud :  %(k1)s  et sa composante :  %(k2)s
"""),

62: _("""
 pour les modes statiques. on attend un :  %(k1)s    noeud :  %(k2)s
      cmp :  %(k3)s
"""),

63: _("""
 champ inexistant.pb champ :  %(k1)s    noeud :  %(k2)s      cmp :  %(k3)s
"""),

64: _("""
 detection d'un terme nul sur la sur diagonale valeur de beta   %(r1)f
 valeur de alpha  %(r2)f
"""),

65: _("""
 on a la  %(i1)d -ieme frequence du systeme reduit  est complexe =  %(r1)f
  et partie_imaginaire/reelle =  %(r2)f
"""),

66: _("""
 la val. pro. est:   %(r1)f
"""),

67: _("""
 la val. pro. est:   %(r1)f
"""),

68: _("""
 la vp est:   %(r1)f
"""),

69: _("""
 la vp est:   %(r1)f
"""),

70: _("""
 la val. pro. est:   %(r1)f
"""),

71: _("""
 la val. pro. est:   %(r1)f
"""),

72: _("""
 la vp est:   %(r1)f
"""),

73: _("""
 la vp est:   %(r1)f
"""),

74: _("""
 calcul d' erreur modale une valeur propre reelle est detectee %(k1)s
 a partir du couple (frequence, amortissement reduit) on ne peut plus  l'a reconstruire %(k2)s
 par convention l'erreur modale est  fixee a :  %(r1)f
"""),

75: _("""
 probleme generalise complexe
 amortissement (reduit) de decalage  superieur en valeur absolu a  %(r1)f
 on le ramene a la valeur :  %(r2)f
"""),

76: _("""
 la reorthogonalisation diverge apres  %(i1)d  iteration(s)   %(i2)d
"""),

77: _("""
 l'option de normalisation  %(k1)s  n'est pas implantee.
"""),

78: _("""
 l'option de normalisation  %(k1)s  n'est pas implantee.
"""),

79: _("""
 champ inexistant  %(k1)s impossible de recuperer neq %(k2)s
"""),

80: _("""
 type de valeurs inconnu   %(k1)s
"""),

81: _("""
 champ inexistant pb champ   %(k1)s
"""),

82: _("""
 incoherence de cer tains parametres modaux propres a arpack
  numero d'erreur  %(i1)d
"""),

83: _("""
 nombre de valeurs propres convergees  %(i1)d
 < nombre de frequences demandees  %(i2)d
 erreur arpack numero :  %(i3)d
 --> le calcul continue, la prochaine fois %(i4)d
 -->   augmenter dim_sous_espace =  %(i5)d
 -->   ou nmax_iter_soren =  %(i6)d
 -->   ou prec_soren =  %(r1)f
"""),

84: _("""
 incoherence de certains parametres modaux propres a arpack
  numero d'erreur  %(i1)d
"""),

85: _("""
 appel errone mode numero %(i1)d position modale %(i2)d
 code retour de rsexch : %(i3)d
 pb cham_no %(k1)s
"""),

86: _("""
 la reorthogonalisation diverge apres  %(i1)d  iteration(s) %(i2)d
       vecteur traite :  %(i3)d
       vecteur teste  :  %(i4)d
 arret de la reorthogonalisation %(k1)s
"""),

87: _("""
 pour le probleme reduitvaleur(s) propre(s) reelle(s)                   :  %(i1)d
 valeur(s) propre(s) complexe(s) avec  conjuguee :  %(i2)d
 valeur(s) propre(s) complexe(s) sans  conjuguee :  %(i3)d
"""),

88: _("""
 votre probleme est fortement amorti.
 valeur(s) propre(s) reelle(s)                   :  %(i1)d
 valeur(s) propre(s) complexe(s) avec  conjuguee :  %(i2)d
 valeur(s) propre(s) complexe(s) sans  conjuguee :  %(i3)d
"""),

89: _("""
 pour le probleme reduitvaleur(s) propre(s) reelle(s)                   :  %(i1)d
 valeur(s) propre(s) complexe(s) avec  conjuguee :  %(i2)d
 valeur(s) propre(s) complexe(s) sans  conjuguee :  %(i3)d
"""),

90: _("""
 votre probleme est fortement amorti.
 valeur(s) propre(s) reelle(s)                   :  %(i1)d
 valeur(s) propre(s) complexe(s) avec  conjuguee :  %(i2)d
 valeur(s) propre(s) complexe(s) sans  conjuguee :  %(i3)d
"""),

91: _("""
 pour le probleme reduitvaleur(s) propre(s) reelle(s)                   :  %(i1)d
 valeur(s) propre(s) complexe(s) avec  conjuguee :  %(i2)d
 valeur(s) propre(s) complexe(s) sans  conjuguee :  %(i3)d
"""),

92: _("""
 votre probleme est fortement amorti.
 valeur(s) propre(s) reelle(s)                   :  %(i1)d
 valeur(s) propre(s) complexe(s) avec  conjuguee :  %(i2)d
 valeur(s) propre(s) complexe(s) sans  conjuguee :  %(i3)d
"""),

93: _("""
 calcul d' erreur modale une valeur propre reelle est detectee %(k1)s
 a partir du couple (frequence, amortissement reduit) on ne peut plus  l'a reconstruire %(k2)s
 par convention l'erreur modale est  fixee a :  %(r1)f
"""),

94: _("""
 probleme quadratique complexe
 amortissement (reduit) de decalage  superieur en valeur absolu a  %(r1)f
 on le ramene a la valeur :  %(r2)f
"""),

95: _("""
 probleme quadratique
 amortissement (reduit) de decalage  superieur en valeur absolu a  %(r1)f
 on le ramene a la valeur :  %(r2)f
"""),

96: _("""
 l'option de normalisation  %(k1)s  n'est pas implantee.
"""),

97: _("""
 incoherence de certains parametres modaux propres a arpack
  numero d'erreur  %(i1)d
"""),

98: _("""
 nombre de valeurs propres convergees  %(i1)d
 < nombre de frequences demandees  %(i2)d
 erreur arpack numero :  %(i3)d
 --> le calcul continue, la prochaine fois %(i4)d
 -->   augmenter dim_sous_espace =  %(i5)d
 -->   ou nmax_iter_soren =  %(i6)d
 -->   ou prec_soren =  %(r1)f
 si votre probleme est fortement amorti  %(i7)d
 il est possible que des modes propres  %(i8)d
 non calcules soient sur-amortis  %(i9)d
 --> diminuez le nombre de frequences  %(i10)d
 demandees %(i11)d
"""),

99: _("""
 incoherence de certains parametres modaux propres a arpack
  numero d'erreur  %(i1)d
"""),

}
