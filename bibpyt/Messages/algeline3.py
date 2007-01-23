#@ MODIF algeline3 Messages  DATE 23/01/2007   AUTEUR ABBAS M.ABBAS 
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
 le mot-cle maillage est obligatoire avec le mot-cle crea_maille. 
"""),

2: _("""
 le mot-cle maillage est obligatoire avec le mot-cle crea_group_ma. 
"""),

3: _("""
 le mot-cle maillage est obligatoire avec le mot-cle crea_poi1. 
"""),

4: _("""
 le mot-cle maillage est obligatoire avec le mot-cle repere. 
"""),

5: _("""
 sous le mot-cle "nom_orig" du mot-facteur "repere", on ne peut donner que les mots "cdg" ou "torsion".
"""),

6: _("""
 maille non creee  %(k1)s 
"""),

7: _("""
  le group_ma :  %(k1)s  existe deja.
"""),

8: _("""
 le mot-cle maillage est obligatoire avec le mot-cle detr_group_ma. 
"""),

9: _("""
 mode non compatible.
"""),

10: _("""
 masses effectives unitaires non calculees par norm_mode
"""),

11: _("""
 structure resultat vide
"""),

12: _("""
 nombre de noeuds sur le contour insuffisant pour determiner correctement les ordres de coque
"""),

13: _("""
 azimut indefini pour un des noeuds de la coque
"""),

14: _("""
 ordre de coque nul pour l un des modes pris en compte pour le couplage. le modele de resolution ne supporte pas une telle valeur.
"""),

15: _("""
 determination du drmax et du dephasage pour le mode  %(k1)s  : le determinant du systeme issu du moindre carre est nul
"""),

16: _("""
 determination du dephasage pour le  mode  %(k1)s  : theta0 indefini
"""),

17: _("""
 pivot nul dans la resolution du systeme complexe
"""),

18: _("""
 annulation du numerateur dans l expression d un coefficient donnant la solution du probleme fluide instationnaire pour umoy = 0
"""),

19: _("""
 determination des valeurs propres de l operateur differentiel : existence d une racine double
"""),

20: _("""
 la  %(k1)s eme valeur propre est trop petite
"""),

21: _("""
 la matr_asse  %(k1)s  n"est pas stockee "morse" le gcpc est donc impossible.
"""),

22: _("""
 conflit une matrice stockee morse ne peut avoir qu"un bloc
"""),

23: _("""
  le preconditionnement ldlt_inc d"une matrice complexe n"est pas implemente
"""),

24: _("""
 stop 1:erreur programation.
"""),

25: _("""
  erreur a l'appel de metis 
"""),

26: _("""
 pb affichage feti dans preml1 !
"""),

27: _("""
 solveur interne ldlt pour l'instant proscrit  avec feti
"""),

28: _("""
 solveur interne mumps pour l'instant proscrit  avec feti
"""),

29: _("""
 solveur interne gcpc pour l'instant proscrit  avec feti
"""),

30: _("""
 matrices a et b incompatibles pour l operation * 
"""),

31: _("""
 la section de la poutre doit etre constante.
"""),

32: _("""
 structure non tubulaire
"""),

33: _("""
 on ne traite pas ce type de cham_elem, icoef different de 1
"""),

34: _("""
 le cham_no :  %(k1)s  n"existe pas
"""),

35: _("""
 la matr_asse  %(k1)s  n"est pas stockee "morse"
"""),

36: _("""
 conflit une matrice stockee morse ne peut avoir q'un seul bloc
"""),

37: _("""
  gcpc n"est pas prevu pour une matrice complexe
"""),

38: _("""
 pas de matrice de preconditionnement : on s'arrete
"""),

39: _("""
  le cham_no : %(k1)s  n"existe pas ==> reprise impossible ==> initialisation par le vecteur nul
"""),

40: _("""
 erreur : lmat est nul
"""),

41: _("""
 la matrice possede des ddls imposes elimines: il faut un vcine
"""),

42: _("""
  la matrice et le vecteur cinematique ne contiennent pas des valeurs de meme type
"""),

43: _("""
 la matrice et le second membre ne contiennent pas des valeurs de meme type
"""),

44: _("""
  la methode de resolution:  %(k1)s  est inconnue. on attend ldlt,gcpc, mult_fro ou feti
"""),

45: _("""
 methode de bathe et wilson : convergence non atteinte
"""),

46: _("""
 recherche de corps rigide pour l'instant proscrite  avec matrice non-symetrique
"""),

47: _("""
 recherche de corps rigide pour l'instant proscrite  avec matrice complexe
"""),

48: _("""
 nom_nume_ddl  %(k1)s  non trouve
"""),

49: _("""
 attention plus de six modes de corps rigides detectes
"""),

50: _("""
 ! attention  %(k1)s .valf existe deja !
"""),

51: _("""
 le tableau b est insuffisamment dimensionne pour l operation * 
"""),

52: _("""
 augmenter la taille de la pile
"""),

53: _("""
 toutes les frequencessont des frequences de corps rigide
"""),

54: _("""
 calcul des nume_modematrice non inversible pour la frequence consideree
"""),

55: _("""
 probleme a la resolution du systeme reduit.
"""),

56: _("""
 valeur propre infinie trouvee
"""),

57: _("""
 methode qr : probleme de convergence 
"""),

58: _("""
 il y a des valeurs propres tres proches
"""),

59: _("""
 il y a des vp tres proches
"""),

60: _("""
 la matrice    :  %(k1)s  a une numerotation incoherente avec le nume_ddl.
"""),

61: _("""
 le concept mode " %(k1)s " a ete cree avec les matrices    matr_a:  %(k2)s , matr_b:  %(k3)s , matr_c:  %(k4)s  et non avec celles  passees en arguments.
"""),

62: _("""
 le concept mode " %(k1)s " a ete cree avec les matrices    matr_a:  %(k2)s , matr_b:  %(k3)s  et non avec celles  passees en arguments.
"""),

63: _("""
 le systeme a resoudre n'a pas de ddl actif.
"""),

64: _("""
 on trouve plus de 9999valeurs propres dans la bande demandee
"""),

65: _("""
 la matrice de raideur est numeriquement singuliere (malgre la strategie de decalage) la valeur de decalage est une valeur propre ou la matrice est non inversible.
"""),

66: _("""
  -> La borne minimale de la bande de fréquences est une valeur propre !
     Malgré la stratégie de décalage, la matrice de raideur est numériquement
     singulière.
  -> Risque & Conseil :
     Augmenter (ou diminuer) la fréquence (ou la charge critique dans le cas du calcul de
     flambement) qui définit la borne minimale de la bande de fréquence.
"""),

67: _("""
 la matrice de raideur est numeriquement singuliere (malgre la strategie de decalage) la borne maximale de la bande est une valeur propre. on poursuit tout de meme
"""),

68: _("""
 la matrice de raideur est singuliere malgre la strategie de decalage (ie structure avec des modes de corps solide). 
"""),

69: _("""
 option  %(k1)s non reconnue.
"""),

70: _("""
 type des valeurs  variable d'un mode a l'autre,  recuperation impossible.
"""),

71: _("""
 nombre d'equations variable d'un mode a l'autre,  recuperation impossible.
"""),

72: _("""
 probleme interne arpack
"""),

73: _("""
 probleme taille workd/l -> augmenter dim_sous_espace
"""),

74: _("""
 probleme interne lapack
"""),

75: _("""
 probleme construction vecteur initial --> si possible diminuer nmax_freq 
"""),

76: _("""
 probleme interne lapack, routine flahqr (forme de schur)
"""),

77: _("""
 probleme interne lapack, routine ftrevc (vecteurs propres)
"""),

78: _("""
 aucune valeur propre a la precision requise --> augmenter prec_soren ou nmax_iter_soren ou augmenter dim_sous_espace
"""),

79: _("""
 la position modale d'une des frequences est negative ou nulle, votre systeme matriciel est surement fortement singulier (ceci correspond generalement a un probleme dans la modelisation).
"""),

80: _("""
 mode a creer avant appel a vpstor
"""),

81: _("""
 " %(k1)s "  argument du mot cle "option" pour le calcul des frequences est invalide.
"""),

82: _("""
 pour l'option  "bande" il faut exactement 2 frequences.
"""),

83: _("""
 frequence min. plus grande ou egale a la frequence max.
"""),

84: _("""
 pour l'option  "centre" il faut exactement 1 frequence.
"""),

85: _("""
 pour l'option  "plus_petite" les frequences de "freq" sont ignorees.
"""),

86: _("""
 pour l'option  "bande" il faut exactement 2 charges critiques.
"""),

87: _("""
 charge crit. min. plus  grande ou egale a la charge crit. max.
"""),

88: _("""
 pour l'option  "centre" il faut exactement 1 charge critique.
"""),

89: _("""
 pour l'option  "plus_petite" les charges critiques de "char_crit" sont ignorees.
"""),

90: _("""
 objet .refe/.refa/.celk inexistant.
"""),

91: _("""
 cham_no non feti !
"""),

92: _("""
 liste de cham_no a concatener heterogene
"""),

93: _("""
 les cham_nos  %(k1)s  et  %(k2)s  sont de type inconnu  %(k3)s 
"""),

94: _("""
 le cham_no  %(k1)s  de type  %(k2)s  ne peut etre copie dans le cham_no  %(k3)s  de type  %(k4)s 
"""),

95: _("""
 champ a representation constante non traite.
"""),

96: _("""
 chout non feti !
"""),

97: _("""
 type de tri inconnu
"""),

98: _("""
 probleme interne lapack, routine dlahqr (forme de schur)
"""),

99: _("""
 probleme interne lapack, routine dtrevc (vecteurs propres)
"""),
}
