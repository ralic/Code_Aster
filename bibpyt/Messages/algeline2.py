#@ MODIF algeline2 Messages  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
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

2: _("""
 l une des valeurs propres de la matrice du probleme modal generalise en eau au repos n est pas reelle
"""),

3: _("""
 calcul des modes en eau au repos : une des valeurs propres obtenues est nulle
"""),

4: _("""
 erreur sur la recherche des lagranges.
"""),

5: _("""
 mot cle facteur incorrect.
"""),

6: _("""
 type de matrice " %(k1)s " inconnu.
"""),

7: _("""
 on ne traite pas cette option.
"""),

8: _("""
 on ne peut pas combiner une matrice non symetrique dans une matrice symetrique.
"""),

9: _("""
 les matrices a combiner ne sont pas construites sur le meme maillage
"""),

10: _("""
 charges cinematiques differentes.
"""),

11: _("""
 les "matass" " %(k1)s "  et  " %(k2)s "  n'ont le meme domaine de definition.
"""),

12: _("""
 attention: une dimesion nulle  ou nmax.lt.dmax(1,n) 
"""),

13: _("""
 attention: une dimension negative ou nulle 
"""),

14: _("""
 attention: les dimensions des  tableaux ne sont pas correctes
"""),

15: _("""
 pas de charge critique  dans l intervalle demande
"""),

16: _("""
  %(k1)s charges critiques  dans l intervalle demande
"""),

17: _("""
 au moins une frequence calculee exterieure a la bande demandee
"""),

18: _("""
 les matrices " %(k1)s " et " %(k2)s " n'ont pas le meme domaine de definition
"""),

19: _("""
 problemes a l'allocation des descripteurs de la matrice " %(k1)s " 
"""),

20: _("""
 l'argument de "bloc_debut" est plus grand que le nombre de bloc de la matrice
"""),

21: _("""
 l'argument de "bloc_fin" doit etre strictement positif
"""),

22: _("""
 la numerotation des inconnues est incoherente entre la matrice et le second membre.
"""),

23: _("""
  %(k1)s  et  %(k2)s  n'ont pas le meme domaine de definition.
"""),

24: _("""
 la matrice a des ddls elimines. il faut utiliser le mot cle cham_cine.
"""),

25: _("""
 la matrice et le second membre sont de type different.
"""),

26: _("""
 le second membre et le champ cinematiquesont de type different.
"""),

27: _("""
 la matrice est d'un type inconnu de l'operateur.
"""),

28: _("""
 err_31a: les "matr_asse" %(k1)s "  et  " %(k2)s "  ne sont pas combinables.
"""),

29: _("""
 la valeur d'entree min et superieure ou egale a la valeur d'entree sup
"""),

30: _("""
 les  matrices  " %(k1)s "  et  " %(k2)s "  n'ont pas le meme domaine de definition.
"""),

31: _("""
 trop de re-ajustement de la borne minimale.
"""),

32: _("""
 trop de re-ajustement de la borne maximale.
"""),

33: _("""
 type de mode inconnu:  %(k1)s 
"""),

34: _("""
 il n'est pas permis de modifier un objet pere
"""),

35: _("""
 mode non calcule a partir de matrices assemblees
"""),

36: _("""
 normalisation impossible, le point n'est pas present dans le modele.
"""),

37: _("""
 normalisation impossible, la composante n'est pas presente dans le modele.
"""),

38: _("""
 manque des parametres entiers
"""),

39: _("""
 manque des parametres reels
"""),

40: _("""
 manque des parametres caracteres
"""),

41: _("""
 normalisation impossible,  aucune composante n'est presente dans le modele.
"""),

42: _("""
 normalisation impossible, le noeud n'est pas present dans le modele.
"""),

43: _("""
 on ne tient pas compte du mot cle facteur "mode_signe" pour des "mode_meca_c"
"""),

44: _("""
 " %(k1)s "  type de mode non traite
"""),

45: _("""
 calcul de flambement et absence du mot cle char_crit ne sont pas compatibles
"""),

46: _("""
 calcul de flambement et matrice d'amortissement ne sont pas compatibles
"""),

47: _("""
 le nombre de frequences demandees est incorrect.
"""),

48: _("""
 nmax_iter_ ajuste ou separe est negatif
"""),

49: _("""
 nmax_iter est negatif
"""),

50: _("""
 prec_ ajuste ou separe est irrealiste 
"""),

51: _("""
 prec est irrealiste (inferieure a 1.e-70)
"""),

52: _("""
 pas de valeur donnee, separation impossible
"""),

53: _("""
 une seule valeur donnee, separation impossible
"""),

54: _("""
 la suite des valeurs donnees n'est pas croissante 
"""),

55: _("""
 mot cle amor_reduit impossible pour cas generalise 
"""),

56: _("""
 mot cle amor_reduit impossible si option differente               de proche
"""),

57: _("""
 nombre different d'arguments entre les mots cles amor_reduit et freq
"""),

58: _("""
 les matrices " %(k1)s " et  " %(k2)s "  sont incompatibles entre elles
"""),

59: _("""
 presence de frequences negatives dans les donnees.
"""),

60: _("""
  trop de reajustement d'une borne de l'intervalle de recherche.
"""),

61: _("""
 erreur trop de reajustementd'une borne de l'intervalle de recherche.
"""),

62: _("""
 pas de valeurs propres dans la bande de calcul,  le concept ne peut etre cree dans ces conditions.
"""),

63: _("""
 " %(k1)s "   option inconnue.
"""),

64: _("""
 le nombre param_ortho_soren n'est pas valide. 
"""),

65: _("""
 detection des modes de corps rigide n'est utilisee qu'avec tri_diag
"""),

66: _("""
 option bande non autorisee pour un probleme avec amortissement
"""),

67: _("""
 approche imaginaire ou complexe et frequence nulle incompatible
"""),

68: _("""
  option modes de corps rigide non utilisee avec amortissement
"""),

69: _("""
 pour le probleme generalise ou quadratique complexe on utilise seulement l'algorithme de sorensen
"""),

70: _("""
 probleme complexe et frequence nulle incompatible
"""),

71: _("""
 calcul quadratique par la methode de sorensen et frequence nulle incompatible
"""),

72: _("""
 la dimension du sous espace de travail est inferieure au nombre de modes rigides
"""),

73: _("""
 pas de verification par sturm pour le probleme quadratique
"""),

74: _("""
  erreur de verification 
"""),

75: _("""
 conclusion du utmess-e precedent
"""),

76: _("""
 3 ou 6 valeurs pour le mot cle "direction"
"""),

77: _("""
 pour le mot cle facteur  "pseudo_mode", il faut donner la matrice de masse.
"""),

78: _("""
 la direction est nulle.
"""),

79: _("""
 base modale 1 et 2 avec numerotations de taille incompatible
"""),

80: _("""
 base modale 1 et 2 avec numerotations incompatibles
"""),

81: _("""
 base modale et matrice avec numerotations incompatibles
"""),

82: _("""
 nombre de modes et d amortissements differents
"""),

83: _("""
 nombre de modes et d amortissements de connors differents
"""),

84: _("""
 nombre d amortissements different du nombre de modes calcules
"""),

85: _("""
 inversion vmin <=> vmax
"""),

86: _("""
 type de matrice inconnu 
"""),

87: _("""
  pas de produit car le cham_no  %(k1)s  existe deja.
"""),








89: _("""
 le mot-cle maillage est obligatoire avec le mot-cle crea_fiss. 
"""),

90: _("""
 le mot-cle maillage est obligatoire avec le mot-cle line_quad. 
"""),

91: _("""
 crea_maillage : l'option line_quad ne traite pas les macros mailles
"""),

92: _("""
 crea_maillage : l'option line_quad ne traite pas les absc_curv
"""),

93: _("""
 le mot-cle maillage est obligatoire avec le mot-cle quad_line. 
"""),

94: _("""
 crea_maillage : l'option quad_line ne traite pas les macros mailles
"""),

95: _("""
 crea_maillage : l'option quad_line ne traite pas les absc_curv
"""),

96: _("""
 le mot-cle maillage est obligatoire avec le mot-cle modi_maille. 
"""),

97: _("""
 une seule occurrence de "quad_tria3"
"""),

98: _("""
 le mot-cle maillage est obligatoire avec le mot-cle coqu_volu. 
"""),

99: _("""
 pas de maille a modifier
"""),
}
