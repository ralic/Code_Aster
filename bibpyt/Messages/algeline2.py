#@ MODIF algeline2 Messages  DATE 09/10/2007   AUTEUR COURTOIS M.COURTOIS 
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

def _(x) : return x

cata_msg = {

2 : _("""
 Calcul des modes en eau au repos :
 une des valeurs propres de la matrice n'est pas réelle
"""),

3 : _("""
 Calcul des modes en eau au repos :
 une des valeurs propres obtenues est nulle
"""),

4 : _("""
 Erreur sur la recherche des lagranges
"""),

5 : _("""
 mot cle facteur incorrect
"""),

6 : _("""
 Type de matrice " %(k1)s " inconnu.
"""),

7 : _("""
 On ne traite pas cette option
"""),

8 : _("""
 On ne peut pas combiner une matrice non symétrique dans une matrice symétrique
"""),

9 : _("""
 Les matrices à combiner ne sont pas construites sur le meme maillage.
"""),

10 : _("""
 Erreur de programmation :
 On cherche à combiner 2 matrices qui n'ont pas les memes charges cinématiques.
 Noms des 2 matrices :
    %(k1)s
    %(k2)s

 Solution :
    1) émettre une fiche d'anomalie / évolution
    2) En attendant : ne pas utiliser de charges cinématiques :
       remplacer AFFE_CHAR_CINE par AFFE_CHAR_MECA
"""),

11 : _("""
 Les matrices "%(k1)s"  et  "%(k2)s"  n'ont pas la meme structure.
"""),

12 : _("""
 Résolution système linéaire méthode de CROUT
 Attention: une dimension nulle ou nmax.lt.dmax(1,n)
"""),

13 : _("""
 Résolution système linéaire méthode de CROUT
 Attention: une dimension negative ou nulle
"""),

14 : _("""
 Résolution système linéaire méthode de CROUT
 Attention: les dimensions des tableaux ne sont pas correctes
"""),

15 : _("""
 Pas de charge critique  dans l'intervalle demandé
"""),

16 : _("""
  %(k1)s charges critiques  dans l'intervalle demandé
"""),

17 : _("""
 Au moins une fréquence calculée extérieure à la bande demandée
"""),

18 : _("""
 Les matrices " %(k1)s " et " %(k2)s " n'ont pas le meme domaine de définition
"""),

19 : _("""
 Problèmes a l'allocation des descripteurs de la matrice " %(k1)s "
"""),

20 : _("""
 L'argument de "BLOC_DEBUT" est plus grand que le nombre de blocs de la matrice
"""),

21 : _("""
 L'argument de "BLOC_FIN" doit etre strictement positif
"""),

22 : _("""
 La numérotation des inconnues est incohérente entre la matrice et le second membre.
"""),

23 : _("""
  %(k1)s  et  %(k2)s  n'ont pas le meme domaine de définition.
"""),

24 : _("""
 La matrice a des ddls eliminés. il faut utiliser le mot clé CHAM_CINE.
"""),

25 : _("""
 La matrice et le second membre sont de type différent.
"""),

26 : _("""
 le second membre et le champ cinématique sont de type différent.
"""),

27 : _("""
 la matrice est d'un type inconnu de l'opérateur.
"""),

28 : _("""
 les "MATR_ASSE" %(k1)s "  et  " %(k2)s "  ne sont pas combinables.
"""),

29 : _("""
 la valeur d'entrée 'min' est supérieure ou égale à la valeur d'entrée 'sup'
"""),

30 : _("""
 les matrices  " %(k1)s "  et  " %(k2)s "  n'ont pas le meme domaine de définition.
"""),

31 : _("""
 trop de ré-ajustement de la borne minimale.
"""),

32 : _("""
 trop de ré-ajustement de la borne maximale.
"""),

33 : _("""
 type de mode inconnu:  %(k1)s
"""),

34 : _("""
 il n'est pas permis de modifier un objet père
"""),

35 : _("""
 mode non calculé à partir de matrices assemblées
"""),

36 : _("""
 normalisation impossible, le point n'est pas present dans le modèle.
"""),

37 : _("""
 normalisation impossible, la composante n'est pas présente dans le modèle.
"""),

38 : _("""
 il manque des paramètres entiers
"""),

39 : _("""
 il manque des paramètres réels
"""),

40 : _("""
 manque des parametres caracteres
"""),

41 : _("""
 normalisation impossible,  aucune composante n'est présente dans le modèle.
"""),

42 : _("""
 normalisation impossible, le noeud n'est pas présent dans le modèle.
"""),

43 : _("""
 on ne tient pas compte du mot cle facteur "MODE_SIGNE" pour des "MODE_MECA_C"
"""),

44 : _("""
 " %(k1)s "  type de mode non traité
"""),

45 : _("""
 calcul de flambement et absence du mot cle char_crit ne sont pas compatibles
"""),

46 : _("""
 calcul de flambement et matrice d'amortissement ne sont pas compatibles
"""),

47 : _("""
 le nombre de frequences demandees est incorrect.
"""),

48 : _("""
 nmax_iter_ ajuste ou separe est negatif
"""),

49 : _("""
 nmax_iter est negatif
"""),

50 : _("""
 prec_ ajuste ou separe est irrealiste
"""),

51 : _("""
 prec est irrealiste (inferieure a 1.e-70)
"""),

52 : _("""
 pas de valeur donnee, separation impossible
"""),

53 : _("""
 une seule valeur donnee, separation impossible
"""),

54 : _("""
 la suite des valeurs donnees n'est pas croissante
"""),

55 : _("""
 mot cle amor_reduit impossible pour cas generalise
"""),

56 : _("""
 mot cle amor_reduit impossible si option differente               de proche
"""),

57 : _("""
 nombre different d'arguments entre les mots cles amor_reduit et freq
"""),

58 : _("""
 les matrices " %(k1)s " et  " %(k2)s "  sont incompatibles entre elles
"""),

59 : _("""
 presence de frequences negatives dans les donnees.
"""),

60 : _("""
  trop de reajustement d'une borne de l'intervalle de recherche.
"""),

61 : _("""
 erreur trop de reajustementd'une borne de l'intervalle de recherche.
"""),

62 : _("""
 pas de valeurs propres dans la bande de calcul,  le concept ne peut etre cree dans ces conditions.
"""),

63 : _("""
 " %(k1)s "   option inconnue.
"""),

64 : _("""
 le nombre param_ortho_soren n'est pas valide.
"""),

65 : _("""
 detection des modes de corps rigide n'est utilisee qu'avec tri_diag
"""),

66 : _("""
 option bande non autorisee pour un probleme avec amortissement
"""),

67 : _("""
 approche imaginaire ou complexe et frequence nulle incompatible
"""),

68 : _("""
  option modes de corps rigide non utilisee avec amortissement
"""),

69 : _("""
 pour le probleme generalise ou quadratique complexe on utilise seulement l'algorithme de sorensen
"""),

70 : _("""
 probleme complexe et frequence nulle incompatible
"""),

71 : _("""
 calcul quadratique par la methode de sorensen et frequence nulle incompatible
"""),

72 : _("""
 la dimension du sous espace de travail est inferieure au nombre de modes rigides
"""),

73 : _("""
 pas de verification par sturm pour le probleme quadratique
"""),

74 : _("""
  erreur de verification
"""),

75 : _("""
 conclusion du utmess-e precedent
"""),

76 : _("""
 3 ou 6 valeurs pour le mot cle "direction"
"""),

77 : _("""
 pour le mot cle facteur  "pseudo_mode", il faut donner la matrice de masse.
"""),

78 : _("""
 la direction est nulle.
"""),

79 : _("""
 base modale 1 et 2 avec numerotations de taille incompatible
"""),

80 : _("""
 base modale 1 et 2 avec numerotations incompatibles
"""),

81 : _("""
 base modale et matrice avec numerotations incompatibles
"""),

82 : _("""
 nombre de modes et d amortissements differents
"""),

83 : _("""
 nombre de modes et d amortissements de connors differents
"""),

85 : _("""
 inversion vmin <=> vmax
"""),

86 : _("""
 type de matrice inconnu
"""),

87 : _("""
  pas de produit car le cham_no  %(k1)s  existe deja.
"""),

88 : _("""
  Problème de programmation :
    La matrice globale %(k1)s n'existe pas.
    Elle est nécessaire pour déterminer les ddls bloqués par AFFE_CHAR_CINE.

  Solution (pour l'utilisateur) :
    1) Ne pas utiliser de charges cinématiques (AFFE_CHAR_CINE)
    2) Emettre une fiche d'anomalie.

  Solution (pour le programmeur) :
    La matrice globale a été détruite abusivement.
    Instrumenter la routine jedetr.f pour déterminer la routine coupable.
"""),

89 : _("""
 le mot-cle maillage est obligatoire avec le mot-cle crea_fiss.
"""),

90 : _("""
 le mot-cle maillage est obligatoire avec le mot-cle line_quad.
"""),

91 : _("""
 crea_maillage : l'option line_quad ne traite pas les macros mailles
"""),

92 : _("""
 crea_maillage : l'option line_quad ne traite pas les absc_curv
"""),

93 : _("""
 le mot-cle maillage est obligatoire avec le mot-cle quad_line.
"""),

94 : _("""
 crea_maillage : l'option quad_line ne traite pas les macros mailles
"""),

95 : _("""
 crea_maillage : l'option quad_line ne traite pas les absc_curv
"""),

96 : _("""
 le mot-cle maillage est obligatoire avec le mot-cle modi_maille.
"""),

97 : _("""
 une seule occurrence de "quad_tria3"
"""),

98 : _("""
 le mot-cle maillage est obligatoire avec le mot-cle coqu_volu.
"""),

99 : _("""
 pas de maille a modifier
"""),

}
