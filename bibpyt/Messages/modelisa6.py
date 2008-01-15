#@ MODIF modelisa6 Messages  DATE 15/01/2008   AUTEUR PROIX J-M.PROIX 
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

1 : _("""
 probleme d'orientation: aucune maille ne touche le noeud indique.
"""),

2 : _("""
 certaines mailles n'ont pas pu etre reorientees. l'ensemble des mailles n'est pas connexe.
"""),

3 : _("""
 on ne trouve pas de noeud assez pres du noeud  %(k1)s 
"""),

4 : _("""
  erreurs dans les donnees
"""),

5 : _("""
 extraction de plus de noeuds que n"en contient la maille
"""),

6 : _("""
  nombre de noeuds negatif
"""),

7 : _("""
 nombre de noeuds sommets non prevu
"""),

8 : _("""
  on est sur 2 mailles orthgonales
"""),

9 : _("""
 type de maille inconnue
"""),

10 : _("""
 la maille  %(k1)s  ne fait pas partie du maillage  %(k2)s 
"""),

11 : _("""
 pref_maille est trop long, pref_nume est trop grand.
"""),

12 : _("""
 pref_maille est trop long
"""),

13 : _("""
 sous  %(k1)s  : (  %(k2)s  le groupe  %(k3)s ne fait pas partie du maillage :  %(k4)s 
"""),

14 : _("""
 sous  %(k1)s  : (  %(k2)s  arret sur erreur(s) utilisateur.
"""),

15 : _("""
 sous  %(k1)s  : (  %(k2)s  : la maille  %(k3)s ne fait pas partie du maillage :  %(k4)s 
"""),

16 : _("""
 le noeuds :  %(k1)s  ne fait pas partie du maillage
"""),

17 : _("""
 la maille  %(k1)s  du group_ma  %(k2)s donne apres le mot cle  %(k3)s n'a pas un type geometrique autorise.
"""),

18 : _("""
 la maille  %(k1)s donne apres le mot cle  %(k2)s n'a pas un type geometrique autorise.
"""),

19 : _("""
  mot cle non admis : %(k1)s  les mots-cles admissibles sont :  %(k2)s  ou  %(k3)s  ou  %(k4)s  ou  %(k5)s  ou  %(k6)s  ou  %(k7)s  ou  %(k8)s ou  %(k9)s 
"""),

20 : _("""
 ce type de maille n"est pas encore traite :  %(k1)s 
"""),

21 : _("""
 le nombre toal de noeuds est /= de la somme des noeuds sommets,arretes et interieurs
"""),

22 : _("""
 les 2 listes %(k1)s  et  %(k2)s  ne sont pas de meme longueur
"""),

26 : _("""
 affe_fibre pour " %(k1)s ": il y a  %(k2)s  valeurs pour "vale", ce devrait etre un multiple de 3
"""),

27 : _("""
 dans le maillage " %(k1)s " la maille " %(k2)s " est de type " %(k3)s " (ni tria3 ni quad4)
"""),

30 : _("""
 l'indicateur :  %(k1)s de position des multiplicateurs de lagrange associes a une relation lineaire n'est pas correct.
"""),

33 : _("""
 il faut coef_group ou fonc_group
"""),

34 : _("""
 un element n'est ni tria3 ni tria6 ni quad4 ni quad8
"""),

35 : _("""
 un element n'est ni tria3 ni tria6 ni tria7 niquad4 ni quad8 ni quad9
"""),

36 : _("""
  le noeud  %(k1)s  doit appartenir a une seule maille
"""),

37 : _("""
 la maille a laquelle appartient le noeud  %(k1)s  doit etre de type seg3 
"""),

38 : _("""
 on ne trouve pas les angles nautiques pour le tuyau
"""),

39 : _("""
 option  %(k1)s  invalide
"""),

40 : _("""
 il faut indiquer le mot-cle 'noeud_2' ou 'group_no_2' apres le mot-facteur  %(k1)s  pour l'option '3d_pou'.
"""),

41 : _("""
 il ne faut donner qu'un seul noeud de poutre a raccorder au massif.
"""),

42 : _("""
 il ne faut donner qu'un un seul group_no a un noeud a raccorder au massif.
"""),

43 : _("""
 il ne faut donner q"un  seul noeud dans le group_no :  %(k1)s 
"""),

44 : _("""
 impossibilite,le noeud  %(k1)s porte le ddl de rotation  %(k2)s 
"""),

45 : _("""
 impossibilite,le noeud poutre  %(k1)s  devrait porter le ddl  %(k2)s 
"""),

46 : _("""
 impossibilite, la surface de raccord du massif est nulle 
"""),

47 : _("""
 il faut donner un cara_elem pour recuperer les caracteristiques de tuyau.
"""),

48 : _("""
 il faut indiquer le mot-cle 'noeud_2' ou 'group_no_2' apres le mot-facteur  %(k1)s  pour l'option  %(k2)s 
"""),

49 : _("""
 il ne faut donner qu'un seul noeud de poutre a raccorder a la coque.
"""),

50 : _("""
 il ne faut donner qu'un un seul group_no a un noeud a raccorder a la coque.
"""),

51 : _("""
 il faut donner un vecteur orientant l'axe de la poutre sous le mot-cle "axe_poutre".
"""),

52 : _("""
 il faut donner un vecteur non nul orientant l'axe de la poutre sous le mot-cle "axe_poutre".
"""),

53 : _("""
 il faut donner un cara_elem pour recuperer l'epaisseur des elements de bord.
"""),

54 : _("""
 impossibilite,le noeud  %(k1)s ne porte pas le ddl de rotation  %(k2)s 
"""),

55 : _("""
 impossibilite, la surface de raccord de la coque est nulle 
"""),

56 : _("""
 plusieurs comportements de type  %(k1)s  ont ete trouves
"""),

57 : _("""
 comportement de type  %(k1)s  non trouve
"""),

58 : _("""
 nappe interdite pour les caracteristiques materiau
"""),

59 : _("""
 deformation plastique cumulee p < 0
"""),

60 : _("""
  prolongement a droite exclu pour la fonction r(p)
"""),

61 : _("""
 on deborde a droite redefinissez vos nappes alpha - moment
"""),

62 : _("""
 la limite d elasticite est deja renseignee dans elas_meta
"""),

63 : _("""
 objet  %(k1)s .materiau.nomrc non trouve 
"""),

64 : _("""
 type sd non traite:  %(k1)s 
"""),

66 : _("""
 fonction constante interdite pour la courbe mz=f(drz)
"""),

67 : _("""
 fonction nappe interdite pour la courbe mz=f(drz)
"""),

68 : _("""
 concept de type :  %(k1)s  interdit pour la courbe de traction
"""),

69 : _("""
 le mot cle: %(k1)s  est identique (sur ses 8 1ers caracteres) a un autre.
"""),

70 : _("""
 erreur lors de la definition de la courbe de traction, il manque le parametre : %(k1)s 
"""),

71 : _("""
 erreur lors de la definition de la courbe de traction : %(k1)s  nb de points < 2  ! 
"""),

72 : _("""
 erreur lors de la definition de la courbe de traction : %(k1)s  nb de points < 1  ! 
"""),

73 : _("""
 erreurs rencontrees.
"""),

74 : _("""
 erreur lors de la definition de la nappe des courbes de traction: nb de points < 2 ! 
"""),

75 : _("""
 erreur lors de la definition de la nappe des courbes de traction:  %(k1)s  nb de points < 1 ! 
"""),

76 : _("""
  erreur lors de la definition dela courbe de traction: fonction ou nappe ! 
"""),

77 : _(""" 
"""),

78 : _("""
 abscisses non croissants.
"""),

79 : _("""
  erreur lors de la definition dela courbe rela_mz: fonction ! 
"""),

80 : _("""
 comportement traction non trouve
"""),

81 : _("""
 fonction sigm non trouvee
"""),

82 : _("""
 comportement meta_traction non trouve
"""),

83 : _("""
 fonction sigm_f1 non trouvee
"""),

84 : _("""
 fonction sigm_f2 non trouvee
"""),

85 : _("""
 fonction sigm_f3 non trouvee
"""),

86 : _("""
 fonction sigm_f4 non trouvee
"""),

87 : _("""
 fonction sigm_c non trouvee
"""),

88 : _("""
 fonction constante interdite pour la courbe de traction %(k1)s 
"""),

89 : _("""
 prolongement a gauche exclu pour la courbe  %(k1)s 
"""),

90 : _("""
 prolongement a droite exclu pour la courbe  %(k1)s 
"""),

91 : _("""
 concept de type :  %(k1)s  interdit pour la courbe de traction %(k2)s 
"""),

92 : _("""
 materiau : %(k1)s  non trouve
"""),

93 : _("""
  les fonctions complexes ne sont pas implementees
"""),

94 : _("""
 nb param. > 30 materiau  %(k1)s 
"""),

95 : _("""
 mauvaise definition de la plage de frequence, aucun mode pris en compte
"""),

96 : _("""
 les mailles imprimees ci-dessus n'appartiennent pas au modele et pourtant elles ont ete affectees dans le mot-cle facteur : %(k1)s 
"""),

97 : _("""
 freq init plus grande que freq fin
"""),

98 : _("""
 freq init necessaire avec chamno
"""),

99 : _("""
 freq fin necessaire avec chamno
"""),

}
