#@ MODIF elements2 Messages  DATE 09/10/2007   AUTEUR COURTOIS M.COURTOIS 
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
 le mot cle normale doit comporter 3 composantes
"""),

3 : _("""
 le degre du polynome doit etre                            au plus egal a 7 
"""),

4 : _("""
 probleme dans rinf et rsup 
"""),

5 : _("""
 le mot cle dtan_orig doit comporter 3 composantes
"""),

6 : _("""
 le mot cle dtan_extr doit comporter 3 composantes
"""),

7 : _("""
 le mot cle vect_grno_orig doit comporter 2 groupes de points
"""),

8 : _("""
 le mot cle vect_grno_extr doit comporter 2 groupes de points
"""),

9 : _("""
 melang1 seg2 et seg3 : les mailles du fond de fissure doivent etre du meme type
"""),

10 : _("""
 le groupe de noeuds  %(k1)s  definissant la fissure n'est pas ordonne
"""),

11 : _("""
 arret sur erreur utilisateur: deux group_no consecutifs incoherents
"""),

12 : _("""
 les mailles du fond de fissure doivent etre du type segment
"""),

13 : _("""
 melang2 seg2 et seg3 : les mailles du fond de fissure doivent etre du meme type
"""),

14 : _("""
 arret sur erreur utilisateur: deux mailles dufond de fissure sont non consecutives dans la numerotation des noeuds 
"""),

15 : _("""
 arret sur erreur utilisateur: 2 group_ma du fond de fissure sontnon consecutifs dans la numerotation des noeuds
"""),

16 : _("""
 les mailles des levres doivent etre du type quadrangle ou triangle
"""),

17 : _("""
 melang3 seg2 et seg3 : les mailles du fond de fissure doivent etre du meme type
"""),

18 : _("""
 la liste de noeuds definissant la fissure n'est pas ordonnee
"""),

19 : _("""
 melang4 seg2 et seg3 : les mailles du fond de fissure doivent etre du meme type
"""),

20 : _("""
 arret sur erreur(s) utilisateur: deux mailles du fond de fissure sont non consecutives dans la numerotation des noeuds 
"""),

21 : _("""
 erreur : le fond de fissure possede un noeud repete 2 fois : noeud  %(k1)s . revoir les donnees
"""),

22 : _("""
 les mailles du fond_inf                           et du fond_sup sont de type different
"""),

24 : _("""
 le noeud  %(k1)s  n appartient pas au fond de fissure 
"""),

25 : _("""
 le fond de fissure n est pas complet
"""),

26 : _("""
 pb prgm
"""),

27 : _("""
 no intersection found
"""),

28 : _("""
 indc = 1 (complet   : translation et rotation) ou                 indc = 0 (incomplet : translation seulement  )                    obligatoirement.
"""),

29 : _("""
 element faisceau homogeneise non prevu
"""),

31 : _("""
  elrefe non prevu
"""),

32 : _("""
 comportement non trouve: %(k1)s 
"""),

33 : _("""
 pas de dilatation thermique orthotrope pour coque_3d
"""),

34 : _("""
 les vecteurs sont au nombre de 1 ou 2
"""),

36 : _("""
 le type d'element :  %(k1)s n'est pas traite.
"""),

37 : _("""
 pas de zero, convergence impossible
"""),

38 : _("""
  ->  L'option ANGL_AXE n'est pas prise en compte en 2D mais seulement
      en 3D.
  -> Risque & Conseil :
     Ce mot clé utilisé dans l'opérateur AFFE_CARA_ELEM (MASSIF), permet
     de définir des axes locaux pour lesquels on utilise une propriété de
     symétrie de révolution, ou d'isotropie transverse. En 2D, on peut définir
     un repère d'orthotropie via ANGL_REP.
"""),

39 : _("""
 loi lema_seuil non implemente avec les poutres multi fibres
"""),

40 : _("""
 on ne sait pas integrer avec  %(k1)s  caracteristiques par fibre
"""),

41 : _("""
 cas avec inerties des fibres non programme
"""),

42 : _("""
 " %(k1)s "    nom d'element inconnu.
"""),

43 : _("""
 noeuds confondus pour un element:  %(k1)s 
"""),

44 : _("""
  option de matrice de masse  %(k1)s  inconnue
"""),

45 : _("""
 on n'a pas trouver de variable interne correspondante a la deformation plastiqueequivalente cumulee 
"""),

46 : _("""
 on ne traite pas les moments
"""),

47 : _("""
 l'option " %(k1)s " est inconnue
"""),

48 : _("""
 type de poutre inconnu
"""),

49 : _("""
 charge repartie variable non admise sur un element courbe.
"""),

50 : _("""
 charge repartie variable non admise sur un element variable.
"""),

51 : _("""
 on ne peut pas imposer de charges reparties suiveuses de type vitesse de vent sur les poutres courbes.
"""),

52 : _("""
 on ne peut pas imposer de charges reparties suiveuses sur les poutres courbes.
"""),

53 : _("""
 un champ de vitesse de vent est impose sans donner un cx dependant de la vitesse sur une des poutres.
"""),

54 : _("""
 g est nul mais pas e
"""),

55 : _("""
 section circulaire uniquement
"""),

56 : _("""
 pour l'instant on ne fait pas le calcul de la  matrice de masse d'un element de plaque q4g excentre.
"""),

57 : _("""
 pour l'instant on ne peut pas excentrer les elements q4g .
"""),

58 : _("""
 echec de convergence dans l'inversion du systeme par newton-raphson.
"""),

59 : _("""
 pb
"""),

60 : _("""
 mauvaise orientation de l element !
"""),

61 : _("""
 " %(k1)s "   nom d'option non reconnue
"""),

62 : _("""
 ! pb rcvala rhocp !
"""),

63 : _("""
 ! comportement non trouve !
"""),

64 : _("""
 ! pb rccoma rhocp !
"""),

65 : _("""
 ! deltat: div par zero !
"""),

66 : _("""
 ! r axi: div par zero !
"""),

67 : _("""
 option non traitee
"""),

68 : _("""
 une deformation initiale est presente dans la charge : incompatible avec la contrainte initiale sigma_init
"""),

69 : _("""
 relation de comportement non traite
"""),

70 : _("""
 option non valide
"""),

71 : _("""
 comp_elas non programme pour les modelisations dkt. il faut utiliser comp_incr.
"""),

72 : _("""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC' sous
     le mot clé COMP_INCR) est déconseillée pour les éléments de type plaque. Les
     grandes rotations ne sont pas modélisées correctement.
  -> Risque & Conseil :
     En présence de grands déplacements et grandes rotations, il est préférable
     d'utiliser la modélisation COQUE_3D, avec DEFORMATION='GREEN_GR'
"""),

73 : _("""
 comportement non traite:  %(k1)s 
"""),

74 : _("""
  %(k1)s  non implante.
"""),

75 : _("""
 option "sief_elno_elga" non implantee pour la deformation "green_gr"
"""),

76 : _("""
 la nature du materiau  %(k1)s  n'est pas traitee.
"""),

77 : _("""
 option :  %(k1)s  interdite
"""),

80 : _("""
 elements de poutre noeuds confondus pour un element:  %(k1)s 
"""),

81 : _("""
 elements de poutre section variable affine :seul rectangle plein disponible.
"""),

82 : _("""
 elements de poutre section variable homothetique : l'aire initiale est nulle.
"""),

83 : _("""
 poutre section variable/constante  passage par section homothetique avec a1 = a2 
"""),

84 : _("""
 elements de poutre l'option " %(k1)s " est inconnue
"""),

85 : _("""
 non prevu pour les sections rectangulaires
"""),

86 : _("""
 non prevu pour les sections generales
"""),

87 : _("""
 " %(k1)s "    : option non traitee
"""),

88 : _("""
 element discret " %(k1)s " inconnu.
"""),

89 : _("""
 element discret (te0044): l'option " %(k1)s " est inconnue pour le type " %(k2)s "
"""),

90 : _("""
 comp_elas non valide
"""),

91 : _("""
 " %(k1)s " matrice de decharge non developpee
"""),

92 : _("""
 la loi dis_gricra doit etre utilisee avec deselements du type meca_dis_tr_l: element seg2 + modelisation dis_tr
"""),

93 : _("""
 longueurs diff. pour rigi et amor
"""),

94 : _("""
 longueurs diff. pour mass et amor
"""),

95 : _("""
 option de calcul invalide
"""),

96 : _("""
 erreur calcul de texnp1
"""),

97 : _("""
 erreur calcul de texn
"""),

98 : _("""
 erreur calcul de echnp1
"""),

99 : _("""
 erreur calcul de echn
"""),

}
