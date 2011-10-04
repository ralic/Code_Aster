#@ MODIF elements2 Messages  DATE 03/10/2011   AUTEUR DELMAS J.DELMAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg = {

27 : _("""
 pas d'intersection trouvé
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
 noeuds confondus pour la maille:  %(k1)s
"""),

44 : _("""
  option de matrice de masse  %(k1)s  inconnue
"""),

45 : _("""
 on n'a pas trouvé de variable interne correspondante a la déformation plastique équivalente cumulée
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
 charge répartie variable non admise sur un élément courbe.
"""),

50 : _("""
 charge répartie variable non admise sur un élément variable.
"""),

51 : _("""
 on ne peut pas imposer de charges réparties suiveuses de type vitesse de vent sur les poutres courbes.
"""),

52 : _("""
 on ne peut pas imposer de charges réparties suiveuses sur les poutres courbes.
"""),

53 : _("""
 un champ de vitesse de vent est imposé sans donner un cx dépendant de la vitesse sur une des poutres.
"""),

54 : _("""
 le module de cisaillement G est nul mais pas le module d'Young E
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

61 : _("""
 " %(k1)s "   nom d'option non reconnue
"""),

62 : _("""
 ! pb rcvala rhocp !
"""),

63 : _("""
 ! comportement non trouve !
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
     d'utiliser la modélisation COQUE_3D, avec DEFORMATION='GROT_GDEP'
"""),

73 : _("""
 comportement non traite:  %(k1)s
"""),

74 : _("""
  %(k1)s  non implante.
"""),

77 : _("""
 option :  %(k1)s  interdite
"""),

80 : _("""
 éléments de poutre noeuds confondus pour un élément:  %(k1)s
"""),

81 : _("""
 éléments de poutre section variable affine :seul une section rectangle plein est disponible.
"""),

82 : _("""
 éléments de poutre section variable homothétique : l'aire initiale est nulle.
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

90 : _("""
 COMP_ELAS non valide
"""),

}
