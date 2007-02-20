#@ MODIF postrele Messages  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
 la courbe krd(lrd) coupee a plus de 100 points
"""),

2: _("""
 le niveau de contrainte depasse le niveau max autorise.
"""),

3: _("""
 on ne peut garantir la tenue du ligament.
"""),

4: _("""
 le defaut debouchant equivalent est donne dans le fichier resultat dans la meme unite de longueur que les donnees geometriques.
"""),

5: _("""
 la longueur du ligament plus la demi longueur de la fissure est superieure a la demi epaisseur : llig+(lfis/2)>(epai/2).
"""),

6: _("""
 la courbe lr_etoile a plus de 100 points
"""),

7: _("""
 le defaut debouchant maximal est trop long pour que l'on puisse calculer le facteur de forme correspondant.
"""),

8: _("""
 la courbe krd_etoil(lrd_etoil) a plus de 100 points
"""),

9: _("""
 lriet est inferieur ou egal a zero
"""),

10: _("""
 lriet est superieur a lrdmax-rho
"""),

11: _("""
 lrdet est inferieur ou egal a zero
"""),

12: _("""
 lrdet est superieur a lrdmax
"""),

13: _("""
 on a un arc ouvert et le "noeud_orig" n'est pas une extremite
"""),

14: _("""
 le "noeud_orig" ne fait pas parti du chemin
"""),

15: _("""
 il faut que les angles soient croissants
"""),

16: _("""
 il faut que les angles soient dans l'intervalle [-180,180]
"""),

17: _("""
 il faut un rayon strictement positif !
"""),

18: _("""
 face illegale
"""),

19: _("""
 maille illegale
"""),

20: _("""
 intersection de type inconnu
"""),

21: _("""
 detection de deux sommets confondus dans une meme face
"""),

22: _("""
 la commande "inte_mail_2d" suppose que le maillage est plan (z=cste) ce qui n'est pas le cas ici, utilisez "inte_mail_3d".
"""),

23: _("""
 creation/extension de la table  %(k1)s 
"""),

24: _("""
 aucun segment ne coupe le maillage
"""),

25: _("""
 il faut definir le comportement "RCCM" dans "DEFI_MATERIAU"
"""),

26: _("""
 manque le vecteur "nocp".
"""),

27: _("""
 on ne traite pas les multicouches
"""),

28: _("""
 chemin nul ou defini en un noeud
"""),

29: _("""
 le chemin est defini par plusieurs segments, modifier la tolerance dans inte_mail.
"""),

30: _("""
 pb pour recuperer ioc seisme
"""),

31: _("""
 il faut definir le comportement "ELAS" dans "DEFI_MATERIAU"
"""),

32: _("""
 il faut definir le comportement "FATIGUE" dans "DEFI_MATERIAU"
"""),

33: _("""
 il faut definir le comportement "RCCM" dans "DEFI_MATERIAU"
"""),

34: _("""
 absence du parametre absc_curv pour la table "tabl_mx"
"""),

35: _("""
 absence du parametre absc_curv pour la table  %(k1)s 
"""),

36: _("""
 "NUME_GROUPE" doit etre strictement positif
"""),

37: _("""
 pb 2
"""),

38: _("""
 materiau non defini, maille %(k1)s 
"""),

39: _("""
 un seul "NB_CYCL_SEISME"
"""),

40: _("""
 pb avec typeke
"""),

41: _("""
 on ne doit pas passer la
"""),

42: _("""
 si on est la, y a un bug
"""),

43: _("""
 absence du parametre INST pour la table tabl_moye_ther
"""),

44: _("""
 absence du parametre INST pour la table tabl_resu_ther
"""),

45: _("""
 absence du parametre ABSC_CURV pour la table tabl_resu_ther
"""),

46: _("""
 en repere local, on ne traite pas le champ  %(k1)s 
"""),

47: _("""
 icoef trop grand
"""),

48: _("""
 probleme maillage
"""),

49: _("""
 on ne traite que des champs de type "depl_r" pour un changement de repere
"""),

50: _("""
 maille de type non attendu
"""),

51: _("""
 mauvaise definition du chemin, probleme de continuite du chemin sur une maille, diminuer la precision dans l'operateur inte_mail
"""),

52: _("""
 on ne traite pas ce cas
"""),

53: _("""
 avec vect_y le groupe de noeuds doit contenir plusieurs noeuds
"""),

54: _("""
 avec VECT_Y il faut preciser soit un seul groupe de noeuds soit plusieurs noeuds
"""),

55: _("""
 on ne peut pas melanger des arcs et des segments.
"""),

56: _("""
 chemin de maille vide
"""),

57: _("""
 contradiction avec INTE_MAIL_2D
"""),

58: _("""
 initialisation de la table  %(k1)s 
"""),

59: _("""
 initialisation de la table associee  a la table  %(k1)s  et au parametre sensible  %(k2)s connue sous le nom de concept  %(k3)s 
"""),

60: _("""
 pas de champ trouve 
"""),

61: _("""
 parametre  %(k1)s de type  %(k2)s 
"""),

62: _("""
 le nombre de composantes a traiter est limite a 6 pour operation "MOYENNE". utiliser "NOM_CMP" avec au plus 6 composantes
"""),

63: _("""
 on ne traite que les complexes
"""),

64: _("""
 tableau de travail limite, reduire le nombre de composantes a traiter
"""),

65: _("""
 + de 3000 cmps!
"""),

66: _("""
 il manque la donnee de la limite d'elasticite (SY_02 ou SY_MAX) pour le calcul du rochet thermique
"""),

67: _("""
 le calcul du critere du rochet thermique pour une variation de temperature lineaire est impossible
        X = SIGM / SYMAX =  %(r1)12.5E
         SIGM =  %(r2)12.5E
        SYMAX =  %(r3)12.5E
        ON DOIT AVOIR 0. < X < 1.                                                                                                   
 
"""),

68: _("""
 le calcul du critere du rochet thermique pour une variation de temperature parabolique est impossible
        X = SIGM / SYMAX =  %(r1)12.5E
         SIGM =  %(r2)12.5E
        SYMAX =  %(r3)12.5E
        ON DOIT AVOIR 0.3 < X < 1.                                                                                                   
 
"""),

69: _("""
  erreur numero :  %(i1)d   %(k1)s occurence numero :  %(i2)d 
  maille numero :  %(i3)d 
"""),

70: _("""
 defi_cheminerreur numero  %(i1)d occurence numero :  %(i2)d 
  maille numero :  %(i3)d 
  maille  %(k1)s 
"""),

71: _("""
 defi_cheminerreur numero :  %(i1)d   %(k1)s occurence numero :  %(i2)d 
  groupe numero :  %(i3)d 
"""),

72: _("""
 defi_cheminerreur numero  %(i1)d occurence numero :  %(i2)d 
  groupe numero :  %(i3)d 
 presence de maille(s)  %(k1)s 
"""),

73: _("""
  
 les objets precedemment evoques sont  inexistants ou de type incompatible %(k1)s 
"""),

74: _("""
 erreur dans les donnees au noeud  %(k1)s  on a  %(i1)d  mailles %(k2)s 
"""),

75: _("""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s 
"""),

76: _("""
  defi-arc, occurence : %(i1)d le mot cle  %(k1)s 
 admet pour argument une liste  de 2 reels (a1,a2), telle que : %(k2)s 
"""),

77: _("""
 mot cle facteur  "defi_arc", occurence  %(i1)d 
 le centre n''est pas vraiment le  centre du cercle %(k1)s 
"""),

78: _("""
  la partie  %(i1)d de la courbe de nom :  %(k1)s 
 ne coupe pas le domaine maille. %(k2)s 
 non production du concept  %(k3)s 
 possibilite de desastre %(k4)s 
"""),

79: _("""
 face inconnuemaille numero :  %(i1)d  face :  %(i2)d 
"""),

80: _("""
 face a nombre de sommets  non traitemaille :  %(i1)d  face :  %(i2)d 
"""),

81: _("""
 face degenereemaille numero :  %(i1)d  face :  %(i2)d 
"""),

82: _("""
 face degenereemaille :  %(i1)d  face :  %(i2)d 
"""),

83: _("""
 face degenreemaille :  %(i1)d  face :  %(i2)d  arete :  %(i3)d 
"""),

84: _("""
 face degenreemaille :  %(i1)d  face :  %(i2)d 
"""),

85: _("""
 ***************************** post_traitement numero :  %(i1)d 
 * inexistence des champ-gd %(k1)s 
 * pas de post-traitement %(k2)s 
 **************************** %(k3)s 
"""),

86: _("""
 ***************************************** post_traitement numero :  %(i1)d 
 * aucunes mailles ne correspondent  aux criteres demandes %(k1)s 
 * pas de post-traitement %(k2)s 
 ******************************************************** %(k3)s 
"""),

87: _("""
  defi_segment, occurence  %(i1)d 
 origine et extremite confondues a la  precision :  %(r1)f 
"""),

88: _("""
 intersection  segment  cnum  maillage  nomail  : vide
       origine   : %(r1)f %(r2)f %(r3)f
       extremite : %(r4)f %(r5)f %(r6)f
"""),

89: _("""
 il y chevauchement entre les mailles  %(k1)s  et  %(k2)s 
"""),

90: _("""
 saut d''abscisse entre les mailles  %(k1)s  et  %(k2)s 
"""),

91: _("""
 probleme pour recuperer dans la table  %(k1)s  la contrainte  %(k2)s 
  pour l''absc_curv  %(r1)f 
  code retour  %(i1)d 
"""),

92: _("""
 plusieurs seismes dans le meme groupe   groupe numero:  %(i1)d 
    occurence situation  %(i2)d 
  et  %(i3)d 
"""),

93: _("""
 probleme pour recuperer dans la table  %(k1)s  les  %(k2)s 
"""),

94: _("""
 probleme pour recuperer dans la table  %(k1)s  les  %(k2)s 
"""),

95: _("""
 probleme pour recuperer dans la table  %(k1)s  la contrainte  %(k2)s 
  pour l''absc_curv  %(r1)f 
"""),

96: _("""
 erreur donnees " etat "pour la situation numero  %(i1)d 
 on n''a pas pu recuperer le "resu_meca"  correspondant au "char_etat_ etat "  %(i2)d 
"""),

97: _("""
 erreur donnees " etat "pour la situation numero  %(i1)d 
 on ne peut pas avoir des charges de  type "seisme" et "autre". %(i2)d 
"""),

98: _("""
 probleme recuperation pour l''occurrence  %(i1)d  dans le resultat  %(k1)s 
"""),

99: _("""
 probleme donnees pour l''occurrence  %(i1)d 
  un seul numero d''ordre  %(i2)d 
"""),

}
