#@ MODIF postrele Messages  DATE 03/10/2006   AUTEUR CIBHHLV L.VIVAN 
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

}
