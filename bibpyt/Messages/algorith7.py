#@ MODIF algorith7 Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

1 : _(u"""
 couplage fluage/fissuration :
 il faut définir deux lois de comportement exactement. 
"""),

2 : _(u"""
 GRANGER et ENDO_ISOT_BETON ou MAZARS non encore développé
"""),

3 : _(u"""
 loi de comportement non autorisée dans le couplage fluage/fissuration
"""),

4 : _(u"""
 DEBORST non compatible avec couplage UMLV/Mazars.
 Mais le traitement analytique est réalisé, il suffit de supprimer le
 mot-clé DEBORST),
 
"""),
5 : _(u"""
 pas de C_PLAN pour ENDO_ISOT_BETON
 utiliser C_PLAN_DEBORST
"""),

6 : _(u"""
 loi de fluage non autorisee dans le couplage fluage/fissuration
"""),

7 : _(u"""
 pas d'orthotropie non linéaire
"""),

8 : _(u"""
 loi de comportement hyper-élastique non prevue
"""),

9 : _(u"""
 C_PLAN méthode DEBORST et grandes déformations sont incompatibles
"""),

10 : _(u"""
 COMP1D et SIMO_MIEHE incompatibles
"""),

11 : _(u"""
 couplage fluage/fissuration :
 la première loi doit etre une loi de fluage de type GRANGER_FP ou GRANGER_FP_V.
"""),

12 : _(u"""
 couplage fluage/fissuration :
 nombre total de variables internes incohérent <--> erreur de programmation. 
"""),

15 : _(u"""
  le concept EVOL_CHAR :  %(k1)s  n'en est pas un !
"""),

16 : _(u"""
  le concept EVOL_CHAR :  %(k1)s  ne contient aucun champ de type EVOL_CHAR.
"""),

20 : _(u"""
 le champ de déplacement DIDI n'est pas trouvé dans le concept  %(k1)s 
"""),









56 : _(u"""
 liste RELATION_KIT vide
"""),

57 : _(u"""
 liste RELATION_KIT trop longue
"""),

58 : _(u"""
 1D ou C_PLAN ?
"""),


60 : _(u"""
  -> Le critère de convergence pour intégrer le comportement 'RESI_INTE_RELA'
     est lache (très supérieur à la valeur par défaut).
  -> Risque & Conseil :
     Cela peut nuire à la qualité de la solution et à la convergence.
"""),

61 : _(u"""
 option  %(k1)s  non traitee
"""),

63 : _(u"""
 pas existence de solution pour le saut
"""),

64 : _(u"""
 existence d'un élément à discontinuité trop grand
 non unicité du saut
"""),

65 : _(u"""
 non convergence du NEWTON pour le calcul du saut numéro 1
"""),

66 : _(u"""
 non convergence du NEWTON pour le calcul du saut numéro 2
"""),

67 : _(u"""
 non convergence du NEWTON pour le calcul du saut numéro 3
"""),

68 : _(u"""
 erreur dans le calcul du saut
"""),

69 : _(u"""
 loi %(k1)s  non implantee pour les elemdisc 
"""),

73 : _(u"""
 le tenseur EPSEQ vaut  0 on a donc une derivée lagrangienne DEPSEQ très grande !
"""),

74 : _(u"""
  valeur de D_SIGM_EPSI non trouvée
"""),

75 : _(u"""
  valeur de SY non trouvée
"""),

76 : _(u"""
 développement non implanté
"""),

79 : _(u"""
 loi de comportement avec irradiation, le paramètre N doit etre supérieur à 0
"""),

80 : _(u"""
 loi de comportement avec irradiation, le paramètre PHI_ZERO doit etre supérieur à 0
"""),

81 : _(u"""
 loi de comportement avec irradiation, le paramètre phi/K.PHI_ZERO+L doit etre supérieur ou égal à 0
"""),

82 : _(u"""
 loi de comportement avec irradiation, le paramètre phi/K.PHI_ZERO+L vaut 0. Dans ces conditions le paramètre BETA doit être positif ou nul
"""),

96 : _(u"""
 comportement ZMAT obligatoire
"""),

98 : _(u"""
 il faut déclarer FONC_DESORP sous ELAS_FO pour le fluage de dessication
 intrinseque avec SECH comme paramètre
"""),

}
