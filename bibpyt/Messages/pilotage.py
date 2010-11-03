#@ MODIF pilotage Messages  DATE 03/11/2010   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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

1  : _("""
 Le pilotage de type PRED_ELAS n'est pas possible en modélisation C_PLAN.
"""),

2  : _("""
 Pour le cas de l'endommagement saturé dans ENDO_ISOT_BETON, on ne pilote pas.
"""),

3  : _("""
 Le paramètre COEF_MULT pour le pilotage ne doit pas valoir zéro.
"""),

4  : _("""
 La recherche linéaire en pilotage n'est possible qu'avec l'option PILOTAGE dans RECH_LINEAIRE  (sauf pour le cas DDL_IMPO).
"""),

48 : _("""
 ETA_PILO_MAX doit être inférieur à ETA_PILO_R_MAX
"""),

49 : _("""
 ETA_PILO_MIN doit être supérieur à ETA_PILO_R_MIN
"""),

50 : _("""
 Il ne faut pas plus d'un noeud pour le pilotage DDL_IMPO.
"""),

55 : _("""
 La liste des composantes est vide pour le pilotage LONG_ARC.
"""),

56 : _("""
 Il faut une et une seule composante de nom NOM_CMP pour le pilotage de type DDL_IMPO.
"""),

57 : _("""
 Il faut plus d'un noeud pour le pilotage LONG_ARC.
"""),

83 : _("""
 Problème lors du pilotage.
 Nombre maximum d'itérations atteint.
"""),

84 : _("""
 Problème lors du pilotage.
 Précision machine depassée.
"""),

85 : _("""
 Problème lors du pilotage.
 Il y a trois solutions ou plus.
"""),

86 : _("""
 Problème lors du pilotage.
 La matrice locale n'est pas inversible.
"""),

87 : _("""
 Problème lors du pilotage.
"""),

88 : _("""
 La loi de comportement <%(k1)s> n'est pas disponible pour le pilotage de type PRED_ELAS.
"""),

89 : _("""
 Le pilotage PRED_ELAS nécessite ETA_PILO_MIN et ETA_PILO_MAX
 pour la loi ENDO_ISOT_BETON
"""),

90 : _("""
 Le pilotage PRED_ELAS nécessite ETA_PILO_MIN et ETA_PILO_MAX
 pour la loi ENDO_ORTH_BETON
"""),

}
