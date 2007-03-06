#@ MODIF compor1 Messages  DATE 06/03/2007   AUTEUR KHAM M.KHAM 
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

cata_msg={

1: _("""
 hujpla :: nombre de variables internes incorrect:
           NVI > NVIMAX
"""),
2: _("""
 hujddd :: on ne calcule pas DPSIDS pour K=4.
           - vérifiez la programmation -
"""),
4: _("""
 hujeux :: les modélisations autorisées sont 3D D_PLAN ou AXIS
"""),
5: _("""
 hujeux :: K différent de NBMECA pour le mécanisme isotrope
           - vérifiez la programmation -
"""),
6: _("""
 hujeux :: erreur inversion par pivot de Gauss
"""),
8: _("""
 hujeux :: mécanisme indéterminé
           - vérifiez la programmation -
"""),
10: _("""
 hujksi :: mot-clé inconnu
"""),
11: _("""
 hujnvi :: modélisation inconnue
"""),
14: _("""
 hujtid :: erreur calcul de la matrice tangente
"""),
15: _("""
 nmcomp :: la loi élastique n'est plus disponible directement avec SIMO_MIEHE : utilisez 
VMIS_ISOT_LINE avec un SY grand
"""),

}
