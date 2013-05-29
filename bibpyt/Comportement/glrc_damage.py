# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: sebastien.fayolle at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'GLRC_DAMAGE',
   doc = """Modèle global de plaque en béton armé capable de représenter son comportement jusqu'à la ruine. 
   Contrairement aux modélisations locales où chaque constituant du matériau est modélisé à part, dans les modèles globaux, 
   la loi de comportement s'écrit directement en terme de contraintes et de déformations généralisées. 
   Les phénomènes pris en compte sont l'élasto-plasticité couplée entre les effets de membrane et de flexion 
   (contre une élasto-plasticité en flexion seulement dans GLRC) et l'endommagement en flexion. 
   L'endommagement couplé membrane/flexion est traité par GLRC_DM, lequel, par contre, néglige complètement l'élasto-plasticité. 
   Pour les précisions sur la formulation du modèle voir [R7.01.31].""",
   num_lc         = 9999,
   nb_vari        = 19,
   nom_vari       = ('EPSP1','EPSP2','EPSP3','KHIP1','KHIP2','KHIP3','DISSIP','ENDOFL+','ENDOFL-','DISSENDO','ANGL1','ANGL2','ANGL3','XMEMB1','XMEMB2','XMEMB3','XFLEX1','XFLEX2','XFLEX3'),
   mc_mater       = ('GLRC_DAMAGE','GLRC_ACIER'),
   modelisation   = ('DKTG','Q4GG'),
   deformation    = ('PETIT', 'GROT_GDEP'),
   nom_varc       = ('TEMP'),
   algo_inte         = ('NEWTON',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)
