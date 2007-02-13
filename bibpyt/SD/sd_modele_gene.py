#@ MODIF sd_modele_gene SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

from SD import *

class sd_modele_gene(AsBase):
    nomj = SDNom(fin=14)
    MODG_LIPR = AsVI(SDNom(nomj='.MODG.LIPR'), )
    MODG_LIDF = AsColl(SDNom(nomj='.MODG.LIDF'), acces='NU', stockage='DISPERSE', modelong='CONSTANT', type='K', ltyp=8, )
    MODG_SSTR = AsColl(SDNom(nomj='.MODG.SSTR'), acces='NU', stockage='CONTIG', modelong='CONSTANT', type='R', ltyp=8, )
    MODG_SSOR = AsColl(SDNom(nomj='.MODG.SSOR'), acces='NU', stockage='CONTIG', modelong='CONSTANT', type='R', ltyp=8, )
    MODG_SSNO = AsObject(SDNom(nomj='.MODG.SSNO'), genr='N', xous='S', type='K', ltyp=8, )
    MODG_SSME = AsColl(SDNom(nomj='.MODG.SSME'), acces='NU', stockage='CONTIG', modelong='CONSTANT', type='K', ltyp=8, )
    MODG_DESC = AsVI(SDNom(nomj='.MODG.DESC'), lonmax=3, )
    MODG_LIMA = AsColl(SDNom(nomj='.MODG.LIMA'), acces='NU', stockage='DISPERSE', modelong='VARIABLE', type='R', ltyp=8, )


