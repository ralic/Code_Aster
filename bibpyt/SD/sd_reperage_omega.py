#@ MODIF sd_reperage_omega SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

class sd_reperage_omega(AsBase):
    nomj = SDNom(fin=13)
    CREFF_EXTR = AsVR(SDNom(nomj='.CREFF.EXTR', debut=13), )
    FACE__ORIG = AsVI(SDNom(nomj='.FACE .ORIG', debut=13), )
    CREFF_ORIG = AsVR(SDNom(nomj='.CREFF.ORIG', debut=13), )
    ARETE_EXTR = AsVI(SDNom(nomj='.ARETE.EXTR', debut=13), )
    FACE__EXTR = AsVI(SDNom(nomj='.FACE .EXTR', debut=13), )
    MAIL = AsColl(SDNom(debut=13), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    CREFM_ORIG = AsVR(SDNom(nomj='.CREFM.ORIG', debut=13), )
    CREFM_EXTR = AsVR(SDNom(nomj='.CREFM.EXTR', debut=13), )
    ARETE_ORIG = AsVI(SDNom(nomj='.ARETE.ORIG', debut=13), )


