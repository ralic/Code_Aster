#@ MODIF sd_macr_elem_stat SD  DATE 24/09/2007   AUTEUR DEVESA G.DEVESA 
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

from SD.sd_matr_asse_gd import sd_matr_asse_gd
from SD.sd_stoc_lciel import sd_stoc_lciel


class sd_macr_elem_stat(AsBase):
#----------------------------------------------
    nomj = SDNom(fin=8)

    # description géométrique et topolique :
    DESM = AsVI(lonmax=10)
    REFM = AsVK8()
    LINO = AsVI()
    VARM = AsVR(lonmax=2)
    CONX = Facultatif(AsVI())

    # rigidité condensée :
    rigimeca = Facultatif(sd_matr_asse_gd(SDNom(nomj='.RIGIMECA', fin=19)))
    MAEL_RAID_VALE = Facultatif(AsVR())
    PHI_IE   = Facultatif(AsColl(acces='NU', stockage='DISPERSE', modelong='CONSTANT', type='R', ltyp=8))

    # masse condensée :
    massmeca = Facultatif(sd_matr_asse_gd(SDNom(nomj='.MASSMECA', fin=19)))
    MAEL_MASS_VALE = Facultatif(AsVR())
    
    # amortissement condensé :
    MAEL_AMOR_VALE = Facultatif(AsVR())

    # chargements condensés :
    LICA = Facultatif(AsColl(acces='NO', stockage='DISPERSE', modelong='CONSTANT', type='R', ltyp=8))
    LICH = Facultatif(AsColl(acces='NO', stockage='CONTIG', modelong='CONSTANT', type='K', ltyp=8))




