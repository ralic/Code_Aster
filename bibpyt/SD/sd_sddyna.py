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

class sd_sddyna(AsBase):
#------------------------------------
    nomj = SDNom(fin=15)

    TYPE_SCH=AsVK16(SDNom(nomj='.TYPE_SCH'),lonmax=8)
    PARA_SCH=Facultatif(AsVR(  SDNom(nomj='.PARA_SCH'),lonmax=4))
    INI_CONT=Facultatif(AsVR(  SDNom(nomj='.INI_CONT'),lonmax=4))
    NOM_SD  =Facultatif(AsVK24(SDNom(nomj='.NOM_SD'  ),lonmax=3))
    TYPE_FOR=Facultatif(AsVI(  SDNom(nomj='.TYPE_FOR'),lonmax=2))
    COEF_SCH=Facultatif(AsVR(  SDNom(nomj='.COEF_SCH'),lonmax=4))
    INFO_SD =Facultatif(AsVL(  SDNom(nomj='.INFO_SD' ),lonmax=5))
