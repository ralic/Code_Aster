#@ MODIF sd_corresp_2_mailla SD  DATE 02/02/2010   AUTEUR PELLET J.PELLET 
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

from SD import *

class sd_corresp_2_mailla(AsBase):
    nomj = SDNom(fin=16)
    PJXX_K1 = AsVK24(lonmax=5)

    # Remarque : pour retirer la plupart des "facultatifs", il faudrait changer
    # les noms : PJEF_NB -> PE.EFNB
    #            ...
    #            PJNG_I1 -> PN.NGI1
    # et faire 2 class sd_corresp_2_elem et sd_corresp_2_nuage)
    PJEF_NB = Facultatif(AsVI())
    PJEF_NU = Facultatif(AsVI())
    PJEF_M1 = Facultatif(AsVI())
    PJEF_CF = Facultatif(AsVR())
    PJEF_TR = Facultatif(AsVI())
    PJEF_AM = Facultatif(AsVI())
    PJEF_CO = Facultatif(AsVR())

    PJNG_I1 = Facultatif(AsVI())
    PJNG_I2 = Facultatif(AsVI())



