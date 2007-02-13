#@ MODIF sd_char_ther SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_ligrel import sd_ligrel
from SD.sd_cham_elem import sd_cham_elem
from SD.sd_carte import sd_carte
from SD.sd_champ import sd_champ


class sd_char_ther(AsBase):
#--------------------------------
    nomj = SDNom(fin=8)

    CHTH_CONVE_VALE = Facultatif(AsVK8(SDNom(nomj='.CHTH.CONVE.VALE'), lonmax=1))
    CHTH_MODEL_NOMO = AsVK8(SDNom(nomj='.CHTH.MODEL.NOMO'), lonmax=1)
    TYPE            = AsVK8(lonmax=1)
    chth_ligre      = Facultatif(sd_ligrel(SDNom(nomj='.CHTH.LIGRE')))

    chth_soure = Facultatif(sd_champ(SDNom(nomj='.CHTH.SOURE')))  # pour l'instant : sd_carte ou sd_cham_elem

    chth_cimpo = Facultatif(sd_carte(SDNom(nomj='.CHTH.CIMPO')))
    chth_cmult = Facultatif(sd_carte(SDNom(nomj='.CHTH.CMULT')))
    chth_coefh = Facultatif(sd_carte(SDNom(nomj='.CHTH.COEFH')))
    chth_flunl = Facultatif(sd_carte(SDNom(nomj='.CHTH.FLUNL')))
    chth_flur2 = Facultatif(sd_carte(SDNom(nomj='.CHTH.FLUR2')))
    chth_flure = Facultatif(sd_carte(SDNom(nomj='.CHTH.FLURE')))
    chth_grain = Facultatif(sd_carte(SDNom(nomj='.CHTH.GRAIN')))
    chth_hechp = Facultatif(sd_carte(SDNom(nomj='.CHTH.HECHP')))
    chth_rayo  = Facultatif(sd_carte(SDNom(nomj='.CHTH.RAYO')))
    chth_t_ext = Facultatif(sd_carte(SDNom(nomj='.CHTH.T_EXT')))


