#@ MODIF sd_char_meca SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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
from SD.sd_carte import sd_carte
from SD.sd_char_unilate import sd_char_unilate
from SD.sd_char_contact import sd_char_contact

class sd_char_meca(AsBase):
    nomj = SDNom(fin=8)

    TYPE            = AsVK8(lonmax=1)
    CHME_MODEL_NOMO = AsVK8(SDNom(nomj='.CHME.MODEL.NOMO'), lonmax=1, )

    chme_ligre = Facultatif(sd_ligrel(SDNom(nomj='.CHME.LIGRE')))

    chme_cimpo = Facultatif(sd_carte(SDNom(nomj='.CHME.CIMPO')))
    chme_cmult = Facultatif(sd_carte(SDNom(nomj='.CHME.CMULT')))
    chme_epsin = Facultatif(sd_carte(SDNom(nomj='.CHME.EPSIN')))
    chme_f1d1d = Facultatif(sd_carte(SDNom(nomj='.CHME.F1D1D')))
    chme_f1d2d = Facultatif(sd_carte(SDNom(nomj='.CHME.F1D2D')))
    chme_f1d3d = Facultatif(sd_carte(SDNom(nomj='.CHME.F1D3D')))
    chme_f2d2d = Facultatif(sd_carte(SDNom(nomj='.CHME.F2D2D')))
    chme_f2d3d = Facultatif(sd_carte(SDNom(nomj='.CHME.F2D3D')))
    chme_f3d3d = Facultatif(sd_carte(SDNom(nomj='.CHME.F3D3D')))
    chme_fco2d = Facultatif(sd_carte(SDNom(nomj='.CHME.FCO2D')))
    chme_fco3d = Facultatif(sd_carte(SDNom(nomj='.CHME.FCO3D')))
    chme_felec = Facultatif(sd_carte(SDNom(nomj='.CHME.FELEC')))
    chme_fl101 = Facultatif(sd_carte(SDNom(nomj='.CHME.FL101')))
    chme_fl102 = Facultatif(sd_carte(SDNom(nomj='.CHME.FL102')))
    chme_flux  = Facultatif(sd_carte(SDNom(nomj='.CHME.FLUX')))
    chme_forno = Facultatif(sd_carte(SDNom(nomj='.CHME.FORNO')))
    chme_impe  = Facultatif(sd_carte(SDNom(nomj='.CHME.IMPE')))
    chme_onde  = Facultatif(sd_carte(SDNom(nomj='.CHME.ONDE')))
    chme_pesan = Facultatif(sd_carte(SDNom(nomj='.CHME.PESAN')))
    chme_press = Facultatif(sd_carte(SDNom(nomj='.CHME.PRESS')))
    chme_rotat = Facultatif(sd_carte(SDNom(nomj='.CHME.ROTAT')))
    chme_sigin = Facultatif(sd_carte(SDNom(nomj='.CHME.SIGIN')))
    chme_vnor  = Facultatif(sd_carte(SDNom(nomj='.CHME.VNOR')))

    unilate = Facultatif(sd_char_unilate(SDNom(nomj='.UNILATE')))
    contact = Facultatif(sd_char_contact(SDNom(nomj='.CONTACT')))

    CHME_EVOL_CHAR = Facultatif(AsVK8(SDNom(nomj='.CHME.EVOL.CHAR'), lonmax=1, ))
    CHME_VEASS = Facultatif(AsVK8(SDNom(nomj='.CHME.VEASS'), lonmax=1, ))
    TRANS01 = Facultatif(AsVR(lonmax=6, ))
    CHME_TEMPE_TEMP = Facultatif(AsVK8(SDNom(nomj='.CHME.TEMPE.TEMP'), lonmax=1, ))
    TRANS02 = Facultatif(AsVR(lonmax=6, ))
    LISMA01 = Facultatif(AsVI(lonmax=12, ))
    LISMA02 = Facultatif(AsVI(lonmax=12, ))
    POIDS_MAILLE = Facultatif(AsVR())



