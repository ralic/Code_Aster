#@ MODIF sd_type_flui_stru SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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


class sd_type_flui_stru(AsBase):
#------------------------------------
    nomj = SDNom(fin=8)
    FSIC = AsVI(SDNom(debut=19),lonmax=2, )

    # AJACOT_PB : j'ai du ajouter plusieurs Facultatifs (sdll116a)
    # il y en a vraiment beaucoup maintenant !!
    FSGM = Facultatif(AsVK8(SDNom(debut=19)))
    FSVR = Facultatif(AsVR(SDNom(debut=19)))
    FSVK = Facultatif(AsVK8(SDNom(debut=19)))
    FSVI = Facultatif(AsVI(SDNom(debut=19)))
    FSCR = Facultatif(AsVR(SDNom(debut=19),lonmax=18, ))
    FSGR = Facultatif(AsVR(SDNom(debut=19),lonmax=7, ))
    UNIT_FAISCEAU = Facultatif(AsVI())
    UNIT_GRAPPES  = Facultatif(AsVI())



