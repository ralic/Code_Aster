#@ MODIF sd_tran_gene SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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
from SD.sd_titre import sd_titre


class sd_tran_gene(sd_titre) :
#--------------------------------------
    nomj = SDNom(fin=19)

    # objets commencant en 19 :
    FDEP = Facultatif(AsVK8())
    ACCE = AsVR()
    ICHO = Facultatif(AsVI())
    REDN = Facultatif(AsVK24(lonmax=1, ))
    INST = AsVR()
    IPSD = Facultatif(AsVR())
    DEPL = AsVR()
    VINT = Facultatif(AsVR())
    FCHO = Facultatif(AsVR())
    DESC = AsVI(lonmax=5, )
    INTI = Facultatif(AsVK8())
    REDC = Facultatif(AsVI())
    NCHO = Facultatif(AsVK8())
    REDD = Facultatif(AsVR())
    REFD = AsVK24(lonmax=6, )
    VITE = AsVR()
    FVIT = Facultatif(AsVK8())
    SST = Facultatif(AsVK8())
    DLOC = Facultatif(AsVR())
    ORDR = AsVI()
    PTEM = AsVR()
    FACC = Facultatif(AsVK8())
    VCHO = Facultatif(AsVR())

    # objets commencant en 8 :
    BASEPR = Facultatif(AsVR(SDNom(debut=8)))
    VNOEUD = Facultatif(AsVI(SDNom(debut=8)))
    VORIEN = Facultatif(AsVR(SDNom(debut=8)))
    VRANGE = Facultatif(AsVK8(SDNom(debut=8)))



