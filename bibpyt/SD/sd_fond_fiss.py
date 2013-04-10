#@ MODIF sd_fond_fiss SD  DATE 09/04/2013   AUTEUR JAUBERT A.JAUBERT 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
from SD.sd_cham_no   import sd_cham_no

class sd_fond_fiss(AsBase):
    nomj           = SDNom(fin=8)
    LEVREINF_MAIL  = Facultatif(AsVK8(SDNom(nomj='.LEVREINF.MAIL'), ))
    NORMALE        = Facultatif(AsVR(lonmax=3, ))
    BASEFOND       = Facultatif(AsVR())
    FOND_TYPE      = AsVK8(SDNom(nomj='.FOND.TYPE'), lonmax=1, )
    FOND_NOEU      = Facultatif(AsVK8(SDNom(nomj='.FOND.NOEU'), ))
    FONDSUP_NOEU   = Facultatif(AsVK8(SDNom(nomj='.FOND_SUP.NOEU'), ))
    FONDINF_NOEU   = Facultatif(AsVK8(SDNom(nomj='.FOND_INF.NOEU'), ))
    DTAN_EXTREMITE = Facultatif(AsVR(lonmax=3, ))
    INFNORM_NOEU   = Facultatif(AsVK8(SDNom(nomj='.INFNORM.NOEU'), ))
    DTAN_ORIGINE   = Facultatif(AsVR(lonmax=3, ))
    SUPNORM_NOEU   = Facultatif(AsVK8(SDNom(nomj='.SUPNORM.NOEU'), ))
    LEVRESUP_MAIL  = Facultatif(AsVK8(SDNom(nomj='.LEVRESUP.MAIL'), ))
    INFO           = AsVK8(SDNom(nomj='.INFO'), lonmax=3, )
    FOND_TAILLE_R  = Facultatif(AsVR(SDNom(nomj='.FOND.TAILLE_R'),))
    FONDFISS       = AsVR()
    LNNO           = Facultatif(sd_cham_no())
    LTNO           = Facultatif(sd_cham_no())
    BASLOC         = Facultatif(sd_cham_no())
    FONDFISG       = Facultatif(AsVR())

    def check_BASEFOND(self,checker):
        info = self.INFO.get_stripped()
        config = info[1]
        basefond = self.BASEFOND.get()
        if config == 'DECOLLEE' :
           assert basefond is None,config
        else:
           pass

