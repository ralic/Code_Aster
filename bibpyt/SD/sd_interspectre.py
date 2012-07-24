#@ MODIF sd_interspectre SD  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

class sd_interspectre(sd_titre):
#------------------------------------
    nomj = SDNom(fin=8)

    REFE = AsVK16(lonmax=2,)
    FREQ = AsVR()
    VALE = AsColl(acces='NU',stockage='DISPERSE',modelong='VARIABLE',type='R',)

    NUMI = Facultatif(AsVI())
    NUMJ = Facultatif(AsVI())

    NOEI = Facultatif(AsVK8())
    NOEJ = Facultatif(AsVK8())
    CMPI = Facultatif(AsVK8())
    CMPJ = Facultatif(AsVK8())

    def check_NUME(self,checker):
    #-------------------------------
        if self.NUMI.exists:
            numi = self.NUMI.get()
            assert self.NUMJ.exists
            numj = self.NUMJ.get()
            assert len(numi) == len(numj)
            assert self.VALE.nmaxoc == len(numi)

        else:
            assert self.NOEI.exists
            noei = self.NOEI.get()
            assert self.NOEJ.exists
            noej = self.NOEJ.get()
            assert len(noej) == len(noei)
            assert self.CMPI.exists
            cmpi = self.CMPI.get()
            assert self.CMPJ.exists
            cmpj = self.CMPJ.get()
            assert len(cmpj) == len(cmpi)
            assert len(noei) == len(cmpj)
            assert self.VALE.nmaxoc == len(noei)

