#@ MODIF sd_compor1 SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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
from SD.sd_fonction import sd_fonction


class sd_compor1(AsBase):
#-----------------------
    nomj = SDNom(fin=19)
    VALC = AsVC(SDNom(debut=19), )
    VALK = AsVK8(SDNom(debut=19), )
    VALR = AsVR(SDNom(debut=19), )


    # parfois, THER_NL crée une sd_fonction pour BETA
    def check_compor1_i_VALK(self, checker):
        nom= self.nomj().strip()
        valk=list(self.VALK.get())
        if not valk : return
        if nom[8:16]=='.THER_NL' :
           k=valk.index('BETA    ')
           nomfon=valk[2*k+1]
           sd2=sd_fonction(nomfon) ; sd2.check(checker)
