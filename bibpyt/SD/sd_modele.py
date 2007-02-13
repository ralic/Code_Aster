#@ MODIF sd_modele SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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
from SD.sd_fiss_xfem import sd_fiss_xfem


class sd_modele(AsBase):
#-----------------------------
    nomj = SDNom(fin=8)
    NOEUD = Facultatif(AsVI())
    SSSA = Facultatif(AsVI())
    NOEUD_UTIL = AsVI()
    FISS = Facultatif(AsVK8(lonmax=1, ))
    MAILLE = Facultatif(AsVI())
    modele = sd_ligrel(SDNom(nomj='.MODELE', fin=19))

    # indirection vers FISS_XFEM car MODI_MODELE_XFEM modifie FISS_XFEM
    # (Damijan va corriger cela avec la multi-fissuration)
    def check_modele_i_FISS(self, checker):
        if not self.FISS.get() : return
        nom=self.FISS.get()[0]
        sd2=sd_fiss_xfem(nom) ; sd2.check(checker)

