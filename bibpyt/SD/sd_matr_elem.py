#@ MODIF sd_matr_elem SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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
from SD.sd_resuelem import sd_resuelem

class sd_matr_elem(AsBase):
    nomj = SDNom(fin=8)
    LISTE_RESU = AsVK24()
    REFE_RESU = AsVK24(lonmax=5, )


    # indirection par LISTE_RESU :
    def check_matr_elem_i_LISTE_RESU(self, checker):
        lnom = self.LISTE_RESU.get()
        if not lnom:
            checker.err(self, "sd_champ LISTE_RESU absent")
            return
        for nom in lnom:
            if not nom.strip():
                continue
            resu1 = sd_resuelem(nom)
            # parfois, le nom est non ' ' et pourtant le sd_resuelem n'existe pas
            # Il faudrait corriger cette anomalie.
            if resu1.NOLI.exists :
                resu1.check(checker)


