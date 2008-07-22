#@ MODIF sd_solveur SD  DATE 22/07/2008   AUTEUR PELLET J.PELLET 
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

class sd_solveur(AsBase):
    nomj = SDNom(fin=19)
    SLVK = AsVK24(SDNom(debut=19), lonmax=11, )
    SLVR = AsVR(SDNom(debut=19), lonmax=4, )
    SLVI = AsVI(SDNom(debut=19), lonmax=6, )


    def check_SLVK(self,checker):
    #---------------------------------------------
        slvk = self.SLVK.get_stripped()
        method=slvk[0]
        if method == 'MUMPS' :
            assert slvk[1] in ('AUTO', 'SANS'), slvk
            assert slvk[2] in ('NONSYM', 'SYMGEN', 'SYMDEF', 'AUTO'), slvk
            assert slvk[3] in ('AMD','AMF','PORD','METIS','QAMD','AUTO'), slvk
            assert slvk[4] in ('OUI', 'NON'), slvk
            assert slvk[5] in ('OUI', 'NON'), slvk
        else :
            pass


