#@ MODIF sd_melasflu SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_l_table import sd_l_table
from SD.sd_table import sd_table
from SD.sd_champ import sd_champ
from SD.sd_matr_asse_gene import sd_matr_asse_gene


class sd_melasflu(AsBase):
#-------------------------------
    nomj = SDNom(fin=8)

    MASG = AsVR(SDNom(debut=19), )
    VITE = AsVR(SDNom(debut=19), )
    REMF = AsVK8(SDNom(debut=19), lonmax=2, )
    FREQ = AsVR(SDNom(debut=19), )
    NUMO = AsVI(SDNom(debut=19))
    FACT = AsVR(SDNom(debut=19), )
    DESC = AsVK16(SDNom(debut=19), lonmax=1, )
    col1 = AsVK24(SDNom(debut=19,nomj='.0001'))
    col3 = Facultatif(AsVK24(SDNom(debut=8,nomj='.TB000000  .0003')))
    col4 = Facultatif(AsVK24(SDNom(debut=8,nomj='.TB000000  .0004')))
    col5 = Facultatif(AsVK24(SDNom(debut=8,nomj='.TB000000  .0005')))

    sd_l_table = Facultatif(sd_l_table(SDNom(nomj='')))
    sd_table   = Facultatif(sd_table(SDNom(nomj='')))

    # indirection vers des champs :
    def check_melasflu_i_COL1(self, checker):
        lnom = self.col1.get()
        for nom in lnom :
            if not nom.strip(): continue
            sd2 = sd_champ(nom) ; sd2.check(checker)

    # indirection vers des sd_matr_asse :
    def check_melasflu_i_COL345(self, checker):
        lnom=[]
        if  self.col3.get() : lnom= lnom+list(self.col3.get())
        if  self.col4.get() : lnom= lnom+list(self.col4.get())
        if  self.col5.get() : lnom= lnom+list(self.col5.get())
        for nom in lnom :
            if not nom.strip(): continue
            sd2 = sd_matr_asse_gene(nom) ; sd2.check(checker)

