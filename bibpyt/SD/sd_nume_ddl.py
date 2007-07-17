#@ MODIF sd_nume_ddl SD  DATE 17/07/2007   AUTEUR COURTOIS M.COURTOIS 
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
from SD.sd_solveur import sd_solveur

from SD.sd_nume_ddl_gene import sd_nume_ddl_gene
from SD.sd_nume_ddl_gd import sd_nume_ddl_gd


#---------------------------------------------------------------------------------
# classe "chapeau" à sd_nume_ddl_gene et sd_nume_ddl_gd ne servant que pour "check"
#---------------------------------------------------------------------------------


class sd_nume_ddl(AsBase):
#--------------------------------------------
    nomj = SDNom(fin=14)


    # pour orienter vers sd_nume_ddl_gene ou sd_nume_ddl_gd :
    def check_nume_ddl_i_GENE(self, checker):
        gene= aster.jeveux_exists(self.nomj()[:14]+'.NUME.ORIG')
        if  gene  :
           sd2=sd_nume_ddl_gene(self.nomj)
        else :
           sd2=sd_nume_ddl_gd(self.nomj)
        sd2.check(checker)

