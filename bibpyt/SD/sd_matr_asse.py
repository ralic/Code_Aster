# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

from SD.sd_matr_asse_gd import sd_matr_asse_gd
from SD.sd_matr_asse_gene import sd_matr_asse_gene
from SD.sd_matr_asse_com import sd_matr_asse_com

#---------------------------------------------------------------------------------
# classe "chapeau" à sd_matr_asse_gene et sd_matr_asse_gd ne servant que pour "check"
#---------------------------------------------------------------------------------


class sd_matr_asse(sd_matr_asse_com):
#--------------------------------------------
    nomj = SDNom(fin=19)


    # pour orienter vers sd_matr_asse_gene ou sd_matr_asse_gd :
    def check_matr_asse_1(self, checker):
        # on est obligé de se protéger dans le cas des Facultatif(sd_matr_asse) :
        if not self.REFA.get() : return
        gene=self.REFA.get()[9].strip()=='GENE'
        if  gene  :
           sd2=sd_matr_asse_gene(self.nomj)
        else :
           sd2=sd_matr_asse_gd(self.nomj)
        sd2.check(checker)
