# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

from SD.sd_matr_asse_com import sd_matr_asse_com
from SD.sd_nume_ddl_gene import sd_nume_ddl_gene


class sd_matr_asse_gene(sd_matr_asse_com):
#-----------------------------------------
    nomj = SDNom(fin=19)

    DESC = AsVI(lonmax=3,)

    def exists(self):
        return self.REFA.exists

    # indirection vers sd_nume_ddl à faire car FACT_LDLT modifie le
    # sd_nume_ddl_gene de la sd_matr_asse :
    def check_gene_REFA(self, checker):
        if not self.exists:
            return
        nom = self.REFA.get()[1]
        sd2 = sd_nume_ddl_gene(nom)
        sd2.check(checker)

    def check_gene_DESC(self, checker):
        if not self.exists:
            return
        desc = self.DESC.get()
        assert desc[0] == 2, desc
        nbvec = desc[1]
        assert nbvec > 0, desc
        type_sto = desc[2]
        assert type_sto in (1, 2, 3), desc
        valm = self.VALM.get()[1]
        n1 = len(valm)
        if type_sto == 1:
            # stockage diagonal
            assert n1 == nbvec, desc
        elif type_sto == 2:
            # stockage plein (mais symetrique) :
            assert n1 == nbvec * (nbvec + 1) / 2, desc
        elif type_sto == 3:
            # stockage quelconque :
            assert n1 >= nbvec
            assert n1 <= nbvec * (nbvec + 1) / 2, desc
