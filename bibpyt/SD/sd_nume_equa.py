# coding=utf-8
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

from SD.sd_prof_chno import sd_prof_chno
from SD.sd_maillage import sd_maillage
from SD.sd_util import *


class sd_nume_equa(sd_prof_chno):
    nomj = SDNom(fin=19)
    NEQU = AsVI(lonmax=2,)
    DELG = AsVI()
    REFN = AsVK24(lonmax=4,)

    def check_REFN(self, checker):
        assert self.REFN.exists
        refn = self.REFN.get_stripped()
        assert refn[3] in ('', 'XXXX')  # non-information

        # nom de la grandeur :
        assert refn[1] != ''
        sdu_verif_nom_gd(refn[1])

        # nom du maillage :
        assert refn[0] != ''
        sd2 = sd_maillage(refn[0])
        sd2.check(checker)

    def check_1(self, checker):
        nequ = self.NEQU.get()
        delg = self.DELG.get()
        neq = nequ[0]
        assert neq > 0
        assert nequ[1] >= 0
        assert len(delg) == neq
        for x in delg:
            assert x in (-2, -1, 0)
