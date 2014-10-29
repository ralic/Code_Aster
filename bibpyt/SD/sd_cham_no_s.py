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
from SD.sd_maillage import sd_maillage
from SD.sd_util import *


class sd_cham_no_s(AsBase):
#----------------------------
    nomj = SDNom(fin=19)

    CNSK = AsVK8(lonmax=2)
    CNSD = AsVI(lonmax=2)
    CNSC = AsVK8()
    CNSV = AsVect(type=Parmi('C', 'K', 'R', 'I'))
    CNSL = AsVL()

    def exists(self):
        return self.CNSK.exists

    def check_CNSK(self, checker):
        if not self.exists():
            return
        cnsk = self.CNSK.get_stripped()
        sd2 = sd_maillage(cnsk[0])
        sd2.check(checker)

    def check_longueurs(self, checker):
        if not self.exists():
            return
        cnsd = self.CNSD.get()
        nbno = cnsd[0]
        nbcmp = cnsd[1]
        assert nbno > 0, cnsd
        assert nbcmp > 0, cnsd

        assert self.CNSC.lonmax == nbcmp
        assert self.CNSL.lonmax == nbno * nbcmp
        assert self.CNSV.lonmax == nbno * nbcmp
