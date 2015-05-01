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
from SD.sd_util import *


class sd_prof_chno(AsBase):
    nomj = SDNom(fin=19)
    PRNO = AsColl(acces='NU', stockage='CONTIG',
                  modelong=Parmi('CONSTANT', 'VARIABLE'), type='I', )
    LILI = AsObject(genr='N', xous='S', type='K', ltyp=24, )
    NUEQ = AsVI()
    DEEQ = AsVI()

    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre
        # vérifiée)
        return self.PRNO.exists

    def check_1(self, checker):
        if not self.exists():
            return
        nueq = self.NUEQ.get()
        deeq = self.DEEQ.get()
        neq = len(deeq) / 2
        for x in nueq:
            assert 1 <= x and x <= neq

        for k in range(neq):
            nuno = deeq[2 * k]
            nucmp = deeq[2 * k + 1]
            assert nuno >= 0
            if nuno == 0:
                assert nucmp == 0
            else:
                assert nucmp != 0
