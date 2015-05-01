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
from SD.sd_table import sd_table
from SD.sd_util import *


class sd_l_table(AsBase):
#------------------------
    nomj = SDNom(fin=19)

    # la SD l_table (liste de tables) est une SD destinée à stocker un ensemble de tables
    # les tables stockées dans la l_table sont identifiées par un "petit nom"
    # (K16)

    LTNT = AsVK16()
    LTNS = AsVK24()

    # existence possible de la SD :
    def exists(self):
        return self.LTNT.exists or self.LTNS.exists

    # indirection vers les tables :
    def check_l_table_i_LTNS(self, checker):
        if not self.exists():
            return
        ltnt = self.LTNT.get()
        ltns = self.LTNS.get()
        nbtable = self.LTNT.lonuti
        sdu_compare(self.LTNT, checker, nbtable, '>', 0, 'NBUTI(LTNT)>0')
        sdu_compare(self.LTNS, checker, self.LTNS.lonuti,
                    '==', nbtable, 'NBUTI(LTNS)==NBUTI(LTNT)')
        for k in range(nbtable):
            petinom = ltnt[k].strip()
            nomtabl = ltns[k].strip()
            sdu_compare(self.LTNT, checker, petinom, '!=', '', "LTNT[k]!=''")
            sdu_compare(self.LTNS, checker, nomtabl, '!=', '', "LTNS[k]!=''")
            sd2 = sd_table(nomtabl)
            sd2.check(checker)
