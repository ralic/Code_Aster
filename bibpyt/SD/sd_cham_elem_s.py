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



class sd_cham_elem_s(AsBase):
#----------------------------
    nomj = SDNom(fin=19)

    CESK = AsVK8(lonmax=3)
    CESD = AsVI()
    CESC = AsVK8()
    CESV = AsVect(type=Parmi('C', 'K', 'R', 'I'))
    CESL = AsVL()


    def exists(self):
        return self.CESK.exists


    def check_CESK(self,checker):
        if not self.exists() : return
        cesk=self.CESK.get_stripped()
        sd2=sd_maillage(cesk[0]) ; sd2.check(checker)
        assert cesk[2] in ('ELNO','ELGA','ELEM') , cesk


    def check_longueurs(self,checker):
        if not self.exists() : return
        cesd=self.CESD.get()
        nbma=cesd[0]
        nbcmp=cesd[1]
        nbval=self.CESV.lonmax
        assert nbma  > 0 , cesd[:5]
        assert nbcmp > 0 , cesd[:5]

        assert self.CESD.lonmax == 5 + 4*nbma
        assert self.CESC.lonmax == nbcmp
        assert self.CESL.lonmax == nbval

