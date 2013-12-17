#@ AJOUT sd_cham_no_s SD
# -*- coding: iso-8859-1 -*-

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


    def check_CNSK(self,checker):
        if not self.exists() : return
        cnsk=self.CNSK.get_stripped()
        sd2=sd_maillage(cnsk[0]) ; sd2.check(checker)


    def check_longueurs(self,checker):
        if not self.exists() : return
        cnsd=self.CNSD.get()
        nbno=cnsd[0]
        nbcmp=cnsd[1]
        assert nbno  > 0 , cnsd
        assert nbcmp > 0 , cnsd

        assert self.CNSC.lonmax == nbcmp
        assert self.CNSL.lonmax == nbno*nbcmp
        assert self.CNSV.lonmax == nbno*nbcmp

