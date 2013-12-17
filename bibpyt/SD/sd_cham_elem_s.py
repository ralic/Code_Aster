#@ AJOUT sd_cham_elem_s SD
# -*- coding: iso-8859-1 -*-

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

