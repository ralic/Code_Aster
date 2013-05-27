# -*- coding: iso-8859-1 -*-
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
from SD.sd_titre import sd_titre
from SD.sd_ligrel import sd_ligrel


class sd_cham_elem(sd_titre):
    nomj = SDNom(fin=19)
    CELD = AsVI(docu='CHML', )
    CELV = AsObject(genr='V', xous='S', type=Parmi('C', 'I', 'K', 'R'), ltyp=Parmi(4,8,16), )
    CELK = AsVK24(lonmax=7, )


    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.CELK.exists

    def check_1(self, checker):
        if not self.exists() : return
        celk=self.CELK.get_stripped()
        sd2=sd_ligrel(celk[0]); sd2.check(checker)
        assert celk[1] != '' , celk
        assert celk[2] in ('ELNO','ELGA','ELEM') , celk
        assert celk[4] in ('','INF','MOY','SUP') , celk
        assert celk[5] != '' , celk
        assert celk[6] in ('MPI_COMPLET','MPI_INCOMPLET') , celk

    def check_2(self, checker):
        if not checker._profond : return
        if not self.exists() : return
        celd=self.CELD.get()
        assert celd[0] > 0 , celd
        ngrel= celd[1]
        assert ngrel > 0 , celd
        mxsp = celd[2]
        assert mxsp > 0 , celd
        mxcmp = celd[3]
        lvari = mxcmp > 0
        for kgrel in range(ngrel) :
            debugr= celd[4+kgrel]-1
            nbel  = celd[debugr+1]
            modelo= celd[debugr+2]
            lgcata= celd[debugr+3]
            lggrel= celd[debugr+4]

            assert nbel > 0 , (nbel,kgrel)
            assert modelo >= 0 , (modelo,kgrel)
            if modelo == 0 :
                assert lgcata == 0 , (lgcata,kgrel)
                assert lggrel == 0 , (lggrel,kgrel)
                continue

            assert lgcata > 0 , (lgcata,kgrel)
            assert lggrel > 0 , (lggrel,kgrel)
            assert lggrel >= nbel*lgcata , (lggrel,kgrel,nbel,lgcata)

            for iel in range(nbel) :
                nbsp  =celd[debugr+4+4*iel+1]
                ncdyn =celd[debugr+4+4*iel+2]
                lgchel=celd[debugr+4+4*iel+3]
                adiel =celd[debugr+4+4*iel+4]
                assert nbsp > 0 , (nbsp,kgrel,iel)
                if lvari :
                   assert ncdyn > 0 , (ncdyn,kgrel,iel)
                   assert lgchel == lgcata*nbsp*ncdyn , (lgchel,lgcata,nbsp,ncdyn,kgrel,iel)
                else :
                   assert ncdyn == 0 , (ncdyn,kgrel,iel)
                   assert lgchel == lgcata*nbsp , (lgchel,lgcata,nbsp,kgrel,iel)
                assert adiel > 0 , (adiel,kgrel,iel)
