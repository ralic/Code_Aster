# coding=utf-8
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
from SD.sd_util import *


class sd_listis(sd_titre):
#-------------------------------------
    nomj = SDNom(fin=19)
    LPAS = AsVI()
    BINT = AsVI()
    NBPA = AsVI()
    VALE = AsVI()


    def check_1(self,checker):
        nbpa=self.NBPA.get()
        bint=self.BINT.get()
        lpas=self.LPAS.get()
        vale=self.VALE.get()

        # cas général :
        if len(vale) > 1 :
            assert len(bint) == len(nbpa)+1
            assert len(nbpa) == len(lpas)

            n1=0
            assert  vale[0] == bint[0]
            for k in range(len(nbpa)) :
                npas=nbpa[k]
                assert npas > 0
                n1 = n1 + npas
                assert  vale[n1] == bint[k+1]

            assert len(vale) == n1+1
            assert sdu_monotone(vale) in (1,) , vale


        # cas particulier :
        if len(vale) == 1 :
            assert len(bint) == 1
            assert len(nbpa) == 1
            assert len(lpas) == 1
            assert  vale[0] == bint[0]
            assert  nbpa[0] == 0, nbpa
            assert  lpas[0] == 0, lpas
