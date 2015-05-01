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
from SD.sd_titre import sd_titre


class sd_listr8(sd_titre):
#----------------------------------
    nomj = SDNom(fin=19)
    LPAS = AsVR()
    BINT = AsVR()
    NBPA = AsVI()
    VALE = AsVR()

    def proche(self, a, b):
        # retourne  1  si a est proche de b
        # retourne -1  si a est loin de b
        # retourne  0  si a = 0. (ou b = 0.)
        if a != 0. and b != 0.:
            erreur = abs(a - b) / (abs(a) + abs(b))
            if erreur < 1.e-12:
                return 1
            else:
                return -1
        else:
            return 0

    def check_1(self, checker):
        nbpa = self.NBPA.get()
        bint = self.BINT.get()
        lpas = self.LPAS.get()
        vale = self.VALE.get()

        # cas général :
        if len(vale) > 1:
            assert len(bint) == len(nbpa) + 1
            assert len(nbpa) == len(lpas)

            n1 = 0
            assert self.proche(vale[0], bint[0]) in (1, 0)
            for k in range(len(nbpa)):
                npas = nbpa[k]
                assert npas > 0
                n1 = n1 + npas
                assert self.proche(vale[n1], bint[k + 1]) in (
                    1, 0), (k + 1, vale[n1], bint[k + 1],)

            assert len(vale) == n1 + 1

        # cas particulier :
        if len(vale) == 1:
            assert len(bint) == 1
            assert len(nbpa) == 1
            assert len(lpas) == 1
            assert vale[0] == bint[0]
