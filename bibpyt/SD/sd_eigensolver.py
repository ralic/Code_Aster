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


class sd_eigensolver(AsBase):
    nomj = SDNom(fin=19)
    ESVK = AsVK24(SDNom(debut=19), lonmax=20, )
    ESVR = AsVR(SDNom(debut=19), lonmax=15, )
    ESVI = AsVI(SDNom(debut=19), lonmax=15, )

    def check_ESVK(self, checker):
    #---------------------------------------------
        esvk = self.ESVK.get_stripped()
#
        assert esvk[0] in ('DYNAMIQUE', 'MODE_FLAMB', 'GENERAL')
        if esvk[0] == 'DYNAMIQUE':
            assert esvk[4] in (
                'PLUS_PETITE', 'CENTRE', 'BANDE', 'TOUT', 'PLUS_GRANDE')
        elif esvk[0] == 'MODE_FLAMB':
            assert esvk[4] in ('PLUS_PETITE', 'CENTRE', 'BANDE', 'TOUT')
        elif esvk[0] == 'GENERAL':
            assert esvk[4] in ('PLUS_PETITE', 'CENTRE', 'BANDE', 'TOUT')
        assert esvk[5] in ('SORENSEN', 'TRI_DIAG', 'JACOBI', 'QZ')
        assert esvk[6] in ('SANS', 'MODE_RIGIDE')
        assert esvk[7] in ('OUI', 'NON')
        assert esvk[9] in ('OUI', 'NON')
        assert esvk[10] in ('OUI', 'NON')
        assert esvk[15] in ('R', 'I', 'C')
        if esvk[5] == 'QZ':
            assert esvk[16] in ('QZ_SIMPLE', 'QZ_EQUI', 'QZ_QR')
