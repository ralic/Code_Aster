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


class sd_solveur(AsBase):
    nomj = SDNom(fin=19)
    SLVK = AsVK24(SDNom(debut=19), lonmax=14, )
    SLVR = AsVR(SDNom(debut=19), lonmax=4, )
    SLVI = AsVI(SDNom(debut=19), lonmax=8, )

    def check_SLVK(self, checker):
    #---------------------------------------------
        slvk = self.SLVK.get_stripped()
        method = slvk[0]
        assert slvk[12] in ('OUI', 'NON', '')
        assert slvk[4] in ('XXXX')
        if method == 'MUMPS':
            assert slvk[1] in ('AUTO', 'SANS')
            assert slvk[2] in ('NONSYM', 'SYMGEN', 'SYMDEF', 'AUTO')
            assert slvk[3] in (
                'AMD', 'AMF', 'PORD', 'METIS', 'QAMD', 'AUTO', 'SCOTCH')
            assert slvk[5] in ('LAGR2', 'NON',), slvk
            assert slvk[6] in ('OUI', 'NON', 'XXXX')
            assert slvk[7] in ('OUI', 'NON', 'XXXX')
            assert slvk[8] in (
                'IN_CORE', 'OUT_OF_CORE', 'AUTO', 'EVAL', 'XXXX')
            assert slvk[9] in ('OUI', 'NON', 'XXXX')
            assert slvk[10] in ('SANS', 'AUTO', 'FORCE', 'XXXX', 'MINI')
            assert slvk[11] in ('XXXX', '4.10.0', 'SNAPSHOT-2015-07-23conso', '5.0.1')
        elif method == 'MULT_FRONT':
            assert slvk[1] in ('XXXX')
            assert slvk[2] in ('XXXX')
            assert slvk[3] in ('MD', 'MDA', 'METIS')
            assert slvk[5] in ('XXXX')
            assert slvk[6] in ('XXXX')
            assert slvk[7] in ('XXXX')
            assert slvk[8] in ('XXXX')
            assert slvk[9] in ('XXXX')
            assert slvk[10] in ('XXXX')
            assert slvk[11] in ('XXXX')
        elif method == 'LDLT':
            assert slvk[1] in ('XXXX')
            assert slvk[2] in ('XXXX')
            assert slvk[3] in ('RCMK', 'SANS')
            assert slvk[5] in ('XXXX')
            assert slvk[6] in ('XXXX')
            assert slvk[7] in ('XXXX')
            assert slvk[8] in ('XXXX')
            assert slvk[9] in ('XXXX')
            assert slvk[10] in ('XXXX')
            assert slvk[11] in ('XXXX')
        elif method == 'GCPC':
            assert slvk[1] in ('LDLT_INC', 'LDLT_SP', 'SANS')
            assert slvk[3] in ('RCMK', 'SANS')
            assert slvk[5] in ('XXXX')
            assert slvk[6] in ('XXXX')
            assert slvk[7] in ('XXXX')
            assert slvk[8] in ('XXXX')
            assert slvk[9] in ('XXXX')
            assert slvk[10] in ('XXXX')
            assert slvk[11] in ('XXXX')
        elif method == 'PETSC':
            assert slvk[1] in (
                'LDLT_INC', 'LDLT_SP', 'JACOBI', 'SOR', 'ML', 'BOOMER', 'SANS')
            assert slvk[3] in ('RCMK', 'SANS')
            assert slvk[5] in ('CG', 'CR', 'GMRES', 'GCR')
            assert slvk[6] in ('XXXX')
            assert slvk[7] in ('XXXX')
            assert slvk[8] in ('XXXX')
            assert slvk[9] in ('XXXX')
            assert slvk[10] in ('XXXX')
            assert slvk[11] in ('XXXX')
        else:
            assert False, method
