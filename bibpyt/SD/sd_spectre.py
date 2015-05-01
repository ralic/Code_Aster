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
from SD.sd_modele import sd_modele
from SD.sd_cara_elem import sd_cara_elem
from SD.sd_fonction import sd_fonction
from SD.sd_table_fonction import sd_table_fonction


types_possibles = [  # liste des divers types de spectres :
    'SPEC_LONG_COR_1',
    'SPEC_LONG_COR_2',
    'SPEC_LONG_COR_3',
    'SPEC_LONG_COR_4',
    'SPEC_CORR_CONV_1',
    'SPEC_CORR_CONV_2',
    'SPEC_CORR_CONV_3',
    'SPEC_FONC_FORME',
    'SPEC_EXCI_POINT', ]


class sd_spectre(sd_titre):
#------------------------------------
    nomj = SDNom(fin=19)

    VAIN = AsVI()
    VATE = AsVK16()

    VARE = Facultatif(AsVR())
    NNOE = Facultatif(AsVK8())

    def u_type(self):
        vate = self.VATE.get()
        type = vate[0].strip()
        assert type in types_possibles
        return type

    def u_nbno(self):
        vain = self.VAIN.get()
        intesp = vain[1]
        nbno = vain[2]
        return (intesp, nbno)

    def check_VAIN(self, checker):
    #-------------------------------
        vain = self.VAIN.get()
        itype = vain[0]
        assert itype in (1, 2, 3, 4, 11, 21), vain

        type = self.u_type()
        if itype == 1:
            assert type in ('SPEC_LONG_COR_1', 'SPEC_CORR_CONV_1'), vain
        if itype == 2:
            assert type in ('SPEC_LONG_COR_2', 'SPEC_CORR_CONV_2'), vain
        if itype == 3:
            assert type in ('SPEC_LONG_COR_3', 'SPEC_CORR_CONV_3'), vain
        if itype == 4:
            assert type in ('SPEC_LONG_COR_4',), vain
        if itype == 11:
            assert type in ('SPEC_FONC_FORME',), vain
        if itype == 21:
            assert type in ('SPEC_EXCI_POINT',), vain

        if type in ('SPEC_EXCI_POINT', 'SPEC_FONC_FORME'):
            assert len(vain) == 3, vain
            assert vain[1] in (0, 1), vain
            assert vain[2] >= 0, vain
        else:
            assert len(vain) == 1, vain

    def check_VARE(self, checker):
    #-------------------------------
        vare = self.VARE.get()
        type = self.u_type()

        if type == 'SPEC_FONC_FORME':
            assert not vare
            return

        elif type == 'SPEC_EXCI_POINT':
            intesp, nbno = self.u_nbno()
            if intesp == 0:
                assert len(vare) == nbno, vare
            else:
                assert len(vare) == 1, vare

        else:
            assert len(vare) == 12, vare

    def check_VATE(self, checker):
    #-------------------------------
        vate = self.VATE.get_stripped()
        type = self.u_type()

        if type == 'SPEC_EXCI_POINT':
        #---------------------------------
            intesp, nbno = self.u_nbno()
            if intesp == 0:
                assert len(vate) == 4 + nbno, vate
            else:
                assert len(vate) == 5, vate
            sd2 = sd_cara_elem(vate[1])
            sd2.check(checker)
            sd2 = sd_modele(vate[2])
            sd2.check(checker)
            if vate[3] == 'GRAPPE_2':
                assert vate[4] in ('ASC_CEN', 'ASC_EXC', 'DES_CEN', 'DES_EXC')
            else:
                for x in vate[4:]:
                    assert x in ('FORCE', 'MOMENT')

        elif type == 'SPEC_FONC_FORME':
        #---------------------------------
            intesp, nbno = self.u_nbno()
            if intesp == 0:
                nbfonc = len(vate) - 4
                assert nbfonc > 0, vate
            else:
                assert len(vate) == 5, vate
            sd2 = sd_cara_elem(vate[1])
            sd2.check(checker)
            sd2 = sd_modele(vate[2])
            sd2.check(checker)
            if vate[3] == 'GRAPPE_1':
                assert vate[4] in ('DEBIT_180', 'DEBIT_300')
            else:
                for x in vate[4:]:
                    sd2 = sd_table_fonction(x)
                    sd2.check(checker)

        elif type == 'SPEC_LONG_COR_1':
        #---------------------------------
            sd2 = sd_fonction(vate[2])
            sd2.check(checker)
            assert vate[3] == 'VISC_CINE'

        elif type == 'SPEC_LONG_COR_2':
        #---------------------------------
            sd2 = sd_fonction(vate[2])
            sd2.check(checker)
            assert vate[5] == 'BETA'

        elif type == 'SPEC_LONG_COR_3':
        #---------------------------------
            sd2 = sd_fonction(vate[2])
            sd2.check(checker)
            assert vate[7] == 'BETA_2'

        elif type == 'SPEC_LONG_COR_4':
        #---------------------------------
            sd2 = sd_fonction(vate[2])
            sd2.check(checker)
            assert vate[5] == 'GAMMA'

        elif type == 'SPEC_CORR_CONV_1':
        #---------------------------------
            assert vate[9] == 'COEF_VITE_FLUI_O'
            assert vate[10] in ('GENERALE', 'CORCOS', 'AU_YANG')

        elif type == 'SPEC_CORR_CONV_2':
        #---------------------------------
            sd2 = sd_fonction(vate[1])
            sd2.check(checker)
            assert vate[4] in ('GENERALE', 'CORCOS', 'AU_YANG')
            assert vate[6] == 'COEF_VITE_FLUI_O'

        elif type == 'SPEC_CORR_CONV_3':
        #---------------------------------
            sd2 = sd_table_fonction(vate[1])
            sd2.check(checker)

    def check_NNOE(self, checker):
    #-------------------------------
        nnoe = self.NNOE.get()
        type = self.u_type()

        if type in ('SPEC_FONC_FORME', 'SPEC_EXCI_POINT'):
            intesp, nbno = self.u_nbno()
            assert len(nnoe) == nbno
        else:
            assert not nnoe
