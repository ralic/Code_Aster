#@ MODIF sd_resuelem SD  DATE 22/10/2007   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
from SD.sd_ligrel import sd_ligrel


class sd_resuelem(AsBase):
    nomj = SDNom(fin=19)
    NOLI = AsVK24(SDNom(debut=19), lonmax=2, )
    RESL = AsColl(SDNom(debut=19), acces='NU', stockage='DISPERSE', modelong='VARIABLE', type=Parmi('C', 'R'), ltyp=Parmi(16, 8), )
    DESC = AsVI(SDNom(debut=19), docu='RESL', )


    def check_1(self, checker):
        noli = self.NOLI.get_stripped()
        sd2=sd_ligrel(noli[0]) ; sd2.check(checker)
        assert noli[1] != '' , noli

        desc = self.DESC.get()
        assert desc[0] > 0 and desc[0] < 1000 , desc
        nbgr=desc[1]
        assert nbgr > 0  , desc
        assert len(desc)==nbgr+2, desc
        assert self.RESL.nmaxoc == nbgr, desc
        for k in desc :
            assert k >= 0, desc
