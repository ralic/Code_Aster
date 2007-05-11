#@ MODIF sd_interf_dyna_clas SD  DATE 09/05/2007   AUTEUR PELLET J.PELLET 
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
from SD.sd_maillage import sd_maillage
from SD.sd_nume_ddl import sd_nume_ddl
from SD.sd_util import *


class sd_interf_dyna_clas(AsBase):
#---------------------------------------
    nomj = SDNom(fin=8)
    IDC_NOMS = AsObject(genr='N', xous='S', type='K', ltyp=8, )
    IDC_DDAC = AsColl(acces='NU', stockage='DISPERSE', modelong='VARIABLE', type='I', )
    IDC_DY_FREQ = AsVR(lonmax=1, )
    IDC_LINO = AsColl(acces='NU', stockage='DISPERSE', modelong='VARIABLE', type='I', )
    IDC_TYPE = AsVK8()
    IDC_DESC = AsVI(lonmax=5, )
    IDC_DEFO = AsVI()
    IDC_REFE = AsVK24(lonmax=3, )


    def check_interf_dyna_class_1(self,checker):
        refe=self.IDC_REFE.get()
        desc=self.IDC_DESC.get()

        sd2=sd_maillage(refe[0]) ; sd2.check(checker)
        sd2=sd_nume_ddl(refe[1]) ; sd2.check(checker)
        sdu_compare(self.IDC_DESC, checker, refe[2].strip(), '==', '', comment='REFE[2]')

        nomgd=sdu_nom_gd(desc[3])
        sdu_compare(self.IDC_DESC, checker, nomgd, '==',  'DEPL_R', comment='DESC[3]')

        sdu_compare(self.IDC_DESC, checker, desc[0], '==',  1, comment='DESC[0]')
        sdu_compare(self.IDC_DESC, checker, desc[4], '>',   0, comment='DESC[4]')

        freq=self.IDC_DY_FREQ.get()
        sdu_compare(self.IDC_DY_FREQ, checker, freq[0], '>',  0., comment='FREQ[0]')

        # nbint = ??? à finir PB_AJACOT
