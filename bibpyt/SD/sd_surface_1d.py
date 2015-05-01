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

from SD.sd_reperage_1d import sd_reperage_1d
from SD.sd_reperage_omega import sd_reperage_omega


class sd_surface_1d(AsBase):
#--------------------------------
    nomj = SDNom(fin=13)
    sgtel = sd_reperage_1d(SDNom(nomj='.SGTEL'))
    repom = sd_reperage_omega(SDNom(nomj=''))
    CONEX_ORIG = AsVI(SDNom(nomj='.CONEX.ORIG'))
    CONEX_EXTR = AsVI(SDNom(nomj='.CONEX.EXTR'))
    DESC = AsVR(SDNom(debut=13), lonmax=6, )
