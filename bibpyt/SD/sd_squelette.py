# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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

from SD.sd_maillage import sd_maillage
from SD.sd_util import *


class sd_squelette(sd_maillage):
#-------------------------------
    nomj = SDNom(fin=8)
    inv_skeleton = Facultatif(AsVI(SDNom(nomj='.INV.SKELETON'),))

    CORRES       = Facultatif(AsVI())
    NOMSST       = Facultatif(AsVK8(SDNom(debut=17),))

    # ENSEMBLE__ : TRANS , ANGL_NAUT
    TRANS        = Facultatif(AsVK8(lonmax=3))
    ANGL_NAUT    = Facultatif(AsVK8(lonmax=3))


    def check_SKELETON(self,checker):
        if not self.inv_skeleton.exists : return
        skeleton=self.inv_skeleton.get()
        dime=self.DIME.get()
        nbno=dime[0]
        assert len(skeleton)==2*nbno, (dime,len(skeleton))
        for k in skeleton :
            assert k > 0 , skeleton


    def check_TRANS_ANGL_NAUT(self,checker):
        trans     =self.TRANS.get()
        angl_naut =self.ANGL_NAUT.get()
        assert (trans and angl_naut) or ((not trans) and (not angl_naut))


    def check_CORRES(self,checker):
        if not self.CORRES.exists : return
        dime=self.DIME.get()
        corres=self.CORRES.get()
        sdu_tous_differents(self.CORRES,checker)
        assert len(corres) == dime[0], (dime, len(corres))
