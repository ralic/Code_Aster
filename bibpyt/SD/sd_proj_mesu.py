#@ MODIF sd_proj_mesu SD  DATE 15/01/2008   AUTEUR PELLET J.PELLET 
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
from sd_modele  import sd_modele



class sd_proj_mesu(AsBase):
#-------------------------------------
    nomj = SDNom(fin=18)
    PJMNO = AsVI()
    PJMRG = AsVK8()
    PJMBP = AsVR()
    PJMRF = AsVK16(lonmax=5)

    # si PROJ_MESU_MODAL :
    PJMOR = Facultatif(AsVR())

    # si MACR_ELEM_STAT :
    PJMIG    = Facultatif(AsVR())
    PJMMM    = Facultatif(AsObject(genr='V',type=Parmi('C', 'R')))


    def exists(self):
    #  retourne .true. si la SD semble exister
        return self.PJMNO.exists


    def check_1(self, checker):
    #------------------------------------
        if not self.exists() : return

        nbutil=self.PJMNO.lonuti
        assert nbutil > 0 , nbutil

        # vérifications communes :
        assert self.PJMRG.lonmax >= nbutil
        n1=self.PJMBP.lonmax
        nbmode=n1/nbutil
        assert n1==nbutil*nbmode , (nbmode,nbutil,n1)
        assert self.PJMRF.exists
        pjmrf=self.PJMRF.get_stripped()
        sd2=sd_modele(pjmrf[0]) ; sd2.check(checker)
        assert pjmrf[1] != '' , pjmrf
        assert pjmrf[2] != '' , pjmrf

        # quel cas de figure : PROJ_MESU_MODAL ou MACR_ELEM_STAT ?
        lproj=self.PJMOR.exists

        # si PROJ_MESU_MODAL :
        if lproj :
            nbcapt=nbutil
            assert self.PJMOR.lonmax >= 3*nbcapt
            assert not self.PJMIG.exists
            assert pjmrf[3] == '' , pjmrf
            assert pjmrf[4] == '' , pjmrf

        # si MACR_ELEM_STAT :
        else :
            nbddle=nbutil
            assert self.PJMIG.exists
            assert self.PJMMM.exists
            n1=self.PJMIG.lonmax
            nbmoid=n1/nbddle
            assert n1==nbddle*nbmoid , (nbmodi,nbddle,n1)

            assert pjmrf[3] != '' , pjmrf
            sd2=sd_proj_mesu(pjmrf[4]) ; sd2.check(checker)

