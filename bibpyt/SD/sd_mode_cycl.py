#@ MODIF sd_mode_cycl SD  DATE 23/10/2007   AUTEUR BODEL C.BODEL 
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
from SD.sd_interf_dyna_clas import sd_interf_dyna_clas
from SD.sd_base_modale import sd_base_modale
from SD.sd_util import *


class sd_mode_cycl(AsBase):
#--------------------------
    nomj = SDNom(fin=8)
    CYCL_TYPE = AsVK8(lonmax=1, )
    CYCL_CMODE = AsVC()
    CYCL_NBSC = AsVI(lonmax=1, )
    CYCL_DIAM = AsVI()
    CYCL_REFE = AsVK24(lonmax=3, )
    CYCL_DESC = AsVI(lonmax=4, )
    CYCL_FREQ = AsVR()
    CYCL_NUIN = AsVI(lonmax=3, )


    def u_dime(self) :
        desc=self.CYCL_DESC.get()
        nb_mod  = desc[0] ; assert nb_mod   >  0
        nb_ddl  = desc[1] ; assert nb_ddl   >  0
        nb_ddli = desc[2] ; assert nb_ddli  >= 0
        nb_freq = desc[3] ; assert nb_freq  >  0
        nb_diam=self.CYCL_DIAM.lonmax / 2  ; assert nb_diam > 0
        assert self.CYCL_DIAM.lonmax == 2*nb_diam
        return (nb_mod, nb_ddl, nb_ddli, nb_freq, nb_diam)

    def check_REFE(self,checker) :
        refe=self.CYCL_REFE.get_stripped()
        sd2=sd_maillage(refe[0]); sd2.check
        sd2=sd_interf_dyna_clas(refe[1]); sd2.check
        sd2=sd_base_modale(refe[2]); sd2.check


    def check_NUIN(self,checker) :
        nuin=self.CYCL_NUIN.get()
        assert nuin[0] >  0 , nuin
        assert nuin[1] >  0 , nuin
        assert nuin[2] >= 0 , nuin


    def check_NBSC(self,checker) :
        nbsc=self.CYCL_NBSC.get()
        assert nbsc[0] > 0 , nbsc


    def check_TYPE(self,checker) :
        type=self.CYCL_TYPE.get_stripped()
        assert type[0] in ('MNEAL', 'CRAIGB', 'CB_HARMO', 'AUCUN') ,type


    def check_CMODE(self,checker) :
        nb_mod, nb_ddl, nb_ddli, nb_freq, nb_diam = self.u_dime()
        assert self.CYCL_CMODE.lonmax== nb_diam*nb_freq*(nb_mod+nb_ddl+nb_ddli)


    def check_DIAM(self,checker) :
        diam=self.CYCL_DIAM.get()
        nb_diam=len(diam)/2
        for x in diam[:nb_diam] : assert x >= 0 , diam
        for x in diam[nb_diam:] : assert x >  0 , diam
        sdu_tous_differents(self.CYCL_DIAM,checker,diam[:nb_diam])


    def check_FREQ(self,checker) :
        nb_mod, nb_ddl, nb_ddli, nb_freq, nb_diam = self.u_dime()
        freq=self.CYCL_FREQ.get()
        assert len(freq) == nb_diam*nb_freq ,(self.CYCL_DESC.get(),len(freq))
        for x in freq : assert x >= 0 , freq


