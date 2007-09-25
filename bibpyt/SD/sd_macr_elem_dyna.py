#@ MODIF sd_macr_elem_dyna SD  DATE 24/09/2007   AUTEUR DEVESA G.DEVESA 
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
from SD.sd_util import *
from SD.sd_nume_ddl_gd import sd_nume_ddl_gd


class sd_macr_elem_dyna_m(AsBase):
#----------------------------------
    nomj = SDNom(fin=18)
    DESC = AsVI   (SDNom(nomj='_DESC'),lonmax=3)
    REFE = AsVK24 (SDNom(nomj='_REFE'),lonmax=2)
    VALE = AsVR   (SDNom(nomj='_VALE'))

    def check_macr_elem_dyna_m_1(self,checker):
        vale=self.VALE.get()
        if not vale : return  # si Facultatif()

        sdu_tous_compris(self.DESC,checker,vmin=1)
        nbdef=self.DESC.get()[1]
        sdu_compare(self.VALE,checker,len(vale),'==',(nbdef*(nbdef+1))/2,'LONMAX(VALE)')


class sd_macr_elem_dyna(AsBase):
#-------------------------------
    nomj = SDNom(fin=8)

    # description géométrique et topolique :
    DESM = AsVI(lonmax=10)
    REFM = AsVK8()
    LINO = AsVI()
    CONX = Facultatif(AsVI())

    # rigidité, masse, amortissement condensés :
    nume     = sd_nume_ddl_gd(SDNom(nomj=''))

    MAEL_RAID = sd_macr_elem_dyna_m()
    MAEL_MASS = sd_macr_elem_dyna_m()
    MAEL_AMOR = Facultatif(sd_macr_elem_dyna_m())

    MAEL_INER_VALE = AsVR()
    MAEL_INER_REFE = AsVK24(lonmax=2, )

    MAEL_DESC      = AsVI(lonmax=3, )
    MAEL_REFE      = AsVK24(lonmax=2, )



    def check_macr_elem_dyna_1(self,checker):
        nbdef=self.MAEL_MASS.DESC.get()[1]
        sdu_compare(self.MAEL_INER_VALE, checker,len(self.MAEL_INER_VALE.get()),'==',3*nbdef,'LONMAX(MAEL_INER_VALE)')

