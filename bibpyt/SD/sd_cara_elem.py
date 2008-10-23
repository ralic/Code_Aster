#@ MODIF sd_cara_elem SD  DATE 23/10/2008   AUTEUR TORKHANI M.TORKHANI 
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

from SD.sd_cham_elem import sd_cham_elem
from SD.sd_carte import sd_carte

class sd_cara_elem(AsBase):
#--------------------------
    nomj = SDNom(fin=8)
    CARGENBA = Facultatif(sd_carte())
    CAFIBR   = Facultatif(sd_cham_elem())
    CARMASSI = Facultatif(sd_carte())
    CARCABLE = Facultatif(sd_carte())
    CARCOQUE = Facultatif(sd_carte())
    CARGEOBA = Facultatif(sd_carte())
    CANBSP   = Facultatif(sd_cham_elem())
    CARDISCK = Facultatif(sd_carte())
    CARARCPO = Facultatif(sd_carte())
    CARGENPO = Facultatif(sd_carte())
    CARDISCM = Facultatif(sd_carte())
    CARORIEN = Facultatif(sd_carte())
    CARDISCA = Facultatif(sd_carte())
    CVENTCXF = Facultatif(sd_carte())
    CARPOUFL = Facultatif(sd_carte())
    CARGEOPO = Facultatif(sd_carte())
    CARRIGXN = Facultatif(AsVK8())
    CARRIGXV = Facultatif(AsVR())
    CARDNSCK = Facultatif(sd_carte())
    CARDNSCM = Facultatif(sd_carte())
    CARDNSCA = Facultatif(sd_carte())
    CARDINFO = Facultatif(sd_carte())


