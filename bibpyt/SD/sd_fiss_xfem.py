#@ MODIF sd_fiss_xfem SD  DATE 08/10/2007   AUTEUR NISTOR I.NISTOR 
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

from SD.sd_cham_no   import sd_cham_no
from SD.sd_cham_elem import sd_cham_elem


class sd_mailfiss_xfem(AsBase):
#-------------------------------
    nomj = SDNom(fin=19)
    CTIP = AsVI(SDNom(nomj='.CTIP'))
    HEAV = AsVI(SDNom(nomj='.HEAV'))
    HECT = AsVI(SDNom(nomj='.HECT'))


class sd_contact_xfem(AsBase):
#-------------------------------
    nomj = SDNom(fin=16)
    CARACF  = AsVR(SDNom(nomj='.CARACF'))
    ECPDON  = AsVI(SDNom(nomj='.ECPDON'))
    LISCO   = AsVR(SDNom(nomj='.LISCO'))
    LISEQ   = AsVI(SDNom(nomj='.LISEQ'))
    LISRL   = AsVI(SDNom(nomj='.LISRL'))
    METHCO  = AsVI(SDNom(nomj='.METHCO'))
    XFEM    = AsVI(SDNom(nomj='.XFEM'))



class sd_xfem_com1(AsBase):
#-------------------------------
#  champs communs aux sd_fiss_xfem et sd_modele :

    nomj = SDNom(fin=8)

    # cham_no :
    BASLOC = sd_cham_no()
    LNNO   = sd_cham_no()
    LTNO   = sd_cham_no()
    STNO   = sd_cham_no()

    # cham_elem :
    topose_pin  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.PIN')))
    topose_cns  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.CNS')))
    topose_hea  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.HEA')))
    topose_lon  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.LON')))
    topofac_pi  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.PI')))
    topofac_ai  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.AI')))
    topofac_cf  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.CF')))
    topofac_lo  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.LO')))
    topofac_ba  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.BA')))
    topofac_gm  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.GM')))
    topofac_ge  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.GE')))
    topofac_oe  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.OE')))
    topofac_om  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.OM')))


class sd_fiss_xfem(AsBase):
#-------------------------------
    nomj = SDNom(fin=8)

    CARAFOND        = AsVR()
    GROUP_MA_ENRI   = AsVI()
    GROUP_NO_ENRI   = AsVI()

    FONDFISS        = Facultatif(AsVR())
    FONDMULT        = Facultatif(AsVI())

    CONTACT   = Facultatif(sd_contact_xfem())

    mailfiss_indic = AsVI(SDNom(nomj='.MAILFISS .INDIC'), lonmax=6, )
    MAILFISS  = Facultatif(sd_mailfiss_xfem())


    # sd_xfem_com1 (idem sd_modele) :
    com1  = sd_xfem_com1(SDNom(nomj=''))

    # cham_no :
    BASCO  = Facultatif(sd_cham_no())
    GRLNNO = sd_cham_no()
    GRLTNO = sd_cham_no()
    STNOR  = sd_cham_no()

    # cham_elem :
    topose_cri  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.CRI')))
    pro_mes_el  = Facultatif(sd_cham_elem(SDNom(nomj='.PRO.MES_EL')))
    pro_normal  = Facultatif(sd_cham_elem(SDNom(nomj='.PRO.NORMAL')))
