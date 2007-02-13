#@ MODIF sd_fiss_xfem SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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



class sd_fiss_xfem(AsBase):
#-------------------------------
    nomj = SDNom(fin=8)

    CARAFOND        = AsVR(lonmax=7)
    FONDFISS        = AsVR()
    GROUP_MA_ENRI   = AsVI()
    GROUP_NO_ENRI   = AsVI()

    FONDMULT        = Facultatif(AsVI())

    contact   = Facultatif(sd_contact_xfem(SDNom(nomj='.CONTACT')))

    MAILFISS__INDIC = AsVI(SDNom(nomj='.MAILFISS .INDIC'), lonmax=6, )
    mailfiss  = Facultatif(sd_mailfiss_xfem(SDNom(nomj='.MAILFISS')))


    # cham_no :
    basco  = Facultatif(sd_cham_no(SDNom(nomj='.BASCO')))
    basloc = sd_cham_no(SDNom(nomj='.BASLOC'))
    grlnno = sd_cham_no(SDNom(nomj='.GRLNNO'))
    grltno = sd_cham_no(SDNom(nomj='.GRLTNO'))
    lnno   = sd_cham_no(SDNom(nomj='.LNNO'))
    ltno   = sd_cham_no(SDNom(nomj='.LTNO'))
    stno   = sd_cham_no(SDNom(nomj='.STNO'))
    stnor  = sd_cham_no(SDNom(nomj='.STNOR'))

    # cham_elem :
    topose_pin  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.PIN')))
    topose_cns  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.CNS')))
    topose_hea  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.HEA')))
    topose_lon  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.LON')))
    topose_cri  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.CRI')))
    topofac_pi  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.PI')))
    topofac_ai  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.AI')))
    topofac_cf  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.CF')))
    topofac_lo  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.LO')))
    topofac_ba  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.BA')))
    pro_mes_el  = Facultatif(sd_cham_elem(SDNom(nomj='.PRO.MES_EL')))
    pro_normal  = Facultatif(sd_cham_elem(SDNom(nomj='.PRO.NORMAL')))
