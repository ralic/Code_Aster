#@ MODIF sd_xfem SD  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
from SD.sd_carte     import sd_carte
from SD.sd_util      import *



#-------------------------------
#       I. sd fiss_xfem
#-------------------------------

class sd_fiss_xfem(AsBase):
    nomj = SDNom(fin=8)

    INFO   = AsVK16(lonmax=2,)   # info discontinuite

# I.1) objets relatifs aux level sets

    LNNO   = sd_cham_no()
    LTNO   = sd_cham_no()
    GRLNNO = sd_cham_no()
    GRLTNO = sd_cham_no()
    BASLOC = sd_cham_no()
    FONDFISS        = Facultatif(AsVR())
    BASEFOND        = Facultatif(AsVR())
    FONDMULT        = Facultatif(AsVI())
    CARAFOND        = Facultatif(AsVR(lonmax=12,))
    RAYON           = Facultatif(AsVR(lonmax=1,))
    TORE            = Facultatif(AsVL())
    GRIAUX          = Facultatif(AsVK8(lonmax=1,))
    
# I.2) objets relatifs à l'enrichissement

    GROUP_MA_ENRI   = AsVI()
    GROUP_NO_ENRI   = AsVI()
    STNO   = sd_cham_no()
    STNOR  = sd_cham_no()

    MAILFISS_CTIP  = Facultatif(AsVI(SDNom(nomj='.MAILFISS  .CTIP')))
    MAILFISS_HEAV  = Facultatif(AsVI(SDNom(nomj='.MAILFISS  .HEAV')))
    MAILFISS_HECT  = Facultatif(AsVI(SDNom(nomj='.MAILFISS  .HECT')))
    MAILFISS_INDIC = AsVI(SDNom(nomj='.MAILFISS .INDIC'), lonmax=6, )
    LISNOH         = Facultatif(AsVI())

# I.3) objets relatifs à la propagation

    PRO_MES_EL  = Facultatif(sd_cham_elem(SDNom(nomj='.PRO.MES_EL')))
    PRO_NORMAL  = Facultatif(sd_cham_elem(SDNom(nomj='.PRO.NORMAL')))

# I.4) objets relatifs au contact

    BASCO  = Facultatif(sd_cham_no())
    LISCO  = Facultatif(AsVR(SDNom(nomj='.LISCO')))
    LISEQ  = Facultatif(AsVI(SDNom(nomj='.LISEQ')))
    LISRL  = Facultatif(AsVI(SDNom(nomj='.LISRL')))
    LISUP  = Facultatif(AsVI(SDNom(nomj='.LISUP')))    


# 1.5) vérifications d'existence :

    def check_existence(self,checker) :
        sdu_ensemble((self.FONDFISS, self.FONDMULT))
        sdu_ensemble((self.LISRL, self.LISCO))
        sdu_ensemble((self.PRO_MES_EL.CELD, self.PRO_NORMAL.CELD))
        sdu_ensemble((self.RAYON, self.TORE))

#-------------------------------
#       II. sd modele
#-------------------------------

class sd_modele_xfem(AsBase):
    nomj = SDNom(fin=8)

# II.1) objets relatifs aux sous-éléments

    TOPOSE_PIN  = sd_cham_elem(SDNom(nomj='.TOPOSE.PIN'))
    TOPOSE_CNS  = sd_cham_elem(SDNom(nomj='.TOPOSE.CNS'))
    TOPOSE_HEA  = sd_cham_elem(SDNom(nomj='.TOPOSE.HEA'))
    TOPOSE_LON  = sd_cham_elem(SDNom(nomj='.TOPOSE.LON'))
    TOPOSE_AIN  = sd_cham_elem(SDNom(nomj='.TOPOSE.AIN'))
    TOPOSE_CRI  = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.CRI')))

# II.2) objets relatifs aux facettes de contact

    TOPOFAC_PI  = sd_cham_elem(SDNom(nomj='.TOPOFAC.PI'))
    TOPOFAC_AI  = sd_cham_elem(SDNom(nomj='.TOPOFAC.AI'))
    TOPOFAC_CF  = sd_cham_elem(SDNom(nomj='.TOPOFAC.CF'))
    TOPOFAC_LO  = sd_cham_elem(SDNom(nomj='.TOPOFAC.LO'))
    TOPOFAC_BA  = sd_cham_elem(SDNom(nomj='.TOPOFAC.BA'))

    TOPOFAC_GM  = sd_cham_elem(SDNom(nomj='.TOPOFAC.GM'))
    TOPOFAC_GE  = sd_cham_elem(SDNom(nomj='.TOPOFAC.GE'))
    TOPOFAC_OE  = sd_cham_elem(SDNom(nomj='.TOPOFAC.OE'))
    TOPOFAC_OM  = sd_cham_elem(SDNom(nomj='.TOPOFAC.OM'))

# II.3) objets concaténés relatifs aux level sets

    LNNO   = sd_cham_no()
    LTNO   = sd_cham_no()
    BASLOC = sd_cham_no()
    STNO   = sd_cham_no()

# II.4) autres objets

    XFEM_CONT   = AsVI(lonmax=1) # contact ou pas
    FISS   = AsVK8()             # noms des fissures
    NFIS   = AsVI(lonmax=1,)     # nombre de fissures
    XMAFIS = sd_carte()          # pour chaque maille : nom de la fissure

