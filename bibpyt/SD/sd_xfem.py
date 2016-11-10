# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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

from SD.sd_cham_no import sd_cham_no
from SD.sd_cham_elem import sd_cham_elem
from SD.sd_carte import sd_carte
from SD.sd_util import *
from SD.sd_l_table import sd_l_table


#-------------------------------
#       I. sd fiss_xfem
#-------------------------------
class sd_fiss_xfem(AsBase):
    nomj = SDNom(fin=8)

    INFO = AsVK16(lonmax=3,)   # info discontinuite et type de fissure
    MAILLAGE = AsVK8(lonmax=1,)

# I.1) objets relatifs aux level sets

    LNNO = sd_cham_no()
    LTNO = sd_cham_no()
    GRLNNO = sd_cham_no()
    GRLTNO = sd_cham_no()
    BASLOC = sd_cham_no()
    FONDFISS = Facultatif(AsVR())
    NOFACPTFON = Facultatif(AsVI())
    BASEFOND = Facultatif(AsVR())
    FOND_TAILLE_R = Facultatif(AsVR(SDNom(nomj='.FOND.TAILLE_R'),))
    FONDMULT = Facultatif(AsVI())
    CARAFOND = Facultatif(AsVR(lonmax=2,))
    JONFISS = Facultatif(AsVK8())
    JONCOEF = Facultatif(AsVI())
    CHAMPS_LVS = Facultatif(AsVL(SDNom(nomj='.CHAMPS.LVS'), lonmax=1))
#   objets relatifs a la grille auxiliaire
    GRILLE_MAILLAGE = Facultatif(AsVK8(SDNom(nomj='.GRI.MAILLA'), lonmax=1,))
    GRILLE_LNNO = Facultatif(sd_cham_no(SDNom(nomj='.GRI.LNNO')))
    GRILLE_LTNO = Facultatif(sd_cham_no(SDNom(nomj='.GRI.LTNO')))
    GRILLE_GRLNNO = Facultatif(sd_cham_no(SDNom(nomj='.GRI.GRLNNO')))
    GRILLE_GRLTNO = Facultatif(sd_cham_no(SDNom(nomj='.GRI.GRLTNO')))
    FONDFISG = Facultatif(AsVR())

# I.2) objets relatifs a l'enrichissement

    GROUP_MA_ENRI = AsVI()
    GROUP_NO_ENRI = AsVI()
    STNO = sd_cham_no()
    STNOR = sd_cham_no()

    MAILFISS_CTIP = Facultatif(AsVI(SDNom(nomj='.MAILFISS.CTIP')))
    MAILFISS_HEAV = Facultatif(AsVI(SDNom(nomj='.MAILFISS.HEAV')))
    MAILFISS_HECT = Facultatif(AsVI(SDNom(nomj='.MAILFISS.HECT')))
    MAILFISS_MAFOND = Facultatif(AsVI(SDNom(nomj='.MAILFISS.MAFOND')))
    MAILFISS_CONT = Facultatif(AsVI(SDNom(nomj='.MAILFISS.CONT')))

# I.3) objets relatifs a la propagation

#   objets relatifs a la localisation du domaine de calcul
    PRO_RAYON_TORE = Facultatif(
        AsVR(SDNom(nomj='.PRO.RAYON_TORE'), lonmax=1,))
    PRO_NOEUD_TORE = Facultatif(AsVL(SDNom(nomj='.PRO.NOEUD_TORE')))
    PRO_NOEUD_PROJ = Facultatif(AsVL(SDNom(nomj='.PRO.NOEUD_PROJ')))

# I.4) objets relatifs au contact

    LISEQ = Facultatif(AsVI(SDNom(nomj='.LISEQ')))
    LISEQ_LAGR = Facultatif(AsVI(SDNom(nomj='.LISEQ_LAGR')))

# I.4) objets relatifs au contact
    # une sd_modele peut avoir une "sd_l_table" contenant des grandeurs
    # caracteristiques de l'etude :
    lt = Facultatif(sd_l_table(SDNom(nomj='')))

# 1.5) verifications d'existence :

    def check_existence(self, checker):
        # si un fond existe alors ...
# sdu_ensemble((self.FONDFISS, self.FONDMULT, self.BASEFOND,
# self.MAILFISS_MAFOND))
        sdu_ensemble((self.FONDFISS, self.FONDMULT, self.BASEFOND))

        # si ....
        sdu_ensemble((self.PRO_RAYON_TORE, self.PRO_NOEUD_TORE))
        sdu_ensemble(
            (self.GRILLE_MAILLAGE, self.GRILLE_LNNO.DESC, self.GRILLE_GRLNNO.DESC))
        sdu_ensemble((self.GRILLE_LTNO.DESC, self.GRILLE_GRLTNO.DESC))

#-------------------------------
#       II. sd modele
#-------------------------------


class sd_modele_xfem(AsBase):
    nomj = SDNom(fin=8)

#   nom du modele sain
    MODELE_SAIN = AsVK8(lonmax=1,)

# II.1) objets relatifs aux sous-elements

    TOPOSE_PIN = sd_cham_elem(SDNom(nomj='.TOPOSE.PIN'))
    TOPOSE_CNS = sd_cham_elem(SDNom(nomj='.TOPOSE.CNS'))
    TOPOSE_HEA = sd_cham_elem(SDNom(nomj='.TOPOSE.HEA'))
    TOPOSE_LON = sd_cham_elem(SDNom(nomj='.TOPOSE.LON'))
    TOPOSE_AIN = sd_cham_elem(SDNom(nomj='.TOPOSE.PAI'))
    TOPOSE_PMI = sd_cham_elem(SDNom(nomj='.TOPOSE.PMI'))
    TOPOSE_PJO = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.PJO')))
    TOPOSE_CRI = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOSE.CRI')))

# II.2) objets relatifs aux facettes de contact
    TOPOFAC_PI = sd_cham_elem(SDNom(nomj='.TOPOFAC.PI'))
    TOPOFAC_AI = sd_cham_elem(SDNom(nomj='.TOPOFAC.AI'))
    TOPOFAC_CF = sd_cham_elem(SDNom(nomj='.TOPOFAC.CF'))
    TOPOFAC_LO = sd_cham_elem(SDNom(nomj='.TOPOFAC.LO'))
    TOPOFAC_BA = sd_cham_elem(SDNom(nomj='.TOPOFAC.BA'))
    TOPOFAC_HE = Facultatif(sd_cham_elem(SDNom(nomj='.TOPOFAC.HE')))
    TOPOFAC_OE = sd_cham_elem(SDNom(nomj='.TOPOFAC.OE'))

# II.3) objets relatifs a l'enrichissement heaviside

    TOPONO_HNO  = sd_cham_elem(SDNom(nomj='.TOPONO.HNO'))
    TOPONO_HSE  = sd_cham_elem(SDNom(nomj='.TOPONO.HSE'))
    TOPONO_HFA  = sd_cham_elem(SDNom(nomj='.TOPONO.HFA'))

# II.4) objets concatenes relatifs aux level sets

    LNNO = sd_cham_elem(SDNom(nomj='.LNNO'))
    LTNO = sd_cham_elem(SDNom(nomj='.LTNO'))
    BASLOC = sd_cham_elem(SDNom(nomj='.BASLOC'))
    STNO = sd_cham_elem(SDNom(nomj='.STNO'))
    FISSNO = sd_cham_elem(SDNom(nomj='.FISSNO'))
    FISSCO = sd_cham_elem(SDNom(nomj='.FISSCO'))
    HEAVNO = sd_cham_elem(SDNom(nomj='.HEAVNO'))
    NOXFEM = sd_cham_no()

# II.5) autres objets

    XFEM_CONT = AsVI(lonmax=1)  # contact ou pas
    FISS = AsVK8()             # noms des fissures
    NFIS = AsVI(lonmax=1,)     # nombre de fissures
    XMAFIS = sd_cham_elem(SDNom(nomj='.XMAFIS'))
                          # pour chaque maille : nom de(s) fissure(s)
    PRE_COND = AsVK8(lonmax=1)  # preconditionnement ou pas

    # si le modele a ete cree par MODI_MODELE_XFEM / MODELE_THER
    MODELE_THER = Facultatif( AsVK8(lonmax=1,) )
