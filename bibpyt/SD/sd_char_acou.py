#@ MODIF sd_char_acou SD  DATE 16/09/2008   AUTEUR PELLET J.PELLET 
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

from SD.sd_ligrel import sd_ligrel
from SD.sd_carte import sd_carte
from SD.sd_char_cine import sd_char_cine


class sd_char_acou(AsBase):
    nomj = SDNom(fin=8)
    chac_vitfa = Facultatif(sd_carte(SDNom(nomj='.CHAC.VITFA', fin=19)))
    CHAC_MODEL_NOMO = AsVK8(SDNom(nomj='.CHAC.MODEL.NOMO'), lonmax=1, )
    chac_cmult = Facultatif(sd_carte(SDNom(nomj='.CHAC.CMULT', fin=19)))
    chac_cimpo = Facultatif(sd_carte(SDNom(nomj='.CHAC.CIMPO', fin=19)))
    chac_imped = Facultatif(sd_carte(SDNom(nomj='.CHAC.IMPED', fin=19)))
    chac_ligre = Facultatif(sd_ligrel(SDNom(nomj='.CHAC.LIGRE', fin=19)))
    ELIM = Facultatif(sd_char_cine())
    TYPE = AsVK8(lonmax=1, )


