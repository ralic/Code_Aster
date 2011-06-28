#@ MODIF sd_char_ther SD  DATE 28/06/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
from SD.sd_cham_elem import sd_cham_elem
from SD.sd_carte import sd_carte
from SD.sd_champ import sd_champ
from SD.sd_fonction import sd_fonction
from SD.sd_char_cine import sd_char_cine


class sd_char_chth(AsBase):
#--------------------------------
    nomj = SDNom(fin=13)

    CONVE_VALE = Facultatif(AsVK8(SDNom(nomj='.CONVE.VALE'), lonmax=1))
    MODEL_NOMO = AsVK8(SDNom(nomj='.MODEL.NOMO'), lonmax=1)
    LIGRE      = Facultatif(sd_ligrel())

    SOURE = Facultatif(sd_champ(SDNom(nomj='.SOURE')))  # pour l'instant : sd_carte ou sd_cham_elem

    CIMPO = Facultatif(sd_carte())
    CMULT = Facultatif(sd_carte())
    COEFH = Facultatif(sd_carte())
    FLUNL = Facultatif(sd_carte())
    FLUR2 = Facultatif(sd_carte())
    FLURE = Facultatif(sd_carte())
    GRAIN = Facultatif(sd_carte())
    HECHP = Facultatif(sd_carte())
    RAYO  = Facultatif(sd_carte())
    T_EXT = Facultatif(sd_carte())


    # parfois, TEMP_IMPO crée une carte de sd_fonction :
    # il faut alors vérifier ces sd_fonction
    def check_CIMPO_FONC(self, checker):
        if self.CIMPO.VALE.ltyp != 24 : return
        vale=self.CIMPO.VALE.get()
        for x in vale :
            if x.strip()=='' : continue
            nomfon=x[:19]
            sd2=sd_fonction(nomfon) ; sd2.check(checker)



class sd_char_ther(AsBase):
#--------------------------------
    nomj = SDNom(fin=8)
    TYPE = AsVK8(lonmax=1)
    CHTH = sd_char_chth()
    ELIM = Facultatif(sd_char_cine())


