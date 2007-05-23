#@ MODIF sd_cham_mater SD  DATE 23/05/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_carte import sd_carte
from SD.sd_util import *


class sd_cham_mater_varc(AsBase):
#----------------------------------
    nomj = SDNom(fin=8)
    CVRCNOM  = AsVK8()
    CVRCGD   = AsVK8()
    CVRCDEF  = AsVR()
    CVRCVARC = AsVK8()
    CVRCCMP  = AsVK8()

    def exists(self):
        return self.CVRCVARC.get()

    # indirection via CVRCVARC:
    def check_cham_mater_i_CVRCVARC(self, checker):
        if not self.exists() : return
        lnom=self.CVRCVARC.get()
        for nom in lnom :
           nom2=self.nomj()[:8]+'.'+nom+'.1'
           sd2=sd_carte(nom2)  ; sd2.check(checker)
           nom2=self.nomj()[:8]+'.'+nom+'.2'
           sd2=sd_carte(nom2)  ; sd2.check(checker)

    # vérification des objets .CVRC* :
    def check_CVRC(self, checker):
        if not self.exists() : return
        xcmp=self.CVRCCMP.get()
        xnom=self.CVRCNOM.get()
        xgd=self.CVRCGD.get()
        xvarc=self.CVRCVARC.get()
        xdef=self.CVRCDEF.get()

        # Les 5 objets ont la meme longueur > 0 :
        nbcvrc=len(xnom)
        assert nbcvrc > 0, (self)
        assert len(xcmp)  == nbcvrc , (xcmp,xnom,self)
        assert len(xgd)   == nbcvrc , (xgd,xnom,self)
        assert len(xvarc) == nbcvrc , (xvarc,xnom,self)
        assert len(xdef)  == nbcvrc , (xdef,xnom,self)

        # Les 4 objets sont "non blancs" :
        sdu_tous_non_blancs(self.CVRCCMP,checker)
        sdu_tous_non_blancs(self.CVRCNOM,checker)
        sdu_tous_non_blancs(self.CVRCGD,checker)
        sdu_tous_non_blancs(self.CVRCVARC,checker)

        # les noms des CRVC doievent etre differents:
        sdu_tous_differents(self.CVRCNOM,checker)


class sd_cham_mater(AsBase):
#----------------------------------
    nomj = SDNom(fin=8)
    champ_mat = sd_carte(SDNom(nomj='.CHAMP_MAT', fin=19))

    # La carte TEMPE_REF n'existe pas si AFFE_VARC/NOM_VARC='TEMP' :
    # (voir routine cmtref.f)
    TEMPE_REF = Facultatif(sd_carte())

    # si AFFE_VARC :
    varc = Facultatif(sd_cham_mater_varc(SDNom(nomj='')))



