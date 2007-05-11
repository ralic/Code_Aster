#@ MODIF sd_cham_mater SD  DATE 09/05/2007   AUTEUR PELLET J.PELLET 
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


class sd_cham_mater(AsBase):
#----------------------------------
    nomj = SDNom(fin=8)
    champ_mat = sd_carte(SDNom(nomj='.CHAMP_MAT', fin=19))

    # La carte TEMPE_REF n'existe pas si AFFE_VARC/NOM_VARC='TEMP' :
    # (voir routine cmtref.f)
    TEMPE_REF = Facultatif(sd_carte())

    CVRCNOM  = Facultatif(AsVK8())
    CVRCGD   = Facultatif(AsVK8())
    CVRCDEF  = Facultatif(AsVR())
    CVRCVARC = Facultatif(AsVK8())
    CVRCCMP  = Facultatif(AsVK8())

    # indirection via CVRCVARC:
    def check_cham_mater_i_CVRCVARC(self, checker):
        if not self.CVRCVARC.get() : return
        lnom=self.CVRCVARC.get()
        for nom in lnom :
           if not nom.strip() : continue
           nom2=self.nomj()[:8]+'.'+nom+'.1'
           sd2=sd_carte(nom2)  ; sd2.check(checker)
           nom2=self.nomj()[:8]+'.'+nom+'.2'
           sd2=sd_carte(nom2)  ; sd2.check(checker)


