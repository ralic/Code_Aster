#@ MODIF sd_matr_elem SD  DATE 22/10/2007   AUTEUR PELLET J.PELLET 
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
from SD.sd_resuelem import sd_resuelem
from SD.sd_modele import sd_modele
from SD.sd_cham_mater import sd_cham_mater
from SD.sd_cara_elem import sd_cara_elem


class sd_matr_elem(AsBase):
    nomj = SDNom(fin=8)
    REFE_RESU  = AsVK24(lonmax=5, )
    LISTE_RESU = Facultatif(AsVK24())


    # indirection par LISTE_RESU :
    def check_matr_elem_i_LISTE_RESU(self, checker):
        if not self.LISTE_RESU.exists : return
        lnom = self.LISTE_RESU.get_stripped()
        for nom in lnom:
            if nom != '' :
                sd2 = sd_resuelem(nom); sd2.check(checker)


    def check_1(self, checker):
        refe = self.REFE_RESU.get_stripped()
        assert refe[2] in ('OUI_SOUS_STRUC', 'NON_SOUS_STRUC'), refe

        # existence de LISTE_RESU :
        if refe[2] == 'NON_SOUS_STRUC':
            assert self.LISTE_RESU.exists

        assert refe[1] != '', refe

        sd2=sd_modele(refe[0]) ; sd2.check(checker)

        if refe[3] != '' :
            sd2=sd_cham_mater(refe[3]) ; sd2.check(checker)

        if refe[4] != '' :
            sd2=sd_cara_elem(refe[4]) ; sd2.check(checker)



