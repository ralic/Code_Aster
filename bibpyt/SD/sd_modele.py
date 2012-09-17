#@ MODIF sd_modele SD  DATE 18/09/2012   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

from SD.sd_ligrel    import sd_ligrel
from SD.sd_maillage  import sd_maillage
from SD.sd_xfem      import sd_modele_xfem
from SD.sd_l_table   import sd_l_table
from SD.sd_partition import sd_partition



class sd_modele(AsBase):
#-----------------------------
    nomj = SDNom(fin=8)

    MODELE = sd_ligrel()
    NOEUD = Facultatif(AsVI())
    MAILLE = Facultatif(AsVI())
    PARTIT = Facultatif(AsVK8(lonmax=1))

    # une sd_modele peut avoir une "sd_l_table" contenant des grandeurs caractéristiques de l'étude :
    lt = Facultatif(sd_l_table(SDNom(nomj='')))

    # Si le modèle vient de MODI_MODELE_XFEM :
    xfem = Facultatif(sd_modele_xfem(SDNom(nomj='')))


    def check_existence(self,checker) :
        exi_liel=self.MODELE.LIEL.exists
        exi_maille=self.MAILLE.exists
        exi_noeud=self.NOEUD.exists

        # si .LIEL => .MAILLE et .NOEUD
        if exi_liel :
            assert exi_maille
            assert exi_noeud


    def check_PARTIT(self,checker) :
        if self.PARTIT.exists :
           partit=self.PARTIT.get_stripped()
           if partit[0] != '' :
              sd2 = sd_partition(partit[0])
              sd2.check(checker)

