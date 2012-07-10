#@ MODIF sd_carte SD  DATE 09/07/2012   AUTEUR PELLET J.PELLET 
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
from SD.sd_titre import sd_titre
from SD.sd_util import *

class sd_carte(sd_titre):
    nomj = SDNom(fin=19)

    DESC = AsVI(docu='CART', )
    NOMA = AsVK8(lonmax=1, )
    VALE = AsVect(type=Parmi('C', 'K', 'R', 'I',), ltyp=Parmi(4,8,16,24,), )

    NOLI = Facultatif(AsVK24())
    LIMA = Facultatif(AsColl(acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', ))

    def exists(self):
        return self.NOMA.exists


    def check_NOMA(self,checker):
        if not self.exists() : return
        noma=self.NOMA.get_stripped()

        # faut-il vérifier le sd_maillage de chaque sd_carte ?   AJACOT_PB  (meme question que pour sd_cham_no)
        #  - cela risque de couter cher
        #  - cela pose un problème "import circulaire" avec sd_maillage -> sd_carte => import ici
        # from SD.sd_maillage import sd_maillage
        # sd2=sd_maillage(noma[0]); sd2.check(checker)
        # Rem : si on vérifie le sd_maillage, il me semble que sdll503a se plante (RuntimeError: maximum recursion depth exceeded)

    def check_DESC(self,checker):
        if not self.exists() : return
        desc=self.DESC.get()
        numgd    =desc[0]
        n_gd_max =desc[1]
        n_gd_edit=desc[2]
        assert numgd      > 0 , desc
        assert n_gd_max   > 0 , desc
        assert n_gd_edit  >=0 , desc  # AJACOT_PB : j'aurais préféré  n_gd_edit > 0
                                      # mais acearp.f crée parfois des cartes "vides" (zzzz200a)
        assert n_gd_edit  <= n_gd_max , desc
        for kedit in range(n_gd_edit) :
            code=desc[3+2*kedit]
            assert abs(code) in (1,2,3) , (code, kedit, desc)

    def check_VALE(self,checker):
        if not self.exists() : return
        n1=self.VALE.lonmax
        desc=self.DESC.get()
        n_gd_max =desc[1]
        numgd    =desc[0]
        ncmp_max=len(sdu_licmp_gd(numgd))
        assert n1==ncmp_max*n_gd_max , (n1, ncmp_max, n_gd_max)

