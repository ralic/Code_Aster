# coding=utf-8
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
from SD.sd_fonction import sd_fonction


class sd_carte(sd_titre):
    nomj = SDNom(fin=19)

    DESC = AsVI(docu='CART', )
    NOMA = AsVK8(lonmax=1, )
    VALE = AsVect(type=Parmi('C', 'K', 'R', 'I',), ltyp=Parmi(4, 8, 16, 24,), )

    NOLI = Facultatif(AsVK24())
    LIMA = Facultatif(
        AsColl(acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', ))

    def exists(self):
        return self.NOMA.exists

    def check_NOMA(self, checker):
        if not self.exists():
            return
        noma = self.NOMA.get_stripped()

        from SD.sd_maillage import sd_maillage
        sd2 = sd_maillage(noma[0])
        # Rem : si on vérifie le sd_maillage, sdll503a se plante (RuntimeError: maximum recursion depth exceeded)
        # sd2.check(checker)
        # => On se contente de vérifier existence d'un objet particulier de la sd_maillage :
        assert sd2.DIME.exists

    def check_DESC(self, checker):
        if not self.exists():
            return
        desc = self.DESC.get()
        numgd = desc[0]
        n_gd_max = desc[1]
        n_gd_edit = desc[2]
        assert numgd > 0, desc
        assert n_gd_max > 0, desc
        assert n_gd_edit > 0, desc

        assert n_gd_edit <= n_gd_max, desc
        exi_lima = 0
        for kedit in range(n_gd_edit):
            code = desc[3 + 2 * kedit]
            assert abs(code) in (1, 2, 3), (code, kedit, desc)
            if abs(code) == 3:
                exi_lima = 1
        if exi_lima:
            assert self.LIMA.exists

    def check_VALE(self, checker):
        if not self.exists():
            return
        n1 = self.VALE.lonmax
        desc = self.DESC.get()
        n_gd_max = desc[1]
        numgd = desc[0]
        ncmp_max = len(sdu_licmp_gd(numgd))
        assert n1 == ncmp_max * n_gd_max, (n1, ncmp_max, n_gd_max)

        # parfois, la carte contient des fonctions crees pendant la commande :
        if self.VALE.type == 'K':
            nom_concept = self.nomj()[:8]
            vale = self.VALE.get()
            for nom in vale:
                if len(nom.strip()) == 0:
                    continue
                nom8 = nom[:8]
                if nom8 == nom_concept:
                    sd2 = sd_fonction(nom)
                    if sd2.PROL.exists:
                        sd2.check(checker)
