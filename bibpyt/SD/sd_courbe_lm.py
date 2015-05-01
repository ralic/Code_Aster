# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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


class sd_courbe_lm(AsBase):
    nomj = SDNom(fin=8)
    MAIL1 = AsColl(SDNom(debut=8), acces='NU',
                   stockage='CONTIG', modelong='VARIABLE', type='I', )
    MAIL2 = AsColl(SDNom(debut=8), acces='NU',
                   stockage='CONTIG', modelong='VARIABLE', type='I', )
    CHEMIN = AsColl(SDNom(debut=8), acces='NU',
                    stockage='CONTIG', modelong='VARIABLE', type='I', )

    def check_DIM(self, checker):
# verification que les 3 collections ont le meme nbre d'objets
        n1 = self.MAIL1.nmaxoc
        n2 = self.MAIL2.nmaxoc
        assert n1 == n2
        bool = self.CHEMIN.exists
        if bool:
            n = self.CHEMIN.nmaxoc
            assert n == n1
            chemin = self.CHEMIN.get()
            mail1 = self.MAIL1.get()
            mail2 = self.MAIL2.get()
            for i in chemin.keys():
                lon = len(chemin[i])
                lon1 = len(mail1[i])
                lon2 = len(mail2[i])
# verification que les objets des 3 collections ont la meme longueur
                assert lon == lon1
                assert lon == lon2
                deb = chemin[i][0]
                fin = chemin[i][lon - 1]
# verification que le chemin est soit simple (fin = 0) soit cyclique (deb
# = fin)
                assert (deb == fin) or (fin == 0)
# verification que le dernier entier des objets des collections mail1 et
# mail2 est nul
                fin = mail1[i][lon1 - 1]
                assert fin == 0
                fin = mail2[i][lon2 - 1]
                assert fin == 0
