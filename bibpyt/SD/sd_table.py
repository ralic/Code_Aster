# coding=utf-8
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
from SD.sd_titre import sd_titre


class sd_table(sd_titre):
#-------------------------------------
    nomj = SDNom(fin=17)
    TBNP = AsVI(SDNom(debut=19), lonmax=2, )
    TBBA = AsVK8(SDNom(debut=19), lonmax=1, )
    TBLP = AsVK24(SDNom(debut=19), )


    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.TBNP.exists

    def nb_column(self):
        # retourne le nombre de colonnes de la table :
        shape = self.TBNP.get()
        return shape[0]

    def get_column(self, i):
        # retourne la colonne de numéro i
        nom = self.nomj()[:19]+".%04d"%i
        return Colonne( nom )

    def get_column_name(self, name):
        # retourne la colonne de nom name
        shape = self.TBNP.get()
        desc = self.TBLP.get()
        for n in range(shape[0]):
            nom = desc[4*n]
            nom2= desc[4*n+2]
            if nom.strip()==name.strip() :
                return Colonne(nom2)
        return None

    def check_table_1(self, checker):
        if not self.exists() : return
        shape = self.TBNP.get()
        desc = self.TBLP.get()
        for n in range(shape[0]):
            nom = desc[4*n+2]
            col = Colonne(nom)
            col.check(checker)
            data = col.data.get()
            if data is not None:
                if col.data.lonuti != shape[1]:
                    checker.err(self,"Taille inconsitante %d!=%d" %
                                (col.data.lonuti,shape[1]))

class Colonne(AsBase):
    nomj = SDNom(debut=0, fin=24)
    data = OJBVect(SDNom("  ",debut=17,fin=19))
    mask = OJBVect(SDNom("LG",debut=17,fin=19))
