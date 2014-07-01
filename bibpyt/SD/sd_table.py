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
        nbcol = self.nb_column()
        if ((i <= 0) or (i > nbcol)) : 
            return [None, None]
        desc = self.TBLP.get()
        data_name = desc[4*(i-1)+2]
        mask_name = desc[4*(i-1)+3]
        return [Colonne_Data(data_name), Colonne_Mask(mask_name)]

    def get_column_name(self, name):
        # retourne la colonne de nom name
        shape = self.TBNP.get()
        desc = self.TBLP.get()
        for n in range(shape[0]):
            nom = desc[4*n]
            data_name = desc[4*n+2]
            mask_name = desc[4*n+3]
            if nom.strip()==name.strip() :
                return [Colonne_Data(data_name), Colonne_Mask(mask_name)]
        return [None, None]

    def check_table_1(self, checker):
        if not self.exists() : return
        shape = self.TBNP.get()
        desc = self.TBLP.get()
        for n in range(shape[0]):
            param_name = desc[4*n].strip()
            data_name = desc[4*n+2]
            mask_name = desc[4*n+3]
            col_d = Colonne_Data(data_name)
            col_m = Colonne_Mask(mask_name)
            col_d.check(checker)
            col_m.check(checker)
            if col_d.data.lonuti != shape[1]:
                checker.err(self,"La taille du vecteur data pour le paramètre '%s' de la "+\
                                 "table est inconsistante %d!=%d" \
                         % (param_name, col_d.data.lonuti, shape[1]))
            if col_m.mask.lonuti != shape[1]:
                checker.err(self,"La taille du vecteur mask pour le paramètre '%s' de la "+\
                                 "table est inconsistante %d!=%d" \
                         % (param_name, col_m.mask.lonuti, shape[1]))


class Colonne_Data(AsBase):
    nomj = SDNom(debut=0, fin=24)
    data = OJBVect(nomj)

class Colonne_Mask(AsBase):
    nomj = SDNom(debut=0, fin=24)
    mask = AsVI(nomj)
