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

from SD.sd_table import sd_table, Colonne
from SD.sd_fonction import sd_fonction

# --------------------------------------------------------------------
# sd_table dont une colonne nommée "FONCTION[_C]" contient des fonctions
# --------------------------------------------------------------------



class sd_table_fonction(sd_table):
#-------------------------------------
    nomj = SDNom(fin=17)


    def check_table_fonction_i_COL_FONC(self, checker):
        shape = self.TBNP.get()
        if shape is None: return
        desc = self.TBLP.get()
        for n in range(shape[0]):
            nomcol=desc[4*n].strip()
            if not (nomcol == 'FONCTION' or nomcol == 'FONCTION_C') : continue
            nom = desc[4*n+2]
            col = Colonne(nom)
            lnom = col.data.get()
            if not lnom : return
            for nom1 in lnom :
                if not nom1.strip() : continue
                sd2=sd_fonction(nom1) ; sd2.check(checker)
