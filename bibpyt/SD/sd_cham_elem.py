#@ MODIF sd_cham_elem SD  DATE 18/03/2008   AUTEUR PELLET J.PELLET 
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
from SD.sd_titre import sd_titre

from SD.sd_ligrel import sd_ligrel


class sd_cham_elem(sd_titre):
#-------------------------------------
    nomj = SDNom(fin=19)
    CELD = AsVI(docu='CHML', )
    CELV = AsObject(genr='V', xous='S', type=Parmi('C', 'I', 'K', 'R'), ltyp=Parmi(4,8,16), )
    CELK = AsVK24(lonmax=7, )


    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.CELK.exists

    def check_1(self, checker):
        if not self.exists() : return
        celk=self.CELK.get_stripped()
        sd2=sd_ligrel(celk[0]); sd2.check(checker)
        assert celk[1] != '' , celk
        assert celk[2] in ('ELNO','ELGA','ELEM') , celk
        assert celk[4] in ('','INF','MOY','SUP') , celk
        assert celk[5] != '' , celk
        assert celk[6] in ('MPI_COMPLET','MPI_INCOMPLET') , celk


