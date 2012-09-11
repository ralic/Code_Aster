#@ MODIF sd_cham_gene SD  DATE 11/09/2012   AUTEUR BERRO H.BERRO 
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
from SD.sd_nume_ddl_gene import sd_nume_ddl_gene

class sd_cham_gene(AsBase):
    nomj = SDNom(fin=19)
    REFE = AsVK24(lonmax=2, )
    VALE = AsObject(genr='V', xous='S', type=Parmi('C', 'R'), ltyp=Parmi(16, 8), )
    DESC = AsVI(docu='VGEN', )


    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.REFE.exists

    # indirection vers NUME_DDL_GENE:
    def check_REFE(self, checker):
        if not self.exists() : return
        refe = self.REFE.get_stripped()
        # ce test fait planter les verif de SD issues de DYNA_TRAN_MODAL + RECU_GENE
        # op0037 cree un refe[1]='$TRAN_GENE' bidon
        #if refe[1] in  ('$TRAN_GENE','$HARM_GENE') : return
        if refe[1] :
            sd2 = sd_nume_ddl_gene(refe[1]) ; sd2.check(checker)


