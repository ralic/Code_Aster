#@ MODIF sd_resultat_dyn SD  DATE 15/01/2008   AUTEUR PELLET J.PELLET 
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

from SD.sd_resultat import sd_resultat
from SD.sd_nume_ddl import sd_nume_ddl
from SD.sd_matr_asse import sd_matr_asse
from SD.sd_interf_dyna_clas import sd_interf_dyna_clas
from SD.sd_proj_mesu import sd_proj_mesu


class sd_resultat_dyn(sd_resultat):
#--------------------------------------------------
    nomj = SDNom(fin=8)
    PROFC_NUME_REFN = Facultatif(AsVK24(SDNom(nomj='.PROFC.NUME.REFN'), lonmax=2, ))
    REFD = Facultatif(AsVK24(SDNom(debut=19), lonmax=6, )) # n'existe pas dans sdll23a

    # si utilisation de PROJ_MESU_MODAL :
    PROJM = Facultatif(sd_proj_mesu())


    # indirection vers les SD de .REFD :
    def check_resultat_dyn_i_REFD(self, checker):
        refd = self.REFD.get_stripped()
        if not refd : return
        for k in 0,1,2 :
            if refd[k] :
                sd2 = sd_matr_asse(refd[0]); sd2.check(checker)
        if refd[3] :
            sd2 = sd_nume_ddl(refd[3]); sd2.check(checker)
        if refd[4] :
            sd2 = sd_interf_dyna_clas(refd[4]); sd2.check(checker)

