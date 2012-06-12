#@ MODIF sd_cham_no SD  DATE 11/06/2012   AUTEUR PELLET J.PELLET 
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
from SD.sd_util import *
from SD.sd_titre import sd_titre
from SD.sd_prof_chno import sd_prof_chno


class sd_cham_no(sd_titre):
    nomj = SDNom(fin=19)
    VALE = AsVect(ltyp=Parmi(4,8,16,24), type=Parmi('C', 'I', 'K', 'R'), docu=Parmi('', '2', '3'), )
    REFE = AsVK24(lonmax=4)
    DESC = AsVI(docu='CHNO', )

    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.REFE.exists

    def u_desc(self):
        desc=self.DESC.get()
        gd  = desc[0]
        num = desc[1]
        return gd,num

    def u_refe(self):
        refe=self.REFE.get_stripped()
        mail      = refe[0]
        prof_chno = refe[1]
        if refe[2] == 'FETI' :
           assert refe[3] != '', refe
        else :
           assert refe[2] == '', refe
           assert refe[3] == '', refe
        return mail,prof_chno

    def check_cham_no_i_REFE(self, checker):
        if not self.exists() : return
        if checker.names.has_key(self.REFE): return

        mail, prof_chno = self.u_refe()

        # faut-il vérifier le sd_maillage de chaque sd_cham_no ?   AJACOT_PB
        #  - cela risque de couter cher
        #  - cela pose un problème "import circulaire" avec sd_maillage -> sd_cham_no => import ici
        from SD.sd_maillage import sd_maillage
        sd2 = sd_maillage(mail)
        sd2.check(checker)

        if prof_chno :
            if checker.names.has_key(prof_chno[:14]+'.NUME.PRNO'):  return
            sd2 = sd_prof_chno(prof_chno)
            sd2.check(checker)

    def check_cham_no_DESC(self, checker):
        if not self.exists(): return
        if checker.names.has_key(self.DESC): return

        gd, num = self.u_desc()
        if (num < 0):
           nb_ec = sdu_nb_ec(gd)
           assert self.DESC.lonmax == 2 + nb_ec
        else:
           assert self.DESC.lonmax == 2

