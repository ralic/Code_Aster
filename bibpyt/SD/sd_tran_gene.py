#@ MODIF sd_tran_gene SD  DATE 27/11/2007   AUTEUR ANDRIAM H.ANDRIAMBOLOLONA 
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
from SD.sd_nume_ddl_gene import sd_nume_ddl_gene
from SD.sd_matr_asse_gene import sd_matr_asse_gene
from SD.sd_proj_mesu import sd_proj_mesu
from SD.sd_util import *


class sd_tran_gene(sd_titre) :
#--------------------------------------
    nomj = SDNom(fin=19)

    # objets commencant en 19 :
    ACCE = AsVR()  # gros objet
    VITE = AsVR()  # gros objet
    DEPL = AsVR()  # gros objet

    INST = AsVR()  # gros objet
    ORDR = AsVI()  # gros objet
    DESC = AsVI(lonmax=5, )
    REFD = AsVK24(lonmax=6, )
    PTEM = AsVR()

    # si CHOC :
    DLOC = Facultatif(AsVR())
    ICHO = Facultatif(AsVI())
    INTI = Facultatif(AsVK8())
    FCHO = Facultatif(AsVR())
    NCHO = Facultatif(AsVK8())
    SST  = Facultatif(AsVK8())
    VCHO = Facultatif(AsVR())
    VINT = Facultatif(AsVR())

    # si nbexcit > 0 :
    FACC = Facultatif(AsVK8())
    FDEP = Facultatif(AsVK8())
    FVIT = Facultatif(AsVK8())
    IPSD = Facultatif(AsVR())

    # si RELA_EFFO_DEPL :
    REDN = Facultatif(AsVK24(lonmax=1, ))
    REDC = Facultatif(AsVI())
    REDD = Facultatif(AsVR())

    # si utilisation de PROJ_MESU_MODAL :
    PROJM = Facultatif(sd_proj_mesu(SDNom(debut=8)))




    def u_dime(self):
        desc=self.DESC.get()
        nbmode=desc[1] ; assert nbmode >  0
        nbchoc=desc[2] ; assert nbchoc >= 0
        nbrede=desc[3] ; assert nbmode >= 0
        nbsauv=self.ORDR.lonmax  ; assert nbsauv > 0
        if self.FACC.exists :
            nbexcit=self.FACC.lonmax / 2  ; assert nbexcit >= 0
        else :
            nbexcit=0
        return (nbmode, nbchoc, nbsauv, nbexcit, nbrede)


    def check_DESC(self,checker):
        desc=self.DESC.get()
        assert desc[0] in (1,2,3) , desc


    def check_REFD(self,checker):
        # AJACOT : j'avais cru comprendre des choses ... mais sdld104a me prouve le contraire !
        # à revoir ?????
        return
        refd=self.REFD.get_stripped()
        assert refd[0] != '' , refd
        sd2= sd_matr_asse_gene(refd[0]) ; sd2.check()
        assert refd[1] != '' , refd
        sd2= sd_matr_asse_gene(refd[0]) ; sd2.check()
        if refd[2] != '' :
            sd2= sd_matr_asse_gene(refd[2]) ; sd2.check()
        assert refd[3] != '' , refd
        sd2= sd_nume_ddl_gene(refd[3]) ; sd2.check()
        assert refd[4] == '' , refd
        # test de refd[5] trop compliqué : je craque !


    def check_ORDR_INST_PTEM(self,checker):
        nbmode, nbchoc, nbsauv, nbexcit, nbrede = self.u_dime()
        assert self.ORDR.lonmax == nbsauv
        assert self.INST.lonmax == nbsauv
        assert self.PTEM.lonmax in (1, nbsauv)
        # sdu_tous_differents(self.ORDR,checker) # AJACOT_PB : j'aimerais bien "tous_différents"
        # sdu_tous_differents(self.INST,checker) #      mais il y a sdld102a  => fiche à émettre ?


    def check_DEPL_VITE_ACCE(self,checker):
        nbmode, nbchoc, nbsauv, nbexcit, nbrede = self.u_dime()
        assert self.DEPL.lonmax == nbsauv*nbmode
        assert self.VITE.lonmax == nbsauv*nbmode
        assert self.ACCE.lonmax == nbsauv*nbmode


    def check_CHOC(self,checker):
        nbmode, nbchoc, nbsauv, nbexcit, nbrede = self.u_dime()
        if nbchoc == 0 : return
        assert self.DLOC.lonmax == 6*nbsauv*nbchoc
        assert self.VCHO.lonmax == 3*nbsauv*nbchoc
        assert self.FCHO.lonmax == 3*nbsauv*nbchoc
        assert self.INTI.lonmax == nbchoc
        assert self.ICHO.lonmax == nbsauv*nbchoc
        assert self.NCHO.lonmax == 2*nbchoc
        assert self.VINT.lonmax == nbsauv*nbchoc
        assert self.SST.lonmax  == 2*nbchoc


    def check_EXCIT(self,checker):
        nbmode, nbchoc, nbsauv, nbexcit, nbrede = self.u_dime()
        if nbexcit == 0 : return
        assert self.FACC.lonmax == 2*nbexcit
        assert self.FDEP.lonmax == 2*nbexcit
        assert self.FVIT.lonmax == 2*nbexcit
        #assert self.IPSD.lonmax == nbexcit*neq # JP : neq != nbmode. Que vaut neq ??


    def check_RELA_DEPL(self,checker):
        nbmode, nbchoc, nbsauv, nbexcit, nbrede = self.u_dime()
        if nbrede == 0 : return
        assert self.REDC.lonmax == nbsauv*nbrede
        assert self.REDD.lonmax == nbsauv*nbrede
        assert self.REDN.lonmax == nbrede

