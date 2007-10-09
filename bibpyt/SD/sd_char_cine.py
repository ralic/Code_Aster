#@ MODIF sd_char_cine SD  DATE 08/10/2007   AUTEUR PELLET J.PELLET 
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
from SD.sd_modele import sd_modele
from SD.sd_fonction import sd_fonction


class sd_char_cine(AsBase):
#===========================
    nomj = SDNom(fin=19)

    AFCK = AsVK8(lonmax=3)
    AFCI = AsVI()
    AFCV = Facultatif(OJBVect(type=Parmi('C','R','K')))



    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.AFCK.exists


    def u_veri1(self):   # retourne (CIME/CITH/CIAC, RE/CX/FO/FT)
    #---------------------------------------------------------------
        afck=self.AFCK.get()
        l1=afck[0].strip().split('_') ; assert len(l1)==2 , afck
        phen, tsca= l1[0], l1[1]
        assert phen in ('CIME', 'CITH', 'CIAC'), afck
        assert tsca in ('RE', 'CX', 'FO', 'FT'), tsca
        return phen, tsca


    def check_AFCK(self,checker):
    #---------------------------------------------
        phen, tsca = self.u_veri1()
        afck=self.AFCK.get()
        nomo=afck[1].strip()
        sd2=sd_modele(nomo); sd2.check(checker)
        if afck[2].strip() != '' : assert phen=='CIME' and tsca=='FT', afck


    def check_AFCI(self,checker):
    #---------------------------------------------
        phen, tsca = self.u_veri1()
        afci=self.AFCI.get()
        nbloc=afci[0]
        assert len(afci)==3*nbloc+1 , afci
        for k in range(nbloc) :
            nuno =afci[3*k +1]
            nucmp=afci[3*k +2]
            assert afci[3*k +3] == 0 , (k,afci)
            assert nuno > 0 , (k,afci)
            assert nucmp> 0 , (k,afci)


    def check_AFCV(self,checker):
    #-------------------------------------------------
        phen, tsca = self.u_veri1()
        afci=self.AFCI.get()
        nbloc=afci[0]
        if not self.AFCV.exists :
            assert tsca=='FT',tsca
            afck=self.AFCK.get()
            assert afck[2].strip() != '' , afck
        else :
            tsca2 = self.AFCV.type.strip()
            assert self.AFCV.lonmax == nbloc , (nbloc,self.AFCV.lonmax)

            if tsca == 'RE' :
                assert tsca2=='R', tsca2
            if tsca in ('FO', 'FT') :
                assert tsca2=='K' , tsca2  # champ de fonctions
            if tsca=='CX' :
                assert tsca2=='C', tsca2

            # vérification des fonctions :
            if tsca in ( 'FO', 'FT')  :  # 'FO' : fonction ; 'FT' : il existe une fonction de 'INST'
                afcv  = self.AFCV.get()
                for fonc in afcv :
                    sd2=sd_fonction(fonc); sd2.check(checker)

