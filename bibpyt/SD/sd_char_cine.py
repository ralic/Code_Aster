#@ MODIF sd_char_cine SD  DATE 19/06/2007   AUTEUR PELLET J.PELLET 
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
    nomj = SDNom(fin=8)

    TYPE = AsVK8(lonmax=1, )

    # UN_PARMI__ : CIAC_MODEL_NOMO  CITH_MODEL_NOMO  CIME_MODEL_NOMO
    CIAC_MODEL_NOMO = Facultatif(AsVK8(SDNom(nomj='.CIAC.MODEL.NOMO'), lonmax=1, ))
    CIME_MODEL_NOMO = Facultatif(AsVK8(SDNom(nomj='.CIME.MODEL.NOMO'), lonmax=1, ))
    CITH_MODEL_NOMO = Facultatif(AsVK8(SDNom(nomj='.CITH.MODEL.NOMO'), lonmax=1, ))

    DEFI = AsVI(SDNom(debut=19), )
    # UN_PARMI__ : VALE  VALF
    VALF = Facultatif(AsVK8(SDNom(debut=19), ))
    # VALE = Facultatif(AsVR(SDNom(debut=19), ))
    VALE = Facultatif(AsObject(SDNom(debut=19),genr='V', type=Parmi('C','R'), xous='S',))


    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.DEFI.exists


    def u_veri1(self):   # retourne (CIME/CITH/CIAC, RE/CX/FO/FT)
    #----------------------------------------------------------
        type=self.TYPE.get()
        l1=type[0].strip().split('_') ; assert len(l1)==2 , type
        phen, tsca= l1[0], l1[1]
        assert phen in ('CIME', 'CITH', 'CIAC'), type
        assert tsca in ('RE', 'CX', 'FO', 'FT'), tsca
        return phen, tsca


    def check_NOMO(self,checker):   # objet .NOMO
    #---------------------------------------------
        phen, tsca = self.u_veri1()
        if phen== 'CIME' :
            nomo=self.CIME_MODEL_NOMO.get()
        elif phen== 'CITH' :
            nomo=self.CITH_MODEL_NOMO.get()
        elif phen== 'CIAC' :
            nomo=self.CIAC_MODEL_NOMO.get()
        sd2=sd_modele(nomo[0]); sd2.check(checker)


    def check_DEFI(self,checker):   # objet .DEFI
    #---------------------------------------------
        phen, tsca = self.u_veri1()
        defi=self.DEFI.get()
        nbloc=defi[0]
        assert len(defi)==3*nbloc+1 , defi
        for k in range(nbloc) :
            nuno =defi[3*k +1]
            nucmp=defi[3*k +2]
            assert defi[3*k +3] in  (1,2) , (k,defi)
            assert nuno > 0 , (k,defi)
            assert nucmp> 0 , (k,defi)


    def check_VALE(self,checker):   # objet .VALE/VALF
    #-------------------------------------------------
        phen, tsca = self.u_veri1()
        defi=self.DEFI.get()
        nbloc=defi[0]
        if tsca in ( 'FO', 'FT') :  # 'FO' : fonction ; 'FT' : il existe une fonction de 'INST'
            vale  = self.VALF.get()
            tsca2 = self.VALF.type.strip()
            for fonc in vale :
                sd2=sd_fonction(fonc); sd2.check(checker)
        elif tsca in ( 'RE', 'CX') :  # 'RE' : réel ; 'CX' : complexe
            vale  = self.VALE.get()
            tsca2 = self.VALE.type.strip()
        assert len(vale)==nbloc , (defi,vale)

        if tsca=='RE' :
            assert tsca2=='R', tsca2
        if tsca in ('FO', 'FT') :
            assert tsca2=='K', tsca2
        if tsca=='CX' :
            assert tsca2=='C', tsca2
