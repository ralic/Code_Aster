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
from SD.sd_cara_elem import sd_cara_elem
from SD.sd_modele import sd_modele
from SD.sd_fonction import sd_fonction
from SD.sd_mater import sd_mater
from SD.sd_util import *


class sd_type_flui_stru(AsBase):
#===============================================================
    nomj = SDNom(fin=8)
    FSIC = AsVI(SDNom(debut=19),lonmax=2,)

    FSGM = Facultatif(AsVK24(SDNom(debut=19)))
    FSVR = Facultatif(AsVR(SDNom(debut=19)))
    FSVK = Facultatif(AsVK8(SDNom(debut=19)))
    FSVI = Facultatif(AsVI(SDNom(debut=19)))
    FSCR = Facultatif(AsVR(SDNom(debut=19)))
    FSGR = Facultatif(AsVR(SDNom(debut=19)))
    UNIT_FAISCEAU = Facultatif(AsVI(lonmax=2,))
    UNIT_GRAPPES  = Facultatif(AsVI(lonmax=2,))


    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre vérifiée)
        return self.FSIC.exists


    def u_veri1(self):
    #-----------------------
        # retourne 2 variables utiles pour les routines chech_xxxx
        #   type_faisceau   : type de faisceau (configuration)
        #   couplage : indicateur de couplage
        fsic=self.FSIC.get()
        type_faisceau=fsic[0]
        couplage=fsic[1]
        return type_faisceau, couplage



    def check_FSIC(self,checker):   # objet .FSIC
    #===============================================================
        fsic=self.FSIC.get()
        type_faisceau, couplage= self.u_veri1()
        assert type_faisceau in (1,2,3,4), (type_faisceau, fsic)
        assert couplage in (0,1), (couplage,fsic)



    def check_FSVI(self,checker):   # objet .FSVI
    #===============================================================
        fsvi=self.FSVI.get()
        type_faisceau, couplage= self.u_veri1()

        if type_faisceau == 1 :  # FAISCEAU_TRANS
        #-----------------------------------------
            nbzone=fsvi[1]
            sdu_compare(self.FSVI,checker,nbzone,'>',0,"nbzone > 0")
            if couplage :
                assert len(fsvi)==2+2*nbzone, fsvi
                assert fsvi[0] in (1,2), fsvi
                for indic in fsvi[2:2+nbzone] :
                    assert indic > 0 , (nbzone,fsvi,self)
                for nbpt in fsvi[2+nbzone:2+2*nbzone] :
                    assert nbpt >= 0 , (nbzone,fsvi,self)  # =0 possible : sdll136a
            else :
                assert len(fsvi)==2

        if type_faisceau == 2 : # GRAPPE
        #-----------------------------------------
            sdu_assert(self.FSVI, checker, not fsvi, "type_faisceau == GRAPPE => FSVI ne doit pas exister")

        if type_faisceau == 3 :  # FAISCEAU_AXIAL
        #-----------------------------------------
            lsimplif=fsvi[0]
            # lsimplif=1 : faisceau simplifié
            # lsimplif=0 : faisceau complet

            nbtype=fsvi[4] # nombre de types de grille
            lgrille=nbtype > 0
            # lgrille=1 : on utilise des grilles
            # lgrille=0 : on n'utilise pas de grille

            assert fsvi[0] in (0,1), fsvi
            assert fsvi[1] in (1,2,3), fsvi
            assert fsvi[2] in (1,2), fsvi

            if not lsimplif :     # faisceau complet
                if lgrille :
                    assert len(fsvi)==6+nbtype, fsvi
                    assert min(fsvi[3:]) > 0, fsvi
                else:
                    assert len(fsvi)==5, fsvi
                    assert fsvi[3]  >= 0, fsvi

            else :                # faisceau simplifié
                nbzone=fsvi[3]
                if lgrille :
                    assert len(fsvi)==6+nbtype+nbzone, fsvi
                    assert min(fsvi[3:]) > 0, fsvi
                else:
                    assert len(fsvi)==6+nbzone, fsvi
                    assert fsvi[3]  > 0, fsvi

        if type_faisceau == 4 :  # COQUE_COAX
        #-----------------------------------------
            assert len(fsvi)==2, fsvi
            assert fsvi[0] in (0,1), fsvi
            assert fsvi[1] in (1,2,3), fsvi



    def check_FSVK(self,checker):   # objet .FSVK
    #===============================================================
        fsvk=self.FSVK.get()
        fsvi=self.FSVI.get()
        type_faisceau, couplage= self.u_veri1()

        if type_faisceau == 1 :  # FAISCEAU_TRANS
        #-----------------------------------------
            nbzone=fsvi[1]
            assert len(fsvk)==4+nbzone, fsvk
            carel=fsvk[0].strip()
            assert carel != '', fsvk
            sd2=sd_cara_elem(carel); sd2.check(checker)
            assert fsvk[1].strip() in ('DX', 'DY', 'DZ'), fsvk
            for k in range(2,4+nbzone) :
               sd2=sd_fonction(fsvk[k]); sd2.check(checker)

        elif type_faisceau == 2 :  # GRAPPE
        #-----------------------------------------
            if couplage :
                assert len(fsvk)==4,(fsvk,self)
                assert fsvk[0] != '',(fsvk,self)
                # on pourrait vérifier que fsvk[1] est un nom de noeud
                assert fsvk[1] != '',(fsvk,self)
                sd2=sd_cara_elem(fsvk[2].strip()); sd2.check(checker)
                sd2=sd_modele(fsvk[3].strip()); sd2.check(checker)
            else:
                assert not fsvk,(fsvk,self)

        elif type_faisceau == 3 :  # FAISCEAU_AXIAL
        #-----------------------------------------
            lsimplif=fsvi[0]
            if not lsimplif :
                assert len(fsvk)==3,(fsvk,self)
                sd2=sd_fonction(fsvk[0].strip()); sd2.check(checker)
                sd2=sd_fonction(fsvk[1].strip()); sd2.check(checker)
                sd2=sd_cara_elem(fsvk[2].strip()); sd2.check(checker)
            else:
                assert len(fsvk)==2,(fsvk,self)
                sd2=sd_fonction(fsvk[0].strip()); sd2.check(checker)
                sd2=sd_fonction(fsvk[1].strip()); sd2.check(checker)

        elif type_faisceau == 4 :  # COQUE_COAX
        #-----------------------------------------
            assert len(fsvk)==3,(fsvk,self)
            sd2=sd_cara_elem(fsvk[0].strip()); sd2.check(checker)
            sd2=sd_mater(fsvk[1].strip()); sd2.check(checker)
            sd2=sd_mater(fsvk[2].strip()); sd2.check(checker)



    def check_FSVR(self,checker):   # objet .FSVR
    #===============================================================
        fsvr=self.FSVR.get()
        fsvi=self.FSVI.get()
        type_faisceau, couplage= self.u_veri1()

        if type_faisceau == 1 :  # FAISCEAU_TRANS
        #-----------------------------------------
            nbzone=fsvi[1]
            if couplage :
                assert len(fsvr)==3+2*nbzone,(fsvr,self)
            else :
                assert len(fsvr)==1,(fsvr,self)
            sdu_tous_compris(self.FSVR,checker,vmin=0.,comment="FSVR > 0")

        elif type_faisceau == 2 :  # GRAPPE
        #-----------------------------------------
            if couplage :
                assert len(fsvr)==2,(fsvr,self)
                sdu_tous_compris(self.FSVR,checker,vmin=0.,comment="FSVR > 0")
            else :
                assert not fsvr,(fsvr,self)

        elif type_faisceau == 3 :  # FAISCEAU_AXIAL
        #-----------------------------------------
            lsimplif=fsvi[0]
            if not lsimplif :
                if fsvi[2]==1 : # enceinte circulaire
                    assert len(fsvr)==8,(fsvr,self)
                else:           # enceinte rectangulaire
                    assert len(fsvr)==10,(fsvr,self)
            else :
                nbzone=fsvi[3]
                if fsvi[2]==1 : # enceinte circulaire
                    assert len(fsvr)==8+nbzone,(fsvr,self)
                    sdu_tous_compris(self.FSVR,checker,fsvi[8:],vmin=0.,comment="FSVR > 0")
                else:           # enceinte rectangulaire
                    assert len(fsvr)==10+nbzone,(fsvr,self)
                    sdu_tous_compris(self.FSVR,checker,fsvi[10:],vmin=0.,comment="FSVR > 0")

        elif type_faisceau == 4 :  # COQUE_COAX
        #-----------------------------------------
            assert len(fsvr)==7,(fsvr,self)
            sdu_tous_compris(self.FSVR,checker,vmin=0.,comment="FSVR > 0")



    def check_FSGM(self,checker):   # objet .FSGM
    #===============================================================
        fsgm=self.FSGM.get()
        fsvi=self.FSVI.get()
        type_faisceau, couplage= self.u_veri1()

        if type_faisceau in (1,2) :
        #-----------------------------------------
            assert not fsgm,(fsvi,self)

        elif type_faisceau == 3 :  # FAISCEAU_AXIAL
        #-----------------------------------------
            nb1=fsvi[3]  # nbgrma ou nbzone
            if nb1==0 :
                assert len(fsgm)==1,(fsgm,self)
            else :
                assert len(fsgm)==nb1,(fsgm,self)
                sdu_tous_differents(self.FSGM,checker,comment='FAISCEAU_AXIAL')

        elif type_faisceau == 4 :  # COQUE_COAX
        #-----------------------------------------
            assert len(fsgm)==2,(fsgm,self)
            sdu_tous_differents(self.FSGM,checker,comment='COQUE_COAX')



    def check_FSGR(self,checker):   # objet .FSGR
    #===============================================================
        fsgr=self.FSGR.get()
        fsvi=self.FSVI.get()
        type_faisceau, couplage= self.u_veri1()

        if type_faisceau in (1,2,4) :
        #-----------------------------------------
            assert not fsgr,(fsvi,self)

        elif type_faisceau ==3 :
        #-----------------------------------------
            nbzone=fsvi[3]
            nbtype=fsvi[4] # nombre de types de grille
            if nbtype==0 :
                assert not fsgr,(fsvi,self)
            else :   # il y a des grilles
                lsimplif=fsvi[0]
                if lsimplif :
                    nbgrille=fsvi[6+nbzone]
                else :
                    nbgrille=fsvi[5]
                assert len(fsgr)==nbgrille+6*nbtype,(fsgr,self)



    def check_FSCR(self,checker):   # objet .FSCR
    #===============================================
        fscr=self.FSCR.get()
        fsvi=self.FSVI.get()
        type_faisceau, couplage= self.u_veri1()

        if type_faisceau in (1,2,4) :
        #-----------------------------------------
            assert not fscr,(fsgm,self)

        elif type_faisceau ==3 :
        #-----------------------------------------
            lsimplif=fsvi[0]
            if lsimplif :
                nbtube=fsvi[5]
                assert len(fscr)==2*nbtube,(fscr,self)
            else :
                assert not fscr,(fsvi,self)



    def check_UNIT_FAISCEAU(self,checker):   # objet .UNIT.FAISCEAU
    #===============================================================
        unite=self.UNIT_FAISCEAU.get()
        fsvi=self.FSVI.get()
        type_faisceau, couplage= self.u_veri1()

        if type_faisceau in (2,3,4) :
        #-----------------------------------------
            assert not unite,(fsvi,self)

        elif type_faisceau == 1 :
        #-----------------------------------------
            assert len(unite)==2,(unite,self)
            assert unite[0] > 0 ,(unite,self)
            assert unite[1] > 0 ,(unite,self)



    def check_UNIT_GRAPPES(self,checker):   # objet .UNIT.GRAPPES
    #===============================================================
        unite=self.UNIT_GRAPPES.get()
        fsvi=self.FSVI.get()
        type_faisceau, couplage= self.u_veri1()

        if type_faisceau in (1,3,4) :
        #-----------------------------------------
            assert not unite,(fsvi,self)

        elif type_faisceau == 2 :
        #-----------------------------------------
            if couplage :
                assert len(unite)==2,(unite,self)
                assert unite[0] > 0 ,(unite,self)
                assert unite[1] > 0 ,(unite,self)
            else :
                assert not unite, (fsic,self)
