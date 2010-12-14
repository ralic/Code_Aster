#@ MODIF sd_compor SD  DATE 14/12/2010   AUTEUR PROIX J-M.PROIX 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
from SD.sd_mater import sd_mater

# Remarque :
#------------
# la sd_compor est produite par la seule commande DEFI_COMPOR.
# C'est pourquoi, on fera appel parfois pour la décrire au vocabulaire de cette
# commande.

class sd_compor(AsBase):
    nomj = SDNom(fin=8)
    CPRK = AsVK16()
    CPRI = AsVI()
    CPRR = Facultatif(AsVR())


    def check_tout(self,checker) :
    #-------------------------------
        cpri=self.CPRI.get()
        type=cpri[0]
        assert type in (1,2,3) , CPRI
        if type == 1 :
            self.monocristal(cpri,checker)
        if type == 2 :
            self.polycristal(cpri,checker)
        if type == 3 :
            self.multifibres(cpri,checker)



    def monocristal(self,cpri,checker) :
    #------------------------------------
        nboccm=cpri[4]
        nvi   =cpri[2]
        if cpri[5] > 0 : 
            nbsys=(nvi-25)/3
            assert nvi==25+3*nbsys , (nvi, nbsys, cpri)
        else :
            nbsys=(nvi-9)/3
            assert nvi==9+3*nbsys , (nvi, nbsys, cpri)
        cprk=self.CPRK.get_stripped()

        # vérif existence et longueur
        assert len(cpri)==7, cpri
        assert len(cprk)==5*nboccm+1, (cpri,cprk)
        assert not self.CPRR.get()

        # vérif CPRI :
        #-------------
        assert cpri[1] == 1   ,cpri
        assert cpri[2] == nvi ,cpri
        assert cpri[3] == 1   ,cpri
        assert cpri[4] > 0    ,cpri
        assert cpri[5] >= 0   ,cpri
        assert cpri[6] == nvi ,cpri

        # vérif CPRK :
        #-------------
        elas=cprk[5*nboccm]
        assert elas in ('ELAS', 'ELAS_ORTH')  ,cprk
        for k in range(nboccm):
            famil     =cprk[5*k+0]
            mater     =cprk[5*k+1]
            ecoul     =cprk[5*k+2]
            MONO_isot =cprk[5*k+3]
            MONO_cine =cprk[5*k+4]
            sd2=sd_mater(mater) ; sd2.check(checker)
            assert famil in ('BASAL','BCC24','PRISMATIQUE','OCTAEDRIQUE','PYRAMIDAL1',
                            'PYRAMIDAL2','CUBIQUE1','CUBIQUE2','MACLAGE','UNIAXIAL')
            assert ecoul in ('MONO_VISC1','MONO_VISC2','MONO_VISC3')
            assert MONO_isot in ('MONO_ISOT1','MONO_ISOT2')
            assert MONO_cine in ('MONO_CINE1','MONO_CINE2')



    def polycristal(self,cpri,checker) :
    #------------------------------------
        nbphases=cpri[1]
        assert nbphases > 0 , cpri
        lgcprk  =cpri[6+3*nbphases-2]
        assert lgcprk > 0 , cpri
        cprk=self.CPRK.get_stripped()
        cprr=self.CPRR.get()

        # vérif existence et longueur
        #------------------------------
        assert len(cpri)==6+3*nbphases, (cpri,nbphases)
        assert len(cprr)==2+4*nbphases, (cpri,cprr,nbphases)
        assert len(cprk)==lgcprk, (cpri,cprk)

        # vérif CPRI :
        #-------------
        nvitot=cpri[2]
        assert nvitot >= 0        ,cpri
        nbmono=cpri[3]
        assert nbmono > 0         ,cpri
        nbpara  =cpri[6+3*nbphases-1]
        assert nbpara in (0,1,2)  ,cpri
        for k in range(nbphases):
            nbfam1 = cpri[4+3*k+0]
            numono = cpri[4+3*k+1]
            nvi1   = cpri[4+3*k+2]
            assert nbfam1 > 0     ,cpri
            assert numono > 0  and numono <= nbmono   ,(cpri,nbmono)
            assert nvi1   >=0     ,cpri

        # vérif CPRR :
        #-------------
        frac_tot=0.
        for k in range(nbphases):
            frac     =cprr[4*k+0]
            assert frac >= 0. and frac <= 1.  ,(cprr,k)
            frac_tot=frac_tot+frac
            for dir in range(1,4):
                angl     =cprr[4*k+dir]
                assert angl >=0. and angl <=360. , (angl,dir)
        assert frac_tot > 0.99 and frac_tot < 1.01

        # vérif CPRK :
        #-------------
        locali   =cprk[0]
        assert locali in ('BZ','BETA')  ,(locali,cprk)
        decal=0
        for k in range(nbmono):
            mono1 =cprk[0+decal+1]
            sd2=sd_compor(mono1) ; sd2.check(checker)
            nbfam1=int(cprk[0+decal+2])
            assert nbfam1 > 0 , (nbfam1,k,decal,cprk)
            decal=decal+2+5*nbfam1+1
            # on pourrait encore vérifier que le .CPRK de mono1 a bien été recopié
            # mais il faut bien s'arreter ...



    def multifibres(self,cpri,checker) :
    #------------------------------------
        nbgmax=cpri[2]
        cprk=self.CPRK.get_stripped()

        # vérif existence et longueur
        assert len(cpri)==3, cpri
        assert len(cprk)==6*nbgmax+1, (cpri,cprk)
        assert not self.CPRR.get()

        # vérif CPRI :
        #-------------
        assert cpri[1] > 0, cpri
        assert cpri[2] > 0, cpri

        # vérif CPRK :
        #-------------
        mater=cprk[6*nbgmax]
        assert mater != '', cprk
        sd2=sd_mater(mater) ; sd2.check(checker)
        for k in range(nbgmax):
            grfib1     =cprk[6*k+0]
            mater1     =cprk[6*k+1]
            loifib1    =cprk[6*k+2]
            algo1d     =cprk[6*k+3]
            deform     =cprk[6*k+4]
            nbfib=    int(cprk[6*k+5])
            assert grfib1 != '' , cprk
            assert mater1 != '' , cprk
            sd2=sd_mater(mater1) ; sd2.check(checker)
            assert loifib1 != '' , cprk
            assert algo1d  in ('ANALYTIQUE','DEBORST') , cprk
            assert deform  in ('PETIT','PETIT_REAC','GROT_GDEP') , cprk
            assert nbfib > 0      , cprk


