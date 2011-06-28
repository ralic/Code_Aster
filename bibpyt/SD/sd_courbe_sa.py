#@ MODIF sd_courbe_sa SD  DATE 28/06/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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

class sd_courbe_sa(AsBase):
    nomj = SDNom(fin=8)
    MAIL1 = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    MAIL2 = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    XYBSGT = AsVR(SDNom(debut=8), )
    XYASGT = AsVR(SDNom(debut=8), )
    XSARC = AsVR(SDNom(debut=8), )
    XYCARC = AsVR(SDNom(debut=8), )
    XRARC = AsVR(SDNom(debut=8), )
    ORSGT = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='R', ltyp=8, )
    EXSGT = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='R', ltyp=8, )
    PAROR = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='R', ltyp=8, )
    PAREX = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='R', ltyp=8, )
    FACOR = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    FACEX = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    CNXOR = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    CNXEX = AsColl(SDNom(debut=8), acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', )
    def check_1(self,checker):
      if self.XYASGT.exists:
        valea=self.XYASGT.get()
        valeb=self.XYBSGT.get()
        assert len(valea) == len(valeb)
        assert  valea[0] == 0.
        assert  valea[1] == 0.
        assert  valeb[0] == 0.
        assert  valeb[1] == 0.
        valec=self.XYCARC.get()
        valeth=self.XSARC.get()
        valer=self.XRARC.get()
        assert len(valec) == len(valeth)
        assert  valec[0] == 0.
        assert  valec[1] == 0.
        assert  valeth[0] == 0.
        assert  valeth[1] == 0.
        assert  valer[0] == 0.

