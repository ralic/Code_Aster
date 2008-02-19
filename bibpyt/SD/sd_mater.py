#@ MODIF sd_mater SD  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
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

from SD.sd_fonction import sd_fonction



class sd_mater_XDEP(AsBase):
#---------------------------
    # on dirait une fonction, mais c'est plutot la concaténation de plusieurs fonctions
    nomj = SDNom(fin=19)
    PROL = AsVK24()
    VALE = AsVR()


class sd_compor1(AsBase):
#-----------------------
    nomj = SDNom(fin=19)
    VALC = AsVC(SDNom(), )
    VALK = AsVK8(SDNom(), )
    VALR = AsVR(SDNom(), )


    # parfois, THER_NL crée une sd_fonction pour BETA
    def check_compor1_i_VALK(self, checker):
        nom= self.nomj().strip()
        if nom[8:16]=='.THER_NL' :
            valk=list(self.VALK.get_stripped())
            if valk :
                nbk2=self.VALK.lonuti
                nbr=self.VALR.lonuti
                nbc=self.VALC.lonuti
                nbk=nbk2-nbr-nbc
                k2=valk.index('BETA')
                k=k2-nbr-nbc
                nomfon=valk[nbr+nbc+nbk/2+k]
                sd2=sd_fonction(nomfon) ; sd2.check(checker)


class sd_mater(AsBase):
#----------------------
    nomj = SDNom(fin=8)
    NOMRC = AsVK16(SDNom(nomj='.MATERIAU.NOMRC'), )
    rdep = Facultatif(sd_mater_XDEP(SDNom(nomj='.&&RDEP')))  # à documenter
    mzp  = Facultatif(sd_mater_XDEP(SDNom(nomj='.&&MZP' )))  # à documenter

    # existence possible de la SD :
    def exists(self):
        return self.NOMRC.exists

    # indirection vers les sd_compor1 de NOMRC :
    def check_mater_i_NOMRC(self, checker):
        lnom = self.NOMRC.get()
        if not lnom: return
        for nom in lnom:
            if not nom.strip(): continue
            nomc1=self.nomj()[:8]+'.'+nom
            comp1 = sd_compor1(nomc1)

            # parfois, comp1 est vide : ssls115g/DEFI_COQU_MULT
            if comp1.VALK.get() : comp1.check(checker)

