#@ MODIF sd_char_contact SD  DATE 12/05/2009   AUTEUR MAZET S.MAZET 
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
from SD.sd_champ import sd_champ
from SD.sd_xfem import sd_modele_xfem, sd_contact_xfem

class sd_char_contact(AsBase):
    nomj      =SDNom(fin=16)

    FORMCO    = Facultatif(AsVI())

    def exists(self):
        # retourne True si la SD semble exister.
        return self.FORMCO.exists


    def formulation_xfem(self):
        if not self.exists() : return False
        iform = self.FORMCO.get()[0]
        return iform == 3

    def contact_xfem_actif(self):
        if not self.formulation_xfem() : return False
        return self.XNBASC.exists

    BAMACO    = Facultatif(AsVI())
    BANOCO    = Facultatif(AsVI())
    CARACF    = Facultatif(AsVR())
    COMAFO    = Facultatif(AsVR())
    JEUSUR    = Facultatif(AsVR())
    CONVCO    = Facultatif(AsVI())
    DIRNOR    = Facultatif(AsVR())
    DIRAPP    = Facultatif(AsVR())
    ECPDON    = Facultatif(AsVI())
    CARFRO    = Facultatif(AsVR())
    FROTE     = Facultatif(AsVR())
    JEUCON    = Facultatif(AsVR())
    JEUCOQ    = Facultatif(AsVR())
    JEUPOU    = Facultatif(AsVR())
    JFO1CO    = Facultatif(AsVK8())
    JFO2CO    = Facultatif(AsVK8())
    JFO3CO    = Facultatif(AsVK8())
    JSUPCO    = Facultatif(AsVR())
    MAESCL    = Facultatif(AsVI())
    MAILCO    = Facultatif(AsVI())
    MAMACO    = Facultatif(AsVI())
    MANOCO    = Facultatif(AsVI())
    METHCO    = Facultatif(AsVI())
    NDIMCO    = Facultatif(AsVI())
    NOESCL    = Facultatif(AsVR())
    NOEUCO    = Facultatif(AsVI())
    NOEUQU    = Facultatif(AsVI())
    NOMACO    = Facultatif(AsVI())
    NORLIS    = Facultatif(AsVI())
    NOZOCO    = Facultatif(AsVI())
    PBAMACO   = Facultatif(AsVI())
    PBANOCO   = Facultatif(AsVI())
    PENAL     = Facultatif(AsVR())
    PMAMACO   = Facultatif(AsVI())
    PMANOCO   = Facultatif(AsVI())
    PNOEUQU   = Facultatif(AsVI())
    PNOMACO   = Facultatif(AsVI())
    PRANOCO   = Facultatif(AsVI())
    PSSNOCO   = Facultatif(AsVI())
    PSANOFR   = Facultatif(AsVI())
    PSUMACO   = Facultatif(AsVI())
    PSUNOCO   = Facultatif(AsVI())
    PZONECO   = Facultatif(AsVI())
    RANOCO    = Facultatif(AsVI())
    SANSNQ    = Facultatif(AsVI())
    SSNOCO    = Facultatif(AsVI())
    SANOFR    = Facultatif(AsVI())
    SYMECO    = Facultatif(AsVI())
    TABFIN    = Facultatif(AsVR())
    TANDEF    = Facultatif(AsVR())
    TANPOU    = Facultatif(AsVR())
    TOLECO    = Facultatif(AsVR())
    xfem      = Facultatif(AsVI())
    XFIMAI    = Facultatif(AsVK8())
    XNBASC    = Facultatif(AsVK24())
    XNRELL    = Facultatif(AsVK24())
    TANINI    = Facultatif(AsVR())
    NORMCO    = Facultatif(AsVR())
    TANGCO    = Facultatif(AsVR())  
    EXCLFR    = Facultatif(AsVR())  
    MODELX    = Facultatif(AsVK8(lonmax=1,))
    CNCTE     = Facultatif(AsVI())

    # si contact xfem :
    xfem      = Facultatif(sd_contact_xfem(SDNom(nomj='')))


    # indirection vers les champs de .XNBASC :
    # Question à Mickael :
    #   la fonction suivante ne serait-elle pas mieux placée dans la classe sd_contact_xfem ?
    def check_char_contact_xfem_XNBASC(self, checker):
        if not self.contact_xfem_actif() : return
        lnom  = self.XNBASC.get()
        nbnom = self.XNBASC.lonuti
        for k in range(nbnom) :
            nom = lnom[k]
            if not nom.strip(): continue
            sd2 = sd_champ(nom)
            sd2.check(checker)


    # indirection vers les champs de .XNRELL :
    # On ne vérifie rien pour l'instant
    # Question à Mickael :
    #   la fonction suivante ne serait-elle pas mieux placée dans la classe sd_contact_xfem ?
    def check_char_contact_xfem_XNRELL(self, checker):
        if not self.contact_xfem_actif() : return
        lnom  = self.XNRELL.get()
        nbnom = self.XNRELL.lonuti
        for k in range(nbnom) :
            nom = lnom[k]
            oo  = AsObject(SDNom(nomj=nom,debut=0),genr='V', xous='S', type=Parmi('I','R'))
            oo.check(checker)


    # Verification MODELE xfem
    def check_char_contact_xfem_MODELX(self, checker):
        if not self.contact_xfem_actif() : return
        nom = self.MODELX.get()[0]
        sd2 = sd_modele_xfem(nom)
        sd2.check(checker)


