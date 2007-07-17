#@ MODIF sd_char_contact SD  DATE 17/07/2007   AUTEUR PELLET J.PELLET 
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

class sd_char_contact(AsBase):
    nomj      =SDNom(fin=16)

    BAMACO    = Facultatif(AsVI())
    BANOCO    = Facultatif(AsVI())
    CARACF    = Facultatif(AsVR())
    CARFRO    = Facultatif(AsVR())
    COMAFO    = Facultatif(AsVR())
    CONVCO    = Facultatif(AsVI())
    DIRCO     = Facultatif(AsVR())
    ECPDON    = Facultatif(AsVI())
    FORMCO    = Facultatif(AsVI())
    FROTE     = Facultatif(AsVR())
    JEUCON    = Facultatif(AsVR())
    JEUCOQ    = Facultatif(AsVR())
    JEUPOU    = Facultatif(AsVR())
    JEUSUR    = Facultatif(AsVR())
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
    PSUMACO   = Facultatif(AsVI())
    PSUNOCO   = Facultatif(AsVI())
    PZONECO   = Facultatif(AsVI())
    RANOCO    = Facultatif(AsVI())
    SANSNQ    = Facultatif(AsVI())
    SSNOCO    = Facultatif(AsVI())
    SYMECO    = Facultatif(AsVI())
    TABFIN    = Facultatif(AsVR())
    TANDEF    = Facultatif(AsVR())
    TANPOU    = Facultatif(AsVR())
    TOLECO    = Facultatif(AsVR())
    XFEM      = Facultatif(AsVI())
    XFIMAI    = Facultatif(AsVK8())
