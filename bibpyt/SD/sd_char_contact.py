#@ MODIF sd_char_contact SD  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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

    BAMACO    =AsVI()
    BANOCO    =AsVI()
    CARACF    =AsVR()
    COMAFO    =AsVR()
    CONVCO    =AsVI()
    DIRCO     =AsVR()
    ECPDON    =AsVI()
    FROTE     =AsVR()
    JEUCON    =Facultatif(AsVR())
    JEUCOQ    =AsVR()
    JEUPOU    =AsVR()
    JFO1CO    =AsVK8()
    JFO2CO    =AsVK8()
    JFO3CO    =AsVK8()
    JSUPCO    =AsVR()
    MAESCL    =Facultatif(AsVI())
    MAILCO    =AsVI()
    MAMACO    =AsVI()
    MANOCO    =AsVI()
    METHCO    =AsVI()
    NDIMCO    =AsVI()
    NOESCL    =Facultatif(AsVR())
    NOEUCO    =AsVI()
    NOEUQU    =Facultatif(AsVI())
    NOMACO    =AsVI()
    NORLIS    =AsVI()
    NOZOCO    =AsVI()
    PBAMACO   =AsVI()
    PBANOCO   =AsVI()
    PENAL     =AsVR()
    PMAMACO   =AsVI()
    PMANOCO   =AsVI()
    PNOEUQU   =AsVI()
    PNOMACO   =AsVI()
    PRANOCO   =AsVI()
    PSSNOCO   =AsVI()
    PSUMACO   =AsVI()
    PSUNOCO   =AsVI()
    PZONECO   =AsVI()
    RANOCO    =AsVI()
    SANSNQ    =AsVI()
    SSNOCO    =AsVI()
    SYMECO    =AsVI()
    TABFIN    =Facultatif(AsVR())
    TANDEF    =AsVR()
    TANPOU    =AsVR()
    TOLECO    =AsVR()
