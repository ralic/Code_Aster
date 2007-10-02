#@ MODIF sd_matr_asse_com SD  DATE 02/10/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_nume_ddl import sd_nume_ddl
from SD.sd_matr_cine import sd_matr_cine


class sd_matr_asse_com(sd_titre):
#-----------------------------
    nomj = SDNom(fin=19)

    REFA = AsVK24(SDNom(debut=19), lonmax=10, )
    VALM = AsColl(acces='NU', stockage='DISPERSE', modelong='CONSTANT', type=Parmi('C', 'R'), ltyp=Parmi(16, 8), )
    UALF = Facultatif(AsColl(acces='NU', stockage='DISPERSE', modelong='CONSTANT', type=Parmi('C', 'R'), ltyp=Parmi(16, 8), ))
    VALF = Facultatif(AsColl(acces='NU', stockage='DISPERSE', modelong='VARIABLE', type=Parmi('C', 'R'), ltyp=Parmi(16, 8), ))
    WALF = Facultatif(AsColl(acces='NU', stockage='DISPERSE', modelong='VARIABLE', type=Parmi('C', 'R'), ltyp=Parmi(16, 8), ))
    CONL = Facultatif(OJBVect(type=Parmi('C', 'R')))
    DIGS = Facultatif(OJBVect(type=Parmi('C', 'R'))) # seulement si solveurs LDLT et MULT_FRONT
    LIME = Facultatif(AsVK8(lonmax=1, ))
    cine = Facultatif(sd_matr_cine(SDNom(nomj='')))


