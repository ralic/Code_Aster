#@ MODIF sd_feti SD  DATE 09/05/2007   AUTEUR PELLET J.PELLET 
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

# AJACOT_PB en attendant la correction de la fiche 10475 :
# on dédouble la SD pour la rendre facultative.


class sd_feti1(AsBase):
    nomj = SDNom(fin=19)
    FLIM = AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', )
    FDIM = AsVI(lonmax=5, )
    FREF = AsVK8()
    FETG = AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', )
    FLIN = AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='K', ltyp=24, )
    FLII = AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', )
    FETB = AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', )
    FETA = AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', )
    FETJ = AsVI()
    FETI = AsVI()
    FETH = AsVI(lonmax=4, )


class sd_feti(AsBase):
    nomj = SDNom(fin=19)
    sd1 = Facultatif(sd_feti1(SDNom('')))

