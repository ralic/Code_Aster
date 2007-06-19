#@ MODIF sd_maillage SD  DATE 19/06/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_cham_no import sd_cham_no
from SD.sd_carte import sd_carte
from SD.sd_l_table import sd_l_table


class sd_maillage(sd_titre):
#-------------------------------
    nomj = SDNom(fin=8)

    DIME = AsVI(lonmax=6, )

    # un sd_maillage a toujours des noeuds :
    NOMNOE = AsPn(ltyp=8)
    COORDO = sd_cham_no()

    # normalement, un sd_maillage a toujours une "sd_l_table" contenant des caractéristiques géométriques :
    lt = sd_l_table(SDNom(nomj=''))

    # si le sd_maillage a des groupes :
    GROUPENO = Facultatif(AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', ))
    GROUPEMA = Facultatif(AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', ))

    # si le sd_maillage a des mailles :
    CONNEX  = Facultatif(AsColl(acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', ))
    TYPMAIL = Facultatif(AsVI())
    NOMMAI  = Facultatif(AsPn(ltyp=8))

    # si le sd_maillage a des super-mailles :
    NOMACR  = Facultatif(AsVK8())
    SUPMAIL = Facultatif(AsColl(acces='NO', stockage='DISPERSE', modelong='VARIABLE', type='I', ))
    PARA_R  = Facultatif(AsVR())
    TYPL    = Facultatif(AsVI())

    # si le sd_maillage est linéique (tube_GV) :
    abs_curv  = Facultatif(sd_carte(SDNom(nomj='.ABS_CURV')))

    ADAPTATION = Facultatif(AsVI(lonmax=1, ))
    FORM = Facultatif(AsVK32(SDNom(debut=19), lonmax=2, ))


    def u_dime(self):
        dime=self.DIME.get()
        nb_no    =dime[0]
        nb_nl    =dime[1]
        nb_ma    =dime[2]
        nb_sm    =dime[3]
        nb_sm_mx =dime[4]
        dim_coor =dime[5]
        return nb_no, nb_nl, nb_ma, nb_sm, nb_sm_mx, dim_coor

# remarque :  la sd_maillage pouvant etre "volumineuse", on s'interdit (pour des raisons de temps CPU)
#             de vérifier le contenu des gros objets.

    def check_DIME(self,checker):
        nb_no, nb_nl, nb_ma, nb_sm, nb_sm_mx, dim_coor = self.u_dime()
        assert nb_sm <= nb_sm_mx , (nb_sm, nb_sm_mx)
        if nb_nl > 0 : assert nb_sm > 0
        assert nb_no > 0  , nb_no
        assert dim_coor in (2,3), dim_coor


    def check_NOEUDS(self,checker):
        nb_no, nb_nl, nb_ma, nb_sm, nb_sm_mx, dim_coor = self.u_dime()
        assert self.NOMNOE.nomuti == nb_no + nb_nl , (nb_no, nb_nl)
        assert self.COORDO.VALE.lonmax == 3*nb_no , nb_no


    def check_MAILLES(self,checker):
        nb_no, nb_nl, nb_ma, nb_sm, nb_sm_mx, dim_coor = self.u_dime()
        if nb_ma == 0 : return
        assert self.NOMMAI.nommax  == nb_ma , nb_ma
        assert self.TYPMAIL.lonmax == nb_ma , nb_ma
        assert self.CONNEX.nmaxoc  == nb_ma , nb_ma


    def check_SSS(self,checker):
        nb_no, nb_nl, nb_ma, nb_sm, nb_sm_mx, dim_coor = self.u_dime()
        if nb_sm == 0 : return
        assert self.NOMACR.lonmax  == nb_sm    , nb_sm
        assert self.PARA_R.lonmax  == 14*nb_sm , nb_sm
        assert self.SUPMAIL.nmaxoc  == nb_sm    , nb_sm

    def check_TYPL(self,checker):
        nb_no, nb_nl, nb_ma, nb_sm, nb_sm_mx, dim_coor = self.u_dime()
        if nb_nl == 0 : return
        assert self.TYPL.lonmax  == nb_nl    , nb_nl
        typl=self.TYPL.get()
        for k in typl :
            assert  k in (-1, -2) , typl

