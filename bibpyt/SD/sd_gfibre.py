# coding=utf-8
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
# person_in_charge: jean-luc.flejou at edf.fr

from SD import *
from SD.sd_titre import sd_titre

class sd_gfibre(sd_titre):
#-------------------------------------
    nomj = SDNom(fin=8)
    POINTEUR          = AsVI()
    CARFI             = AsVR()
    NOMS_GROUPES      = AsPn(ltyp=24)
    NB_FIBRE_GROUPE   = AsVI()
    TYPE_GROUPE       = AsVI()
    GFMA              = Facultatif(AsVK8(lonmax=1))
    CARACSD           = AsVI(lonmax=3, )

    def u_caracsd(self):
        caracsd  = self.CARACSD.get()
        nbgfsd   = caracsd[0]
        nbcarasd = caracsd[1:3]
        return nbgfsd,nbcarasd

    def check_dimension(self,checker):
        nbgfsd, nbcarasd  = self.u_caracsd()
        nbgf = self.NOMS_GROUPES.nommax
        # Vérif des dimensions des objets
        assert nbgf == nbgfsd, (nbgf, nbgfsd)
        assert self.NB_FIBRE_GROUPE.lonmax == nbgf
        assert self.TYPE_GROUPE.lonmax == nbgf
        assert self.POINTEUR.lonmax == nbgf

    def check_CARFI(self,checker) :
        nbgfsd, nbcarasd  = self.u_caracsd()
        nbgf = self.NOMS_GROUPES.nommax
        #
        assert nbgf == nbgfsd , (nbgf, nbgfsd)
        #
        pointeur  = self.POINTEUR.get()
        nb_fibre  = self.NB_FIBRE_GROUPE.get()
        ty_groupe = self.TYPE_GROUPE.get()
        nbfib_tot = 0
        for igf in range(nbgf) :
            assert ty_groupe[igf] in [1,2], (ty_groupe[igf])
            assert pointeur[igf] == nbfib_tot+1 , (nbcarasd[ty_groupe[igf]-1], igf, nbfib_tot, pointeur[igf])
            nbfib_tot += nb_fibre[igf]*nbcarasd[ty_groupe[igf]-1]
        assert self.CARFI.lonmax == nbfib_tot , (nbfib_tot, self.CARFI.lonmax)

    def check_GFMA(self,checker):
        if not self.GFMA.exists: return
        gfma = self.GFMA.get_stripped()
        from SD.sd_maillage import sd_maillage
        sd2=sd_maillage(gfma[0])
        sd2.check(checker)
