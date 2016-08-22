# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
from SD.sd_resu_dyna import sd_resu_dyna

from SD.sd_proj_mesu import sd_proj_mesu
from SD.sd_util import *

class sd_dyna_gene_common(AsBase):
#--------------------------------------
    nomj = SDNom(fin=19)

    # Ces trois objets sont facultatifs car dans le cas d'un calcul HARM, seul
    # un parmi les trois est obligatoire.
    ACCE = Facultatif(OJBVect(type=Parmi('C', 'R')))
    VITE = Facultatif(OJBVect(type=Parmi('C', 'R')))
                      # peuvent etre des gros objets. Contienent des reels si
                      # calcul TRAN et des complexes si HARM
    DEPL = Facultatif(OJBVect(type=Parmi('C', 'R')))

    PTEM = Facultatif(AsVR())
                      # Pas de temps d'integration sauvegardés aux instants
                      # d'archivage

    DISC = AsVR()
                # gros objet. contient la liste des instants du calcul
                # sauvegardes si TRANS et les frequences si HARM
    ORDR = AsVI()  # gros objet.

    DESC = AsVI(lonmax=5)

    # si nbexcit > 0 :
    FACC = Facultatif(AsVK8())
    FDEP = Facultatif(AsVK8())
    FVIT = Facultatif(AsVK8())
    IPSD = Facultatif(AsVR())

    def u_dime(self):  # --> ok

        desc = self.DESC.get()
        indic = desc[0]

        assert indic in (1, 2, 3, 4)

        if (indic in (1, 2, 3)):
                type_calcul = 'TRAN'
        elif (indic == 4):
                type_calcul = 'HARM'

        nbmode = desc[1]
        assert nbmode > 0
        nbnoli = desc[2]
        assert nbnoli >= 0
        nbvint = desc[3]
        assert nbvint >= 0

        nbsauv = self.ORDR.lonmax
        assert nbsauv > 0

        if self.FACC.exists:
            nbexcit = self.FACC.lonmax / 2
            assert nbexcit >= 0
        else:
            nbexcit = 0

        return (type_calcul, nbmode, nbnoli, nbsauv, nbexcit, nbvint)

    def check_DESC(self, checker):  # --> ok
        desc = self.DESC.get()

        # on verifie que le .DESC est rempli avec des valeurs autorisees
        assert desc[0] in (1, 2, 3, 4), desc

        # on verifie que dans le cas d'un calcul harmonique, il n'y a pas de
        # presence de non linearites
        if (desc[0] == 4):
                assert desc[2] == 0  # pas de nonlinearie si HARM
                assert desc[3] == 0  # pas de correction statique ou multi appui si HARM


    def check_ORDR_DISC(self, checker):
        type_calcul, nbmode, nbnoli, nbsauv, nbexcit, nbvint = self.u_dime()

        assert self.ORDR.lonmax == nbsauv  # verification de la longueur
        sdu_tous_differents(self.ORDR, checker)
                            ## verification de tous les elements differents
        if (nbsauv > 1):
                assert sdu_monotone(
                    self.ORDR.get()) == 1  # test de monotonie croissante

        assert self.DISC.lonmax == nbsauv  # verification de la longueur
        sdu_tous_differents(self.DISC, checker)
                            ## verification de tous les elements differents

        if (type_calcul == 'TRAN' and nbsauv > 1):
                assert sdu_monotone(
                    self.DISC.get()) == 1  # test de monotonie croissante
                assert self.PTEM.lonmax == nbsauv

    def check_DEPL_VITE_ACCE(self, checker):
        type_calcul, nbmode, nbnoli, nbsauv, nbexcit, nbvint = self.u_dime()

        if (type_calcul == 'TRAN'):
                assert (self.DEPL.lonmax == nbsauv * nbmode and self.VITE.lonmax == nbsauv *
                        nbmode and self.ACCE.lonmax == nbsauv * nbmode)
                        # on verifie que les trois objets existent et sont de
                        # la bonne longueur
                assert (
                    self.DEPL.type == 'R' and self.VITE.type == 'R' and self.ACCE.type == 'R')
                        ## on verifie que les valeurs sont reelles
        elif (type_calcul == 'HARM'):
                # on verifie qu'au moins un des trois objets existe et qu'il
                # est de type complexe
                assert self.DEPL.exists or self.VITE.exists or self.ACCE.exists
                if self.DEPL.exists:
                    assert self.DEPL.lonmax == nbsauv * \
                        nbmode and self.DEPL.type == 'C'
                if self.VITE.exists:
                    assert self.VITE.lonmax == nbsauv * \
                        nbmode and self.VITE.type == 'C'
                if self.ACCE.exists:
                    assert self.ACCE.lonmax == nbsauv * \
                        nbmode and self.ACCE.type == 'C'

    def check_EXCIT(self, checker):
        type_calcul, nbmode, nbnoli, nbsauv, nbexcit, nbvint = self.u_dime()
        if nbexcit == 0:
            return
        assert self.FACC.lonmax == 2 * nbexcit
        assert self.FDEP.lonmax == 2 * nbexcit
        assert self.FVIT.lonmax == 2 * nbexcit
        # assert self.IPSD.lonmax == nbexcit*neq # JP : neq != nbmode. Que vaut
        # neq ??


class sd_dyna_gene_nl(AsBase):
#--------------------------------------
    nomj = SDNom(fin=19)

    VIND = AsVI()
    VINT = AsVR()
    INTI = AsVK24()
    TYPE = AsVI()

    def u_dime_nl(self):
        
        if self.TYPE.lonmax == None : return (0, 0, 0)
        
        nbnoli  = self.TYPE.lonmax
        vindex  = self.VIND.get()
        nbvint  = vindex[-1]-1

        nbsaves = self.VINT.lonmax/nbvint
        return nbnoli, nbvint, nbsaves

    def check_NONL(self, checker):
        nbnoli, nbvint, nbsaves = self.u_dime_nl()
        if nbnoli > 0  :
            assert self.VIND.lonmax == nbnoli+1
            assert self.VINT.lonmax == nbvint*nbsaves
            assert self.INTI.lonmax == 5*nbnoli
            assert self.TYPE.lonmax == nbnoli



class sd_dyna_gene(sd_titre, sd_resu_dyna, sd_dyna_gene_common):
#--------------------------------------
    nomj      = SDNom(fin=19)
    sd_nl     = Facultatif(sd_dyna_gene_nl(SDNom(nomj='.NL', debut=16,fin=19)))

    # non linearities
    def check_NONL(self, checker):
        desc      = self.DESC.get()
        nbnoli    = desc[2]
        if nbnoli > 0 : assert(self.sd_nl != None)
