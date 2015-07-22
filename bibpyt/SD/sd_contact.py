# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
from SD.sd_char_meca import sd_char_chme
from SD.sd_char_meca import sd_char_dual
from SD.sd_ligrel import sd_ligrel
from sd_prof_chno import sd_prof_chno
from SD.sd_champ import sd_champ
from SD.sd_xfem import sd_modele_xfem


class sd_contact(AsBase):

#   Nom des objets préfixé par le nom du concept (8 premiers caractères)
    nomj = SDNom(fin=8)

#   Longueurs des vecteurs fixes (voir CFMMVD.F)
    zpari = 29
    zparr = 5
    zdime = 18
    zmeth = 22
    zdirn = 6
    ztole = 3
    ztypn = 2
    ztypm = 2
    zmaes = 4
    zcmdf = 6
    zcmcf = 13
    zexcl = 3
    zcmxf = 16
    zmesx = 5

# --------------------------------------------------------------------------------------------------#

#   Objets présents quelle que soit la formulation
    MODELE = AsVK8(SDNom(nomj='.CHME.MODEL.NOMO'), lonmax=1,)
    PARACI = AsVI(SDNom(nomj='.CONTACT.PARACI'), lonmax=zpari,)
    PARACR = AsVR(SDNom(nomj='.CONTACT.PARACR'), lonmax=zparr,)
    TYPE = AsVK8(SDNom(nomj='.TYPE'), lonmax=1,)

# --------------------------------------------------------------------------------------------------#

#   Méthodes pour connaître la formulation
    def type_form(self):
        iform = self.PARACI.get()[-1 + 4]
        assert iform in (1, 2, 3, 4,)
        return iform

    def formulation_disc(self):
        return self.type_form() == 1

    def formulation_cont(self):
        return self.type_form() == 2

    def formulation_xfem(self):
        return self.type_form() == 3

    def formulation_unil(self):
        return self.type_form() == 4

    def formulation_mail(self):
        return self.formulation_disc() or self.formulation_cont()

    def contact_resolu(self):
        if not self.formulation_mail():
            return True
        iallverif = self.PARACI.get()[-1 + 8]
        return iallverif == 0

# --------------------------------------------------------------------------------------------------#

#   Formulation unilatérale
    NDIMCU = Facultatif(AsVI(SDNom(nomj='.UNILATE.NDIMCU')))
    CMPGCU = Facultatif(AsVK8(SDNom(nomj='.UNILATE.CMPGCU')))
    COEFD = Facultatif(AsVK8(SDNom(nomj='.UNILATE.COEFD')))
    COEFG = Facultatif(AsVK8(SDNom(nomj='.UNILATE.COEFG')))
    LISNOE = Facultatif(AsVI(SDNom(nomj='.UNILATE.LISNOE')))
    POINOE = Facultatif(AsVI(SDNom(nomj='.UNILATE.POINOE')))

#   Infos sur la formulation unilatérale
    def dimeCU(self):
        if (self.formulation_unil()):
            para = self.NDIMCU.get()
            nnocu = para[-1 + 1]
            ncmpg = para[-1 + 2]
            return nnocu, ncmpg
        return

#   Vérification de la formulation unilatérale
    def check_formulation_unil(self, checker):
        if (self.formulation_unil()):
            nnocu, ncmpg = self.dimeCU()
            assert self.NDIMCU.lonmax == 2
            assert self.CMPGCU.lonmax == ncmpg
            assert self.COEFD.lonmax == nnocu
            assert self.COEFG.lonmax == ncmpg
            assert self.LISNOE.lonmax == nnocu
            assert self.POINOE.lonmax == nnocu + 1
        return

# --------------------------------------------------------------------------------------------------#

#   Formulations DISCRETE/CONTINUE/XFEM
#   Objet commun
    NDIMCO = Facultatif(AsVI(SDNom(nomj='.CONTACT.NDIMCO')))

    def dimeCO(self):
        if (not self.formulation_unil()):
            para = self.NDIMCO.get()
            nzoco = para[-1 + 2]
            nsuco = para[-1 + 3]
            nmaco = para[-1 + 4]
            nnoco = para[-1 + 5]
            ntnoe = para[-1 + 8]
            ntmae = para[-1 + 9]
            ntpt = para[-1 + 16]
            ntelno = para[-1 + 18]
            return nzoco, nsuco, nmaco, nnoco, ntnoe, ntmae, ntpt, ntelno
        return

    def check_dimeco(self, checker):
        if (not self.formulation_unil()):
            nzoco, nsuco, nmaco, nnoco, ntnoe, ntmae, ntpt, ntelno = self.dimeCO(
            )
            assert self.NDIMCO.lonmax == self.zdime
        return

# --------------------------------------------------------------------------------------------------#

#   Formulations maillées (DISCRETE/CONTINUE)
#   Objets communs

#   Objets par zone
    METHCO = Facultatif(AsVI(SDNom(nomj='.CONTACT.METHCO')))
    DIRAPP = Facultatif(AsVR(SDNom(nomj='.CONTACT.DIRAPP')))
    DIRNOR = Facultatif(AsVR(SDNom(nomj='.CONTACT.DIRNOR')))
    JEUFO1 = Facultatif(AsVK8(SDNom(nomj='.CONTACT.JFO1CO')))
    JEUFO2 = Facultatif(AsVK8(SDNom(nomj='.CONTACT.JFO2CO')))
    TOLECO = Facultatif(AsVR(SDNom(nomj='.CONTACT.TOLECO')))

#   Objets par maille esclave
    JEUCOQ = Facultatif(AsVR(SDNom(nomj='.CONTACT.JEUCOQ')))
    JEUPOU = Facultatif(AsVR(SDNom(nomj='.CONTACT.JEUPOU')))

#   Objets de description des zones de contact
    PZONE = Facultatif(AsVI(SDNom(nomj='.CONTACT.PZONECO')))
    PSURMA = Facultatif(AsVI(SDNom(nomj='.CONTACT.PSUMACO')))
    PSURNO = Facultatif(AsVI(SDNom(nomj='.CONTACT.PSUNOCO')))
    CONTMA = Facultatif(AsVI(SDNom(nomj='.CONTACT.MAILCO')))
    CONTNO = Facultatif(AsVI(SDNom(nomj='.CONTACT.NOEUCO')))
    PMANO = Facultatif(AsVI(SDNom(nomj='.CONTACT.PMANOCO')))
    MANOCO = Facultatif(AsVI(SDNom(nomj='.CONTACT.MANOCO')))
    PNOMA = Facultatif(AsVI(SDNom(nomj='.CONTACT.PNOMACO')))
    NOMACO = Facultatif(AsVI(SDNom(nomj='.CONTACT.NOMACO')))

#   Objets pour l'exclusion de noeuds
    PSANS = Facultatif(AsVI(SDNom(nomj='.CONTACT.PSSNOCO')))
    SANSN = Facultatif(AsVI(SDNom(nomj='.CONTACT.SSNOCO')))

#   Objets d'information sur les noeuds/mailles
    TYPEMA = Facultatif(AsVI(SDNom(nomj='.CONTACT.TYPEMA')))
    TYPENO = Facultatif(AsVI(SDNom(nomj='.CONTACT.TYPENO')))
    MAESCL = Facultatif(AsVI(SDNom(nomj='.CONTACT.MAESCL')))

    def check_mail(self, checker):
        if (self.formulation_mail()):
            nzoco, nsuco, nmaco, nnoco, ntnoe, ntmae, ntpt, ntelno = self.dimeCO(
            )
            assert self.METHCO.lonmax == self.zmeth * nzoco
            assert self.DIRAPP.lonmax == 3 * nzoco
            assert self.DIRNOR.lonmax == self.zdirn * nzoco
            assert self.JEUFO1.lonmax == nzoco
            assert self.JEUFO2.lonmax == nzoco
            assert self.TOLECO.lonmax == self.ztole * nzoco

            assert self.JEUCOQ.lonmax == nmaco
            assert self.JEUPOU.lonmax == nmaco

            assert self.PZONE.lonmax == nzoco + 1
            assert self.PSURMA.lonmax == nsuco + 1
            assert self.PSURNO.lonmax == nsuco + 1
            # On utilise lonuti car on a pu éliminer des noeuds/mailles
            assert self.CONTMA.lonuti == nmaco
            assert self.CONTNO.lonuti == nnoco

            assert self.MANOCO.lonmax == 20 * max(nnoco, nmaco)
            assert self.PMANO.lonmax == nnoco + 1

            assert self.NOMACO.lonmax == 20 * max(nnoco, nmaco)
            assert self.PNOMA.lonmax == nmaco + 1

            assert self.PSANS.lonmax == nzoco + 1
            assert self.SANSN.lonmax >= 1

            assert self.TYPENO.lonmax == self.ztypn * nnoco
            assert self.TYPEMA.lonmax == self.ztypm * nmaco
            assert self.MAESCL.lonmax == self.zmaes * ntmae
        return

# --------------------------------------------------------------------------------------------------#

#   Formulation DISCRETE

#   Caractéristisques diverses
    CARADF = Facultatif(AsVR(SDNom(nomj='.CONTACT.CARADF')))

#   Relations linéaires pour QUAD8
    RELLIN = Facultatif(sd_char_chme(SDNom(nomj='.CHME')))

    def check_form_disc(self, checker):
        if (self.formulation_disc()):
            nzoco, nsuco, nmaco, nnoco, ntnoe, ntmae, ntpt, ntelno = self.dimeCO(
            )
            assert self.CARADF.lonmax == self.zcmdf * nzoco
            assert ntnoe == ntpt
        return

# --------------------------------------------------------------------------------------------------#

#   Formulation CONTINUE

#   Caractéristiques diverses
    CARACF = Facultatif(AsVR(SDNom(nomj='.CONTACT.CARACF')))

#   Objets pour l'exclusion des noeuds du frottement seulement
    PFROT = Facultatif(AsVI(SDNom(nomj='.CONTACT.PSANOFR')))
    FROTNO = Facultatif(AsVI(SDNom(nomj='.CONTACT.SANOFR')))
    EXCLFR = Facultatif(AsVR(SDNom(nomj='.CONTACT.EXCLFR')))

#   Ligrel tardif pour l'ajout des modélisations du contact continu
    LIGRE = Facultatif(sd_ligrel(SDNom(nomj='.CHME.LIGRE')))

    def check_form_cont(self, checker):
        if (self.formulation_cont()):
            nzoco, nsuco, nmaco, nnoco, ntnoe, ntmae, ntpt, ntelno = self.dimeCO(
            )
            assert self.CARACF.lonmax == self.zcmcf * nzoco

            assert self.PFROT.lonmax == nzoco + 1
            assert self.FROTNO.lonmax >= 1
            assert self.EXCLFR.lonmax == self.zexcl * nzoco

            if self.contact_resolu():
                # ne pas oublier les () car sd_ligrel.exists est une méthode
                assert self.LIGRE.exists()
        return

# --------------------------------------------------------------------------------------------------#

#   Formulation XFEM

    CARAXF = Facultatif(AsVR(SDNom(nomj='.CONTACT.CARAXF')))
    XFIMAI = Facultatif(AsVK8(SDNom(nomj='.CONTACT.XFIMAI')))
    XNRELL = Facultatif(AsVK24(SDNom(nomj='.CONTACT.XNRELL')))

#   Relations linéaires pour LBB
    RELLBB = Facultatif(sd_char_dual(SDNom(nomj='.DUAL')))

#   Objet spécifique grands glissements
    MAESCX = Facultatif(AsVI(SDNom(nomj='.CONTACT.MAESCX')))

#   Glute objets spécifiques petits glissements (2 fissures max dans les tests)
    PRCHNO = Facultatif(sd_prof_chno(SDNom(nomj='.PRCHN00000')))
    PRCHN1 = Facultatif(sd_prof_chno(SDNom(nomj='.PRCHN00001')))

    def check_form_xfem(self, checker):
        if (self.formulation_xfem()):
            nzoco, nsuco, nmaco, nnoco, ntnoe, ntmae, ntpt, ntelno = self.dimeCO(
            )
            assert self.CARAXF.lonmax == self.zcmxf * nzoco
            assert self.XFIMAI.lonmax == nzoco
            assert self.XNRELL.exists
            paraci = self.PARACI.get()
            if (paraci[0] != 0):
                assert self.MAESCX.lonuti == self.zmesx * ntmae
        return


    def check_char_contact_xfem_XNRELL(self, checker):
        if (self.formulation_xfem()):
            lnom = self.XNRELL.get()
            nbnom = self.XNRELL.lonuti
            nom = lnom[0]
            if (nom[8:14] != '.LISEQ'):
                oo = AsObject(
                    SDNom(nomj=nom, debut=0), genr='V', xous='S', type=Parmi('I', 'R'))
                oo.check(checker)

# --------------------------------------------------------------------------------------------------#
