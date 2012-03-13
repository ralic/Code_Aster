#@ MODIF sd_ligrel SD  DATE 13/03/2012   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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

from SD import *

from SD.sd_maillage  import sd_maillage

class sd_ligrel(AsBase):
    nomj = SDNom(fin=19)

    LGRF = AsVK8(lonmax=2, docu=Parmi('ACOU', 'MECA', 'THER'), )
    NBNO = AsVI(lonmax=1,)
    PRNM = AsVI()

    # AU_MOINS_UN : LIEL, SSSA
    # LIEL : il existe des éléments finis
    # SSSA : il existe des sous-structures statiques
    LIEL = Facultatif(AsColl( acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', ))
    SSSA = Facultatif(AsVI())
    # ENSEMBLE  : LIEL, REPE
    REPE = Facultatif(AsVI())

    # si mailles tardives :
    NEMA = Facultatif(AsColl( acces='NU', stockage='CONTIG', modelong='VARIABLE', type='I', ))
    # si noeuds tardifs :
    PRNS = Facultatif(AsVI())
    LGNS = Facultatif(AsVI())


    def exists(self):
        # retourne True si la SD semble exister.
        return self.LGRF.exists

    def check_LGRF(self,checker):
        if not self.exists() : return
        lgrf=self.LGRF.get_stripped()
        sd2=sd_maillage(lgrf[0]); sd2.check(checker)
        # assert lgrf[1] != ''   # on ne sait pas toujours "remonter" à un modèle (lgphmo.f)
        # Je n'arrive pas à importer sd_modele (cyclage des imports):
        # from SD.sd_modele    import sd_modele
        # sd2=sd_modele.sd_modele(lgrf[1]); sd2.check(checker)


    def check_presence(self,checker):
        if not self.exists() : return
        exi_liel=self.LIEL.exists
        exi_sssa=self.SSSA.exists
        exi_repe=self.REPE.exists
        exi_nema=self.NEMA.exists
        exi_prns=self.PRNS.exists
        exi_lgns=self.LGNS.exists

        # AU_MOINS_UN : .LIEL, .SSSA
        assert exi_liel or exi_sssa

        # SI .LIEL AU_MOINS_UN : .REPE, .NEMA
        if exi_liel :
            assert exi_repe or exi_nema

        # .REPE => .LIEL
        if exi_repe : assert exi_liel

        # .NEMA => .LIEL
        if exi_nema : assert exi_liel

        # noeuds tardifs => .PRNS .LGNS et .NEMA
        nb_no_tard= self.NBNO.get()[0]
        if nb_no_tard > 0 :
            assert exi_prns
            assert exi_lgns
            assert exi_nema
            assert self.LGNS.lonmax >= nb_no_tard   # .LGNS est surdimensionné
            nbec= self.PRNS.lonmax / nb_no_tard
            assert self.PRNS.lonmax == nb_no_tard * nbec , (nbec, nb_no_tard)
            assert nbec >= 1 and nbec < 10 , nbec


