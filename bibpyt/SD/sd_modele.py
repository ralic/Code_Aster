#@ MODIF sd_modele SD  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_ligrel import sd_ligrel
from SD.sd_fiss_xfem import sd_fiss_xfem, sd_xfem_com1
from SD.sd_prof_chno import sd_prof_chno


class sd_modele_XFEM(AsBase):
#-----------------------------
    nomj = SDNom(fin=8)
    # Questions aux responsables XFEM :
    #   - faut-il garder FISS et NFIS ?
    #   - Est-il normal de modifier les sd_fiss_xfem dans MODI_MODELE_XFEM ?

    XFEM_CONT   = AsVI()
    XFEM_SDCONT = AsVK24()
    FISS   = AsVK8()
    NFIS   = AsVI(lonmax=2,)  # nombre de fissures , degré de l'approximation
    com1   = sd_xfem_com1(SDNom(nomj=''))

    glute_XFEM = Facultatif(sd_prof_chno(SDNom(nomj='.PRCHN00000'))) # fiche 10833
    glute_XFEM2= Facultatif(sd_prof_chno(SDNom(nomj='.PRCHN00001'))) # fiche 10833


    if 0 :
        # Questions aux responsables XFEM :
        #   - faut-il garder FISS et NFIS ?
        #   - Est-il normal de modifier les sd_fiss_xfem dans MODI_MODELE_XFEM ?
        # indirection vers FISS_XFEM car MODI_MODELE_XFEM modifie FISS_XFEM
        # (Damijan va corriger cela avec la multi-fissuration)
        # ATTENTION : Ce bout de programme suppose que FISS est de longueur 1 ce qui contradictoire avec la multi-fissuration)
        def check_modele_i_FISS(self, checker):
            if not self.FISS.get() : return
            nom=self.FISS.get()[0]
            sd2=sd_fiss_xfem(nom) ; sd2.check(checker)



class sd_modele(AsBase):
#-----------------------------
    nomj = SDNom(fin=8)

    MODELE = sd_ligrel()
    NOEUD = Facultatif(AsVI())
    MAILLE = Facultatif(AsVI())

    # Si le modèle vient de MODI_MODELE_XFEM :
    xfem = Facultatif(sd_modele_XFEM(SDNom(nomj='')))


    def check_existence(self,checker) :
        exi_liel=self.MODELE.LIEL.exists
        exi_maille=self.MAILLE.exists
        exi_noeud=self.NOEUD.exists

        # si .LIEL => .MAILLE et .NOEUD
        if exi_liel :
            assert exi_maille
            assert exi_noeud

