#@ MODIF sd_cham_mater SD  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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

from SD.sd_carte import sd_carte
from SD.sd_mater import sd_mater
from SD.sd_compor import sd_compor
from SD.sd_util import *


# Remarque :
#------------
# la sd_cham_mater est produite par la seule commande AFFE_MATERIAU.
# C'est pourquoi, on fera appel parfois pour la décrire au vocabulaire de cette
# commande.



class sd_cham_mater_varc(AsBase):
#----------------------------------
# la sd_cham_mater_varc est la partie de la sd_cham_mater correspondant aux variables de commande
# (mot clé AFFE_VARC)

    nomj = SDNom(fin=8)
    CVRCNOM  = AsVK8()
    CVRCGD   = AsVK8()
    CVRCDEF  = AsVR()
    CVRCVARC = AsVK8()
    CVRCCMP  = AsVK8()

    def exists(self):
        return self.CVRCVARC.exists

    # indirection via CVRCVARC:
    def check_cham_mater_i_CVRCVARC(self, checker):
        if not self.exists() : return
        lnom=self.CVRCVARC.get()
        for nom in lnom :
           nom2=self.nomj()[:8]+'.'+nom+'.1'
           sd2=sd_carte(nom2)  ; sd2.check(checker)
           nom2=self.nomj()[:8]+'.'+nom+'.2'
           sd2=sd_carte(nom2)  ; sd2.check(checker)

    # vérification des objets .CVRC* :
    def check_CVRC(self, checker):
        if not self.exists() : return
        xcmp=self.CVRCCMP.get()
        xnom=self.CVRCNOM.get()
        xgd=self.CVRCGD.get()
        xvarc=self.CVRCVARC.get()
        xdef=self.CVRCDEF.get()

        # Les 5 objets ont la meme longueur > 0 :
        nbcvrc=len(xnom)
        assert nbcvrc > 0, (self)
        assert len(xcmp)  == nbcvrc , (xcmp,xnom,self)
        assert len(xgd)   == nbcvrc , (xgd,xnom,self)
        assert len(xvarc) == nbcvrc , (xvarc,xnom,self)
        assert len(xdef)  == nbcvrc , (xdef,xnom,self)

        # Les 4 objets sont "non blancs" :
        sdu_tous_non_blancs(self.CVRCCMP,checker)
        sdu_tous_non_blancs(self.CVRCNOM,checker)
        sdu_tous_non_blancs(self.CVRCGD,checker)
        sdu_tous_non_blancs(self.CVRCVARC,checker)

        # les noms des CRVC doivent etre differents:
        sdu_tous_differents(self.CVRCNOM,checker)



class sd_cham_mater(AsBase):
#=============================
    nomj = SDNom(fin=8)

    # CHAMP_MAT est une carte contenant la liste des noms de matériaux
    # affectées sur les mailles du maillage.
    CHAMP_MAT = sd_carte()

    # La carte TEMPE_REF n'existe pas si AFFE_VARC/NOM_VARC='TEMP' :
    # (voir routine cmtref.f)
    TEMPE_REF = Facultatif(sd_carte())

    # si AFFE_VARC :
    varc = Facultatif(sd_cham_mater_varc(SDNom(nomj='')))

    # si AFFE_COMPOR :
    # COMPOR est une carte définissant les sd_compor affectés
    # sur les mailles du maillage
    COMPOR = Facultatif(sd_carte())


    def check_CHAMP_MAT(self,checker) :
    #----------------------------------
        # on vérifie que la carte .CHAMP_MAT contient bien des noms de matériau.
        vale=self.CHAMP_MAT.VALE.get_stripped()
        desc=self.CHAMP_MAT.DESC.get()
        numgd    =desc[0]
        n_gd_edit=desc[2]
        assert sdu_nom_gd(numgd) == 'NEUT_F', (desc,sdu_nom_gd(numgd))
        ncmp_max=len(sdu_licmp_gd(numgd))
        assert ncmp_max==30 , ncmp_max

        for kedit in range(n_gd_edit):
            v1=vale[kedit*ncmp_max:(kedit+1)*ncmp_max]
            ktref=None
            for k1 in range(len(v1)) :
                x1=v1[k1]
                if x1 == '' : continue

                # cas particulier : 'TREF=>', '25.0'
                if x1=='TREF=>' :
                    ktref=k1
                    continue
                if ktref :
                    assert k1 == ktref+1 , (k1, ktref)
                    # on doit retrouver la valeur de TREF :
                    try :
                        tref=float(x1)
                    except :
                        assert 0 , ' On doit trouver la valeur de TREF: '+x1
                    continue

                # cas général : x1 est un nom de sd_mater
                sd2=sd_mater(x1) ; sd2.check(checker)


    def check_COMPOR(self,checker) :
    #----------------------------------
        # on vérifie (un peu)  la carte .COMPOR (si elle existe)
        desc=self.COMPOR.DESC.get()
        if not desc : return
        vale=self.COMPOR.VALE.get_stripped()
        numgd    =desc[0]
        n_gd_edit=desc[2]
        assert sdu_nom_gd(numgd) == 'COMPOR', (desc,sdu_nom_gd(numgd))
        ncmp_max=len(sdu_licmp_gd(numgd))

        for kedit in range(n_gd_edit):
            v1=vale[kedit*ncmp_max:(kedit+1)*ncmp_max]
            assert v1[3]=='COMP_INCR'   , v1
            sd2=sd_compor(v1[5].split('.')[0].strip()) ; sd2.check(checker)

            for x1 in v1[7:] :
                assert x1=='' , v1







