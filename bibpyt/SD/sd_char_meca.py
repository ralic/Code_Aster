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

from SD.sd_ligrel import sd_ligrel
from SD.sd_carte import sd_carte


class sd_char_gf_xx(AsBase):
    nomj = SDNom(fin=19)
    VALE = AsVR()
    NCMP = AsVK8()


class sd_char_dual(AsBase):
#   -- pour les C.L. dualisees eventuellement non-lineaires (TYPE_CHARGE='SUIV') :
#      nb_paquet : nombre de "paquets" de relations cinematiques  dualisees
#      Le "paquet" correspond a l'ensemble des relations ajoutees a la charge lors d'un appel a aflrch.F90
#      Un paquet correspond par exemple aux relations dues a 1 occurrence du mot cle LIAISON_SOLIDE
#      Les relations cinematiques dualisees sont linearisees pendant le calcul : B*U=U0
#      Les coefficients de B sont stockes dans la carte .CMULT et ceux de U0 dans .CIMPO
#      A chaque terme non nul de B est associe une maille tardive dans charge.LIGRE.NEMA.
#      Les termes correspondant a une relation sont stockes a la suite les uns des autres.
#      En resume :
#         une charge contient une liste de paquets de relations
#         un paquet de relation contient une liste de relations
#         une relation correspond a une liste de termes
#         pour chaque terme il existe une maille tardive dans .NEMA

    nomj = SDNom(fin=13)
    PRDK = AsVK8()
    PRDI = AsVI()
    PRDSO = AsVK8()

    def exists(self):
        # retourne True si la SD semble exister.
        return self.PRDK.exists

    def nb_paquet(self):
        if not self.exists() : return 0
        return self.PRDK.lonuti

    def check_PRDK(self,checker):
#       contient les "types" des paquets de relations
        nbl=self.nb_paquet()
        if nbl > 0 :
           prdk=self.PRDK.get_stripped()
           for typ1 in prdk[0:nbl] :
               assert typ1 in ('LIN','NLIN','2D2','2D1','3D3','3D2','3D1','ROTA3D','ROTA2D')
#              'LIN'  : Le paquet ne contient que des relations TOUJOURS lineaires. Ex : FACE_IMPO (sauf DNOR)
#              'NLIN' : Le paquet contient des relations qu'il faut reactualiser si TYPE_CHARGE='SUIV'.


#              Valeurs pour LIAISON_SOLIDE :
#              '2D2' : Le paquet correspond au cas LIAISON_SOLIDE en 2D, avec des ddls de translation.
#                      Le nuage des noeuds solidifie est surfacique
#              '2D1' : Le paquet correspond au cas LIAISON_SOLIDE en 2D, avec des ddls de translation.
#                      Le nuage des noeuds solidifie est lineique
#              '3D3' : Le paquet correspond au cas LIAISON_SOLIDE en 3D, avec des ddls de translation.
#                      Le nuage des noeuds solidifie est volumique
#              '3D2' : Le paquet correspond au cas LIAISON_SOLIDE en 3D, avec des ddls de translation.
#                      Le nuage des noeuds solidifie est surfacique
#              '3D1' : Le paquet correspond au cas LIAISON_SOLIDE en 3D, avec des ddls de translation.
#                      Le nuage des noeuds solidifie est lineique
#              'ROTA2D' : Le paquet correspond au cas LIAISON_SOLIDE en 2D, avec des ddls de rotation.
#              'ROTA3D' : Le paquet correspond au cas LIAISON_SOLIDE en 3D, avec des ddls de rotation.


    def check_PRDSO(self,checker):
        nbl=self.nb_paquet()
        if nbl > 0 :
           assert 4*nbl <= self.PRDSO.lonmax

    def check_PRDI(self,checker):
        nbl=self.nb_paquet()
        if nbl > 0 :
           assert 3*nbl <= self.PRDI.lonmax


class sd_char_chme(AsBase):
    nomj = SDNom(fin=13)

    MODEL_NOMO = AsVK8(SDNom(nomj='.MODEL.NOMO'), lonmax=1, )

    LIGRE = Facultatif(sd_ligrel())

    CIMPO = Facultatif(sd_carte())
    CMULT = Facultatif(sd_carte())
    EPSIN = Facultatif(sd_carte())
    F1D1D = Facultatif(sd_carte())
    F1D2D = Facultatif(sd_carte())
    F1D3D = Facultatif(sd_carte())
    F2D2D = Facultatif(sd_carte())
    F2D3D = Facultatif(sd_carte())
    F3D3D = Facultatif(sd_carte())
    FCO2D = Facultatif(sd_carte())
    FCO3D = Facultatif(sd_carte())
    FELEC = Facultatif(sd_carte())
    FL101 = Facultatif(sd_carte())
    FL102 = Facultatif(sd_carte())
    FLUX  = Facultatif(sd_carte())
    FORNO = Facultatif(sd_carte())
    IMPE  = Facultatif(sd_carte())
    ONDE  = Facultatif(sd_carte())
    PESAN = Facultatif(sd_carte())
    PRESS = Facultatif(sd_carte())
    PREFF = Facultatif(sd_carte())
    ROTAT = Facultatif(sd_carte())
    SIGIN = Facultatif(sd_carte())
    SIINT = Facultatif(sd_carte())
    VNOR  = Facultatif(sd_carte())
    ONDPL = Facultatif(sd_carte())
    ONDPR = Facultatif(sd_carte())
    EFOND = Facultatif(sd_carte())

    VEASS = Facultatif(AsVK8(lonmax=1, ))
    VEISS = Facultatif(AsVK24(lonmax=6, ))
    EVOL_CHAR  = Facultatif(AsVK8(SDNom(nomj='.EVOL.CHAR'), lonmax=1, ))
    TEMPE_TEMP = Facultatif(AsVK8(SDNom(nomj='.TEMPE.TEMP'), lonmax=1, ))

class sd_char_meca(AsBase):
    nomj = SDNom(fin=8)

    TYPE = AsVK8(lonmax=1)

    CHME = Facultatif(sd_char_chme())
    DUAL = Facultatif(sd_char_dual())

    TRANS01 = Facultatif(AsVR(lonmax=6, ))
    TRANS02 = Facultatif(AsVR(lonmax=6, ))
    LISMA01 = Facultatif(AsVI(lonmax=12, ))
    LISMA02 = Facultatif(AsVI(lonmax=12, ))
    POIDS_MAILLE = Facultatif(AsVR())

    def check_DUAL(self,checker):
#       si CIMPO existe, il doit aussi exister CMULT et DUAL :
        if self.CHME.CIMPO.DESC.exists :
            assert self.CHME.CMULT.DESC.exists
            assert self.DUAL.exists()
