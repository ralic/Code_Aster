# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: samuel.geniaut at edf.fr

import os.path as osp
import string
from pprint import pformat

import aster_core
from code_aster.Cata.DataStructure import (modele_sdaster, maillage_sdaster,
    table_sdaster)
from mac3coeur_coeur import CoeurFactory


def perm_mac3coeur_ops(self, **args):
    """Corps principal de la macro pour la permutation des AC dans MAC3COEUR"""
    import aster
    from code_aster.Cata.Syntax import _F
    from mac3coeur_ac_permute import MACRO_AC_PERMUTE
    from Utilitai.UniteAster import UniteAster
    from Utilitai.Utmess import UTMESS

    STAT_NON_LINE = self.get_cmd('STAT_NON_LINE')
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')
    CREA_RESU = self.get_cmd('CREA_RESU')
    AFFE_CHAR_CINE = self.get_cmd('AFFE_CHAR_CINE')

    self.set_icmd(1)
    datg = aster_core.get_option("repdex")
    coeur_factory = CoeurFactory(datg)

    _typ_coeur_N = self['TYPE_COEUR_N']
    _typ_coeur_P = self['TYPE_COEUR_NP1']
    _TAB_N = self['TABLE_N']
    _l_tabn1 = []
    for el in _TAB_N :
        _l_tabn1.append(el.EXTR_TABLE())

    l_RESUI = self['RESU_N']
    assert (len(_TAB_N) == len(l_RESUI))
    l_last_i = []
    _l_MA_N = []
    for RESUI in l_RESUI :
        l_last_i.append(RESUI.LIST_PARA()['INST'][-1])
        # on recupere le concept maillage
        iret, ibid, nom_mo = aster.dismoi('MODELE', RESUI.nom, 'RESULTAT', 'F')
        iret, ibid, nom_ma = aster.dismoi(
            'NOM_MAILLA', nom_mo.strip(), 'MODELE', 'F')

        _MA_N = self.get_concept_by_type(nom_ma, maillage_sdaster)
        _l_MA_N.append(_MA_N)

    _l_coeur = []
    for _tabn1 in _l_tabn1 :
    # on recupere le nom du coeur
        name = _tabn1.para[0]

    # et on renomme la colonne qui identifie les assemblages
        _tabn1.Renomme(name, 'idAC')
        _coeur = coeur_factory.get(_typ_coeur_N)(name, _typ_coeur_N, self, datg)
        _coeur.init_from_table(_tabn1)
        _l_coeur.append(_coeur)


    _TAB_NP1 = self['TABLE_NP1']
    _tabp1 = _TAB_NP1.EXTR_TABLE()

    # on recupere le nom du coeurq
    namep1 = _tabp1.para[0]

    # et on renomme la colonne qui identifie les assemblages
    _tabp1.Renomme(namep1, 'idAC')
    _coeurp1 = coeur_factory.get(_typ_coeur_P)(namep1, _typ_coeur_P, self, datg)
    _coeurp1.init_from_table(_tabp1)

    _MA1 = self['MAILLAGE_NP1']
    _MA_NP1 = _coeurp1.affectation_maillage(_MA1)
    _MO_NP1 = _coeurp1.affectation_modele(_MA_NP1)
    _coeurp1.recuperation_donnees_geom(_MA_NP1)
    _GFF_NP1 = _coeurp1.definition_geom_fibre()
    _CARANP1 = _coeurp1.definition_cara_coeur(_MO_NP1, _GFF_NP1)

    _fluence = 0.0
    _timep1 = _coeurp1.definition_time(_fluence, 1.)
    _FLU_NP1 = _coeurp1.definition_fluence(_fluence, _MA_NP1,0.)
    _CHTHNP1 = _coeurp1.definition_champ_temperature(_MA_NP1)
    _AFSCNP1 = _coeurp1.definition_materiau(
        _MA_NP1, _GFF_NP1, _FLU_NP1, _CHTHNP1, CONTACT='NON')

    _CL_BID = AFFE_CHAR_CINE(MODELE=_MO_NP1, MECA_IMPO=(
        _F(TOUT='OUI', DX=0.0, DY=0.0, DZ=0.0, DRX=0.0, DRY=0.0, DRZ=0.0,),))

    tran_x = 0.0

    indice = 0

    # calcul bidon aster pour initialisation de donnees

    compor = [_F(
        RELATION='MULTIFIBRE', GROUP_MA=('CRAYON', 'T_GUIDE'), PARM_THETA=0.5, DEFORMATION='GROT_GDEP',),
        _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
        _F(RELATION='DIS_CHOC',   GROUP_MA ='RES_TOT',),
        _F(RELATION='ELAS',       GROUP_MA =(
           'EBOINF', 'EBOSUP', 'RIG', 'DIL',),),
        _F(RELATION='VMIS_ISOT_TRAC', GROUP_MA ='MAINTIEN', DEFORMATION='PETIT',), ]

    self.DeclareOut('BIDON', self.sd)
    __BIDON = STAT_NON_LINE(MODELE=_MO_NP1,
                           CHAM_MATER=_AFSCNP1,
                           CARA_ELEM=_CARANP1,
                           EXCIT=(_F(CHARGE=_CL_BID,),),
                           COMPORTEMENT =(
                           _F(RELATION='MULTIFIBRE', GROUP_MA=(
                              'CRAYON', 'T_GUIDE'), PARM_THETA=0.5, DEFORMATION='GROT_GDEP',),
                           _F(RELATION='DIS_GRICRA',
                                       GROUP_MA='ELA',),
                           _F(RELATION='DIS_CHOC',
                                       GROUP_MA='RES_TOT',),
                           _F(RELATION='ELAS',       GROUP_MA=(
                              'EBOINF', 'EBOSUP', 'RIG', 'DIL',),),
                           _F(RELATION='VMIS_ISOT_TRAC', GROUP_MA ='MAINTIEN', DEFORMATION='PETIT',),),
                           INCREMENT=_F(
                           LIST_INST=_timep1, NUME_INST_FIN=1,),
                           NEWTON=_F(
                           MATRICE='TANGENTE', REAC_ITER=1,),
                           SOLVEUR=_F(
                           METHODE='MUMPS', RENUM='AMF', GESTION_MEMOIRE='OUT_OF_CORE', ELIM_LAGR='NON', PCENT_PIVOT=80,),
                           )
# il faut un resultat avec un seul instant : 0.
# on le reconstruit a partir de __BIDON
    _tini = __BIDON.LIST_PARA()['INST'][-1]

    __CHDEP = CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                         OPERATION='EXTR',
                         PRECISION=1.0E-10,
                         RESULTAT=__BIDON,
                         NOM_CHAM='DEPL',
                         INST=_tini,)

    __ASSDEP = CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                          MODELE=_MO_NP1,
                          OPERATION='ASSE',
                          ASSE=(_F(TOUT='OUI',
                                    CHAM_GD=__CHDEP,
                                    CUMUL='NON',
                                    COEF_R=0.0),),)

    #__RESU_F = CREA_RESU(OPERATION='AFFE',
    BIDON=CREA_RESU(
                         OPERATION='AFFE',
                         TYPE_RESU='EVOL_NOLI',
                         NOM_CHAM='DEPL',
                         AFFE=_F(CHAM_GD=__ASSDEP,
                                 INST=0.0,
                                 MODELE=_MO_NP1,))

    __CHSIE = CREA_CHAMP(TYPE_CHAM='ELGA_SIEF_R',
                         OPERATION='EXTR',
                         PRECISION=1.0E-10,
                         RESULTAT=__BIDON,
                         NOM_CHAM='SIEF_ELGA',
                         INST=_tini,)

    __ASSSIE = CREA_CHAMP(TYPE_CHAM='ELGA_SIEF_R',
                          MODELE=_MO_NP1,
                          OPERATION='ASSE',
                          ASSE=(_F(TOUT='OUI',
                                   CHAM_GD=__CHSIE,
                                   CUMUL='NON',
                                   COEF_R=0.0),),)

    CREA_RESU(reuse=BIDON,
              OPERATION='AFFE',
              TYPE_RESU='EVOL_NOLI',
              NOM_CHAM='SIEF_ELGA',
              AFFE=_F(CHAM_GD=__ASSSIE,
                      INST=0.0,
                      MODELE=_MO_NP1,))

    __CHVAR = CREA_CHAMP(TYPE_CHAM='ELGA_VARI_R',
                         OPERATION='EXTR',
                         PRECISION=1.0E-10,
                         RESULTAT=__BIDON,
                         NOM_CHAM='VARI_ELGA',
                         INST=_tini,)

    __ASSVAR = CREA_CHAMP(TYPE_CHAM='ELGA_VARI_R',
                          MODELE=_MO_NP1,
                          OPERATION='ASSE',
                          ASSE=(_F(TOUT='OUI',
                                    CHAM_GD=__CHVAR,
                                    CUMUL='NON',
                                    COEF_R=0.0),),)

    CREA_RESU(reuse=BIDON,
              OPERATION='AFFE',
              TYPE_RESU='EVOL_NOLI',
              NOM_CHAM='VARI_ELGA',
              AFFE=_F(CHAM_GD=__ASSVAR,
                      INST=0.0,
                      MODELE=_MO_NP1,))
    nbresu = len(l_RESUI)
    assert (len(_l_coeur) == nbresu)
    assert (len(l_last_i) == nbresu)
    assert (len(_l_MA_N) == nbresu)

    for nom in _coeurp1.nameAC.keys():
        for i in xrange(len(_l_coeur)) :
            _coeur = _l_coeur[i]
            last_i = l_last_i[i]
            RESUI  = l_RESUI[i]
            _MA_N  = _l_MA_N[i]
            if nom in _coeur.nameAC:
                #print 'index z : ',_coeurp1.get_index(_coeurp1.nameAC[nom][0]),_coeurp1.get_index(_coeur.nameAC[nom][0])
                #print 'index y : ',_coeurp1.get_index(_coeurp1.nameAC[nom][2]),_coeurp1.get_index(_coeur.nameAC[nom][2])
                tran_z = _coeurp1.pas_assemblage * \
                    (_coeurp1.get_index(_coeurp1.nameAC[nom][
                    0]) - _coeurp1.get_index(_coeur.nameAC[nom][0]))
                tran_y = _coeurp1.pas_assemblage * \
                    (_coeurp1.get_index(_coeurp1.nameAC[nom][
                    2]) - _coeurp1.get_index(_coeur.nameAC[nom][2]))
                #print 'tran_z, tran_y, tran_x = ',tran_z, tran_y, tran_x
                #print 'AC init, AC_fin = ',_coeur.nameAC[nom],_coeurp1.nameAC[nom]

                MACRO_AC_PERMUTE(
                    POS_INIT=_coeur.nameAC[nom],
                    POS_FIN=_coeurp1.nameAC[nom],
                    RESU_INI=RESUI,
                    RESU_FIN=BIDON,
                    MAILLAGE_INIT=_MA_N,
                    INSTANT=last_i,
                    MAILLAGE_FINAL=_MA_NP1,
                    MODELE_FINAL=_MO_NP1,
                    TRAN=(tran_x, tran_y, tran_z))
                UTMESS('I', 'COEUR0_3', valk=(_coeur.position_todamac(
                    _coeur.nameAC[nom]), _coeurp1.position_todamac(_coeurp1.nameAC[nom])))
                break
    UTMESS('I', 'COEUR0_2', vali=(indice))
