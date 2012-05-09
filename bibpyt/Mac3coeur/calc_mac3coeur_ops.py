#@ MODIF calc_mac3coeur_ops Mac3coeur  DATE 10/05/2012   AUTEUR CHEIGNON E.CHEIGNON 
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
# RESPONSABLE FERNANDES R.FERNANDES

import os.path as osp
import string
from pprint import pformat

import aster_core
from Cata.cata import MACRO, SIMP, table_sdaster
from mac3coeur_coeur import CoeurFactory

def calc_mac3coeur_ops(self, **args):
    """Corps principal de la macro MAC3COEUR"""
    import aster
    from Accas import _F
    from mac3coeur_ac_permute import MACRO_AC_PERMUTE
    from Utilitai.UniteAster import UniteAster

    STAT_NON_LINE    = self.get_cmd('STAT_NON_LINE')
    MODI_MAILLAGE    = self.get_cmd('MODI_MAILLAGE')
    AFFE_CHAR_MECA   = self.get_cmd('AFFE_CHAR_MECA')
    CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
    CREA_RESU        = self.get_cmd('CREA_RESU')
    AFFE_CHAR_CINE   = self.get_cmd('AFFE_CHAR_CINE')
    DETRUIRE         = self.get_cmd('DETRUIRE')

    self.set_icmd(1)
    datg = aster_core.get_option("repdex")
    coeur_factory = CoeurFactory(datg)

    _typ_coeur   = self['TYPE_COEUR']
    _LAME        = self['LAME']
    _DEFORMATION = self['DEFORMATION']

    _TAB_N       = self['TABLE_N']
    _tabn1       = _TAB_N.EXTR_TABLE()

    # ITER_GLOB_MAXI pour STAT_NON_LINE
    NITER=20

    # on recupere le nom du coeur
    name = _tabn1.para[0]

    # et on renomme la colonne qui identifie les assemblages
    _tabn1.Renomme(name, 'idAC')
    _coeur = coeur_factory.get(_typ_coeur)(name, _typ_coeur, self, datg)

    _coeur.init_from_table(_tabn1)

    _MA0  = self['MAILLAGE_N']
    _MA_N = _coeur.affectation_maillage(_MA0)
    _MO_N = _coeur.affectation_modele(_MA_N)
    _GFF  = _coeur.definition_geom_fibre()
    _CARA = _coeur.definition_cara_coeur(_MO_N,_GFF)

    if (_DEFORMATION!=None):

       UL = UniteAster()
       _unit_eftx = _DEFORMATION['UNITE_THYC']
       nomfich=UL.Nom(_unit_eftx)
       # 1er cycle

       _CH_TRNO,_CH_TRFX,_HYDR_1,_FOHYDR_1=_coeur.lire_resu_thyc(_MO_N,nomfich)

       _fluence   = _DEFORMATION['NIVE_FLUENCE']

       _AVEC_CONTACT = 'OUI'
       _SANS_CONTACT = 'NON'

       print 'DEFINITION DE LA DISCRETISATION TEMPORELLE'
       _time    = _coeur.definition_time(_fluence)
       print 'DEFINITION DU CHAMP DE FLUENCE'
       _FLUENC  = _coeur.definition_fluence(_fluence,_MA_N)
       print 'DEFINITION DU CHAMP DE TEMPERATURE'
       _CHTH    = _coeur.definition_champ_temperature(_MA_N)
       print 'DEFINITION DES MATERIAUX'
       _AF_MAC  = _coeur.definition_materiau(_MA_N,_GFF,_AVEC_CONTACT,_FLUENC,_CHTH)
       _AF_MSC  = _coeur.definition_materiau(_MA_N,_GFF,_SANS_CONTACT,_FLUENC,_CHTH)
       print 'DEFINITION DU CHAMP PESANTEUR'
       _PESANT  = _coeur.definition_pesanteur(_MO_N)
       print 'DEFINITION DES EFFORTS DE MAINTIEN'
       _F_EBS   = _coeur.definition_effor_maintien()
       print 'DEFINITION DU CHAMP DE POUSSE D ARCHIMEDE'
       _ARCH_1  = _coeur.definition_archimede1(_MO_N)
       _FOARCH_1= _coeur.definition_archimede2(_MO_N)
       _ARCH_F1 = _coeur.definition_temp_archimede()
       print 'DEFINITION DES CHARGEMENTS HYDRAULIQUES'
       _HYDR_F1 = _coeur.definition_temp_hydro_axiale()
       _F_TRAN1 = _coeur.definition_effort_transverse()

       cl_liaison_solide = _coeur.cl_rigidite_grille()

       _CL_PER_1  = AFFE_CHAR_MECA( MODELE   = _MO_N,
                                 DDL_IMPO = ( _F(GROUP_MA = 'CRAYON',           DRX=0.,               ),
                                              _F(GROUP_NO = 'LISPG',            DRX=0., DRY=0., DRZ=0.),
                                              _F(GROUP_NO = 'FIX',              DX=0.,  DY=0.,  DZ=0.),
                                              _F(GROUP_MA =('EBOSUP','EBOINF'),         DY=0.,  DZ=0.,
                                                                                DRX=0., DRY=0., DRZ=0.),
                                              _F(GROUP_NO = 'P_CUV',            DX=0.,  DY=0.,  DZ=0. )),
                                 LIAISON_SOLIDE = cl_liaison_solide,
                                );

       _F_EMBO = AFFE_CHAR_MECA( MODELE = _MO_N, FORCE_NODALE = _F(GROUP_NO='PEBO_S',FX=(-1.),),);

       self.DeclareOut('RESUC1',self.sd)

       RESUC1 = STAT_NON_LINE(
                      MODELE      = _MO_N,
                      CHAM_MATER  = _AF_MAC,
                      CARA_ELEM   = _CARA,
                      EXCIT       =(
                                    _F(CHARGE = _ARCH_1,   FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _FOARCH_1, FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _HYDR_1,   FONC_MULT = _HYDR_F1,),
                                    _F(CHARGE = _FOHYDR_1, FONC_MULT = _HYDR_F1,),
                                    _F(CHARGE = _F_EMBO,   FONC_MULT = _F_EBS,  ),
                                    _F(CHARGE = _CH_TRNO,  FONC_MULT = _F_TRAN1,),
                                    _F(CHARGE = _CH_TRFX,  FONC_MULT = _F_TRAN1,),
                                    _F(CHARGE = _CL_PER_1,),
                                    _F(CHARGE = _PESANT,),),
                      COMP_INCR   =(
                                    _F(RELATION='ELAS',       TOUT     = 'OUI',),
                                    _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, ),
                                    _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                    _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                    _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),),
                      INCREMENT   = _F(LIST_INST = _time, INST_FIN=_coeur.temps_simu['T8']),
                      NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                      SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',OUT_OF_CORE='OUI',ELIM_LAGR2='NON',PCENT_PIVOT=200,),
                      SUIVI_DDL   = (_F(NOM_CHAM  = 'DEPL',
                                      EVAL_CHAM = 'MIN',
                                      NOM_CMP   = ('DX'),
                                      GROUP_NO  = ('CRAYON'))),
                      CONVERGENCE=_F(ITER_GLOB_MAXI=NITER),
                                      );

       RESUC1 = STAT_NON_LINE( reuse = RESUC1,
                      MODELE      = _MO_N,
                      CHAM_MATER  = _AF_MSC,
                      CARA_ELEM   = _CARA,
                      ETAT_INIT   = _F(EVOL_NOLI=RESUC1),
                      EXCIT       =(
                                    _F(CHARGE = _ARCH_1,   FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _FOARCH_1, FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _HYDR_1,   FONC_MULT = _HYDR_F1,),
                                    _F(CHARGE = _FOHYDR_1, FONC_MULT = _HYDR_F1,),
                                    _F(CHARGE = _F_EMBO,   FONC_MULT = _F_EBS,  ),
                                    _F(CHARGE = _CH_TRNO,  FONC_MULT = _F_TRAN1,),
                                    _F(CHARGE = _CH_TRFX,  FONC_MULT = _F_TRAN1,),
                                    _F(CHARGE = _CL_PER_1,),
                                    _F(CHARGE = _PESANT,),),
                      COMP_INCR   =(
                                    _F(RELATION='ELAS',       TOUT     = 'OUI',),
                                    _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, ),
                                    _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                    _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                    _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),),
                      INCREMENT   = _F(LIST_INST = _time),
                      NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                      SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',OUT_OF_CORE='OUI',ELIM_LAGR2='NON',PCENT_PIVOT=200,),
                      SUIVI_DDL   = (_F(NOM_CHAM  = 'DEPL',
                                        EVAL_CHAM = 'MIN',
                                        NOM_CMP   = ('DX'),
                                        GROUP_NO  = ('CRAYON'),)
                       ),
                       CONVERGENCE=_F(ITER_GLOB_MAXI=NITER),
                       );



    elif (_LAME!=None):

       _fluence = 0.0
       _AVEC_CONTACT = 'OUI'
       _SANS_CONTACT = 'NON'

       _time    = _coeur.definition_time(_fluence)
       _FLUENC  = _coeur.definition_fluence(_fluence,_MA_N)
       _CHTH    = _coeur.definition_champ_temperature(_MA_N)
       _AF_MAC  = _coeur.definition_materiau(_MA_N,_GFF,_AVEC_CONTACT,_FLUENC,_CHTH)
       _AF_MSC  = _coeur.definition_materiau(_MA_N,_GFF,_SANS_CONTACT,_FLUENC,_CHTH)
       _PESANT  = _coeur.definition_pesanteur(_MO_N)
       _F_EBS   = _coeur.definition_effor_maintien()
       _ARCH_1  = _coeur.definition_archimede1(_MO_N)
       _FOARCH_1= _coeur.definition_archimede2(_MO_N)
       _ARCH_F1 = _coeur.definition_temp_archimede()
       _HYDR_F1 = _coeur.definition_temp_hydro_axiale()
       _F_TRAN1 = _coeur.definition_effort_transverse()

       _CL_LAME = _coeur.affe_char_lame(_MO_N)

       _SNL_LAME = STAT_NON_LINE( MODELE  = _MO_N,
                              CHAM_MATER  = _AF_MSC,
                              CARA_ELEM   = _CARA,
                              EXCIT       = (_F(CHARGE = _CL_LAME,),),
                              COMP_INCR   = (_F(RELATION='ELAS',       TOUT     = 'OUI',),
                                             _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, ),
                                             _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                             _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                             _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG',),),),
                              INCREMENT   = _F(LIST_INST = _time, INST_FIN = _coeur.temps_simu['T1'],),
                              NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                              SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',OUT_OF_CORE='OUI',ELIM_LAGR2='NON',PCENT_PIVOT=80,),
                              CONVERGENCE=_F(ITER_GLOB_MAXI=NITER),
                              )

       _TAB_NP1   = _LAME['TABLE_NP1']
       _tabp1     = _TAB_NP1.EXTR_TABLE()

       # on recupere le nom du coeur
       namep1 = _tabp1.para[0]

       # et on renomme la colonne qui identifie les assemblages
       _tabp1.Renomme(namep1, 'idAC')
       _coeurp1 = coeur_factory.get(_typ_coeur)(namep1, _typ_coeur, self, datg)
       _coeurp1.init_from_table(_tabp1)


       _MA1      = _LAME['MAILLAGE_NP1']
       _MA_NP1   = _coeurp1.affectation_maillage(_MA1)
       _MO_NP1   = _coeurp1.affectation_modele(_MA_NP1)
       _GFF_NP1  = _coeurp1.definition_geom_fibre()
       _CARANP1  = _coeurp1.definition_cara_coeur(_MO_NP1,_GFF_NP1)

       _timep1   = _coeurp1.definition_time(_fluence)
       _FLU_NP1  = _coeurp1.definition_fluence(_fluence,_MA_NP1)
       _CHTHNP1  = _coeurp1.definition_champ_temperature(_MA_NP1)
       _AFACNP1  = _coeurp1.definition_materiau(_MA_NP1,_GFF_NP1,_AVEC_CONTACT,_FLU_NP1,_CHTHNP1)
       _AFSCNP1  = _coeurp1.definition_materiau(_MA_NP1,_GFF_NP1,_SANS_CONTACT,_FLU_NP1,_CHTHNP1)

       _CL_BID = AFFE_CHAR_CINE(MODELE=_MO_NP1,MECA_IMPO = (_F(TOUT = 'OUI', DX = 0.0, DY = 0.0, DZ = 0.0, DRX = 0.0, DRY = 0.0, DRZ = 0.0,),))

       tran_x = 0.0
       COE    = [None]*_coeurp1.nbac
       DEP    = [None]*_coeurp1.nbac
       VAR    = [None]*_coeurp1.nbac
       lisdep = []
       lisvar = []
       lisdet = []

       indice = 0

       _BIDON = STAT_NON_LINE( MODELE     = _MO_NP1,
                              CHAM_MATER  = _AFSCNP1,
                              CARA_ELEM   = _CARANP1,
                              EXCIT       = (_F(CHARGE = _CL_BID,),),
                              COMP_INCR   = (_F(RELATION='ELAS',       TOUT     = 'OUI',),
                                             _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, ),
                                             _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                             _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                             _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG',),),),
                              INCREMENT   = _F(LIST_INST = _timep1, NUME_INST_FIN = 1,),
                              NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                              SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',OUT_OF_CORE='OUI',ELIM_LAGR2='NON',PCENT_PIVOT=80,),
                              CONVERGENCE=_F(ITER_GLOB_MAXI=NITER),
                              )

       for nom in _coeurp1.nameAC.keys() :
           if nom in _coeur.nameAC :
              tran_z = _coeurp1.pas_assemblage * (_coeurp1.ALPHAMAC.index(_coeurp1.nameAC[nom][0]) - _coeurp1.ALPHAMAC.index(_coeur.nameAC[nom][0]))
              tran_y = _coeurp1.pas_assemblage * (_coeurp1.ALPHAMAC.index(_coeurp1.nameAC[nom][2]) - _coeurp1.ALPHAMAC.index(_coeur.nameAC[nom][2]))

              COE[indice]  = MACRO_AC_PERMUTE(POS_INIT       = _coeur.nameAC[nom],
                              POS_FIN        = _coeurp1.nameAC[nom],
                              RESU_INI       = _SNL_LAME,
                              RESU_FIN       = _BIDON,
                              MAILLAGE_INIT  = _MA_N,
                              INSTANT        = _coeur.temps_simu['T1'],
                              MAILLAGE_FINAL = _MA_NP1,
                              MODELE_FINAL   = _MO_NP1,
                              TRAN       = (tran_x,tran_y,tran_z))

              DEP[indice] = CREA_CHAMP(TYPE_CHAM = 'NOEU_DEPL_R',
                           OPERATION = 'EXTR',
                           PRECISION =  1.0E-10,
                           RESULTAT  =  COE[indice],
                           NOM_CHAM  = 'DEPL',
                           INST  =  0.0,);

              VAR[indice] = CREA_CHAMP(TYPE_CHAM = 'ELGA_VARI_R',
                           OPERATION = 'EXTR',
                           PRECISION =  1.0E-10,
                           RESULTAT  =  COE[indice],
                           NOM_CHAM  = 'VARI_ELGA',
                           INST  =  0.0,);

              mtdep = (_F(TOUT = 'OUI', CHAM_GD = DEP[indice], CUMUL = 'OUI', COEF_R = 1.0),)
              mtvar = (_F(TOUT = 'OUI', CHAM_GD = VAR[indice], CUMUL = 'OUI', COEF_R = 1.0),)
              lisdep.extend(mtdep)
              lisvar.extend(mtvar)

              mtdet={}
              mtdet["NOM"] =  (COE[indice],DEP[indice],VAR[indice])
              lisdet.append(mtdet)

              indice = indice + 1

       print 'ASSEMBLAGE DES',indice,'PERMUTATIONS'

       _RES_DEP = CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',MODELE=_MO_NP1,OPERATION='ASSE',ASSE=lisdep)
       _RES_VAR = CREA_CHAMP(TYPE_CHAM='ELGA_VARI_R',MODELE=_MO_NP1,OPERATION='ASSE',ASSE=lisvar)

       _RES_SIG = CREA_CHAMP(TYPE_CHAM = 'ELGA_SIEF_R',
                     OPERATION = 'EXTR',
                     PRECISION =  1.0E-10,
                     RESULTAT  =  COE[0],
                     NOM_CHAM  = 'SIEF_ELGA',
                     INST      =  0.0,);

       _RESU_F = CREA_RESU( OPERATION = 'AFFE',
                    TYPE_RESU = 'EVOL_NOLI',
                    NOM_CHAM  = 'DEPL',
                    AFFE      = _F(CHAM_GD=_RES_DEP,INST=0.0,MODELE=_MO_NP1,));

       _RESU_F = CREA_RESU( reuse     = _RESU_F,
                    OPERATION = 'AFFE',
                    TYPE_RESU = 'EVOL_NOLI',
                    NOM_CHAM  = 'SIEF_ELGA',
                    AFFE      = _F(CHAM_GD=_RES_SIG,INST=0.0,MODELE=_MO_NP1,));

       _RESU_F = CREA_RESU( reuse     = _RESU_F,
                    OPERATION = 'AFFE',
                    TYPE_RESU = 'EVOL_NOLI',
                    NOM_CHAM  = 'VARI_ELGA',
                    AFFE      = _F(CHAM_GD=_RES_VAR,INST=0.0,MODELE=_MO_NP1,));

       DETRUIRE(CONCEPT=tuple(lisdet))

       _MVDEPL = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM ='DEPL', RESULTAT = _RESU_F)

       _MA_NP1 = MODI_MAILLAGE( reuse = _MA_NP1, MAILLAGE = _MA_NP1, DEFORME = _F( OPTION = 'TRAN', DEPL = _MVDEPL))

       cl_liaison_solide = _coeurp1.cl_rigidite_grille()

       _BLOC2  = AFFE_CHAR_MECA( MODELE   = _MO_NP1,
                                 DDL_IMPO = ( _F(GROUP_MA = 'CRAYON',           DRX=0.,               ),
                                              _F(GROUP_NO = 'LISPG',            DRX=0., DRY=0., DRZ=0.),
                                              _F(GROUP_MA =('EBOSUP','EBOINF'), DX=0.,  DY=0.,  DZ=0.,
                                                                                DRX=0., DRY=0., DRZ=0.),
                                              _F(GROUP_NO = 'P_CUV',            DX=0.,  DY=0.,  DZ=0. )),
                                 LIAISON_SOLIDE = cl_liaison_solide,
                                );

       self.DeclareOut('RESUJ',self.sd)
       RESUJ = STAT_NON_LINE( MODELE      = _MO_NP1,
                                CHAM_MATER  = _AFACNP1,
                                CARA_ELEM   = _CARANP1,
                                EXCIT       =(_F(CHARGE = _BLOC2,),),
                                COMP_INCR   =(_F(RELATION='ELAS', TOUT = 'OUI',),
                                              _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, ),
                                              _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                              _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                              _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG',),),),
                                INCREMENT   = _F(LIST_INST = _timep1, INST_FIN = _coeurp1.temps_simu['T1'],),
                                NEWTON      = _F(MATRICE='TANGENTE',REAC_ITER=1,),
                                SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',OUT_OF_CORE='OUI',ELIM_LAGR2='NON',PCENT_PIVOT=200,),
                                CONVERGENCE=_F(ITER_GLOB_MAXI=NITER),
                                );

