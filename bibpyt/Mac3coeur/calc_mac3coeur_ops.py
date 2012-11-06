#@ MODIF calc_mac3coeur_ops Mac3coeur  DATE 05/11/2012   AUTEUR FERNANDES R.FERNANDES 
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
from Cata.cata import modele_sdaster, maillage_sdaster
from mac3coeur_coeur import CoeurFactory

def calc_mac3coeur_ops(self, **args):
    """Corps principal de la macro MAC3COEUR"""
    import aster
    from Accas import _F
    from Utilitai.UniteAster import UniteAster
    from Utilitai.Utmess import  UTMESS

    STAT_NON_LINE    = self.get_cmd('STAT_NON_LINE')
    MODI_MAILLAGE    = self.get_cmd('MODI_MAILLAGE')
    AFFE_CHAR_MECA   = self.get_cmd('AFFE_CHAR_MECA')
    CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
    CREA_RESU        = self.get_cmd('CREA_RESU')
    AFFE_CHAR_CINE   = self.get_cmd('AFFE_CHAR_CINE')
    PERM_MAC3COEUR   = self.get_cmd('PERM_MAC3COEUR')

    self.set_icmd(1)
    datg = aster_core.get_option("repdex")
    coeur_factory = CoeurFactory(datg)

    _typ_coeur   = self['TYPE_COEUR']
    _LAME        = self['LAME']
    _DEFORMATION = self['DEFORMATION']

    _TAB_N       = self['TABLE_N']
    _tabn1       = _TAB_N.EXTR_TABLE()

    # ITER_GLOB_MAXI pour STAT_NON_LINE
    NITER=30

    # on recupere le nom du coeur
    name = _tabn1.para[0]

    # et on renomme la colonne qui identifie les assemblages
    _tabn1.Renomme(name, 'idAC')
    _coeur = coeur_factory.get(_typ_coeur)(name, _typ_coeur, self, datg)

    _coeur.init_from_table(_tabn1)

    _MA0  = self['MAILLAGE_N']
    if (_DEFORMATION!=None):

       _RESU_INI  = _DEFORMATION['RESU_INIT']
       if (_RESU_INI!=None):
          iret,ibid,nom_ma = aster.dismoi('F','NOM_MAILLA',_RESU_INI.nom,'RESULTAT')
          iret,ibid,nom_mo = aster.dismoi('F','NOM_MODELE',_RESU_INI.nom,'RESULTAT')

          if (_MA0!=None):
              UTMESS('A','COEUR0_1')
          _MA_N = self.get_concept_by_type(nom_ma, maillage_sdaster)
          _MO_N = self.get_concept_by_type(nom_mo, modele_sdaster)

       else:
          _MA_N = _coeur.affectation_maillage(_MA0)
          _MO_N = _coeur.affectation_modele(_MA_N)

       _GFF  = _coeur.definition_geom_fibre()
       _CARA = _coeur.definition_cara_coeur(_MO_N,_GFF)

       UL = UniteAster()
       _unit_eftx = _DEFORMATION['UNITE_THYC']
       nomfich=UL.Nom(_unit_eftx)

       _CH_TRNO,_CH_TRFX,_HYDR_1,_FOHYDR_1=_coeur.lire_resu_thyc(_MO_N,nomfich)

       _fluence   = _DEFORMATION['NIVE_FLUENCE']

       _AVEC_CONTACT = 'OUI'
       _SANS_CONTACT = 'NON'

       _time    = _coeur.definition_time(_fluence)
       _FLUENC  = _coeur.definition_fluence(_fluence,_MA_N)
       _CHTH    = _coeur.definition_champ_temperature(_MA_N)
       _DILAT   = _coeur.dilatation_cuve(_MO_N,_MA_N)
       _AF_MAC  = _coeur.definition_materiau(_MA_N,_GFF,_AVEC_CONTACT,_FLUENC,_CHTH)
       _AF_MSC  = _coeur.definition_materiau(_MA_N,_GFF,_SANS_CONTACT,_FLUENC,_CHTH)
       _PESANT  = _coeur.definition_pesanteur(_MO_N)
       _F_EMBO  = _coeur.definition_effor_maintien(_MO_N)
       _ARCH_1  = _coeur.definition_archimede1(_MO_N)
       _FOARCH_1= _coeur.definition_archimede2(_MO_N)
       _ARCH_F1 = _coeur.definition_temp_archimede()
       _HYDR_F1 = _coeur.definition_temp_hydro_axiale()
       _F_TRAN1 = _coeur.definition_effort_transverse()

       cl_liaison_solide = _coeur.cl_rigidite_grille()

       _CL_PER_1  = AFFE_CHAR_MECA( MODELE   = _MO_N,
                                 DDL_IMPO = ( _F(GROUP_MA = 'CRAYON',           DRX=0.,               ),
                                              _F(GROUP_NO = 'LISPG',            DRX=0., DRY=0., DRZ=0.),
                                              _F(GROUP_MA =('EBOSUP','EBOINF'), DRX=0., DRY=0., DRZ=0.),),
                                 LIAISON_GROUP = (_F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                     
                                                  _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                     
                                                  _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                 ),
                                 LIAISON_SOLIDE = cl_liaison_solide,
                                );

       self.DeclareOut('RESUC1',self.sd)

       if (_RESU_INI!=None):
          RESUC1 = STAT_NON_LINE(
                      MODELE      = _MO_N,
                      CHAM_MATER  = _AF_MAC,
                      CARA_ELEM   = _CARA,
                      EXCIT       =(
                                    _F(CHARGE = _ARCH_1,   FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _FOARCH_1, FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _HYDR_1,   FONC_MULT = _HYDR_F1,),
                                    _F(CHARGE = _FOHYDR_1, FONC_MULT = _HYDR_F1,),
                                    _F(CHARGE = _CH_TRNO,  FONC_MULT = _F_TRAN1,),
                                    _F(CHARGE = _CH_TRFX,  FONC_MULT = _F_TRAN1,),
                                    _F(CHARGE = _F_EMBO,  ),
                                    _F(CHARGE = _DILAT,   ),
                                    _F(CHARGE = _CL_PER_1,),
                                    _F(CHARGE = _PESANT,),),
                      ETAT_INIT = _F(EVOL_NOLI= _RESU_INI,),AFFICHAGE=_F(INFO_RESIDU='OUI',),
                      COMP_INCR   =(
                                    _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, DEFORMATION = 'GROT_GDEP', ),
                                    _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                    _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                    _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),
                                    _F(RELATION='VMIS_ISOT_TRAC',GROUP_MA ='MAINTIEN',DEFORMATION='PETIT',),),
                      INCREMENT   = _F(LIST_INST = _time, INST_FIN=_coeur.temps_simu['T8']),
                      NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                      SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',GESTION_MEMOIRE='OUT_OF_CORE',ELIM_LAGR2='NON',PCENT_PIVOT=200,),
                      CONVERGENCE=_F(ITER_GLOB_MAXI=NITER),
                                      );
       else:
          RESUC1 = STAT_NON_LINE(
                      MODELE      = _MO_N,
                      CHAM_MATER  = _AF_MAC,
                      CARA_ELEM   = _CARA,
                      EXCIT       =(
                                    _F(CHARGE = _ARCH_1,   FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _FOARCH_1, FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _HYDR_1,   FONC_MULT = _HYDR_F1,),
                                    _F(CHARGE = _FOHYDR_1, FONC_MULT = _HYDR_F1,),
                                    _F(CHARGE = _CH_TRNO,  FONC_MULT = _F_TRAN1,),
                                    _F(CHARGE = _CH_TRFX,  FONC_MULT = _F_TRAN1,),
                                    _F(CHARGE = _F_EMBO,  ),
                                    _F(CHARGE = _DILAT,   ),
                                    _F(CHARGE = _CL_PER_1,),
                                    _F(CHARGE = _PESANT,),),AFFICHAGE=_F(INFO_RESIDU='OUI',),
                      COMP_INCR   =(
                                    _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, DEFORMATION = 'GROT_GDEP', ),
                                    _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                    _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                    _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),
                                    _F(RELATION='VMIS_ISOT_TRAC',GROUP_MA ='MAINTIEN',DEFORMATION='PETIT',),),
                      INCREMENT   = _F(LIST_INST = _time, INST_FIN=_coeur.temps_simu['T8']),
                      NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                      SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',GESTION_MEMOIRE='OUT_OF_CORE',ELIM_LAGR2='NON',PCENT_PIVOT=200,),
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
                                    _F(CHARGE = _F_EMBO,  ),
                                    _F(CHARGE = _DILAT,   ),
                                    _F(CHARGE = _CL_PER_1,),
                                    _F(CHARGE = _PESANT,),),AFFICHAGE=_F(INFO_RESIDU='OUI',),
                      COMP_INCR   =(
                                    _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, DEFORMATION = 'GROT_GDEP', ),
                                    _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                    _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                    _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),
                                    _F(RELATION='VMIS_ISOT_TRAC',GROUP_MA ='MAINTIEN',DEFORMATION='PETIT',),),
                      INCREMENT   = _F(LIST_INST = _time, INST_FIN=_coeur.temps_simu['T8b']),
                      NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                      SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',GESTION_MEMOIRE='OUT_OF_CORE',ELIM_LAGR2='NON',PCENT_PIVOT=200,),
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
                                    _F(CHARGE = _DILAT,   ),
                                    _F(CHARGE = _CL_PER_1,),
                                    _F(CHARGE = _PESANT,),),AFFICHAGE=_F(INFO_RESIDU='OUI',),
                      COMP_INCR   =(
                                    _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, DEFORMATION = 'GROT_GDEP', ),
                                    _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                    _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                    _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),
                                    _F(RELATION='VMIS_ISOT_TRAC',GROUP_MA ='MAINTIEN',DEFORMATION='PETIT',),),
                      INCREMENT   = _F(LIST_INST = _time),
                      NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                      SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',GESTION_MEMOIRE='OUT_OF_CORE',ELIM_LAGR2='NON',PCENT_PIVOT=200,),
                      CONVERGENCE=_F(ITER_GLOB_MAXI=NITER
                                     #RESI_GLOB_MAXI=1e-10,
                                     ),
                       );



    elif (_LAME!=None):

       _fluence = 0.0
       _AVEC_CONTACT = 'OUI'
       _SANS_CONTACT = 'NON'


       _MA_N = _coeur.affectation_maillage(_MA0)
       _MO_N = _coeur.affectation_modele(_MA_N)
       _GFF  = _coeur.definition_geom_fibre()
       _CARA = _coeur.definition_cara_coeur(_MO_N,_GFF)
       _time    = _coeur.definition_time(_fluence)
       _FLUENC  = _coeur.definition_fluence(_fluence,_MA_N)
       _CHTH    = _coeur.definition_champ_temperature(_MA_N)
       _DILAT   = _coeur.dilatation_cuve(_MO_N,_MA_N)
       _AF_MSC  = _coeur.definition_materiau(_MA_N,_GFF,_SANS_CONTACT,_FLUENC,_CHTH)
       _PESANT  = _coeur.definition_pesanteur(_MO_N)
       _F_EMBO  = _coeur.definition_effor_maintien(_MO_N)
       _ARCH_1  = _coeur.definition_archimede1(_MO_N)
       _FOARCH_1= _coeur.definition_archimede2(_MO_N)
       _ARCH_F1 = _coeur.definition_temp_archimede()

       _CL_LAME = _coeur.affe_char_lame(_MO_N)

       _CL_PER_2  = AFFE_CHAR_MECA( MODELE   = _MO_N,
                                 LIAISON_GROUP = (_F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                     
                                                  _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                     
                                                  _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                 ),
                                );

       # calcul de deformation d'apres DAMAC
       _SNL_LAME = STAT_NON_LINE( MODELE  = _MO_N,
                              CHAM_MATER  = _AF_MSC,
                              CARA_ELEM   = _CARA,
                              EXCIT       = (
                                    _F(CHARGE = _ARCH_1,   FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _FOARCH_1, FONC_MULT = _ARCH_F1,),
                                    _F(CHARGE = _F_EMBO,  ),
                                    _F(CHARGE = _DILAT,   ),
                                    _F(CHARGE = _PESANT,  ),
                    _F(CHARGE = _CL_LAME, ),
                    _F(CHARGE = _CL_PER_2,),),
                              COMP_INCR   =(
                                    _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, DEFORMATION = 'GROT_GDEP', ),
                                    _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                    _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                    _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),
                                    _F(RELATION='VMIS_ISOT_TRAC',GROUP_MA ='MAINTIEN',DEFORMATION='PETIT',),),
                              INCREMENT   = _F(LIST_INST = _time, INST_FIN = _coeur.temps_simu['T1'],),
                              NEWTON      = _F(MATRICE='TANGENTE', REAC_ITER=1,),
                              SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',GESTION_MEMOIRE='OUT_OF_CORE',ELIM_LAGR2='NON',PCENT_PIVOT=80,),
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

       _MA1     = _LAME['MAILLAGE_NP1']

       __resuf   = PERM_MAC3COEUR( TYPE_COEUR   = _typ_coeur,
                                  RESU_N       = _SNL_LAME,
                                  TABLE_N      = _TAB_N,
                                  TABLE_NP1    = _TAB_NP1,
                                  MAILLAGE_NP1 = _MA1,)

       _MVDEPL = CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='NOEU_DEPL_R', NOM_CHAM ='DEPL', RESULTAT = __resuf)
       
       iret,ibid,nom_ma = aster.dismoi('F','NOM_MAILLA',__resuf.nom,'RESULTAT')
       nom_ma = nom_ma.strip()
       iret,ibid,nom_mo = aster.dismoi('F','NOM_MODELE',__resuf.nom,'RESULTAT')
       nom_mo = nom_mo.strip()

       _MO_NP1 = self.get_concept_by_type(nom_mo, modele_sdaster)
       _MA_NP1 = self.get_concept_by_type(nom_ma, maillage_sdaster)

       _GFF_NP1  = _coeurp1.definition_geom_fibre()
       _CARANP1  = _coeurp1.definition_cara_coeur(_MO_NP1,_GFF_NP1)

       _timep1   = _coeurp1.definition_time(_fluence)
       _FLU_NP1  = _coeurp1.definition_fluence(_fluence,_MA_NP1)
       _CHTHNP1  = _coeurp1.definition_champ_temperature(_MA_NP1)
       _DILATP1  = _coeurp1.dilatation_cuve(_MO_NP1,_MA_NP1)
       _AFACNP1  = _coeurp1.definition_materiau(_MA_NP1,_GFF_NP1,_AVEC_CONTACT,_FLU_NP1,_CHTHNP1)
       _AFSCNP1  = _coeurp1.definition_materiau(_MA_NP1,_GFF_NP1,_SANS_CONTACT,_FLU_NP1,_CHTHNP1)
       
       _PESANT1  = _coeurp1.definition_pesanteur(_MO_NP1)
       _F_EMBO1  = _coeurp1.definition_effor_maintien(_MO_NP1)
       _ARCH_11  = _coeurp1.definition_archimede1(_MO_NP1)
       _FOARCH1  = _coeurp1.definition_archimede2(_MO_NP1)
       _ARCHF11  = _coeurp1.definition_temp_archimede()

       _MA_NP1 = MODI_MAILLAGE( reuse = _MA_NP1, MAILLAGE = _MA_NP1, DEFORME = _F( OPTION = 'TRAN', DEPL = _MVDEPL))

       cl_liaison_solide = _coeurp1.cl_rigidite_grille()

       _BLOC2  = AFFE_CHAR_MECA( MODELE   = _MO_NP1,
                                 DDL_IMPO = ( _F(GROUP_MA = 'CRAYON',           DRX=0.,               ),
                                              _F(GROUP_NO = 'LISPG',            DRX=0., DRY=0., DRZ=0.),
                                              _F(GROUP_MA =('EBOSUP','EBOINF'), DRX=0., DRY=0., DRZ=0.),),
                                 LIAISON_GROUP = (_F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PMNT_S', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                     
                                                  _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PSUP', GROUP_NO_2='PEBO_S',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                     
                                                  _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                     DDL_1='DY', DDL_2='DY', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                  _F(GROUP_NO_1='PINF', GROUP_NO_2='FIX',SOMMET='OUI',
                                                     DDL_1='DZ', DDL_2='DZ', COEF_MULT_1=1., COEF_MULT_2=-1., COEF_IMPO=0.,),
                                                 ),
                                 LIAISON_SOLIDE = cl_liaison_solide,
                                );

       self.DeclareOut('RESUJ',self.sd)
       RESUJ = STAT_NON_LINE( MODELE      = _MO_NP1,
                              CHAM_MATER  = _AFACNP1,
                              CARA_ELEM   = _CARANP1,
                              EXCIT   =(
                                    _F(CHARGE = _ARCH_11,  FONC_MULT = _ARCHF11,),
                                    _F(CHARGE = _FOARCH1,  FONC_MULT = _ARCHF11,),
                                    _F(CHARGE = _F_EMBO1,  ),
                                    _F(CHARGE = _PESANT1,  ),
                                    _F(CHARGE = _DILATP1,  ),
                    _F(CHARGE = _BLOC2,),),
                              COMP_INCR   =(
                                    _F(RELATION='MULTIFIBRE', GROUP_MA =('CRAYON','T_GUIDE'), PARM_THETA=0.5, DEFORMATION = 'GROT_GDEP', ),
                                    _F(RELATION='DIS_GRICRA', GROUP_MA = 'ELA',),
                                    _F(RELATION='DIS_CHOC',   GROUP_MA =('RES_EXT','RES_CONT'),),
                                    _F(RELATION='ELAS',       GROUP_MA =('EBOINF','EBOSUP','RIG','DIL',),),
                                    _F(RELATION='VMIS_ISOT_TRAC',GROUP_MA ='MAINTIEN',DEFORMATION='PETIT',),),
                              INCREMENT   = _F(LIST_INST = _timep1, INST_FIN = _coeurp1.temps_simu['T4'],),
                              NEWTON      = _F(MATRICE='TANGENTE',REAC_ITER=1,),
                              SOLVEUR     = _F(METHODE='MUMPS',RENUM='AMF',GESTION_MEMOIRE='OUT_OF_CORE',ELIM_LAGR2='NON',PCENT_PIVOT=200,),
                              CONVERGENCE=_F(ITER_GLOB_MAXI=NITER),
                              );

