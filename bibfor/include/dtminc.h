!
! COPYRIGHT (C) 1991 - 2015  EDF R&D  WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! -------------------------------------------------------------------------
! Parameters definitions
! -------------------------------------------------------------------------
!
    integer :: parind(_DTM_NBPAR)
    character(len=3)  :: partyp(_DTM_NBPAR)
    character(len=8)  :: params(_DTM_NBPAR)
    
    data  params /'AMOR_MAT', 'APPND_SD', 'BASE_MOD', 'CALC_SD ', 'FV_NUMB ',&
                  'FX_NUMB ', 'MASS_MAT', 'NB_ANTSI', 'NB_CHOC ', 'NB_DISVI',&
                  'NB_EXC_T', 'NB_FLAMB', 'NB_MODES', 'NB_NONLI', 'NB_PALIE',&
                  'NB_PHYEQ', 'NB_R_FIS', 'NUM_DDL ', 'RESU_SD ', 'RIGI_MAT',&
                  'SCHEMA  ', 'SUB_STRU', 'TYP_BASE', 'V_ROT'   , 'ARCH_NB ',&
                  'DT'      , 'NB_STEPS', 'ARCH_PER', 'DT_EDYOS', 'DT_MAX  ',&
                  'DT_MIN  ', 'INST_INI', 'INST_FIN', 'V_ROT_F ', 'A_ROT_F ',&
                  'MULTI_AP', 'ANGL_INI', 'ANGL_FON', 'NB_MOD_D', 'ADAPT   ',&
                  'NL_CASE ', 'NL_TREAT',&

                  'IND_ALOC', 'AMOR_DIA', 'AMOR_FUL', 'BASE_VEC', 'CHO_DEPL',&
                  'CHO_NAME', 'CHO_NOEU', 'CHO_PARA', 'CHO_RANK', 'COEF_MLT',&
                  'DESC_FRC', 'FIMPO   ', 'FUNC_NAM', 'FV_DEPLR', 'FV_FONCT',&
                  'FX_DEPLR', 'FX_FONCT', 'GYRO_FUL', 'INDIC   ', 'MASS_DIA',&
                  'MASS_FUL', 'N_ORD_VC', 'OMEGA   ', 'OMEGA_SQ', 'PSI_DELT',&
                  'PTCHOC  ', 'RFIMPO  ', 'RIGI_DIA', 'RIGI_FUL', 'RIGY_FUL',&
                  'ROTR_DFK', 'ROTR_FK ', 'SOLVER  ', 'SOUPL   ', 'TRLOC   ',&
                  'VECT_GEN', 'VECT_RES', 'AR_LINST', 'AR_LINDI', 'MAT_DESC',&
                  'ARCH_STO', 'ACC_WORK', 'ADRES_VC', 'NL_SAVES', 'FX_SREDR',&
                  'FX_SREDI', 'FV_SREVR', 'FV_SREVI', 'MASS_FAC', 'NL_SAVE0',&
                  'NL_SAVE1', 'F_TOT_WK', 'F_NL_ADD', 'NL_DEPL0', 'NL_VITE0',&
                  'NL_ACCE0', 'F_TAN_WK', 'PAL_TYP ', 'PAL_FIN ', 'PAL_CN  ',&
                  'PAL_FSAV', 'PAL_NBCV', 'IMP_DEPL', 'IMP_VITE', 'IMP_ACCE',&
                  'IMP_FEXT', &
                  'SCHEMA_I', 'IARCH_SD',&

                  'FSI_BASE', 'FSI_TYPF', 'FSI_IVIT', 'FSI_CASE', 'FSI_VGAP',&
                  'FSI_CPLD', 'FSI_IRES', 'FSI_PRVI', 'FSI_RHOE', 'FSI_BASF',&
                  'FSI_PHIE', 'FSI_ABSC', 'FSI_CODM', 'FSI_POID', 'FSI_NUOR',&
                  'FSI_ITYP', 'FSI_ZET0'/


    data  partyp /'K24', 'I  ', 'K24', 'K24', 'I  ',&
                  'I  ', 'K24', 'I  ', 'I  ', 'I  ',&
                  'I  ', 'I  ', 'I  ', 'I  ', 'I  ',&
                  'I  ', 'I  ', 'K24', 'K24', 'K24',&
                  'K24', 'I  ', 'K24', 'R  ', 'I  ',&
                  'R  ', 'I  ', 'I  ', 'R  ', 'R  ',&
                  'R  ', 'R  ', 'R  ', 'K24', 'K24',&
                  'K24', 'R  ', 'K24', 'I  ', 'I  ',&
                  'I  ', 'I  ',&

                  'I  ', 'R  ', 'R  ', 'R  ', 'R  ',&
                  'K8 ', 'K8 ', 'R  ', 'I  ', 'R  ',&
                  'I  ', 'K24', 'K8 ', 'R  ', 'K8 ',&
                  'R  ', 'K8 ', 'R  ', 'I  ', 'R  ',&
                  'R  ', 'I  ', 'R  ', 'R  ', 'R  ',&
                  'K24', 'R  ', 'R  ', 'R  ', 'R  ',&
                  'K8 ', 'K8 ', 'K24', 'K24', 'R  ',&
                  'K8 ', 'K8 ', 'R  ', 'I  ', 'I  ',&
                  'I  ', 'R  ', 'I  ', 'R  ', 'R  ',&
                  'I  ', 'R  ', 'I  ', 'R  ', 'R  ',&
                  'R  ', 'R  ', 'R  ', 'R  ', 'R  ',&
                  'R  ', 'R  ', 'K8 ', 'K8 ', 'K8 ',&
                  'R  ', 'I  ', 'R  ', 'R  ', 'R  ',&
                  'R  ',&
                  'I  ', 'I  ',&

                  'K24', 'K24', 'I  ', 'I  ', 'R  ',&
                  'I  ', 'I  ', 'R  ', 'R  ', 'R  ',&
                  'R  ', 'R  ', 'R  ', 'R  ', 'I  ',&
                  'I  ', 'R  '/
                  
! -------------------------------------------------------------------------
!   parind = -2 : vector global        ; = -1 : scalar global ; 
!          =  2 : vector per occurence ; =  1 : scalar per occurence
! -------------------------------------------------------------------------

    data  parind /-1, -1, -1, -1, -1,&
                  -1, -1, -1, -1, -1,&
                  -1, -1, -1, -1, -1,&
                  -1, -1, -1, -1, -1,&
                  -1, -1, -1, -1, -1,&
                  -1, -1, -1, -1, -1,&
                  -1, -1, -1, -1, -1,&
                  -1, -1, -1, -1, -1,&
                  -1, -1,&

                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2,&
                  -1, -1,&

                  -1, -1, -1, -1, -1,&
                  -2, -2, -2, -2, -2,&
                  -2, -2, -2, -2, -2,&
                  -2, -2/
