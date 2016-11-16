! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! -------------------------------------------------------------------------
! sd_dtm data structure : Parameters <-> integer definitions
! -------------------------------------------------------------------------
!
#define _DTM_NBPAR 80

#define _ACC_WORK 1
#define _ADAPT 2
#define _ADRES_VC 3
#define _AMOR_DIA 4
#define _AMOR_FUL 5
#define _AMOR_MAT 6
#define _APPND_SD 7
#define _ARCH_NB 8
#define _ARCH_PER 9
#define _ARCH_STO 10
#define _AR_LINST 11
#define _A_ROT_F 12
#define _BASE_MOD 13
#define _BASE_VEC 14
#define _CALC_SD 15
#define _COEF_MLT 16
#define _DESC_FRC 17
#define _DT 18
#define _DT_MAX 19
#define _DT_MIN 20
#define _FSI_ABSC 21
#define _FSI_BASE 22
#define _FSI_BASF 23
#define _FSI_CASE 24
#define _FSI_CODM 25
#define _FSI_CPLD 26
#define _FSI_IRES 27
#define _FSI_ITYP 28
#define _FSI_PHIE 29
#define _FSI_POID 30
#define _FSI_PRVI 31
#define _FSI_RHOE 32
#define _FSI_TYPF 33
#define _FSI_VGAP 34
#define _FSI_ZET0 35
#define _FUNC_NAM 36
#define _F_NL_ADD 37
#define _GYRO_FUL 38
#define _IARCH_SD 39
#define _IMP_ACCE 40
#define _IMP_DEPL 41
#define _IMP_FEXT 42
#define _IMP_VITE 43
#define _IND_ALOC 44
#define _INST_FIN 45
#define _INST_INI 46
#define _MASS_DIA 47
#define _MASS_FAC 48
#define _MASS_FUL 49
#define _MASS_MAT 50
#define _MAT_DESC 51
#define _MULTI_AP 52
#define _NB_EXC_T 53
#define _NB_MODES 54
#define _NB_NONLI 55
#define _NB_PHYEQ 56
#define _NB_STEPS 57
#define _NL_BUFFER 58
#define _NL_CASE 59
#define _NL_SAVE0 60
#define _NL_SAVES 61
#define _NL_TREAT 62
#define _NUM_DDL 63
#define _N_ORD_VC 64
#define _OMEGA 65
#define _OMEGA_SQ 66
#define _RESU_SD 67
#define _RIGI_DIA 68
#define _RIGI_FUL 69
#define _RIGI_MAT 70
#define _RIGY_FUL 71
#define _SCHEMA 72
#define _SCHEMA_I 73
#define _SD_NONL 74
#define _SOLVER 75
#define _SUB_STRU 76
#define _TYP_BASE 77
#define _VITE_VAR 78
#define _V_ROT 79
#define _V_ROT_F 80

! For fast dispatching of the integration scheme (method)
#define _DTM_NB_SCHEMAS     8

#define _SCH_EULER          1
#define _SCH_DEVOGE         2
#define _SCH_NEWMARK        3
#define _SCH_RUNGE_KUTTA_32 4
#define _SCH_RUNGE_KUTTA_54 5
#define _SCH_ADAPT_ORDRE1   6
#define _SCH_ADAPT_ORDRE2   7
#define _SCH_ITMI           8