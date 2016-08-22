!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
! sd_dtm data structure : Parameters <-> integer definitions
! -------------------------------------------------------------------------
!
#define _DTM_NBPAR 83

#define _ACC_WORK 1
#define _ADAPT 2
#define _ADRES_VC 3
#define _AMOR_DIA 4
#define _AMOR_FUL 5
#define _AMOR_MAT 6
#define _ANGL_FON 7
#define _ANGL_INI 8
#define _APPND_SD 9
#define _ARCH_NB 10
#define _ARCH_PER 11
#define _ARCH_STO 12
#define _AR_LINST 13
#define _A_ROT_F 14
#define _BASE_MOD 15
#define _BASE_VEC 16
#define _CALC_SD 17
#define _COEF_MLT 18
#define _DESC_FRC 19
#define _DT 20
#define _DT_MAX 21
#define _DT_MIN 22
#define _FSI_ABSC 23
#define _FSI_BASE 24
#define _FSI_BASF 25
#define _FSI_CASE 26
#define _FSI_CODM 27
#define _FSI_CPLD 28
#define _FSI_IRES 29
#define _FSI_ITYP 30
#define _FSI_PHIE 31
#define _FSI_POID 32
#define _FSI_PRVI 33
#define _FSI_RHOE 34
#define _FSI_TYPF 35
#define _FSI_VGAP 36
#define _FSI_ZET0 37
#define _FUNC_NAM 38
#define _F_NL_ADD 39
#define _GYRO_FUL 40
#define _IARCH_SD 41
#define _IMP_ACCE 42
#define _IMP_DEPL 43
#define _IMP_FEXT 44
#define _IMP_VITE 45
#define _IND_ALOC 46
#define _INST_FIN 47
#define _INST_INI 48
#define _MASS_DIA 49
#define _MASS_FAC 50
#define _MASS_FUL 51
#define _MASS_MAT 52
#define _MAT_DESC 53
#define _MULTI_AP 54
#define _NB_MODES 55
#define _NB_NONLI 56
#define _NB_STEPS 57
#define _NL_CASE 58
#define _NL_SAVE0 59
#define _NL_SAVE1 60
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
#define _SOLVER 74
#define _SUB_STRU 75
#define _TYP_BASE 76
#define _V_ROT 77
#define _V_ROT_F 78
#define _SD_NONL 79
#define _NB_EXC_T 80
#define _NB_PHYEQ 81
#define _NL_BUFFER 82
#define _VITE_VAR 83


#define _DTM_NB_SCHEMAS     8

#define _SCH_EULER          1
#define _SCH_DEVOGE         2
#define _SCH_NEWMARK        3
#define _SCH_RUNGE_KUTTA_32 4
#define _SCH_RUNGE_KUTTA_54 5
#define _SCH_ADAPT_ORDRE1   6
#define _SCH_ADAPT_ORDRE2   7
#define _SCH_ITMI           8
