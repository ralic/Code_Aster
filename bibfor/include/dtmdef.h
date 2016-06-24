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
#define _DTM_NBPAR 129

#define _AMOR_MAT 1
#define _APPND_SD 2
#define _BASE_MOD 3
#define _CALC_SD  4
#define _FV_NUMB  5
#define _FX_NUMB  6
#define _MASS_MAT 7
#define _NB_ANTSI 8
#define _NB_CHOC  9
#define _NB_DIS_VISC 10
#define _NB_EXC_T 11
#define _NB_FLAMB 12
#define _NB_MODES 13
#define _NB_NONLI 14
#define _NB_PALIE 15
#define _NB_PHYEQ 16
#define _NB_R_FIS 17
#define _NUM_DDL  18
#define _RESU_SD  19
#define _RIGI_MAT 20
#define _SCHEMA   21
#define _SUB_STRU 22
#define _TYP_BASE 23
#define _V_ROT    24
#define _ARCH_NB  25
#define _DT       26
#define _NB_STEPS 27
#define _ARCH_PER 28
#define _DT_EDYOS 29
#define _DT_MAX   30
#define _DT_MIN   31
#define _INST_INI 32
#define _INST_FIN 33
#define _V_ROT_F  34
#define _A_ROT_F  35
#define _MULTI_AP 36
#define _ANGL_INI 37
#define _ANGL_FON 38
#define _NB_MOD_D 39
#define _ADAPT    40
#define _NL_CASE  41
#define _NL_TREAT 42
#define _IND_ALOC 43
#define _AMOR_DIA 44
#define _AMOR_FUL 45
#define _BASE_VEC 46
#define _CHO_DEPL 47
#define _CHO_NAME 48
#define _CHO_NOEU 49
#define _CHO_PARA 50
#define _CHO_RANK 51
#define _COEF_MLT 52
#define _DESC_FRC 53
#define _FIMPO    54
#define _FUNC_NAM 55
#define _FV_DEPLR 56
#define _FV_FONCT 57
#define _FX_DEPLR 58
#define _FX_FONCT 59
#define _GYRO_FUL 60
#define _INDIC    61
#define _MASS_DIA 62
#define _MASS_FUL 63
#define _N_ORD_VC 64
#define _OMEGA    65
#define _OMEGA_SQ 66
#define _PSI_DELT 67
#define _PTCHOC   68
#define _RFIMPO   69
#define _RIGI_DIA 70
#define _RIGI_FUL 71
#define _RIGY_FUL 72
#define _ROTR_DFK 73
#define _ROTR_FK  74
#define _SOLVER   75
#define _SOUPL    76
#define _TRLOC    77
#define _VECT_GEN 78
#define _VECT_RES 79
#define _AR_LINST 80
#define _AR_LINDI 81
#define _MAT_DESC 82
#define _ARCH_STO 83
#define _ACC_WORK 84
#define _ADRES_VC 85
#define _NL_SAVES 86
#define _FX_SREDR 87
#define _FX_SREDI 88
#define _FV_SREVR 89
#define _FV_SREVI 90
#define _MASS_FAC 91
#define _NL_SAVE0 92
#define _NL_SAVE1 93
#define _F_TOT_WK 94
#define _F_NL_ADD 95
#define _NL_DEPL0 96
#define _NL_VITE0 97
#define _NL_ACCE0 98
#define _F_TAN_WK 99
#define _PAL_TYP 100
#define _PAL_FIN 101
#define _PAL_CN 102
#define _PAL_FSAV 103
#define _PAL_NBCV 104
#define _IMP_DEPL 105
#define _IMP_VITE 106
#define _IMP_ACCE 107
#define _IMP_FEXT 108
#define _SCHEMA_I 109
#define _IARCH_SD 110
#define _FSI_BASE 111
#define _FSI_TYPF 112
#define _FSI_IVIT 113
#define _FSI_CASE 114
#define _FSI_VGAP 115
#define _FSI_CPLD 116
#define _FSI_IRES 117
#define _FSI_PRVI 118
#define _FSI_RHOE 119
#define _FSI_BASF 120
#define _FSI_PHIE 121
#define _FSI_ABSC 122
#define _FSI_CODM 123
#define _FSI_POID 124
#define _FSI_NUOR 125
#define _FSI_ITYP 126
#define _FSI_ZET0 127
#define _NB_DIS_ECRO_TRAC 128
#define _CHO_PAIN 129

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
