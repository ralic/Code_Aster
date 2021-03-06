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
! Parameters definitions
! -------------------------------------------------------------------------
!
    integer :: parind(_NL_NBPAR)
    character(len=3)  :: partyp(_NL_NBPAR)
    character(len=8)  :: params(_NL_NBPAR)

    data params /'ANG_INIT', 'ANG_ROTA', 'AS_C    ', 'AS_DX_MX', 'AS_K1   ', &
                 'AS_K2   ', 'AS_ALPHA', 'AS_FX_0 ', 'BKL_FLIM', 'BKL_FCST', &
                 'BKL_STIF', 'NOM_CMP ', 'COOR_NO1', 'COOR_NO2', 'COOR_ORI', &
                 'DAMP_NOR', 'DAMP_TAN', 'DISO_DX ', 'DISO_DX0', 'DIST_NO1', &
                 'DIST_NO2', 'DVSC_A  ', 'DVSC_C  ', 'DVSC_K1 ', 'DVSC_K2 ', &
                 'DVSC_K3 ', 'FRIC_DYN', 'FRIC_STA', 'FRIC_UNI', 'FV_FONCT', &
                 'FX_FONCT', 'F_TAN_WK', 'F_TOT_WK', 'GAP     ', 'VAR_INTR', &
                 'VAR_INDX', 'MAX_INTE', 'MX_LEVEL', 'MESH_1  ', 'MESH_2  ', &
                 'MOD_DNO1', 'MOD_DNO2', 'NB_ANTSI', 'NB_CHOC ', 'NB_DISEC', &
                 'NB_DISVI', 'NB_FLAMB', 'NB_PALIE', 'NB_RELFV', 'NB_RELFX', &
                 'NB_R_FIS', 'NL_FUNC ', 'NL_TITLE', 'NL_TYPE ', 'NO1_NAME', &
                 'NO2_NAME', 'VEC_NORM', 'NUMDDL_1', 'NUMDDL_2', 'OBST_TYP', &
                 'PSI_DEL1', 'PSI_DEL2', 'RES_INTE', 'STIF_TAN', 'ROTR_DFK', &
                 'ROTR_FK ', 'SIGN_DYZ', 'SINCOS_A', 'SINCOS_B', 'SINCOS_G', &
                 'SS1_NAME', 'SS2_NAME', 'STIF_NOR'/

    data partyp /'R  ', 'K24', 'R  ', 'R  ', 'R  ', &
                 'R  ', 'R  ', 'R  ', 'R  ', 'R  ', &
                 'R  ', 'K24', 'R  ', 'R  ', 'R  ', &
                 'R  ', 'R  ', 'R  ', 'R  ', 'R  ', &
                 'R  ', 'R  ', 'R  ', 'R  ', 'R  ', &
                 'R  ', 'R  ', 'R  ', 'I  ', 'K24', &
                 'K24', 'R  ', 'R  ', 'R  ', 'R  ', &
                 'I  ', 'I  ', 'I  ', 'K24', 'K24', &
                 'R  ', 'R  ', 'I  ', 'I  ', 'I  ', &
                 'I  ', 'I  ', 'I  ', 'I  ', 'I  ', &
                 'I  ', 'I  ', 'K24', 'I  ', 'K24', &
                 'K24', 'R  ', 'K24', 'K24', 'K24', &
                 'R  ', 'R  ', 'R  ', 'R  ', 'K24', &
                 'K24', 'R  ', 'R  ', 'R  ', 'R  ', &
                 'K24', 'K24', 'R  '/ 

! -------------------------------------------------------------------------
!   parind = -2 : vector global        ; = -1 : scalar global ;
!          =  2 : vector per occurence ; =  1 : scalar per occurence
! -------------------------------------------------------------------------

    data parind / 1,  1,  1,  1,  1, &
                  1,  1,  1,  1,  1, &
                  1,  1,  2,  2,  2, &
                  1,  1,  1,  1,  1, &
                  1,  1,  1,  1,  1, &
                  1,  1,  1,  1,  1, &
                  1, -2, -2,  1, -2, &
                 -2,  1, -1,  1,  1, &
                  2,  2, -1, -1, -1, &
                 -1, -1, -1, -1, -1, &
                 -1,  2,  1,  1,  1, &
                  1,  2,  1,  1,  1, &
                  2,  2,  1,  1,  1, &
                  1,  2,  2,  2,  2, &
                  1,  1,  1/