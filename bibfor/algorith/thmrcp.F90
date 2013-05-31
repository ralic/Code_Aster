subroutine thmrcp(etape, imate, thmc, meca, hydr,&
                  ther, t0, p10, p20, phi0,&
                  pvp0, t, p1, p1m, p2,&
                  phi, endo, pvp, rgaz, rhod,&
                  cpd, biot, satm, satur, dsatur,&
                  pesa, permfh, permli, dperml, permgz,&
                  dperms, dpermp, fick, dfickt, dfickg,&
                  lambp, dlambp, rhol, unsurk, alpha,&
                  cpl, lambs, dlambs, viscl, dviscl,&
                  mamolg, cpg, lambt, dlambt, viscg,&
                  dviscg, mamolv, cpvg, viscvg, dvisvg,&
                  fickad, dfadt, cpad, kh, pad,&
                  em, lambct, isot, dficks, instap,&
                  retcom)
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
! =====================================================================
! =====================================================================
! --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
! =====================================================================
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
! aslint: disable=W1501,W1504
    implicit none
    include 'asterc/r8prem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/permea.h'
    include 'asterfort/permvc.h'
    include 'asterfort/permvg.h'
    include 'asterfort/rcvala.h'
    include 'asterfort/satura.h'
    include 'asterfort/satuvg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: imate, retcom
    real(kind=8) :: t0, p10, p20, phi0, pvp0, t, p1, p2, phi, pvp
    real(kind=8) :: rgaz, rhod, cpd, biot, satm, satur, dsatur, pesa(3)
    real(kind=8) :: permfh, permli, dperml, permgz, dperms, dpermp
    real(kind=8) :: fick, dfickt, dficks, dfickg, lambp, dlambp, rhol
    real(kind=8) :: alpha, cpl, lambs, dlambs, viscl, dviscl, cpg, pad
    real(kind=8) :: lambt, dlambt, viscg, dviscg, mamolg, cpvg, viscvg
    real(kind=8) :: dvisvg, fickad, dfadt, endo, mamolv, p1m, cpad, kh, em
    real(kind=8) :: lambct, unsurk, isot(6), instap
    character(len=8) :: etape
    character(len=16) :: meca, thmc, ther, hydr
! =====================================================================
! --- VARIABLES LOCALES -----------------------------------------------
! =====================================================================
    integer :: ii, dimsat, dimvg
    integer :: dim1, dim2, dim3, dim4, dim5, dim6, dim7, dim8
    integer :: dim9, dim10, dim11, dim12, dim13, dim14, dim15
    integer :: dim16, dim17, dim18, dim19, dim20, dim21, dim22
    integer :: dim23, dim24, dim25, dim26, dim27, dim28, dim29
    integer :: dim30, dim31, dim32, dim33, ncon, dim35
    integer :: dimpar
    integer :: dim36, dim37, dim38, dim40, dim41, dim42, dim43, dim39
    parameter   ( dimsat =  2 )
    parameter   ( dimvg  =  5 )
    parameter   ( ncon   =  9 )
    parameter   ( dim1   =  5 )
    parameter   ( dim2   =  4 )
    parameter   ( dim3   =  4 )
    parameter   ( dim4   =  5 )
    parameter   ( dim5   =  2 )
    parameter   ( dim6   =  6 )
    parameter   ( dim7   =  4 )
    parameter   ( dim8   =  2 )
    parameter   ( dim9   =  7 )
    parameter   ( dim10  =  4 )
    parameter   ( dim11  =  2 )
    parameter   ( dim12  =  2 )
    parameter   ( dim13  =  7 )
    parameter   ( dim14  =  4 )
    parameter   ( dim15  =  2 )
    parameter   ( dim16  =  6 )
    parameter   ( dim17  =  4 )
    parameter   ( dim18  = 16 )
    parameter   ( dim19  =  4 )
    parameter   ( dim20  = 17 )
    parameter   ( dim21  =  3 )
    parameter   ( dim22  = 26 )
    parameter   ( dim23  =  4 )
    parameter   ( dim24  =  3 )
    parameter   ( dim25  = 33 )
    parameter   ( dim26  =  4 )
    parameter   ( dim27  =  3 )
    parameter   ( dim28  =  1 )
    parameter   ( dim29  = 26 )
    parameter   ( dim30  =  4 )
    parameter   ( dim31  =  3 )
    parameter   ( dim32  = 22 )
    parameter   ( dim33  =  4 )
!
    parameter   ( dim35  =  7 )
    parameter   ( dim36  =  4 )
    parameter   ( dim37  =  2 )
    parameter   ( dim38  =  2 )
    parameter   ( dim39  =  2 )
    parameter   ( dim40  = 38 )
    parameter   ( dim41  =  4 )
    parameter   ( dim42  =  3 )
    parameter   ( dim43  =  1 )
!
    parameter   ( dimpar  =  4 )
!
!   NRESMA EST LE MAX DE DIMPAR, DIMSAT ET DE DIMI, AVEC I DE 1 A 43
    integer :: nresma
    parameter ( nresma = 34 )
!
    real(kind=8) :: val1(dim1), val2(dim2), val3(dim3), val4(dim4)
    real(kind=8) :: val5(dim5), val6(dim6), val7(dim7), val8(dim8)
    real(kind=8) :: val9(dim9+1), val10(dim10), val11(dim11), rbid1
    real(kind=8) :: val12(dim12), val13(dim13+1), val14(dim14)
    real(kind=8) :: val15(dim15), val16(dim16+1), val17(dim17)
    real(kind=8) :: val18(dim18), val19(dim19), val20(dim20)
    real(kind=8) :: val21(dim21), val22(dim22), val23(dim23)
    real(kind=8) :: val24(dim24), val25(dim25), val26(dim26)
    real(kind=8) :: val27(dim27), val28(dim28), val29(dim29)
    real(kind=8) :: val30(dim30), val31(dim31), val32(dim32)
    real(kind=8) :: val33(dim33), valsat(dimsat)
    real(kind=8) :: val35(dim35+1), val36(dim36), val37(dim37), val38(dim38)
    real(kind=8) :: val40(dim40), val41(dim41), val42(dim42), val43(dim43)
    real(kind=8) :: val39(dim39), valpar(dimpar), cond(ncon)
    real(kind=8) :: vg(dimvg), fpesa, un, zero
!
!
    integer :: icodre(nresma)
    character(len=4) :: nompar(dimpar)
    character(len=8) :: ncra1(dim1), ncra2(dim2), ncra3(dim3), ncra4(dim4)
    character(len=8) :: ncra5(dim5), ncra6(dim6), ncra7(dim7), ncra8(dim8)
    character(len=8) :: ncra9(dim9), ncra10(dim10), ncra11(dim11)
    character(len=8) :: ncra12(dim12), ncra13(dim13), ncra14(dim14)
    character(len=8) :: ncra15(dim15), ncra16(dim16), ncra17(dim17)
    character(len=8) :: ncra18(dim18), ncra19(dim19), ncra20(dim20)
    character(len=8) :: ncra21(dim21), ncra22(dim22), ncra23(dim23)
    character(len=8) :: ncra24(dim24), ncra25(dim25), ncra26(dim26)
    character(len=8) :: ncra27(dim27), ncra28(dim28), ncra29(dim29)
    character(len=8) :: ncra30(dim30), ncra31(dim31), ncra32(dim32)
    character(len=8) :: ncra33(dim33)
    character(len=8) :: ncra35(dim35), ncra36(dim36), ncra37(dim37)
    character(len=8) :: ncra38(dim38), ncra39(dim39), ncra40(dim40)
    character(len=8) :: ncra41(dim41), ncra42(dim42), ncra43(dim43)
    character(len=8) :: crad35(dim35), crad36(dim36), crad37(dim37)
    character(len=8) :: crad39(dim39), crad40(dim40)
    character(len=8) :: crad41(dim41), crad42(dim42)
    character(len=8) :: nsat(dimsat), nvg(dimvg)
! =====================================================================
! --- DEFINITION DES DONNEES INITIALES --------------------------------
! =====================================================================
    data ncra1  / 'TEMP'     ,&
     &              'PRE1'     ,&
     &              'PRE2'     ,&
     &              'PORO'     ,&
     &              'PRES_VAP' /
! =====================================================================
! --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_SATU -----
! =====================================================================
    data ncra2  / 'RHO'      ,&
     &              'BIOT_COE' ,&
     &              'CP'    ,&
     &              'EMMAG' /
    data ncra3  / 'RHO'      ,&
     &              'UN_SUR_K' ,&
     &              'ALPHA'    ,&
     &              'CP'       /
! =====================================================================
! --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS GAZ -----------
! =====================================================================
    data ncra4  / 'R_GAZ'    ,&
     &              'RHO'      ,&
     &              'BIOT_COE' ,&
     &              'CP' ,&
     &              'EMMAG'       /
    data ncra5  / 'MASS_MOL' ,&
     &              'CP'       /
! =====================================================================
! --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_VAPE -----
! =====================================================================
    data ncra6  / 'R_GAZ'    ,&
     &              'RHO'      ,&
     &              'BIOT_COE' ,&
     &              'CP'       ,&
     &              'SATU_PRE' ,&
     &              'EMMAG'  /
    data ncra7  / 'RHO'      ,&
     &              'UN_SUR_K' ,&
     &              'ALPHA'    ,&
     &              'CP'       /
    data ncra8  / 'MASS_MOL' ,&
     &              'CP'       /
! =====================================================================
! --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_VAPE_GAZ -
! =====================================================================
    data ncra9  / 'R_GAZ'    ,&
     &              'RHO'      ,&
     &              'BIOT_COE' ,&
     &              'CP'       ,&
     &              'SATU_PRE' ,&
     &              'D_SATU_P' ,&
     &              'EMMAG'    /
    data ncra10 / 'RHO'      ,&
     &              'UN_SUR_K' ,&
     &              'ALPHA'    ,&
     &              'CP'       /
    data ncra11 / 'MASS_MOL' ,&
     &              'CP'       /
    data ncra12 / 'MASS_MOL' ,&
     &              'CP'       /
! =====================================================================
! --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_GAZ ------
! =====================================================================
    data ncra13 / 'R_GAZ'    ,&
     &              'RHO'      ,&
     &              'BIOT_COE' ,&
     &              'CP'       ,&
     &              'SATU_PRE' ,&
     &              'D_SATU_P' ,&
     &              'EMMAG' /
    data ncra14 / 'RHO'      ,&
     &              'UN_SUR_K' ,&
     &              'ALPHA'    ,&
     &              'CP'       /
    data ncra15 / 'MASS_MOL' ,&
     &              'CP'       /
! =====================================================================
! --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_GAZ_ATM --
! =====================================================================
    data ncra16 / 'RHO'      ,&
     &              'BIOT_COE' ,&
     &              'CP'       ,&
     &              'SATU_PRE' ,&
     &              'D_SATU_P' ,&
     &              'EMMAG' /
    data ncra17 / 'RHO'      ,&
     &              'UN_SUR_K' ,&
     &              'ALPHA'    ,&
     &              'CP'       /
! =====================================================================
! --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_SATU ------------
! =====================================================================
    data ncra18 / 'PESA_X'   ,&
     &              'PESA_Y'   ,&
     &              'PESA_Z'   ,&
     &              'PERM_IN'  ,&
     &              'PERM_END' ,&
     &              'LAMB_T'   ,&
     &              'D_LB_T',&
     &              'LAMB_P'   ,&
     &              'D_LB_P',&
     &              'LAMB_CT',&
     &              'PERMIN_X','PERMIN_Y',&
     &              'PERMIN_Z','PERMINXY',&
     &              'PERMINYZ','PERMINZX'/
    data ncra19 / 'UN_SUR_K' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' ,&
     &              'ALPHA' /
! =====================================================================
! --- DEFINITION DES DONNEES FINALES DANS LE CAS GAZ ------------------
! =====================================================================
    data ncra20 / 'R_GAZ'    ,&
     &              'PESA_X'   ,&
     &              'PESA_Y'   ,&
     &              'PESA_Z'   ,&
     &              'PERM_IN'  ,&
     &              'PERM_END' ,&
     &              'LAMB_T'   ,&
     &              'D_LB_T',&
     &              'LAMB_P'   ,&
     &              'D_LB_P',&
     &              'LAMB_CT',&
     &              'PERMIN_X','PERMIN_Y',&
     &              'PERMIN_Z','PERMINXY',&
     &              'PERMINYZ','PERMINZX'/
    data ncra21 / 'MASS_MOL' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' /
! =====================================================================
! --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_VAPE ------------
! =====================================================================
    data ncra22 / 'R_GAZ'    ,&
     &              'PESA_X'   ,&
     &              'PESA_Y'   ,&
     &              'PESA_Z'   ,&
     &              'PERM_IN'  ,&
     &              'PERM_END' ,&
     &              'LAMB_T'   ,&
     &              'D_LB_T' ,  'LAMB_P' ,&
     &              'D_LB_P' ,'LAMB_S'   ,&
     &              'D_LB_S' ,'LAMB_CT'  ,&
     &              'SATU_PRE' ,'D_SATU_P'  ,&
     &              'PERM_LIQ' , 'D_PERM_L' ,&
     &              'PERM_GAZ' , 'D_PERM_S' ,&
     &              'D_PERM_P',&
     &              'PERMIN_X','PERMIN_Y',&
     &              'PERMIN_Z','PERMINXY',&
     &              'PERMINYZ','PERMINZX'/
    data ncra23 / 'UN_SUR_K' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' ,&
     &              'ALPHA'    /
    data ncra24 / 'MASS_MOL' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' /
! =====================================================================
! --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_VAPE_GAZ --------
! =====================================================================
    data ncra25 / 'R_GAZ'    ,'PESA_X'   ,&
     &              'PESA_Y'   ,'PESA_Z'   ,&
     &              'PERM_IN'  ,'PERM_END' ,&
     &              'LAMB_T'   ,'D_LB_T' ,&
     &              'LAMB_P'   ,'D_LB_P' ,&
     &              'LAMB_S'   ,'D_LB_S' ,&
     &              'LAMB_CT'   ,&
     &              'SATU_PRE' ,'D_SATU_P' ,&
     &              'PERM_LIQ' ,'D_PERM_L' ,&
     &              'PERM_GAZ' ,'D_PERM_S' ,&
     &              'D_PERM_P' ,'FICKV_T'  ,&
     &              'FICKV_PV' ,'FICKV_PG' ,&
     &              'FICKV_S'  ,'D_FV_T'   ,&
     &              'D_FV_PG',&
     &              'PERMIN_X','PERMIN_Y',&
     &              'PERMIN_Z','PERMINXY',&
     &              'PERMINYZ','PERMINZX',&
     &              'D_FV_S'/
    data ncra26 / 'UN_SUR_K' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' ,&
     &              'ALPHA'  /
    data ncra27 / 'MASS_MOL' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' /
    data ncra28 / 'MASS_MOL' /
! =====================================================================
! --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_GAZ -------------
! =====================================================================
    data ncra29 / 'R_GAZ'    ,&
     &              'PESA_X'   ,&
     &              'PESA_Y'   ,&
     &              'PESA_Z'   ,&
     &              'PERM_IN'  ,'PERM_END' ,&
     &              'LAMB_T'   ,'D_LB_T' ,&
     &              'LAMB_P'   ,'D_LB_P' ,&
     &              'LAMB_S'   ,'D_LB_S' ,&
     &              'LAMB_CT'  ,'SATU_PRE' ,&
     &              'D_SATU_P' ,'PERM_LIQ' ,&
     &              'D_PERM_L' ,'PERM_GAZ' ,&
     &              'D_PERM_S' ,'D_PERM_P',&
     &              'PERMIN_X',&
     &              'PERMIN_Y',&
     &              'PERMIN_Z',&
     &              'PERMINXY',&
     &              'PERMINYZ',&
     &              'PERMINZX'/
    data ncra30 / 'UN_SUR_K' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' ,&
     &              'ALPHA'  /
    data ncra31 / 'MASS_MOL' ,&
     &              'VISC'     ,&
     &              'D_VISC_T'/
! =====================================================================
! --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_GAZ_ATM ---------
! =====================================================================
    data ncra32 / 'PESA_X'   ,&
     &              'PESA_Y'   ,&
     &              'PESA_Z'   ,&
     &              'PERM_IN'  ,&
     &              'PERM_END' ,&
     &              'LAMB_T'   ,&
     &              'D_LB_T',&
     &              'LAMB_P',&
     &              'D_LB_P' ,&
     &              'LAMB_S'   ,&
     &              'D_LB_S' ,&
     &              'LAMB_CT'   ,&
     &              'SATU_PRE','D_SATU_P' ,&
     &              'PERM_LIQ','D_PERM_L' ,&
     &              'PERMIN_X','PERMIN_Y',&
     &              'PERMIN_Z','PERMINXY',&
     &              'PERMINYZ','PERMINZX'/
    data ncra33 / 'UN_SUR_K' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' ,&
     &              'ALPHA'   /
! =====================================================================
! -- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_AD_GAZ_VAPE
! =====================================================================
    data ncra35  / 'R_GAZ'    ,&
     &              'RHO'      ,&
     &              'BIOT_COE' ,&
     &              'CP'       ,&
     &              'SATU_PRE' ,&
     &              'D_SATU_P' ,&
     &              'EMMAG' /
    data ncra36 / 'RHO'      ,&
     &              'UN_SUR_K' ,&
     &              'ALPHA'    ,&
     &              'CP'       /
    data ncra37 / 'MASS_MOL' ,&
     &              'CP'       /
    data ncra38 / 'MASS_MOL' ,&
     &              'CP'       /
!      DATA NCRA39 / 'CP'        ,
!    &              'COEF_HENRY' /
    data ncra39 / 'CP'        ,&
     &              'COEF_HEN' /
! =====================================================================
! --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_AD_GAZ_VAPE -----
! =====================================================================
    data ncra40 / 'R_GAZ'    ,'PESA_X'   ,&
     &               'PESA_Y'    , 'PESA_Z'  ,&
     &               'PERM_IN'   ,'PERM_END' ,&
     &               'LAMB_T'    ,'D_LB_T' ,&
     &               'LAMB_P'    ,'D_LB_P' ,&
     &               'LAMB_S'    ,'D_LB_S' ,&
     &               'LAMB_CT'    ,&
     &               'SATU_PRE' ,'D_SATU_P' ,&
     &              'PERM_LIQ' ,'D_PERM_L' ,&
     &              'PERM_GAZ' ,'D_PERM_S' ,&
     &              'D_PERM_P' ,'FICKV_T'  ,&
     &              'FICKV_PV' ,'FICKV_PG' ,&
     &              'FICKV_S'  ,'D_FV_T',&
     &              'D_FV_PG','FICKA_T'  ,&
     &              'FICKA_PA' , 'FICKA_PL' ,&
     &              'FICKA_S'  ,'D_FA_T' ,&
     &              'PERMIN_X','PERMIN_Y',&
     &              'PERMIN_Z','PERMINXY',&
     &              'PERMINYZ','PERMINZX',&
     &              'D_FV_S'/
    data ncra41 / 'UN_SUR_K' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' ,&
     &              'ALPHA'  /
    data ncra42 / 'MASS_MOL' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' /
    data ncra43 / 'MASS_MOL' /
! =====================================================================
! -- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_AD_GAZ
! =====================================================================
    data crad35  / 'R_GAZ'    ,&
     &              'RHO'      ,&
     &              'BIOT_COE' ,&
     &              'CP'       ,&
     &              'SATU_PRE' ,&
     &              'D_SATU_P' ,&
     &              'EMMAG' /
    data crad36 / 'RHO'      ,&
     &              'UN_SUR_K' ,&
     &              'ALPHA'    ,&
     &              'CP'       /
    data crad37 / 'MASS_MOL' ,&
     &              'CP'       /
!      DATA CRAD39 / 'CP'        ,
!    &              'COEF_HENRY' /
    data crad39 / 'CP'        ,&
     &              'COEF_HEN' /
! =====================================================================
! --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_AD_GAZ -----
! =====================================================================
!     DANS CRAD40 ON NE LIT PAS DE 21 A 26 INCLUS ET 38
    data crad40 / 'R_GAZ'    ,'PESA_X'   ,&
     &                'PESA_Y'    , 'PESA_Z'  ,&
     &                'PERM_IN'   ,'PERM_END' ,&
     &                'LAMB_T'    ,'D_LB_T' ,&
     &                'LAMB_P'    ,'D_LB_P' ,&
     &                'LAMB_S'    ,'D_LB_S' ,&
     &                'LAMB_CT'    ,&
     &                'SATU_PRE' ,'D_SATU_P' ,&
     &                'PERM_LIQ' ,'D_PERM_L' ,&
     &                'PERM_GAZ' ,'D_PERM_S' ,&
     &                'D_PERM_P' ,'FICKV_T'  ,&
     &                'FICKV_PV' ,'FICKV_PG' ,&
     &                'FICKV_S'  ,'D_FV_T',&
     &                'D_FV_PG','FICKA_T'  ,&
     &                'FICKA_PA' , 'FICKA_PL' ,&
     &                'FICKA_S'  ,'D_FA_T' ,&
     &                'PERMIN_X','PERMIN_Y','PERMIN_Z','PERMINXY',&
     &                'PERMINYZ','PERMINZX',&
     &                'D_FV_S'/
    data crad41 / 'UN_SUR_K' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' ,&
     &              'ALPHA'  /
    data crad42 / 'MASS_MOL' ,&
     &              'VISC'     ,&
     &              'D_VISC_T' /
! =====================================================================
! --- DEFINITION SATURATION -------------------------------------------
! =====================================================================
    data nsat   / 'SATU_PRE' ,&
     &              'D_SATU_P' /
! =====================================================================
! --- DEFINITION PARAMETRES MUALEM VAN GENUCHTEN ----------------------
! =====================================================================
    data nvg    / 'VG_N' ,&
     &               'VG_PR' ,&
     &               'VG_SR' ,&
     &               'VG_SMAX',&
     &               'VG_SATUR' /
! =====================================================================
! --- CAS DE L'INITIALISATION -----------------------------------------
! =====================================================================
!
    retcom = 0
    un = 1.d0
    zero=0.d0
!
    if (etape .eq. 'INITIALI') then
        do 10 ii = 1, dim1
            val1(ii) = 0.0d0
10      continue
        val1(1) = r8vide()
        call rcvala(imate, ' ', 'THM_INIT', 0, ' ',&
                    0.d0, dim1, ncra1, val1, icodre,&
                    0)
        t0 = val1(1)
        p10 = val1(2)
        p20 = val1(3)
        phi0 = val1(4)
        pvp0 = val1(5)
!
! VERIFICATION TEMPERATURE DE REFERENCE
        if ((thmc.eq.'GAZ') .or. (thmc.eq.'LIQU_VAPE') .or. ( thmc.eq.'LIQU_VAPE_GAZ') .or.&
            (thmc.eq.'LIQU_AD_GAZ_VAPE') .or. (thmc.eq.'LIQU_AD_GAZ') .or.&
            (thmc.eq.'LIQU_GAZ')) then
            if (t0 .eq. r8vide()) then
                call u2mesk('F', 'ALGORITH10_90', 1, thmc)
            endif
            if (t0 .le. r8prem()) then
                call u2mess('F', 'ALGORITH17_12')
            endif
        endif
        if (t0 .eq. r8vide()) t0 = 0.0d0
! VERIFICATION PRESSION DE GAZ DE REFERENCE NON NULLE
        if ((thmc.eq.'LIQU_VAPE_GAZ') .or. ( thmc.eq.'LIQU_AD_GAZ_VAPE') .or.&
            (thmc.eq.'LIQU_AD_GAZ') .or. (thmc.eq.'LIQU_GAZ')) then
            if (abs(p20) .le. r8prem()) then
                call u2mess('F', 'ALGORITH17_13')
            endif
        endif
!
!
    else if (etape.eq.'INTERMED') then
! =====================================================================
! --- CAS INTERMEDIAIRE -----------------------------------------------
! =====================================================================
        if (thmc .eq. 'LIQU_SATU') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_SATU -------------------------------
! =====================================================================
            do 20 ii = 1, dim2
                val2(ii) = 0.0d0
20          continue
            do 30 ii = 1, dim3
                val3(ii) = 0.0d0
30          continue
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 2, ncra2, val2, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_LIQU', 0, ' ',&
                        0.0d0, 2, ncra3, val3, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra2(3), val2(3), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 2, ncra3(3), val3(3), icodre,&
                            1)
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 1, ncra2(4), val2(4), icodre,&
                        0)
            rhod = val2(1)
            biot = val2(2)
            cpd = val2(3)
            em = val2(4)
            satur = 1.0d0
            rhol = val3(1)
            unsurk = val3(2)
            alpha = val3(3)
            cpl = val3(4)
        else if (thmc.eq.'GAZ') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE GAZ -------------------------------------
! =====================================================================
            do 41 ii = 1, dim4
                val4(ii) = 0.0d0
41          continue
            do 50 ii = 1, dim5
                val5(ii) = 0.0d0
50          continue
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 3, ncra4, val4, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_GAZ', 0, ' ',&
                        0.0d0, 1, ncra5, val5, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra4(4), val4(4), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                            t, 1, ncra5(2), val5(2), icodre,&
                            1)
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 1, ncra4(5), val4(5), icodre,&
                        0)
            rgaz = val4(1)
            rhod = val4(2)
            biot = val4(3)
            cpd = val4(4)
            em = val4(5)
            satur = 1.0d0
            mamolg = val5(1)
            cpg = val5(2)
        else if (thmc.eq.'LIQU_VAPE') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_VAPE -------------------------------
! =====================================================================
            do 60 ii = 1, dim6
                val6(ii) = 0.0d0
60          continue
            do 70 ii = 1, dim7
                val7(ii) = 0.0d0
70          continue
            do 80 ii = 1, dim8
                val8(ii) = 0.0d0
80          continue
            call rcvala(imate, ' ', 'THM_DIFFU', 0, 'TEMP',&
                        0.0d0, 3, ncra6, val6, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_LIQU', 0, ' ',&
                        0.0d0, 2, ncra7, val7, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_VAPE_GAZ', 0, ' ',&
                        0.0d0, 2, ncra8, val8, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra6(4), val6(4), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 2, ncra7(3), val7(3), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1m, val6(5), rbid1)
                elseif (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1m, 1, ncra6(5), val6(5), icodre,&
                            1)
            else
                call satura(hydr, p1m, val6(5), rbid1)
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 1, ncra6(6), val6(6), icodre,&
                        0)
            rgaz = val6(1)
            rhod = val6(2)
            biot = val6(3)
            cpd = val6(4)
            satm = val6(5)
            em = val6(6)
            rhol = val7(1)
            unsurk = val7(2)
            alpha = val7(3)
            cpl = val7(4)
            mamolv = val8(1)
            cpvg = val8(2)
            if (satm .gt. un .or. satm .lt. zero) then
                retcom = 2
                goto 500
            endif
        else if (thmc.eq.'LIQU_VAPE_GAZ') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ---------------------------
! =====================================================================
            do 90 ii = 1, dim9+1
                val9(ii) = 0.0d0
90          continue
            do 100 ii = 1, dim10
                val10(ii) = 0.0d0
100          continue
            do 110 ii = 1, dim11
                val11(ii) = 0.0d0
110          continue
            do 120 ii = 1, dim12
                val12(ii) = 0.0d0
120          continue
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 3, ncra9, val9, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_LIQU', 0, ' ',&
                        0.0d0, 2, ncra10, val10, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_GAZ', 0, ' ',&
                        0.0d0, 1, ncra11, val11, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_VAPE_GAZ', 0, ' ',&
                        0.0d0, 2, ncra12, val12, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra9(4), val9(4), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 2, ncra10(3), val10(3), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                            t, 1, ncra11(2), val11(2), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1m, val9(5), rbid1)
                call satuvg(vg, p1, val9(6), val9(7))
!
                else if (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1m, 1, ncra9(5), val9(5), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, ncra9(5), val9(6), icodre,&
                            1)
            else
                call satura(hydr, p1m, val9(5), rbid1)
                call satura(hydr, p1, val9(6), val9(7))
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 1, ncra9(7), val9(8), icodre,&
                        0)
            rgaz = val9(1)
            rhod = val9(2)
            biot = val9(3)
            cpd = val9(4)
            satm = val9(5)
            satur = val9(6)
            dsatur = val9(7)
            em = val9(8)
            rhol = val10(1)
            unsurk = val10(2)
            alpha = val10(3)
            cpl = val10(4)
            mamolg = val11(1)
            cpg = val11(2)
            mamolv = val12(1)
            cpvg = val12(2)
            if (satm .gt. un .or. satm .lt. zero) then
                retcom = 2
                goto 500
            endif
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
        else if (thmc.eq.'LIQU_AD_GAZ_VAPE') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE   -----------
! =====================================================================
            do 91 ii = 1, dim35+1
                val35(ii) = 0.0d0
91          continue
            do 101 ii = 1, dim36
                val36(ii) = 0.0d0
101          continue
            do 111 ii = 1, dim37
                val37(ii) = 0.0d0
111          continue
            do 121 ii = 1, dim38
                val38(ii) = 0.0d0
121          continue
            do 131 ii = 1, dim39
                val39(ii) = 0.0d0
131          continue
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 3, ncra35, val35, icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_LIQU', 0, ' ',&
                        0.0d0, 2, ncra36, val36, icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_GAZ', 0, ' ',&
                        0.0d0, 1, ncra37, val37, icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_VAPE_GAZ', 0, ' ',&
                        0.0d0, 2, ncra38, val38, icodre,&
                        0)
!
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra35(4), val35(4), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 2, ncra36(3), val36(3), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                            t, 1, ncra37(2), val37(2), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1m, val35(5), rbid1)
                call satuvg(vg, p1, val35(6), val35(7))
                else if (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1m, 1, ncra35(5), val35(5), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, ncra35(5), val35(6), icodre,&
                            1)
            else
                call satura(hydr, p1m, val35(5), rbid1)
                call satura(hydr, p1, val35(6), val35(7))
            endif
            call rcvala(imate, ' ', 'THM_AIR_DISSOUS', 0, ' ',&
                        0.0d0, 1, ncra39(1), val39(1), icodre,&
                        1)
            call rcvala(imate, ' ', 'THM_AIR_DISSOUS', 1, 'TEMP',&
                        t, 1, ncra39(2), val39(2), icodre,&
                        1)
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 1, ncra35(7), val35(8), icodre,&
                        0)
            rgaz = val35(1)
            rhod = val35(2)
            biot = val35(3)
            cpd = val35(4)
            satm = val35(5)
            satur = val35(6)
            dsatur = val35(7)
            em = val35(8)
            rhol = val36(1)
            unsurk = val36(2)
            alpha = val36(3)
            cpl = val36(4)
            mamolg = val37(1)
            cpg = val37(2)
            mamolv = val38(1)
            cpvg = val38(2)
            cpad = val39(1)
            kh = val39(2)
            if (satm .gt. un .or. satm .lt. zero) then
                retcom = 2
                goto 500
            endif
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
!
!
!
        else if (thmc.eq.'LIQU_AD_GAZ') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE   -----------
! =====================================================================
            do 991 ii = 1, dim35+1
                val35(ii) = 0.0d0
991          continue
            do 9101 ii = 1, dim36
                val36(ii) = 0.0d0
9101          continue
            do 9111 ii = 1, dim37
                val37(ii) = 0.0d0
9111          continue
            do 9131 ii = 1, dim39
                val39(ii) = 0.0d0
9131          continue
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 3, crad35, val35, icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_LIQU', 0, ' ',&
                        0.0d0, 2, crad36, val36, icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_GAZ', 0, ' ',&
                        0.0d0, 1, crad37, val37, icodre,&
                        0)
!
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, crad35(4), val35(4), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 2, crad36(3), val36(3), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                            t, 1, crad37(2), val37(2), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1m, val35(5), rbid1)
                call satuvg(vg, p1, val35(6), val35(7))
                else if (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1m, 1, crad35(5), val35(5), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, crad35(5), val35(6), icodre,&
                            1)
            else
                call satura(hydr, p1m, val35(5), rbid1)
                call satura(hydr, p1, val35(6), val35(7))
            endif
            call rcvala(imate, ' ', 'THM_AIR_DISSOUS', 0, ' ',&
                        0.0d0, 1, crad39(1), val39(1), icodre,&
                        1)
            call rcvala(imate, ' ', 'THM_AIR_DISSOUS', 1, 'TEMP',&
                        t, 1, crad39(2), val39(2), icodre,&
                        1)
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 1, crad35(7), val35(8), icodre,&
                        0)
            rgaz = val35(1)
            rhod = val35(2)
            biot = val35(3)
            cpd = val35(4)
            satm = val35(5)
            satur = val35(6)
            dsatur = val35(7)
            em = val35(8)
            rhol = val36(1)
            unsurk = val36(2)
            alpha = val36(3)
            cpl = val36(4)
            mamolg = val37(1)
            cpg = val37(2)
            cpad = val39(1)
            kh = val39(2)
            if (satm .gt. un .or. satm .lt. zero) then
                retcom = 2
                goto 500
            endif
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
!
!
!
        else if (thmc.eq.'LIQU_GAZ') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_GAZ --------------------------------
! =====================================================================
            do 130 ii = 1, dim13+1
                val13(ii) = 0.0d0
130          continue
            do 141 ii = 1, dim14
                val14(ii) = 0.0d0
141          continue
            do 150 ii = 1, dim15
                val15(ii) = 0.0d0
150          continue
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 3, ncra13, val13, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_LIQU', 0, ' ',&
                        0.0d0, 2, ncra14, val14, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_GAZ', 0, ' ',&
                        0.0d0, 1, ncra15, val15, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra13(4), val13(4), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 2, ncra14(3), val14(3), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                            t, 1, ncra15(2), val15(2), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1m, val13(5), rbid1)
                call satuvg(vg, p1, val13(6), val13(7))
                elseif (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1m, 1, ncra13(5), val13(5), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, ncra13(5), val13(6), icodre,&
                            1)
            else
                call satura(hydr, p1m, val13(5), rbid1)
                call satura(hydr, p1, val13(6), val13(7))
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 1, ncra13(7), val13(8), icodre,&
                        0)
            rgaz = val13(1)
            rhod = val13(2)
            biot = val13(3)
            cpd = val13(4)
            satm = val13(5)
            satur = val13(6)
            dsatur = val13(7)
            em = val13(8)
            rhol = val14(1)
            unsurk = val14(2)
            alpha = val14(3)
            cpl = val14(4)
            mamolg = val15(1)
            cpg = val15(2)
            if (satm .gt. un .or. satm .lt. zero) then
                retcom = 2
                goto 500
            endif
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
        else if (thmc.eq.'LIQU_GAZ_ATM') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM ----------------------------
! =====================================================================
            do 160 ii = 1, dim16+1
                val16(ii) = 0.0d0
160          continue
            do 170 ii = 1, dim17
                val17(ii) = 0.0d0
170          continue
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 2, ncra16, val16, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_LIQU', 0, ' ',&
                        0.0d0, 2, ncra17, val17, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra16(3), val16(3), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 2, ncra17(3), val17(3), icodre,&
                            1)
            endif
            if (hydr .eq. 'HYDR_UTIL' .or. hydr .eq. 'HYDR_ENDO') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1m, 1, ncra16(4), val16(4), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, ncra16(4), val16(5), icodre,&
                            1)
            else
                call satura(hydr, p1m, val16(4), rbid1)
                call satura(hydr, p1, val16(5), val16(6))
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 1, ncra16(6), val16(7), icodre,&
                        0)
            rhod = val16(1)
            biot = val16(2)
            cpd = val16(3)
            satm = val16(4)
            satur = val16(5)
            dsatur = val16(6)
            em = val16(7)
            rhol = val17(1)
            unsurk = val17(2)
            alpha = val17(3)
            cpl = val17(4)
            if (satm .gt. un .or. satm .lt. zero) then
                retcom = 2
                goto 500
            endif
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
        endif
    else if (etape.eq.'SATURATI') then
        if (hydr .eq. 'HYDR_UTIL' .or. hydr .eq. 'HYDR_ENDO') then
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                        p1, dimsat, nsat, valsat, icodre,&
                        1)
            satur = valsat(1)
            dsatur = valsat(2)
        else if (hydr.eq.'HYDR') then
            call satura(hydr, p1, satur, dsatur)
        else
            call u2mess('F', 'ALGORITH16_95')
        endif
        if (satur .gt. un .or. satur .lt. zero) then
            retcom = 2
            goto 500
        endif
    else if (etape.eq.'FINALE') then
! =====================================================================
! --- CAS FINAL -------------------------------------------------------
! =====================================================================
        call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                    0.0d0, 1, 'BIOT_COE', biot, icodre,&
                    1)
        if (thmc .eq. 'LIQU_SATU') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_SATU -------------------------------
! =====================================================================
            do 180 ii = 1, dim18
                val18(ii) = 0.0d0
180          continue
            do 190 ii = 1, dim19
                val19(ii) = 0.0d0
190          continue
!
!       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
!
            val18(8) = 1.0d0
!
!       INITIALISATION POUR L'ANISOTROPIE
!
            val18( 4) = 1.0d0
            val18(11) = 1.0d0
            val18(12) = 1.0d0
            val18(13) = 1.0d0
            val18(14) = 0.d0
            val18(15) = 0.d0
            val18(16) = 0.d0
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 3, ncra18(1), val18(1), icodre,&
                        0)
            if ((hydr.eq.'HYDR_UTIL') .or. (hydr.eq.'HYDR_VGM') .or. ( hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 1, ncra18(4), val18(4), icodre,&
                            0)
            else if (hydr.eq.'HYDR_ENDO') then
                if ((meca.eq.'MAZARS') .or. ( meca.eq.'ENDO_ISOT_BETON')) then
! =====================================================================
! --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL18 POUR ENDO ----
! =====================================================================
                    call rcvala(imate, ' ', 'THM_DIFFU', 1, 'ENDO',&
                                endo, 1, ncra18(5), val18(4), icodre,&
                                1)
                endif
            endif
            call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                        t, dim19-1, ncra19, val19, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra18(6), val18(6), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra18(7), val18(7), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 2, ncra18(8), val18(8), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.d0, 1, ncra18(10), val18(10), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 1, ncra19(4), val19(4), icodre,&
                            1)
            endif
!
!--- TENSEUR ISOTOPE LE CAS ECHEANT
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 6, ncra18(11), val18(11), icodre,&
                        0)
!
!
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'INST',&
                        instap, 1, 'PESA_MUL', fpesa, icodre,&
                        0)
! COMME IL N'EST PAS POSSIBLE D'AFFECTER UNE VALEUR PAR DEFAUT
! A LA FONCTION PESA_MULT DANS LE FICHIER DE COMMANDE
! DEFI_MATERIAU.CAPY, ON UTILISE LE CODE RETOUR POUR LA METTRE
! A SA VALEUR PAR DEFAUT (EGALE A 1) SI ELLE N A PAS ETE
! DEFINIE DANS LE FICHIER DE COMMANDE
            if (icodre(1) .eq. 1) then
                fpesa=1.d0
            endif
!
            pesa(1) = fpesa*val18(1)
            pesa(2) = fpesa*val18(2)
            pesa(3) = fpesa*val18(3)
            permfh = val18(4)
            lambt = val18(6)
            dlambt = val18(7)
            lambp = val18(8)
            dlambp = val18(9)
            lambct = val18(10)
            lambs = 1.0d0
            dlambs = 0.0d0
            satur = 1.0d0
            dsatur = 0.0d0
            unsurk = val19(1)
            viscl = val19(2)
            dviscl = val19(3)
            alpha = val19(4)
            isot(1) = val18(11)
            isot(2) = val18(12)
            isot(3) = val18(13)
            isot(4) = val18(14)
            isot(5) = val18(15)
            isot(6) = val18(16)
        else if (thmc.eq.'GAZ') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE GAZ -------------------------------------
! =====================================================================
            do 200 ii = 1, dim20
                val20(ii) = 0.0d0
200          continue
            do 210 ii = 1, dim21
                val21(ii) = 0.0d0
210          continue
!
!       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
!
            val20(9) = 1.0d0
!
!       INITIALISATION POUR L'ANISOTROPIE
!
            val20(5) = 1.0d0
            val20(12) = 1.0d0
            val20(13) = 1.0d0
            val20(14) = 1.0d0
            val20(15) = 0.d0
            val20(16) = 0.d0
            val20(17) = 0.d0
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, dim20-7, ncra20(1), val20(1), icodre,&
                        0)
            if ((hydr.eq.'HYDR_UTIL') .or. (hydr.eq.'HYDR_VGM') .or. ( hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 1, ncra20(5), val20(5), icodre,&
                            0)
            else if (hydr.eq.'HYDR_ENDO') then
                if ((meca.eq.'MAZARS') .or. ( meca.eq.'ENDO_ISOT_BETON')) then
! =====================================================================
! --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL21 POUR ENDO ----
! =====================================================================
                    call rcvala(imate, ' ', 'THM_DIFFU', 1, 'ENDO',&
                                endo, 1, ncra20(6), val20(5), icodre,&
                                1)
                endif
            endif
            call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                        t, dim21, ncra21, val21, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra20(7), val20(7), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra20(8), val20(8), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 2, ncra20(9), val20(9), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.d0, 1, ncra20(11), val20(11), icodre,&
                            0)
            endif
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.d0, 6, ncra20(12), val20(12), icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'INST',&
                        instap, 1, 'PESA_MUL', fpesa, icodre,&
                        0)
! COMME IL N'EST PAS POSSIBLE D'AFFECTER UNE VALEUR PAR DEFAUT
! A LA FONCTION PESA_MULT DANS LE FICHIER DE COMMANDE
! DEFI_MATERIAU.CAPY, ON UTILISE LE CODE RETOUR POUR LA METTRE
! A SA VALEUR PAR DEFAUT (EGALE A 1) SI ELLE N A PAS ETE
! DEFINIE DANS LE FICHIER DE COMMANDE
            if (icodre(1) .eq. 1) then
                fpesa=1.d0
            endif
            rgaz = val20( 1)
            pesa(1) = val20( 2)*fpesa
            pesa(2) = val20( 3)*fpesa
            pesa(3) = val20( 4)*fpesa
            permfh = val20( 5)
            lambt = val20( 7)
            dlambt = val20( 8)
            lambp = val20( 9)
            dlambp = val20(10)
            lambct = val20(11)
            lambs = 1.0d0
            dlambs = 0.0d0
            satur = 1.0d0
            dsatur = 0.0d0
            mamolg = val21( 1)
            viscg = val21( 2)
            dviscg = val21( 3)
            lambs = 0.0d0
            dlambs = 0.0d0
            isot(1) = val20(12)
            isot(2) = val20(13)
            isot(3) = val20(14)
            isot(4) = val20(15)
            isot(5) = val20(16)
            isot(6) = val20(17)
        else if (thmc.eq.'LIQU_VAPE') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_VAPE -------------------------------
! =====================================================================
            do 220 ii = 1, dim22
                val22(ii) = 0.0d0
220          continue
            do 230 ii = 1, dim23
                val23(ii) = 0.0d0
230          continue
            do 241 ii = 1, dim24
                val24(ii) = 0.0d0
241          continue
!
!       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
!
            val22(9) = 1.0d0
            val22(11) = 1.0d0
!
!       INITIALISATION POUR L'ANISOTROPIE
            val22(5) = 1.0d0
            val22(21) = 1.0d0
            val22(22) = 1.0d0
            val22(23) = 1.0d0
            val22(24) = 0.d0
            val22(25) = 0.d0
            val22(26) = 0.d0
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 4, ncra22, val22, icodre,&
                        0)
            if ((hydr.eq.'HYDR_UTIL') .or. (hydr.eq.'HYDR_VGM') .or. ( hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 1, ncra22(5), val22(5), icodre,&
                            0)
            else if (hydr.eq.'HYDR_ENDO') then
                if ((meca.eq.'MAZARS') .or. ( meca.eq.'ENDO_ISOT_BETON')) then
! =====================================================================
! --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
! =====================================================================
                    call rcvala(imate, ' ', 'THM_DIFFU', 1, 'ENDO',&
                                endo, 1, ncra22(6), val22(5), icodre,&
                                1)
                endif
            endif
            call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                        t, 3, ncra23, val23, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_VAPE_GAZ', 1, 'TEMP',&
                        t, 3, ncra24, val24, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra22(7), val22(7), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra22(8), val22(8), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 2, ncra22(9), val22(9), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.d0, 1, ncra22(13), val22(13), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, dim23-3, ncra23(4), val23(4), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, pvp-p1, val22(14), val22(15))
                if (hydr .eq. 'HYDR_VGM') then
                    call permvg(vg, val22(14), val22(16), val22(17), val22(18),&
                                val22(19))
                else
                    call permvc(vg, val22(14), val22(16), val22(17), val22(18),&
                                val22(19))
                endif
                val22(20) = 0.d0
                elseif (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            pvp-p1, 2, ncra22(14), val22(14), icodre,&
                            1)
            else
                call satura(hydr, pvp-p1, val22(14), val22(15))
            endif
            nompar(1) = 'SAT'
            nompar(2) = 'PGAZ'
            valpar(1) = val22(14)
            valpar(2) = p2
            if ((hydr.ne.'HYDR_VGM') .and. (hydr.ne.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                            valpar, 5, ncra22(16), val22(16), icodre,&
                            1)
            endif
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'SAT',&
                            valpar( 1), 2, ncra22(11), val22(11), icodre,&
                            0)
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.d0, 6, ncra22(21), val22(21), icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'INST',&
                        instap, 1, 'PESA_MUL', fpesa, icodre,&
                        0)
! COMME IL N'EST PAS POSSIBLE D'AFFECTER UNE VALEUR PAR DEFAUT
! A LA FONCTION PESA_MULT DANS LE FICHIER DE COMMANDE
! DEFI_MATERIAU.CAPY, ON UTILISE LE CODE RETOUR POUR LA METTRE
! A SA VALEUR PAR DEFAUT (EGALE A 1) SI ELLE N A PAS ETE
! DEFINIE DANS LE FICHIER DE COMMANDE
            if (icodre(1) .eq. 1) then
                fpesa=1.d0
            endif
            rgaz = val22( 1)
            pesa(1) = val22( 2)*fpesa
            pesa(2) = val22( 3)*fpesa
            pesa(3) = val22( 4)*fpesa
            permfh = val22( 5)
            lambt = val22( 7)
            dlambt = val22( 8)
            lambp = val22( 9)
            dlambp = val22(10)
            lambs = val22(11)
            dlambs = val22(12)
            lambct = val22(13)
            satur = val22(14)
            dsatur = val22(15)
            permli = val22(16)
            dperml = val22(17)
            permgz = val22(18)
            dperms = val22(19)
            dpermp = val22(20)
            unsurk = val23( 1)
            viscl = val23( 2)
            dviscl = val23( 3)
            alpha = val23( 4)
            mamolv = val24( 1)
            viscvg = val24( 2)
            dvisvg = val24( 3)
            isot(1) = val22(21)
            isot(2) = val22(22)
            isot(3) = val22(23)
            isot(4) = val22(24)
            isot(5) = val22(25)
            isot(6) = val22(26)
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
        else if (thmc.eq.'LIQU_VAPE_GAZ') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ---------------------------
! =====================================================================
            do 250 ii = 1, dim25
                val25(ii) = 0.0d0
250          continue
            do 260 ii = 1, dim26
                val26(ii) = 0.0d0
260          continue
            do 270 ii = 1, dim27
                val27(ii) = 0.0d0
270          continue
            do 280 ii = 1, dim28
                val28(ii) = 0.0d0
280          continue
!
!       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
!
            val25(9) = 1.0d0
            val25(11) = 1.0d0
!
!       INITIALISATION POUR L'ANISOTROPIE
!
            val25(5) = 1.0d0
            val25(27) = 1.0d0
            val25(28) = 1.0d0
            val25(29) = 1.0d0
            val25(30) = 0.0d0
            val25(31) = 0.0d0
            val25(32) = 0.0d0
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 4, ncra25, val25, icodre,&
                        0)
            if ((hydr.eq.'HYDR_UTIL') .or. (hydr.eq.'HYDR_VGM') .or. ( hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 1, ncra25(5), val25(5), icodre,&
                            0)
            else if (hydr.eq.'HYDR_ENDO') then
                if ((meca.eq.'MAZARS') .or. ( meca.eq.'ENDO_ISOT_BETON')) then
! =====================================================================
! --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
! =====================================================================
                    call rcvala(imate, ' ', 'THM_DIFFU', 1, 'ENDO',&
                                endo, 1, ncra25(6), val25(5), icodre,&
                                1)
                endif
            endif
            call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                        t, 3, ncra26, val26, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                        t, 3, ncra27, val27, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_VAPE_GAZ', 0, ' ',&
                        0.0d0, 1, ncra28, val28, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra25(7), val25(7), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra25(8), val25(8), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 2, ncra25(9), val25(9), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.d0, 1, ncra25(13), val25(13), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, 1, ncra26(4), val26(4), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1, val25(14), val25(15))
                if (hydr .eq. 'HYDR_VGM') then
                    call permvg(vg, val25(14), val25(16), val25(17), val25(18),&
                                val25(19))
                else
                    call permvc(vg, val25(14), val25(16), val25(17), val25(18),&
                                val25(19))
                endif
                val25(20) = 0.d0
                else if (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, ncra25(14), val25(14), icodre,&
                            1)
            else
                call satura(hydr, p1, val25(14), val25(15))
            endif
            nompar(1) = 'SAT'
            nompar(2) = 'PGAZ'
            nompar(3) = 'TEMP'
            valpar(1) = val25(14)
            valpar(2) = p2
            valpar(3) = t
            if ((hydr.ne.'HYDR_VGM') .and. (hydr.ne.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 3, nompar,&
                            valpar, 5, ncra25(16), val25(16), icodre,&
                            1)
            endif
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'SAT',&
                            valpar( 1), 2, ncra25(11), val25(11), icodre,&
                            0)
            endif
!
!    RÉCUPÉRATION DES FONCTIONS FICKS ET LEUR DÉRIVÉES AU DESSUS PB
!
            nompar(1) = 'TEMP'
            valpar(1) = t
!
            call rcvala(imate, ' ', 'THM_DIFFU', 1, nompar,&
                        valpar, 1, ncra25(21), val25(21), icodre,&
                        1)
            nompar(1) = 'PVAP'
            nompar(2) = 'PGAZ'
            nompar(3) = 'SAT'
            valpar(1) = pvp
            valpar(2) = p2
            valpar(3) = val25(14)
!
!           DERIVEE PAR RAPPORT A S MISE 0 0 PAR DEFAUT
!
            val25(30) = 0.d0
            call rcvala(imate, ' ', 'THM_DIFFU', 1, nompar(3),&
                        valpar( 3), 1, ncra25(33), val25(33), icodre,&
                        0)
!
! INITIALISATION DES AUTRES COMPOSANTES FICKIENNES
!
            val25(22) = 1.0d0
            val25(23) = 1.0d0
            val25(24) = 1.0d0
            call rcvala(imate, ' ', 'THM_DIFFU', 3, nompar,&
                        valpar, 3, ncra25(22), val25(22), icodre,&
                        0)
            nompar(1) = 'TEMP'
            nompar(2) = 'PGAZ'
            valpar(1) = t
            valpar(2) = p2
            call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                        valpar, 2, ncra25(25), val25(25), icodre,&
                        0)
!
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.d0, 6, ncra25(27), val25(27), icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'INST',&
                        instap, 1, 'PESA_MUL', fpesa, icodre,&
                        0)
! COMME IL N'EST PAS POSSIBLE D'AFFECTER UNE VALEUR PAR DEFAUT
! A LA FONCTION PESA_MULT DANS LE FICHIER DE COMMANDE
! DEFI_MATERIAU.CAPY, ON UTILISE LE CODE RETOUR POUR LA METTRE
! A SA VALEUR PAR DEFAUT (EGALE A 1) SI ELLE N A PAS ETE
! DEFINIE DANS LE FICHIER DE COMMANDE
            if (icodre(1) .eq. 1) then
                fpesa=1.d0
            endif
            rgaz = val25( 1)
            pesa(1) = val25( 2)*fpesa
            pesa(2) = val25( 3)*fpesa
            pesa(3) = val25( 4)*fpesa
            permfh = val25( 5)
            lambt = val25( 7)
            dlambt = val25( 8)
            lambp = val25( 9)
            dlambp = val25(10)
            lambs = val25(11)
            dlambs = val25(12)
            lambct = val25(13)
            satur = val25(14)
            dsatur = val25(15)
            permli = val25(16)
            dperml = val25(17)
            permgz = val25(18)
            dperms = val25(19)
            dpermp = val25(20)
!
            fick = val25(21)*val25(22)*val25(23)*val25(24)
            dfickt = val25(25)*val25(22)*val25(23)*val25(24)
            dfickg = val25(26)*val25(21)*val25(22)*val25(24)
            dficks = val25(21)*val25(22)*val25(23)*val25(33)
            unsurk = val26( 1)
            viscl = val26( 2)
            dviscl = val26( 3)
            alpha = val26( 4)
            mamolg = val27( 1)
            viscg = val27( 2)
            dviscg = val27( 3)
            mamolv = val28( 1)
            isot(1) = val25(27)
            isot(2) = val25(28)
            isot(3) = val25(29)
            isot(4) = val25(30)
            isot(5) = val25(31)
            isot(6) = val25(32)
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
!
        else if (thmc.eq.'LIQU_AD_GAZ_VAPE') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE PROVISOIRE--------------
! =====================================================================
            do 251 ii = 1, dim40
                val40(ii) = 0.0d0
251          continue
            do 261 ii = 1, dim41
                val41(ii) = 0.0d0
261          continue
            do 271 ii = 1, dim42
                val42(ii) = 0.0d0
271          continue
            do 281 ii = 1, dim43
                val43(ii) = 0.0d0
281          continue
!
!       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
!
            val40(9) = 1.0d0
            val40(11) = 1.0d0
!
!       INITIALISATION POUR L'ANISOTROPIE
!
            val40(5) = 1.0d0
            val40(32) = 1.0d0
            val40(33) = 1.0d0
            val40(34) = 1.0d0
!
            val40(35) = 0.0d0
            val40(36) = 0.0d0
            val40(37) = 0.0d0
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 4, ncra40, val40, icodre,&
                        0)
            if (hydr .eq. 'HYDR_UTIL' .or. hydr .eq. 'HYDR_VGM' .or. hydr .eq. 'HYDR_VGC') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 1, ncra40(5), val40(5), icodre,&
                            0)
            else if (hydr.eq.'HYDR_ENDO') then
                if ((meca.eq.'MAZARS') .or. ( meca.eq.'ENDO_ISOT_BETON')) then
! =====================================================================
! --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
! =====================================================================
                    call rcvala(imate, ' ', 'THM_DIFFU', 1, 'ENDO',&
                                endo, 1, ncra40(6), val40(5), icodre,&
                                1)
                endif
            endif
            call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                        t, 3, ncra41, val41, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                        t, 3, ncra42, val42, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_VAPE_GAZ', 0, ' ',&
                        0.0d0, 1, ncra43, val43, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra40(7), val40(7), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra40(8), val40(8), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 2, ncra40(9), val40(9), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.d0, 1, ncra40(13), val40(13), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, dim41-3, ncra41(4), val41(4), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1, val40(14), val40(15))
                if (hydr .eq. 'HYDR_VGM') then
                    call permvg(vg, val40(14), val40(16), val40(17), val40(18),&
                                val40(19))
                else
                    call permvc(vg, val40(14), val40(16), val40(17), val40(18),&
                                val40(19))
                endif
                val40(20) = 0.d0
!
!
                elseif (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, ncra40(14), val40(14), icodre,&
                            1)
            else
                call satura(hydr, p1, val40(14), val40(15))
            endif
            nompar(1) = 'SAT'
            nompar(2) = 'PGAZ'
            nompar(3) = 'TEMP'
            valpar(1) = val40(14)
            valpar(2) = p2
            valpar(3) = t
            if ((hydr.ne.'HYDR_VGM') .and. (hydr.ne.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 3, nompar,&
                            valpar, 5, ncra40(16), val40(16), icodre,&
                            1)
            endif
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'SAT',&
                            valpar( 1), 2, ncra40(11), val40(11), icodre,&
                            0)
            endif
!
!    RECUPERATION DES FONCTIONS FICK VAPEUR ET LEURS DERIVEES
!
            nompar(1) = 'TEMP'
            valpar(1) = t
            call rcvala(imate, ' ', 'THM_DIFFU', 1, nompar,&
                        valpar, 1, ncra40(21), val40(21), icodre,&
                        1)
!
! INITIALISATION DES AUTRES COMPOSANTES FICKIENNES
!
            val40(22) = 1.0d0
            val40(23) = 1.0d0
            val40(24) = 1.0d0
!
            nompar(1) = 'PVAP'
            nompar(2) = 'PGAZ'
            nompar(3) = 'SAT'
            valpar(1) = pvp
            valpar(2) = p2
            valpar(3) = val40(14)
!
!
!
!           DERIVEE PAR RAPPORT A S MISE 0 0 PAR DEFAUT
!
            val40(35) = 0.d0
            call rcvala(imate, ' ', 'THM_DIFFU', 1, nompar(3),&
                        valpar( 3), 1, ncra40(38), val40(38), icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_DIFFU', 3, nompar,&
                        valpar, 3, ncra40(22), val40(22), icodre,&
                        0)
            nompar(1) = 'TEMP'
            nompar(2) = 'PGAZ'
            valpar(1) = t
            valpar(2) = p2
            call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                        valpar, 2, ncra40(25), val40(25), icodre,&
                        0)
!
!
!    RECUPERATION DES FONCTIONS FICK AIR DISSOUS ET LEURS DERIVEES
!
            nompar(1) = 'TEMP'
            nompar(2) = 'PAD'
            nompar(3) = 'PLIQ'
            nompar(4) = 'SAT'
            valpar(1) = t
            valpar(2) = pad
            valpar(3) = p2-p1
            valpar(4) = val40(14)
!
! INITIALISATION DES AUTRES COMPOSANTES FICKIENNES
!
            val40(28) = 1.0d0
            val40(29) = 1.0d0
            val40(30) = 1.0d0
            call rcvala(imate, ' ', 'THM_DIFFU', 4, nompar,&
                        valpar, 4, ncra40(27), val40(27), icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                        t, 1, ncra40(31), val40(31), icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.d0, 6, ncra40(32), val40(32), icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'INST',&
                        instap, 1, 'PESA_MUL', fpesa, icodre,&
                        0)
! COMME IL N'EST PAS POSSIBLE D'AFFECTER UNE VALEUR PAR DEFAUT
! A LA FONCTION PESA_MULT DANS LE FICHIER DE COMMANDE
! DEFI_MATERIAU.CAPY, ON UTILISE LE CODE RETOUR POUR LA METTRE
! A SA VALEUR PAR DEFAUT (EGALE A 1) SI ELLE N A PAS ETE
! DEFINIE DANS LE FICHIER DE COMMANDE
            if (icodre(1) .eq. 1) then
                fpesa=1.d0
            endif
            rgaz = val40( 1)
            pesa(1) = val40( 2)*fpesa
            pesa(2) = val40( 3)*fpesa
            pesa(3) = val40( 4)*fpesa
            permfh = val40( 5)
            lambt = val40( 7)
            dlambt = val40( 8)
            lambp = val40( 9)
            dlambp = val40(10)
            lambs = val40(11)
            dlambs = val40(12)
            lambct = val40(13)
            satur = val40(14)
            dsatur = val40(15)
            permli = val40(16)
            dperml = val40(17)
            permgz = val40(18)
            dperms = val40(19)
            dpermp = val40(20)
!
            fick = val40(21)*val40(22)*val40(23)*val40(24)
            dfickt= val40(25) * val40(22)*val40(23)*val40(24)
            dfickg = val40(26)* val40(21)*val40(22)*val40(24)
            dficks = val40(21)*val40(22)*val40(23)*val40(38)
            fickad = val40(27)*val40(28)*val40(29)*val40(30)
            dfadt= val40(31)*val40(28)*val40(29)*val40(30)
!
            unsurk = val41( 1)
            viscl = val41( 2)
            dviscl = val41( 3)
            alpha = val41( 4)
            mamolg = val42( 1)
            viscg = val42( 2)
            dviscg = val42( 3)
            mamolv = val43( 1)
            isot(1) = val40(32)
            isot(2) = val40(33)
            isot(3) = val40(34)
            isot(4) = val40(35)
            isot(5) = val40(36)
            isot(6) = val40(37)
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
!
        else if (thmc.eq.'LIQU_AD_GAZ') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ--------------
! =====================================================================
            do 9251 ii = 1, dim40
                val40(ii) = 0.0d0
9251          continue
            do 9261 ii = 1, dim41
                val41(ii) = 0.0d0
9261          continue
            do 9271 ii = 1, dim42
                val42(ii) = 0.0d0
9271          continue
!
!       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
!
            val40(9) = 1.0d0
            val40(11) = 1.0d0
!
!       INITIALISATION POUR L'ANISOTROPIE
!
            val40(5) = 1.0d0
            val40(32) = 1.0d0
            val40(33) = 1.0d0
            val40(34) = 1.0d0
!
            val40(35) = 0.0d0
            val40(36) = 0.0d0
            val40(37) = 0.0d0
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 4, crad40, val40, icodre,&
                        0)
            if ((hydr.eq.'HYDR_UTIL') .or. (hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 1, crad40(5), val40(5), icodre,&
                            0)
            else if (hydr.eq.'HYDR_ENDO') then
                if ((meca.eq.'MAZARS') .or. ( meca.eq.'ENDO_ISOT_BETON')) then
! =====================================================================
! --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
! =====================================================================
                    call rcvala(imate, ' ', 'THM_DIFFU', 1, 'ENDO',&
                                endo, 1, crad40(6), val40(5), icodre,&
                                1)
                endif
            endif
            call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                        t, 3, crad41, val41, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                        t, 3, crad42, val42, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, crad40(7), val40(7), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, crad40(8), val40(8), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 2, crad40(9), val40(9), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.d0, 1, crad40(13), val40(13), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, dim41-3, crad41(4), val41(4), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1, val40(14), val40(15))
                if (hydr .eq. 'HYDR_VGM') then
                    call permvg(vg, val40(14), val40(16), val40(17), val40(18),&
                                val40(19))
                else
                    call permvc(vg, val40(14), val40(16), val40(17), val40(18),&
                                val40(19))
                endif
                val40(20) = 0.d0
!
!
                elseif (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, crad40(14), val40(14), icodre,&
                            1)
            else
                call satura(hydr, p1, val40(14), val40(15))
            endif
            nompar(1) = 'SAT'
            nompar(2) = 'PGAZ'
            nompar(3) = 'TEMP'
            valpar(1) = val40(14)
            valpar(2) = p2
            valpar(3) = t
            if ((hydr.ne.'HYDR_VGM') .and. (hydr.ne.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 3, nompar,&
                            valpar, 5, crad40(16), val40(16), icodre,&
                            1)
            endif
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'SAT',&
                            valpar( 1), 2, crad40(11), val40(11), icodre,&
                            0)
            endif
!
            nompar(1) = 'PVAP'
            nompar(2) = 'PGAZ'
            nompar(3) = 'SAT'
            valpar(1) = pvp
            valpar(2) = p2
            valpar(3) = val40(14)
!
!
!
!           DERIVEE PAR RAPPORT A S MISE 0 0 PAR DEFAUT
!
            val40(35) = 0.d0
!
!    RECUPERATION DES FONCTIONS FICK AIR DISSOUS ET LEURS DERIVEES
!
            nompar(1) = 'TEMP'
            nompar(2) = 'PAD'
            nompar(3) = 'PLIQ'
            nompar(4) = 'SAT'
            valpar(1) = t
            valpar(2) = pad
            valpar(3) = p2-p1
            valpar(4) = val40(14)
!
! INITIALISATION DES AUTRES COMPOSANTES FICKIENNES
!
            val40(28) = 1.0d0
            val40(29) = 1.0d0
            val40(30) = 1.0d0
            call rcvala(imate, ' ', 'THM_DIFFU', 4, nompar,&
                        valpar, 4, crad40(27), val40(27), icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                        t, 1, crad40(31), val40(31), icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.d0, 6, crad40(32), val40(32), icodre,&
                        0)
!
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'INST',&
                        instap, 1, 'PESA_MUL', fpesa, icodre,&
                        0)
! COMME IL N'EST PAS POSSIBLE D'AFFECTER UNE VALEUR PAR DEFAUT
! A LA FONCTION PESA_MULT DANS LE FICHIER DE COMMANDE
! DEFI_MATERIAU.CAPY, ON UTILISE LE CODE RETOUR POUR LA METTRE
! A SA VALEUR PAR DEFAUT (EGALE A 1) SI ELLE N A PAS ETE
! DEFINIE DANS LE FICHIER DE COMMANDE
            if (icodre(1) .eq. 1) then
                fpesa=1.d0
            endif
            rgaz = val40( 1)
            pesa(1) = val40( 2)*fpesa
            pesa(2) = val40( 3)*fpesa
            pesa(3) = val40( 4)*fpesa
            permfh = val40( 5)
            lambt = val40( 7)
            dlambt = val40( 8)
            lambp = val40( 9)
            dlambp = val40(10)
            lambs = val40(11)
            dlambs = val40(12)
            lambct = val40(13)
            satur = val40(14)
            dsatur = val40(15)
            permli = val40(16)
            dperml = val40(17)
            permgz = val40(18)
            dperms = val40(19)
            dpermp = val40(20)
!
            fick = val40(21)*val40(22)*val40(23)*val40(24)
            dfickt= val40(25) * val40(22)*val40(23)*val40(24)
            dfickg = val40(26)* val40(21)*val40(22)*val40(24)
            dficks = val40(21)*val40(22)*val40(23)*val40(38)
            fickad = val40(27)*val40(28)*val40(29)*val40(30)
            dfadt= val40(31)*val40(28)*val40(29)*val40(30)
!
            unsurk = val41( 1)
            viscl = val41( 2)
            dviscl = val41( 3)
            alpha = val41( 4)
            mamolg = val42( 1)
            viscg = val42( 2)
            dviscg = val42( 3)
            isot(1) = val40(32)
            isot(2) = val40(33)
            isot(3) = val40(34)
            isot(4) = val40(35)
            isot(5) = val40(36)
            isot(6) = val40(37)
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
!
!
        else if (thmc.eq.'LIQU_GAZ') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_GAZ --------------------------------
! =====================================================================
            do 290 ii = 1, dim29
                val29(ii) = 0.0d0
290          continue
            do 300 ii = 1, dim30
                val30(ii) = 0.0d0
300          continue
            do 310 ii = 1, dim31
                val31(ii) = 0.0d0
310          continue
!
!       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
!
            val29(9) = 1.0d0
            val29(11) = 1.0d0
!
!       INITIALISATION POUR L'ANISOTROPIE
!
            val29(5) = 1.0d0
            val29(21) = 1.0d0
            val29(22) = 1.0d0
            val29(23) = 1.0d0
            val29(24) = 0.0d0
            val29(25) = 0.0d0
            val29(26) = 0.0d0
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 4, ncra29, val29, icodre,&
                        0)
            if ((hydr.eq.'HYDR_UTIL') .or. (hydr.eq.'HYDR_VGM') .or. ( hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 1, ncra29(5), val29(5), icodre,&
                            0)
            else if (hydr.eq.'HYDR_ENDO') then
                if ((meca.eq.'MAZARS') .or. ( meca.eq.'ENDO_ISOT_BETON')) then
! =====================================================================
! --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL29 POUR ENDO ----
! =====================================================================
                    call rcvala(imate, ' ', 'THM_DIFFU', 1, 'ENDO',&
                                endo, 1, ncra29(6), val29(5), icodre,&
                                1)
                endif
            endif
            call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                        t, 3, ncra30, val30, icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_GAZ', 1, 'TEMP',&
                        t, 3, ncra31, val31, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra29(7), val29(7), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra29(8), val29(8), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 2, ncra29(9), val29(9), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.d0, 1, ncra29(13), val29(13), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, dim30-3, ncra30(4), val30(4), icodre,&
                            1)
            endif
            if ((hydr.eq.'HYDR_VGM') .or. (hydr.eq.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.0d0, 5, nvg(1), vg(1), icodre,&
                            1)
                if (icodre(1) .eq. 1) then
                    call u2mess('F', 'ALGORITH16_94')
                endif
                call satuvg(vg, p1, val29(14), val29(15))
                if (hydr .eq. 'HYDR_VGM') then
                    call permvg(vg, val29(14), val29(16), val29(17), val29(18),&
                                val29(19))
                else
                    call permvc(vg, val29(14), val29(16), val29(17), val29(18),&
                                val29(19))
                endif
                val29(20) = 0.d0
                else if (hydr.eq.'HYDR_UTIL' .or. hydr.eq.'HYDR_ENDO')&
            then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, ncra29(14), val29(14), icodre,&
                            1)
            else
                call satura(hydr, p1, val29(14), val29(15))
            endif
            nompar(1) = 'SAT'
            nompar(2) = 'PGAZ'
            valpar(1) = val29(14)
            valpar(2) = p2
            if ((hydr.ne.'HYDR_VGM') .and. (hydr.ne.'HYDR_VGC')) then
                call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                            valpar, 5, ncra29(16), val29(16), icodre,&
                            1)
                if (ther .ne. ' ') then
                endif
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'SAT',&
                            valpar( 1), 2, ncra29(11), val29(11), icodre,&
                            0)
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.d0, 6, ncra29(21), val29(21), icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'INST',&
                        instap, 1, 'PESA_MUL', fpesa, icodre,&
                        0)
! COMME IL N'EST PAS POSSIBLE D'AFFECTER UNE VALEUR PAR DEFAUT
! A LA FONCTION PESA_MULT DANS LE FICHIER DE COMMANDE
! DEFI_MATERIAU.CAPY, ON UTILISE LE CODE RETOUR POUR LA METTRE
! A SA VALEUR PAR DEFAUT (EGALE A 1) SI ELLE N A PAS ETE
! DEFINIE DANS LE FICHIER DE COMMANDE
            if (icodre(1) .eq. 1) then
                fpesa=1.d0
            endif
            rgaz = val29( 1)
            pesa(1) = val29( 2)*fpesa
            pesa(2) = val29( 3)*fpesa
            pesa(3) = val29( 4)*fpesa
            permfh = val29( 5)
            lambt = val29( 7)
            dlambt = val29( 8)
            lambp = val29( 9)
            dlambp = val29(10)
            lambs = val29(11)
            dlambs = val29(12)
            lambct = val29(13)
            satur = val29(14)
            dsatur = val29(15)
            permli = val29(16)
            dperml = val29(17)
            permgz = val29(18)
            dperms = val29(19)
            dpermp = val29(20)
            unsurk = val30( 1)
            viscl = val30( 2)
            dviscl = val30( 3)
            alpha = val30( 4)
            mamolg = val31( 1)
            viscg = val31( 2)
            dviscg = val31( 3)
            isot(1) = val29(21)
            isot(2) = val29(22)
            isot(3) = val29(23)
            isot(4) = val29(24)
            isot(5) = val29(25)
            isot(6) = val29(26)
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
        else if (thmc.eq.'LIQU_GAZ_ATM') then
! =====================================================================
! --- LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM ----------------------------
! =====================================================================
            do 320 ii = 1, dim32
                val32(ii) = 0.0d0
320          continue
            do 330 ii = 1, dim33
                val33(ii) = 0.0d0
330          continue
!
!       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
!
            val32(8) = 1.0d0
            val32(10) = 1.0d0
!
!       INITIALISATION POUR L'ANISOTROPIE
!
            val32(4) = 1.0d0
            val32(17) = 1.0d0
            val32(18) = 1.0d0
            val32(19) = 1.0d0
            val32(20) = 0.0d0
            val32(21) = 0.0d0
            val32(22) = 0.0d0
!
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.0d0, 3, ncra32, val32, icodre,&
                        0)
            if (hydr .eq. 'HYDR_UTIL') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 1, ncra32(4), val32(4), icodre,&
                            0)
            else if (hydr.eq.'HYDR_ENDO') then
                if ((meca.eq.'MAZARS') .or. ( meca.eq.'ENDO_ISOT_BETON')) then
! =====================================================================
! --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL32 POUR ENDO ----
! =====================================================================
                    call rcvala(imate, ' ', 'THM_DIFFU', 1, 'ENDO',&
                                endo, 1, ncra32(5), val32(4), icodre,&
                                1)
                endif
                else if ((hydr.eq.'HYDR_VGM').or.(hydr.eq.'HYDR_VGC'))&
            then
                call u2mess('F', 'ALGORITH16_95')
            endif
            call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                        t, 3, ncra33, val33, icodre,&
                        0)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra32(6), val32(6), icodre,&
                            1)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'TEMP',&
                            t, 1, ncra32(7), val32(7), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PORO',&
                            phi, 2, ncra32(8), val32(8), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                            0.d0, 1, ncra32(12), val32(12), icodre,&
                            0)
                call rcvala(imate, ' ', 'THM_LIQU', 1, 'TEMP',&
                            t, dim33-3, ncra33(4), val33(4), icodre,&
                            1)
            endif
            if (hydr .eq. 'HYDR_UTIL' .or. hydr .eq. 'HYDR_ENDO') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'PCAP',&
                            p1, 2, ncra32(13), val32(13), icodre,&
                            1)
            else
                call satura(hydr, p1, val32(13), val32(14))
            endif
            nompar(1) = 'SAT'
            nompar(2) = 'PGAZ'
            valpar(1) = val32(13)
            valpar(2) = p2
            call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                        valpar, 2, ncra32(15), val32(15), icodre,&
                        1)
            if (ther .ne. ' ') then
                call rcvala(imate, ' ', 'THM_DIFFU', 1, 'SAT',&
                            valpar( 1), 2, ncra32(11), val32(11), icodre,&
                            0)
            endif
            call rcvala(imate, ' ', 'THM_DIFFU', 0, ' ',&
                        0.d0, 6, ncra32(17), val32(17), icodre,&
                        0)
            call rcvala(imate, ' ', 'THM_DIFFU', 1, 'INST',&
                        instap, 1, 'PESA_MUL', fpesa, icodre,&
                        0)
! COMME IL N'EST PAS POSSIBLE D'AFFECTER UNE VALEUR PAR DEFAUT
! A LA FONCTION PESA_MULT DANS LE FICHIER DE COMMANDE
! DEFI_MATERIAU.CAPY, ON UTILISE LE CODE RETOUR POUR LA METTRE
! A SA VALEUR PAR DEFAUT (EGALE A 1) SI ELLE N A PAS ETE
! DEFINIE DANS LE FICHIER DE COMMANDE
            if (icodre(1) .eq. 1) then
                fpesa=1.d0
            endif
            pesa(1) = val32( 1)*fpesa
            pesa(2) = val32( 2)*fpesa
            pesa(3) = val32( 3)*fpesa
            permfh = val32( 4)
            lambt = val32( 6)
            dlambt = val32( 7)
            lambp = val32( 8)
            dlambp = val32( 9)
            lambs = val32(10)
            dlambs = val32(11)
            lambct = val32(12)
            satur = val32(13)
            dsatur = val32(14)
            permli = val32(15)
            dperml = val32(16)
            unsurk = val33( 1)
            viscl = val33( 2)
            dviscl = val33( 3)
            alpha = val33( 4)
            isot(1) = val32(17)
            isot(2) = val32(18)
            isot(3) = val32(19)
            isot(4) = val32(20)
            isot(5) = val32(21)
            isot(6) = val32(22)
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
        endif
        if (hydr .eq. 'HYDR') then
            call permea(imate, hydr, phi, t, satur,&
                        ncon, cond)
            permfh = cond(1)
            permli = cond(2)
            dperml = cond(3)
            permgz = cond(4)
            dperms = cond(5)
            dpermp = cond(6)
            fick = cond(7)
            dfickt = cond(8)
            dfickg = cond(9)
            if (satur .gt. un .or. satur .lt. zero) then
                retcom = 2
                goto 500
            endif
        endif
    endif
500  continue
!
! =====================================================================
end subroutine
