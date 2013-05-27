subroutine thmlec(imate, thmc, meca, hydr, ther,&
                  t, p1, p2, phi, end,&
                  pvp, pad, rgaz, biot, satur,&
                  dsatur, pesa, permfh, permli, dperml,&
                  permgz, dperms, dpermp, fick, dfickt,&
                  dfickg, lambp, dlambp, unsurk, alpha,&
                  lambs, dlambs, viscl, dviscl, mamolg,&
                  lambt, dlambt, viscg, dviscg, mamolv,&
                  fickad, dfadt, lambct, isot, dficks,&
                  instap)
! =====================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
! =====================================================================
! --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
! =====================================================================
    implicit none
    include 'asterfort/thmrcp.h'
    integer :: imate, retcom
    real(kind=8) :: t, p1, p2, phi, pvp
    real(kind=8) :: rgaz, biot, satur, dsatur, pesa(3)
    real(kind=8) :: permfh, permli, dperml, permgz, dperms, dpermp
    real(kind=8) :: fick, dfickt, dfickg, lambp, dlambp
    real(kind=8) :: alpha, lambs, dlambs, viscl, dviscl, end
    real(kind=8) :: lambt, dlambt, viscg, dviscg, mamolg, instap
    real(kind=8) :: mamolv, fickad, dfadt, pad, lambct, unsurk, isot(6)
    real(kind=8) :: dficks
    character(len=16) :: meca, thmc, ther, hydr
! =====================================================================
! --- VARIABLES LOCALES -----------------------------------------------
! =====================================================================
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6
    real(kind=8) :: rbid7, rbid8, rbid9
    real(kind=8) :: rbid11, rbid12, rbid13, rbid14, rbid15, rbid16
    real(kind=8) :: rbid17, rbid18, rbid19, rbid20, rbid21, rbid22
    real(kind=8) :: rbid23, rbid24, rbid25, rbid26, rbid27, rbid28
    real(kind=8) :: rbid29, rbid30, rbid31, rbid32
    real(kind=8) :: rbid35, rbid36, rbid37, rbid38, rbid39, rbid40
    real(kind=8) :: rbid41, rbid42, rbid43, rbid44, rbid45, rbid46
    real(kind=8) :: rbid47, rbid48, rbid49, rbid50
! =====================================================================
    rbid1=0.d0
    rbid2=0.d0
    rbid3=0.d0
    rbid4=0.d0
    rbid5=0.d0
    rbid6=0.d0
    rbid7=0.d0
    rbid8=0.d0
    rbid9=0.d0
    rbid11=0.d0
    rbid12=0.d0
    rbid13=0.d0
    rbid14=0.d0
    rbid15=0.d0
    rbid16=0.d0
    rbid17=0.d0
    rbid18=0.d0
    rbid19=0.d0
    rbid20=0.d0
    rbid21=0.d0
    rbid22=0.d0
    rbid23=0.d0
    rbid24=0.d0
    rbid25=0.d0
    rbid26=0.d0
    rbid27=0.d0
    rbid28=0.d0
    rbid29=0.d0
    rbid30=0.d0
    rbid31=0.d0
    rbid32=0.d0
    rbid35=0.d0
    rbid36=0.d0
    rbid37=0.d0
    rbid38=0.d0
    rbid39=0.d0
    rbid40=0.d0
    rbid41=0.d0
    rbid42=0.d0
    rbid43=0.d0
    rbid44=0.d0
    rbid45=0.d0
    rbid46=0.d0
    rbid47=0.d0
    rbid48=0.d0
    rbid49=0.d0
    rbid50=0.d0
    retcom=0
    if (thmc .eq. 'LIQU_SATU') then
        call thmrcp('FINALE  ', imate, thmc, meca, hydr,&
                    ther, rbid1, rbid2, rbid3, rbid4,&
                    rbid5, t, rbid6, rbid41, rbid7,&
                    phi, end, rbid11, rbid12, rbid13,&
                    rbid14, biot, rbid16, rbid17, rbid18,&
                    pesa, permfh, rbid19, rbid20, rbid21,&
                    rbid22, rbid23, rbid24, rbid25, rbid26,&
                    lambp, dlambp, rbid27, unsurk, alpha,&
                    rbid28, lambs, dlambs, viscl, dviscl,&
                    rbid31, rbid32, lambt, dlambt, rbid35,&
                    rbid36, rbid37, rbid38, rbid39, rbid40,&
                    rbid45, rbid46, rbid47, rbid48, rbid49,&
                    rbid50, lambct, isot, rbid50, instap,&
                    retcom)
    else if (thmc.eq.'GAZ') then
        call thmrcp('FINALE  ', imate, thmc, meca, hydr,&
                    ther, rbid1, rbid2, rbid3, rbid4,&
                    rbid5, t, rbid6, rbid44, rbid7,&
                    phi, end, rbid11, rgaz, rbid13,&
                    rbid14, biot, rbid16, rbid17, rbid18,&
                    pesa, permfh, rbid19, rbid20, rbid21,&
                    rbid22, rbid23, rbid24, rbid25, rbid26,&
                    lambp, dlambp, rbid27, rbid42, rbid43,&
                    rbid29, lambs, dlambs, rbid41, rbid31,&
                    mamolg, rbid28, lambt, dlambt, viscg,&
                    dviscg, rbid37, rbid38, rbid39, rbid40,&
                    rbid45, rbid46, rbid47, rbid48, rbid49,&
                    rbid50, lambct, isot, rbid50, instap,&
                    retcom)
    else if (thmc.eq.'LIQU_VAPE') then
        call thmrcp('FINALE  ', imate, thmc, meca, hydr,&
                    ther, rbid1, rbid2, rbid3, rbid4,&
                    rbid5, t, p1, rbid6, p2,&
                    phi, end, pvp, rgaz, rbid8,&
                    rbid9, biot, rbid11, satur, dsatur,&
                    pesa, permfh, permli, dperml, permgz,&
                    dperms, dpermp, rbid14, rbid15, rbid16,&
                    lambp, dlambp, rbid17, unsurk, alpha,&
                    rbid18, lambs, dlambs, viscl, dviscl,&
                    rbid19, rbid20, lambt, dlambt, rbid23,&
                    rbid24, mamolv, rbid25, viscg, dviscg,&
                    rbid45, rbid46, rbid47, rbid48, rbid49,&
                    rbid50, lambct, isot, rbid50, instap,&
                    retcom)
    else if (thmc.eq.'LIQU_VAPE_GAZ') then
        call thmrcp('FINALE  ', imate, thmc, meca, hydr,&
                    ther, rbid1, rbid2, rbid3, rbid4,&
                    rbid5, t, p1, rbid6, p2,&
                    phi, end, pvp, rgaz, rbid8,&
                    rbid9, biot, rbid11, satur, dsatur,&
                    pesa, permfh, permli, dperml, permgz,&
                    dperms, dpermp, fick, dfickt, dfickg,&
                    lambp, dlambp, rbid17, unsurk, alpha,&
                    rbid18, lambs, dlambs, viscl, dviscl,&
                    mamolg, rbid19, lambt, dlambt, viscg,&
                    dviscg, mamolv, rbid25, rbid26, rbid27,&
                    rbid45, rbid46, rbid47, rbid48, rbid49,&
                    rbid50, lambct, isot, dficks, instap,&
                    retcom)
    else if (thmc.eq.'LIQU_AD_GAZ_VAPE') then
        call thmrcp('FINALE  ', imate, thmc, meca, hydr,&
                    ther, rbid1, rbid2, rbid3, rbid4,&
                    rbid5, t, p1, rbid6, p2,&
                    phi, end, rbid28, rgaz, rbid8,&
                    rbid9, biot, rbid11, satur, dsatur,&
                    pesa, permfh, permli, dperml, permgz,&
                    dperms, dpermp, fick, dfickt, dfickg,&
                    lambp, dlambp, rbid17, unsurk, alpha,&
                    rbid18, lambs, dlambs, viscl, dviscl,&
                    mamolg, rbid19, lambt, dlambt, viscg,&
                    dviscg, mamolv, rbid25, rbid26, rbid27,&
                    fickad, dfadt, rbid47, rbid48, pad,&
                    rbid50, lambct, isot, dficks, instap,&
                    retcom)
!
    else if (thmc.eq.'LIQU_AD_GAZ') then
        call thmrcp('FINALE  ', imate, thmc, meca, hydr,&
                    ther, rbid1, rbid2, rbid3, rbid4,&
                    rbid5, t, p1, rbid6, p2,&
                    phi, end, rbid28, rgaz, rbid8,&
                    rbid9, biot, rbid11, satur, dsatur,&
                    pesa, permfh, permli, dperml, permgz,&
                    dperms, dpermp, rbid50, rbid50, rbid50,&
                    lambp, dlambp, rbid17, unsurk, alpha,&
                    rbid18, lambs, dlambs, viscl, dviscl,&
                    mamolg, rbid19, lambt, dlambt, viscg,&
                    dviscg, rbid50, rbid25, rbid26, rbid27,&
                    fickad, dfadt, rbid47, rbid48, pad,&
                    rbid50, lambct, isot, dficks, instap,&
                    retcom)
    else if (thmc.eq.'LIQU_GAZ') then
        call thmrcp('FINALE  ', imate, thmc, meca, hydr,&
                    ther, rbid1, rbid2, rbid3, rbid4,&
                    rbid5, t, p1, rbid6, p2,&
                    phi, end, rbid28, rgaz, rbid8,&
                    rbid9, biot, rbid11, satur, dsatur,&
                    pesa, permfh, permli, dperml, permgz,&
                    dperms, dpermp, rbid14, rbid15, rbid16,&
                    lambp, dlambp, rbid17, unsurk, alpha,&
                    rbid18, lambs, dlambs, viscl, dviscl,&
                    mamolg, rbid19, lambt, dlambt, viscg,&
                    dviscg, mamolv, rbid25, rbid26, rbid27,&
                    rbid45, rbid46, rbid47, rbid48, rbid49,&
                    rbid50, lambct, isot, rbid50, instap,&
                    retcom)
    else if (thmc.eq.'LIQU_GAZ_ATM') then
        call thmrcp('FINALE  ', imate, thmc, meca, hydr,&
                    ther, rbid1, rbid2, rbid3, rbid4,&
                    rbid5, t, p1, rbid6, p2,&
                    phi, end, rbid28, rbid29, rbid8,&
                    rbid9, biot, rbid11, satur, dsatur,&
                    pesa, permfh, permli, dperml, rbid30,&
                    rbid31, rbid32, rbid14, rbid15, rbid16,&
                    lambp, dlambp, rbid17, unsurk, alpha,&
                    rbid18, lambs, dlambs, viscl, dviscl,&
                    rbid20, rbid19, lambt, dlambt, rbid23,&
                    rbid24, mamolv, rbid25, rbid26, rbid27,&
                    rbid45, rbid46, rbid47, rbid48, rbid49,&
                    rbid50, lambct, isot, rbid50, instap,&
                    retcom)
    endif
! =====================================================================
end subroutine
