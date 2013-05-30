subroutine betmat(fami, kpg, ksp, mod, imat,&
                  nmat, tempd, tempf, materd, materf,&
                  matcst, ndt, ndi, nr, nvi)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!  BETON_DOUBLE_DP : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
!                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
!                    MATER(*,1) = E , NU , ALPHA , B_ENDOGE , K_DESSIC
!                    MATER(*,2) = RESI_COMP_UNIAX, RESI_TRAC_UNIAX,
!                                 RESI_COEF_BIAX,
!                                 E_RUPT_COMP,     E_RUPT_TRAC
!                                 COMP_POST_PIC,   TRAC_POST_PIC'
!                                 COEF_POST_PIC
!                    VARIABLES INTERNES : EPPC, EPPC, THETA , E1, E2
!       ----------------------------------------------------------------
!       IN  IMAT   :  ADRESSE DU MATERIAU CODE
!           MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION  DE MATER
!           TEMPD  :  TEMPERATURE  A T
!           TEMPF  :  TEMPERATURE  A T+DT
!       OUT MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
!                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
!           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                     'NON' SINON
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NR     :  NB DE COMPOSANTES SYSTEME NL
!           NVI    :  NB DE VARIABLES INTERNES
!       ----------------------------------------------------------------
    include 'asterc/iisnan.h'
    include 'asterc/r8nnem.h'
    include 'asterfort/betnvi.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    integer :: nmat, ndt, ndi, nr, nvi, kpg, ksp
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), tempd, tempf
    real(kind=8) :: valpaf
    real(kind=8) :: epsi, theta
    character(len=8) :: mod, nomc(14), nompar
    integer :: cerr(14)
    character(len=3) :: matcst
    character(len=*) :: fami
!-----------------------------------------------------------------------
    integer :: i, imat
!-----------------------------------------------------------------------
    data epsi       /1.d-15/
!       ----------------------------------------------------------------
!
! -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
!
    call betnvi(mod, ndt, ndi, nr, nvi)
!
    call r8inir(2*nmat, 0.d0, materd, 1)
    call r8inir(2*nmat, 0.d0, materf, 1)
!
! -     RECUPERATION MATERIAU -----------------------------------------
!
    nomc(1) = 'E       '
    nomc(2) = 'NU      '
    nomc(3) = 'ALPHA   '
    nomc(4) = 'B_ENDOGE'
    nomc(5) = 'K_DESSIC'
    nomc(6) = 'F_C     '
    nomc(7) = 'F_T     '
    nomc(8) = 'COEF_BIA'
    nomc(9) = 'ENER_COM'
    nomc(10)= 'ENER_TRA'
    nomc(11)= 'COEF_ELA'
    nomc(12)= 'ECRO_COM'
    nomc(13)= 'ECRO_TRA'
    nomc(14)= 'LONG_CAR'
!
! -     TEMPERATURE MAXIMAL AU COURS DE L'HISTORIQUE DE CHARGEMENT
! -     THEMIQUE THETA (T+DT)
!
    theta = tempf
    if ((iisnan(tempd).ne.0) .or. (iisnan(tempf).ne.0)) then
        theta=r8nnem()
    else
        if (tempd .gt. tempf) theta = tempd
    endif
!
    nompar = 'TEMP'
    valpaf = theta
!
! -     RECUPERATION MATERIAU A TEMPD (T)
!
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                5, nomc(1), materd(1, 1), cerr(1), 0)
    if (cerr(3) .ne. 0) materd(3,1) = 0.d0
    if (cerr(4) .ne. 0) materd(4,1) = 0.d0
    if (cerr(5) .ne. 0) materd(5,1) = 0.d0
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'BETON_DOUBLE_DP', 0, ' ', 0.d0,&
                8, nomc(6), materd(1, 2), cerr(6), 2)
    call rcvalb(fami, kpg, ksp, '-', imat,&
                ' ', 'BETON_DOUBLE_DP', 0, ' ', 0.d0,&
                1, nomc(14), materd(9, 2), cerr(14), 0)
    if (cerr(14) .ne. 0) materd(9,2) = -1.d0
!
! -     RECUPERATION MATERIAU A TEMPF (T+DT)
!
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'ELAS', 1, nompar, valpaf,&
                5, nomc(1), materf(1, 1), cerr(1), 0)
    if (cerr(3) .ne. 0) materf(3,1) = 0.d0
    if (cerr(4) .ne. 0) materf(4,1) = 0.d0
    if (cerr(5) .ne. 0) materf(5,1) = 0.d0
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'BETON_DOUBLE_DP', 1, nompar, valpaf,&
                8, nomc(6), materf(1, 2), cerr(6), 2)
    call rcvalb(fami, kpg, ksp, '+', imat,&
                ' ', 'BETON_DOUBLE_DP', 1, nompar, valpaf,&
                1, nomc(14), materf(9, 2), cerr(14), 0)
    if (cerr(14) .ne. 0) materf(9,2) = -1.d0
!
!
    materd(6,2) = materd(6,2) * 0.01d0
    materf(6,2) = materf(6,2) * 0.01d0
!
! -     MATERIAU CONSTANT ?
!
    matcst = 'OUI'
    do 30 i = 1, 5
        if (abs ( materd(i,1) - materf(i,1) ) .gt. epsi) then
            matcst = 'NON'
            goto 9999
        endif
30  continue
    do 40 i = 1, 9
        if (abs ( materd(i,2) - materf(i,2) ) .gt. epsi) then
            matcst = 'NON'
            goto 9999
        endif
40  continue
!
9999  continue
end subroutine
