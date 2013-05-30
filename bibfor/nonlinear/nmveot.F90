subroutine nmveot(drbdb, drbdp, drpdb, drpdp, drbde,&
                  drpde, dsgde, dsgdb, dsgdp, np,&
                  nb, nr, dsidep)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
    implicit none
    include 'asterfort/lcicma.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/r8inir.h'
    integer :: np, nb, nr
    real(kind=8) :: drbdb(nb, nb), drbdp(nb, np)
    real(kind=8) :: drpdp(np, np), drpdb(np, nb)
    real(kind=8) :: drbde(nb, nb), drpde(np, nb)
    real(kind=8) :: dsgdb(nb, nb), dsgdp(nb, np), dsgde(nb, nb)
    real(kind=8) :: dsidep(nb, nb)
! ----------------------------------------------------------------------
!     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
!     CHABOCHE AVEC ENDOMAGEMENT
!     METHODE ITERATIVE D'EULER IMPLICITE
!
!     CALCUL DE L'OPERATEUR TANGENT DSIDEP(6,6)
! ----------------------------------------------------------------------
    integer :: nmod
    parameter  (nmod = 25)
    integer :: i, j, k, iret
    real(kind=8) :: a(6, 6), b(6, 6), r(nmod, nmod), dpde(2, 6), dbde(6, 6)
    real(kind=8) :: drdy(nmod, nmod), mun, det
    parameter  (mun = -1.d0)
!
! ----------------------------------------------------------------------
!-- 1.1. INITIALISATION DE L OPERATEUR LINEAIRE DU SYSTEME
!                     DRDY = ( DRBDB, DRBDP )
!                            ( DRPDB, DRPDP )
!
    call r8inir(nmod*nmod, 0.d0, drdy, 1)
    call lcicma(drbdb, nb, nb, nb, nb,&
                1, 1, drdy, nmod, nmod,&
                1, 1)
    call lcicma(drbdp, nb, np, nb, np,&
                1, 1, drdy, nmod, nmod,&
                1, nb+1)
    call lcicma(drpdb, np, nb, np, nb,&
                1, 1, drdy, nmod, nmod,&
                nb+1, 1)
    call lcicma(drpdp, np, np, np, np,&
                1, 1, drdy, nmod, nmod,&
                nb+1, nb+1)
!-- 1.2. INITIALISATION R = ( -DRBDE , -DRPDE )
!
    call r8inir(nmod*nmod, 0.d0, r, 1)
    call lcicma(drbde, nb, nb, nb, nb,&
                1, 1, r, nmod, nmod,&
                1, 1)
    call lcicma(drpde, np, nb, np, nb,&
                1, 1, r, nmod, nmod,&
                nb+1, 1)
    do 121 i = 1, nmod
        do 121 j = 1, nmod
            r(i,j) = mun * r(i,j)
121      continue
!
!-- 2. CALCUL DE DBDE ET DPDE
    call mgauss('NFVP', drdy, r, nmod, nr,&
                nb, det, iret)
    call lcicma(r, nmod, nmod, nb, nb,&
                1, 1, dbde, nb, nb,&
                1, 1)
    call lcicma(r, nmod, nmod, np, nb,&
                nb+1, 1, dpde, np, nb,&
                1, 1)
!
!-- 3. CALCUL DE L'OPERATEUR
!-- 3.1. INITIALISATION
    call r8inir(nb*nb, 0.d0, a, 1)
    call r8inir(nb*nb, 0.d0, b, 1)
!-- 3.2. CALCUL
    do 300 i = 1, nb
        do 300 j = 1, nb
            do 300 k = 1, nb
                a(i,j) = a(i,j) + dsgdb(i,k) * dbde(k,j)
300          continue
!
    do 305 i = 1, nb
        do 305 j = 1, nb
            do 305 k = 1, np
                b(i,j) = b(i,j) + dsgdp(i,k) * dpde(k,j)
305          continue
!
    do 310 i = 1, nb
        do 320 j = 1, nb
            dsidep(i,j) = a(i,j) + b(i,j) + dsgde(i,j)
320      continue
310  end do
!
end subroutine
