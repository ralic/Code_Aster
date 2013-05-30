subroutine lccong(nr, itmax, toler, iter, r,&
                  rini, yd, dy, irtet)
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
!     ------------------------------------------------------------------
!     CONTROLE DE LA CONVERGENCE DU NEWTON LOCAL DE LETK
!                   - CONTROLE DU NOMBRE D ITERATIONS
!                   - CONTROLE DE LA PRECISION DE CONVERGENCEC
!     ----------------------------------------------------------------
!     IN  ITMAX  :  NB MAXI D ITERATIONS LOCALES
!         TOLER  :  TOLERANCE A CONVERGENCE
!         ITER   :  NUMERO ITERATION COURANTE
!         NR     :  DIMENSION R
!         R      :  RESIDU DU SYSTEME NL A L'ITERATION COURANTE
!         RINI   :  RESIDU DU SYSTEME NL A LA 1ERE ITERATION
!         YD     :  SOLUTION A DEBUT DU PAS DE TEMPS
!         DY     :  INCREMENT DE SOLUTION
!
!     OUT IRET = 0:  CONVERGENCE
!         IRET = 1:  ITERATION SUIVANTE
!         IRET = 2:  RE-INTEGRATION
!         IRET = 3:  REDECOUPAGE DU PAS DE TEMPS
!     ----------------------------------------------------------------
    implicit none
    include 'asterc/r8prem.h'
    integer :: nr, itmax, iter, irtet, i
    real(kind=8) :: toler, r(nr), e1, e2, e1ini, e2ini, errr(2), rini(*)
    real(kind=8) :: yd(*), dy(*), err
!     ----------------------------------------------------------------
! === ==================================================================
! --- CALCUL DE LA NORME DE RINI ET DE R(Y)
! === ==================================================================
!
    e1=0.d0
    e1ini=0.d0
    do 101 i = 1, 6
        e1 = max(e1, abs(r(i)))
        e1ini = max(e1ini, abs(rini(i)))
101  end do
!     R8PREM CAR R HOMOGENE A DES DEFORMATIONS
    errr(1)=e1
    if (e1ini .gt. r8prem()) then
        errr(1)=e1/e1ini
    endif
!
    e2=0.d0
    e2ini=0.d0
    do 102 i = 7, nr
        e2 = max(e2, abs(r(i)))
        e2ini = max(e2ini, abs(yd(i)+dy(i)))
102  end do
!
    errr(2)=e2
    if (e2ini .gt. r8prem()) then
        errr(2)=e2/e2ini
    endif
!
!     MAX DES 6 PREMIERS TERMES ET DES SUIVANTS
    err=max(errr(1),errr(2))
!
! === =================================================================
! --- TEST DE CONVERGENCE PAR RAPPORT A TOLER
! === =================================================================
    if (err .lt. toler) then
        irtet = 0
        goto 9999
    endif
!
! === ==================================================================
! --- SI NON CONVERGENCE: TEST DU N°ITERATION
! === ==================================================================
    if (iter .lt. itmax) then
        irtet = 1
    else
        irtet = 3
    endif
!
9999  continue
!
end subroutine
