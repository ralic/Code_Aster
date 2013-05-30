subroutine dxefro(ne, t2ve, edgle, edglc)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'asterfort/utbtab.h'
    integer :: ne
    real(kind=8) :: t2ve(2, 2)
    real(kind=8) :: edgle(*)
    real(kind=8) :: edglc(*)
!     ------------------------------------------------------------------
!     PASSAGE DES EFFORTS OU DEFORMATIONS GENERALISES DU REPERE
!     INTRINSEQUE DE L'ELEMENT AU REPERE LOCAL DE LA COQUE
!     ------------------------------------------------------------------
!     IN  NE    I      NOMBRE DE POINTS A TRAITER
!     IN  T2VE  R 2,2  MATRICE DE PASSAGE VARIETE - ELEMENT
!     IN  EDGLE R  8   NXX NYY NXY MXX MYY MXY VX VY
!     OUT EDGLC R  8   NXX NYY NXY MXX MYY MXY VX VY
!  OU IN  EDGLE R  8   EXX EYY EXY KXX KYY KXY GAX GAY
!     OUT EDGLE R  8   EXX EYY EXY KXX KYY KXY GAX GAY
!     ------------------------------------------------------------------
    real(kind=8) :: nle(4), mle(4), xab(2, 2)
    real(kind=8) :: nlc(4), mlc(4), t2ev(2, 2)
!
!     TRANSPOSEE DE T2VE
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    t2ev(1,1) = t2ve(1,1)
    t2ev(1,2) = t2ve(2,1)
    t2ev(2,1) = t2ve(1,2)
    t2ev(2,2) = t2ve(2,2)
!
    do 120 i = 1, ne
        nle(1) = edgle(1+8*(i-1))
        nle(2) = edgle(3+8*(i-1))
        nle(3) = edgle(3+8*(i-1))
        nle(4) = edgle(2+8*(i-1))
!
        mle(1) = edgle(4+8*(i-1))
        mle(2) = edgle(6+8*(i-1))
        mle(3) = edgle(6+8*(i-1))
        mle(4) = edgle(5+8*(i-1))
!
        call utbtab('ZERO', 2, 2, nle, t2ev,&
                    xab, nlc)
        call utbtab('ZERO', 2, 2, mle, t2ev,&
                    xab, mlc)
!
        edglc(1+8*(i-1)) = nlc(1)
        edglc(2+8*(i-1)) = nlc(4)
        edglc(3+8*(i-1)) = nlc(2)
!
        edglc(4+8*(i-1)) = mlc(1)
        edglc(5+8*(i-1)) = mlc(4)
        edglc(6+8*(i-1)) = mlc(2)
!
        edglc(7+8*(i-1)) = edgle( 7+8*(i-1)) * t2ev(1, 1) + edgle(8+8*( i-1)) * t2ev(2, 1 )
        edglc(8+8*(i-1)) = edgle( 7+8*(i-1)) * t2ev(1, 2) + edgle(8+8*( i-1)) * t2ev(2, 2 )
120  end do
end subroutine
