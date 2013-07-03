subroutine dfddd(eps, endo, ndim, lambda, mu,&
                 ecrod, dfd)
!
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
!
    implicit none
#include "asterfort/diago3.h"
#include "asterfort/r8inir.h"
    integer :: ndim
    real(kind=8) :: eps(6), lambda, mu
    real(kind=8) :: dfd, endo, ecrod
!
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDO_ORTH_BETON
!     CALCUL DE LA DERIVEE DE LA FORCE THERMODYNAMIQUE(ENDO COMPRESSION)
!     PAR RAPPORT A L ENDOMMAGEMENT DE COMPRESSION:DFD/DD
!
!     FD=(1-ENDO)(LAMBDA/2*(TR(E).H(-TR(E)))**2+MU*TR(E-**2))-ECROD*ENDO
!     IN  NDIM      : DIMENSION 3(3D) OU 2(2D)
!     IN  EPS      : DEFORMATION
!     IN  ENDO     : ENDOMMAGEMENT DE COMPRESSION
!     IN  LAMBDA   : /
!     IN  MU       : / COEFFICIENTS DE LAME
!     IN  ECROD    : PARAMETRE D ECROUISSAGE ENDO COMPRESSION
!     OUT DFD      : DFD/DD
! ----------------------------------------------------------------------
!
!
    real(kind=8) :: treps
    real(kind=8) :: vpe(3)
    real(kind=8) :: valeps(3), veceps(3, 3), phid
    integer :: i, t(3, 3)
!
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
    phid = -2.d0
!
    dfd=0.d0
    treps=0.d0
    do 5 i = 1, ndim
        treps=treps+eps(t(i,i))
 5  end do
    if (treps .lt. 0.d0) then
        dfd=dfd+phid*lambda/2.d0*treps**2
    endif
!
    call diago3(eps, veceps, valeps)
    call r8inir(3, 0.d0, vpe, 1)
!
    do 19 i = 1, ndim
        if (valeps(i) .lt. 0.d0) then
            vpe(i)=valeps(i)
        else
            vpe(i)=0.d0
        endif
19  end do
!
    dfd=dfd+mu*phid*(vpe(1)**2+vpe(2)**2+vpe(3)**2)-2.d0*ecrod
!
end subroutine
