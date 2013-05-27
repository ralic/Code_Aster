subroutine lcgrla(f, eps)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     DEFORMATION DE GREEN-LAGRANGE ASSOCIEE AU TENSEUR F
!     ----------------------------------------------------------------
!
    include 'asterfort/lctr2m.h'
    include 'asterfort/pmat.h'
    include 'asterfort/tnsvec.h'
    include 'blas/daxpy.h'
    include 'blas/dscal.h'
    real(kind=8) :: f(3, 3), ft(3, 3), ftf(3, 3), eps(6), id6(6)
    data id6/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
    call lctr2m(3, f, ft)
    call pmat(3, ft, f, ftf)
    call tnsvec(3, 3, ftf, eps, 1.d0)
    call daxpy(6, -1.d0, id6, 1, eps,&
               1)
    call dscal(6, 0.5d0, eps, 1)
!
end subroutine
