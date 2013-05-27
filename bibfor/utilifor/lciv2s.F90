function lciv2s(a)
    implicit none
!       ----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       SECOND INVARIANT TENSEUR CONTRAINTE(3X3) SOUS FORME VECTEUR(6X1)
!       IN  A      :   TENSEUR
!                                             T  1/2
!       OUT EQUIV  :  EQUIVALENT DE A = (3/2 D D)
!                     AVEC          D = A - 1/3 TR(A) I
!       ----------------------------------------------------------------
    include 'asterfort/lcdevi.h'
    include 'asterfort/lcnrts.h'
    integer :: n, nd
    real(kind=8) :: a(6), dev(6)
    real(kind=8) :: lciv2s
    common /tdim/   n , nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call lcdevi(a, dev)
    lciv2s = lcnrts ( dev )
end function
