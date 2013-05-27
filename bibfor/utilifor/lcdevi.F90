subroutine lcdevi(a, d)
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
!       DEVIATEUR D UN TENSEUR (3X3) SOUS FORME VECTEUR  (6X1)
!       IN  A      :  TENSEUR
!       OUT D      :  DEVIATEUR DE A = A - 1/3 TR(A) I
!       ----------------------------------------------------------------
    integer :: n, nd
    real(kind=8) :: a(6), d(6), ta
    common /tdim/   n , nd
!
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    ta = 0.d0
    do 1 i = 1, nd
        ta = ta + a(i)
 1  continue
    ta = ta / 3.d0
    do 2 i = 1, nd
        d(i) = a(i) - ta
 2  continue
    do 3 i = nd + 1, n
        d(i) = a(i)
 3  continue
end subroutine
