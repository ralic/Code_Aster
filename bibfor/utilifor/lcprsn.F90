subroutine lcprsn(n, x, y, p)
    implicit none
    integer :: n
    real(kind=8) :: x(n), y(n), p
!       ----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!
! LOI DE COMPORTEMENT - PRODUIT SCALAIRE DE 2 VECTEURS - VERSION NORMALE
! *      *              **      *                                *
!
! ----------------------------------------------------------------------
!
! IN  N    : DIMENSION DES VECTEURS X ET Y
! IN  X    : VECTEUR X
! IN  Y    : VECTEUR Y
! OUT P    : PRODUIT SCALAIRE DE X ET Y
!
! ----------------------------------------------------------------------
!
    integer :: i
!
    p = 0.d0
    do 1 i = 1, n
        p = p + x(i)*y(i)
 1  continue
end subroutine
