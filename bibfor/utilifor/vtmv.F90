subroutine vtmv(n, v, a, r)
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
    integer :: n
    real(kind=8) :: v(n), a(n, n), r
!     ------------------------------------------------------------------
! IN  N      :  DIMENSION DU VECTEUR
! IN  V      :  VECTEUR
! IN  A      :  MATRICE
! OUT R      :  REEL, RESULTAT DE VT*A*V
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    r = 0.d0
    do 10 i = 1, n
        do 10 j = 1, n
            r = r + v(i)*a(i,j)*v(j)
10      continue
!
end subroutine
