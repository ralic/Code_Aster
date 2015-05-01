subroutine xmafr1(ndim, nd, p)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
    real(kind=8) :: nd(3), p(3, 3)
    integer :: ndim
! ----------------------------------------------------------------------
!        CALCUL DE LA MATRICE DE L'OPÉRATEUR DE PROJECTION
!
! IN    NDIM : DIMENSION DU MAILLAGE
! IN    ND   : NORMALE
!
! OUT   P    : OPÉRATEUR DE PROJECTION
!
!
!
!
!
    integer :: i, j
!
!     P : OPÉRATEUR DE PROJECTION
    do 10 i = 1, ndim
        do 20 j = 1, ndim
            p(i,j) = -1.d0 * nd(i)*nd(j)
20      continue
10  end do
!
    do 30 i = 1, ndim
        p(i,i) = 1.d0 + p(i,i)
30  end do
!
end subroutine
