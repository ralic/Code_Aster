subroutine dxqgm(shpr1, shpr2, gm)
    implicit  none
#include "asterf_types.h"
    real(kind=8) :: shpr1(3,4), shpr2(3,4), gm(3, 4)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     MATRICE GM(3,4) MEMBRANE AU POINT QSI ETA POUR ELEMENTS DKQ ET DSQ
!     ------------------------------------------------------------------
!     ADDED LOCAL VARIABLES :
    integer :: j
!     ------------------------------------------------------------------
!
!
    do j = 1, 4
     gm(1,j) = shpr1(1,j)
     gm(2,j) = shpr2(2,j)
     gm(3,j) = shpr1(2,j) + shpr2(1,j)
    end do


!
end subroutine
