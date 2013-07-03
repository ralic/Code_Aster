subroutine coefdg(compor, mat, dpida2)
!
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
#include "asterfort/rcvalb.h"
    character(len=16) :: compor
    integer :: mat
    real(kind=8) :: dpida2
!
! ---------------------------------------------------------------------
!     LOIS A GRADIENTS : COEFFICIENT DIAGONAL MATRICE GVNO
! ---------------------------------------------------------------------
!
! ---------------------------------------------------------------------
!
    real(kind=8) :: val(1)
    character(len=8) :: nom(2), fami, poum
    integer :: k2(5), kpg, spt
! ---------------------------------------------------------------------
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    if (compor .eq. 'ENDO_CARRE') then
!
        nom(1) = 'E'
        nom(2) = 'NU'
        call rcvalb(fami, kpg, spt, poum, mat,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, nom(1), val(1), k2, 2)
!
        dpida2 = val(1)
!
    endif
!
end subroutine
