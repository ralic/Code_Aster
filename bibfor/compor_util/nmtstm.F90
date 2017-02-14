subroutine nmtstm(carcri, jv_matr, l_matr_symm)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jevech.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    real(kind=8), intent(in) :: carcri(*)
    aster_logical, intent(out) :: l_matr_symm
    integer, intent(out) :: jv_matr
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility
!
! Select symmtric or unsymmetric matrix for tangent operator
!
! --------------------------------------------------------------------------------------------------
!
! In  carcri           : parameter for comportment
! Out l_matr_symm      : .true. if matrix is symmetric
! Out jv_matr          : JEVEUX address for matrix
!
! --------------------------------------------------------------------------------------------------
!
    l_matr_symm = .true.
    if (nint(carcri(17)) .gt. 0) then
        l_matr_symm = .false.
    endif
    if (l_matr_symm) then
        call jevech('PMATUUR', 'E', jv_matr)
    else
        call jevech('PMATUNS', 'E', jv_matr)
    endif
end subroutine
