!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine char_read_tran(keywordfact, iocc , ndim, l_tran, tran, &
                              l_cent, cent, l_angl_naut, angl_naut)
        character(len=16), intent(in) :: keywordfact
        integer, intent(in) :: iocc
        integer, intent(in) :: ndim
        aster_logical, intent(out) :: l_tran
        real(kind=8), intent(out) :: tran(3)
        aster_logical, intent(out) :: l_cent
        real(kind=8), intent(out) :: cent(3)  
        aster_logical, intent(out) :: l_angl_naut
        real(kind=8), intent(out) :: angl_naut(3) 
    end subroutine char_read_tran
end interface
