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
    subroutine ntetcr(nume_dof  , l_temp_nonl, ds_inout  ,&
                      list_load_, compor_    , hydr_     , hydr_init_)
        use NonLin_Datastructure_type
        character(len=24), intent(in) :: nume_dof
        aster_logical, intent(in) :: l_temp_nonl
        type(NL_DS_InOut), intent(inout) :: ds_inout
        character(len=19), optional, intent(in) :: list_load_
        character(len=*), optional, intent(in) :: compor_
        character(len=*), optional, intent(in) :: hydr_
        character(len=*), optional, intent(in) :: hydr_init_
    end subroutine ntetcr
end interface
