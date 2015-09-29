subroutine nmetob(ds_inout, field_type, i_field_obsv)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/jeveuo.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=24), intent(in) :: field_type
    integer, intent(out) :: i_field_obsv
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Get index of field used for OBSERVATION
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_inout         : datastructure for input/output management
! In  field_type       : name of field (type) in results datastructure
! Out i_field_obsv     : index of field - 0 if not used for OBSERVATION
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_field, i_field
    character(len=24) :: obsv_keyw
!
! --------------------------------------------------------------------------------------------------
!
    i_field_obsv = 0
    nb_field     = ds_inout%nb_field
!
! - Find field
!
    do i_field = 1, nb_field
        obsv_keyw = ds_inout%field(i_field)%obsv_keyw
        if (obsv_keyw .eq. field_type) then
            i_field_obsv = i_field
        endif
    end do
!
end subroutine
