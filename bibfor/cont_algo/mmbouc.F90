subroutine mmbouc(ds_contact, loop_name, operation, loop_value)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=4), intent(in) :: loop_name
    character(len=4), intent(in) :: operation
    integer, intent(out), optional :: loop_value
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! All methods - Loops management
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  loop_name        : name of loop
!                        'CONT' - Contact status
!                        'FROT' - Friction trigger
!                        'GEOM' - Geometric loop
! In  operation        : type of operation on loop
!                        'READ' - Read value of loop iteration
!                        'INCR' - Add iteration to loop
!                        'INIT' - Initialization of loop
! Out loop_value       : value of loop (iteration)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_mboucl
    integer, pointer :: v_sdcont_mboucl(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_mboucl = ds_contact%sdcont_solv(1:14)//'.MBOUCL'
    call jeveuo(sdcont_mboucl, 'E', vi = v_sdcont_mboucl)
!
    if (loop_name .eq. 'CONT') then
        if (operation .eq. 'INIT') then
            v_sdcont_mboucl(1) = 0
        else if (operation.eq.'INCR') then
            v_sdcont_mboucl(1) = v_sdcont_mboucl(1) +1
        else if (operation.eq.'READ') then
            loop_value = v_sdcont_mboucl(1)
        else
            ASSERT(.false.)
        endif
        if (present(loop_value)) then
            loop_value = v_sdcont_mboucl(1)
        endif
    else if (loop_name.eq.'FROT') then
        if (operation .eq. 'INIT') then
            v_sdcont_mboucl(2) = 0
        else if (operation.eq.'INCR') then
            v_sdcont_mboucl(2) = v_sdcont_mboucl(2) +1
        else if (operation.eq.'READ') then
            loop_value = v_sdcont_mboucl(2)
        else
            ASSERT(.false.)
        endif
        if (present(loop_value)) then
            loop_value = v_sdcont_mboucl(2)
        endif
    else if (loop_name.eq.'GEOM') then
        if (operation .eq. 'INIT') then
            v_sdcont_mboucl(3) = 0
        else if (operation.eq.'INCR') then
            v_sdcont_mboucl(3) = v_sdcont_mboucl(3) +1
        else if (operation.eq.'READ') then
            loop_value = v_sdcont_mboucl(3)
        else
            ASSERT(.false.)
        endif
        if (present(loop_value)) then
            loop_value = v_sdcont_mboucl(3)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
