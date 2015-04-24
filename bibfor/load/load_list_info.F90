subroutine load_list_info(load_empty  , nb_load    , v_load_name, v_load_info,&
                          lload_name_ , lload_info_,&
                          list_load_  ,&
                          list_nbload_, list_name_)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(out) :: nb_load
    aster_logical, intent(out) :: load_empty
    character(len=24), pointer, intent(out) :: v_load_name(:)
    integer, pointer, intent(out) :: v_load_info(:)
    character(len=19), optional, intent(in) :: list_load_
    character(len=*), optional, intent(in) :: lload_name_
    character(len=*), optional, intent(in) :: lload_info_
    character(len=*), optional, target, intent(in) :: list_name_(*)
    integer, optional, intent(in) :: list_nbload_
!
! --------------------------------------------------------------------------------------------------
!
! List of loads - Utility
!
! Get informations
!
! --------------------------------------------------------------------------------------------------
!
! In  list_load        : list of loads
! In  lload_name       : name of JEVEUX object in list_load datastructure for loads name
! In  lload_info       : name of JEVEUX object in list_load datastructure for loads info
! In  list_nbload      : length of list_name
! In  list_name        : list of loads name
! Out nb_load          : number of loads
! Out load_empty       : .true. if not loads in list
! Out v_load_name      : vector of loads name
! Out v_load_info      : vector of loads info
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=24) :: lload_name, lload_info, orig_list
!
! --------------------------------------------------------------------------------------------------
!
    load_empty = .true.
    nb_load    = 0
!
! - Origin of informations
!
    if (present(list_load_)) then
        lload_name = list_load_(1:19)//'.LCHA'
        lload_info = list_load_(1:19)//'.INFC'
        orig_list  = 'SD_LIST_LOAD'
    elseif (present(lload_name_)) then
        lload_name = lload_name_
        lload_info = lload_info_
        orig_list  = 'SD_LIST_LOAD'
    elseif (present(list_name_)) then
        orig_list  = 'LIST_NAME'
    else
        ASSERT(.false.)
    endif
!
    if (orig_list.eq.'SD_LIST_LOAD') then
        call jeexin(lload_name, iret)
        if (iret .ne. 0) then
            call jelira(lload_name, 'LONMAX', nb_load)
            if (nb_load .ne. 0) then
                load_empty = .false.
                call jeveuo(lload_name, 'L', vk24 = v_load_name)
                call jeveuo(lload_info, 'L', vi   = v_load_info)
            endif
        endif
    else
        nb_load    = list_nbload_
        load_empty = nb_load.eq.0
        if (.not.load_empty) then
            v_load_name => list_name_(1:nb_load)
            v_load_info => null()
        endif
    endif

end subroutine
