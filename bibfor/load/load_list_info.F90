subroutine load_list_info(load_empty, nb_load   , v_load_name, v_load_info,&
                          load_namez, load_infoz, list_load  )
!
implicit none
!
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
    character(len=19), optional, intent(in) :: list_load
    character(len=*), optional, intent(in) :: load_namez
    character(len=*), optional, intent(in) :: load_infoz
!
! --------------------------------------------------------------------------------------------------
!
! List of loads - Utility
!
! Get informations
!
! --------------------------------------------------------------------------------------------------
!
! In  list_load   : list of loads
! In  load_name   : name of object for loads name
! In  load_name   : name of object for loads info
! Out nb_load     : number of loads
! Out load_empty  : .true. if not loads in list
! Out v_load_name : vector of loads name
! Out v_load_info : vector of loads info
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=24) :: lload_name, lload_info
!
! --------------------------------------------------------------------------------------------------
!
    load_empty = .true.
    nb_load    = 0
    if (present(list_load)) then
        lload_name = list_load(1:19)//'.LCHA'
        lload_info = list_load(1:19)//'.INFC'
    else
        lload_name = load_namez
        lload_info = load_infoz    
    endif
!
    call jeexin(lload_name, iret)
    if (iret .ne. 0) then
        call jelira(lload_name, 'LONMAX', nb_load)
        if (nb_load .ne. 0) then
            load_empty = .false.
            call jeveuo(lload_name, 'L', vk24 = v_load_name)
            call jeveuo(lload_info, 'L', vi   = v_load_info)
        endif
    endif

end subroutine
