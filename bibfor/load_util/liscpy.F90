subroutine liscpy(list_load_in, list_load_out, base)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscad.h"
#include "asterfort/lisccr.h"
#include "asterfort/liscli.h"
#include "asterfort/lisnch.h"
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
!
    character(len=19), intent(in) :: list_load_in
    character(len=19), intent(in) :: list_load_out
    character(len=1), intent(in) :: base
!
! --------------------------------------------------------------------------------------------------
!
! List of loads - Utility
!
! Special copy (DO NOT KEEP "ELEM_TARDIF" loads)
!
! --------------------------------------------------------------------------------------------------
!
! In  list_load_in      : list of loads to copy
! In  list_load_out     : list of loads to save
! In  base              : JEVEUX base
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_info_maxi
    parameter   (nb_info_maxi=99)
    character(len=24) :: list_info_type(nb_info_maxi)
!
    integer :: nb_load_in, i_load_in, i_load_out, nb_load_out, i_neum_lapl, i_type_info
    character(len=8) :: load_name, load_func
    integer :: nb_info_type
    character(len=24) :: lload_info
    integer, pointer :: v_load_info(:) => null()
    integer, pointer :: v_load_info_out(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Datastructure access
!
    lload_info = list_load_in(1:19)//'.INFC'
    call jeveuo(lload_info, 'L', vi   = v_load_info)
!
    nb_load_in = v_load_info(1)
!
    if (nb_load_in .eq. 0) then
        call lisccr('MECA', list_load_out, 1, base)
        call jeveuo(list_load_out(1:19)//'.INFC', 'E', vi = v_load_info_out)
        v_load_info_out(1) = 0
        goto 999
    else
        call lisnch(list_load_in, nb_load_in)
        nb_load_out = nb_load_in
    endif
!
! - Don't keep "ELEM_TARDIF" load type
!
    do i_load_in = 1, nb_load_in
        i_type_info = v_load_info(nb_load_in+i_load_in+1)
        if (i_type_info .eq. 10) then
            nb_load_out = nb_load_out-1
        endif
    end do
!
! - No loads but contact method
!
    if (nb_load_out.le.0) then
        call lisccr('MECA', list_load_out, 1, base)
        call jeveuo(list_load_out(1:19)//'.INFC', 'E', vi = v_load_info_out)
        v_load_info_out(1) = 0
        goto 999
    endif
!
    ASSERT(nb_load_out.gt.0)
    ASSERT(nb_load_out.le.nb_load_in)
!
! - Create new Datastructure
!
    call lisccr('MECA', list_load_out, nb_load_out, base)
!
! - Copy all loads except "ELEM_TARDIF" load type
!
    i_load_out = 1
    do i_load_in = 1, nb_load_in
        i_type_info = v_load_info(nb_load_in+i_load_in+1)
        if (i_type_info .ne. 10) then
            call liscli(list_load_in  , i_load_in   , nb_info_maxi  , list_info_type, load_name,&
                        load_func     , nb_info_type, i_neum_lapl)
            call liscad('MECA'      , list_load_out , i_load_out  , load_name, load_func,&
                        nb_info_type, list_info_type, i_neum_laplz = i_neum_lapl)
            i_load_out = i_load_out + 1
        endif
    end do
!
    ASSERT(nb_load_out.eq.(i_load_out-1))
!
999 continue
    call jedema()
end subroutine
