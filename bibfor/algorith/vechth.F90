subroutine vechth(type_ther , model_, lload_name_, lload_info_, cara_elem_,&
                  mate_     , time_ , temp_prev_ , vect_elem_ , varc_curr_,&
                  time_move_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/load_list_info.h"
#include "asterfort/load_neut_2mbr.h"
#include "asterfort/load_neut_prep.h"
#include "asterfort/assert.h"
#include "asterfort/inical.h"
#include "asterfort/reajre.h"
#include "asterfort/detrsd.h"
#include "asterfort/memare.h"
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
!
    character(len=4), intent(in) :: type_ther
    character(len=*), intent(in) :: model_
    character(len=*), intent(in) :: lload_name_
    character(len=*), intent(in) :: lload_info_
    character(len=*), intent(in) :: cara_elem_
    character(len=*), intent(in) :: time_
    character(len=*), intent(in) :: temp_prev_
    character(len=*), intent(inout) :: vect_elem_
    character(len=*), intent(in) :: mate_
    character(len=*), optional, intent(in) :: varc_curr_
    character(len=*), optional, intent(in) :: time_move_
!
! --------------------------------------------------------------------------------------------------
!
! Thermic - Loads
! 
! Neumann loads elementary vectors (second member)
!
! --------------------------------------------------------------------------------------------------
!
! In  type_ther        : type of thermics
!                        'MOVE' for moving sources
!                        'STAT' if not
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  lload_name       : name of object for list of loads name
! In  lload_info       : name of object for list of loads info
! In  time             : time (<CARTE>)
! In  varc_curr        : command variable for current time
! In  temp_prev        : previous temperature
! In  time_move        : modified time (<CARTE>) for THER_NON_LINE_MO
! IO  vect_elem        : name of vect_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_in_maxi, nbout
    parameter (nb_in_maxi = 16, nbout = 1)
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
!
    character(len=19) :: vect_elem, resu_elem, varc_curr
    integer :: nb_in_prep
    integer :: nb_load, i_load, load_nume
    character(len=1) :: base, stop_calc
    character(len=8) :: load_name
    character(len=24) :: model, cara_elem, time, temp_prev, mate, time_move
    aster_logical :: load_empty
    character(len=24) :: lload_name
    character(len=24), pointer :: v_load_name(:) => null()
    character(len=24) :: lload_info
    integer, pointer :: v_load_info(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initializations
!
    resu_elem   = '&&VECHTH.0000000'
    model       = model_
    lload_name  = lload_name_
    lload_info  = lload_info_
    cara_elem   = cara_elem_
    mate        = mate_
    time        = time_
    temp_prev   = temp_prev_
    vect_elem   = vect_elem_
    time_move   = ' '
    if (present(time_move_)) then
        ASSERT(type_ther.eq.'MOVE')
        time_move  = time_move_
    endif
    varc_curr   = ' '
    if (present(varc_curr_)) then
        varc_curr   = varc_curr_
    endif
    stop_calc    = 'S'
    base         = 'V'
!
! - Init fields
!
    call inical(nb_in_maxi, lpain, lchin, nbout, lpaout,&
                lchout)
!
! - Result name for vect_elem
!
    if (vect_elem .eq. ' ') then
        vect_elem = '&&VECHTH'
    endif
!
! - Loads
!
    call load_list_info(load_empty, nb_load   , v_load_name, v_load_info,&
                        lload_name, lload_info)
!
! - Allocate result
!
    call detrsd('VECT_ELEM', vect_elem)
    call memare(base, vect_elem, model, mate, cara_elem,&
                'CHAR_THER')
    call reajre(vect_elem, ' ', base)
    if (load_empty) then
        goto 99
    endif
!
! - Preparing input fields
!
    call load_neut_prep(model, nb_in_maxi, nb_in_prep, lchin, lpain, &
                        mate_ = mate, varc_curr_ = varc_curr, temp_prev_ = temp_prev)
!
! - Computation
!
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8)
        load_nume = v_load_info(nb_load+i_load+1)
        if (load_nume .gt. 0) then
            if (type_ther.eq.'MOVE') then
                call load_neut_2mbr(stop_calc, model     , time      , i_load    , load_name ,&
                                    load_nume, nb_in_maxi, nb_in_prep, lpain     , lchin    , &
                                    base     , resu_elem , vect_elem , &
                                    time_move_ = time_move)
            else
                call load_neut_2mbr(stop_calc, model     , time      , i_load    , load_name ,&
                                    load_nume, nb_in_maxi, nb_in_prep, lpain     , lchin    , &
                                    base     , resu_elem , vect_elem )
            endif
        endif
    end do
!
 99 continue
!
    vect_elem_ = vect_elem//'.RELR'
!
end subroutine
