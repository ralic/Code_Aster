subroutine lobs(sd_obsv, nume_time, time, l_obsv)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/impfoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrpo.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sd_obsv
    integer, intent(in) :: nume_time
    real(kind=8), intent(in) :: time
    aster_logical, intent(out) :: l_obsv
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear operators - Observation
!
! Decision for observation
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_obsv          : datastructure for observation parameters
! In  time             : current time
! In  nume_time        : index of time
! Out l_obsv           : .true. if execute observation
!
! --------------------------------------------------------------------------------------------------
!
    character(len=14) :: sdextr_obsv
    integer :: i_keyw_fact, nb_keyw_fact
    character(len=2) :: chaine
    character(len=19) :: list_inst_obsv
    aster_logical :: l_select
    character(len=24) :: extr_info, extr_flag
    integer, pointer :: v_extr_info(:) => null()
    aster_logical, pointer :: v_extr_flag(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    l_obsv = .false.
!
! - Access to extraction datastructure
!
    sdextr_obsv = sd_obsv(1:14)
!
! - Get information vector
!
    extr_info    = sdextr_obsv(1:14)//'     .INFO'
    call jeveuo(extr_info, 'L', vi = v_extr_info)
    nb_keyw_fact = v_extr_info(1)
!
    if (nb_keyw_fact .ne. 0) then
!
! ----- Access to extraction flag vector
!
        extr_flag = sdextr_obsv(1:14)//'     .ACTI'
        call jeveuo(extr_flag, 'E', vl = v_extr_flag)
!
! ----- Initial time: always !
!
        if (nume_time .eq. 0) then
            l_obsv = .true.
            do i_keyw_fact = 1, nb_keyw_fact
                v_extr_flag(i_keyw_fact) = .true.
            end do
            goto 99
        endif
!
! ----- Other times ?
!
        do i_keyw_fact = 1, nb_keyw_fact
            call impfoi(0, 2, i_keyw_fact, chaine)
            list_inst_obsv = sd_obsv(1:14)//chaine(1:2)//'.LI'
            call nmcrpo(list_inst_obsv, nume_time, time, l_select)
            v_extr_flag(i_keyw_fact) = l_select
            l_obsv = l_select.or.l_obsv
        end do
 99     continue
    endif
!
end subroutine
