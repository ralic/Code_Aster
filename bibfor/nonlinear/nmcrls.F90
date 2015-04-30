subroutine nmcrls(sddisc   , list_inst  , nume_ini, nume_end, l_init_noexist,&
                  inst_init, nb_inst_new, dtmin)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: list_inst
    integer, intent(in) :: nume_ini
    integer, intent(in) :: nume_end
    aster_logical, intent(in) :: l_init_noexist
    real(kind=8), intent(in) :: inst_init
    integer, intent(out) :: nb_inst_new
    real(kind=8), intent(out) :: dtmin
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Time discretization datastructure
!
! Resize list of times
!
! --------------------------------------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! In  list_inst        : list of times from INCREMENT/LIST_INST
! In  nume_ini         : index of initial time
! In  nume_end         : index of final time
! In  inst_init        : initial time if ETAT_INIT
! In  l_init_noexist   : .true. if initial time doesn't exist in list of times
! Out nb_inst          : number of time steps in list after resize
! Out dtmin            : minimum time between two steps after resize
!
! --------------------------------------------------------------------------------------------------
!
    integer :: pos, i_inst, nb_inst
    real(kind=8) :: deltat, valr(2)
    real(kind=8), pointer :: v_list_inst(:) => null()
    character(len=24) :: sddisc_ditr
    real(kind=8), pointer :: v_sddisc_ditr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call utdidt('L', sddisc, 'LIST', 'NBINST',&
                vali_ = nb_inst)
!
! - Final number of time steps
!
    nb_inst_new = (nume_end-nume_ini) + 1
    ASSERT(nb_inst_new.le.nb_inst)  
!
! - Acces to list of times
!
    call jeveuo(list_inst, 'L', vr = v_list_inst)
!
! - Create new list of time
!
    sddisc_ditr = sddisc(1:19)//'.DITR'
    call wkvect(sddisc_ditr, 'V V R', nb_inst_new, vr = v_sddisc_ditr)
!
! - Update new list of time
!
    pos = 0
    do i_inst = nume_ini, nume_end
        v_sddisc_ditr(pos+1) = v_list_inst(i_inst+1)
        pos = pos+1
    end do
!
! - New minimum time between two steps
!
    dtmin = r8maem()
    do i_inst = 1, nb_inst_new-1
        deltat = v_sddisc_ditr(i_inst+1) - v_sddisc_ditr(i_inst)
        dtmin  = min(deltat,dtmin)
    end do
!
! - Initial time doesn't exist in list of times => change for real initial time
!
    if (l_init_noexist) then
        v_sddisc_ditr(1) = inst_init
        if (inst_init .ge. v_sddisc_ditr(2)) then
            valr(1) = inst_init
            valr(2) = v_sddisc_ditr(2)
            call utmess('F', 'DISCRETISATION_2', nr=2, valr=valr)
        endif
    endif
!
end subroutine
