subroutine dinuar(result    , sddisc     , nume_inst, force,&
                  nume_store, nume_reuse_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/diinst.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrpo.h"
#include "asterfort/rsadpa.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: result
    character(len=19), intent(in) :: sddisc
    integer, intent(in) :: nume_inst
    aster_logical, intent(in) :: force
    integer, intent(out) :: nume_store
    integer, optional, intent(out) :: nume_reuse_
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Get storing index
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of datastructure for results
! In  sddisc           : datastructure for time discretization
! In  nume_inst        : index of current time step
! In  force            : to "froce" storing (ex.: error)
! Out nume_store       : index to store in results
! Out nume_reuse       : index for reuse rsults datastructure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdarch_ainf
    integer, pointer :: v_sdarch_ainf(:) => null()
    integer :: nume_reuse, jv_para
    real(kind=8) :: time_curr, time_prev
    aster_logical :: l_store
    character(len=19) :: sdarch
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    l_store    = .false._1
    nume_store = - 1
    nume_reuse = - 1
!
! - Acces to storing objects
!
    sdarch      = sddisc(1:14)//'.ARCH'
    sdarch_ainf = sdarch(1:19)//'.AINF'
    call jeveuo(sdarch_ainf, 'E', vi = v_sdarch_ainf)
!
! - Initial storing or not
!
    if (nume_inst .eq. 0) then
        l_store = .true.
    else
        time_curr = diinst(sddisc, nume_inst)
        call nmcrpo(sdarch, nume_inst, time_curr, l_store)
    endif
!
! - "forced" storing
!
    if (force) then
        l_store = .true.
    endif
!
! - Stroing index
!
    if (l_store) then
        nume_store = v_sdarch_ainf(1)
    else
        nume_store = - 1
    endif
!
! - REUSE for PARA_CALC table
!
    nume_reuse = v_sdarch_ainf(3)
!
! - Already stored ?
!
    if (nume_store .ge. 2) then
        call rsadpa(result, 'L', 1, 'INST', nume_store-1,&
                    0, sjv=jv_para)
        time_prev = zr(jv_para)
        if (time_curr .le. time_prev) then
            nume_store = -1
            l_store    = .false._1
        endif
    endif
!
! - Increase storing index
!
    if (l_store) then
        v_sdarch_ainf(1) = v_sdarch_ainf(1) + 1
    endif
!
    if (present(nume_reuse_)) then
        nume_reuse_ = nume_reuse
    endif
!
    call jedema()
!
end subroutine
