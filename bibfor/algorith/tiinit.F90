subroutine tiinit(ds_inout, sddisc, lostat, l_evol)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/ntcrar.h"
#include "asterfort/ntcrli.h"
#include "asterfort/getvid.h"
#include "asterfort/ntcra0.h"
#include "asterfort/utmess.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19), intent(in) :: sddisc
    aster_logical, intent(in) :: lostat
    aster_logical, intent(out) :: l_evol
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Datastructures
!
! Time discretization and storing datastructures
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_inout         : datastructure for input/output management
! In  sddisc           : datastructure for time discretization
! In  lostat           : .true. for initial stationnary computation
! Out l_evol           : .true. for transient computation
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
    real(kind=8) :: init_time
    character(len=19) :: list_inst
    integer :: nocc
    aster_logical :: l_reuse
!
! --------------------------------------------------------------------------------------------------
!
    l_evol    = .false._1
    result    = ds_inout%result
    init_time = ds_inout%init_time
    l_reuse   = ds_inout%l_reuse
!
! - Transient computation ?
!
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=list_inst, nbret=nocc)
    if (nocc .eq. 0) then
        if (.not.lostat) then
            call utmess('F', 'DISCRETISATION_8')
        endif
        l_evol = .false.
    else
        l_evol = .true.
    endif
!
! - Create time discretization datastructure and storing datastructure
!
    if (l_evol) then
        call ntcrli(init_time, list_inst, sddisc, lostat)
        call ntcrar(result, sddisc, l_reuse)
    else
        call ntcra0(sddisc)
    endif
!
end subroutine
