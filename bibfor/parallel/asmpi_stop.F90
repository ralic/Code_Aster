!> Brief A processor ask the others to stop.
!
!> If imode = 1, one of the procs did not answer in the delay,
!> the execution must be interrupted by MPI_Abort.
!>
!> If imode = 2, all the processors synchronize themself and end with the
!> same utmess('s').
!> Should not be called in sequential.
!XXX I wrote that but why does it not work ? (MC)
!>
!> The caller should execute nothing after the call to asmpi_stop().
!
!> @param[in]   imode   interruption mode
!
subroutine asmpi_stop(imode)
! person_in_charge: mathieu.courtois at edf.fr

! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.

    use parameters_module, only : ST_ER_OTH, ST_EXCEPT
    implicit none
#include "asterf.h"
#include "asterf_debug.h"
#include "asterf_types.h"
#include "asterc/asabrt.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"
#include "asterfort/gtstat.h"
#include "asterfort/onerrf.h"
#include "asterfort/ststat.h"
    integer :: imode

    integer :: lout, imod2
    character(len=16) :: compex
    aster_logical :: labort
!
    call ststat(ST_ER_OTH)
    labort = .not. gtstat(ST_EXCEPT)
!
!   If an abort is required, force imode to 1
    imod2 = imode
    if (labort .and. imod2 == 2) then
        call onerrf(' ', compex, lout)
        if (compex(1:lout) == 'ABORT') then
            imod2 = 1
            DEBUG_MPI('mpi_stop ', 'mode forced to', imod2)
        endif
    endif
    DEBUG_MPI('mpi_stop', imod2, ' (1:abort, 2:except)')
!
    if (imod2 == 1) then
#ifdef _USE_MPI
        call utmess('D', 'APPELMPI_99')
#endif
        call asabrt(6)
!
    else if (imod2 == 2) then
        if (labort) then
            call utmess('M', 'APPELMPI_95')
        endif
!
    else
        ASSERT(.false.)
    endif
!
end subroutine
