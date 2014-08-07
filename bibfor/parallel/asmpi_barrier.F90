subroutine asmpi_barrier(comm)
! person_in_charge: mathieu.courtois@edf.fr
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterc/asmpi_comm.h"
#include "asterf_debug.h"
!
    mpi_int, intent(in), optional :: comm
!
!   Set a MPI Barrier on the given communicator or the current one.
!
    mpi_int :: comm2, idummy
#ifdef _USE_MPI
#include "asterc/asmpi_barrier_wrap.h"
    if (.not. present(comm)) then
        call asmpi_comm('GET', comm2)
    else
        comm2 = comm
    endif
!
!   `asmpi_check()` is called from C
    DEBUG_MPI('mpi_barrier', 'communicator', comm2)
    call asmpi_barrier_wrap(comm2, idummy)
#else
    if (.not. present(comm)) then
        comm2 = 0
    else
        comm2 = comm
    endif
    idummy = 0
#endif
end subroutine asmpi_barrier
