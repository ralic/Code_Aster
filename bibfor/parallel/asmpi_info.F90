subroutine asmpi_info(comm, rank, size)
! person_in_charge: mathieu.courtois@edf.fr
!
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
    implicit none
#include "asterc/asmpi_comm.h"
#include "asterc/asmpi_info_wrap.h"
!
    mpi_int, intent(in), optional :: comm
    mpi_int, intent(out), optional :: rank
    mpi_int, intent(out), optional :: size
!
!   Return the rank of the processor and the number of processors assigned to a communicator.
!   If comm is not provided, use the current communicator.
!
    mpi_int :: comm2, rank2, size2
!
#ifdef _USE_MPI
    if (.not. present(comm)) then
        call asmpi_comm('GET', comm2)
    else
        comm2 = comm
    endif
    call asmpi_info_wrap(comm2, rank2, size2)
#else
    if (.not. present(comm)) then
        comm2 = 0
    else
        comm2 = comm
    endif
    rank2 = 0
    size2 = 1
#endif
    if (present(rank)) then
        rank = rank2
    endif
    if (present(size)) then
        size = size2
    endif
end subroutine asmpi_info
