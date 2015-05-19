subroutine asmpi_check(iret)
! person_in_charge: mathieu.courtois at edf.fr
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
! aslint: disable=W1306
#include "asterf_debug.h"
#include "asterf_types.h"
#include "asterf.h"
#include "asterc/asmpi_comm.h"
#include "asterc/asmpi_wtime.h"
#include "asterc/uttrst.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/asmpi_status.h"
#include "asterfort/gtstat.h"
#include "asterfort/asmpi_stop.h"
#include "asterfort/ststat.h"
#include "asterfort/utmess.h"
    integer, intent(out) :: iret
!-----------------------------------------------------------------------
!     FONCTION REALISEE : MPI CHECK ERROR
!       AVANT D'EFFECTUER UNE COMMUNICATION BLOQUANTE
!       ON VERIFIE :
!           - QU'AUCUN PROCESSEUR N'A SIGNALE DE PROBLEMES
!           - QUE TOUS LES PROCESSEURS SONT AU RENDEZ-VOUS
!       EN CAS DE PROBLEME, ON RETOURNE IRET != 0, CAR IL NE FAUT ALORS
!       PAS INITIER DE NOUVELLES COMMUNICATIONS.
!-----------------------------------------------------------------------
#if defined(_USE_MPI) && !defined(ASTER_DISABLE_MPI_CHECK)

#include "mpif.h"
#include "asterf_constant.h"
#include "asterc/asmpi_irecv_i4.h"
#include "asterc/asmpi_send_i4.h"
#include "asterc/asmpi_cancel.h"
#include "asterc/asmpi_test.h"

    aster_logical :: lcont
    mpi_int :: term
    integer :: i, nbterm, nbproc, np1, resp0
    mpi_int :: nbpro4, rank, istat, mpicou, wki(1), nbv, ip4
    real(kind=8) :: tres, timeout, t0, tf
    aster_logical, pointer :: isterm(:) => null()
    mpi_int, pointer :: diag(:) => null()
    mpi_int, pointer :: request(:) => null()

!   Current mpi communicator
    call asmpi_comm('GET', mpicou)
    iret = 0
    call asmpi_info(mpicou, rank=rank, size=nbpro4)
    nbproc = to_aster_int(nbpro4)
    np1 = nbpro4 - 1
    nbv = 1

    DEBUG_MPI('mpi_check', rank, nbpro4)

!   On the processor #0
    if (rank == 0) then
        AS_ALLOCATE(vl=isterm, size=nbproc)
        AS_ALLOCATE(vi4=diag, size=nbproc)
        AS_ALLOCATE(vi4=request, size=nbproc)

        call uttrst(tres)
        timeout = tres * 0.2d0
        if (timeout < 0) then
            timeout = 120.
            call utmess('A', 'APPELMPI_94', sr=timeout)
        endif

!       Ask each processor for its status
        do i = 1, np1
            isterm(i) = .false.
            ip4 = i
            DEBUG_MPI('mpi_check', 'irecv from ', ip4)
            call asmpi_irecv_i4(diag(i:i), nbv, ip4, ST_TAG_CHK, mpicou,&
                                request(i))
        end do
!
        nbterm = 0
        t0 = asmpi_wtime()
        lcont = .true.
        do while (lcont)
            do i = 1, np1
                if (.not. isterm(i)) then
                    call asmpi_test(request(i), term)
                    if (term .eq. 1) then
                        nbterm = nbterm + 1
                        isterm(i) = .true.
                        if (diag(i) .eq. ST_ER) then
                            call utmess('I', 'APPELMPI_84', si=i)
                            call ststat(ST_ER_OTH)
                        endif
                    endif
                endif
            end do
            lcont = nbterm .lt. np1
!           timeout
            tf = asmpi_wtime()
            if (lcont .and. (tf - t0) .gt. timeout) then
                lcont = .false.
                call utmess('E', 'APPELMPI_97', sr=timeout)
                do i = 1, np1
                    if (.not. isterm(i)) then
                        call utmess('E+', 'APPELMPI_96', si=i)
                        call utmess('E', 'APPELMPI_83', sk='MPI_IRECV')
                        call ststat(ST_UN_OTH)
                    endif
                end do
            endif
        end do

        if (gtstat(ST_ER_PR0)) then
            call utmess('I', 'APPELMPI_84', si=0)
        endif
!       Tell to all processors that answered if it continues or not
        if (gtstat(ST_OK)) then
            istat = ST_OK
        else
            istat = ST_ER
        endif
        do i = 1, np1
            if (isterm(i)) then
                if (istat .ne. ST_OK) then
                    call utmess('I', 'APPELMPI_81', si=i)
                endif
                wki(1) = istat
                ip4 = i
                DEBUG_MPI('mpi_check:send status / to', wki(1), ip4)
                call asmpi_send_i4(wki, nbv, ip4, ST_TAG_CNT, mpicou)
            else
!               cancel those have not answered
                DEBUG_MPI('mpi_check', 'cancel request for proc', i)
                call asmpi_cancel(request(i))
            endif
        end do
!
        if (.not. gtstat(ST_OK)) then
            iret = 1
            if (gtstat(ST_UN_OTH)) then
                call asmpi_stop(1)
            else
                call asmpi_stop(2)
            endif
        endif
        AS_DEALLOCATE(vl=isterm)
        AS_DEALLOCATE(vi4=diag)
        AS_DEALLOCATE(vi4=request)

!   On all others processors (not #0)
    else
!       Each processor sends ST_OK to the processor #0
        call asmpi_status(ST_OK, resp0)
        if (resp0 .ne. ST_OK) then
            iret = 1
            call utmess('I', 'APPELMPI_80')
            call asmpi_stop(2)
        endif
    endif

#else
    iret = 0
#endif

end subroutine asmpi_check
