subroutine asmpi_status(istat, resp0)
! person_in_charge: mathieu.courtois at edf.fr
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
!
    implicit none
!     ARGUMENTS          IN     OUT
#include "asterf_constant.h"
#include "asterf_debug.h"
#include "asterf_types.h"
#include "asterf.h"
#include "asterc/asmpi_comm.h"
#include "asterc/uttrst.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/mpistp.h"
#include "asterfort/utmess.h"
    integer :: istat, resp0
!-----------------------------------------------------------------------
!     FONCTION REALISEE : MPI SEND STAT
!       ENVOIE L'ETAT OK OU ERREUR AU PROC #0 ET RETOURNE LA REPONSE
!       DU PROC #0.
!-----------------------------------------------------------------------
#ifdef _USE_MPI
!
#include "mpif.h"
#include "asterc/asmpi_irecv_i4.h"
#include "asterc/asmpi_isend_i4.h"
#include "asterc/asmpi_test.h"
#include "asterc/asmpi_wtime.h"
!
    mpi_int :: term
    mpi_int :: rank, ist4(1), irp0(1), req, mpicou, nbv
    mpi_int, parameter :: pr0=0
    real(kind=8) :: tres, timout, t0, tf
! --- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
!
    call asmpi_info(mpicou, rank=rank)
    ASSERT(rank .ne. 0)
    ASSERT(istat.eq.ST_OK .or. istat.eq.ST_ER)
!
    call uttrst(tres)
    timout = tres * 0.2d0
!
!     ENVOI ST_OK OU ST_ERR AU PROC #0
    ist4(1) = istat
    nbv = 1
    DEBUG_MPI('mpi_status', 'isend to proc #0:', istat)
    call asmpi_isend_i4(ist4, nbv, pr0, ST_TAG_CHK, mpicou,&
                        req)
    t0 = asmpi_wtime()
300  continue
!     WHILE NOT TERM
    call asmpi_test(req, term)
!       TIMOUT
    tf = asmpi_wtime()
    if ((tf - t0) .gt. timout) then
        call utmess('E+', 'APPELMPI_96', si=0)
        call utmess('E', 'APPELMPI_83', sk='MPI_ISEND')
        call mpistp(1)
        goto 999
    endif
    if (term .ne. 1) goto 300
!     END WHILE
    DEBUG_MPI('mpi_status', 'isend ', 'done')
!
!     REPONSE DE PROC #0
    irp0(1) = ST_ER
    call asmpi_irecv_i4(irp0, nbv, pr0, ST_TAG_CNT, mpicou,&
                        req)
    t0 = asmpi_wtime()
200  continue
!     WHILE NOT TERM
    call asmpi_test(req, term)
!       TIMOUT
    tf = asmpi_wtime()
    if ((tf - t0) .gt. timout * 1.2) then
        call utmess('E+', 'APPELMPI_96', si=0)
        call utmess('E', 'APPELMPI_83', sk='MPI_IRECV')
        call mpistp(1)
        goto 999
    endif
    if (term .ne. 1) goto 200
!     END WHILE
!
    resp0 = irp0(1)
    DEBUG_MPI('mpi_status', 'proc #0 returns ', resp0)
!
999  continue
#else
    resp0 = istat
#endif
end subroutine
