subroutine asmpi_check(nbpro4, iret)
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
! aslint: disable=W1306
!     ARGUMENT IN
#include "asterf_types.h"
#include "asterf.h"
#include "asterc/asmpi_comm.h"
#include "asterc/asmpi_wtime.h"
#include "asterc/uttrst.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/asmpi_status.h"
#include "asterfort/gtstat.h"
#include "asterfort/mpistp.h"
#include "asterfort/ststat.h"
#include "asterfort/utmess.h"
    mpi_int :: nbpro4
!     ARGUMENT OUT
    integer :: iret
!-----------------------------------------------------------------------
!     FONCTION REALISEE : MPI CHECK ERROR
!       AVANT D'EFFECTUER UNE COMMUNICATION BLOQUANTE
!       ON VERIFIE :
!           - QU'AUCUN PROCESSEUR N'A SIGNALE DE PROBLEMES
!           - QUE TOUS LES PROCESSEURS SONT AU RENDEZ-VOUS
!       EN CAS DE PROBLEME, ON RETOURNE IRET != 0, CAR IL NE FAUT ALORS
!       PAS INITIER DE NOUVELLES COMMUNICATIONS.
!-----------------------------------------------------------------------
#ifdef _USE_MPI
!
# ifdef ASTER_DISABLE_MPI_CHECK
    iret = 0
# else
!
#include "mpif.h"
#include "asterf_constant.h"
#include "asterc/asmpi_irecv_i4.h"
#include "asterc/asmpi_send_i4.h"
#include "asterc/asmpi_test.h"
!
    aster_logical :: isterm(nbpro4), lcont
    mpi_int :: term
    integer :: i, nbterm, np1, resp0
    mpi_int :: rank, istat, mpicou, wki(1), nbv, ip4
    mpi_int :: diag(nbpro4), request(nbpro4)
    real(kind=8) :: valr(1), tres, timout, t0, tf
!
! --- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
    iret = 0
    call asmpi_info(mpicou, rank=rank)
    np1 = nbpro4 - 1
    nbv = 1
!
!     SUR LES PROCESSEURS AUTRES QUE #0
!
    if (rank .ne. 0) then
!       CHAQUE PROCESSEUR ENVOIE ST_OK AU PROC #0
        call asmpi_status(ST_OK, resp0)
        if (resp0 .ne. ST_OK) then
            iret = 1
            call utmess('I', 'APPELMPI_80')
            call mpistp(2)
            goto 999
        endif
!
!     SUR LE PROCESSEUR #0
!
    else
        call uttrst(tres)
        timout = tres * 0.2d0
        if (timout < 0) then
            timout = 120.
            call utmess('A', 'APPELMPI_94', sr=timout)
        endif
!
!       DEMANDE LE STATUT A CHAQUE PROCESSEUR
        do 10 i = 1, np1
            isterm(i) = .false.
            ip4 = i
            call asmpi_irecv_i4(diag(i), nbv, ip4, ST_TAG_CHK, mpicou,&
                                request(i))
 10     continue
!
        nbterm = 0
        t0 = asmpi_wtime()
        lcont = .true.
100     continue
!       WHILE LCONT
        do 101 i = 1, np1
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
101     continue
        lcont = nbterm .lt. np1
!         TIMOUT
        tf = asmpi_wtime()
        if (lcont .and. (tf - t0) .gt. timout) then
            lcont = .false.
            valr(1) = timout
            call utmess('E', 'APPELMPI_97', sr=valr(1))
            do 102 i = 1, np1
                if (.not. isterm(i)) then
                    call utmess('E+', 'APPELMPI_96', si=i)
                    call utmess('E', 'APPELMPI_83', sk='MPI_IRECV')
                    call ststat(ST_UN_OTH)
                endif
102         continue
        endif
        if (lcont) goto 100
!       END WHILE
!
        if (gtstat(ST_ER_PR0)) then
            call utmess('I', 'APPELMPI_84', si=0)
        endif
!       DIRE A CEUX QUI ONT REPONDU SI ON CONTINUE OU PAS
        if (gtstat(ST_OK)) then
            istat = ST_OK
        else
            istat = ST_ER
        endif
        do 103 i = 1, np1
            if (isterm(i)) then
                if (istat .ne. ST_OK) then
                    call utmess('I', 'APPELMPI_81', si=i)
                endif
                wki(1) = istat
                ip4 = i
                call asmpi_send_i4(wki, nbv, ip4, ST_TAG_CNT, mpicou)
            endif
103     continue
!
        if (.not. gtstat(ST_OK)) then
            iret = 1
            if (gtstat(ST_UN_OTH)) then
                call mpistp(1)
            else
                call mpistp(2)
            endif
        endif
    endif
999 continue
# endif
!
#else
    iret = to_aster_int(nbpro4)
    iret = 0
#endif
end subroutine
