subroutine asmpi_checkalarm()
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
!-----------------------------------------------------------------------
!     FONCTION REALISEE : MPI CHECK ALARM
!       EN FIN D'EXECUTION, LE PROCESSEUR #0 DONNE A L'UTILISATEUR
!       LA LISTE DES ALARMES QUI ONT ETE EMISES PAR PROCESSEUR.
!-----------------------------------------------------------------------
#include "asterf_types.h"
#include "asterf.h"
#include "asterc/asmpi_comm.h"
#include "asterc/gtalrm.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/gtstat.h"
#include "asterfort/utmess.h"
#ifdef _USE_MPI
!
#include "mpif.h"
#include "asterf_constant.h"
#include "asterc/asmpi_recv_i4.h"
#include "asterc/asmpi_send_i4.h"
!
    mpi_int :: i, rank, nbpro4, ival(1), mpicou, mpicow, nbv
    mpi_int, parameter :: pr0=0
    integer :: ia, np1, vali(2)
    aster_logical :: vu
!
! --- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET_WORLD', mpicow)
    call asmpi_comm('GET', mpicou)
    ASSERT(mpicow == mpicou)
    call asmpi_info(mpicou, rank=rank, size=nbpro4)
    np1 = nbpro4 - 1
    nbv = 1
!
    if (.not. gtstat(ST_OK)) then
        if (rank .eq. 0) then
            call utmess('I', 'CATAMESS_88')
        endif
        goto 9999
    endif
!
!     SUR LES PROCESSEURS AUTRES QUE #0
!
    if (rank .ne. 0) then
!       RECUPERER LA LISTE DES ALARMES
        ia = 0
        call gtalrm(ia)
!       CHAQUE PROCESSEUR ENVOIE LA LISTE DES ALARMES EMISES AU PROC #0
        ival(1) = ia
        call asmpi_send_i4(ival, nbv, pr0, ST_TAG_ALR, mpicou)
!
!     SUR LE PROCESSEUR #0
!
    else
!       DEMANDE LA LISTE DES ALARMES A CHAQUE PROCESSEUR
        vu = .false.
        do i = 1, np1
            call asmpi_recv_i4(ival, nbv, i, ST_TAG_ALR, mpicou)
            if (ival(1) .ne. 0) then
                vu = .true.
                vali(1) = i
                vali(2) = ival(1)
                if (ival(1) .eq. 1) then
                    call utmess('A+', 'APPELMPI_1', ni=2, vali=vali)
                else
                    call utmess('A+', 'APPELMPI_2', ni=2, vali=vali)
                endif
            endif
        end do
        if (vu) then
            call utmess('A', 'VIDE_1')
        endif
!
    endif
9999 continue
#endif
end subroutine
