subroutine mpicmw(iexc)
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
#include "asterfort/comcou.h"
#include "asterfort/gtstat.h"
#include "asterfort/mpichk.h"
#include "asterfort/mpierr.h"
#include "asterfort/mpiexe.h"
#include "asterfort/mpisst.h"
#include "asterfort/ststat.h"
#include "asterfort/u2mess.h"
    integer :: iexc
!-----------------------------------------------------------------------
!     FONCTION REALISEE : MPI COMM WARN
!       UN PROCESSEUR A RENCONTRE UNE ERREUR, IL EMET SON MESSAGE
!       ET ENVOIE L'INFO AU PROC #0.
!-----------------------------------------------------------------------
#if defined(_USE_MPI) && !defined(ASTER_DISABLE_MPI_CHECK)
!
#include "mpif.h"
#include "aster_constant.h"
!
    integer(kind=4) :: iermpi, rank, nbpro4, mpicou, mpicow
    integer :: iret, ibid
!
! --- COMMUNICATEUR MPI COM_WORLD (MPICOW) ET COM COURANT (MPICOU)
! --- SI ILS SONT DIFFERENTS, ON NETTOIE ET ON MET LE COM WORLD POUR
! --- QUE LE PLANTON CONCERNE TOUS LES PROCESSUS MPI.
    mpicow=comcou(0)
    mpicou=comcou(1)
    if (mpicou .ne. mpicow) then
        call mpiexe('MPI_COMM_FREE', mpicou, ibid, ibid, ibid)
        call mpiexe('AFFE_COMM_REFE', mpicow, ibid, 1, ibid)
        mpicou=mpicow
    endif
!
    call MPI_COMM_RANK(mpicou, rank, iermpi)
    call mpierr(iermpi)
    call MPI_COMM_SIZE(mpicou, nbpro4, iermpi)
    call mpierr(iermpi)
!
!     SI PAS 'ST_OK', IL NE FAUT PAS COMMUNIQUER ENCORE UNE FOIS
    if (nbpro4 .le. 1 .or. .not. gtstat(ST_OK)) then
        goto 9999
    endif
!
!     SUR LES PROCESSEURS AUTRES QUE #0
    if (rank .ne. 0) then
        call ststat(ST_ER_OTH)
        if (iexc .eq. 0) then
            call u2mess('I', 'APPELMPI_82')
        else
            call u2mess('I', 'APPELMPI_92')
            call ststat(ST_EXCEPT)
        endif
        call mpisst(ST_ER, iret)
!
!     SUR LE PROCESSEUR #0
    else
        call ststat(ST_ER_PR0)
        if (iexc .eq. 1) then
            call ststat(ST_EXCEPT)
        endif
        call mpichk(nbpro4, iret)
    endif
!     INUTILE DE TESTER IRET, ON SAIT QU'IL Y A UNE ERREUR
!
9999  continue
#endif
end subroutine
