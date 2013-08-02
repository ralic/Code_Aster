subroutine mpialr()
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
#include "asterf.h"
#include "asterc/gtalrm.h"
#include "asterfort/comcou.h"
#include "asterfort/gtstat.h"
#include "asterfort/mpierr.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
#ifdef _USE_MPI
!
#include "mpif.h"
#include "aster_constant.h"
!
    integer(kind=4) :: rank, nbpro4, iermpi, ival, mpst(MPI_STATUS_SIZE), mpicou
    integer :: i, np1, vali(2)
    logical :: vu
!
! --- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
    call MPI_COMM_RANK(mpicou, rank, iermpi)
    call mpierr(iermpi)
    call MPI_COMM_SIZE(mpicou, nbpro4, iermpi)
    call mpierr(iermpi)
    np1 = nbpro4 - 1
!
    if (.not. gtstat(ST_OK)) then
        if (rank .eq. 0) then
            call u2mess('I', 'CATAMESS_88')
        endif
        goto 9999
    endif
!
!     SUR LES PROCESSEURS AUTRES QUE #0
!
    if (rank .ne. 0) then
!       RECUPERER LA LISTE DES ALARMES
        i = 0
        call gtalrm(i)
!       CHAQUE PROCESSEUR ENVOIE LA LISTE DES ALARMES EMISES AU PROC #0
        ival = i
        call MPI_SEND(ival, 1, MPI_INTEGER4, 0, ST_TAG_ALR,&
                      mpicou, iermpi)
        call mpierr(iermpi)
!
!     SUR LE PROCESSEUR #0
!
    else
!       DEMANDE LA LISTE DES ALARMES A CHAQUE PROCESSEUR
        vu = .false.
        do 10 i = 1, np1
            call MPI_RECV(ival, 1, MPI_INTEGER4, i, ST_TAG_ALR,&
                          mpicou, mpst, iermpi)
            call mpierr(iermpi)
            if (ival .ne. 0) then
                vu = .true.
                vali(1) = i
                vali(2) = ival
                if (ival .eq. 1) then
                    call u2mesi('A+', 'APPELMPI_1', 2, vali)
                else
                    call u2mesi('A+', 'APPELMPI_2', 2, vali)
                endif
            endif
10      continue
        if (vu) then
            call u2mess('A', 'VIDE_1')
        endif
!
    endif
9999  continue
#endif
end subroutine
