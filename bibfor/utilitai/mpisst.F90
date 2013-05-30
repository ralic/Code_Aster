subroutine mpisst(istat, resp0)
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
    include 'asterc/uttrst.h'
    include 'asterfort/assert.h'
    include 'asterfort/comcou.h'
    include 'asterfort/mpierr.h'
    include 'asterfort/mpistp.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    integer :: istat, resp0
!-----------------------------------------------------------------------
!     FONCTION REALISEE : MPI SEND STAT
!       ENVOIE L'ETAT OK OU ERREUR AU PROC #0 ET RETOURNE LA REPONSE
!       DU PROC #0.
!-----------------------------------------------------------------------
#ifdef _USE_MPI
!
    include 'mpif.h'
    include 'aster_constant.h'
!
    logical(kind=4) :: term
    integer(kind=4) :: rank, iermpi
    integer(kind=4) :: ist4, irp0, mpst(MPI_STATUS_SIZE), req, mpicou
    real(kind=8) :: tres, timout, t0, tf
! --- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
!
    call MPI_COMM_RANK(mpicou, rank, iermpi)
    call mpierr(iermpi)
    call assert(rank .ne. 0)
    call assert(istat.eq.st_ok .or. istat.eq.st_er)
!
    call uttrst(tres)
    timout = tres * 0.2d0
!
!     ENVOI ST_OK OU ST_ERR AU PROC #0
    ist4 = istat
    call MPI_ISEND(ist4, 1, MPI_INTEGER4, 0, ST_TAG_CHK,&
                   mpicou, req, iermpi)
    call mpierr(iermpi)
    t0 = MPI_WTIME()
300  continue
!     WHILE NOT TERM
    call MPI_TEST(req, term, mpst, iermpi)
    call mpierr(iermpi)
!       TIMOUT
    tf = MPI_WTIME()
    if ((tf - t0) .gt. timout) then
        call u2mesi('E+', 'APPELMPI_96', 1, 0)
        call u2mesk('E', 'APPELMPI_83', 1, 'MPI_ISEND')
        call mpistp(1)
        goto 9999
    endif
    if (.not.term) goto 300
!     END WHILE
!
!     REPONSE DE PROC #0
    irp0 = st_er
    call MPI_IRECV(irp0, 1, MPI_INTEGER4, 0, ST_TAG_CNT,&
                   mpicou, req, iermpi)
    call mpierr(iermpi)
    t0 = MPI_WTIME()
200  continue
!     WHILE NOT TERM
    call MPI_TEST(req, term, mpst, iermpi)
    call mpierr(iermpi)
!       TIMOUT
    tf = MPI_WTIME()
    if ((tf - t0) .gt. timout * 1.2) then
        call u2mesi('E+', 'APPELMPI_96', 1, 0)
        call u2mesk('E', 'APPELMPI_83', 1, 'MPI_IRECV')
        call mpistp(1)
        goto 9999
    endif
    if (.not.term) goto 200
!     END WHILE
!
    resp0 = irp0
!
9999  continue
#endif
end subroutine
