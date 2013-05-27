subroutine mpichk(nbpro4, iret)
!           CONFIGURATION MANAGEMENT OF EDF VERSION
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ==================================================================
! person_in_charge: mathieu.courtois at edf.fr
    implicit none
!     ARGUMENT IN
    include 'asterc/uttrst.h'
    include 'asterfort/comcou.h'
    include 'asterfort/gtstat.h'
    include 'asterfort/mpierr.h'
    include 'asterfort/mpisst.h'
    include 'asterfort/mpistp.h'
    include 'asterfort/ststat.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/u2mess.h'
    integer(kind=4) :: nbpro4
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
    include 'mpif.h'
    include 'aster_constant.h'
!
    logical :: isterm(nbpro4), lcont
    logical(kind=4) :: term
    integer :: i, nbterm, np1, resp0
    integer(kind=4) :: rank, iermpi, istat, mpicou
    integer(kind=4) :: diag(nbpro4), request(nbpro4), mpst(MPI_STATUS_SIZE)
    real(kind=8) :: valr(1), tres, timout, t0, tf
!
! --- COMMUNICATEUR MPI DE TRAVAIL
    mpicou=comcou(1)
    iret = 0
    call MPI_COMM_RANK(mpicou, rank, iermpi)
    call mpierr(iermpi)
    np1 = nbpro4 - 1
!
    call uttrst(tres)
    timout = tres * 0.2d0
!
!     SUR LES PROCESSEURS AUTRES QUE #0
!
    if (rank .ne. 0) then
!       CHAQUE PROCESSEUR ENVOIE ST_OK AU PROC #0
        call mpisst(st_ok, resp0)
        if (resp0 .ne. st_ok) then
            iret = 1
            call u2mess('I', 'APPELMPI_80')
            call mpistp(2)
            goto 9999
        endif
!
!     SUR LE PROCESSEUR #0
!
    else
!       DEMANDE LE STATUT A CHAQUE PROCESSEUR
        do 10 i = 1, np1
            isterm(i) = .false.
            call MPI_IRECV(diag(i), 1, MPI_INTEGER4, i, ST_TAG_CHK,&
                           mpicou, request(i), iermpi)
            call mpierr(iermpi)
10      continue
!
        nbterm = 0
        t0 = MPI_WTIME()
        lcont = .true.
100      continue
!       WHILE LCONT
        do 101 i = 1, np1
            if (.not. isterm(i)) then
                call MPI_TEST(request(i), term, mpst, iermpi)
                call mpierr(iermpi)
                if (term) then
                    nbterm = nbterm + 1
                    isterm(i) = .true.
                    if (diag(i) .eq. st_er) then
                        call u2mesi('I', 'APPELMPI_84', 1, i)
                        call ststat(ST_ER_OTH)
                    endif
                endif
            endif
101      continue
        lcont = nbterm .lt. np1
!         TIMOUT
        tf = MPI_WTIME()
        if (lcont .and. (tf - t0) .gt. timout) then
            lcont = .false.
            valr(1) = timout
            call u2mesr('E', 'APPELMPI_97', 1, valr)
            do 102 i = 1, np1
                if (.not. isterm(i)) then
                    call u2mesi('E+', 'APPELMPI_96', 1, i)
                    call u2mesk('E', 'APPELMPI_83', 1, 'MPI_IRECV')
                    call ststat(ST_UN_OTH)
                endif
102          continue
        endif
        if (lcont) goto 100
!       END WHILE
!
        if (gtstat(ST_ER_PR0)) then
            call u2mesi('I', 'APPELMPI_84', 1, 0)
        endif
!       DIRE A CEUX QUI ONT REPONDU SI ON CONTINUE OU PAS
        if (gtstat(st_ok)) then
            istat = st_ok
        else
            istat = st_er
        endif
        do 103 i = 1, np1
            if (isterm(i)) then
                if (istat .ne. st_ok) then
                    call u2mesi('I', 'APPELMPI_81', 1, i)
                endif
                call MPI_SEND(istat, 1, MPI_INTEGER4, i, ST_TAG_CNT,&
                              mpicou, iermpi)
                call mpierr(iermpi)
            endif
103      continue
!
        if (.not. gtstat(st_ok)) then
            iret = 1
            if (gtstat(ST_UN_OTH)) then
                call mpistp(1)
            else
                call mpistp(2)
            endif
        endif
    endif
9999  continue
# endif
!
#endif
end subroutine
