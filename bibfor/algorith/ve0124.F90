subroutine ve0124()
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
! ----------------------------------------------------------------------
!     COMMANDE: CREA_RESU
!     VERIFICATION DE NIVEAU 1
! ----------------------------------------------------------------------
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: k8bid, resu
    character(len=16) :: type, oper, typres
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, iocc, k, n0, n1
    real(kind=8) :: r8bid
!-----------------------------------------------------------------------
    call getres(resu, type, oper)
    call getvtx(' ', 'TYPE_RESU', 0, iarg, 1,&
                typres, n1)
    call getfac('AFFE', iocc)
!
    if (typres .eq. 'EVOL_THER') then
        do 700 k = 1, iocc
            call getvtx('AFFE', 'NOM_CAS', k, iarg, 1,&
                        k8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_7')
            endif
            call getvis('AFFE', 'NUME_MODE', k, iarg, 1,&
                        ibid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_8')
            endif
700      continue
!
    else if (typres .eq. 'MULT_ELAS') then
        do 702 k = 1, iocc
            call getvis('AFFE', 'NUME_MODE', k, iarg, 1,&
                        ibid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_9')
            endif
            call getvr8('AFFE', 'INST', k, iarg, 1,&
                        r8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_10')
            endif
            call getvid('AFFE', 'LIST_INST', k, iarg, 1,&
                        k8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_10')
            endif
702      continue
!
    else if (typres .eq. 'FOURIER_ELAS') then
        do 704 k = 1, iocc
            call getvtx('AFFE', 'NOM_CAS', k, iarg, 1,&
                        k8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_11')
            endif
            call getvr8('AFFE', 'INST', k, iarg, 1,&
                        r8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_12')
            endif
            call getvid('AFFE', 'LIST_INST', k, iarg, 1,&
                        k8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_12')
            endif
704      continue
!
    else if (typres .eq. 'FOURIER_THER') then
        do 705 k = 1, iocc
            call getvtx('AFFE', 'NOM_CAS', k, iarg, 1,&
                        k8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_13')
            endif
            call getvr8('AFFE', 'INST', k, iarg, 1,&
                        r8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_14')
            endif
            call getvid('AFFE', 'LIST_INST', k, iarg, 1,&
                        k8bid, n0)
            if (n0 .ne. 0) then
                call u2mess('E', 'ALGORITH11_14')
            endif
705      continue
    endif
!
    call getfac('PERM_CHAM', iocc)
    if (iocc .gt. 0) then
        call getvid(' ', 'RESU_INIT', 1, iarg, 0,&
                    k8bid, n1)
        if (n1 .eq. 0) then
            call u2mess('E', 'ALGORITH11_15')
        endif
        call getvid(' ', 'MAILLAGE_INIT', 1, iarg, 0,&
                    k8bid, n1)
        if (n1 .eq. 0) then
            call u2mess('E', 'ALGORITH11_16')
        endif
        call getvid(' ', 'RESU_FINAL', 1, iarg, 0,&
                    k8bid, n1)
        if (n1 .eq. 0) then
            call u2mess('E', 'ALGORITH11_17')
        endif
        call getvid(' ', 'MAILLAGE_FINAL', 1, iarg, 0,&
                    k8bid, n1)
        if (n1 .eq. 0) then
            call u2mess('E', 'ALGORITH11_18')
        endif
    endif
!
end subroutine
