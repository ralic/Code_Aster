subroutine ibtcpu(ier)
    implicit none
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/gtoptr.h'
    include 'asterc/rdtmax.h'
    include 'asterfort/assert.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    integer :: ier
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     OPTION DE MODIFICATION DE LA LIMITE DE TEMPS CPU POUR CONSERVER
!     UNE MARGE SUFFISANTE AFIN DE TERMINER PROPREMENT UNE EXECUTION
!     ------------------------------------------------------------------
!            0 TOUT C'EST BIEN PASSE
!            1 ERREUR DANS LA LECTURE DE LA COMMANDE
!     ------------------------------------------------------------------
!
    integer :: l1, l2, l3, lcpu, iborne, itpmax, iret, vali(3)
    real(kind=8) :: pccpu, tpmax, dix, ntmax
    parameter(dix=10.d0)
    character(len=16) :: cbid, nomcmd
    integer :: iarg
!
    ier = 0
    tpmax = 0.d0
    ntmax = 0.d0
    l1=0
    l2=0
!     RECUPERATION DU TEMPS LIMITE DE L'EXECUTION
    call gtoptr('tpmax', tpmax, iret)
    call assert(iret.eq.0)
    itpmax = nint(tpmax)
!
    call getfac('CODE', iret)
!
    call getvis('RESERVE_CPU', 'VALE', 1, iarg, 1,&
                lcpu, l1)
    call getvr8('RESERVE_CPU', 'POURCENTAGE', 1, iarg, 1,&
                pccpu, l2)
    call getvis('RESERVE_CPU', 'BORNE', 1, iarg, 1,&
                iborne, l3)
!
!     PERMET D'AFFECTER DES VALEURS PAR DEFAUT EN FONCTION DE LA
!     PRESENCE DE CODE
!
!     SI CODE PRESENT
!
    if (iret .gt. 0 .and. l1 .eq. 0 .and. l2 .eq. 0) then
        ntmax = tpmax - dix
        call rdtmax(itpmax-ntmax)
        goto 100
    endif
!
!     SI CODE ABSENT
!
    if (iret .eq. 0 .and. l1 .eq. 0 .and. l2 .eq. 0) then
        pccpu=0.1d0
        ntmax = max ( tpmax*(1-pccpu) , tpmax-iborne )
        call rdtmax(itpmax-ntmax)
        goto 100
    endif
!
    if (l1 .gt. 0) then
        if (lcpu .gt. tpmax) then
            call getres(cbid, cbid, nomcmd)
            call u2mess('F', 'SUPERVIS_31')
            ier = 1
        endif
        ntmax = tpmax - lcpu
        call rdtmax(itpmax-ntmax)
    endif
!
    if (l2 .gt. 0) then
        ntmax = max ( tpmax*(1-pccpu) , tpmax-iborne )
        call rdtmax(itpmax-ntmax)
    endif
!
!     IMPRESSION D'UN MESSAGE D'INFORMATION
!
100  continue
    vali(1)=itpmax
    vali(2)=int(ntmax)
    vali(3)=int(itpmax-ntmax)
    call u2mesi('I', 'SUPERVIS_64', 3, vali)
!
end subroutine
