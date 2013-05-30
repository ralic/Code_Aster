subroutine acevpc(nbocc, nlm, nlg, ier)
    implicit none
    include 'asterc/getres.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/u2mess.h'
    integer :: nbocc, nlm, nlg, ier
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     VERIFICATION DES MOTS CLES POUR L'ELEMENT POUTRE COURBE
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! OUT : NLM    : NOMBRE TOTAL DE MAILLE
! OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
! ----------------------------------------------------------------------
    real(kind=8) :: xrc, xoa, xfl, xsi
    character(len=8) :: nomu
    character(len=16) :: concep, cmd
    character(len=8) :: k8bid
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ioc, na, nc, nf, ng, nm, np
    integer :: nr, ns, nsom
    real(kind=8) :: xoc, xop
!-----------------------------------------------------------------------
    call getres(nomu, concep, cmd)
!
    nlm = 0
    nlg = 0
    do 10 ioc = 1, nbocc
        call getvtx('DEFI_ARC', 'GROUP_MA', ioc, iarg, 0,&
                    k8bid, ng)
        call getvtx('DEFI_ARC', 'MAILLE', ioc, iarg, 0,&
                    k8bid, nm)
        call getvr8('DEFI_ARC', 'RAYON', ioc, iarg, 1,&
                    xrc, nr)
        call getvr8('DEFI_ARC', 'ORIE_ARC', ioc, iarg, 1,&
                    xoa, na)
        call getvr8('DEFI_ARC', 'CENTRE', ioc, iarg, 0,&
                    xoc, nc)
        call getvr8('DEFI_ARC', 'POIN_TANG', ioc, iarg, 0,&
                    xop, np)
        call getvr8('DEFI_ARC', 'COEF_FLEX', ioc, iarg, 1,&
                    xfl, nf)
        call getvr8('DEFI_ARC', 'INDI_SIGM', ioc, iarg, 1,&
                    xsi, ns)
!
        if (nr .ne. 0) then
            if (xrc .le. 0.d0) then
                call u2mess('E', 'MODELISA_61')
                ier = ier + 1
            endif
        endif
        if (nc .ne. 0 .and. nc .ne. -3) then
            call u2mess('E', 'MODELISA_62')
            ier = ier + 1
        endif
        if (np .ne. 0 .and. np .ne. -3) then
            call u2mess('E', 'MODELISA_63')
            ier = ier + 1
        endif
        if (nf .ne. 0) then
            if (xfl .le. 0.d0) then
                call u2mess('E', 'MODELISA_64')
                ier = ier + 1
            endif
        endif
        if (ns .ne. 0) then
            if (xsi .le. 0.d0) then
                call u2mess('E', 'MODELISA_65')
                ier = ier + 1
            endif
        endif
!
        nsom = ng + nm
        if (nsom .eq. ng .or. nsom .eq. nm) then
            nlm = max(nlm,-nm)
            nlg = max(nlg,-ng)
        endif
!
10  end do
!
end subroutine
