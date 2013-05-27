subroutine op0165()
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit   none
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM
!
!     ------------------------------------------------------------------
!
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8vide.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/rc3200.h'
    include 'asterfort/rc3600.h'
    include 'asterfort/rccome.h'
    include 'asterfort/rcevol.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesk.h'
    integer :: n1, nbopt, iopt, nbther
    real(kind=8) :: symax
    logical :: pmpb, sn, snet, fatigu, lrocht
    integer :: icodre
    character(len=8) :: k8b, nommat
    character(len=16) :: typtab, typmec, kopt(4), phenom
    integer :: iarg
! DEB ------------------------------------------------------------------
!
    call infmaj()
!
    symax = r8vide()
!
    call getvtx(' ', 'TYPE_RESU', 1, iarg, 1,&
                typtab, n1)
!
    call getvtx(' ', 'TYPE_RESU_MECA', 1, iarg, 1,&
                typmec, n1)
!
!     ------------------------------------------------------------------
!
!     ------------------- TYPE_RESU_MECA = EVOLUTION -------------------
!
!     ------------------------------------------------------------------
!
    if (typmec .eq. 'EVOLUTION') then
!
        call getvtx(' ', 'OPTION', 1, iarg, 0,&
                    k8b, n1)
        nbopt = -n1
        call getvtx(' ', 'OPTION', 1, iarg, nbopt,&
                    kopt, n1)
!
        call getvid(' ', 'MATER', 1, iarg, 1,&
                    nommat, n1)
        call getvr8(' ', 'SY_MAX', 1, iarg, 1,&
                    symax, n1)
!
        call rccome(nommat, 'RCCM', phenom, icodre)
        if (icodre .eq. 1) call u2mesk('F', 'POSTRCCM_7', 1, 'RCCM')
!
        call rcevol(typtab, nommat, symax, nbopt, kopt)
!
!     ------------------------------------------------------------------
!
!     ------------------ TYPE_RESU_MECA = TUYAUTERIE ------------------
!
!     ------------------------------------------------------------------
!
    else if (typmec .eq. 'TUYAUTERIE') then
!
        call getvtx(' ', 'OPTION', 1, iarg, 1,&
                    kopt, n1)
!
        if (kopt(1) .eq. 'FATIGUE') then
!
            call rc3600()
!
        endif
!
!     ------------------------------------------------------------------
!
!     ------------------- TYPE_RESU_MECA = UNITAIRE -------------------
!
!     ------------------------------------------------------------------
!
    else
!
        fatigu = .false.
        pmpb = .false.
        sn = .false.
        snet = .false.
        lrocht = .false.
!
        call getfac('RESU_THER', nbther)
        if (nbther .ne. 0) then
            snet = .true.
            lrocht = .true.
        endif
!
        call getvtx(' ', 'OPTION', 1, iarg, 0,&
                    k8b, n1)
        nbopt = -n1
        call getvtx(' ', 'OPTION', 1, iarg, nbopt,&
                    kopt, n1)
        do 30 iopt = 1, nbopt
            if (kopt(iopt) .eq. 'PM_PB') then
                pmpb = .true.
            else if (kopt(iopt) .eq. 'SN') then
                sn = .true.
            else if (kopt(iopt) .eq. 'FATIGUE') then
                fatigu = .true.
                pmpb = .true.
                sn = .true.
            endif
30      continue
!
        call getvid(' ', 'MATER', 1, iarg, 1,&
                    nommat, n1)
        call getvr8(' ', 'SY_MAX', 1, iarg, 1,&
                    symax, n1)
!
        call rc3200(pmpb, sn, snet, fatigu, lrocht,&
                    nommat, symax)
!
    endif
!
!     ------------------------------------------------------------------
!
    call titre()
!
!
end subroutine
