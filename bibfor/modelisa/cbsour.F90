subroutine cbsour(char, noma, ligrmo, ndim, fonree)
    implicit   none
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterfort/casour.h'
    include 'asterfort/copisd.h'
    include 'asterfort/u2mess.h'
    integer :: ndim
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: nbcalc, icalc, nbfac, isour, iocc
    real(kind=8) :: r8bid
    character(len=8) :: scalc
    character(len=16) :: motfac
    character(len=24) :: carte, chsour
    integer :: iarg
!     ------------------------------------------------------------------
!
    carte = char//'.CHTH.SOURE'
!
    motfac = 'SOURCE'
    call getfac(motfac, nbfac)
!
    nbcalc = 0
    if (fonree .eq. 'REEL') then
        do 10 iocc = 1, nbfac
            call getvid(motfac, 'SOUR_CALCULEE', iocc, iarg, 1,&
                        chsour, icalc)
            nbcalc = nbcalc + icalc
10      continue
        if (nbcalc .gt. 1) then
            call u2mess('F', 'MODELISA3_64')
        else if (nbcalc.eq.1) then
            call copisd('CHAMP_GD', 'G', chsour(1:19), carte(1:19))
        endif
    endif
!
    if (fonree .eq. 'REEL') then
        call getvr8(motfac, 'SOUR', 1, iarg, 1,&
                    r8bid, isour)
    else
        call getvid(motfac, 'SOUR', 1, iarg, 1,&
                    scalc, isour)
    endif
    if (isour .eq. 1) then
        call casour(char, ligrmo, noma, ndim, fonree)
    endif
!
end subroutine
