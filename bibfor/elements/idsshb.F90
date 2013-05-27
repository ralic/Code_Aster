subroutine idsshb(ndim, nno, npg, nomshb)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!--------------------------------------------------------
    implicit none
    integer :: ndim, nno, npg
    character(len=16) :: nomshb
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if ((ndim.eq.3) .and. (nno.eq.8) .and. (npg.eq.5)) then
        nomshb='SHB8'
    endif
!
    if ((ndim.eq.3) .and. (nno.eq.15) .and. (npg.eq.15)) then
        nomshb='SHB15'
    endif
!
    if ((ndim.eq.3) .and. (nno.eq.20) .and. (npg.eq.20)) then
        nomshb='SHB20'
    endif
!
    if ((ndim.eq.3) .and. (nno.eq.6) .and. (npg.eq.5)) then
        nomshb='SHB6'
    endif
end subroutine
