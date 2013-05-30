function jexnom(nomc, nomo)
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
    include 'jeveux.h'
!
    character(len=32) :: jexnom
    character(len=*) :: nomc, nomo
!     ------------------------------------------------------------------
    integer :: numec
    common /inumje/  numec
    real(kind=8) :: reelc
    common /reelje/  reelc
    character(len=24) :: nomec
    common /knomje/  nomec
!     ------------------------------------------------------------------
    character(len=24) :: ch24
    character(len=8) :: ch8
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data             ch8      / '$$XNOM  ' /
!     ------------------------------------------------------------------
!
    numec = 0
    reelc = 0.d0
    nomec = nomo
    ch24 = nomc
    jexnom( 1:24) = ch24
    jexnom(25:32) = ch8
end function
