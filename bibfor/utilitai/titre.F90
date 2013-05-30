subroutine titre()
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CREATION D'UN TITRE ATTACHE A UN CONCEPT
!     ------------------------------------------------------------------
    include 'asterc/getres.h'
    include 'asterfort/titrea.h'
    character(len=8) :: nomcon, cbid
    character(len=24) :: nomobj
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call getres(nomcon, cbid, cbid)
    nomobj = ' '
    nomobj(1:8) = nomcon
    nomobj(20:24) = '.TITR'
    call titrea('T', nomcon, nomcon, nomobj, 'C',&
                ' ', 0, 'G', '(1PE12.5)')
end subroutine
