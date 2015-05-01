subroutine deimpj(itestc, itest0, tetaj, testc)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : DETERMINATION DU POINT D'IMPLICITATION DU JACOBIEN
! -----------
!               APPELANT : MDCHOE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    integer :: itestc, itest0
    real(kind=8) :: tetaj
    integer :: testc
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    if ((itest0.ne.0) .or. (itestc.ne.0)) then
!
        if ((itest0.eq.0) .and. (itestc.eq.-1)) then
            tetaj = 0.0d0
            testc = 0
        else if ((itest0.eq.0).and.(itestc.eq.1)) then
            tetaj = 1.0d0
            testc = 1
        else if ((itest0.eq.-1).and.(itestc.eq.0)) then
            tetaj = 1.0d0
            testc = 0
        else if ((itest0.eq.-1).and.(itestc.eq.1)) then
            tetaj = 1.0d0
            testc = 1
        else if ((itest0.eq.1).and.(itestc.eq.0)) then
            tetaj = 0.0d0
            testc = 1
        else if ((itest0.eq.1).and.(itestc.eq.-1)) then
            tetaj = 0.0d0
            testc = 1
        else if ((itest0.eq.1).and.(itestc.eq.1)) then
            tetaj = 1.0d0
            testc = 1
        endif
!
    endif
!
! --- FIN DE DEIMPJ.
end subroutine
