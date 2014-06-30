!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine defgen(testl1, testl2, nno, r, x3,&
                      sina, cosa, cour, vf, dfds,&
                      depl, eps, epsx3)
        logical(kind=1) :: testl1
        logical(kind=1) :: testl2
        integer :: nno
        real(kind=8) :: r
        real(kind=8) :: x3
        real(kind=8) :: sina
        real(kind=8) :: cosa
        real(kind=8) :: cour
        real(kind=8) :: vf(*)
        real(kind=8) :: dfds(*)
        real(kind=8) :: depl(*)
        real(kind=8) :: eps(*)
        real(kind=8) :: epsx3
    end subroutine defgen
end interface
