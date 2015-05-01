!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine mmsauv(resoco, izone, iptc, nummam, ksipr1,&
                      ksipr2, tau1, tau2, nummae, numnoe,&
                      ksipc1, ksipc2, wpc)
        character(len=24) :: resoco
        integer :: izone
        integer :: iptc
        integer :: nummam
        real(kind=8) :: ksipr1
        real(kind=8) :: ksipr2
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        integer :: nummae
        integer :: numnoe
        real(kind=8) :: ksipc1
        real(kind=8) :: ksipc2
        real(kind=8) :: wpc
    end subroutine mmsauv
end interface
