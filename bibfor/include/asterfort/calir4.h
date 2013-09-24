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
    subroutine calir4(noma, lisrel, nono2, ino2, v1,&
                      jconb1, jcocf1, jconu1, ideca1, jconb2,&
                      jcocf2, jconu2, ideca2)
        character(len=8) :: noma
        character(len=19) :: lisrel
        character(len=8) :: nono2
        integer :: ino2
        real(kind=8) :: v1(3)
        integer :: jconb1
        integer :: jcocf1
        integer :: jconu1
        integer :: ideca1
        integer :: jconb2
        integer :: jcocf2
        integer :: jconu2
        integer :: ideca2
    end subroutine calir4
end interface
