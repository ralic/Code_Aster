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
    subroutine w155ma(numa, nucou, nicou, nangl, nufib,&
                      motfac, jce2d, jce2l, jce2v, jce5d,&
                      jce5l, jce5v, ksp1, ksp2, c1,&
                      c2, iret)
        integer :: numa
        integer :: nucou
        character(len=3) :: nicou
        integer :: nangl
        integer :: nufib
        character(len=16) :: motfac
        integer :: jce2d
        integer :: jce2l
        integer :: jce2v
        integer :: jce5d
        integer :: jce5l
        integer :: jce5v
        integer :: ksp1
        integer :: ksp2
        real(kind=8) :: c1
        real(kind=8) :: c2
        integer :: iret
    end subroutine w155ma
end interface
