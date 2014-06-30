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
    subroutine mmvppe(typmae, typmam, iresog, ndim, nne,&
                      nnm, nnl, nbdm, laxis, ldyna,&
                      lfovit, jeusup, ffe, ffm, ffl,&
                      norm, tau1, tau2, mprojt, jacobi,&
                      wpg, dlagrc, dlagrf, jeu, djeu,&
                      djeut)
        character(len=8) :: typmae
        character(len=8) :: typmam
        integer :: iresog
        integer :: ndim
        integer :: nne
        integer :: nnm
        integer :: nnl
        integer :: nbdm
        logical(kind=1) :: laxis
        logical(kind=1) :: ldyna
        logical(kind=1) :: lfovit
        real(kind=8) :: jeusup
        real(kind=8) :: ffe(9)
        real(kind=8) :: ffm(9)
        real(kind=8) :: ffl(9)
        real(kind=8) :: norm(3)
        real(kind=8) :: tau1(3)
        real(kind=8) :: tau2(3)
        real(kind=8) :: mprojt(3, 3)
        real(kind=8) :: jacobi
        real(kind=8) :: wpg
        real(kind=8) :: dlagrc
        real(kind=8) :: dlagrf(2)
        real(kind=8) :: jeu
        real(kind=8) :: djeu(3)
        real(kind=8) :: djeut(3)
    end subroutine mmvppe
end interface
