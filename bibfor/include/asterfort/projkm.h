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
    subroutine projkm(nmabet, nbmabe, nbnobe, mailla, caelem,&
                  nnoeca, x3dca, noebe, numail, nbcnx,&
                  cxma, xyzma, normal, itria, xbar,&
                  iproj, excent)
        character(len=24) :: nmabet
        integer :: nbmabe
        integer :: nbnobe
        character(len=8) :: mailla
        character(len=8) :: caelem
        character(len=8) :: nnoeca
        real(kind=8) :: x3dca(*)
        integer :: noebe
        integer :: numail
        integer :: nbcnx
        integer :: cxma(*)
        real(kind=8) :: xyzma(3, *)
        real(kind=8) :: normal(*)
        integer :: itria
        real(kind=8) :: xbar(*)
        integer :: iproj
        real(kind=8) :: excent
    end subroutine projkm
end interface
