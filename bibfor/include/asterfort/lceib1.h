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
    subroutine lceib1(fami, kpg, ksp, imate, compor,&
                      ndim, epsm, sref, sechm, hydrm,&
                      t, lambda, deuxmu, epsthe, kdess,&
                      bendo, gamma, seuil)
        character(len=*) :: fami
        integer :: kpg
        integer :: ksp
        integer :: imate
        character(len=16) :: compor(*)
        integer :: ndim
        real(kind=8) :: epsm(6)
        real(kind=8) :: sref
        real(kind=8) :: sechm
        real(kind=8) :: hydrm
        integer :: t(3, 3)
        real(kind=8) :: lambda
        real(kind=8) :: deuxmu
        real(kind=8) :: epsthe(2)
        real(kind=8) :: kdess
        real(kind=8) :: bendo
        real(kind=8) :: gamma
        real(kind=8) :: seuil
    end subroutine lceib1
end interface
