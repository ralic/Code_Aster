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
    subroutine ermes3(noe, ifa, tymvol, nnof, typmav,&
                      iref1, ivois, isig, nbcmp, dsg11,&
                      dsg22, dsg33, dsg12, dsg13, dsg23)
        integer :: noe(9, 6, 4)
        integer :: ifa
        integer :: tymvol
        integer :: nnof
        character(len=8) :: typmav
        integer :: iref1
        integer :: ivois
        integer :: isig
        integer :: nbcmp
        real(kind=8) :: dsg11(9)
        real(kind=8) :: dsg22(9)
        real(kind=8) :: dsg33(9)
        real(kind=8) :: dsg12(9)
        real(kind=8) :: dsg13(9)
        real(kind=8) :: dsg23(9)
    end subroutine ermes3
end interface
