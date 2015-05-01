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
    subroutine b3d_elas(var0, nvari, nvar0, depsv, dgamd6,&
                        xk0, xmu0, sigef6, varf, hydra0,&
                        hydra1)
        integer :: nvari
        real(kind=8) :: var0(nvari)
        integer :: nvar0
        real(kind=8) :: depsv
        real(kind=8) :: dgamd6(6)
        real(kind=8) :: xk0
        real(kind=8) :: xmu0
        real(kind=8) :: sigef6(6)
        real(kind=8) :: varf(nvari)
        real(kind=8) :: hydra0
        real(kind=8) :: hydra1
    end subroutine b3d_elas
end interface 
