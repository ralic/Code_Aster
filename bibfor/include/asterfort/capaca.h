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
    subroutine capaca(rho0, rho11, rho12, rho21, rho22,&
                      sat, phi, csigm, cp11, cp12,&
                      cp21, cp22, dalal, t, coeps,&
                      retcom)
        real(kind=8) :: rho0
        real(kind=8) :: rho11
        real(kind=8) :: rho12
        real(kind=8) :: rho21
        real(kind=8) :: rho22
        real(kind=8) :: sat
        real(kind=8) :: phi
        real(kind=8) :: csigm
        real(kind=8) :: cp11
        real(kind=8) :: cp12
        real(kind=8) :: cp21
        real(kind=8) :: cp22
        real(kind=8) :: dalal
        real(kind=8) :: t
        real(kind=8) :: coeps
        integer :: retcom
    end subroutine capaca
end interface 
