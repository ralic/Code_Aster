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
    subroutine vipvp1(nbvari, vintm, vintp, advico, vicpvp,&
                      dimcon, p2, congem, adcp11, adcp12,&
                      ndim, pvp0, dp1, dp2, t,&
                      dt, mamolv, r, rho11, signe,&
                      cp11, cp12, yate, pvp, pvpm,&
                      retcom)
        integer :: dimcon
        integer :: nbvari
        real(kind=8) :: vintm(nbvari)
        real(kind=8) :: vintp(nbvari)
        integer :: advico
        integer :: vicpvp
        real(kind=8) :: p2
        real(kind=8) :: congem(dimcon)
        integer :: adcp11
        integer :: adcp12
        integer :: ndim
        real(kind=8) :: pvp0
        real(kind=8) :: dp1
        real(kind=8) :: dp2
        real(kind=8) :: t
        real(kind=8) :: dt
        real(kind=8) :: mamolv
        real(kind=8) :: r
        real(kind=8) :: rho11
        real(kind=8) :: signe
        real(kind=8) :: cp11
        real(kind=8) :: cp12
        integer :: yate
        real(kind=8) :: pvp
        real(kind=8) :: pvpm
        integer :: retcom
    end subroutine vipvp1
end interface
