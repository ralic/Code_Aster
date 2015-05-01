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
    subroutine vipvp2(nbvari, vintm, vintp, advico, vicpvp,&
                      pvp0, pvp1, p2, dp2, t,&
                      dt, kh, mamolv, r, rho11m,&
                      yate, pvp, pvpm, retcom)
        integer :: nbvari
        real(kind=8) :: vintm(nbvari)
        real(kind=8) :: vintp(nbvari)
        integer :: advico
        integer :: vicpvp
        real(kind=8) :: pvp0
        real(kind=8) :: pvp1
        real(kind=8) :: p2
        real(kind=8) :: dp2
        real(kind=8) :: t
        real(kind=8) :: dt
        real(kind=8) :: kh
        real(kind=8) :: mamolv
        real(kind=8) :: r
        real(kind=8) :: rho11m
        integer :: yate
        real(kind=8) :: pvp
        real(kind=8) :: pvpm
        integer :: retcom
    end subroutine vipvp2
end interface
