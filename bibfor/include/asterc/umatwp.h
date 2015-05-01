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
    subroutine umatwp(pfumat, stress, statev, ddsdde,&
                      sse, spd, scd, rpl, ddsddt,&
                      drplde, drpldt, stran, dstran, time,&
                      dtime, temp, dtemp, predef, dpred,&
                      cmname, ndi, nshr, ntens, nstatv,&
                      props, nprops, coords, drot, pnewdt,&
                      celent, dfgrd0, dfgrd1, noel, npt,&
                      layer, kspt, kstep, kinc)
        integer :: pfumat
        real(kind=8) :: stress(*)
        real(kind=8) :: statev(*)
        real(kind=8) :: ddsdde(*)
        real(kind=8) :: sse
        real(kind=8) :: spd
        real(kind=8) :: scd
        real(kind=8) :: rpl
        real(kind=8) :: ddsddt(*)
        real(kind=8) :: drplde(*)
        real(kind=8) :: drpldt
        real(kind=8) :: stran(*)
        real(kind=8) :: dstran(*)
        real(kind=8) :: time(*)
        real(kind=8) :: dtime
        real(kind=8) :: temp
        real(kind=8) :: dtemp
        real(kind=8) :: predef(*)
        real(kind=8) :: dpred(*)
        character(len=*) :: cmname
        integer :: ndi
        integer :: nshr
        integer :: ntens
        integer :: nstatv
        real(kind=8) :: props(*)
        integer :: nprops
        real(kind=8) :: coords(*)
        real(kind=8) :: drot(*)
        real(kind=8) :: pnewdt
        real(kind=8) :: celent
        real(kind=8) :: dfgrd0(3,3)
        real(kind=8) :: dfgrd1(3,3)
        integer :: noel
        integer :: npt
        integer :: layer
        integer :: kspt
        integer :: kstep
        integer :: kinc
    end subroutine umatwp
end interface
