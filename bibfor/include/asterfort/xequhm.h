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
    subroutine xequhm(imate, option, ta, ta1, ndim,&
                      compor, kpi, npg, dimenr,&
                      enrmec, dimdef, dimcon, nbvari, defgem,&
                      congem, vintm, defgep, congep, vintp,&
                      mecani, press1, press2, tempe,&
                      rinstp, dt, r, drds,&
                      dsde, retcom, idecpg, angmas)
        integer :: nbvari
        integer :: dimcon
        integer :: dimdef
        integer :: dimenr
        integer :: imate
        character(len=16) :: option
        real(kind=8) :: ta
        real(kind=8) :: ta1
        integer :: ndim
        character(len=16) :: compor(*)
        integer :: kpi
        integer :: npg
        integer :: enrmec(3)
        real(kind=8) :: defgem(1:dimdef)
        real(kind=8) :: congem(1:dimcon)
        real(kind=8) :: vintm(1:nbvari)
        real(kind=8) :: defgep(1:dimdef)
        real(kind=8) :: congep(1:dimcon)
        real(kind=8) :: vintp(1:nbvari)
        integer :: mecani(5)
        integer :: press1(7)
        integer :: press2(7)
        integer :: tempe(5)
        real(kind=8) :: rinstp
        real(kind=8) :: dt
        real(kind=8) :: r(1:dimenr)
        real(kind=8) :: drds(1:dimenr, 1:dimcon)
        real(kind=8) :: dsde(1:dimcon, 1:dimenr)
        integer :: retcom
        integer :: idecpg
        real(kind=8) :: angmas(3)
    end subroutine xequhm
end interface 
