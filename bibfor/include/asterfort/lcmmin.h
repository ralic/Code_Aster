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
    subroutine lcmmin(typess, essai, mod, nmat, materf,&
                      nr, nvi, yd, deps, dy,&
                      comp, nbcomm, cpmono, pgl, nfs,&
                      nsg, toutms, timed, timef, vind,&
                      sigd, epstr)
        integer :: nsg
        integer :: nfs
        integer :: nr
        integer :: nmat
        integer :: typess
        real(kind=8) :: essai
        character(len=8) :: mod
        real(kind=8) :: materf(nmat, 2)
        integer :: nvi
        real(kind=8) :: yd(nr)
        real(kind=8) :: deps(6)
        real(kind=8) :: dy(nr)
        character(len=16) :: comp(*)
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        real(kind=8) :: pgl(3, 3)
        real(kind=8) :: toutms(nfs, nsg, 6)
        real(kind=8) :: timed
        real(kind=8) :: timef
        real(kind=8) :: vind(*)
        real(kind=8) :: sigd(6)
        real(kind=8) :: epstr(6)
    end subroutine lcmmin
end interface
