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
    subroutine tgverm(option, carcri, compor, nno1, nno2,&
                      nno3, geom, ndim, nddl, deplp,&
                      sdepl, vu, vg, vp, vectu,&
                      svect, ncont, contp, scont, nvari,&
                      varip, svari, matuu, smatr, matsym,&
                      epsilo, epsilp, epsilg, varia, iret)
        integer :: nddl
        character(len=16) :: option
        real(kind=8) :: carcri(*)
        character(len=16) :: compor(*)
        integer :: nno1
        integer :: nno2
        integer :: nno3
        real(kind=8) :: geom(*)
        integer :: ndim
        real(kind=8) :: deplp(*)
        real(kind=8) :: sdepl(*)
        integer :: vu(3, 27)
        integer :: vg(27)
        integer :: vp(27)
        real(kind=8) :: vectu(*)
        real(kind=8) :: svect(*)
        integer :: ncont
        real(kind=8) :: contp(*)
        real(kind=8) :: scont(*)
        integer :: nvari
        real(kind=8) :: varip(*)
        real(kind=8) :: svari(*)
        real(kind=8) :: matuu(*)
        real(kind=8) :: smatr(*)
        logical(kind=1) :: matsym
        real(kind=8) :: epsilo
        real(kind=8) :: epsilp
        real(kind=8) :: epsilg
        real(kind=8) :: varia(*)
        integer :: iret
    end subroutine tgverm
end interface
