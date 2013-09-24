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
    subroutine tgveri(option, carcri, compor, nno, geom,&
                      ndim, nddl, deplp, sdepl, vectu,&
                      svect, ncont, contp, scont, nvari,&
                      varip, svari, matuu, smatr, matsym,&
                      epsilo, varia, iret)
        character(len=16) :: option
        real(kind=8) :: carcri(*)
        character(len=16) :: compor(*)
        integer :: nno
        real(kind=8) :: geom(*)
        integer :: ndim
        integer :: nddl
        real(kind=8) :: deplp(*)
        real(kind=8) :: sdepl(*)
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
        logical :: matsym
        real(kind=8) :: epsilo
        real(kind=8) :: varia(*)
        integer :: iret
    end subroutine tgveri
end interface
