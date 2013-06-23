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
    subroutine nmedpi(spg, sdg, qg, d, npg,&
                      typmod, mate, up, ud, geom,&
                      nno, def)
        integer :: nno
        real(kind=8) :: spg(2)
        real(kind=8) :: sdg(2)
        real(kind=8) :: qg(2, 2)
        real(kind=8) :: d(4, 2)
        integer :: npg
        character(len=8) :: typmod(*)
        integer :: mate
        real(kind=8) :: up(8)
        real(kind=8) :: ud(8)
        real(kind=8) :: geom(2, nno)
        real(kind=8) :: def(4, nno, 2)
    end subroutine nmedpi
end interface
