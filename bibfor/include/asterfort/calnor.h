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
    subroutine calnor(chdim, geom, iare, nnos, nnoa,&
                      orien, nno, npg, noe, ifa,&
                      tymvol, idfde, jac, nx, ny,&
                      nz, tx, ty, hf)
        character(len=2) :: chdim
        real(kind=8) :: geom(*)
        integer :: iare
        integer :: nnos
        integer :: nnoa
        real(kind=8) :: orien
        integer :: nno
        integer :: npg
        integer :: noe(9, 6, 4)
        integer :: ifa
        integer :: tymvol
        integer :: idfde
        real(kind=8) :: jac(9)
        real(kind=8) :: nx(9)
        real(kind=8) :: ny(9)
        real(kind=8) :: nz(9)
        real(kind=8) :: tx(3)
        real(kind=8) :: ty(3)
        real(kind=8) :: hf
    end subroutine calnor
end interface
