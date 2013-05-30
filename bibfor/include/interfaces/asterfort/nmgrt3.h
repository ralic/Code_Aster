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
    subroutine nmgrt3(nno, poids, kpg, vff, def,&
                      pff, option, axi, r, resi,&
                      rigi, dsidep, sign, sigma, matsym,&
                      matuu, vectu)
        integer :: nno
        real(kind=8) :: poids
        integer :: kpg
        real(kind=8) :: vff(*)
        real(kind=8) :: def(6, nno, 3)
        real(kind=8) :: pff(6, nno, nno)
        character(len=16) :: option
        logical :: axi
        real(kind=8) :: r
        logical :: resi
        logical :: rigi
        real(kind=8) :: dsidep(6, 6)
        real(kind=8) :: sign(6)
        real(kind=8) :: sigma(6)
        logical :: matsym
        real(kind=8) :: matuu(*)
        real(kind=8) :: vectu(3, nno)
    end subroutine nmgrt3
end interface
