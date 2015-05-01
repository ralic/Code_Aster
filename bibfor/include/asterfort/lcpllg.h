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
    subroutine lcpllg(toler, itmax, mod, nbmat, mater,&
                      nr, nvi, deps, sigd, vind,&
                      seuil, icomp, sigf, vinf, devg,&
                      devgii, irtet)
        integer :: nbmat
        real(kind=8) :: toler
        integer :: itmax
        character(len=8) :: mod
        real(kind=8) :: mater(nbmat, 2)
        integer :: nr
        integer :: nvi
        real(kind=8) :: deps(6)
        real(kind=8) :: sigd(6)
        real(kind=8) :: vind(*)
        real(kind=8) :: seuil
        integer :: icomp
        real(kind=8) :: sigf(6)
        real(kind=8) :: vinf(*)
        real(kind=8) :: devg(6)
        real(kind=8) :: devgii
        integer :: irtet
    end subroutine lcpllg
end interface
