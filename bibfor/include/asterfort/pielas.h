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
    subroutine pielas(ndim, npg, kpg, compor, typmod,&
                      mate, elgeom, lgpg, vim, epsm,&
                      epsp, epsd, sigma, etamin, etamax,&
                      tau, copilo)
        integer :: lgpg
        integer :: npg
        integer :: ndim
        integer :: kpg
        character(len=16) :: compor(*)
        character(len=8) :: typmod(*)
        integer :: mate
        real(kind=8) :: elgeom(10, *)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: epsm(6)
        real(kind=8) :: epsp(6)
        real(kind=8) :: epsd(6)
        real(kind=8) :: sigma(6)
        real(kind=8) :: etamin
        real(kind=8) :: etamax
        real(kind=8) :: tau
        real(kind=8) :: copilo(5, npg)
    end subroutine pielas
end interface
