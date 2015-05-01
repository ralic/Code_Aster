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
    subroutine pipepe(pilo, ndim, nno, npg, ipoids,&
                      ivf, idfde, geom, typmod, mate,&
                      compor, lgpg, deplm, sigm, vim,&
                      ddepl, depl0, depl1, copilo, dfdi,&
                      elgeom, iborne, ictau)
        integer :: lgpg
        integer :: npg
        integer :: ndim
        character(len=16) :: pilo
        integer :: nno
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        real(kind=8) :: geom(ndim, *)
        character(len=8) :: typmod(*)
        integer :: mate
        character(len=16) :: compor(*)
        real(kind=8) :: deplm(*)
        real(kind=8) :: sigm(2*ndim, npg)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: ddepl(*)
        real(kind=8) :: depl0(*)
        real(kind=8) :: depl1(*)
        real(kind=8) :: copilo(5, npg)
        real(kind=8) :: dfdi(*)
        real(kind=8) :: elgeom(10, *)
        integer :: iborne
        integer :: ictau
    end subroutine pipepe
end interface
