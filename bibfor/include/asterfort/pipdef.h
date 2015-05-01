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
    subroutine pipdef(ndim, nno, kpg, ipoids, ivf,&
                      idfde, geom, typmod, compor, deplm,&
                      ddepl, depl0, depl1, dfdi, fm,&
                      epsm, epsp, epsd)
        integer :: ndim
        integer :: nno
        integer :: kpg
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        real(kind=8) :: geom(ndim, *)
        character(len=8) :: typmod(*)
        character(len=16) :: compor(*)
        real(kind=8) :: deplm(*)
        real(kind=8) :: ddepl(*)
        real(kind=8) :: depl0(*)
        real(kind=8) :: depl1(*)
        real(kind=8) :: dfdi(*)
        real(kind=8) :: fm(3, 3)
        real(kind=8) :: epsm(6)
        real(kind=8) :: epsp(6)
        real(kind=8) :: epsd(6)
    end subroutine pipdef
end interface
