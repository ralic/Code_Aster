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
    subroutine pipef3(ndim, nno, nddl, npg, lgpg,&
                      wref, vff, dfde, mate, geom,&
                      vim, ddepl, deplm, ddepl0, ddepl1,&
                      dtau, copilo, typmod)
        integer :: lgpg
        integer :: npg
        integer :: nddl
        integer :: nno
        integer :: ndim
        real(kind=8) :: wref(npg)
        real(kind=8) :: vff(nno, npg)
        real(kind=8) :: dfde(2, nno, npg)
        integer :: mate
        real(kind=8) :: geom(nddl)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: ddepl(nddl)
        real(kind=8) :: deplm(nddl)
        real(kind=8) :: ddepl0(nddl)
        real(kind=8) :: ddepl1(nddl)
        real(kind=8) :: dtau
        real(kind=8) :: copilo(5, npg)
        character(len=8) :: typmod(2)
    end subroutine pipef3
end interface
