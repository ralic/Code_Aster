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
    subroutine lchobr(toler, itmax, mod, nbmat, materf,&
                      nr, nvi, depsm, sigm, vim,&
                      seuil, vp, vecp, icomp, sigp,&
                      vip, irtet)
        integer :: nbmat
        real(kind=8) :: toler
        integer :: itmax
        character(len=8) :: mod
        real(kind=8) :: materf(nbmat, 2)
        integer :: nr
        integer :: nvi
        real(kind=8) :: depsm(6)
        real(kind=8) :: sigm(6)
        real(kind=8) :: vim(*)
        real(kind=8) :: seuil
        real(kind=8) :: vp(3)
        real(kind=8) :: vecp(3, 3)
        integer :: icomp
        real(kind=8) :: sigp(6)
        real(kind=8) :: vip(*)
        integer :: irtet
    end subroutine lchobr
end interface
