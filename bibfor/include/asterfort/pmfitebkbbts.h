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
    subroutine pmfitebkbbts(typfib, nf, ncarf, vf, ve, b, wi, gxjx, gxjxpou, g, gg, &
                            nbassepou, maxfipoutre, nbfipoutre, vev, yj, zj, vfv, skp, &
                            sk, vv, vvp)


        integer :: typfib
        integer :: nf
        integer :: ncarf
        real(kind=8) :: vf(ncarf, nf)
        real(kind=8) :: ve(nf)
        real(kind=8) :: b(4)
        real(kind=8) :: wi
        real(kind=8) :: gxjx
        real(kind=8) :: gxjxpou(*)
        real(kind=8) :: g
        real(kind=8) :: gg
        integer :: nbassepou
        integer :: maxfipoutre
        integer :: nbfipoutre(*)
        real(kind=8) :: vev(*)
        real(kind=8) :: yj(*)
        real(kind=8) :: zj(*)
        real(kind=8) :: vfv(7,*)
        real(kind=8) :: skp(78,*)
        real(kind=8) :: sk(78)
        real(kind=8) :: vv(12)
        real(kind=8) :: vvp(12,*)
    end subroutine pmfitebkbbts
end interface
