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
    subroutine pmfdgedef (typfib, b, gg, depl, alicom, nbfibr, nbcarm, vf, &
                          nbassepou, maxfipoutre, nbfipoutre, yj, zj, deffibasse, vfv, deffib)
        integer :: typfib
        real(kind=8) :: b(4)
        real(kind=8) :: gg
        real(kind=8) :: depl(12)
        real(kind=8) :: alicom
        integer :: nbfibr
        integer :: nbcarm
        real(kind=8) :: vf(nbcarm, nbfibr)
        integer :: nbassepou
        integer :: maxfipoutre
        integer :: nbfipoutre(*)
        real(kind=8) :: yj(*)
        real(kind=8) :: zj(*)
        real(kind=8) :: deffibasse(*)
        real(kind=8) :: vfv(7,*)
        real(kind=8) :: deffib(nbfibr)
    end subroutine pmfdgedef
end interface
