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
    subroutine rc36sp(nbm, ima, ipt, c, k,&
                      cara, mati, pi, mi, matj,&
                      pj, mj, mse, nbthp, nbthq,&
                      ioc1, ioc2, spij, typeke, spmeca,&
                      spther)
        integer :: nbm
        integer :: ima(*)
        integer :: ipt
        real(kind=8) :: c(*)
        real(kind=8) :: k(*)
        real(kind=8) :: cara(*)
        real(kind=8) :: mati(*)
        real(kind=8) :: pi
        real(kind=8) :: mi(*)
        real(kind=8) :: matj(*)
        real(kind=8) :: pj
        real(kind=8) :: mj(*)
        real(kind=8) :: mse(*)
        integer :: nbthp
        integer :: nbthq
        integer :: ioc1
        integer :: ioc2
        real(kind=8) :: spij
        real(kind=8) :: typeke
        real(kind=8) :: spmeca
        real(kind=8) :: spther
    end subroutine rc36sp
end interface
