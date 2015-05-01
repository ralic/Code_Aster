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
    subroutine nmvecd(imate, mate, nmat, matcst, loi,&
                      hook, dt, tp, p, np,&
                      beta, nb, ep, rm, dm,&
                      dsgde, dsgdb, dsgdp, drbde, drpde,&
                      rb, rp, drbdb, drbdp, drpdb,&
                      drpdp, etatf, ier)
        integer :: nb
        integer :: np
        integer :: nmat
        integer :: imate
        real(kind=8) :: mate(nmat, 2)
        character(len=3) :: matcst
        character(len=16) :: loi
        real(kind=8) :: hook(6, 6)
        real(kind=8) :: dt
        real(kind=8) :: tp
        real(kind=8) :: p(np)
        real(kind=8) :: beta(nb)
        real(kind=8) :: ep(*)
        real(kind=8) :: rm
        real(kind=8) :: dm
        real(kind=8) :: dsgde(nb, nb)
        real(kind=8) :: dsgdb(nb, nb)
        real(kind=8) :: dsgdp(nb, np)
        real(kind=8) :: drbde(nb, nb)
        real(kind=8) :: drpde(np, nb)
        real(kind=8) :: rb(nb)
        real(kind=8) :: rp(np)
        real(kind=8) :: drbdb(nb, nb)
        real(kind=8) :: drbdp(nb, np)
        real(kind=8) :: drpdb(np, nb)
        real(kind=8) :: drpdp(np, np)
        character(len=7) :: etatf(3)
        integer :: ier
    end subroutine nmvecd
end interface
