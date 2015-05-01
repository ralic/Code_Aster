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
    subroutine glrcdd(zimat, maxmp, minmp, matr, ep,&
                      surfgp, q, epst, deps, dsig,&
                      ecr, delas, dsidep, normm, normn,&
                      crit, codret)
        integer :: zimat
        real(kind=8) :: maxmp(*)
        real(kind=8) :: minmp(*)
        real(kind=8) :: matr(*)
        real(kind=8) :: ep
        real(kind=8) :: surfgp
        real(kind=8) :: q(2, 2)
        real(kind=8) :: epst(*)
        real(kind=8) :: deps(*)
        real(kind=8) :: dsig(*)
        real(kind=8) :: ecr(*)
        real(kind=8) :: delas(6, *)
        real(kind=8) :: dsidep(6, *)
        real(kind=8) :: normm
        real(kind=8) :: normn
        real(kind=8) :: crit(*)
        integer :: codret
    end subroutine glrcdd
end interface
