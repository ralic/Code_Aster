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
    subroutine glrcad(zimat, mp1, mp2, delas, rpara,&
                      dmax1, dmax2, dam1, dam2, curvcu,&
                      c1, c2, nbackn, deps, depsp,&
                      df, ddiss, dsidep, normm, normn,&
                      crit, codret)
        integer :: zimat
        real(kind=8) :: mp1(*)
        real(kind=8) :: mp2(*)
        real(kind=8) :: delas(6, 6)
        real(kind=8) :: rpara(5)
        real(kind=8) :: dmax1
        real(kind=8) :: dmax2
        real(kind=8) :: dam1
        real(kind=8) :: dam2
        real(kind=8) :: curvcu(3)
        real(kind=8) :: c1(6, 6)
        real(kind=8) :: c2(6, 6)
        real(kind=8) :: nbackn(6)
        real(kind=8) :: deps(6)
        real(kind=8) :: depsp(6)
        real(kind=8) :: df(6)
        real(kind=8) :: ddiss
        real(kind=8) :: dsidep(6, 6)
        real(kind=8) :: normm
        real(kind=8) :: normn
        real(kind=8) :: crit(*)
        integer :: codret
    end subroutine glrcad
end interface
