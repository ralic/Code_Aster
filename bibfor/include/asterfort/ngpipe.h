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
    subroutine ngpipe(typilo, npg, neps, nddl, b,&
                      li2ldc, typmod, mat, compor, lgpg,&
                      ddlm, sigm, vim, ddld, ddl0,&
                      ddl1, tau, etamin, etamax, copilo)
        integer :: lgpg
        integer :: nddl
        integer :: neps
        integer :: npg
        character(len=16) :: typilo
        real(kind=8) :: b(neps, npg, nddl)
        real(kind=8) :: li2ldc(0:neps-1)
        character(len=8) :: typmod(*)
        integer :: mat
        character(len=16) :: compor(*)
        real(kind=8) :: ddlm(nddl)
        real(kind=8) :: sigm(0:neps*npg-1)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: ddld(nddl)
        real(kind=8) :: ddl0(nddl)
        real(kind=8) :: ddl1(nddl)
        real(kind=8) :: tau
        real(kind=8) :: etamin
        real(kind=8) :: etamax
        real(kind=8) :: copilo(5, npg)
    end subroutine ngpipe
end interface
