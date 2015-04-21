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
    subroutine dhrc_lc(epsm, deps, vim, pgl, option,&
                      sig, vip, a0, c0,&
                      aa_t, ga_t, ab, gb, ac,&
                      gc, aa_c, ga_c, cstseu, crit,&
                      codret, dsidep)
! aslint: disable=W1504
        real(kind=8) :: epsm(6)
        real(kind=8) :: deps(6)
        real(kind=8) :: vim(*)
        real(kind=8) :: pgl(3, 3)
        character(len=16) :: option
        real(kind=8) :: sig(8)
        real(kind=8) :: vip(*)
        real(kind=8) :: a0(6, 6)
        real(kind=8) :: c0(2, 2, 2)
        real(kind=8) :: aa_t(6, 6, 2)
        real(kind=8) :: ga_t(6, 6, 2)
        real(kind=8) :: ab(6, 2, 2)
        real(kind=8) :: gb(6, 2, 2)
        real(kind=8) :: ac(2, 2, 2)
        real(kind=8) :: gc(2, 2, 2)
        real(kind=8) :: aa_c(6, 6, 2)
        real(kind=8) :: ga_c(6, 6, 2)
        real(kind=8) :: cstseu(6)
        real(kind=8) :: crit(*)
        integer :: codret
        real(kind=8) :: dsidep(6, 6)
    end subroutine dhrc_lc
end interface 
