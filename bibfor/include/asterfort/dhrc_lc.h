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
    subroutine dhrc_lc(epsm, deps, vim, pgl, option,&
                      sig, vip, a0, c0,&
                      aa_t, ga_t, ab, gb, ac,&
                      gc, aa_c, ga_c, cstseu, crit,&
                      codret, dsidep, debug)
! aslint: disable=W1504
        real(kind=8), intent(in) :: epsm(6)
        real(kind=8), intent(in) :: deps(6)
        real(kind=8), intent(in) :: vim(*)
        real(kind=8), intent(in) :: pgl(3, 3)
        character(len=16), intent(in) :: option
        real(kind=8), intent(out) :: sig(8)
        real(kind=8), intent(out) :: vip(*)
        real(kind=8), intent(in) :: a0(6, 6)
        real(kind=8), intent(in) :: c0(2, 2, 2)
        real(kind=8), intent(in) :: aa_t(6, 6, 2)
        real(kind=8), intent(in) :: ga_t(6, 6, 2)
        real(kind=8), intent(in) :: ab(6, 2, 2)
        real(kind=8), intent(in) :: gb(6, 2, 2)
        real(kind=8), intent(in) :: ac(2, 2, 2)
        real(kind=8), intent(in) :: gc(2, 2, 2)
        real(kind=8), intent(in) :: aa_c(6, 6, 2)
        real(kind=8), intent(in) :: ga_c(6, 6, 2)
        real(kind=8), intent(in) :: cstseu(6)
        real(kind=8), intent(in) :: crit(*)
        integer, intent(out) :: codret
        real(kind=8), intent(out) :: dsidep(6, 6)
        aster_logical, intent(in):: debug
    end subroutine dhrc_lc
end interface 
