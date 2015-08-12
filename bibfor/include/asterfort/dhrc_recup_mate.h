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
    subroutine dhrc_recup_mate(imate, compor, a0, c0,&
                     aa_t, ga_t, ab, gb, ac,&
                     gc, aa_c, ga_c, cstseu)
        integer, intent(in) :: imate
        character(len=16), intent(in) :: compor
        real(kind=8), intent(out) :: a0(6, 6)
        real(kind=8), intent(out) :: c0(2, 2, 2)
        real(kind=8), intent(out) :: aa_t(6, 6, 2)
        real(kind=8), intent(out) :: ga_t(6, 6, 2)
        real(kind=8), intent(out) :: ab(6, 2, 2)
        real(kind=8), intent(out) :: gb(6, 2, 2)
        real(kind=8), intent(out) :: ac(2, 2, 2)
        real(kind=8), intent(out) :: gc(2, 2, 2)
        real(kind=8), intent(out) :: aa_c(6, 6, 2)
        real(kind=8), intent(out) :: ga_c(6, 6, 2)
        real(kind=8), intent(out) :: cstseu(6)
    end subroutine dhrc_recup_mate
end interface
