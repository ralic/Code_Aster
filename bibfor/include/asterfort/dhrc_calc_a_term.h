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
    subroutine dhrc_calc_a_term(i, j, sup_s, inf_s,a0, aa_t, ga_t, aa_c, ga_c, vint, &
                                a, ap1, ap2, as1, as2, rvp, sup_d, inf_d)
        integer, intent(in) :: i
        integer, intent(in) :: j
        integer, intent(in) :: sup_s
        integer, intent(in) :: inf_s
        integer, optional, intent(in) :: sup_d
        integer, optional, intent(in) :: inf_d
        real(kind=8), optional, intent(in) :: rvp
        real(kind=8), intent(in) :: vint(*)
        real(kind=8), intent(in) :: a0(6, 6)
        real(kind=8), intent(in) :: aa_t(6, 6, 2)
        real(kind=8), intent(in) :: ga_t(6, 6, 2)
        real(kind=8), intent(in) :: aa_c(6, 6, 2)
        real(kind=8), intent(in) :: ga_c(6, 6, 2)
        real(kind=8), intent(out) :: a
        real(kind=8), intent(out) :: ap1
        real(kind=8), intent(out) :: ap2
        real(kind=8), intent(out) :: as1
        real(kind=8), intent(out) :: as2
    end subroutine dhrc_calc_a_term
end interface 
