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
    subroutine ccchcr(crit, name_gd, nb_val_in, val_in, cmp_in,&
                      nb_cmp_out, val_out, ichk)
        character(len=16), intent(in) :: crit
        character(len=8), intent(in) :: name_gd
        integer, intent(in) :: nb_val_in
        real(kind=8), intent(in) :: val_in(nb_val_in)
        character(len=8), intent(in) :: cmp_in(nb_val_in)
        integer, intent(in) :: nb_cmp_out
        real(kind=8), intent(out) :: val_out(nb_cmp_out)
        integer, intent(out) :: ichk
    end subroutine ccchcr
end interface
