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
    subroutine load_neut_prep(model, nb_in_maxi, nb_in_prep, lchin, lpain,&
                              mate_, varc_curr_, temp_prev_)
        character(len=24), intent(in) :: model
        integer, intent(in) :: nb_in_maxi
        character(len=8), intent(inout) :: lpain(nb_in_maxi)
        character(len=19), intent(inout) :: lchin(nb_in_maxi)
        integer, intent(out) :: nb_in_prep
        character(len=24), optional, intent(in) :: mate_
        character(len=19), optional, intent(in) :: varc_curr_
        character(len=19), optional, intent(in) :: temp_prev_
    end subroutine load_neut_prep
end interface
