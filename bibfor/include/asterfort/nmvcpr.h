!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine nmvcpr(modelz   , mate , cara_elem      , varc_refe      , compor   ,&
                      hval_incr, base_, vect_elem_curr_, vect_elem_prev_, nume_dof_,&
                      cnvcpr_, nume_harm_)
        character(len=*), intent(in) :: modelz
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: varc_refe
        character(len=24), intent(in) :: cara_elem
        character(len=24), intent(in) :: compor
        character(len=19), intent(in) :: hval_incr(*)
        character(len=1), optional, intent(in) :: base_
        character(len=*), optional, intent(in) :: vect_elem_curr_
        character(len=*), optional, intent(in) :: vect_elem_prev_
        character(len=24), optional, intent(in) :: nume_dof_
        character(len=24), optional, intent(in) :: cnvcpr_
        integer, optional, intent(in) :: nume_harm_
    end subroutine nmvcpr
end interface
