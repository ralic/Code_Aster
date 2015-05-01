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
    subroutine vechth(type_ther , model_   , lload_name_, lload_info_, cara_elem_,&
                      mate_     , time_curr, time_      , temp_prev_ , vect_elem_,&
                      varc_curr_, time_move_)
        character(len=4), intent(in) :: type_ther
        character(len=*), intent(in) :: model_
        character(len=*), intent(in) :: lload_name_
        character(len=*), intent(in) :: lload_info_
        character(len=*), intent(in) :: cara_elem_
        real(kind=8), intent(in) :: time_curr
        character(len=*), intent(in) :: time_
        character(len=*), intent(in) :: temp_prev_
        character(len=*), intent(inout) :: vect_elem_
        character(len=*), intent(in) :: mate_
        character(len=*), optional, intent(in) :: varc_curr_
        character(len=*), optional, intent(in) :: time_move_
    end subroutine vechth
end interface
