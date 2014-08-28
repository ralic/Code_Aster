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
    subroutine load_neum_evcu(model    , ligrel_calc, cara_elem, load_name     , i_load,&
                              inst_curr, disp_prev  , strx_prev, disp_cumu_inst, vite_curr,&
                              base     , resu_elem  , vect_elem)
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: cara_elem
        character(len=24), intent(in)  :: ligrel_calc
        real(kind=8), intent(in) :: inst_curr
        character(len=8), intent(in) :: load_name
        character(len=19), intent(in) :: disp_prev
        character(len=19), intent(in) :: strx_prev
        character(len=19), intent(in) :: disp_cumu_inst
        character(len=19), intent(in) :: vite_curr
        integer, intent(in) :: i_load
        character(len=19), intent(inout) :: resu_elem
        character(len=19), intent(in) :: vect_elem
        character(len=1), intent(in) :: base
    end subroutine load_neum_evcu
end interface
