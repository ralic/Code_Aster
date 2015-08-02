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
    subroutine vecgme(model    , cara_elem   , matez          , lload_namez, lload_infoz,&
                      inst_curr, disp_prevz  , disp_cumu_instz, vect_elemz , inst_prev  ,&
                      compor   , ligrel_calcz, vite_currz     , strx_prevz )
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: cara_elem
        character(len=*), intent(in) :: matez
        real(kind=8), intent(in) :: inst_curr
        character(len=*), intent(in) :: disp_prevz
        character(len=*), intent(in) :: disp_cumu_instz
        character(len=*), intent(in) :: lload_namez
        character(len=*), intent(in) :: lload_infoz
        character(len=*), intent(inout) :: vect_elemz
        real(kind=8), intent(in) :: inst_prev
        character(len=24), intent(in) :: compor
        character(len=*), intent(in) :: ligrel_calcz
        character(len=*), intent(in) :: vite_currz
        character(len=*), intent(in) :: strx_prevz
    end subroutine vecgme
end interface
